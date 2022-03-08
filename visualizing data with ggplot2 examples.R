##################################################################
# The purpose of this R script is to demonstrate data visualization 
# with ggplot. This was made mostly for my own reference and a way to 
# practice these functions so it is far from perfect or comprehensive,  
# but I will share it for others who may find it helpful.
#
# Script by: Kim Fake
#
#
###################################################################

# To Clear working environment
rm(list=ls())
graphics.off()

# Load madeup data --------------------------------------------------------

#load data
data <- read.csv (
  'C:/Users/kfake/Dropbox/Kim Fake/R Resources/Data Cleaning Example/Biology 101 Data CLEAN.csv', 
  stringsAsFactors = FALSE, 
  fileEncoding = 'UTF-8-BOM'
)


# load ggplot -------------------------------------------------------------



#load ggplot package
library(ggplot2)

# Don't see an example of what you need? Check out this helpful resource
# https://www.r-graph-gallery.com/index.html


# fix data types ----------------------------------------------------------


# change a numeric to an integer because these are discrete counts,
# not continuous variables
data$Classes_Attended <- as.integer(data$Classes_Attended) 

# change an integer to a numeric because these are continuous variables
data$Percent_Grade <- as.numeric(data$Percent_Grade)

#change character to category (i.e. factor)
data$Sex <- as.factor(data$Sex)

#change character to category (i.e. factor) with ordered levels
data$Letter_Grade <- factor(data$Letter_Grade,
                            levels = c("A", "B", "C", "D", "F")
)


# simple bar graph --------------------------------------------------------

# summarize sex data we want in the graph
sex_summary <- data %>%
  group_by(Sex) %>%
  summarize(average_grade = mean(Percent_Grade),
            sd_grade = sd(Percent_Grade)
  )


# build a bar graph step by step
# to learn what each part does
# a simple bar graph of grade for each sex
p <- ggplot(sex_summary, aes(x=Sex, y=average_grade, fill = Sex)) +
  geom_bar(stat="identity") 
p

p <- ggplot(sex_summary, aes(x=Sex, y=average_grade)) +
  geom_bar(stat="identity") 
p

p <- p + xlab("Student Sex")
p

p <- p +  ylab ("Grade (%)")
p

p <- p + geom_errorbar( aes(x=Sex, 
                     ymin=average_grade-sd_grade, 
                     ymax=average_grade+sd_grade), 
                width=0.4, 
                colour="black", 
                alpha=0.9, 
                size=1.3
                )
p

p <- p +  theme_minimal()
p

# put the code all together
simple_bar <- ggplot(sex_summary, aes(x=Sex, y=average_grade, fill = Sex)) +
  geom_bar(stat="identity") +
  xlab("Student Sex") +
  ylab ("Grade (%)") +
  geom_errorbar(aes(ymin=average_grade-sd_grade, 
                    ymax=average_grade+sd_grade), 
                  width=0.4, 
                  colour="black", 
                  alpha=0.9, 
                  size=1.3
              ) 
simple_bar


# clustered bar graph -----------------------------------------------------

# summarize birth year and sex data we want in the graph
age_sex_summary <- data %>%
  group_by(Year, Sex) %>%
  summarize(average_grade = mean(Percent_Grade),
            sd_grade = sd(Percent_Grade)
  ) %>%
  mutate(Age = 2022-Year)

# put the code all together
cluster_bar <- ggplot(age_sex_summary, 
                    aes(x=as.factor(Age), 
                        y=average_grade, 
                        fill = Sex
                        )
                      ) +
  geom_bar(stat="identity", position = "dodge") +
  xlab("Student Age (years)") +
  ylab ("Grade (%)") +
  geom_errorbar( aes(ymin=average_grade-sd_grade,
                     ymax=average_grade+sd_grade),
                 position=position_dodge(.9),
                 width=0.4,
                 colour="black",
                 alpha=0.9,
                 size=1.3
  ) 
cluster_bar


# stacked bar graph -------------------------------------------------------



# facet wrap --------------------------------------------------------------

# face wrap will separates data into 
# multiple graphs based on a designated variable
facet_bar <- ggplot(age_sex_summary, #source the data
            aes(x=Sex, # variable on x axis
                y=average_grade, # variable on y axis
                fill = Sex # variable to color by
            )
) +
  geom_bar(stat="identity", position = "dodge") + # clustered bar graph
  xlab("Student Age (years)") +
  ylab ("Grade (%)") +
  geom_errorbar( aes(ymin=average_grade-sd_grade,
                     ymax=average_grade+sd_grade),
                 position=position_dodge(.9),
                 width=0.4,
                 colour="black",
                 alpha=0.9,
                 size=1.3
  ) +
  facet_wrap(age_sex_summary$Age)
facet_bar


# ggplot themes -----------------------------------------------------------
simple_bar + theme_classic()

simple_bar + theme_bw()

simple_bar + theme_test()

simple_bar + theme_minimal()

simple_bar + theme_linedraw()

simple_bar + theme_light()

simple_bar + theme_dark()



# scatter plot ------------------------------------------------------------

#make scatter plot
p <- ggplot(age_sex_summary, #source the data
            aes(x=Age, # variable on x axis
                y=average_grade # variable on y axis
            )
) +
  geom_point() + # scatter plot
  xlab("Age (years)") + # x axis label
  ylab ("Grade (%)") + # y axis label
  ggtitle("Relationship Between Grades and Age") + # add title
  geom_smooth(method=lm) + # add trend line with 95% confidence interval
  theme_minimal() # used minimal theme

p



