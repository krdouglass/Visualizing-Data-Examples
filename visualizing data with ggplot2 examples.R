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

# Load made up data --------------------------------------------------------

#load data
data <- read.csv (
  'C:/Users/kfake/Dropbox/Kim Fake/R Resources/Ggplot Bar Graph Example/visualizing-data-with-ggplot2-examples/Biology 101 Data CLEAN.csv', 
  stringsAsFactors = FALSE, 
  fileEncoding = 'UTF-8-BOM'
                  )


# load packages -------------------------------------------------------------

#we will use dplyr to prep some data to plot
library(dplyr)


#load ggplot package we will to visualize data
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
data$Letter_Grade <- ordered(data$Letter_Grade,
                            levels = c("A", "B", "C", "D", "F"),
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
p <- ggplot(sex_summary, #data
            aes(x=Sex, #x variable
                y=average_grade, #y variable
                fill = Sex #color by sex
                )
            ) +
  geom_bar(stat="identity") #bar graph
p

p <- p + xlab("Student Sex")
p

p <- p +  ylab ("Grade (%)")
p

p <- p + geom_errorbar( aes(x=Sex, 
                     ymin=average_grade-sd_grade, 
                     ymax=average_grade+sd_grade), 
                width=0.4, #width of T
                colour="black", #color
                alpha=0.9, #transparency
                size=1.3 #thickness
                )
p


# put the code all together
simple_bar <- ggplot(sex_summary, 
                     aes(x=Sex, 
                         y=average_grade, 
                         fill = Sex
                         )
                     ) +
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



# ggplot themes -----------------------------------------------------------

simple_bar + theme_classic()

simple_bar + theme_bw()

simple_bar + theme_test()

simple_bar + theme_minimal()

simple_bar + theme_linedraw()

simple_bar + theme_light()

simple_bar + theme_dark()

# reorder categories on axis ----------------------------------------------

# to create a specific order, order the factor the bars are based on
# reorder the factors
data1 <- data # make a new dta set where Sex is ordered
data1$Sex <- ordered(data1$Sex, # ordering sex
                    levels = c("M", "F")
                    )

# summarize sex data we want in the graph
ordered_sex_summary <- data1 %>%
  group_by(Sex) %>%
  summarize(average_grade = mean(Percent_Grade),
            sd_grade = sd(Percent_Grade)
  )

#plot
ordered_simple_bar <- ggplot(ordered_sex_summary, 
                     aes(x=Sex, 
                         y=average_grade, 
                         fill = Sex
                     )
) +
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

ordered_simple_bar


# customize colors --------------------------------------------------------

# change outlines and fill
white_simple_bar <- ggplot(sex_summary, 
                     aes(x=Sex, 
                         y=average_grade
                         
                     )
) +
  geom_bar(stat="identity",
           color = "black",# outline color
           fill = "white",# bar color
           ) +
  xlab("Student Sex") +
  ylab ("Grade (%)") +
  geom_errorbar(aes(ymin=average_grade-sd_grade, 
                    ymax=average_grade+sd_grade), 
                width=0.4, 
                colour="black", 
                alpha=0.9, 
                size=1.3
  ) 

white_simple_bar

# using color palettes from RColorBrewer package
# https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html
simple_bar + scale_fill_brewer(palette="Dark2")

# some options for grey scaled palettes
simple_bar + theme_classic() + scale_fill_brewer(palette="Greys")

simple_bar + scale_fill_grey()

# custom pick colors
# there are lots of helpful websites for finding hex codes for colors
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# https://htmlcolorcodes.com/
simple_bar + scale_fill_manual(values=c("#999999", "#E69F00"))

# alter axis --------------------------------------------------------------

# change scale and/or ticks for a continuous axis
simple_bar + scale_y_continuous(
  breaks = seq(0, 100,10), #ticks 0 to 100 every 10
  limits = c(0,100) #bottom and top of axis
                                ) 

# change labels for discrete axis
simple_bar + scale_x_discrete(
  breaks = c("M", "F"),# what is this useful for?
  labels = c("Male", "Female"), # custom names for bars
                              )

# axis labels angle and placement
simple_bar + theme(axis.text.x = element_text(
  angle = 90, #angle
  vjust = 2, # vertical placement
  hjust=2 # horizontal placement
                                              )
                  )


#change axis line
simple_bar + theme(axis.line.y = element_line(color="blue"))

# change length of axis tick marks
simple_bar + theme(axis.ticks.length = unit(1, "cm")) 

#adjust axis text
simple_bar + theme(
       axis.title.x = element_text(
                      size=12, # font size
                      face="bold", #bold, italics, etc.
                      margin = margin(
                        t = 20, #space above text before graph
                        b = 20 #space to bottom edge of figure
                                      ),                              
                      hjust=0.8, #horizontal justification
                      vjust=0.8, #vertical justification
                      angle = 45 #make at an angle    
                                  )
                    )

# graph title -------------------------------------------------------------



# Add title
simple_bar + ggtitle("Title") +
  theme(plot.title = element_text(
                      size=15, 
                      face="bold",
                      margin = margin(t = 20, #space above text edge of figure 
                                      b = 20 #space to bottom before graph
                      ),                              
                      hjust=0.8, #horizontal justification
                      vjust=0.8, #vertal justification
                                  )
        )

  

# alter legend ------------------------------------------------------------

simple_bar +  theme(legend.position = "none") #remove legend
simple_bar +  theme(legend.position = "right") #position right of graph
simple_bar +  theme(legend.position = "left") #position left of graph
simple_bar +  theme(legend.position = "bottom") #position below graph
simple_bar +  theme(legend.position = c(0.8, 0.8)) #custom position

simple_bar + theme(legend.text=element_text(
                      color= "blue", #text color
                      size=12, # text size
                      face = "bold.italic" #bold, italics etc.
                                           )
                  )


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
  geom_bar(stat="identity", 
           position = "dodge", #put bars next to each other
           width = 0.8 #bar width
  ) +
  xlab("Student Age (years)") +
  ylab ("Grade (%)") +
  geom_errorbar(aes(ymin=average_grade-sd_grade,
                    ymax=average_grade+sd_grade),
                position=position_dodge(0.8),
                width=0.5, # width of T
                colour="black", #color
                alpha=1, #transparency
                size=0.8 #line thickness
  ) +
  scale_fill_brewer(palette = "Set1")
cluster_bar


# stacked bar graph -------------------------------------------------------

# summarize data to use
class_sex_summary <- data %>%
  group_by(Class, Sex) %>%
  summarize(Quantity = n())

# make stacked bar graph
stacked_bar <- ggplot(class_sex_summary, 
                      aes(x=as.factor(Class), 
                          y=Quantity,
                          fill = Sex
                      )
) +
  geom_bar(stat="identity", 
           position = "stack" # stack bars on each other
  ) +
  xlab("Class") +
  ylab ("Number of Students")+
  scale_fill_brewer(palette = "Pastel1")

stacked_bar


# facet wrap --------------------------------------------------------------
# summarize each class' sex data we want in the graph
class_age_sex_summary <- data %>%
  group_by(Class, Sex) %>%
  summarize(average_grade = mean(Percent_Grade),
            sd_grade = sd(Percent_Grade)
  )

# face wrap will separates data into 
# multiple graphs based on a designated variable
facet_bar <- ggplot(class_age_sex_summary, 
                    aes(x=as.factor(Sex), 
                        y=average_grade, 
                        fill = Sex
                    )
) +
  geom_bar(stat="identity", position = "dodge") +
  xlab("Class") +
  ylab ("Grade (%)") +
  geom_errorbar(aes(ymin=average_grade-sd_grade,
                    ymax=average_grade+sd_grade),
                position=position_dodge(.9),
                width=0.4,
                colour="black",
                alpha=0.9,
                size=1.3
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(class_age_sex_summary$Class) # here we designate the variable 
# by which to make the multiple graphs

facet_bar

# scatter plot ------------------------------------------------------------

#make scatter plot
scatter_plot <- ggplot(age_sex_summary, #source the data
            aes(x=Age, # variable on x axis
                y=average_grade, # variable on y axis
                )
) +
  geom_point() + # scatter plot
  xlab("Age (years)") + # x axis label
  ylab ("Grade (%)") + # y axis label
  geom_smooth(method=lm, # add trend line with 95% confidence interval
              color = "black" # make the line black
              ) + 
  theme_minimal() # used minimal theme

scatter_plot


#make scatter plot without confidence interval
scatter_plot <- ggplot(age_sex_summary, #source the data
                       aes(x=Age, # variable on x axis
                           y=average_grade, # variable on y axis
                       )
) +
  geom_point() + # scatter plot
  xlab("Age (years)") + # x axis label
  ylab ("Grade (%)") + # y axis label
  geom_smooth(method=lm, # add trend line with 95% confidence interval
              color = "black", # make the line black
              se = FALSE # remove confidence interval
              ) + 
  theme_minimal() # used minimal theme

scatter_plot

#make scatter plot with color by variable
scatter_plot <- ggplot(age_sex_summary, #source the data
                       aes(x=Age, # variable on x axis
                           y=average_grade, # variable on y axis
                           shape = Sex
                           )
                      ) +
  geom_point() + # scatter plot
  xlab("Age (years)") + # x axis label
  ylab ("Grade (%)") + # y axis label
  geom_smooth(aes(group=Sex, # add trend line with 95% confidence interval
                  fill = Sex # color interval
                  ), 
              method="lm", # trend line formula
              color = "black", #color of lines
              size=1) + # size of lines
  theme_minimal() + # used minimal theme
  scale_fill_brewer(palette = "Pastel1")

scatter_plot

# box plot ----------------------------------------------------------------

#Load package with a bigger data set for the box plot examples
library(gapminder)

#look at the start of the data
head(gapminder)

#summaraize population of the continents
gapminder_summary <- gapminder %>%
  group_by(continent, pop) %>%
  summarize(avg_pop = mean(pop/1000000),
            sd_pop = sd(pop/1000000)
  )

#make a box plot
boxplot <- ggplot(gapminder_summary, #data
                  aes(x=continent, #x variable
                      y=avg_pop #y variable
                      )
) +
  geom_boxplot() + #box plot
  ylab("Average Country Populaiton (millions)")+
  xlab("Continent")

boxplot


# model coefficients graph ------------------------------------------------

#prep some data with 95% confidence interval
sex_summary <- sex_summary %>%
  mutate(lower = average_grade - 1.96 * sd_grade,
         upper = average_grade + 1.96 * sd_grade
         )

Coefficent_plot <- ggplot(sex_summary, #source the data
                       aes(x=average_grade, # variable on x axis
                           y=Sex, # variable on y axis
                           color=Sex
                       )
) +
  geom_point() + # point plot
  ylab(" ") + # y axis label
  xlab ("Grade (%)") + # x axis label
  scale_y_discrete(
    breaks = c("M", "F"),# what is this useful for?
    labels = c("Male", "Female" ) # custom names for bars
  ) +
  geom_pointrange( # add point with 95% Confidence Interval
    aes(xmin=lower, 
        xmax=upper
        ), 
    size = 1
                  ) +
  geom_vline(aes(xintercept = 70), # add y-axis where you want (passing grade)
             linetype="dotted" ,   # type of line, solid, dotted, etc.
             size = 1.2 #line thickness
             ) 


Coefficent_plot

# Make a multiple graph figure --------------------------------------------

#load ggpubr with useful functions for arranging figures
library(ggpubr)

#make multi panel figure
ggarrange(simple_bar, scatter_plot, nrow = 1, labels = "AUTO")

#
