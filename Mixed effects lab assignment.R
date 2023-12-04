library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("Matrix")

#1. DATA EXPLORATION

#to load the data from base R
install.packages("lme4")
library(lme4)
data("sleepstudy")

# Checking the data structure
str(sleepstudy) 

#checking the first 6 rows in the dataset
head(sleepstudy) 
?sleepstudy
#viewing the all rows in the dataset
View(sleepstudy) 

#checking the definitions of variables in the data set
?sleepstudy

#data visualisation 

#reaction times distribution-Histogram
library(ggplot2)
ggplot(sleepstudy, aes(x = Reaction)) +
  geom_histogram(binwidth = 10, fill = "coral", color = "black") +
  labs(title = "Distribution of Reaction Times", x = "Reaction Time", y = "Frequency")
#here I am trying to visualize the distribution of the dependent variable(reaction times)using a histogram
#the reason is to check if the reaction time is normally distributed


# distribution of reaction time for each subject-boxplot
ggplot(sleepstudy, aes(x = factor(Subject), y = Reaction)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Reaction Time by Subject",
       x = "Subject", y = "Reaction Time")


# Reaction time over days of sleep deprivation  for each subject-Line plot 
ggplot(sleepstudy, aes(x = Days, y = Reaction, group = Subject, color = Subject)) +
  geom_line() +
  labs(title = "Reaction Time Over Time for Each Subject",
       x = "Days", y = "Reaction Time")



#2 DESCRIPTVE STATISTICS*****

# Summary statistics for Reaction time
summary(sleepstudy[, c("Reaction" )])

#Standard deviation for reaction time 
sd_reaction <- sd(sleepstudy$Reaction)
sd_reaction

# Summary statistics for days
summary(sleepstudy[, c( "Days")])
#Standard deviation for days
sd_days <- sd(sleepstudy$Days)
sd_days

#  relationship between reaction time and days-Scatter plot with a regression line
ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship Between Reaction Time and Days",
       x = "Days", y = "Reaction Time")
#I try to visualize the relationship between the reaction time and the number of days of sleep deprivation
#the scater plot shows a positive relationship between the number of days of sleep deprivation and the reaction time.
#it means, the more participants were deprived of sleep(sleep deprivavtion increases), the the more the reaction time increases.
#Thus, as the number of days of sleep deprivation increases, the more their response time increases.


# Reaction Times for each day-Boxplot
ggplot(sleepstudy, aes(x = factor(Days), y = Reaction)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Reaction Times for Each Day", x = "Days", y = "Reaction Time") +
  theme_minimal()

#The relationship between reaction time and the day of sleep deprivation is positive ; The average reaction time increases with days of sleep deprivation. 

# Fitting linear mixed-effects model using lmer
library(lme4)
# Logarithmic transformation
sleepstudy$LogReaction <- log(sleepstudy$Reaction) 
sleepstudy$LogReaction
# becuase the dependent variable, reactions time distribution was not normal
#I have to transform the variable using log function


#Linear Mixed Models: 
model <- lmer(LogReaction ~ Days + (1|Subject), data = sleepstudy)
summary(model)

 #residuals analysis
residuals <- residuals(model)
residuals 

#Plotted residuals analysis

par(mfrow = c(1,3))

hist(residuals)

qqnorm(residuals)

qqline(residuals)

plot(fitted(model),residuals)
