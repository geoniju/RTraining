

install.packages("ggplot2movies")  # one-time only - do this if you have not done so already
library(ggplot2movies)  # once per R session
data(movies)

install.packages("plyr")  # one-time only
library(plyr)  # once per R session
data(baseball)

data(ozone)

install.packages("ggplot2")  # one-time only
library(ggplot2)  # once per R session
data(diamonds)

  
install.packages("robustHD")
library("robustHD")

library(MASS)

library(reshape2)

data(tips)


is.na

complete.cases

na.omit

na.rm --> argument

mean(movies$length)

mean(movies$budget)


mean(movies$budget,na.rm = TRUE)

mean(is.na(movies$budget))

moviesNONA = na.omit(movies)

qplot(rating,budget,data = moviesNONA, size = I(1.2)) + stat_smooth(color = 'red', size = I(2), se = F)

Dealing with outlers

Truncating
Remove outlier values
Winsorization
shrink outliers to border of main part of data
Robustness
Choose robust procedure for analysis

library(robustHD)
#Create the data to be Winsorized:
originalData <- c(1000.0000000  ,  1.8410249 ,  -0.7923505 ,   0.1776514  ,  0.4002135)
originalData <- c(1000,rnorm(10))
#Print the data to be Winsorized:
print(originalData[1:5])

#TODO winsorize the data:
print(winsorize(originalData))

?winsorize


