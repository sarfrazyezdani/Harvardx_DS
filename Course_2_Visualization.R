#Section 1: Introduction to Data Visualization and ---------
#Distributions----------------------------------------------
##1.1 Introduction to Data Visualization--------------------
data(murders)
head(murders)
##1.2 Introduction to Distributions-------------------------
data(heights)
prop.table(table(heights$sex))
###CDF(Cumulative Distribution Function) for Dataset my_data
library(tidyverse)
library(dslabs)
data(heights)

a <- seq(min(my_data), max(my_data), 
         length=100) #Define Range of 
                     #Values, and span.
cdf_function <- function(x){
  mean(my_data <=x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_function)

###Smooth Density Plots
hist(heights$height, equidist)

###Normal Distribution
avg <- function(x){
  sum(x)/length(x)
}
SD <- sqrt(sum((x-avg(x))^2)/(length(x)))

index <- heights$sex =="Male"
x <- heights$height[index]

avg(x)
SD

z <- scale(x) ##Returns the heights in Standard Units
mean(abs(z) < 2)

###Let's redefine average to prebuilt funcs.
average <- mean(x)
SD <- sd(x)
c(average=average, SD=SD)

###The Normal CDF and pnorm
pnorm()


plot(prop.table(table(x)), xlab="a= Heights in Inches"
     , ylab = "Pr(X=a)") ###Shows discrete heights reported
                         ###by students.

###The normal distribution is useful for approximating the 
###proportion of students reporting heights between 69.5 
###and 70.5 and so on.

mean(x <= 68.5) - mean(x <= 67.5) ###Actual Data
mean(x <= 69.5) - mean(x <= 68.5) ###Actual Data
mean(x <= 70.5) - mean(x <= 69.5) ###Actual Data

pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
###Normal Distribution Approximation
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
###Normal Distribution Approximation
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
###Normal Distribution Approximation

### Conclusion - For these Intervals the approximation is 
### quite useful.

### The approximation messes up for distributions that don't
### include an integer. For e.g.
mean(x <= 70.9) - mean(x <= 70.1) ###Actual Data
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
###Normal Distribution Approximation 
#### This is often referred to as "Discretization".


##1.3 Quantiles, Percentiles, and Boxplots------------------
### Quantiles - Quantiles are cutoff points that divide a 
### dataset into Intervals with set probabilities. The qth 
### quantile is the value at which q% of the observations 
### are equal to or less than that value.

### Using Quantile - given a dataset `data` and desired 
### quantile `q`, you can find the `q`th quantile of `data` 
### with:

quantile(data,q)

##1.4 Exploratory Data Analysis-----------------------------
#Section 2: Introduction to ggplot2-------------------------
##2.1 Basics of ggplot2-------------------------------------
##2.2 Customizing Plots-------------------------------------
#Section 3: Summarizing with Dplyr--------------------------
##3.1 Summarizing with Dplyr--------------------------------
#Section 4: Gapminder---------------------------------------
##4.1 Introduction to Gapminder-----------------------------
#Section 5: Data Visualization Principles-------------------
##5.1 Data Visualization Principles, Part 1-----------------
##5.2 Data Visualization Principles, Part 2-----------------
##5.3 Data Visualization Principles, Part 3-----------------
## Assessment: Titanic Survival-----------------------------
#Comprehensive Assessment-----------------------------------