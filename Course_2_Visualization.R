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

### Percentile - Percentiles are quantiles that divide the 
### dataset into 100 intervals each with 1% probability. 
### You can determine all percentiles of a dataset `data` 
### like this:

p <- seq(0.01, 0.99, 0.01)
quantile (data,p)

### Quartiles - Quartiles are quantiles that divide the 
### dataset into 4 parts, each of probability of 25%. 
### They are equal to the 25th, 50th, and the 75th 
### percentiles. The 25th percentile is also known as the 
### 1st quartile, the 50th Percentile is also known as the 
### median, and the 75th percentile is also known as the 
### 3rd Quartile.

### The `summary()` function returns the minimum, quartiles 
### and maximum of a vector.

### Examples:

### Summary Function

summary(heights$height)

### Percentiles

p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)

#### Since quantiles() returns named vector, we can access 
### the `25th` and the `75th` percentiles like:

percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

### Finding Quantiles with qnorm

### The `qnorm()` function gives the theoretical value of a
### quantile with probability `p` of observing a value equal
### to or less than that quantile value given a normal
### distribution with mean `mu` and standard deviation
### `sigma`:

qnorm(p, mu, sigma)

### By default, mu=0 and sigma = 1. Therefore, calling
### `qnorm()` with no arguments gives quantiles for the
### standard deviation for the standard normal distribution.

qnorm(p)

#### Recall that quantiles are defined such that p is the
#### probability of a random observation less than or equal
#### to the quantile.

### Relation with pnorm()

### The `pnorm()`function gives the probability that a value
### from a standard normal distribution will be less than or
### equal to a z-score value z. Consider:

round(pnorm(-1.96),3) == 0.025

### The result of `pnorm()` is the quantile. Note that:

round(qnorm(0.025),2) == -1.96

### qnorm() and pnorm() are inverse functions:

pnorm(qnorm(0.025))
 
### Lecture
mean(x <= 69.5) ### Basically the Proportion of the Values 
                ### in the data below q(69.5 inches) is 
                ### 51.5% (p). Q is Quantiles


### Now the thought is that we can make this computation for
### a series of `p`. If the Quantiles for the data match the
### Quantiles for the normal distribution, then it must be
### because the data is approximated by a normal
### distribution.

### Quantiles of the Actual Data

p <- seq(0.1, 0.95, 0.05)

observed_quantiles <- quantile(x,p)

observed_quantiles

### Quantiles of the Normal Distribution

theoretical_quantiles <- qnorm(p, mean=mean(x), sd = sd(x))

theoretical_quantiles


### Now to see if they match or not, we can plot them
### against each other, draw an Identity line to see if all
### the points fall on the line.

plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

### Note: The points fall almost on the line, meaning that
### the normal approximation is a pretty good approximation.

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

### Percentiles - Quantiles when p <- seq(0.01,0.99, 0.01)
### For eg. The 25th percentile gives us the data for which
### 25% other data is below.

### The median is the 50th percentile.

### Boxplots

##1.4 Exploratory Data Analysis-----------------------------

### Distribution of Female Heights

female_index <- heights$sex == "Female"
y_actual <- heights$height[female_index]
y <- scale(y_actual)
hist(y)
observed_female_quantiles <- quantile(y,p)
theoretical_female_quantiles <- qnorm(p)
plot(theoretical_female_quantiles,observed_female_quantiles)
abline(0,1)

#Section 2: Introduction to ggplot2-------------------------
##2.1 Basics of ggplot2-------------------------------------
library(tidyverse)

##Graph Components
library(dslabs)
library(murders)

ggplot(data=murders) ##Data Component
murders %>% ggplot()

p <- ggplot(data = murders) ### assigning ggplot of data to 
                            ### an object.
class(p)

##2.2 Customizing Plots-------------------------------------

## Layers

?geom_point

murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y=total))

#### Plotting Directly using gg plot

#### We can also plot using p
p + geom_point(aes(population/10^6, total))

### Now adding State name Layer and other Labels
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))

## Tinkering further, changing size of points
p + geom_point(aes(population/10^6, total), size=3) +
  geom_text(aes(population/10^6, total, label = abb),
            nudge_x = 1)
### Note that every time we add some property to our plot we
### have to define the aesthetics mapping of
### Population/10^6, and Total. We can bypass this by
### defining the `global aes`.

args(ggplot)

### The idea is then if we define the mapping to ggplot
### itself then by default any further layer that we add
### will be addressed to this default mapping.

### so now, we're redefining p, this time defining the
### mapping inside the ggplot function.

p <- murders%>% ggplot(aes(population/10^6, total, 
                           label=abb))

### now, moving further with previous code

p + geom_point(size=3) + geom_text(nudge_x = 1)

### Also, remember that the local aesthetics override global
### aesthetics.
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))

## Tinkering further, we now focus on Adjusting Scales,
## Labels and Colors and adding Lines.

### Since our desired scales are Log Scales (which are not
### the default scales), we then need them added to the
### scales layer.

p + geom_point(size = 3) +
geom_text(nudge_x = 0.075) +
scale_x_continuous(trans = "log10") +
scale_y_continuous(trans = "log10")

### Since the log scales are so common we can then use the
### following code in place.

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()

### Now just adding labels

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (Log scale)") +
  ylab("Total number of murders (Log scale)") +
  ggtitle("US Gun Murders in US 2010")

### Redefining p to include all this in one go

p <- murders%>% ggplot(aes(population/10^6, total, 
                           label=abb)) + 
  geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (Log scale)") +
  ylab("Total number of murders (Log scale)") +
  ggtitle("US Gun Murders in US 2010")

### Adding color to p

p+geom_point(aes(color=region), size=3)


###Average Rate Calculations for Murder Rates

r <- murders %>% 
  summarize(rate =(((sum(total))/
                      (sum(population)))*(10^6))) %>%
  pull(rate)

### Now we're plotting the line with slope r

p+geom_point(aes(color=region), size=3) + 
  geom_abline(intercept = log10(r), size=1)

### To get the desired Graph, we need a dashed line rather
### than a solid one, change the colour of the line from
### black to grey; and we need to make the line before the
### points.

### Thus we redefine our p

p <- murders%>% ggplot(aes(population/10^6, total, 
                           label=abb)) + 
  geom_abline(intercept = log10(r), 
              lty=2, color="darkgrey") +
  geom_point(aes(color=region), size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (Log scale)") +
  ylab("Total number of murders (Log scale)") +
  ggtitle("US Gun Murders in US 2010") 

###In order to Caps the region to Region

p <- p+ scale_color_discrete(name = "Region")

p

## Add on Packages

ds_theme_set()

install.packages("ggthemes")
install.packages("ggrepel")

library(ggthemes)
library(ggrepel)
### Since we've added new themes. Time to shine our p

p <- p + theme_economist()

p

### Note that the only problem now is that some of our text
### is overlapping with each other. To do this is why we
### needed ggrepel to repel from each other. just redefine
### geom_text with geom_text_repel in defining p and that
### should take care of itself. Remember to load gg repel in
### the library beforehand.

p <- murders%>% ggplot(aes(population/10^6, total, 
                           label=abb)) + 
  geom_abline(intercept = log10(r), 
              lty=2, color="darkgrey") +
  geom_point(aes(color=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (Log scale)") +
  ylab("Total number of murders (Log scale)") +
  ggtitle("US Gun Murders in US 2010") +
  scale_color_discrete(name = "Region")

p <- p + theme_economist()

p


### The Final Code

library(tidyverse)
library(dslabs)
library(ggthemes)
library(ggrepel)

data(murders)


r <- murders %>% 
  summarize(rate =(((sum(total))/
                      (sum(population)))*(10^6))) %>%
  pull(rate)


p <- murders%>% ggplot(aes(population/10^6, total, 
                           label=abb)) + 
  geom_abline(intercept = log10(r), 
              lty=2, color="darkgrey") +
  geom_point(aes(color=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (Log scale)") +
  ylab("Total number of murders (Log scale)") +
  ggtitle("US Gun Murders in US 2010") +
  scale_color_discrete(name = "Region")

p <- p + theme_economist()

p

## Other Examples - Histogram of Male Heights

heights %>% filter(sex == "Male")

p <- heights %>% filter(sex == "Male") %>% 
  ggplot(aes(x = height))

p + geom_histogram(binwidth = 1, fill='blue',col="Black", 
                   size=1 ) +
  xlab("Male Heights in inches") +
  ggtitle("Histogram")

## Smooth Densities 

p + geom_density(size=1, fill='blue')

## QQ Plots

p <- heights %>% filter(sex == "Male") %>% 
  ggplot(aes(sample = height))

p + geom_qq() + geom_abline()


### The QQ Plot by default uses Standard Normal Distribution
### i.e. mean = 0, sd = 1. In order to change this, we need
### to define dparams argument

params <- heights %>% 
  filter(sex =="Male") %>%
  summarize(mean = mean(height), sd = sd(height))


p + geom_qq(dparams = params) + geom_abline()

### QQ Plot of scaled data against Standard Normal
### Distribution

heights%>%
  ggplot(aes(sample=scale(height))) +
  geom_qq() +
  geom_abline()

## Making Grids of Plots (Multiple Plots in one Plot)
install.packages("gridExtra")
library(gridExtra)

p <- heights %>% 
  filter(sex=="Male") %>% 
  ggplot(aes(x = height))

p1 <- p +
  geom_histogram(binwidth = 1, fill = "blue", col="black")
p2 <- p +
  geom_histogram(binwidth = 2, fill = "blue", col="black")
p3 <- p +
  geom_histogram(binwidth = 3, fill = "blue", col="black")


grid.arrange(p1,p2,p3, ncol=3)

#Section 3: Summarizing with Dplyr--------------------------
##3.1 Summarizing with Dplyr--------------------------------

## Summarize 

library(tidyverse)
library(dslabs)
data(heights)

### We'll compute the average and the sd for Males

s <- heights %>% 
  filter(sex=="Male") %>% 
  summarize(  average= mean(height), 
              standard_deviation = sd(height))

s$average
s$standard_deviation

### Computing Median, Min, and Max

heights %>%
  filter(sex == "Male") %>%
  summarize(median = median(height),
            minimum= min(height),
            maximum= max(height))

### Computing median, min, and max using quantiles function

quantile(heights$height, probs=c(0.5,0,1))

### Remember: With the Function Summarize we can only call
### functions that return a single value

## dot placeholder
data(murders)

murders %>% mutate(murder_rate = total/population *10^6) %>%
  summarize(murders=mean(murder_rate))

### Note that the Actual US Average is not the Average of
### State murder rates as the size of state should also be
### taken into account. Thus, acutal murder rate should be
### Total Murders/Total Population *1000000

us_murder_rate <- murders %>%
  summarize(rate = sum(total)/ sum(population) *10^5)
us_murder_rate

us_murder_rate 
class(us_murder_rate)
### The problem with this computation is still data.frame
### and not a numeric even though it contains just one
### numeric value. This can pose severe challenges in
### functions that require only numeric values. In order to
### resolve we use the concept of dot placeholder. 














#Section 4: Gapminder---------------------------------------
##4.1 Introduction to Gapminder-----------------------------
#Section 5: Data Visualization Principles-------------------
##5.1 Data Visualization Principles, Part 1-----------------
##5.2 Data Visualization Principles, Part 2-----------------
##5.3 Data Visualization Principles, Part 3-----------------
## Assessment: Titanic Survival-----------------------------
#Comprehensive Assessment-----------------------------------

