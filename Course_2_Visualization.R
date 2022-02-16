# Section 1: Introduction to Data Visualization and ---------
# Distributions----------------------------------------------
## 1.1 Introduction to Data Visualization--------------------
data(murders)
head(murders)
## 1.2 Introduction to Distributions-------------------------
data(heights)
prop.table(table(heights$sex))
### CDF(Cumulative Distribution Function) for Dataset
### my_data
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

### Smooth Density Plots
hist(heights$height, equidist)

### Normal Distribution
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

# Section 2: Introduction to ggplot2-------------------------
## 2.1 Basics of ggplot2-------------------------------------
library(tidyverse)

## Graph Components
library(dslabs)
library(murders)

ggplot(data=murders) ##Data Component
murders %>% ggplot()

p <- ggplot(data = murders) ### assigning ggplot of data to 
                            ### an object.
class(p)

## 2.2 Customizing Plots-------------------------------------

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


### Average Rate Calculations for Murder Rates

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

### In order to Caps the region to Region

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
### numeric value. And most of Dplyr functions work
### similarly. This can pose severe challenges in functions
### that require only numeric values. In order to resolve we
### use the concept of dot placeholder.

us_murder_rate %>% .$rate

class(us_murder_rate %>% .$rate)

### The above code can be replicated in the following way to
### return a numeric value rather than a Data Frame.

us_murder_rate <- murders %>%
  summarize(rate = sum(total)/ sum(population) *10^5) %>%
  .$rate

us_murder_rate 
class(us_murder_rate)

##Group By - group then summarize 

### Used when you need to group then summarize the data
### frame into those different groups.

heights %>% group_by(sex)

### This generates a special data frame called "Group
### DataFrame" and `summarize` function will act differently
### on this object.

class(heights%>%group_by(sex))

heights %>% 
  group_by(sex) %>% 
  summarize(average = mean(height), 
            standard_deviation = sd(height))

### Another Example - Median Murder Rate in the Four
### Different Regions of the Country

murders <- murders%>% 
  mutate(murder_rate = total/population *100000)

murders %>% 
  group_by(region) %>% 
  summarize(median_rate = median(murder_rate)) 

## Sorting Data Tables

### In order to sort entire data tables we can use arrange()
### in place of sort() and order()

murders %>% arrange(population) %>% head() # Answer in 
                              #ascending Order  is default

murders %>% arrange(murder_rate) %>% head()

### To arrange in descending order - use desc()

murders %>% arrange(desc(murder_rate)) %>% head()

### We can do nested sorting - sorting within sorting -
### sort-ception!!

murders %>% arrange(region, desc(murder_rate)) %>% head()

#### Interestingly, the use of the head() is to limit the
#### amount of data visible to around 6. That is only 6
#### entries are displayed within our table. If we're to
#### display like say TOP TEN then we should be using the
#### top_n() function.

murders %>% top_n(10, murder_rate) # Notice they're not 
                                   # ordered.

### In order to order them.

murders %>% arrange(desc(murder_rate)) %>% top_n(10)

#Section 4: Gapminder---------------------------------------
##4.1 Introduction to Gapminder-----------------------------
###Case Study: Trends in World Health and Economics --------

#### Question 1- Is it fair to say the world is divided into
#### rich and poor?

#### Question 2- Has income inequality worsened during the
#### last 40 years?

library(dslabs)
data(gapminder)
head(gapminder)
library(tidyquant)
library(tidyverse)

### Q.1 Which country has the highest child mortality rate
### Sri-Lanka vs. Turkey?

### Q.2 Which pairs do you think are most similar?

gapminder %>% filter(year == 2015 & 
                       country %in% 
                       c("Sri Lanka", "Turkey")) %>% 
  select(country, infant_mortality)

### Poland vs. South Korea

gapminder %>% filter(year == 2015 & country %in% 
                       c("Poland", "South Korea")) %>% 
  select(country, infant_mortality)

### Malaysia vs. Russia

gapminder %>% filter(year == 2015 & country %in% 
                       c("Malaysia", "Russia")) %>% 
  select(country, infant_mortality)

### Pakistan vs. Vietnam

gapminder %>% filter(year == 2015 & country %in% 
                       c("Pakistan", "Vietnam")) %>% 
  select(country, infant_mortality)

### Thailand vs. South Africa

gapminder %>% filter(year == 2015 & country %in% 
                       c("Thailand", "South Africa")) %>% 
  select(country, infant_mortality)

### Scatterplot of Life expectancy vs. Fertility rates

ds_theme_set()
filter(gapminder, year == 1962) %>% 
  ggplot(aes(fertility, life_expectancy, color= continent, 
             size = country[log10(population)]))+ 
  geom_point()

### But this was in 1962, is this still a reality?

### Faceting

filter(gapminder, year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color= continent))+ 
  geom_point() + 
  facet_grid(continent~year)

### but this show continents separately, but what about the
### BIG PICTURE?

gapminder %>% filter(year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color= continent))+
  geom_point() + facet_grid(.~year)

### What about multiple time periods? For that use
### Facet_wrap().

years <- c(1962,1970,1980,1990,2000,2010,2012)

continents <- c("Europe", "Asia")

gapminder %>% 
  filter(year %in% years & 
                       continent %in% continents) %>% 
  ggplot(aes(fertility, life_expectancy, col= continent))+
  geom_point() + 
  facet_wrap(~year)

### Time Series Plots

### US fertility Rates across the years

gapminder %>% filter(country == "United States") %>% 
  ggplot(aes(year, fertility)) + geom_point()


### Fertility Rates comparison - US vs India
gapminder %>% filter(country %in% 
                       c("United States", "India")) %>% 
  ggplot(aes(year, fertility, col= country)) + geom_point()

### in lines

gapminder %>% filter(country %in% 
                       c("United States", "India", 
                         "Germany", "South Korea")) %>% 
  ggplot(aes(year, fertility, col=country)) + 
  geom_line(size = 1)

### For Time Series Plots - it is recommended to use labels
### instead of legends. (Although Legends are generated
### automatically and easier to generate)

### Adding Labels
countries <- c("South Korea", "Germany")
labels <- data.frame(country = countries, x = c(1977, 1965),
                     y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 4) +
  theme(legend.position = "none")

## Transformations

gapminder <- gapminder %>% 
  mutate(dollars_per_day = (gdp)/(population*365))

### Scaled Function - You see values with powers of 2

past_year <- 1970

gapminder%>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color="black")
  


### X- Axis Scaled - You see OG value on a Log scaled Axis

gapminder%>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color="black") +
  scale_x_continuous(trans="log2")


### Stratify & Boxplots


### Stratification is necessary to differentiate countries that are poor from rich.

p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

### rotating text

p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### reorder() - This function lets us change the order of
### the levels of a factor variable based on a summary
### computed on a numeric vector.

#### Example

fac <- factor(c("Asia", "Asia", "West", "West", "West"))

levels(fac)

value <- c(10, 11, 12, 6, 4)

fac <- fac %>% reorder(value, FUN=mean)

levels(fac)

### Back to our original example

p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, 
                          FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text = element_text(angle=90, hjust=1)) +
  xlab("")

p

p + scale_y_continuous(trans = "log2")

p + 
  scale_y_continuous(trans = "log2") + 
  geom_point(show.legend = FALSE)

### Comparing Distributions

west <- c("Western Europe", "Northern Europe", 
          "Southern Europe", "Northern America", 
          "Australia and New Zealand")






#####################################################################################################################
# add dollars per day variable and define past year
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
past_year <- 1970

# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

# facet by West vs developing
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)

# facet by West/developing and year
present_year <- 2010
gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)


# define countries that have data available in both years
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# make histogram including only countries with data available in both years
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)

# arrange matching boxplots next to each other, colored by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))


# see the code below the previous video for variable definitions

# smooth density plots - area under each curve adds to 1
gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()

# smooth density plots - variable counts on y-axis
p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)


# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))


# note you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)


# weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)











#Section 5: Data Visualization Principles-------------------
##5.1 Data Visualization Principles, Part 1-----------------
##5.2 Data Visualization Principles, Part 2-----------------
##5.3 Data Visualization Principles, Part 3-----------------
## Assessment: Titanic Survival-----------------------------
#Comprehensive Assessment-----------------------------------

### Question 1
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)

str(stars)
mean(stars$magnitude)
sd(stars$magnitude)


library(ggplot2)

stars%>%ggplot(aes(magnitude))+geom_density()

stars%>%ggplot(aes(temp)) + geom_density()

stars%>%ggplot(aes(log10(temp), magnitude, color=type, 
                   label=star)) + geom_point() + 
  scale_y_reverse() + 
  scale_x_reverse() + geom_text_repel()


### Question 2

library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

head(temp_carbon)
str(temp_carbon)
names(temp_carbon)

temp_carbon

is.na(temp_carbon)

temp_carbon$carbon_emissions[which.max(
  temp_carbon$year & 
  !is.na(temp_carbon$carbon_emissions))]

temp_carbon %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(.$year)


p <- temp_carbon %>% ggplot(aes(year, carbon_emissions)) + geom_point()
p

p <- p + geom_vline(aes(xintercept = 0), col = "blue")
p
p <- p + geom_hline(aes(y = 0), col = "blue")
p
p <- p + geom_hline(aes(yintercept = 0, col = blue))
p
p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p


p + ylim("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean", col = "blue"))

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

p + ylab("Temperature anomaly (degrees C)") +
  title("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

rm(list=ls())

p <- temp_carbon %>% ggplot(aes(year, temp_anomaly)) + geom_point() + geom_hline(aes(yintercept = 0), col = "blue") + ylab("Temperature anomaly (degrees C)") +
  title("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

p

p <- p + geom_density(aes(year, ocean_anomaly))
p
temp_carbon


temp_carbon %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_line(aes(year, land_anomaly), col = "red") +
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  xlim(c(1880, 2018)) +
  ggtitle("Temperature anomaly on land and ocean")




library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, color=source)) +
  geom_line() + xlim(c(-800000, -775000))
co2_time

co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, color=source)) +
  geom_line() + xlim(c(-375000, -330000))
co2_time


co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, color=source)) +
  geom_line() + xlim(c(-140000, -120000))
co2_time

co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, color=source)) +
  geom_line() + xlim(c(-3000, 2018))
co2_time

install.packages("titanic")

library(titanic)

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

?titanic_train

head(titanic_train)
