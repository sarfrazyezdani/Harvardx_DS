#Section 1: R Basics, Functions, and Data Types------------
##R Basics-------------------------------------------------
##Data Types-----------------------------------------------
##Section 1 Assessment-------------------------------------
###End of Section------------------------------------------

#Section 2: Vectors, Sorting-------------------------------
##Vectors--------------------------------------------------
##Sorting--------------------------------------------------
install.packages("dslabs") ###Installing dslabs package
library("dslabs")   ###Loading the dslabs package into the
                    ###R session.
data(murders)       ###Loading the Data of Gun Murders.
sort(murders$total) ###Sorts the Data of Total Murders
                    ###In an ascending order.
murders
order(murders$total)###Tells which entry is the lowest to
                    ###highest in a table.
index<-order(murders$total) ###We ordered the murders - 
                            ###total no. in an ascending 
                            ###order and named it "Index".
                            ###Now Index shows which Entry 
                            ###has the lowest entry of Total
                            ###Murders from the Murder 
                            ###Table.
murders$state[index]  ###Now we display the state names from
                      ###table murder depending on the order
                      ###of the entries taken from "Index".
murders$abb[index]  ###Now we display the abb names from
                    ###table murder depending on the order
                    ###of the entries taken from "Index".
###If we're only interested in the Largest and the Smallest 
###Values - then we can make use of Max and Min Functions
### Max Functions - For finding only the largest values
max(murders$total)###Displays the max. Value of total 
                  ###murders from Murder table.
i_max<-which.max(murders$total) ###Which.Max gives the index
                                ###value (which entry) at 
                                ###which this max. value of
                                ###1257 resides.
i_max

murders$state[i_max] ###Displays the State from the Murder
                     ###Total at the i_max Index Value.

### Min Functions - For finding only the smallest values
min(murders$total)###Displays the min. Value of total 
                  ###murders from Murder table.
i_min<-which.min(murders$total) ###Which.Max gives the index
                                ###value (which entry) at 
                                ###which this min. value of
                                ###2 resides.
i_min

murders$state[i_min] ###Displays the State from the Murder
                     ###Total at the i_min Index Value.
###Ranking - telling which entry is where in terms of size.
x<-c(31,4,15,92,65)
x
rank(x) ###Since 31 is 3 smallest of the vector, thus first 
        ###entry 3. so on and so forth.
        ###Returns rank (smallest to largest)
####Assessment on Data Camp.

##Vector Arithmetic----------------------------------------

murders$state[which.max(murders$population)] ###Which state 
                              ###has the largest population?
max(murders$population)

####Need to compute Murders Per Capita
#### Height Example
heights <- c(69,62,66,70,70,73,67,73,67,70) 
heights * 2.54 
heights - 69
#### Example illustrates that arithmetic operations occurs 
####element wise in R.

###To compute the murder per capita i.e. the Murder Rate
murder_rate <- (murders$total/murders$population)*100000
    ###the above operation defines murder rates as the ratio
    ###of total murders per total populations and multiplied
    ###by 100,000 to get the proper values.
murders$state[order(murder_rate, decreasing=TRUE)]
    ###the above operations displays states with murder 
    ###rates in a decreasing order.
####Assessment on Data Camp.

##Section 2 Assessment-------------------------------------
###Question 1
x <- c(2,43,27,96,18)
####output - 1,2,3,4,5
####Input- None of these
####Output - 1,5,3,2,4
####Input
order(x)
####Output - 1,4,3,5,2
####Input
rank(x)
####Output - 2,18,27,43,96
####Input
sort(x)

###Question 2
min(x)
which.min(x)
max(x)
which.max(x)

###Question 3
####Mandi, Amy, Nicole, and Olivia all ran different 
####distances in different time intervals. Their distances 
####(in miles) and times (in minutes) are as follows:

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time <- time/60
time <- round(time, digits =4)

speed <- distance/time
speed <- round(speed, 2)
records <- data.frame(name=name, distance=distance, time=time, speed=speed)

records

records$name[which.max(records$speed)]
###End of Section------------------------------------------

#Section 3: Indexing, Data Wrangling, Plots----------------

##Indexing-------------------------------------------------
murder_rate <- (murders$total/murders$population) *100000

####Suppose you're travelling from Italy whose murder rate 
####is 0.7, and you'll only stay in the states where the 
####murder rates is equal or less. Therefore,

index <- murder_rate <= 0.7 ###This gives the "True" Logical
                            ###value for those states with 
                            ###equal or lower murder rates.
index

murders$state[index] ###States with equal or lower murder 
                     ###rates
sum(index) ###Logical Vectors are turned to numeric with 
           ###True = 1 and False = 0

#### Problem - Safe State - Murder rate at most 1.00
#### in West

#Logical Operators in R
# < - Less than
# <= - Less than or equal to
# > - Greater than
# >= - Greater than or equal to
# == - exactly equal to (Check if equal)
# != - not equal to (check if not equal)
# ! - NOT
# | - OR
# & - AND

west <- murders$region == "West" ### west region defined.
safe <- murder_rate <= 1 ###murder rate equal or less 
                         ###than 1 defined.
index <- safe & west ###Index involves both the traits

index

murders$state[index] 

##Indexing Functions

### Three Indexing Functions - which, match, %in%

### which
### which - gives the entry number corresponding to the 
### operation.
### which example 1
x <- c(FALSE,TRUE,FALSE,TRUE,TRUE,FALSE)
which(x) ### which in x are TRUE i.e. Entry Number - 2, 4, 5

### which example 2
index <- which(murders$state != "Massachusetts")
index

murder_rate[index]

### match
### match - returns the entry number from vector 2 that 
### matches the operation of vector 1
### match example 1
index <- match(c("New York","Florida","Texas"),murders$state)
index ### Notice that match function returns the entry 
      ### number that New York, Texas, etc. had in the 
      ### murders$states table.
murders$state[index]
murder_rate[index]

### %in% 
### %in% - to check whether elements of one vector are in 
### the other vector or not.
### %in% example 1
x <- c("a","b","c","d","e")
y <- c("a","d","f")
y %in% x

### %in% example 2
c("Boston", "Dakota", "Washington") %in% murders$state

####Assessment on Data Camp.

##Basic Data Wrangling-------------------------------------
install.packages("dplyr")
library("dplyr")

####Dplyr adds easy to remember Data Wrangling Functions
####Namely, mutate - Change data table by adding new column 
####                 or changing existing columns.
#### filter - filter the data by subsetting rows.
#### select - to subset the data by selecting specific 
####          columns.
#### %>% - the pipe

### mutate
murders <- mutate(murders, rate=(total/population)*100000)

head(murders)

### filter
filter(murders, rate<=0.7)

### select

new_table <- select(murders,state,region,rate)
filter(new_table, rate <= 0.7)

### %>%
### Pipe is gonna perform the above job.

murders %>% select(state,region,rate) %>% filter(rate <=0.7)
###This pipe is basically equivalent to...
new_table <- select(murders,state,region,rate)
filter(new_table, rate<=0.7)

## creating data frames

grades <- data.frame(name=c("John", "Juan", "Jean", "Yao"), 
                     exam_1=c(95,80,90,85),
                     exam_2=c(90,95,85,90))

grades
class(grades$name) ### if the class of strings is factor and
                   ### not characters then.

grades <- data.frame(name=c("John", "Juan", "Jean", "Yao"), 
                     exam_1=c(95,80,90,85),
                     exam_2=c(90,95,85,90),
                     stringsAsFactors = FALSE)
####Assessment on Data Camp.

##Basic Plots----------------------------------------------
###Scatter Plots example
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total

plot(population_in_millions,total_gun_murders)

###Histogram example
hist(murders$rate)
#### which is the murder at 15?
murders$state[which.max(murders$rate)]

###Boxplot example
boxplot(rate~region, data=murders)



####Assessment on Data Camp.

##Section 3 Assessment-------------------------------------
library("dslabs")
data("heights")
options(digits = 3) #report 3 significant digits for all 
                    #answers.

###Question 1


#First, determine the average height in this dataset. Then 
#create a logical vector ind with the indices for those 
#individuals who are above average height.

heights
ind <- heights$height > mean(heights$height)
sum(ind)

filter(heights, sex=="Female" & height>mean(heights$height))
mean(heights$sex=="Female")

min(heights$height)
match(min(heights$height), heights$height)
heights$sex[1032]

max(heights$height)

x<-c(min(heights$height):max(heights$height))
x

ind <- sum(!x %in% heights$height)

ind
!x %in% heights$height


heights2<- heights%>%mutate(ht_cm=height*2.54)

heights2

heights2$ht_cm[18]

mean(heights2$ht_cm)

females = filter(heights2, sex=="Female")

str(females)

mean(females$ht_cm)


library(dslabs)
data(olive)
head(olive)

palmitic <- olive$palmitic
palmitoleic <- olive$palmitoleic
stearic <- olive$stearic
oleic <- olive$oleic
linoleic <- olive$linoleic
linolenic <- olive$linolenic
arachidic <- olive$arachidic
eicosenoic <- olive$eicosenoic

plot(palmitic,palmitoleic)


hist(eicosenoic)

boxplot(palmitic~region, data=olive)

###End of Section------------------------------------------

#Section 4: Programming Basics-----------------------------
##For Loops------------------------------------------------
###Arithematic Addition
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
} ###Function defined for n values. x is a series of 
###integers from 1 to n and y is equal sum of this series.
compute_s_n(15)
###Loops
### For Loop - define the range that the variable's function
###is executed.

### General form of For Loop - for(i in range of values) {
### operations that use i, which is changing across the 
### range of values
### }

###For Loop example
for (i in 1:5) {
  print(i)
}

###For Loop example
m <- 25
#### create an empty vector
s_n <- vector(length=m)
for (i in 1:m) {
    s_n[i] <- compute_s_n(i)
}
s_n

i<- 1:m

plot(i,s_n)
lines(i, (i*(i+1))/2)

### other functions
### Functions more readily used in place of FOR loops
### - apply, sapply, tapply, mapply - Apply Family
### split, cut, quantile, reduce, identical, unique are some
### other functions as well.

##Section 4 Assessment-------------------------------------
library(dslabs)
data(heights)

sum(ifelse(heights$sex=="Male",2,1))
mean(ifelse(heights$height>72,heights$height,0))

inches_to_ft <- function(n){
  n<- n/12
  n
}
sum(inches_to_ft(heights$height)<5)


# define a vector of length m

m <- 10
f_n <- vector(length = m)
for (n in 1:m) {
    f_n[n] <- factorial(n)
}

# inspect f_n
f_n
f_n(2)

###End of Section------------------------------------------