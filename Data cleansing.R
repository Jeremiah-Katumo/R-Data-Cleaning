rm(list=ls())
setwd("C:/Users/hp/Desktop/Projects")
myproject = read.csv("test.csv", header = TRUE)
myproject
head(myproject)
library(ggplot2)
ggplot(data=myproject, aes(x=Fare, y=Ticket)) + geom_smooth(method = "lm", span=1) + geom_point() + xlab("Fare") + ylab("Ticket")
myproject[!complete.cases(myproject),]
nrow(myproject[!complete.cases(myproject),])

# detecting NAs in the dataset
detect_nas <- is.na(myproject)
detect_nas

# detecting NULLs in the dataset
detect_Nulls <- is.null(myproject)
detect_Nulls

# checking for infinity values: try the following example
pi/0
2*Inf
Inf - 1e+10
Inf + Inf
3< -Inf
Inf == Inf

# detecting NaNs in the dataset
detect_NaNs <- is.nan(myproject)
detect_NaNs

# removing the 87 rows from above code
myproject <- na.omit(myproject)
myproject

# default encoding used by your OS can be requested by typing
Sys.getlocale("LC_CTYPE")

# consistent data
# missing values
age=c(23,16,NA)
mean(age)
mean(age, na.rm = TRUE)

# detecting rows in a data.frame that do not contain any missing values using complete.cases() function
print(person)
person <- data.frame(age=c(21,42,18,21), height=c(6.0,5.9,5.7,NA))
person
complete.cases(person)
persons_complete <- na.omit(person) # 
na.action(persons_complete)

# special values
# The function is.finite determines which values are 'regular' values
is.finite(c(1, Inf, NaN, NA))
is.special <- function(x){
  if(is.numeric(x), !is.finite(x) else is.na(x))
}
sapply(person, is.special)

# outliers
# listing outliers with function boxplot.stats
x <- c(1:10, 20, 30)
x
boxplot.stats(x)$out
boxplot.stats(x, coef = 2)$out

# obvious inconsistencies
x_nonnegative <- x>=0
x_nonnegative
ppeople <- data.frame(age=c(21,2,18,221,34),
                      agegroup=c("adult","child","adult","elderly","child"),
                      height=c(6.0,3,5.7,5,-7),
                      status=c("single","married","married","widowed","married"),
                      yearsmarried=c(-1,0,20,2,3))
ppeople
library(editrules)
library(igraph)
E <- editset(c("age>=0","age<=150"))
E
violatedEdits(E, ppeople) # checking data against the rules with violatedEdits function

# 3.2 CORRECTION
# simple transformation rules
marx <- data.frame(name=c("Gaucho", "Zeppo", "Chico", "Gummo", "Harpo"),
                   height=c(170.00,1.74,70.00,168.00,5.91),
                   unit=c("cm","m","inch","cm","ft"))
marx
# convert centimeters
if ( unit == "cm" ){
  height <- height/100
}
# convert inches
if (unit == "inch" ){
  height <- height/39.37
}
# convert feet
if (unit == "ft" ){
  height <- height/3.28
}
# set all units to meter
unit <- "m"
unit

apply(data=myproject,1,function(x),sum(is.na(x)))


