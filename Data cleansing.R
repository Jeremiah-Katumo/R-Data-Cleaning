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

# 3.2.2 Deductive correction
## correctRounding function 
library(deducorrect)
e <- editmatrix("x + y == z")
d <- data.frame(x = 100, y = 101, z = 200)
cor <- correctRounding(e, d)
cor$corrected
cor$corrections
# The function correctSigns is able to detect and repair sign errors
e <- editmatrix("x + y == z")
d <- data.frame(x = 100, y = -100, z = 200)
cor <- correctSigns(e, d)
cor$corrected
cor$corrections
# Function correctTypos is capable of detecting and correcting typographic errors in numbers
e <- editmatrix("x + y == z")
d <- data.frame(x = 123, y = 132, z = 246)
cor <- correctTypos(e, d)
cor$corrected
cor$corrections

## 3.2.3 Deterministic imputation
E <- editmatrix(expression(
  staff + cleaning + housing == total,
  staff >= 0,
  housing >= 0,
  cleaning >= 0
))
dat <- data.frame(
  staff = c(100,100,100),
  housing = c(NA,50,NA),
  cleaning = c(NA,NA,NA),
  total = c(100,180,NA)
)
dat
cor <- deduImpute(E,dat)
cor$corrected
#### similar situations exist for categorical data, which are handled by deduImpute as well
E <- editarray(expression(
  age %in% c("adult","under-aged"),
  driverslicense %in% c(TRUE, FALSE),
  if ( age == "under-aged" ) !driverslicense
))
dat <- data.frame(
  age = NA,
  driverslicense = TRUE
)
dat
cor <- deduImpute(E,dat)
cor$corrected

#### IMPUTATION
# 3.3.1 Basic numeric imputation
library(Hmisc)
x <- impute(x, fun = mean) # mean imputation
x
x <- impute(x, fun = median) # median imputation
x
# linear regression models
data(iris)
iris$Sepal.Length[1:10] <- NA
model <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)
model
I <- is.na(iris$Sepal.Length)
I
iris$Sepal.Length[I] <- predict(model, newdata = iris[I, ])
iris$Sepal.Length[I]
# Hot deck imputation
# random hot-deck imputation
data("women")
head(women)
women
height <- women$height
height[c(6, 9)] <- NA
height
(height <- impute(height, "random"))
# sequential hot-deck imputation
# x : vector to be imputed
# last : value to use if last value of x is empty
seqImpute <- function(x,last){
  n <- length(x)
  x <- c(x,last)
  i <- is.na(x)
  while(any(i)){
    x[i] <- x[which(i) + 1]
    i <- is.na(x)
  }
  x[1:n]
}

# Example of kNN imputation
library(VIM)
data(iris)
n <- nrow(iris)
n
## provide some empty values (10 in each column, randomly)
for (i in 1:ncol(iris)) {
  iris[sample(1:n, 10, replace = FALSE), i] <- NA
}
iris2 <- kNN(iris)
iris2

### Minimal value adjustment
library(editrules)
library(rspa)
E <- editmatrix(expression(x + y == z, x >= 0, y >= 0))
E
d <- data.frame(x = 10, y = 10, z = 21)
d
d1 <- adjustRecords(E, d)
d1
d1$adjusted
violatedEdits(E, d1$adjusted, tol = 0.01)
A <- array(c(x = FALSE, y = FALSE, z = TRUE), dim = c(1, 3))
A