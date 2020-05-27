
# Please use this source file to complete the lab1 assignment.  
# To submit, commit and push your source file.
#
# There is information in our M&S Bootcamp persistent WebCourse
# about using Git
#   https://webcourses.ucf.edu/courses/1249560
#
# You can use git directly from RStudio if you want!  Here is more
# information about that:
#   https://jennybc.github.io/2014-05-12-ubc/ubc-r/session03_git.html
#
# You are *not permitted* to share your code with others for this 
# assignment.
#

#  vvvvv Put your code down here vvvvvv
#Part 2: Get the Data

#Inspect the data set for the lab (lab1.dat)
#Are the column headers included in the file? --> It was identified that the first row is the headers. For this, it is going to be used the argument "header=TRUE" 
#How are data columns separated? --> In the file, the values of each column have no separation, they are differentiated by commas. Hence, to transform it in a table, the argument that is going to be used is "sep=","
#Are there comments in the data? --> No, there aren't. Either way, it is added the argument "comment.char="#"

#Load that data file into a data frame in R directly from the web.  Pay special attention to the following parameters of read.table
#Explanation: For the lab1, the name of the data is "project". 
#Then, as the file is a text-formatted data, it is used the function "read.table". 
#Moreover, it is used the arguments:
#1. header=true <- as it is shown the names of the variables in the table
#2. sep="," <- the comma is to separate each column
#3. comment.char="" <- to identify the comment (none)
#lab1 is the name of the table for the file
lab1<-read.table("lab1.dat",header = TRUE, sep = ",", comment.char = "#")

#Part 3: Peruse the data

#Print the names of the variables in the data table
colnames(lab1)
#Provide a global summary of the data table
summary(lab1)

#Part 4: Investigate the WineType variable
#List all the values of the WineType variable in the data table
#col1 is the name of the table for the winetype values.
col1 <- lab1['WineType']
#Compute the levels of that factor
levels(as.factor(col1))
#Compute the number of records of each level
table(col1)

# Part 5: Investigate the ColorIntensity variable
#Compute mean of the ColorIntensity variable
#col2 is the name of the table for the colorintensity values
col2<-lab1['ColorIntensity']
colMeans(col2,na.rm = FALSE,dims=1)
#Compute variance of the ColorIntensity variable
var(lab1$ColorIntensity)
#Compute standard deviation of the ColorIntensity variable
sd(lab1$ColorIntensity)
#Compute 95% confidence interval of the ColorIntensity variable
t.test(lab1$ColorIntensity)

# Part 6: Investigate the Hue variable for WineType Class1
# subclass 1 ( values of Hue factor that are classified by class 1 winetype), the list is converted in numbers to calculate the variance, standard deviation and run the test.
subclass1 <- as.numeric(unlist(subset(lab1, lab1$WineType == 'Class1', select='Hue')))
# Compute variance of the Hue variable across only the records with WineType=Class1 (hint: ?subset)
var(subclass1)
# Compute standard deviation of the Hue variable across only the records with WineType=Class1
sd(subclass1)
# Compute 95% confidence interval of the Hue variable across only the records with WineType=Class1
t.test(subclass1)

#Part 7: Perform simple math operations:
# Subtract 120 from 536
a<- 120
b<-536
b-a
# Multiply 3 and 7
c<-3
d<-7
c*d
# Divide 12 by 3
e<-12
f<-3
e/f
# Take the base 2 logarithm of 128
log2(128)
# Compute nine cubed
9^3

#Part 8:Perform math operations on data columns
#Compute the vector resulting from a pair-wise multiplication of all Hue variable values with ColorIntensity values
g<-c(lab1$ColorIntensity)
h<-c(lab1$Hue)
i <- g*h
#Sum the resulting vector from the previous step
sum(i,na.rm = FALSE)

#Part 9: Drawing random numbers
#Draw 10 numbers from the uniform distribution, between 0.0 and 10.0 (hint: ?runif)
j<- runif(10,min = 0,max=10)
#Draw 10 numbers from the Normal distribution, mean of 1.0, standard deviation of 0.3 (hint: ?rnorm)
k<- rnorm(10,mean=1,sd=0.3)
#Compute PDF result for the standard Normal distribution at the value 0.12 (hint: ?dnorm)
dnorm(0.12,mean = 0,sd=1,log = FALSE)
#Compute the probability that a value drawn from the standard normal distribution will be less than 1.96 (hint: ?pnorm)
pnorm(1.96,mean=0,lower.tail = FALSE)

#Part 10: Simulation
#Perform a Monte Carlo sampling simulation of customers arriving at a queue over the period of 10 minutes, assuming this process is governed by the Poisson distribution and that 2 customers arrive per minute on average (hint: ?rpois)
x <- rpois(10,2)
print(x)
#Compute the total number of people standing in line at each minute for one such simulation? (hint: ?cumsum)
cumsum(x)
#Enter the R code below, which defines a function. Call that function using n=10, then again using n=10000, then again using n=10000000. 
#What does the function estimate?
#After running the function with 10, 10000, and 10000000 observations, the estimation is for determining the value of pi (3.14159265359...). So, as there are more observations, the higher the accuracy of the final answer. 

estimate <- function(n) {
  return (4*sum((runif(n)^2 + runif(n)^2) < 1)/n)
}
estimate(10)
estimate(10000)
estimate(10000000)
