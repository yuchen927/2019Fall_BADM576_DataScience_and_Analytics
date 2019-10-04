

# install.packages('readr')
library(readr)

test_salaries <- read_csv("C:/Users/Yu Chen Su/Documents/UIUC/2019 Fall/BADM 576 Data Science and Analytics/Homework/HW3/test_salaries.csv")

# C:\Users\Yu Chen Su\Documents\UIUC\2019 Fall\BADM 576 Data Science and Analytics\Homework\HW3

salary <- test_salaries$Salary/1000 #Change units to thousands

mean(salary)
sd(salary)

hist(salary,breaks = 25)




#Simple Monte Carlo

n<-10
draws<-100
xmean<-c(rep(NA,n))
for (i in 1:n){
  x<-rnorm(draws)
  xmean[i]<-mean(x)
}

mean(xmean)
sd(xmean)

#Hockey salaries

library(readr)
test_salaries <- read_csv("C:/Users/Yu Chen Su/Documents/UIUC/2019 Fall/BADM 576 Data Science and Analytics/Homework/HW3/test_salaries.csv")

salary <- test_salaries$Salary/1000 #Change units to thousands
mean(salary)
sd(salary)

hist(salary,breaks = 25)

#Bootstrap

n<-200
draws<-100
sample.mean<-c(rep(NA,n))

for (i in 1:n){
  
  x<-sample(1:length(salary),draws,replace=T)
  sample.mean[i]<-mean(salary[x])
}

mean(sample.mean)
sd(sample.mean)

