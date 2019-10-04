x<- rnorm(10)
y<-rnorm(10)
?rnorm
t.test(x,y)
t.test(x,y)


x<-rnorm(10,5,2)
y<-rnorm(10,4,1)
t.test(x,y)


x<-rnorm(1000,5,2)
y<-rnorm(1000,4,1)
t.test(x,y)


fit <- lm(B ~ A0, data=my_data)


x1 <- runif(10,min = 1, max = 6)
?runif
as.integer(x1)  # turn decimals to integer

DieRoll <- sample (1:6,12, replace=T)
DieRoll
EqualThree <- DieRoll == 3
EqualThree
sum(EqualThree)
