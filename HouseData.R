install.packages("readr")
library(readr)

houseprice <- read_csv("C:/Users/Yu Chen Su/Documents/UIUC/2019 Fall/BADM 576 Data Science and Analytics/Chapter/FinalPresentation/Houseprice(1).csv")

Saleprice <- houseprice$SalePrice
OverallQuality <- houseprice$OverallQual
FirstFloor <- houseprice$FlrSF
Bedroom <- houseprice$BedroomAbvGr

Model<-lm(Saleprice ~ OverallQuality + FirstFloor + Bedroom)
summary(Model)

names(Model)

coefficients(Model)
Model$residual



# 1. The shapiro.test() function can be used to check the normality of the residual:

shapiro.test(Model$residual)


# 2. test the independence
require(car)
durbinWatsonTest(Model) 


# 3. test the Homogeneity of Variance
require(car)
ncvTest(Model)



plot(log(Saleprice)~log(OverallQuality))

#par(mfrow=c(3,1))
plot(log(Saleprice)~log(OverallQuality))
plot(log(Saleprice)~log(FirstFloor))
plot(log(Saleprice)~log(Bedroom))


plot(Saleprice~OverallQuality)
plot(Saleprice~FirstFloor)
plot(Saleprice~Bedroom)
# scatter plot
pairs(cbind(Saleprice, OverallQuality, FirstFloor, Bedroom))



# To see if there is multicollinarity
#install.packages ("car", dep=T)
library (car)
M <- lm(houseprice$SalePrice~.,data=houseprice)
vif(M)
vif(Model)

# 0 < VIF < 10 : no multicollinarity
# 10<=VIF<100 : stronger multicollinarity
# VIF >= 100 : strongest multicollinarty


########


summary(Saleprice)
summary(FirstFloor)
summary(Bedroom)
summary(OverallQuality)

range(Saleprice)
range(FirstFloor)
range(Bedroom)
range(OverallQuality)


basicStats(Saleprice)
basicStats(FirstFloor)
basicStats(Bedroom)
basicStats(OverallQuality)

# correlations 
cor( x = Saleprice, y = FirstFloor )
cor( x = Saleprice, y = Bedroom )
cor( x = Saleprice, y = OverallQuality )
cor( x = houseprice )  

install.packages("ggpubr")

library("ggpubr")
ggscatter(houseprice, x = "OverallQual", y = "SalePrice", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")


#### 5.7.5 Interpreting a correlation  
knitr::kable(
  rbind(
    c("-1.0 to -0.9" ,"Very strong", "Negative"),
    c("-0.9 to -0.7", "Strong", "Negative") ,
    c("-0.7 to -0.4", "Moderate", "Negative") ,
    c("-0.4 to -0.2", "Weak", "Negative"),
    c("-0.2 to 0","Negligible", "Negative") ,
    c("0 to 0.2","Negligible", "Positive"),
    c("0.2 to 0.4", "Weak", "Positive"), 
    c("0.4 to 0.7", "Moderate", "Positive"), 
    c("0.7 to 0.9", "Strong", "Positive"), 
    c("0.9 to 1.0", "Very strong", "Positive")), col.names=c("Correlation", "Strength", "Direction"),
  booktabs = TRUE)

# install.packages("corrr")
#library(corrr)
#correlate( houseprice, corr.method="spearman" )


### https://learningstatisticswithr.com/book/graphics.html
### drawing graphs



### http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

# Two sample t-test to test if the two variables are significantly different.


install.packages("ggpubr")

x <- Saleprice
y <- Bedroom
t.test(x, y, alternative = "two.sided", var.equal = FALSE)

# independent 2-group t-test
t.test(x,y) # where x and y are numeric

Saleprice <- houseprice$SalePrice
OverallQuality <- houseprice$OverallQual
FirstFloor <- houseprice$FlrSF
Bedroom <- houseprice$BedroomAbvGr


#cor( x = Saleprice, y = Bedroom )
#cor( x = Saleprice, y = OverallQuality )
#cor( x = houseprice )  