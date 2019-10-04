
library("readxl")
DrinkDeathD <- read_excel("C:/Users/Yu Chen Su/Documents/UIUC/2019 Fall/BADM 576 Data Science and Analytics/Homework/HW3/DrinkDeath.xlsx")

#C:\Users\Yu Chen Su\Documents\UIUC\2019 Fall\BADM 576 Data Science and Analytics\Homework\HW3

DrinkDeathD <- lm(B ~ A1 + A2 +A3 , data = DrinkDeath)

summary(DrinkDeathD)
confint(DrinkDeathD)

# p-value < 0.05 : significant
DrinkDeath0 <- lm(B ~ A0, data = DrinkDeath)
summary(DrinkDeath0)
