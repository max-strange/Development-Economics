library(ggplot2)
library(readxl)
library("report")
setwd("C:/R/DevEconMidterm")
tanzania <- read_excel("Tanzania.xlsx")

#Question 5. As income goes up, share spent on food goes down
#Plot income to food expenditure share. Which can be calculated by food expenditure/total expenditure
#Scatter plot with fitted line. R function 

tanzania$percentFoodExpenditure <- (tanzania$`Per capita food expenditure, daily` / tanzania$`Per capita expenditure, daily`) * 100

cor.test(log(tanzania$`Per capita expenditure, daily`),tanzania$percentFoodExpenditure)
report(lm(log(tanzania$`Per capita expenditure, daily`)~tanzania$percentFoodExpenditure))

ggplot(tanzania, aes(x = log(`Per capita expenditure, daily`),y= percentFoodExpenditure)) + 
  geom_point(alpha = 0.05) +
  geom_smooth(method=lm) +
  coord_cartesian(ylim = c(0, 100)) +  
  ylab('Percentage of income spent on food')+
  xlab('Log of daily per-capita expenditure') +
  theme_classic() 

#Moderately correlated, Engels law holds largely true