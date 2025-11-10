library(gt)
library(gtsummary)
library(readxl)
library(gtExtras)
library(dplyr)
setwd("C:/R/DevEconMidterm")
tanzania <- read_excel("Tanzania.xlsx")
#Question 4 have to turn marital status and other word data to numerical
#Create variable divorced, 1 for divorced, 0 for no
#Create variable married, 1 for married, 0 for no
#: head education, head age, head marital status, household size, share of household under 14. 

summary(sapply(tanzania, is.na))
#One blank case under education, and one under marital status
#na.omit to remove those rows

tanzania$childShare <- tanzania$`Population between 0-14 years old` / tanzania$`Number of people in household`
tanzania$schoolBool <- ifelse(tanzania$`Ever attended school of Household head` == "Yes", 1, 0)

summary(tanzania)
table(tanzania$`Marital status of Household head`)

#can group marital status'
tanzania$isMarried <- ifelse(tanzania$`Marital status of Household head`%in% c("Married monogamous",'Married polygamous','Living together'), 1, 0) #monogamous, polygamous or living together
print(tanzania$isMarried)
tanzania$isSeparated <-ifelse(tanzania$`Marital status of Household head`%in% c("Divorced/Separated","Widowed"), 1, 0)#group divorced and widowed
tanzania$isSingle <- ifelse(tanzania$`Marital status of Household head`== "Never married", 1, 0)
tanzaniaComplete <- na.omit(tanzania)
#Should be 10184
sum(tanzaniaComplete$isSeparated) + sum(tanzaniaComplete$isMarried) + sum(tanzaniaComplete$isSingle) #Check it adds up to correct number of households

summary(tanzaniaComplete)
table(tanzaniaComplete$`Ever attended school of Household head`)
expenditureRegression <- lm(`Per capita expenditure, daily` ~ schoolBool + `Age of Household head` + isMarried + isSeparated  + `Number of people in household` + childShare, data = tanzaniaComplete)
coefficients(expenditureRegression)
#One value returning as NA between the marital status, multicolinearity problem? Too closely related variables
#Going to group them further, add never married to isSeparated?. Still obviously colinear as one is now the inverse of the otehr
#Dummy variable trap, drop one as a reference value

#can drop single, this is the reference value. Seems being married and seperated is far worse for expenditure
##Age of household head has fairly small effect but not neglible 
#School has very large positive effect
#Number of people in house have reasonable effect
#More children has also a large effect.

#Create professional regression table
# When discussing, explain why this makes sense from a Dev Econ persepctive
#150 words per question

#Creates model into regression table then converts it to gt table
#Can be saved in html
summary(expenditureRegression)
tbl_regression(expenditureRegression) %>%
  as_gt() %>%
  tab_header(title = 'Regression results for log household per-capita expenditure') %>%
  gt_theme_guardian()

#Table can be saved as html file and edited there
