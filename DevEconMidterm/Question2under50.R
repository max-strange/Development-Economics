library(haven)
library(mosaic)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggmagnify)
setwd("C:/R/DevEconMidterm")
tanzania <- read_excel("Tanzania.xlsx")

# Create a vector of numbers from 0 to 50 by increments of one cent
zvalues <- seq(0, 50, by = 0.01)

#Create separate vectors for rural urban household sizes and expenditure, run both through algorithm for y values 
expenditure_urban <- subset(tanzania$`Per capita expenditure, daily`,tanzania$`Rural/urban` == "Urban" & tanzania$`Per capita expenditure, daily` <= 50)
householdSize_urban <- subset(tanzania$`Number of people in household`,tanzania$`Rural/urban` == "Urban" & tanzania$`Per capita expenditure, daily` <= 50)

expenditure_rural <- subset(tanzania$`Per capita expenditure, daily`,tanzania$`Rural/urban` == "Rural" & tanzania$`Per capita expenditure, daily` <= 50)
householdSize_rural <- subset(tanzania$`Number of people in household`,tanzania$`Rural/urban` == "Rural" & tanzania$`Per capita expenditure, daily` <= 50)

yvalues_urban <- c()
urbanPoors <-c()
cumulativePoors_urban <-c()

#for each z value (can do by each cent but not necessary) between 0 and 50 inclusive
#work out how many household are poor, multiply by household size then divide by sum of householdsizes
#calculates what percentage of rural or urban households they make up
#append this number to end of a vector of yvalues for each iteration
for (z in zvalues){
  print(z)
  povertyBool_urban <- ifelse(expenditure_urban <= z, 1,0)
  urbanPoors <- povertyBool_urban * householdSize_urban
  cumulativePoors_urban <- append(cumulativePoors_urban,sum(urbanPoors))
  yvalues_urban <- cumulativePoors_urban / sum(householdSize_urban)
  print(yvalues_urban)
}

yvalues_rural <- c()
ruralPoors <-c()
cumulativePoors_rural <-c()
for (z in zvalues){
  povertyBool_rural <- ifelse(expenditure_rural <= z, 1,0)
  ruralPoors <- povertyBool_rural * householdSize_rural
  cumulativePoors_rural <- append(cumulativePoors_rural,sum(ruralPoors))
  yvalues_rural <- cumulativePoors_rural / sum(householdSize_rural)
}

summary(expenditure_urban)
summary(expenditure_rural)

from = c(20,50,0.98,1.02)
to = c(20,50,0.2,0.7)
povertyIncidenceCurves <- data.frame(zvalues,yvalues_urban,yvalues_rural)
print(povertyIncidenceCurves)
tail(povertyIncidenceCurves,700)
ggplot(povertyIncidenceCurves,aes(x=zvalues)) +
  geom_line(aes(y=yvalues_rural,colour='Rural')) +
  geom_line(aes(y=yvalues_urban,colour='Urban')) +
  ylab('Headcount Index')+
  xlab('Daily per capita expenditure') +
  labs(title='Poverty Incidence Curves for Rural and Urban Tanzania') +
  theme_classic() +
  geom_magnify(from = from, to = to,shadow = TRUE) +
  geom_vline(xintercept=45.8,linetype=3) +
  theme(legend.title=element_blank())

robustness <- ifelse(yvalues_urban >= yvalues_rural,1,0)
print(robustness)
tail(robustness,1000)
print(zvalues)
table(robustness)


#No data for expenditure below 0.24. Urban has lower rates of poverty for every value of z
#Therefore robust comparison
#Perhaps mention higher cost of living in urban areas meaning they are not necessarily better off?
#However when only comparing households with under 50, i.e without outliers, they intersect at around 45
#Only due to highest urban value in data set being higher than rural value but lower than 50