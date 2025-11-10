library(haven)
library(mosaic)
library(ggplot2)
library(readxl)
library(gt) #for making tables
library(gtExtras)
library(dplyr)

setwd("C:/R/DevEconMidterm")
tanzania <- read_excel("Tanzania.xlsx")

headcount = sum(tanzania$`Number of people in household`)

headcountIndexFunction <- function(expenditure){
  #Tests if adjusted expenditure is at or below poverty line
  #assigns 1 if yes, 0 for above
  tanzania$poverty_ex <- ifelse(expenditure <= 1.90, 1, 0) 
  
  #multiplies headcount per household by 1 if poor, or 0 if not. Giving us number of people below
  tanzania$poorHeadcount <- tanzania$poverty_ex * (tanzania$`Number of people in household`)
  
  headcountIndex = sum(tanzania$poorHeadcount) / sum(tanzania$`Number of people in household`)
  return(headcountIndex)
}

#Copied straight from practise might need tweaking
povertyGapIndexfunction <- function(expenditure){
  #Tests if expenditure is at or below poverty line
  #assigns 1 if yes, 0 for above
  tanzania$poverty_ex <- ifelse(expenditure <= 1.90, 1, 0) 
  #Creates new column of incomes in households but is 0 if above poverty line.
  tanzania$poorExpenditure <- expenditure * tanzania$poverty_ex
  #print(tanzania$poorExpenditure)
  
  #Creates new vector without 0s in
  nonZeroExpenditure <- tanzania$poorExpenditure[tanzania$poorExpenditure != 0]
  #print(nonZeroExpenditure)
  povertyDiff <- (1.90-nonZeroExpenditure)
  print(povertyDiff)
  povertyGaps <- povertyDiff/1.90
  povertyGapIndex <- sum(povertyGaps)*(1/headcount)
  return(povertyGapIndex)
  
}

#'* a) per capita expenditure*
#puts per capita expenditure value from excel into function
headcountIndex_percapita <- headcountIndexFunction(tanzania$`Per capita expenditure, daily`)
print(headcountIndex_percapita)

povertyGapIndex_percapita <- povertyGapIndexfunction(tanzania$`Per capita expenditure, daily`)
print(povertyGapIndex_percapita)

#'* b) Square-root economies of scale (no adult equivalence) a*
#multiplying householdExpenditure per capita by number in household then dividing by n^0.5
tanzania$householdExpenditure <- tanzania$`Number of people in household` * tanzania$`Per capita expenditure, daily`
tanzania$expenditure_sqr <- tanzania$householdExpenditure / ((tanzania$`Number of people in household`)^0.5)
print(tanzania$expenditure_sqr)
headcountIndex_sqr <- headcountIndexFunction(tanzania$expenditure_sqr)
print(headcountIndex_sqr)
#2% seems low

povertyGapIndex_sqr <- povertyGapIndexfunction(tanzania$expenditure_sqr)
print(povertyGapIndex_sqr)
#0.0010 seems low

#'* c) An adult equivalence scale (no economies of scale), setting children 14 years and younger equal to onehalf of adults. *
#Find adults per household by subtracting kids. Calculate adults + 0.5*kids.
#Divide household expenditure by that value for adjusted expenditure
tanzania$adultPopulation <- tanzania$`Number of people in household` - tanzania$`Population between 0-14 years old`
tanzania$headcount_equivalence <- tanzania$adultPopulation + (0.5*tanzania$`Population between 0-14 years old`)
tanzania$expenditure_equivalence <- tanzania$householdExpenditure / tanzania$headcount_equivalence
headcountIndex_equivalence <- headcountIndexFunction(tanzania$expenditure_equivalence)
print(headcountIndex_equivalence)
#21% seems roughly correct so maybe not

povertyGapIndex_equivalence <- povertyGapIndexfunction(tanzania$expenditure_equivalence)
print(povertyGapIndex_equivalence)
#0.00816 also seems too low

#Creates Table
#round to 3 decimal places for display
allHeadcountIndexs <- round(c(headcountIndex_percapita,headcountIndex_sqr,headcountIndex_equivalence),3)
allPovertyGapIndexs <- round(c(povertyGapIndex_percapita,povertyGapIndex_sqr,povertyGapIndex_equivalence),3)
indexTable <- data.frame(allHeadcountIndexs,allPovertyGapIndexs)
rownames(indexTable) <- c('Per capita expenditure','Square root economies of scale','Adult equivalence Scale')
colnames(indexTable) = c('Headcount Index','Poverty Gap Index')
gtindexTable <- gt(indexTable,rownames_to_stub = T)
gt_theme_guardian(tab_header(gtindexTable, title = 'Measures of poverty with adjusted household expenditures', subtitle = NULL, preheader = NULL))

