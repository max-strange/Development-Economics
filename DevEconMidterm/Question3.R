library("report")
library(dplyr)
library(readxl)
library(gt) #for making tables
library(gtExtras)
setwd("C:/R/DevEconMidterm")
tanzania <- read_excel("Tanzania.xlsx")
#Uses values loaded from Question1.R
tanzania$poverty_ex <- ifelse(tanzania$`Per capita expenditure, daily`<= 1.90, 1, 0) 
tanzania$householdExpenditure <- tanzania$`Number of people in household` * tanzania$`Per capita expenditure, daily`
tanzania$expenditure_sqr <- tanzania$householdExpenditure / ((tanzania$`Number of people in household`)^0.5)
tanzania$poverty_sqr <- ifelse(tanzania$expenditure_sqr<= 1.90, 1, 0) 

male <- tanzania[which(tanzania$`Sex of Household head`=="Male"),]
female <- tanzania[which(tanzania$`Sex of Household head`=="Female"),]

#Example of report table, #could probably use tidyverse to merge the other t-tests on
t.test(log(male$`Per capita expenditure, daily`), log(female$`Per capita expenditure, daily`))
#pvalues around 7.5% suggesting there isn't a statistically significant difference in log per capita expenditure


boxplot(log(female$`Per capita expenditure, daily`))
summary(male)
summary(female)

#t-test whether a household is poor or not, p-value 6.6% suggesting not significant?
t.test(male$poverty_ex, female$poverty_ex)

#t-test whether a household is poor or not with adjusted expenditure
#p-value 1.455e-06 suggesting it is significant
#Female lead households more likely to be poor adjusting for sqr equivalence
t.test(male$poverty_sqr, female$poverty_sqr)

#p values greater than 0.05 says we cant be confident enough the difference is statistically significant
# then it is likely to be random
# i.e if null hypothesis was true, the chance this data would be found is less than 5%
# Our null hypothes is is there is no statistical link between gender and per capita income

#**All together, combining tables using tidyverse**
question3A <- t.test(log(male$`Per capita expenditure, daily`), log(female$`Per capita expenditure, daily`)) %>%
  report() %>%
  as.data.frame()

question3B <- t.test(male$poverty_ex, female$poverty_ex) %>%
  report() %>%
  as.data.frame()

question3C <- t.test(male$poverty_sqr, female$poverty_sqr) %>%
  report() %>%
  as.data.frame()

question3Table <- rbind(question3A,question3B,question3C) %>% 
  gt() %>%
  tab_header(title = 'T Test Results Comparing Gender Differences in Household Outcomes in Tanzania') %>%
  cols_hide(columns = c('Parameter2','CI','Method','Alternative','Cohens_d','Cohens_d_CI_low','Cohens_d_CI_high')) %>%
  cols_merge(columns = c('CI_low','CI_high')) %>%
  cols_label(
    Parameter1 = "Male/Female Parameters",
    Mean_Parameter1 = "Male mean",
    Mean_Parameter2 = "Female mean",
    CI_low = '95% CI'
  ) %>%
  text_case_match(
    "log(male$`Per capita expenditure, daily`)" ~ "Log per-capita expenditure ",
    "male$poverty_ex" ~ "Below poverty line",
    "male$poverty_sqr" ~ "Below poverty line adjusted for square-root economies of scale",
    "0.000001455" ~ "<0.00001"
  )%>%
  tab_footnote(
    footnote = "CI = Confidence Interval",
    locations = cells_column_labels(columns = CI_low)
  ) %>%
  tab_footnote(
    footnote = "Poverty line set at $1.90"
  ) %>%
  fmt_number(n_sigfig=4) %>%
  gt_theme_guardian()
question3Table
report(t.test(log(male$`Per capita expenditure, daily`), log(female$`Per capita expenditure, daily`)))
report(t.test(male$poverty_ex, female$poverty_ex))
report(t.test(male$poverty_sqr, female$poverty_sqr))

