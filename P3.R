

#install.packages('memisc')
#install.packages('RCurl')
#install.packages('bitops')

library(ggplot2)
library(gridExtra)
library(plyr)
library(alr3)
library(tidyr)
library(dplyr )
library(reshape)
library(lubridate )
library(reshape2)
library(GGally)
library(car)
library(MASS)

library(scales)

library(memisc)

library('RCurl')
library(bitops)

ploan <- read.csv ("C:/Nano.degreee/3.EDA_Using_R/P3/prosperLoanData.CSV")

str(ploan)

hist(ploan$Term)
histogram(ploan$Term)

table(ploan$Term)
/*
  12    36    60 
1614 87778 24545 
*/
  
table(ploan$LoanStatus)
table(ploan$BorrowerAPR)

plot(ploan$BorrowerAPR)


ggplot(data = ploan, aes(x=Term, y=LoanStatus)) + 
  geom_point(alpha = 1/100, aes(color=BorrowerAPR))

ggplot(data = ploan, aes(x=Term, y=LoanStatus)) + 
  geom_point(aes(color=BorrowerAPR))

ggplot(data = ploan, aes(x=Term, y=LoanStatus)) + 
  geom_point(aes(color=BorrowerAPR))


#################################################
## State ..... BorrowerState
#################################################



plot(ploan$BorrowerState)

sort(table(ploan$BorrowerState))

# Borrower state 

ggplot(ploan, aes(x=ploan$BorrowerState)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = 1) + 
  scale_y_continuous(breaks = c(0.05, 0.1, 0.15)) +
  ylab ("Percentage") +
  xlab ("Borrower State")



### APR by state ...
ggplot(data = ploan, aes(x=BorrowerState , y=BorrowerAPR)) + 
  geom_point(stat="summary", fun.y="median") +
  geom_smooth()


### Type of loan i,e ListingCategory


/*
  0 - Not Available, 1 - Debt Consolidation, 
2 - Home Improvement, 3 - Business, 
4 - Personal Loan, 5 - Student Use, 
6 - Auto, 7- Other, 8 - Baby&Adoption, 
9 - Boat, 10 - Cosmetic Procedure, 
11 - Engagement Ring, 12 - Green Loans, 
13 - Household Expenses, 14 - Large Purchases, 
15 - Medical/Dental, 16 - Motorcycle, 
17 - RV, 18 - Taxes, 19 - Vacation, 
20 - Wedding Loans
*/
  

/* Majority of them are Debt Consolidation loans */

ggplot(ploan, aes( x = ploan$ListingCategory)) + 
  geom_bar(binwidth = 1)


/*** APR is high for 10 - Cosmetic Procedure - 0.275 *******/
/*** APR is high for 13 - Household Expenses next to cosmetci prodecure */
/*** APR is lowest for 4 - Personal Loan */


ggplot(ploan, aes(x=ploan$ListingCategory)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = 1) + 
  scale_y_continuous(breaks = c(0.1,0.25,0.5)) +
  ylab ("Percentage")


### APR by Loan Type...

ggplot(data = ploan, aes(x= ploan$ListingCategory, y=BorrowerAPR)) + 
  geom_point(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab (" Loan Category") + 
  geom_smooth()


  
#### Employement duration .. 


ploan$EmploymentStatusDuration_Y = ploan$EmploymentStatusDuration/12

histogram(ploan$EmploymentStatusDuration_Y)

plot(ploan$EmploymentStatusDuration_Y, ploan$BorrowerAPR )

### Employement duration doesn't seems to be corrilated to APR

ploan$EmploymentStatusDuration_Y = ploan$EmploymentStatusDuration/12

ggplot(data = ploan, aes(x= ploan$EmploymentStatusDuration_Y, y=BorrowerAPR)) + 
  geom_point(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab ("Employement duration") +
  geom_smooth()


### INCOME to APR .. 

str(ploan)
table(ploan$IncomeRange)

### Order the income range 
ploan$IncomeRange <- factor(ploan$IncomeRange, 
                            levels = c("Not displayed","$0","Not employed", 
                                       "$1-24,999","$25,000-49,999",
                                       "$50,000-74,999","$75,000-99,999",
                                       "$100,000+") )


histogram(ploan$IncomeRange, xlab = "Income Range")


#### NICE CORRILATION BETWEEN INCOME TO APR 

ggplot(data = ploan, aes(x= ploan$IncomeRange, y=BorrowerAPR)) + 
  geom_point(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab ("Income Range") 



#### Homeowner to APR 
### Home owner does have slightly lower APR..
ggplot(data = ploan, aes(x= ploan$IsBorrowerHomeowner, y=BorrowerAPR)) + 
  geom_histogram(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab ("Homeowner indicator") 


### Does homeowner have relevance to type of loan ???
####  8 - Baby&Adoption, and 17 - RV Loas are high for the non-homeowers *//

ggplot(data = ploan, aes(x= ploan$ListingCategory,  y=BorrowerAPR)) + 
  geom_histogram(stat="summary", fun.y="count") + 
  ylab(" Mean Borrower APR") + xlab ("Loan Category")  +
  facet_wrap (~ IsBorrowerHomeowner)

ggplot(data = ploan, aes(x= ploan$ListingCategory,  y=BorrowerAPR)) + 
  geom_histogram(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab ("Loan Category")  +
  facet_wrap (~ IsBorrowerHomeowner)


##############################################################
##################  ProsperScore #############################
##############################################################


### As expected .. higher risk leads to high APR .

ggplot(data = ploan, aes(x= ploan$ProsperScore, y=BorrowerAPR)) + 
  geom_histogram(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab ("Prosper Score") 

## Credit CreditScoreRangeLower

ggplot(data = ploan, aes(x= ploan$CreditScoreRangeLower, y=BorrowerAPR)) + 
  geom_histogram(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab ("Credit Range Lower") 

## Credit CreditScoreRangeUpper


ggplot(data = ploan, aes(x= ploan$CreditScoreRangeUpper, y=BorrowerAPR)) + 
  geom_histogram(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab ("Credit Range Higher") 


##### OpenCreditLines  - No good corrilation .. 


ggplot(data = ploan, aes(x= ploan$OpenCreditLines, y=BorrowerAPR)) + 
  geom_histogram(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab ("Open Credit Lines") 



##### 
##### 
##### TotalCreditLinespast7years -  no effect seems , 


ggplot(data = ploan, aes(x= ploan$TotalCreditLinespast7years, y=BorrowerAPR)) + 
  geom_histogram(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab ("Total Credit Line past 7 years") 


#### AmountDelinquent

ploan$AmountDelinquent_ind <- ploan$AmountDelinquent >0

table(ploan$AmountDelinquent>0)

histogram(ploan[ploan$AmountDelinquent>1 & ploan$AmountDelinquent< 60000,]$AmountDelinquent)


#### AS EXPECTED DELIQUENT AMOUNT HAVE IMPACT ON APR .. 
ggplot(data = ploan, aes(x= ploan$AmountDelinquent_ind, y=BorrowerAPR)) + 
  geom_histogram(stat="summary", fun.y="median") + 
  ylab(" Mean Borrower APR") + xlab ("Delinquent Status") 




####### Buildig model to predict ProsperScore.. 


m1 <- lm( ProsperScore ~ CreditScoreRangeLower, data = ploan)
m2 <- update ( m1,  ~ . + OpenCreditLines)
m3 <- update (m2, ~. + AmountDelinquent)
m4 <- update (m3, ~. + IsBorrowerHomeowner)
m5 <- update (m4, ~ . + IncomeRange) 
m6 <- update ( m5, ~. + EmploymentStatusDuration)
m7 <- update ( m6, ~ . + ploan$ListingCategory)
m8 <- update (m7, ~ . + DebtToIncomeRatio)
m9 <- update (m8, ~. + BorrowerState)

mtable(m1, m8, m9)

