install.packages("readr")
Needed <- c("DescTools","choroplethrMaps","choroplethr",
"tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(Needed, dependencies=TRUE)   

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")   
install.packages("tm",dependencies =T)
library(tm)  

library(choroplethr)
library(choroplethrMaps)
library(DescTools)
library(dplyr)
library(ggplot2)
library(readr)
library(zoo)

## Change Char data to numeric data
load("NewLoan.Rdata")
old=loan_raw

c=loan_raw$int_rate
for(j in seq(c))   
{   
  c[[j]] <- gsub("%"," ", c[[j]])   
}  
c=as.numeric(c)
loan_raw$int_rate=c

loan_raw$installment = as.numeric(loan_raw$installment)
loan_raw$termAsFactor=as.factor(loan_raw$term)
loan_raw <- select(loan_raw, -term)


loan_raw$gradeASFactor = mapvalues(loan_raw$grade, from = c("A","B","C","D","E","F","G"), to = c(1,2,3,4,5,6,7))
loan_raw$gradeASFactor = as.numeric(loan_raw$gradeASFactor)
loan_raw$gradeASFactor = as.factor(loan_raw$gradeASFactor)
loan_raw <- select(loan_raw, -grade)

loan_raw$emp_lengthAsNum = mapvalues(loan_raw$emp_length, from = c("10+ years","< 1 year","1 year",
                                                                   "3 years","8 years","9 years","4 years",
                                                                   "5 years","6 years","2 years","7 years",
                                                                   "n/a"), to = c(10,0,1,3,8,9,4,5,6,2,7,NA))

loan_raw$emp_lengthAsNum = as.numeric(loan_raw$emp_lengthAsNum)
loan_raw <- select(loan_raw, -emp_length)


loan_raw$home_ownershipIfOwn = mapvalues(loan_raw$home_ownership , from = c("RENT","OWN","MORTGAGE","OTHER","NONE","ANY"),
                                         to = c(0,1,1,0,0,0) )

loan_raw$home_ownershipIfOwn = as.numeric(loan_raw$home_ownershipIfOwn)
loan_raw$home_ownershipIfOwn =  (loan_raw$home_ownershipIfOwn-1)*(-1)
loan_raw <- select(loan_raw, - home_ownership)

loan_raw$annual_inc = as.numeric(loan_raw$annual_inc)

loan_raw$verification_statusAsNum = mapvalues(loan_raw$verification_status , from = c("Verified","Source Verified",
                                                                                      "Not Verified"), to = c(0,0,1) )

loan_raw$verification_statusAsNum = as.numeric(loan_raw$verification_statusAsNum)
loan_raw <- select(loan_raw, -verification_status)

## 1 as default and 0 as not default, becasue we need to know who's going to default
loan_raw$loan_statusASNum = mapvalues(loan_raw$loan_status , from = c("Fully Paid","Charged Off","In Grace Period","Current","Late (31-120 days)",
                                                                 "Late (16-30 days)","Default"), to = c(0,1,0,0,1,1,1) )
loan_raw <- select(loan_raw, - loan_status)

loan_raw$loan_statusASNum=as.numeric(loan_raw$loan_statusASNum)
loan_raw$dti <- as.numeric(loan_raw$dti)
loan_raw$total_acc = as.numeric(loan_raw$total_acc)
loan_raw$delinq_amnt=as.numeric(loan_raw$delinq_amnt)
loan_raw$chargeoff_within_12_mths = as.numeric(loan_raw$chargeoff_within_12_mths)
loan_raw$acc_now_delinq=as.numeric(loan_raw$acc_now_delinq)
loan_raw$application_type=as.factor(loan_raw$application_type)
loan_raw$policy_code=as.numeric(loan_raw$policy_code)
loan_raw$last_pymnt_amnt=as.numeric(loan_raw$last_pymnt_amnt)
loan_raw$collection_recovery_fee=as.numeric(loan_raw$collection_recovery_fee)
loan_raw$recoveries = as.numeric(loan_raw$recoveries)
loan_raw$total_rec_late_fee = as.numeric(loan_raw$total_rec_late_fee)
loan_raw$total_rec_int = as.numeric(loan_raw$total_rec_int)
loan_raw$total_rec_prncp= as.numeric(loan_raw$total_rec_prncp)
loan_raw$total_pymnt_inv= as.numeric(loan_raw$total_pymnt_inv)
loan_raw$total_pymnt = as.numeric(loan_raw$total_pymnt)
loan_raw$out_prncp_inv = as.numeric(loan_raw$out_prncp_inv)
loan_raw$out_prncp= as.numeric(loan_raw$out_prncp)
loan_raw$initial_list_status=as.factor(loan_raw$initial_list_status)
loan_raw$revol_bal  = as.numeric(loan_raw$revol_bal )
loan_raw$pub_rec = as.numeric(loan_raw$pub_rec)
loan_raw$open_acc = as.numeric(loan_raw$open_acc )
loan_raw$mths_since_last_record= as.numeric(loan_raw$mths_since_last_record)
## empty meaning what?loan_raw$mths_since_last_delinq 
loan_raw$inq_last_6mths = as.numeric(loan_raw$inq_last_6mths )
loan_rawinq_last_6mths = as.numeric(loan_raw$inq_last_6mths)
loan_raw$pymnt_plan=as.factor(loan_raw$pymnt_plan)


loan_raw$earliest_cr_line <- as.yearmon(loan_raw$earliest_cr_line, format = '%b-%Y')
loan_raw$earliest_cr_lineMonth <- format(loan_raw$earliest_cr_line, '%m')
loan_raw$earliest_cr_lineYear <- format(loan_raw$earliest_cr_line, '%Y')
loan_raw <- select(loan_raw, - earliest_cr_line)

loan_raw$last_pymnt_d <- as.yearmon(loan_raw$last_pymnt_d, format = '%b-%Y')
loan_raw$last_pymnt_dMonth <- format(loan_raw$last_pymnt_d, '%m')
loan_raw$last_pymnt_dYear <- format(loan_raw$last_pymnt_d, '%Y')
loan_raw <- select(loan_raw, - last_pymnt_d)

loan_raw$last_credit_pull_d <- as.yearmon(loan_raw$last_credit_pull_d, format = '%b-%Y')
loan_raw$last_credit_pull_dMonth <- format(loan_raw$last_credit_pull_d, '%m')
loan_raw$last_credit_pull_dYear <- format(loan_raw$last_credit_pull_d, '%Y')
loan_raw <- select(loan_raw, - last_credit_pull_d)


rm(c)
c=loan_raw$revol_util
for(j in seq(c))   
{   
  c[[j]] <- gsub("%"," ", c[[j]])   
}  
c=as.numeric(c)
loan_raw$revol_util=c
rm(c)


str(loan_raw)
loanNew=Filter(is.numeric,loan_raw)
loanNewFactor=Filter(is.factor,loan_raw)
loanNewNUM=cbind(loanNew,loanNewFactor)
dim(loanNewNUM)
na_count <-sapply(loanNewNUM, function(y) sum(length(which(is.na(y)))))
na_count=data.frame(na_count)
na_count

#too many NA FROM mths_since_last_record ,DELETE IT
loanNewNUM=select(loanNewNUM,-mths_since_last_record )

LoanNoNA = loanNewNUM %>%
  na.omit()
dim(LoanNoNA)
## Create and save subset of rejection data
set.seed(10)
loan_sample <- sample_n(LoanNoNA, size = 200000)
save(loan_sample, file = 'loan_sample.RData')







