library(data.table)
library(dplyr)
library(zoo)

setwd('C:\\Users\\LENOVO\\Desktop\\Academic\\ADA\\lending_data\\data')

## Process the Loan Data
load('Loan_Raw.RData')
loan_raw <- loan_raw %>%
  select(loan_amnt, issue_d, dti, zip_code, addr_state,
         emp_length, policy_code)
loan_raw$status <- 1
loan_raw$dti <- as.numeric(loan_raw$dti)
loan_raw$issue_d <- as.yearmon(loan_raw$issue_d, format = '%b-%Y')
loan_raw$month <- format(loan_raw$issue_d, '%m')
loan_raw$year <- format(loan_raw$issue_d, '%Y')
loan_raw <- select(loan_raw, -issue_d)

## Process the Rejection Data
load('Reject_Raw.RData')

reject_raw$status <- 0
reject_raw <- reject_raw %>%
  select(-Risk_Score) %>%
  select(-`Loan Title`) %>%
  select(-year)
names(reject_raw) <- c('loan_amnt', 'issue_d', 'dti', 'zip_code', 'addr_state', 
                       'emp_length', 'policy_code', 'status')
reject_raw$dti <- as.numeric(sub("%", "", reject_raw$dti))
reject_raw$issue_d <- as.Date(reject_raw$issue_d)
reject_raw$month <- format(reject_raw$issue_d, '%m')
reject_raw$year <- format(reject_raw$issue_d, '%Y')
reject_raw <- select(reject_raw, -issue_d)

## Create and save full rejection data
reject_full <- bind_rows(reject_raw, loan_raw)
reject_full$loan_amnt <- as.numeric(reject_full$loan_amnt)
reject_full$dti <- ifelse(reject_full$dti == -1, NA, reject_full$dti)
reject_full <- select(reject_full, -policy_code)
for (i in colnames(reject_full)){
  if (is.character(reject_full[[i]])){
    reject_full[[i]] <- as.factor(reject_full[[i]])
  }
}


save(reject_full, file = 'Reject_Full.RData')

## Create and save subset of rejection data
set.seed(10)
reject_sample <- sample_n(reject_full, size = 200000)
save(reject_sample, file = 'Reject_Sample.RData')

