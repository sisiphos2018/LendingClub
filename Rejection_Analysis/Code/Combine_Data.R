library(data.table)
library(dplyr)

setwd('C:\\Users\\LENOVO\\Desktop\\Academic\\ADA\\lending_data\\data')

loan_a <- fread('LoanStats3a.csv', header = T, skip = 1)
loan_a$year <- '07-11'

loan_b <- fread('LoanStats3b.csv', header = T, skip = 1)
loan_b$year <- '12-13'

loan_c <- fread('LoanStats3c.csv', header = T, skip = 1)
loan_c$year <-'14'
  
loan_d <- fread('LoanStats3d.csv', header = T, skip = 1)
loan_d$year <- '15'

loan_e1 <- fread('LoanStats_2016Q1.csv', header = T, skip = 1)
loan_e1$year <- '16'

loan_e2 <- fread('LoanStats_2016Q2.csv', header = T, skip = 1)
loan_e2$year <- '16'

loan_raw <- bind_rows(loan_a, loan_b, loan_c, loan_d, loan_e1, loan_e2)

rm(loan_a, loan_b, loan_c, loan_d, loan_e1, loan_e2)
save(loan_raw, file = 'Loan_Raw.RData')

reject_a <- fread('RejectStatsA.csv', header = T, skip = 1)
reject_a$year <- '07-12'

reject_b <- fread('RejectStatsB.csv', header = T, skip = 1)
reject_b$year <- '13-14'

reject_d <- fread('RejectStatsD.csv', header = T, skip = 1)
reject_d$year <- '15'

reject_e1 <- fread('RejectStats_2016Q1.csv', header = T, skip = 1)
reject_e1$year <- '16'

reject_e2 <- fread('RejectStats_2016Q2.csv', header = T, skip = 1)
reject_e2$year <- '16'

reject_raw <- bind_rows(reject_a, reject_b, reject_d, reject_e1, reject_e2)

rm(reject_a, reject_b, reject_d, reject_e1, reject_e2)
save(reject_raw, file = 'Reject.RData')

