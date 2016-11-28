library(dplyr)

setwd('C:\\Users\\LENOVO\\Desktop\\Academic\\ADA\\lending_data\\data')
load('Reject_Sample.RData')

#Replace data name with "reject"
reject <- reject_sample
rm(reject_sample)

### EDA)

#DTI
boxplot(dti ~ status, data = reject)
boxplot(log(dti+.0001) ~ status, data = reject)

#Loan Amt
boxplot(loan_amnt ~ status, data = reject)
boxplot(log(loan_amnt) ~ status, data = reject)

#Loan Amt vs. DTI
cor(log(reject$loan_amnt), log(reject$dti+0.0001), use = 'complete.obs')
plot(log(reject$loan_amnt), log(reject$dti + 0.0001))

#State
state_mean <- reject %>%
  group_by(addr_state) %>%
  summarise(mean = mean(status))

barplot(state_mean$mean, names.arg = state_mean$addr_state)
rm(state_mean)

#Employment Length
emp_mean <- reject %>%
  group_by(emp_length) %>%
  summarise(mean = mean(status))

barplot(emp_mean$mean, names.arg = emp_mean$emp_length)
rm(emp_mean)

#Month
month_mean <- reject %>%
  group_by(month) %>%
  summarise(mean = mean(status))

barplot(month_mean$mean, names.arg = month_mean$month)
rm(month_mean)

#Year
year_mean <- reject %>%
  group_by(year) %>%
  summarise(mean = mean(status))

barplot(year_mean$mean, names.arg = year_mean$year)
rm(year_mean)

# # Month-Year
# monyear_mean <- reject %>%
#   group_by(year, month) %>%
#   summarise(mean = mean(status))
# 
# barplot(monyear_mean$mean, names.arg = monyear_mean$year)
# rm(monyear_mean)

# #Policy Code
# pc_mean <- reject %>%
#   group_by(policy_code) %>%
#   summarise(mean = mean(status))
# 
# barplot(pc_mean$mean, names.arg = pc_mean$policy_code)

