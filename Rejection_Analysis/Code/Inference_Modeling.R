library(dplyr)

setwd('C:\\Users\\LENOVO\\Desktop\\Academic\\ADA\\lending_data\\data')
load('Reject_Sample.RData')

#Replace data name with "reject"
reject <- reject_sample
rm(reject_sample)

####################################
###########Logistic Fit - Inference
####################################

#### Standard fit with no transformations
reject_logistic <- glm(status ~ dti + loan_amnt + year + addr_state + emp_length + month +
                       dti:loan_amnt, 
                       data = reject,
                       family = binomial(link = 'logit'))
#AIC: 71998
summary(reject_logistic)

#### Log Transform fit (add 0.0001 to dti)
reject_logistic_log <- glm(status ~ log(dti + 0.0001) + log(loan_amnt) + year + addr_state 
                           + emp_length + month + log(dti + 0.0001):log(loan_amnt), 
                       data = reject,
                       family = binomial(link = 'logit'))

#AIC: 71617
summary(reject_logistic_log)

# #### Remove DTI linearly separable states
# ub_dti <- range(filter(reject, status == 1)$dti, na.rm = T)[2]
# reject_logistic <- glm(status ~ log(dti + 0.0001) + log(loan_amnt) + year + addr_state + emp_length + month, 
#                        data = filter(reject, dti <= ub_dti),
#                        family = binomial(link = 'logit'))
#
# summary(reject_logistic_log_trim)

