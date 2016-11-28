library(pROC) #ROC metric
library(MASS) #lda and qda
library(glmnet) #ridge and lasso logistic
library(randomForest) #random forests
library(gam) #Generalized Additive Models
library(dplyr)
#library(gbm) #boosted trees
#library(e1071) #SVM

setwd('C:\\Users\\LENOVO\\Desktop\\Academic\\ADA\\lending_data\\data')
load('Reject_Sample.RData')

set.seed(1)

cv_values <- function(data, k = 5){
  data_nrow <- nrow(data)
  data_cv <- data[sample(data_nrow, data_nrow), ]
  k_folds <- cut(1:data_nrow, breaks = k, labels = FALSE)
  list(data_cv = data_cv, k_folds = k_folds, k = k)
}

reject_cv <- cv_values(reject_sample)
rm(reject_sample)

############################
#### Standard Logistic
############################

roc_log_error <- rep(0, reject_cv$k)

for(i in 1:reject_cv$k){
  test_indices <- which(reject_cv$k_folds == i, arr.ind = TRUE)
  test_data <- reject_cv$data_cv[test_indices, ]
  train_data <- reject_cv$data_cv[-test_indices, ]
  
  ###Fit model
  train_model <- glm(status ~ loan_amnt + dti + addr_state + emp_length + dti:loan_amt
                     + month + year,
                     data = train_data, 
                     family = binomial(link='logit'))
  
  predictions <- predict(train_model, newdata = test_data,
                         type = 'response')
  
  #Use error functions
  roc_log_error[i] <- roc(test_data$status, predictions)$auc[1]
  print(i)
}

roc_log <- mean(roc_log_error)

############################
#### Log Transformed Logistic
############################

roc_log_tran_errors <- rep(0, reject_cv$k)

for(i in 1:reject_cv$k){
  test_indices <- which(reject_cv$k_folds == i, arr.ind = TRUE)
  test_data <- reject_cv$data_cv[test_indices, ]
  train_data <- reject_cv$data_cv[-test_indices, ]
  
  ###Fit model
  train_model <- glm(status ~ log(loan_amnt) + log(dti + 0.0001) + addr_state + emp_length
                     + month + year + log(dti + 0.0001):log(loan_amnt),
                     data = train_data, 
                     family = binomial(link='logit'))
  
  predictions <- predict(train_model, newdata = test_data,
                         type = 'response')
  
  #Use error functions
  roc_log_tran_errors[i] <- roc(test_data$status, predictions)$auc[1]
  print(i)
}

roc_log_tran <- mean(roc_log_tran_errors)


############################
#### Standard Logistic - Probit Link
############################

roc_prob_errors <- rep(0, reject_cv$k)

for(i in 1:reject_cv$k){
  test_indices <- which(reject_cv$k_folds == i, arr.ind = TRUE)
  test_data <- reject_cv$data_cv[test_indices, ]
  train_data <- reject_cv$data_cv[-test_indices, ]
  
  ###Fit model
  train_model <- glm(status ~ loan_amnt + dti + addr_state + emp_length + dti:loan_amnt
                     + month + year,
                     data = train_data, 
                     family = binomial(link='probit'))
  
  predictions <- predict(train_model, newdata = test_data,
                         type = 'response')
  
  #Use error functions
  roc_prob_errors[i] <- roc(test_data$status, predictions)$auc[1]
  print(i)
}

roc_prob <- mean(roc_prob_errors)

############################
#### Probit Transformed Logistic
############################

roc_prob_tran_errors <- rep(0, reject_cv$k)

for(i in 1:reject_cv$k){
  test_indices <- which(reject_cv$k_folds == i, arr.ind = TRUE)
  test_data <- reject_cv$data_cv[test_indices, ]
  train_data <- reject_cv$data_cv[-test_indices, ]
  
  ###Fit model
  train_model <- glm(status ~ log(loan_amnt) + log(dti + 0.0001) + addr_state + emp_length
                     + month + year + log(loan_amnt):log(dti + 0.0001),
                     data = train_data, 
                     family = binomial(link='probit'))
  
  predictions <- predict(train_model, newdata = test_data,
                         type = 'response')
  
  #Use error functions
  roc_prob_tran_errors[i] <- roc(test_data$status, predictions)$auc[1]
  print(i)
}

roc_prob_tran <- mean(roc_prob_tran_errors)


############################
#### LDA
############################

roc_lda_errors <- rep(0, reject_cv$k)

for(i in 1:reject_cv$k){
  test_indices <- which(reject_cv$k_folds == i, arr.ind = TRUE)
  test_data <- reject_cv$data_cv[test_indices, ]
  train_data <- reject_cv$data_cv[-test_indices, ]
  
  ###Fit model
  train_model <- lda(status ~ loan_amnt + dti + addr_state + emp_length
                     + month + year + loan_amnt:dti,
                     data = train_data)
  predictions <- predict(train_model, newdata = test_data)
  
  #Use error functions
  roc_lda_errors[i] <- roc(test_data$status, apply(predictions$posterior, 1, max))$auc[1]
}

roc_lda <- mean(roc_lda_errors)


# ############################
# #### QDA
# ############################
# 
# roc_qda_errors <- rep(0, reject_cv$k)
# 
# for(i in 1:reject_cv$k){
#   test_indices <- which(reject_cv$k_folds == i, arr.ind = TRUE)
#   test_data <- reject_cv$data_cv[test_indices, ]
#   train_data <- reject_cv$data_cv[-test_indices, ]
#   
#   ###Fit model
#   train_model <- qda(status ~ loan_amnt + dti + addr_state + emp_length
#                      + month + year,
#                      data = train_data)
#   predictions <- predict(train_model, newdata = test_data)
#   
#   #Use error functions
#   roc_qda_errors[i] <- roc(test_data$status, apply(predictions$posterior, 1, max))$auc[1]
# }
# 
# roc_qda <- mean(roc_qda_errors)


############################
#### RF
############################

roc_rf_errors <- rep(0, reject_cv$k)

for(i in 1:reject_cv$k){
  test_indices <- which(reject_cv$k_folds == i, arr.ind = TRUE)
  test_data <- reject_cv$data_cv[test_indices, ]
  train_data <- reject_cv$data_cv[-test_indices, ]
  
  ###Fit model
  train_model <- randomForest(as.factor(status) ~ loan_amnt + dti + addr_state + emp_length
                              + month + year + dti:loan_amnt,
                              data = train_data, 
                              nodesize = 2,
                              ntree = 500, 
                              na.action = na.omit)
  predictions <- predict(train_model, newdata = test_data, type = 'prob')[, 2]
  
  #Use error functions
  roc_rf_errors[i] <- roc(test_data[['status']], predictions)$auc[1]
  print(i)
}

roc_rf <- mean(roc_rf_errors)


############################
#### Generalized Additive Models
############################


gam_formula <- formula(status ~ s(loan_amnt) + s(dti) + addr_state + emp_length
                       + month + year + s(loan_amnt:dti))

roc_gam_errors <- rep(0, reject_cv$k)

for(i in 1:reject_cv$k){
  test_indices <- which(reject_cv$k_folds == i, arr.ind = TRUE)
  test_data <- reject_cv$data_cv[test_indices, ]
  train_data <- reject_cv$data_cv[-test_indices, ]
  
  ###Fit appropriate model
  train_model <- gam(gam_formula, data = train_data, family = binomial)
  predictions <- predict(train_model, newdata = test_data)
  
  #Use appropriate error functions
  roc_gam_errors[i] <- roc(test_data[['status']], predictions)$auc[1]
  print(i)
}

roc_gam <- mean(roc_gam_errors)

