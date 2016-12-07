
## Create and save subset of rejection data
set.seed(10)
loan_sample <- sample_n(loan, size = 100000)
save(loan_sample, file = 'loan_sample.RData')

load("loan_sample.RData")
na_count <-sapply(loan_sample, function(y) sum(length(which(is.na(y)))))
na_count=data.frame(na_count)
na_count

logit0 = glm(loan_sample$loan_statusASNum~  1
             ,family = binomial(link=logit), data = loan_sample)

sapply(1:ncol(loan), function(index) (unique(loan[,index]))[1:2])
sapply(loan_sample,function(x) is.factor(x))
length(levels(loan_sample$  gradeASFactor ))
par(mfrow=c(2,2))

sapply(1:ncol(loan_sample), function(index) (hist(loan_sample[,index],
                                                  main = c(colnames(loan_sample)[index]))))

sapply(1:ncol(loan_sample), function(index) (boxplot(loan_sample[,index]~loan_sample$loan_statusASNum,
                                                     main = c(colnames(loan_sample)[index]))))

summary(logit0)

logitfull = glm(loan_sample$loan_statusASNum~  . ,family = binomial(link=logit), data = loan_sample)

library(MASS)    


Forward=stepAIC(logit0,scope=(~  
                                int_rate                          +
                                annual_inc                        +
                                dti                               +
                                delinq_2yrs                       +
                                inq_last_6mths                    +
                                open_acc                        +
                                pub_rec                           +
                                revol_bal                         +
                                revol_util                        +
                                total_acc                         +
                                out_prncp                         +
                                total_pymnt                       +
                                total_rec_int                     +
                                total_rec_late_fee                +
                                last_pymnt_amnt                   +
                                collections_12_mths_ex_med        +
                                acc_now_delinq                    +
                                chargeoff_within_12_mths          +
                                delinq_amnt                       +
                                pub_rec_bankruptcies              +
                                tax_liens                         +
                                home_ownershipIfOwn               +
                                initial_list_status               +
                                application_type                  +
                                termAsFactor                      +
                                gradeASFactor              ),data=loan_sample,direction = "forward")


a=glm(loan_sample$loan_statusASNum ~ last_pymnt_amnt + out_prncp + 
        total_pymnt + total_rec_int + int_rate + total_rec_late_fee + 
        termAsFactor + gradeASFactor + initial_list_status + total_acc + 
        annual_inc + inq_last_6mths + home_ownershipIfOwn + application_type + 
        pub_rec_bankruptcies + pub_rec
      ,family = binomial(link=logit), data = loan_sample)

b=glm( loan_sample$loan_statusASNum ~ last_pymnt_amnt + out_prncp + 
         int_rate + termAsFactor + total_rec_late_fee + total_pymnt + 
         total_rec_int + annual_inc + dti + gradeASFactor + revol_bal + 
         total_acc + delinq_2yrs + initial_list_status + home_ownershipIfOwn + 
         revol_util + pub_rec + pub_rec_bankruptcies,family = binomial(link=logit), data = loan_sample)
summary(a)



#####


vif(a)
#logit1=update(backAic,.~.-delinq_2yrs)
a=update(a,.~.-total_rec_int )
summary(a)
a=update(a,.~.-application_type)
a=update(a,.~.-pub_rec_bankruptcies)


vif(b)
b=update(a,.~.-int_rate    )
summary(b)
b=update(a,.~.-application_type  )
b=update(a,.~.-pub_rec_bankruptcies )


## a=b
par(mfrow=c(1,1))

plot(cooks.distance(a),pch=16)
outlierTest(a)
library(lme4)
head(cooks.distance(a))
##
plot(predict(a), residuals(a))
abline(h=0,lty=2,col="grey")

par(mfrow=c(2,2))
plot(a)
##
library(lmtest)
lrtest(mod_fit_one, mod_fit_two)

#McFadden R2
library(pscl)
pR2(a)

#Hosmer-Lemeshow
library(ResourceSelection)
hoslem.test(a$y, fitted(a), g=10)

##D-W test require var are normal and idependent
durbinWatsonTest(a)

##
set.seed(120)
load("loan.Rdata")
loan_test <- sample_n(loan, size = 100000)

library(ROCR)
p <- predict(a, newdata=subset(loan, type="response"))
pr <- prediction(p, loan$loan_statusASNum)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
## precision is around 0.87


