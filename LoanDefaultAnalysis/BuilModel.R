
load("loan_sample.Rdata")

logit1=glm(loan_statusASNum~ int_rate+installment+termAsFactor
           +gradeASFactor+emp_lengthAsNum+home_ownershipIfOwn+annual_inc+verification_statusAsNum+
             dti+total_acc,data=loan_sample,family="binomial"(link = 'logit'))
summary(logit1)

#Explaination
#...

#https://liberalarts.utexas.edu/prc/_files/cs/Fall2013_Moore_Logistic_Probit_Regression.pdf
#test LIKELIHOOD RATIO, WALD TEST TO SATISFY ASSUMPTION

logitNull=glm(loan_statusASNum~1,data=loan_sample,family="binomial"(link = 'logit'))

backward = step(logit1) #http://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf
forward <- step(logitNull, direction = "forward", scope = ~ int_rate+installment+termAsFactor
                  +gradeASFactor+emp_lengthAsNum+home_ownershipIfOwn+annual_inc+verification_statusAsNum+
                  dti+total_acc)
formula(forward)


#Diagnosis

logit2=glm(loan_statusASNum~ int_rate+installment+termAsFactor
           +gradeASFactor+emp_lengthAsNum+home_ownershipIfOwn+annual_inc+verification_statusAsNum+
             total_acc,data=loan_sample,family="binomial"(link = 'logit'))

summary(logit2)



anova(logit1,logit2,test="Chisq") 
#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
## seems that two models are not very different, so dti can be deleted

#While no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
#install.packages("pscl")
library(pscl)
pR2(logit2)

#http://scg.sdsu.edu/logit_r/
library(car)
vif(logit2)

