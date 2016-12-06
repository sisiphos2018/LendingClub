#  https://www.r-bloggers.com/logistic-regression-in-r-part-two/
#  https://www.r-bloggers.com/residuals-from-a-logistic-regression/

library(dplyr)

iris_log <- iris

iris_log$y <- ifelse(iris_log$Species == 'setosa', 1, 0)
iris_log <- select(iris_log, -Species)

model_log <- glm(y ~ ., data = iris_log, family = binomial(link = 'logit'))

#####################################
##### Multicollinearity
#####################################

# vif > 2 should be a concern
library(car)
vif(model_log)

#####################################
##### Influential Points
#####################################

# d > (4/n)
library(lme4)
cooks.distance(model_log)

#####################################
##### Residuals
#####################################

plot(predict(model_log), residuals(model_log))
abline(h=0,lty=2,col="grey")

#####################################
##### LR (Deviance) Test for Interaction
#####################################

library(lmtest)
lrtest(mod_fit_one, mod_fit_two)


#####################################
##### Hosmer-Lemeshow GoF & Pseudo-R^2
#####################################

#McFadden R2
library(pscl)
pR2(model_log)

#Hosmer-Lemeshow
library(ResourceSelection)
hoslem.test(iris_log$y, fitted(model_log), g=8)






##############################
### ORDER OF MODEL FITTING ###
##############################

## ALL ON ONE SLIDE ##
#1) Remedy quasi-perfect separation (exclude rows)
#2) Show LR test for interaction term
#3) Show Rest of Diagnostics (R2, Hosmer, Multicollinearity, Residuals, Cooks D)
#4) Explain any reasons for excluding data (separation, Cooks D, etc.)








