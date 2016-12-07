library(data.table)
library(corrplot)
load("NoMonthHaveNA.Rdata")
loan = loanNewNUM %>%
  na.omit()
loan=as.data.frame(loan)
par(mfrow=c(2,2))
sapply(1:ncol(loan), function(index) (hist(loan[,index],
                                           main = c(colnames(loan)[index]))))

sapply(2:ncol(loan), function(index) (boxplot(loan[,index]~loan$loan_statusASNum,
                                              main = c(colnames(loan)[index]))))

boxplot(loan_sample$int_rate~loan_sample$loan_statusASNum)
boxplot(loan_sample$annual_inc~loan_sample$loan_statusASNum)
mode(colnames(loan)[3])

data.frame(colnames(loan))
loanCor=cor(Filter(is.numeric,loan))
M=loanCor
dim(loanCor)

for(i in seq(1,35)){
  
  for (j in seq(1,35)){
    if(is.na(M[i,j]==T)){
      break
    }
    
    if(M[i,j]>0.9 && i<j){
      print(c(i,j,M[i,j])  )
    }
    
  }}

loan=loan[,-c(2,3,5,16,18,19)]
dim(loan)
par(mfrow=c(2,2))
sapply(1:ncol(loan)
       , function(index) (boxplot(loan[,index]~loan$loan_statusASNum,
                                  main = c(colnames(loan)[index]))))
par(mfrow=c(1,1))

boxplot(loan$annual_inc)
quantile(loan$annual_inc, c(.75, .85, .95)) 
# not want these high income since out of 95% threshold
loan=loan[loan$annual_inc<=154000,]
dim(loan)
boxplot(loan$annual_inc)
# seems much better

boxplot(loan$annual_inc~loan$loan_statusASNum)


## dti
quantile(loan$dti, c(.75, .85, .95)) 
loan=loan[loan$dti<34,]
boxplot(loan$dti~loan$loan_statusASNum)

##
a=glm(loan$loan_statusASNum~    home_ownershipIfOwn
      ,family = binomial(link=logit), data = loan)


loan=select(loan,-c(status,collection_recovery_fee,recoveries,policy_code))
dim(loan)
loanCor=cor(Filter(is.numeric,loan))
corrplot.mixed(loanCor)
dim(loanCor)
for(i in seq(1,25)){
  for (j in seq(1,25)){
    if(is.na(loanCor[i,j]==T)){
      break
    }
    if(loanCor[i,j]>0.8 && i<j){
      print(i,j,loanCor[i,j])
    }
    
  }
}

############
############


#load("loan.Rdata")
save(loan,file="loan.Rdata")

