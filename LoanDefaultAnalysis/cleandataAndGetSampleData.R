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

## Change Char data to numeric data
load("NewLoan.Rdata")
old=loan_raw



c=loan_raw$int_rate
for(j in seq(c))   
{   
  c[[j]] <- gsub("%"," ", c[[j]])   
}  
c=as.numeric(c)

#head(c)
#head(loan_raw$int_rate)
loan_raw$int_rate=c

loan_raw$installment = as.numeric(loan_raw$installment)
loan_raw$termAsFactor=as.factor(loan_raw$term)
loan_raw$termAsNum = mapvalues(loan_raw$term, from = c(" 36 months", " 60 months"), to = c(36, 60))
loan_raw$termAsNum = as.numeric(loan_raw$termAsNum)


loan_raw$gradeASFactor = mapvalues(loan_raw$grade, from = c("A","B","C","D","E","F","G"), to = c(1,2,3,4,5,6,7))
loan_raw$gradeASFactor = as.numeric(loan_raw$gradeASFactor)
loan_raw$gradeASFactor = as.factor(loan_raw$gradeASFactor)

loan_raw$emp_lengthAsNum = mapvalues(loan_raw$emp_length, from = c("10+ years","< 1 year","1 year",
                                                                   "3 years","8 years","9 years","4 years",
                                                                   "5 years","6 years","2 years","7 years",
                                                                   "n/a"), to = c(10,0,1,3,8,9,4,5,6,2,7,NA))

loan_raw$emp_lengthAsNum = as.numeric(loan_raw$emp_lengthAsNum)


loan_raw$home_ownershipIfOwn = mapvalues(loan_raw$home_ownership , from = c("RENT","OWN","MORTGAGE","OTHER","NONE","ANY"),
                                         to = c(0,1,1,0,0,0) )

loan_raw$home_ownershipIfOwn = as.numeric(loan_raw$home_ownershipIfOwn)
loan_raw$home_ownershipIfOwn =  (loan_raw$home_ownershipIfOwn-1)*(-1)


loan_raw$annual_inc = as.numeric(loan_raw$annual_inc)

loan_raw$verification_statusAsNum = mapvalues(loan_raw$verification_status , from = c("Verified","Source Verified",
                                                                                      "Not Verified"), to = c(0,0,1) )

loan_raw$verification_statusAsNum = as.numeric(loan_raw$verification_statusAsNum)

## 1 as default and 0 as not default, becasue we need to know who's going to default
loan_raw$loan_statusASNum = mapvalues(loan_raw$loan_status , from = c("Fully Paid","Charged Off","In Grace Period","Current","Late (31-120 days)",
                                                                 "Late (16-30 days)","Default"), to = c(0,1,0,0,1,1,1) )

loan_raw$loan_statusASNum=as.numeric(loan_raw$loan_statusASNum)

loan_raw$dti <- as.numeric(loan_raw$dti)

loan_raw$total_acc = as.numeric(loan_raw$total_acc)

# next is to assigin X and Y

LoanForModel <- loan_raw %>%
  select(loan_amnt, int_rate,installment,termAsFactor,gradeASFactor,
         emp_lengthAsNum, home_ownershipIfOwn,annual_inc,verification_statusAsNum,
          loan_statusASNum,dti, total_acc)

head(LoanForModel)
str(LoanForModel)


save(LoanForModel,file = "~/Downloads/modelData.Rdata")

#####

load("modelData.Rdata")
colnames(LoanForModel)
dim(LoanForModel)
LoanNoNA = LoanForModel %>%
  na.omit()

## Create and save subset of rejection data
set.seed(10)
loan_sample <- sample_n(LoanNoNA, size = 200000)
save(loan_sample, file = 'loan_sample.RData')







