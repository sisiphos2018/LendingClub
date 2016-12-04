library(corrplot)
LoanNoNANum=Filter(is.numeric,LoanNoNA)
M <- cor(LoanNoNANum)
corrplot(M, method="circle",type="upper")
corrplot.mixed(M)

#Many vars are strongly correlated
mode(loan_raw$month)
