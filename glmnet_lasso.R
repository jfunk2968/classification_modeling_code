
library(dplyr)

df = read.csv("~/Desktop/us_census_income/census_train.csv")



y = as.factor(df[['income50k']])
inputs = select(df, -income50k, -instance.weight)
x = model.matrix(~.+0,inputs)
str(x)



library(glmnet)
cvfit = cv.glmnet(x,y, family='binomial', type.measure='auc')


plot(cvfit)
cvfit$lambda.min
log(cvfit$lambda.1se)
coef(cvfit, s = "lambda.1se")
coef(cvfit, s = "lambda.min")

coefs = coef(cvfit, s = "lambda.1se")
coefs[is.na(coefs)==FALSE]



lamby <- cvfit$lambda[23]
coef(cvfit, s = lamby)


cvfit$cvm

params = as.data.frame(as.matrix(coef(cvfit,s = "lambda.1se")))
colnames(params) = c("original.1se")
params$var = rownames(params)

for(i in 0:length(cvfit$lambda)) { 
  print(i)
  temp <- as.data.frame(as.matrix(coef(cvfit,s = cvfit$lambda[i])))
  colnames(temp) = c(str(i))
  temp$var = rownames(temp)
  params <- merge(params,temp,by="var")
}

summary = rbind(unname(cvfit$nzero),cvfit$cvm,cvfit$lambda,params)
View(summary)
write.csv(summary,'/Users/Jeremy/Desktop/PTBC/SME/SME_Scorecard/Results/lass_results.csv')

