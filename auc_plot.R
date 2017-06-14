

library(caret)
library(ROCR)


# read data
df = read.csv("~/Desktop/us_census_income/census_train.csv")


# split full data into train and test
intrain<-createDataPartition(y=df$income50k,p=0.2,list=FALSE)
train<-df[intrain,]
test<-df[-intrain,]
table(train$income50k)
table(test$income50k)


# fit model
options(contrasts = c("contr.treatment", "contr.poly"))
m1 <- glm(income50k ~ 
            age
          + sex
          + capital.gains
          + capital.losses
          + num.persons.worked.for.employer
          + weeks.worked.in.year
          , family=binomial(link='logit'), data=train)


# score model
train$mfinal_score <- predict(m1, train, type="response")
test$mfinal_score <- predict(m1, test, type="response")


# training data rank ordering
pr_train <- prediction(train$mfinal_score, train$income50k)
prf_train <- performance(pr_train, measure = "tpr", x.measure = "fpr")
plot(prf_train,col="red")

auc_train <- performance(pr_train, measure = "auc")
auc_train <- auc_train@y.values[[1]]
auc_train

gini_train <- 2*(auc_train-.5)
gini_train


# test data rank ordering
pr_test <- prediction(test$mfinal_score, test$income50k)
prf_test <- performance(pr_test, measure = "tpr", x.measure = "fpr")
plot(prf_test,col="blue")

auc_test <- performance(pr_test, measure = "auc")
auc_test <- auc_test@y.values[[1]]
auc_test

gini_test <- 2*(auc_test-.5)
gini_test


# add 45 degree line for ROC plots
pred_45 <- seq(0,1,.00001)
label_45 <- append(rep(c(0,1),50000),0)
pr <- prediction(pred_45, label_45)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col='green', add=TRUE)