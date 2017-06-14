

library(caret)
library(boot)


# read data
df = read.csv("~/Desktop/us_census_income/census_train.csv")


# gini/auc cost function for cross validation
cost <- function(resp, score)  {  
  pr <- prediction(score, resp)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  gini <- 2*(auc-.5)
  gini
}


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

summary(m1)


# define funciton to bootstrap parameter estimate distributions
boot.glm <- function(df, indices) {
  df <- df[indices,] # select obs. in bootstrap sample
  options(contrasts = c("contr.treatment", "contr.poly"))
  mod <- glm(income50k ~ 
                age
              + sex
              + capital.gains
              + capital.losses
              + num.persons.worked.for.employer
              + weeks.worked.in.year
              , family=binomial(link='logit'), data=df)  
  coefficients(mod) # return coefficient vector
}


# run bootstrap
system.time(model.boot <- boot(train, boot.glm, 100, parallel="multicore", ncpus=7))


# optionally print plot to pdf
#pdf("~/Desktop/.....pdf")


# plot bootstrap distribution output
for (i in 1:length(model.boot$t0))  {
  
  ci <- boot.ci(model.boot, type=c('perc'), conf=.9, t0=model.boot$t0[i], t=model.boot$t[,i])
  
  p <- qplot(model.boot$t[,i],
             geom='histogram',
             bins=100,
             fill=I("blue"), 
             col=I("green")) +
    labs(x=names(model.boot$t0)[i], y="Count") +
    geom_vline(xintercept = ci$percent[4], color = "red", linetype = "dashed") +
    geom_vline(xintercept = ci$percent[5], color = "red", linetype = "dashed") +
    geom_vline(xintercept = 0, color = "purple", linetype = "dashed", size=2)
  
  print(p)
}

# end pdf output
#dev.off()


# define function to bootstrap CV GINI estimates
boot.glm.cv <- function(df, indices) {
  df <- df[indices,] # select obs. in bootstrap sample  
  mod <- glm(income50k ~ 
               age
             + sex
             + capital.gains
             + capital.losses
             + num.persons.worked.for.employer
             + weeks.worked.in.year
             , family=binomial(link='logit'), data=df)  
  cv <- cv.glm(df, mod, cost, 3)
  cv$delta[1]
}


# run CV bootstrap
system.time(cv.boot <- boot(df, boot.glm.cv, 100, parallel="multicore", ncpus=7))
plot(cv.boot, index=1)


# fancy plot CV bootstrap results
ci <- boot.ci(cv.boot, type='perc', conf=.9)

qplot(cv.boot$t,
      geom='histogram',
      bins=100,
      fill=I("blue"), 
      col=I("green")) +
  labs(x="3-Fold Cross Validated GINI", y="Count", title="Bootstrap Distribution") +
  geom_vline(xintercept = ci$percent[4], color = "red", linetype = "dashed") +
  geom_vline(xintercept = ci$percent[5], color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(cv.boot$t), color = "green", size=2)