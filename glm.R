

library(caret)
library(car)  # for vif
library(boot)  # for cv.glm


# read data
df = read.csv("~/Desktop/us_census_income/census_train.csv")
colSums(is.na(df))
str(df)
table(complete.cases(df))


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


# get model formula
cat(paste(names(df), collapse='\n+ '))


# fit initial model
options(contrasts = c("contr.treatment", "contr.poly"))
m1 <- glm(income50k ~ 
            age
          + class.of.worker
          #+ industry.code
          #+ occupation.code
          + education
          #+ enrolled.in.edu.inst.last.wk
          + marital.status
          #+ major.industry.code
          #+ major.occupation.code
          + race
          + hispanic.Origin
          + sex
          + member.of.a.labor.union
          #+ reason.for.unemployment
          + full.or.part.time.employment.stat
          + capital.gains
          + capital.losses
          + divdends.from.stocks
          + tax.filer.status
          #+ region.of.previous.residence
          #+ state.of.previous.residence
          #+ detailed.household.and.family.stat
          #+ detailed.household.summary.in.household
          #+ migration.code.change.in.msa
          #+ migration.code.change.in.reg
          #+ migration.code.move.within.reg
          + live.in.this.house.1.year.ago
          #+ migration.prev.res.in.sunbelt
          + num.persons.worked.for.employer
          #+ family.members.under.18
          #+ country.of.birth.father
          #+ country.of.birth.mother
          #+ country.of.birth.self
          #+ citizenship
          + own.business.or.self.employed
          #+ fill.inc.questionnaire.for.veterans.admin
          #+ veterans.benefits
          + weeks.worked.in.year
          , family=binomial(link='logit'), data=train)

summary(m1)

vif(m1)

options(contrasts=c("contr.sum","contr.poly"))
d <- drop1(m1, ~., test="LRT") 
d[order(d$AIC),]

cv1 <- cv.glm(train, m1, cost, 5)
cv1$delta
