
library(ggplot2)
library(dplyr)

train =  read.csv("~/Desktop/us_census_income/census_train_scored.csv")


# calibrate scores to credit score scale

base_score = 620
base_odds = 1/50
pdo = 40

factor = round(pdo/log(2),4)
factor

offset = round(base_score - (factor * log(1/base_odds)), 4)
offset

calibrate <- function(log_odds_score) {
  return(round(offset - log_odds_score * factor))
}

score_calibrated <- sapply(train$mfinal_score_log, calibrate)

head(score_calibrated)
min(score_calibrated)
max(score_calibrated)


# plot caibrated score distribution

qplot(score_calibrated,
    #  xlim = c(400,900),
      geom='histogram',
      bins=20,
      fill=I("blue"), 
      col=I("green")) +
  labs(x="Score", y="Count", title="Calibrated Score Distribution") 


# plot lift chart using credit score scale

train$score_bucket <- cut2(score_calibrated, seq(400,900,50))

perf <- train %>%
  group_by(score_bucket) %>%
  summarise(count = n(),
            bads = sum(income50k),
            bad_rate = mean(income50k))

ggplot(perf, aes(x=score_bucket, y=bad_rate, group=1)) + 
  geom_line(stat='identity', size=2, color='red') +
  labs(x="Calibrated Score Bucket", y="income50k", title="Target Ranking by Score Bucket")