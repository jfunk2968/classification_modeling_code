
# cross validation function with data and plotting output
cv.glm.funk <- function (data, glmfit, target, cost = function(y, yhat) mean((y - yhat)^2), K = n) 
{
  call <- match.call()
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  n <- nrow(data)
  if ((K > n) || (K <= 1)) 
    stop("'K' outside allowable range")
  K.o <- K
  K <- round(K)
  kvals <- unique(round(n/(1L:floor(n/2))))
  temp <- abs(kvals - K)
  if (!any(temp == 0)) 
    K <- kvals[temp == min(temp)][1L]
  if (K != K.o) 
    warning(gettextf("'K' has been set to %f", K), domain = NA)
  f <- ceiling(n/K)
  s <- boot:::sample0(rep(1L:K, f), n)
  n.s <- table(s)
  glm.y <- glmfit$y
  cost.0 <- cost(glm.y, fitted(glmfit))
  ms <- max(s)
  CV <- 0
  Call <- glmfit$call
  p <- ggplot()
  p.df <- list()
  for (i in seq_len(ms)) {
    j.out <- seq_len(n)[(s == i)]
    j.in <- seq_len(n)[(s != i)]
    Call$data <- data[j.in, , drop = FALSE]
    d.glm <- eval.parent(Call)
    p.alpha <- n.s[i]/n
    cost.i <- cost(glm.y[j.out], predict(d.glm, data[j.out, 
                                                     , drop = FALSE], type = "response"))
    CV <- CV + p.alpha * cost.i
    cost.0 <- cost.0 - p.alpha * cost(glm.y, predict(d.glm, 
                                                     data, type = "response"))
    
    pr <- prediction(predict(d.glm, data[j.out, , drop = FALSE], type = "response"), data[j.out, target])
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    
    prf.df <- data.frame(prf@x.values[[1]],prf@y.values[[1]])
    colnames(prf.df) <- c('x','y')
    print(cost.i)
    p <- p + geom_line(data=prf.df, aes(x, y)) #,col="red")
    
    prf.df$fold <- i
    str(prf.df)
    p.df[[i]] <- prf.df
  }
  list(call = call, K = K, delta = as.numeric(c(CV, CV + cost.0)), seed = seed, p = p, p.df = p.df)
}


# cross validation with manually defined folds, data must have a column named 'fold'
cv.glm.funk.fold <- function (data, glmfit, target, cost = function(y, yhat) mean((y - yhat)^2)) 
{
  call <- match.call()
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  n <- nrow(data)
  
  glm.y <- glmfit$y
  cost.0 <- cost(glm.y, fitted(glmfit))
  CV <- 0
  Call <- glmfit$call
  p <- ggplot()
  p.df <- list()
  for (i in unique(data$fold)) {
    j.out <- seq_len(n)[(data$fold==i)]
    j.in <- seq_len(n)[(data$fold!=i)]
    Call$data <- data[j.in, , drop = FALSE]
    d.glm <- eval.parent(Call)
    p.alpha <- length(j.out)/n
    cost.i <- cost(glm.y[j.out], predict(d.glm, data[j.out, , drop = FALSE], type = "response"))
    CV <- CV + p.alpha * cost.i
    cost.0 <- cost.0 - p.alpha * cost(glm.y, predict(d.glm, data, type = "response"))
    
    pr <- prediction(predict(d.glm, data[j.out, , drop = FALSE], type = "response"), data[j.out, target])
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    
    prf.df <- data.frame(prf@x.values[[1]],prf@y.values[[1]])
    colnames(prf.df) <- c('x','y')
    print(p.alpha)
    print(cost.i)
    p <- p + geom_line(data=prf.df, aes(x, y))
    prf.df$fold <- i
    str(prf.df)
    p.df[[i]] <- prf.df
  }
  list(call = call, delta = as.numeric(c(CV, CV + cost.0)), seed = seed, p = p, p.df = p.df)
}




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



# development sample rank ordering
train$mfinal_score <- predict(m1, train, type="response")
pr <- prediction(train$mfinal_score, train$income50k)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prf.overall <- data.frame(prf@x.values[[1]],prf@y.values[[1]])
colnames(prf.overall) <- c('x','y')
prf.overall$fold <- 'Full Training Data'


# 45 degree line for ROC plot
pred_45 <- seq(0,1,.00001)
label_45 <- append(rep(c(0,1),50000),0)
pr <- prediction(pred_45, label_45)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col='green', add=TRUE)
prf45.df <- data.frame(prf@x.values[[1]],prf@y.values[[1]])
colnames(prf45.df) <- c('x','y')


# run and plot CV curves using 'race' as a manual fold
train$fold <- train$race
cv.fold = cv.glm.funk.fold(train, m1, 'income50k', cost)
#print(cv.year$p)

cv_data = rbind(dplyr::bind_rows(cv.fold$p.df), prf.overall)

ggplot() + 
  geom_line(data=cv_data, aes(x, y, color=fold), size=1) +
  geom_line(data=prf45.df, aes(x=x, y=y), color='black') +
  labs(title="Cross Validation ROC by Race",
       x='Actual Cum. Bad Percentage',
       y='Actual Cum. Good Percentage')


# run and plot CV auc curves
cv = cv.glm.funk(train, m1, 'income50k', cost, 10)
#cv$p + geom_line(data=prf.overall, aes(x=x, y=y), color="red", lab="full")
#cv$delta

cv_data2 = rbind(dplyr::bind_rows(cv$p.df), prf.overall)

ggplot() + 
  geom_line(data=cv_data2, aes(x, y, color=fold), size=1) +
  geom_line(data=prf45.df, aes(x=x, y=y), color='black') +
  labs(title="Cross Validation ROC",
       x='Actual Cum. Bad Percentage',
       y='Actual Cum. Good Percentage')













