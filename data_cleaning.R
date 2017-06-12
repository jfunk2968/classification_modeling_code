
library(ggplot2)
library(dplyr)

df = read.csv("~/Desktop/us_census_income/census_train.csv")

df_clean = select(df, instance.weight)


# function for producing basic stats for initial review of numeric attributes
check_num <- function(var)  {
  print(paste(" Missing  : ", as.character(sum(is.na(var))), sep=''))
  print(paste(" Mean     : ", as.character(mean(var, na.rm=TRUE)), sep=''))
  print(paste(" Min      : ", as.character(min(var, na.rm=TRUE)), sep=''))
  print(paste(" Max      : ", as.character(max(var, na.rm=TRUE)), sep=''))
  print(" ")
  
  p <- qplot(var,
             geom='histogram',
             bins=100,
             fill=I("blue"), 
             col=I("green")) +
    labs(x="Variable", y="Count")
  
  print(p)
  print(quantile(var, c(.01, .05, .1, .5, .9, .95, .99), na.rm=TRUE))
}


# function for initial cleaning of numeric attributes
clean_num <- function(var,
                      missing_impute=NULL,
                      invalid=NULL,
                      invalid_impute=NULL,
                      floor=NULL,
                      cap=NULL)  {
  
  new_var <- var
  
  if (!is.null(missing_impute))
    new_var[is.na(new_var)] <- missing_impute
  
  if (!is.null(invalid))
    new_var[new_var == invalid] <- invalid_impute
  
  if (!is.null(floor))
    new_var[new_var < floor] <- floor
  
  if (!is.null(cap))
    new_var[new_var > cap] <- cap
  
  return(new_var)
}



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

check_num(df$age)

df_clean$age <- clean_num(df$age,
                                    missing_impute=34,
                                    cap=85)

check_num(df_clean$age)






#write.csv(df_clean, "~/Desktop/.....csv", row.names=FALSE)
