# function to create an summary of a dataframe

df_status <- function(data, print_results)
{
  if(missing(print_results))
    print_results=T
  
  df_status_res=data.frame(
    type=sapply(data, get_type_v),
    q_zeros=sapply(data, function(x) sum(x==0,na.rm=T)),
    p_zeros=round(100*sapply(data, function(x) sum(x==0,na.rm=T))/nrow(data),2),
    q_na=sapply(data, function(x) sum(is.na(x))),
    p_na=round(100*sapply(data, function(x) sum(is.na(x)))/nrow(data),2),
    q_nnine=sapply(data, function(x) sum(x==-9,na.rm=T)),
    p_nnine=round(100*sapply(data, function(x) sum(x==-9,na.rm=T))/nrow(data),2),
    q_inf=sapply(data, function(x) sum(is.infinite(x))),
    p_inf=round(100*sapply(data, function(x) sum(is.infinite(x)))/nrow(data),2),
    unique=sapply(data, function(x) sum(!is.na(unique(x)))),
    mode=sapply(data, function(x) get_mode(x)),
    q_mode=sapply(data, function(x) sum(x == get_mode(x), na.rm=TRUE)),
    p_mode=round(100*sapply(data, function(x) sum(x == get_mode(x), na.rm=TRUE))/nrow(data),2)
  )  
  
  
  ## Create new variable for column name
  df_status_res$variable=rownames(df_status_res)
  rownames(df_status_res)=NULL
  
  ## Reordering columns
  df_status_res=df_status_res[, c(14,1,2,3,4,5,6,7,8,9,10,11,12,13)]
  
  
  nums <- data[, sapply(data, is.numeric)]
    
  df_num_status=data.frame(
    x_min=sapply(nums, function(x) min(x, na.rm = TRUE)),
    x_mean=sapply(nums, function(x) mean(x, na.rm = TRUE)),
    x_max=sapply(nums, function(x) max(x, na.rm = TRUE)),
    x_99=sapply(nums, function(x) quantile(x, .99, na.rm = TRUE))
  )  
    
  ## Create new variable for column name
  df_num_status$variable=rownames(df_num_status)
  rownames(df_num_status)=NULL
  
  ## Reordering columns
  df_num_status=df_num_status[, c(5,1,2,3,4)]

  
  final=merge(df_status_res,df_num_status,by='variable',all.x=TRUE, sort=FALSE)
  
  ## Print or return results
  if(print_results) print(final) else return(final)
}

is.POSIXct <- function(x) inherits(x, "POSIXct")
is.POSIXlt <- function(x) inherits(x, "POSIXlt")
is.POSIXt <- function(x) inherits(x, "POSIXt")

get_type_v <- function(x)
{
  ## handler for posix object, because class function returns a list in this case
  posix=ifelse(is.POSIXct(x), "POSIXct", "")
  posix=ifelse(is.POSIXlt(x), paste(posix, "POSIXlt", sep="/"), posix)
  posix=ifelse(is.POSIXt(x), paste(posix, "POSIXt", sep="/"), posix)
  
  # ifnot posix..then something else
  if(posix=="")
  {
    cl=class(x)
    return(ifelse(length(cl)>1, paste(cl, collapse = "-"), cl))
  } else {
    return(posix)
  }
}

get_mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  if (length(mod)>1)
    return ("multiple values")
  else
    return(mod)
}




train <- read.csv("~/Desktop/us_census_income/census_train.csv")
test <- read.csv("~/Desktop/us_census_income/census_test.csv")

train_summary = df_status(train)
View(train_summary)

test_summary = df_status(test)
View(test_summary)
