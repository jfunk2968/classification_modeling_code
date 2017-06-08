
# create information value tables and plots used to rank order candidate variables

train <- read.csv("~/Desktop/us_census_income/census_train.csv")


library(Information)

IV <- Information::create_infotables(data=subset(train, select=-c(instance.weight)), 
                                     y="income50k",
                                     ncore=6,
                                     bins=5)

bins=data.frame(sapply(names(IV$Tables), function(x) nrow(IV$Tables[[x]])))
colnames(bins)=c("bins")
bins$Variable=rownames(bins)
rownames(bins)=NULL
bins

IV_Summary <- merge(IV$Summary,bins,by='Variable')
IV_Summary <- IV_Summary[order(-IV_Summary$IV),]
IV_Summary


# plots/tables in list ordered by IV value 

plots <- list()
tables <- list()

for (i in IV$Summary$Variable){
  plots[[i]] <- plot_infotables(IV, i)
  tables[[i]] <- IV$Tables[i] 
}

plots[0:10]
tables[0:10]