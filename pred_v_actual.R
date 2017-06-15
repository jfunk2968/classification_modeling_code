

library(reshape2)
library(ggplot2)
library(Hmisc)


# function for printing plots side by side
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# function to plot predicted vs. actuals for a categorical variable and binary target
plot_char_diag <- function(df, v, target, score)  {
  
  print("--------------------------------")
  print(v)
  
  mean <- data.frame(
    level=levels(as.factor(df[,v])),
    count=as.numeric(table(df[,v])),
    mean_score=as.numeric(tapply(df[,score], df[,v], mean)),
    mean_resp=as.numeric(tapply(df[,target], df[,v], mean)))
  
  print(mean)
  
  p1 <- ggplot(mean, aes(level,count)) + 
    geom_bar(stat="identity") + 
    labs(x=v, title="Distribution", y="Count")
  
  mean2 = melt(mean,id.vars="level",measure.vars=c("mean_resp","mean_score"))
  
  p2 <- ggplot(mean2, aes(level,value)) + 
    geom_bar(aes(fill=variable),position="dodge",stat="identity") + 
    labs(title="Predicted vs. Actual",x=v,y=paste('Mean',target))
  
  print(multiplot(p1,p2,cols=2))
}


# plots predicted vs. actuals for a numeric variable and binary target
plot_num_diag<- function(df, v, target, score, legend_loc=c(.2,.8))  {
  
  print("--------------------------------")
  print(v)
  
  #q <- quantile(df[,v], seq(0,1,.2))
  
  temp_bin <- cut2(df[,v], seq(min(df[,v]),max(df[,v]),(max(df[,v])-min(df[,v]))/6), levels.mean=TRUE)
  #temp_bin <- cut2(df[,v], q, levels.mean=TRUE)
  
  temp_factor <- as.factor(temp_bin)
  
  mean <- data.frame(
    level=as.numeric(names(table(temp_factor))),
    count=as.numeric(table(temp_factor)),
    mean_score=as.numeric(tapply(df[,score], temp_factor, mean)),
    mean_resp=as.numeric(tapply(df[,target], temp_factor, mean)))
  
  print(mean)
  
  p1 <- ggplot(mean, aes(level,count)) + geom_bar(stat="identity") + 
    labs(title='Distribution',y='Count', x=v) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p2 <- ggplot(mean, aes(level,mean_resp)) + 
    labs(title='Predicted vs. Actuals',y=paste('Mean',target), x=v) + 
    geom_line(aes(colour="Actual"), size=2) +
    geom_line(aes(level,mean_score, colour='Model'), size=2) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_colour_manual("Legend", values = c("red", "blue", "green")) +
    theme(legend.position=legend_loc)
  
  print(multiplot(p1,p2,cols=2))
}


train =  read.csv("~/Desktop/us_census_income/census_train_scored.csv")

char <- c(  'sex',
            'race',
            'citizenship')

nums <- c('divdends.from.stocks',
          'age',
          'weeks.worked.in.year')


#pdf("~/Desktop/PTBC/SME/SME_Scorecard/Results/model_diagnostic_plots.pdf", height=4, width=7)


for (v in char)  {
  plot_char_diag(train, v, 'income50k', 'mfinal_score')
}


# leg_place = list(c(.2,.8),
#                  c(.2,.8),
#                  c(.8,.8),
#                  c(.2,.8),
#                  c(.8,.8),
#                  c(.8,.8),
#                  c(.8,.8),
#                  c(.2,.8))
# names(leg_place) <- nums  

for (v in nums)  {
  plot_num_diag(train, v, 'income50k', 'mfinal_score')
}


#dev.off()

