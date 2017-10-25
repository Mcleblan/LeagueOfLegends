
WinningHistogram <- function(players.dt,variable,filter=-1){
  #plot that gives the average winnig percentage per value of 'variable'
  dataplot <- players.dt[,.(VarSum = sum(as.numeric(get(variable)))),.(matchid,win)]
  dataplot <- dataplot[,.N,.(VarSum,win)]
  dataplot[,total := sum(N),.(VarSum)]
  dataplot[,Perc := N/total,.(VarSum)]
  if(filter>0)dataplot<-dataplot[VarSum<=filter]
  
  plot1 <- ggplot(dataplot[win==1])+
    geom_bar(aes(x=VarSum,y=Perc),stat='identity',fill='salmon') +
    scale_y_continuous(label=percent,limits=c(0,1))+
    labs(title=paste('Winning percentage per',variable),x=variable,y='Winning Percentage')
  print(plot1)
}

WinningHistogramNormalized <- function(players.dt,variable,filter=-1){
  
  #plot that gives the average winnig percentage per value of normalized 'variable' i.e. per 'variable' per second
  #also buckets the value, as divinding by duration may lead to only one observation otherwise per value.
  
  dataplot <- players.dt[,.(VarSum = sum(as.numeric(get(variable)))/duration),.(matchid,win)]
  dataplot[,VarSum:=cut(VarSum,breaks=100)]
  dataplot <- dataplot[,.N,.(VarSum,win)]
  dataplot[,total := sum(N),.(VarSum)]
  dataplot[,Perc := N/total,.(VarSum)]
  if(filter>0)dataplot<-dataplot[VarSum<=filter]
  
  plot1 <- ggplot(dataplot[win==1])+
    geom_bar(aes(x=VarSum,y=Perc),stat='identity',fill='salmon') +
    scale_y_continuous(label=percent,limits=c(0,1))+
    labs(title=paste('Winning percentage per',variable, 'per second'),x=paste(variable,'per second'),'Winning Percentage')+
    theme_bw()+
    theme(axis.text.x = element_text(angle=90))
  print(plot1)
}
