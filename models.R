PositionGoldLinearModel <- function(players.dt){
  model.dt <- players.dt[,.(win,goldearned,role,duration)]
  model.dt[,goldearnedpersecond := goldearned/duration]
  model <- glm(data=model.dt,formula = win ~ goldearnedpersecond:role,family='binomial')
  return(model)
}

SimpleClassification <- function(){
  temp.dt <- players.dt[, .(TotalGold = sum(as.numeric(goldearned))),.(matchid,win)]
  temp.dt[,Team:=ifelse(win==1,'WinningTeam','LosingTeam')]
  temp.dt <- dcast(temp.dt[,.(Team,matchid,TotalGold)],matchid~Team,value.var='TotalGold')
  temp.dt[,GoldHigherForWinning:=WinningTeam>LosingTeam]
  cat('Winning team had more gold than the losing team',nrow(temp.dt[GoldHigherForWinning==T])/nrow(temp.dt)*100,'% of the time')
}

CompleteTeamModel <- function(teams.dt){
  model <- glm(data=teams.dt[,!c("matchid","teamid")],formula= win~.,family='binomial')
  
  validation <- as.data.table(cbind(teams.dt[,win],model$fitted.values))
  setnames(validation,c('Actual','WinningProbality'))
  validation[,ActualFactor:=factor(Actual,levels=c('0','1'))]
  
  #boxplots of probability, not very interesting as it's pretty much only outliers
  plot1 <- ggplot(data = validation) + geom_boxplot(aes(x = ActualFactor,y = WinningProbality))
  print(plot1)
  
  validation[,PredictedWin := 1*(WinningProbality>0.5)]
  
  cat('Prediction accuracy :',nrow(validation[PredictedWin==Actual]) / nrow(validation),'\n')
  cat('Recall :',nrow(validation[PredictedWin==1 & PredictedWin==Actual]) / nrow(validation[PredictedWin==1 ]),'\n')
  cat('Precision :',nrow(validation[PredictedWin==1 & PredictedWin==Actual]) / nrow(validation[Actual==1]))
  
  return(model)
  
}

CompletePlayerModel <- function(players.dt){
  #remove categorical variables for the moment (+ non variables like teamid)
  model.data <- players.dt[,!c("queueid","matchid","teamid","player","id","championid","ss1",
                               "ss2","item1","item2","item3","item4","item5","item6","trinket")]
  
  model <- glm(data=model.data,formula= win~.,family='binomial')
  
  validation <- as.data.table(cbind(model.data[,win],model$fitted.values))
  setnames(validation,c('Actual','WinningProbality'))
  validation[,ActualFactor:=factor(Actual,levels=c('0','1'))]
  
  #boxplots of probability, not very interesting as it's pretty much only outliers
  plot1 <- ggplot(data = validation) + geom_boxplot(aes(x = ActualFactor,y = WinningProbality))
  print(plot1)
  
  validation[,PredictedWin := 1*(WinningProbality>0.5)]
  
  cat('Prediction accuracy :',nrow(validation[PredictedWin==Actual]) / nrow(validation),'\n')
  cat('Recall :',nrow(validation[PredictedWin==1 & PredictedWin==Actual]) / nrow(validation[PredictedWin==1 ]),'\n')
  cat('Precision :',nrow(validation[PredictedWin==1 & PredictedWin==Actual]) / nrow(validation[Actual==1]))
  
  return(model)
  
}