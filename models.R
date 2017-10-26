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

CompleteTeamModel <- function(teams.dt,NormalizeDuration=F){
  
  if(NormalizeDuration){
    #compute normalization (could use apply here for speed but w/e)
    for(sd in setdiff(colnames(teams.dt),c("firstblood","firsttower","firstinhib","firstbaron","firstdragon",
                                           "firstharry","duration","win"))){
      teams.dt <- teams.dt[,as.character(sd):=get(sd)/duration]
    }
    teams.dt <- teams.dt[,!"duration"]
  }
  
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

CompleteTeamModel_kCV <- function(teams.dt,NormalizeDuration=F,k=10){
  cat('Running Team Model',k,'Fold Cross Validation','\n')
  if(NormalizeDuration){
    #compute normalization (could use apply here for speed but w/e)
    for(sd in setdiff(colnames(teams.dt),c("firstblood","firsttower","firstinhib","firstbaron","firstdragon",
                                           "firstharry","duration","win"))){
      teams.dt <- teams.dt[,as.character(sd):=get(sd)/duration]
    }
    teams.dt <- teams.dt[,!"duration"]
  }
  
  data.model <- teams.dt[,!c("matchid","teamid")]
  #set up variable to do the CV
  idx.total <- 1:nrow(teams.dt)
  size.k.fold <- floor(nrow(teams.dt)/k)
  validation.k.fold <- c()

  for(i in 0:(k-1)){
    cat('Running fold',i,'...')
    idx.test <- (size.k.fold*i):(size.k.fold*(i+1))
    idx.train <- setdiff(idx.total,idx.test) 
    model.k.fold <- glm(data.model[idx.train],formula= win~.,family='binomial')
    predict.k.fold <- predict(model.k.fold,data.model[idx.test],type='response')
    validation.k.fold <- rbind(validation.k.fold,cbind(data.model[idx.test]$win,predict.k.fold))
    cat('Done !','\n')

  }
  validation.k.fold <- as.data.table(validation.k.fold)
  setnames(validation.k.fold,c('Actual','WinningProbality'))
  WinningProbBoxPlot(validation.k.fold)
  
  validation.k.fold[,PredictedWin := 1*(WinningProbality>0.5)]
  
  cat('Prediction accuracy :',nrow(validation.k.fold[PredictedWin==Actual]) / nrow(validation.k.fold),'\n')
  cat('Recall :',nrow(validation.k.fold[PredictedWin==1 & PredictedWin==Actual]) / nrow(validation.k.fold[PredictedWin==1 ]),'\n')
  cat('Precision :',nrow(validation.k.fold[PredictedWin==1 & PredictedWin==Actual]) / nrow(validation.k.fold[Actual==1]),'\n')
  
  return(validation.k.fold)
  
}


CompletePlayerModel <- function(players.dt, NormalizeDuration = F){
  #remove categorical variables for the moment (+ non variables like teamid)
  model.data <- players.dt[,!c("queueid","matchid","teamid","player","id","championid","ss1",
                               "ss2","item1","item2","item3","item4","item5","item6","trinket")]
  
  if(NormalizeDuration){
    #compute normalization (could use apply here for speed but w/e)
    for(sd in setdiff(colnames(model.data),c("role","position","duration","win"))){
      model.data <- model.data[,as.character(sd):=get(sd)/duration]
    }
    model.data <- model.data[,!"duration"]
  }
  
  model <- glm(data=model.data,formula= win~.,family='binomial')
  
  validation <- as.data.table(cbind(model.data[,win],model$fitted.values))
  setnames(validation,c('Actual','WinningProbality'))
  
  #boxplots of probability, not very interesting as it's pretty much only outliers
  WinningProbBoxPlot(validation)
  
  validation[,PredictedWin := 1*(WinningProbality>0.5)]
  
  cat('Prediction accuracy :',nrow(validation[PredictedWin==Actual]) / nrow(validation),'\n')
  cat('Recall :',nrow(validation[PredictedWin==1 & PredictedWin==Actual]) / nrow(validation[PredictedWin==1 ]),'\n')
  cat('Precision :',nrow(validation[PredictedWin==1 & PredictedWin==Actual]) / nrow(validation[Actual==1]))
  
  return(model)
  
}