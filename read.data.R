library(data.table)
library(ggplot2)
library(scales)
source('plot.functions.R')
source('models.R')

ReadData <- function(){
  match.dt <<- fread('matches.csv')
  #merging the two datasets removes 3 players from the 'participants' that do not appear in 'stats'. 
  #My guess is they never connected to the game.
  players.dt <<- merge(fread('participants.csv'),rbind(fread('stats1.csv'),fread('stats2.csv')),by='id')
  teambans.dt <<- fread('teambans.csv')
  teamstats.dt <<- fread('teamstats.csv')
}

Formatdata <- function(players.dt){
  #fixes the weird '\N' character to NA and converts the rest back to integers
  players.dt[wardsbought=='\\N',wardsbought := NA]
  players.dt[,wardsbought := as.integer(wardsbought)]
  #adding duration for ease of computation later on
  players.dt <- merge(players.dt,match.dt[,.(id,duration)],by.x='matchid',by.y='id')
  return(players.dt)
}

SimpleClassification <- function(){
  temp.dt <- players.dt[, .(TotalGold = sum(as.numeric(goldearned))),.(matchid,win)]
  temp.dt[,Team:=ifelse(win==1,'WinningTeam','LosingTeam')]
  temp.dt <- dcast(temp.dt[,.(Team,matchid,TotalGold)],matchid~Team,value.var='TotalGold')
  temp.dt[,GoldHigherForWinning:=WinningTeam>LosingTeam]
  cat('Winning team had more gold than the losing team',nrow(temp.dt[GoldHigherForWinning==T])/nrow(temp.dt)*100,'% of the time')
}

#### Execution ####

# ReadData()
# players.dt <- Formatdata(players.dt)

model1 <- PositionGoldLinearModel(players.dt)