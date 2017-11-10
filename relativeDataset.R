relativeDataset <- function(teams.dt){
  # separate the dataset along the different teams
  team100 <- teams.dt[teamid==100]
  team200 <- teams.dt[teamid==200]
  # goal : concentrate on predicting wins of team100
  
  # create relative table
  rel.team.dt <- team100-team200
  
  # rm team id
  rel.team.dt[, teamid:= NULL]
  
  # restore some parameters (enlever pour SVM?)
  rel.team.dt[, matchid:= team100$matchid]
  rel.team.dt[, firstblood:= team100$firstblood]
  rel.team.dt[, firsttower:= team100$firsttower]
  rel.team.dt[, firstinhib:= team100$firstinhib]
  rel.team.dt[, firstbaron:= team100$firstbaron]
  rel.team.dt[, firstdragon:= team100$firstdragon]
  rel.team.dt[, firstharry:= team100$firstharry]
  rel.team.dt[, duration:= team100$duration]
  rel.team.dt[, win:=team100$win]
  
  return(rel.team.dt)
}
