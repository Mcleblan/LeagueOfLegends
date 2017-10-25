PositionGoldLinearModel <- function(players.dt){
  model.dt <- players.dt[,.(win,goldearned,role,duration)]
  model.dt[,goldearnedpersecond := goldearned/duration]
  model <- glm(data=model.dt,formula = win ~ goldearnedpersecond:role,family='binomial')
  return(model)
}
