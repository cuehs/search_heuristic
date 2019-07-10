# fit the parameter for the probabilistic model
library(here)
library(lme4)
source(here("/data_analysis/_basic.R"))



if(!exists("m2") | !exists("m2plus")){
  explorationExtendedDf <- explorationDf %>%
    group_by(userid,source, levelid, rich)  %>%
    mutate( 
      maxVisiblePayoff = maxVisiblePayoff(levelid, positionX, positionY),
      isMaximizingPayoff =  isMaximizingUnvisitedPayoff(levelid, positionX, positionY),
      isMaximizingPayoff = if_else(is.na(isMaximizingPayoff),F,isMaximizingPayoff)) %>%
    ungroup()%>%
    mutate(userid = as.character(userid))
  
  m2 <- (glm(isMaximizingPayoff~maxVisiblePayoff,family=binomial(),data = explorationExtendedDf))
  summary(m2)
  m2plus <- (glmer(isMaximizingPayoff~(maxVisiblePayoff|userid)+maxVisiblePayoff,family=binomial(),data = explorationExtendedDf ))
  summary(m2plus)
}

if(!exists("noTime")){
  noTime <- tibble(maxVisiblePayoff = integer()) %>% complete(maxVisiblePayoff = -1:100)
  noTime <- noTime %>% mutate(maximizePayoff = predict(m2,noTime,type="response"))
}

if(!exists("easyPlus")){
  easyPlus = tibble(maxVisiblePayoff = integer(),userid = character() ) %>%
    complete(maxVisiblePayoff = -1:100,userid = explorationExtendedDf%>%pull(userid)%>%unique())
  easyPlus <- easyPlus %>% mutate(maximizePayoff = predict(m2plus,newdata=easyPlus,type="response"))
}
