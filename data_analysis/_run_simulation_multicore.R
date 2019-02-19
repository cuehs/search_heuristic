print("starting")
library(here)
source(here("stat/180821_evaluation/_basic.R"))
usemulticore <- T

if(usemulticore){
  library(multidplyr)
  if(!exists("cluster")){
    numberOfCores <- 24
    cluster <- create_cluster(numberOfCores)
  }
}


multidplyr::cluster_copy(cluster,oneStopSimulation)
multidplyr::cluster_copy(cluster,oneSearchSimulation)
multidplyr::cluster_copy(cluster,oneCombineSimulation)
multidplyr::cluster_copy(cluster,landscapesMatrix)
multidplyr::cluster_copy(cluster,getAreaMatrix1D)
multidplyr::cluster_copy(cluster,getAreaMatrix2D)
multidplyr::cluster_copy(cluster,resultArray1D)
multidplyr::cluster_copy(cluster,resultArray2D)
multidplyr::cluster_copy(cluster,safetyLevelCalc)

#search
print("starting search simulation")
experimentalLevelIdSearch <- searchDf%>% arrange(levelid) %>% pull(levelid) %>% unique() 
experimentalLevelIdSearch <- seq(1,4000)
searchSimulationDf <- tibble(
  userid = integer(),
  step = integer(),
  levelid = integer(),
  noiseLevel = numeric(),
  rich = logical()
) %>%
  complete(userid =  1,
           levelid = experimentalLevelIdSearch,
           noiseLevel = seq(.1,.3,.025),
           step = c(1:31)) %>%
  mutate(rich = ifelse((levelid %% 2 == 0), "rich", "poor"))

searchSimulationDf <-
  searchSimulationDf%>%
  partition(userid, levelid,noiseLevel,cluster=cluster) %>%
  mutate(sim = oneSearchSimulation(levelidP = levelid,noiseLevel = noiseLevel)) %>%
  collect() %>% 
  separate(
    col = sim,
    sep = ",",
    into = c("positionX", "positionY","payoff","behavior")
  )%>%
  mutate(
    positionX = as.integer(positionX),
    positionY = as.integer(positionY),
    payoff = as.numeric(payoff),
    source = "simulation",
    step=step-1
  )

searchSimulationDf<- searchSimulationDf %>%ungroup()%>%
  mutate(position = getPosition(positionX,positionY),maxValue = maxValue[levelid],normPayoff = payoff/maxValue)%>%
  group_by(userid,levelid,noiseLevel) %>% mutate(maxTill = cummax(payoff), normMaxTill = cummax(normPayoff) )

#stop
print("starting stop simulation")
experimentalLevelIdStop <- stopDf %>% pull(levelid) %>% unique()
experimentalLevelIdStop <- seq(4001,8000)
stopSimulationDf <- tibble(
  userid = integer(),
  safetyLevel = integer(),
  stopLevel = integer(),
  ignoreLevel = integer(),
  step = integer(),
  levelid = integer(),
  rich = logical()
) %>%
  complete(
    userid =  1,
    levelid = experimentalLevelIdStop,
    step = c(1:31),
    safetyLevel=seq(.7,.9,.025),
    stopLevel = seq(60,80,2),
    ignoreLevel = 1
  ) %>%
  mutate(rich = ifelse((levelid %% 2 == 0), "rich", "poor"))%>% 
  filter(stopLevel>=ignoreLevel) %>%arrange(userid,levelid,safetyLevel,stopLevel,ignoreLevel,step)



stopSimulationDf <-
  stopSimulationDf%>%
  partition(userid, levelid, ignoreLevel, stopLevel, safetyLevel,cluster = cluster) %>%
  mutate(sim = oneStopSimulation(levelid, ignoreLevel =ignoreLevel , stopLevel = stopLevel, safetyLevel = safetyLevel, step)) %>%
  collect() %>%
  separate(
    col = sim,
    sep = ",",
    into = c("position", "payoff", "behavior")
  ) %>%
  mutate(
    position = as.integer(position),
    payoff = as.numeric(payoff),
    source = "simulation",
    step = step-1
  )

stopSimulationDf<- stopSimulationDf %>%ungroup()%>%
  mutate(maxValue = maxValue[levelid],normPayoff = payoff/maxValue)%>%
  group_by(userid,levelid,stopLevel,ignoreLevel,safetyLevel) %>%
  mutate(maxTill = cummax(payoff), normMaxTill = cummax(normPayoff) )

#combine
print("starting combine simulation")
experimentalLevelIdCombine <- combineDf %>% pull(levelid) %>% unique()
experimentalLevelIdCombine <- seq(8001,12000)

combineSimulationDf <- tibble(
  userid = integer(),
  step = integer(),
  levelid = integer(),
  ignoreLevel= integer(),
  stopLevel = integer(),
  safetyLevel = numeric(),
  noiseLevel = numeric(),
  rich = logical()
) %>%
  complete(userid = c(1),
           levelid = experimentalLevelIdCombine,
           ignoreLevel = 1,
           stopLevel = seq(30,50,4),
           safetyLevel =  seq(.4,.5,.05),
           noiseLevel = seq(.1,.3,.05),
           step = c(1:31)) %>%
  mutate(rich = ifelse((levelid %% 2 == 0), "rich", "poor"))

combineSimulationDf <-
  combineSimulationDf%>%
  partition(userid, levelid,noiseLevel, stopLevel,safetyLevel,cluster=cluster) %>%
  mutate(sim = oneCombineSimulation(levelidP = levelid,noiseLevel = noiseLevel, stopLevel = stopLevel,safetyLevel = safetyLevel)) %>%
  collect()%>%
  separate(
    col = sim,
    sep = ",",
    into = c("positionX", "positionY","payoff","behavior")
  )%>%
  mutate(
    positionX = as.integer(positionX),
    positionY = as.integer(positionY),
    payoff = as.numeric(payoff),
    source = "simulation",
    step=step-1
  )
combineSimulationDf<- combineSimulationDf %>%ungroup()%>%
  mutate(position = getPosition(positionX,positionY),maxValue = maxValue[levelid],normPayoff = payoff/maxValue)%>%
  group_by(userid,levelid,stopLevel,ignoreLevel,safetyLevel,noiseLevel) %>% mutate(maxTill = cummax(payoff), normMaxTill = cummax(normPayoff))

save(searchSimulationDf,stopSimulationDf,combineSimulationDf,file=here(paste0("stat/180821_evaluation/simulation",sample(1:1000,1),".RData")))

usemulticore <- F