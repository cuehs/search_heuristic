if(!exists("DATAISREAD")){
  library(here)
  library(viridis)
  library(truncnorm)
  library(cowplot)
  library(fields)
  library(reshape2)
  library(tidyverse)

  set.seed(20190118)
  select <- dplyr::select
  sample<-base::sample
  
  source(here("/data_analysis/_helper_behavior.R"))
  source(here("/data_analysis/_readData.R"))
  source(here("/data_analysis/_helper_simulation.R"))
  source(here("/data_analysis/_simulation_loops.R"))
  
  movement <- movement %>% group_by(levelid) %>%
    mutate(maxTill = cummax(payoff), source = "behavioral",normMaxTill = maxTill/maxValue)

  explorationDf <- filter(movement,stage == 0) 
  exploitationDf <- filter(movement,stage == 1) %>% mutate(position = position + 1) 
  
  exploitationDf<- full_join(exploitationDf,exploitationDf %>%
                       mutate(notremove = ifelse(position > (step+1), F, T))%>%
                       select(levelid, notremove) %>%
                       group_by(levelid)%>%
                       summarise(notremove = all(notremove))) %>% filter(notremove)
  
  exploitationDf<-exploitationDf %>% group_by(levelid) %>%
    mutate(positionMaxTill = positionOfMaxTill(payoff,position),
           positionMaxTillLast = positionOfMaxTillLast(payoff,position),
           distToMax = abs(position-positionMaxTill),
           distToMaxLast = abs(position-positionMaxTillLast),
           delta = distToMax - (30-seq(0,30)))
  
  combinedDf<- filter(movement,stage == 2)
  
  DATAISREAD <- T
} else{
  source(here("/data_analysis/_helper_behavior.R"))
  source(here("/data_analysis/_helper_simulation.R"))
  source(here("/data_analysis/_simulation_loops.R"))
}


