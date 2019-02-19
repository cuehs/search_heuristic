source(here("/data_analysis/_basic.R"))

#needs simulations to be run before
#source(here("/data_analysis/run_and_fit_simulation.R"))

explorationDf %>%
  group_by(rich) %>%
  summarize(mean(normMaxTill),sd(normMaxTill))

explorationDf %>% group_by(userid,levelid,rich) %>%
  summarize(count=n_distinct(position)) %>% group_by(rich) %>%
  summarize(mean(count),sd(count))

explorationDf %>% group_by(userid,levelid,rich) %>%
  summarize(count=n_distinct(position)) %>% ungroup %>%
  summarize(fields=mean(count)) %>% pull(fields) / 31


exploitationDf %>%
  group_by(rich) %>% filter(step == 30) %>% 
  summarize(mean(normPayoff),sd(normPayoff))

exploitationDf %>%
  group_by(rich,userid,level) %>% filter(step %in% c(0,30)) %>%
  mutate(dist = max(abs(positionX[1]-positionX[2]),
                    abs(positionY[1]-positionY[2]))) %>%
  group_by(rich)  %>% 
  summarize(mean(dist),sd(dist))



exploitationDf %>% 
  group_by(userid,levelid) %>%
  mutate(endPosition = position[31],
         endPayoff = payoff[31],
         endMaxPosition = positionMaxTill[31],
         endMaxPeriod = which(endMaxPosition==position)[1]) %>% 
  group_by(userid,levelid,endPosition,endMaxPosition,endMaxPeriod,rich) %>% 
  summarise(D_optimal = mean(floor((32 - endMaxPeriod)/2)),
            R = howFarFromEndPos1(position=position,endPosition = endMaxPosition),
            S = R / D_optimal,
            endPayoff = mean(endPayoff)) %>%
  mutate(include  = ((D_optimal > 2) && (S > 1)),
         nothing = endPayoff == 0) %>%ungroup()%>%
  summarise(mean(include),mean(nothing))


combinedDf %>% 
  group_by(rich) %>% filter(step == 30) %>% 
  summarize(mean(normPayoff),sd(normPayoff))

explorationDf %>%
  group_by(rich,userid,level) %>% filter(step %in% c(0,30)) %>%
  mutate(dist = max(abs(positionX[1]-positionX[2]),
                    abs(positionY[1]-positionY[2]))) %>%ungroup()%>%
  summarize(mean(dist,na.rm=T),sd(dist,na.rm=T))

combinedDf %>%
  group_by(rich,userid,level) %>% filter(step %in% c(0,30)) %>%
  mutate(dist = max(abs(positionX[1]-positionX[2]),
                    abs(positionY[1]-positionY[2]))) %>%ungroup()%>%
  summarize(mean(dist),sd(dist))

movement %>% group_by(userid) %>%
  summarise(diff = max(time)- min(time)) %>% 
  ungroup() %>% summarise(mean(diff), sd(diff))
