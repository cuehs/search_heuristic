library(here)
source(here("/data_analysis/_basic.R"))

###
# exploration phase
###

print("run simulation explore (this may take a while)")
experimentalLevelId <- seq(1,1000)
explorationSimulationDf <- tibble(
  userid = integer(),
  step = integer(),
  levelid = integer(),
  noiseLevel = numeric(),
  rich = logical()) %>%
  complete(userid = 1, levelid = experimentalLevelId, 
           noiseLevel =seq(.15,.2,.01),step = c(1:31)) %>%
  mutate(rich = ifelse((levelid %% 2 == 0), "rich", "poor"))

explorationSimulationDf <-
  explorationSimulationDf%>%
  group_by(userid, levelid,noiseLevel) %>%
  mutate(sim = oneExplorationSimulation(levelidP = levelid, noiseLevel = noiseLevel)) %>%
  separate(col = sim, sep = ",",
    into = c("positionX", "positionY","payoff","behavior"))%>%
  mutate( positionX = as.integer(positionX),
    positionY = as.integer(positionY),
    payoff = as.numeric(payoff),
    source = "simulation",
    step=step-1)
explorationSimulationDf<- explorationSimulationDf %>%ungroup()%>%
  mutate(position = 
           getPosition(positionX,positionY),
         maxValue = maxValue[levelid],
         normPayoff = payoff/maxValue)%>%
  group_by(userid,levelid,noiseLevel) %>% 
  mutate(maxTill = cummax(payoff), normMaxTill = cummax(normPayoff) )


print("run simulation exploration")
## fit is max payoff found with noiseLevel

toTrain <-  explorationDf %>%
  group_by(step) %>%
  summarise(meanMaxPayoff = mean(normMaxTill)) %>%
  pull(meanMaxPayoff)
bestEpsilon <- explorationSimulationDf %>%
  group_by(levelid,noiseLevel) %>%
  mutate(maxTill = cummax(payoff)) %>%
  group_by(noiseLevel,step) %>%
  summarise(meanMaxPayoff = mean(normMaxTill)) %>%
  summarise(dist= euclidianDistance(meanMaxPayoff,toTrain)) %>%
  arrange(dist) %>% filter(dist == min(dist)) %>% pull(noiseLevel)
print(paste("explore phase, epsilon:", bestEpsilon))

explorationSimulationDf <- filter(explorationSimulationDf,
                             between(noiseLevel, bestEpsilon - .001, bestEpsilon+.001))
###
# exploitation phase
###



print("run: fit exploit")
## fit kStop and safetyLevelMean and meanStop and sdStop
experimentalData <- exploitationDf %>% group_by(levelid)%>% 
  mutate(endMaxPayoff = maxTill[31],
         endMaxPosition = positionMaxTill[31],
         endMaxPeriod = which(endMaxPosition==position)[1],
         endPayoff = payoff[31]) %>% 
  group_by(userid,source,levelid,rich,endMaxPayoff,endMaxPosition) %>% 
  summarise(D_optimal = mean(floor((32 - endMaxPeriod)/2)),
            R = howFarFromEndPos1(position=position,endPosition = endMaxPosition),
            S = R / D_optimal,
            endPayoff = mean(endPayoff)) %>% 
  ungroup()  %>%mutate(stops = S==0, noReturn = S > 1)


fitData<-experimentalData %>%
  group_by(endPayoff,stops,noReturn) %>% tally()%>% group_by(endPayoff)%>% mutate(freq=n/sum(n)) 

# fit for S=0
m1<-lm(freq~0+endPayoff,weight=n,data=filter(fitData,stops==T))
kStop <- m1$coefficients
plot(seq(0,100),seq(0,100)*kStop, type = "l")
points(filter(fitData,stops==T)%>%
         pull(endPayoff),filter(fitData,stops==T)%>%pull(freq))


# fit for S>0
S <- experimentalData  %>% 
  filter(stops ==F,endPayoff > 0, S<=1) %>%ungroup()%>% mutate(S = pmin(S,.99))%>%
  mutate(S=S) %>%
  pull(S)
safetyStop <- fitdistrplus::fitdist(S,
                                     "truncnorm",fix.arg = list(a=0,b=1.1),
                                     start = list(mean = mean(S),sd = sd(S)/2))
meanStop <- safetyStop[[1]][1]
sdStop <- safetyStop[[1]][2]

hist(
  experimentalData  %>% filter(stops ==F,endPayoff > 0)%>%pull(S),freq = F)
lines(seq(0,2,.01),dtruncnorm(seq(0,2,.01),0,1,meanStop,sdStop))


summary(m1)
summary(safetyStop)
print(paste("k:",(m1$coefficients), 
            "safety level: mean: ", safetyStop[[1]][1] , "sd:",
            safetyStop[[1]][2]))



# fit for S>1
m1<-lm(I(freq-1)~0+endPayoff,weight=n,data=filter(fitData,noReturn==T))
kIgnore <- m1$coefficients
plot(seq(0,100),1+(seq(0,100)*kIgnore), type = "l")
points(filter(fitData,noReturn==T)%>%
         pull(endPayoff),filter(fitData,noReturn==T)%>%pull(freq))
summary(m1)

print("run: simulation exploit")
experimentalLevelIdStop <- seq(4001,8000)

exploitationSimulationDf <- tibble(userid = integer(), safetyLevelMean = numeric(),
                           safetyLevelSd = numeric(),ignoreLevel = integer(),
                           stopLevel = integer(), noiseLevel = numeric(),
                           step = integer(),levelid = integer(),rich = logical()) %>%
  complete(userid = 1, levelid = experimentalLevelIdStop, step = c(1:31),
           safetyLevelMean = meanStop, safetyLevelSd = sdStop,
           ignoreLevel = round(1/kIgnore)*-1,
           stopLevel = round(1/kStop),
           noiseLevel = bestEpsilon) %>%
  mutate(rich = ifelse((levelid %% 2 == 0), "rich", "poor")) %>%
  arrange(userid,levelid,safetyLevelMean,safetyLevelSd,stopLevel,noiseLevel,step)

exploitationSimulationDf <-
  exploitationSimulationDf%>%
  group_by(userid, levelid, noiseLevel, stopLevel,ignoreLevel, safetyLevelMean,safetyLevelSd) %>%
  mutate(sim = 
           oneExploitationSimulation(levelid, noiseLevel = noiseLevel, 
                             safetyLevelMean = safetyLevelMean,ignoreLevel=ignoreLevel,
                             safetyLevelSd = safetyLevelSd, stopLevel = stopLevel,step)) %>%
  separate(col = sim,sep = ",",into = c("position", "payoff", "behavior")) %>%
  mutate(position = as.integer(position),payoff = as.numeric(payoff),
    type = "simulation",step = step-1)

exploitationSimulationDf<- exploitationSimulationDf %>%ungroup()%>%
  mutate(maxValue = maxValue[levelid],normPayoff = payoff/maxValue)%>%
  group_by(userid,levelid) %>%
  mutate(maxTill = cummax(payoff), normMaxTill = cummax(normPayoff) ) %>%
  mutate(source="simulation")%>% group_by(userid,levelid) %>%
  mutate(positionMaxTill = positionOfMaxTill(payoff,position),
         positionMaxTillLast = positionOfMaxTillLast(payoff,position),
         distToMax = abs(position-positionMaxTill),
         distToMaxLast = abs(position-positionMaxTillLast),
         delta = distToMax - (30-seq(0,30)))

###
# combined phase
###

print("run: fit combined")

experimentalData <- combinedDf %>% group_by(levelid)%>% 
  mutate(endPositionX = positionX[31], endPositionY = positionY[31],
         endPayoff = payoff[31],endMaxPayoff = max(maxTill[31]))  %>% 
  group_by(userid,source,levelid,rich,endMaxPayoff) %>% 
  summarise(endMaxPeriod = which(endPayoff==payoff)[1],
            D_optimal = mean(floor((32 - endMaxPeriod)/2)),
            R = howFarFromEndPos2(positionX=positionX,positionY=positionY,
                                  endPositionX = endPositionX, endPositionY = endPositionY),
            S = R / D_optimal,
            endPayoff = mean(endPayoff)) %>% 
  ungroup() %>%mutate(stops = S==0,noReturn = S==1)


fitData<-experimentalData %>%
  group_by(endPayoff,stops,noReturn) %>% tally()%>% group_by(endPayoff)%>% mutate(freq=n/sum(n)) 

# fit for S=0
m2<-lm(freq~0+endPayoff,weight=n,data=filter(fitData,stops==T))
kCombined <- m2$coefficients
plot(seq(0,100),seq(0,100)*kCombined, type = "l")
points(filter(fitData,stops==T)%>%
         pull(endPayoff),filter(fitData,stops==T)%>%pull(freq))
# fit for S>0
S <- experimentalData  %>% 
  filter(stops ==F,endPayoff > 0, S<=1) %>%ungroup()%>% mutate(S = pmin(S,.99))%>%
  mutate(S=S) %>%
  pull(S)
safetyCombined <- fitdistrplus::fitdist(S,
                                    "truncnorm",fix.arg = list(a=0,b=1.1),
                                    start = list(mean = mean(S),sd = sd(S)/2))
meanCombined <- safetyCombined[[1]][1]
sdCombined <- safetyCombined[[1]][2]

hist(
  experimentalData  %>% filter(stops ==F,endPayoff > 0)%>%pull(S),freq = F)
lines(seq(0,2,.01),dtruncnorm(seq(0,2,.01),0,1,meanCombined,sdCombined))

# fit for S>1
m2<-lm(I(freq-1)~0+endPayoff,weight=n,data=filter(fitData,noReturn==T))
kIgnore <- m2$coefficients
plot(seq(0,100),1+(seq(0,100)*kIgnore), type = "l")
points(filter(fitData,noReturn==T)%>%
         pull(endPayoff),filter(fitData,noReturn==T)%>%pull(freq))
summary(m2)
print("combined phase")
summary(m2)
summary(safetyCombined)
print(paste("k:",(m2$coefficients), 
            "safety level: mean: ", meanCombined , "sd:",sdCombined))

print("run: simulation combine")
experimentalLevelId <- seq(8001,12000)

combinedSimulationDf <- tibble(
  userid = integer(),
  step = integer(),
  levelid = integer(),
  ignoreLevel= integer(),
  stopLevel = integer(),
  safetyLevelMean = numeric(),
  safetyLevelSd = numeric(),
  noiseLevel = numeric(),
  rich = logical()
) %>%
  complete(userid = 1,
           levelid = experimentalLevelId,
           ignoreLevel = round(1/(kIgnore))*-1,
           stopLevel = round(1/kCombined),
           safetyLevelMean = meanCombined, 
           safetyLevelSd = sdCombined, 
           noiseLevel = bestEpsilon,
           step = c(1:31)) %>%
  mutate(rich = ifelse((levelid %% 2 == 0), "rich", "poor"))


combinedSimulationDf <-
  combinedSimulationDf%>%
  group_by(userid, levelid,noiseLevel, stopLevel,  safetyLevelMean,safetyLevelSd) %>%
  mutate(sim = 
           oneCombinedSimulation(levelidP = levelid,
                                 
                                 ignoreLevel = ignoreLevel,
                                noiseLevel = noiseLevel, 
                                safetyLevelMean = safetyLevelMean,
                                safetyLevelSd = safetyLevelSd, 
                                stopLevel = stopLevel)) %>%
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


combinedSimulationDf<- combinedSimulationDf %>%ungroup()%>%
  mutate(position = getPosition(positionX,positionY),
         maxValue = maxValue[levelid],normPayoff = payoff/maxValue)%>%
  group_by(userid,levelid,stopLevel,ignoreLevel,safetyLevelMean,safetyLevelSd,noiseLevel) %>%
  mutate(maxTill = cummax(payoff), normMaxTill = cummax(normPayoff) )

