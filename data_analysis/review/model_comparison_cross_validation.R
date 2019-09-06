library(here)
source(here("/data_analysis/_basic.R"))
source(here("/data_analysis/review/propabilistic_model.R"))
source(here("/data_analysis/review/_simulation_loops.R"))

landscapesMatrix <- read_rds(here("landscapes.rds"))
maxValue <- unlist(lapply(landscapesMatrix,max))

noise <- 0.17 #0.17 bestEpsilon
landscapeSize <- 63
step <- 1:31

result <- tibble()

combinedLevelIds <- combinedDf %>% pull(levelid) %>% unique()
combinedDf<- filter(movement,stage == 2)
combinedDf<- 
  full_join(combinedDf,tibble(levelid = combinedLevelIds) %>% sample_frac(1) %>% mutate(group = rep(1:5,200)))

for(i in 1:5){
print(i)


trainingId <- combinedDf %>% filter(group != i) %>% pull(levelid)
testId <- combinedDf %>% filter(group == i) %>% pull(levelid)

combinedDf <- combinedDf %>% mutate(training= levelid %in% trainingId)

experimentalData <- combinedDf %>% group_by(levelid)%>% filter(training)%>%
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
# fit for S>1
m2<-lm(I(freq-1)~0+endPayoff,weight=n,data=filter(fitData,noReturn==T))
kIgnore <- m2$coefficients

ignoreLevelP <<- round(1/(-1*kIgnore))
stopLevelP <<- round(1/kCombined)
safetyLevelMean <<- meanCombined
safetyLevelSd <<- sdCombined

print(paste(ignoreLevelP,stopLevelP,safetyLevelMean,safetyLevelSd))

####
# vary exploration
####

strategies <- c("random", "blind", "follow_gradient", "full","probabalistic_simple")

modelCompareExplorationDf <- tibble(levelid = integer(), strategy = character(),step = integer())%>%
  complete(levelid = testId, strategy = strategies, step = 1:31 ) %>% arrange(levelid,strategy,step)%>%
  mutate(rich = if_else((levelid %% 2 == 0), "rich", "poor"),
         clus = if_else(levelid > 12000, ("patchy"),"continuous"))

modelCompareExplorationDf <- modelCompareExplorationDf%>% group_by(levelid, strategy) %>%
  mutate(sim =  simulationExplore(levelid,strategy))


modelCompareExplorationDf <- modelCompareExplorationDf%>%
  separate(col = sim,sep = ",",
           into = c("positionX", "positionY","payoff"))%>%
  mutate(positionX = as.integer(positionX),positionY = as.integer(positionY),
         payoff = as.numeric(payoff),source = "simulation",step=step-1)



modelCompareExplorationDf <- bind_rows(modelCompareExplorationDf,combinedDf%>% filter(levelid %in% testId)%>%
                                         mutate(clus = "continuous",strategy = "experimental"))

#compare difference in mean payoff
##table
modelData <- modelCompareExplorationDf%>% filter(clus == "continuous") %>% group_by(strategy,step) %>%
  summarise(meanPayoff = mean(payoff)) 
experimentalData <- modelData%>% filter(strategy == "experimental") %>% ungroup()%>%
  mutate(targetPayoff = meanPayoff) %>% select(-meanPayoff,-strategy)
message("exploration payoff")
A <- full_join(modelData ,experimentalData, by = c("step" = "step"))%>% group_by(strategy) %>%
  summarise(difference = sum((targetPayoff-meanPayoff)^2)/31)%>% arrange(difference) %>%
  mutate(id = i,type = "payoff", source = "exploration")

#table
modelData <- modelCompareExplorationDf %>% 
  filter(strategy == "experimental"|levelid <= 9000)%>%
  mutate(position = getPosition(positionX,positionY))%>%  
  group_by(levelid,strategy) %>%
  mutate(duplicated = !duplicated(position)) %>%
  filter(duplicated)%>%
  group_by(strategy,position) %>%
  summarise(n = n()) %>%
  complete(position = 0:3968, fill = list(n=0))%>%
  group_by(strategy,position) %>%
  mutate(positionX = getX(position),positionY=getY(position))


message("exploration density")
B<-full_join(modelData,
             modelData %>% filter(strategy == "experimental")%>%
               ungroup()%>% mutate(targetN = n) %>% select(-n,-strategy))%>% 
  group_by(strategy) %>%
  summarise(difference = sum(abs(targetN-n),na.rm=T)/3969)%>% arrange(difference)%>%
  mutate(id = i,type = "density", source = "exploration")



####
# vary exploitation
####

strategies <- c("stop_first_peak","aspiration","aspiration_and_minimal","no_safety")
modelCompareExploitationDf <- tibble(levelid = integer(), strategy = character(),step = integer())%>%
  complete(levelid = testId, strategy = strategies, step = 1:31 ) %>% arrange(levelid,strategy,step)%>%
  mutate(rich = if_else((levelid %% 2 == 0), "rich", "poor"),
         clus = if_else(levelid > 12000, ("patchy"),"continuous"))


modelCompareExploitationDf <- modelCompareExploitationDf%>% group_by(levelid, strategy) %>%
  mutate(sim =  simulationExploit(levelid,strategy))


modelCompareExploitationDf <- modelCompareExploitationDf%>%
  separate(col = sim,sep = ",",
           into = c("positionX", "positionY","payoff"))%>%
  mutate(positionX = as.integer(positionX),positionY = as.integer(positionY),
         payoff = as.numeric(payoff),source = "simulation",step=step-1)



modelCompareExploitationDf <- bind_rows(modelCompareExploitationDf,combinedDf%>% filter(levelid %in% testId)%>%
                                          mutate(clus = "continuous",strategy = "experimental"))
##table
modelData <- modelCompareExploitationDf%>% filter(clus == "continuous") %>% group_by(strategy,step) %>%
  summarise(meanPayoff = mean(payoff)) 


experimentalData <- modelData%>% filter(strategy == "experimental") %>% ungroup()%>%
  mutate(targetPayoff = meanPayoff) %>% select(-meanPayoff,-strategy)
message("exploitation payoff")
C<-full_join(modelData ,experimentalData, by = c("step" = "step"))%>% group_by(strategy) %>%
  summarise(difference = sum((targetPayoff-meanPayoff)^2)/31)%>% arrange(difference)%>%
  mutate(id = i,type = "payoff", source = "exploitation")


#table

modelData <- modelCompareExploitationDf %>%
  filter(strategy == "experimental"|levelid <= 9000)%>%
  mutate(position = getPosition(positionX,positionY))%>%  
  group_by(levelid,strategy) %>%
  mutate(duplicated = !duplicated(position)) %>%
  filter(duplicated)%>%
  group_by(strategy,position) %>%
  summarise(n = n()) %>%
  complete(position = 0:3968, fill = list(n=0))%>%
  group_by(strategy,position) %>%
  mutate(positionX = getX(position),positionY=getY(position))
message("exploitation density")
D<-full_join(modelData,
             modelData %>% filter(strategy == "experimental")%>%
               ungroup()%>% mutate(targetN = n) %>% select(-n,-strategy))%>% 
  group_by(strategy) %>%
  summarise(difference = sum(abs(targetN-n),na.rm=T)/3969)%>% arrange(difference)%>%
  mutate(id = i,type = "density", source = "exploitation")
result <- bind_rows(result,bind_rows(A,B,C,D))

}

write_csv(result,here("cross_validated_fit_2.csv"),col_names = F,append = T)



