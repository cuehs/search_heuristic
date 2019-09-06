library(here)
source(here("/data_analysis/_basic.R"))
source(here("/data_analysis/review/_simulation_loops.R"))
source(here("/data_analysis/review/propabilistic_model.R"))

noise <- 0.17 #0.17 bestEpsilon
landscapeSize <- 63
step <- 1:31
maxValue <- unlist(lapply(landscapesMatrix,max))
landscapesMatrix <- read_rds(here("landscapes.rds"))

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

ignoreLevelP <- round(1/(-1*kIgnore))
stopLevelP <- round(1/kCombined)
safetyLevelMean <- meanCombined
safetyLevelSd <- sdCombined

print(paste(ignoreLevelP,stopLevelP,safetyLevelMean,safetyLevelSd))

####
# vary exploration
####

strategies <- c("random","full","blind","directed_random","follow_gradient",
                "probabalistic_time", "probabalistic_simple","probabalistic_enviorment")

strategies <- c("random", "blind", "follow_gradient", "full","probabalistic_simple")
strategies <- "full"
levelids <- seq(8001,12000)

modelCompareExplorationDf <- tibble(levelid = integer(), strategy = character(),step = integer())%>%
  complete(levelid = levelids, strategy = strategies, step = 1:31 ) %>% arrange(levelid,strategy,step)%>%
  mutate(rich = if_else((levelid %% 2 == 0), "rich", "poor"),
         clus = if_else(levelid > 12000, ("patchy"),"continuous"))%>% filter(rich=="poor")


modelCompareExplorationDf <- modelCompareExplorationDf%>% group_by(levelid, strategy) %>%
  mutate(sim =  simulationExplore(levelid,strategy))


modelCompareExplorationDf <- modelCompareExplorationDf%>%
  separate(col = sim,sep = ",",
           into = c("positionX", "positionY","payoff"))%>%
  mutate(positionX = as.integer(positionX),positionY = as.integer(positionY),
         payoff = as.numeric(payoff),source = "simulation",step=step-1)



modelCompareExplorationDf <- bind_rows(modelCompareExplorationDf,combinedDf%>% filter(rich == "poor")%>%
                                         mutate(clus = "continuous",strategy = "experimental"))

#compare difference in mean payoff

##table
modelData <- modelCompareExplorationDf%>% filter(clus == "continuous") %>% group_by(strategy,step) %>%
  summarise(meanPayoff = mean(payoff)) 
experimentalData <- modelData%>% filter(strategy == "experimental") %>% ungroup()%>%
  mutate(targetPayoff = meanPayoff) %>% select(-meanPayoff,-strategy)
message("exploration payoff")
A <- full_join(modelData ,experimentalData, by = c("step" = "step"))%>% group_by(strategy) %>%
  summarise(difference = sum((targetPayoff-meanPayoff)^2)/31)%>% arrange(difference)
message(A)


##figure
tmp <- 
  modelCompareExplorationDf%>%ungroup()%>%
  mutate(strategy = factor(strategy),
         strategy = recode(strategy, `1`= "experimental"),
         strategy = recode(strategy,
                           "full" = "Take-the-best",
                           "follow_gradient" = "Hill-climbing",
                           "blind" = "Blind search",
                           "probabalistic_simple" = "Probabilistic",
                           "random" = "Random search",
                           "experimental" = "Experimental data"))%>%
  group_by(strategy,step,rich,clus) %>%
  summarise(meanPayoff = mean(payoff)) 


tmp %>% filter(strategy != "Experimental data") %>%
  ggplot(aes(step,meanPayoff,color=rich))+geom_point(aes(shape = strategy)) + 
  geom_line(data = tmp %>% filter(strategy == "Experimental data"),size = 1.2) + facet_grid(~clus)+
  labs(color="landscape",linetype = "model")+
  labs(x="round", y="payoff")

#ggsave("A_exploration_payoff.pdf",height = 5, width = 8)

#compare difference in density
## figure
modelData <- modelCompareExplorationDf %>% 
  filter(strategy == "experimental" | levelid %in% c(8001:9000,12001:13000)) %>%
  mutate(position = getPosition(positionX,positionY))%>%  
  group_by(levelid,strategy) %>%
  mutate(duplicated = !duplicated(position)) %>%
  filter(duplicated)%>%
  group_by(rich,strategy,clus,position) %>%
  summarise(n = n()) %>%
  complete(position = 0:3968, fill = list(n=0))%>%
  group_by(rich,strategy,position,clus) %>%
  mutate(positionX = getX(position),positionY=getY(position))



modelData %>%ungroup()%>%
  mutate(n=log(n),n=if_else(n == -Inf,0,n),strategy = factor(strategy),
         strategy = fct_relevel(strategy, "experimental"),
         strategy = recode(strategy,
                           "full" = "Take-the-\nbest",
                           "follow_gradient" = "Hill-\nclimbing",
                           "blind" = "Blind\nsearch",
                           "probabalistic_simple" = "Probabilistic",
                           "random" = "Random\nsearch",
                           "experimental" = "Experimental\ndata"))%>%
  ggplot()+ geom_raster(aes(positionX,positionY,fill=(n)))+
  facet_grid(paste(clus,rich)~strategy)+ scale_fill_viridis(option = "B")+
  labs(x = "position x", y= "position y", fill = "log(density)")+
  theme(panel.grid.major = element_blank(),strip.text = element_text(size=10),
        strip.background = element_rect(fill="white", colour="black"))

#ggsave("A_exploration_density.pdf",height=8,width = 12)

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
B<- full_join(modelData,
          modelData %>% filter(strategy == "experimental")%>%
            ungroup()%>% mutate(targetN = n) %>% select(-n,-strategy))%>% 
  group_by(strategy) %>%
  summarise(difference = sum(abs(targetN-n),na.rm=T)/3969)%>% arrange(difference)
message(B)

#check presence of X-Shape
if((modelCompareExplorationDf %>% filter(strategy == "diagonal") %>% nrow())>0){
modelData <- modelCompareExplorationDf %>% 
  mutate(position = getPosition(positionX,positionY))%>%  
  filter(abs(positionX-positionY) < 1 | (positionX+positionY) == 64 )%>%
  group_by(levelid,strategy) %>%
  mutate(duplicated = !duplicated(position)) %>%
  filter(duplicated)%>%
  group_by(rich,strategy,clus,position) %>%
  summarise(n = n()) %>%
  complete(position = 0:3968, fill = list(n=0))%>%
  group_by(rich,strategy,position,clus) %>%
  mutate(positionX = getX(position),positionY=getY(position))

full_join(modelData,
          modelData %>% filter(strategy == "diagonal")%>%ungroup()%>% mutate(targetN = n) %>% select(-n,-strategy))%>% 
  group_by(strategy,rich) %>%
  summarise(difference = sum((targetN-n),na.rm=T)/126)%>% arrange(-difference)
}

####
# vary exploitation
####

strategies <- c("stop_first_peak","aspiration","aspiration_and_minimal","no_safety")



modelCompareExploitationDf <- tibble(levelid = integer(), strategy = character(),step = integer())%>%
  complete(levelid = levelids, strategy = strategies, step = 1:31 ) %>% arrange(levelid,strategy,step)%>%
  mutate(rich = if_else((levelid %% 2 == 0), "rich", "poor"),
         clus = if_else(levelid > 12000, ("patchy"),"continuous"))


modelCompareExploitationDf <- modelCompareExploitationDf%>% group_by(levelid, strategy) %>% mutate(sim =  simulationExploit(levelid,strategy))


modelCompareExploitationDf <- modelCompareExploitationDf%>%
  separate(col = sim,sep = ",",
           into = c("positionX", "positionY","payoff"))%>%
  mutate(positionX = as.integer(positionX),positionY = as.integer(positionY),
         payoff = as.numeric(payoff),source = "simulation",step=step-1)



modelCompareExploitationDf <- bind_rows(modelCompareExploitationDf,combinedDf%>% mutate(clus = "continuous",strategy = "experimental"))

#compare difference in mean payoff
tmp <- 
  modelCompareExploitationDf%>%ungroup()%>%
  mutate(strategy = factor(strategy),
         strategy = recode(strategy, `1`= "experimental"),
         strategy = recode(strategy,
                           "stop_first_peak" = "Early-stop",
                           "aspiration" = "Simple returning",
                           "aspiration_and_minimal" = "Returning",
                           "no_safety" = "Normative",
                           "experimental" = "Experimental data"))%>%
  group_by(strategy,step,rich,clus) %>%
  summarise(meanPayoff = mean(payoff)) 


tmp %>% filter(strategy != "Experimental data") %>%
  ggplot(aes(step,meanPayoff,color=rich))+geom_point(aes(shape = strategy)) + 
  geom_line(data = tmp %>% filter(strategy == "Experimental data"),size = 1.2) + facet_grid(~clus)+
  labs(color="landscape",linetype = "model")

ggsave("A_exploitation_payoff.pdf",height = 5, width = 8)

##table
modelData <- modelCompareExploitationDf%>% filter(clus == "continuous") %>% group_by(strategy,step) %>%
  summarise(meanPayoff = mean(payoff)) 


experimentalData <- modelData%>% filter(strategy == "experimental") %>% ungroup()%>%
  mutate(targetPayoff = meanPayoff) %>% select(-meanPayoff,-strategy)
message("exploitation payoff")
C<-full_join(modelData ,experimentalData, by = c("step" = "step"))%>% group_by(strategy) %>%
  summarise(difference = sum((targetPayoff-meanPayoff)^2)/31)%>% arrange(difference)
message(C)
#compare difference in density
modelData <- modelCompareExploitationDf %>% 
  filter(strategy == "experimental" | levelid %in% c(8001:9000,12001:13000)) %>%
  mutate(position = getPosition(positionX,positionY))%>%  
  group_by(levelid,strategy) %>%
  mutate(duplicated = !duplicated(position)) %>%
  filter(duplicated)%>%
  group_by(rich,strategy,clus,position) %>%
  summarise(n = n()) %>%
  complete(position = 0:3968, fill = list(n=0))%>%
  group_by(rich,strategy,position,clus) %>%
  mutate(positionX = getX(position),positionY=getY(position))



modelData %>%ungroup()%>%
  mutate(n=log(n),n=if_else(n == -Inf,0,n),strategy = factor(strategy),
         strategy = fct_relevel(strategy, "experimental"),
         strategy = recode(strategy,
                           "stop_first_peak" = "Early\nstop",
                           "aspiration" = "Simple\nreturning",
                           "aspiration_and_minimal" = "Returning",
                           "no_safety" = "Normative",
                           "experimental" = "Experimental\ndata"))%>%
  ggplot()+ geom_raster(aes(positionX,positionY,fill=(n)))+
  facet_grid(paste(clus,rich)~strategy)+ scale_fill_viridis(option = "B")+
  labs(x = "position x", y= "position y", fill = "log(density)")+
  theme(panel.grid.major = element_blank(),strip.text = element_text(size=10),
        strip.background = element_rect(fill="white", colour="black"))

ggsave("A_exploitation_density.pdf",height=8,width = 12)

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
  summarise(difference = sum(abs(targetN-n),na.rm=T)/3969)%>% arrange(difference)
message(D)

