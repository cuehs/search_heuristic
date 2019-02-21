library(here)
source(here("/data_analysis/_basic.R"))

#needs simulations to be run before
#source(here("/data_analysis/run_and_fit_simulation.R"))

###
# exploration
###


# example trajectories

tmp<- explorationDf %>% filter(userid == 34, level == 8)
A<-plotTrajectoryFromCoordinates2D(tmp %>% pull(positionX),tmp %>% pull(positionY),tmp %>% pull(levelid) %>% unique()) 
C <- get_legend(A)
tmp<- explorationDf %>% filter(userid == 20, level == 20)
B<- plotTrajectoryFromCoordinates2D(tmp %>% pull(positionX),tmp %>% pull(positionY),tmp %>% pull(levelid) %>% unique())
plot_grid(B+ theme(legend.position='none'),A+ theme(legend.position='none'),C,nrow = 1,rel_widths=c(3,3,.5),labels=c("poor","rich",NULL) )
# describtive
ggsave("000_exploration_example_trajectory.pdf",height =4,width = 9)


explorationBehaSimu <- bind_rows(explorationDf,
                            explorationSimulationDf%>%mutate(source = "simulation"))%>%
  ungroup()%>%
  mutate(source = 
           ifelse(source == "behavioral",
                  "experimental\ndata\n", "numerical\nsimulation"))
tmp<-explorationBehaSimu %>% group_by(rich,step,source) %>%
  summarise(meanMaxPayoff = mean(normMaxTill), sd = sd(normMaxTill)/sqrt(n()))  


tmp%>%ggplot(aes(step,meanMaxPayoff,color=factor(rich),linetype = source))+
  geom_line(size=1.2)+geom_errorbar(aes(ymin =meanMaxPayoff-sd,ymax=meanMaxPayoff+sd,fill = factor(rich)),
                                    alpha=.5,data = tmp %>% filter(source =="experimental\ndata\n" ))+
  labs(x="round",y="mean maximal norm. payoff", color="landscape")

ggsave("001_exploration_payoff.pdf",height =4,width = 5)

levelidnumbers <- c(base::sample(seq(1,4000,2),500),
                    base::sample(seq(2,4000,2),500))
explorationBehaSimuMini <- bind_rows(explorationDf,explorationSimulationDf %>%
                                  filter(levelid %in%levelidnumbers)%>%
                                  mutate(source = "simulation"))

explorationBehaSimuMini %>%  
  group_by(userid,levelid,source,rich) %>%
  mutate(duplicated = !duplicated(position)) %>% filter(duplicated)%>% group_by(source,rich) %>%
  group_by(rich,source,position) %>%
  summarise(n = log(n())) %>%
  complete(position = 0:3968, fill = list(n=0))%>%
  group_by(rich,source,position) %>%
  mutate(positionX = getX(position),positionY=getY(position)) %>% ungroup()%>%
  mutate(source = ifelse(source == "behavioral", "experimental data", "numerical simulation"))%>%
  ggplot()+ geom_raster(aes(positionX,positionY,fill=n))+
  facet_grid(rich~source)+ scale_fill_viridis(option = "B")+
  labs(x = "position x", y= "position y", fill = "log(density)")+
  theme(panel.grid.major = element_blank(),strip.text = element_text(size=14),
        strip.background = element_rect(fill="white", colour="black"))
ggsave("002_exploration_density.pdf",height =8,width = 11)

A <- explorationBehaSimu %>%
  group_by(userid, source, levelid, rich, position) %>% tally() %>%
  summarise(revisit = 1-(sum(n - 1) / 30))  %>%
  ggplot(aes(x = rich, y = revisit,fill=source)) + geom_boxplot() + 
  coord_cartesian() + labs(x = "", y = "freq. non-visitied rule")
B<- explorationBehaSimu %>%
  group_by(userid,source, levelid, rich)  %>%
  mutate( isMaximizingPayoff =  isMaximizingUnvisitedPayoff(levelid, positionX, positionY)) %>%
  group_by(userid,source, levelid, rich) %>%
  summarise(isMaximizingPayoff = mean(isMaximizingPayoff, na.rm = T))%>%
  ggplot(aes(x = paste(rich), y = isMaximizingPayoff,fill=source))+
  geom_boxplot(outlier.shape = NA) + labs(x = "", y = "freq. payoff rule")
C<-explorationBehaSimu %>% mutate(source=ifelse(source == "experimental\ndata\n","experimental data", "numerical simulation"))%>%
  group_by(userid,source, levelid, rich)  %>%
  mutate( isMaximizingVisibility =  isMaximizingVisibility(levelid, positionX, positionY)) %>%
  summarise(isMaximizingVisibility = mean(isMaximizingVisibility, na.rm = T))%>%
  ggplot(aes(paste(rich), isMaximizingVisibility, fill = source)) + 
  geom_boxplot() + labs(x ="", y = "freq. novelty rule")
plot_grid((A+ theme(legend.position='none')),
          (B+ theme(legend.position='none')),
          (C+ theme(legend.position='none')),
          NULL,get_legend(C),NULL,nrow = 2,rel_widths=c(3,3,3),
          rel_heights = c(1,.2),labels=c("A","B","C",NULL),align = "v",axis="t")
ggsave("010_rule_follow.pdf",height =4,width = 12)

###
# exploitation
###


# example trajectories

landscapeDf <- melt(landscapesMatrix[[4010]]/ maxValue[[4010]]) %>% 
  select(positionX=Var1,positionY = Var2,value = value)

A<-landscapeDf %>% ggplot(aes(positionY,value))+
  geom_line() + geom_point(aes(color=value),size=2)+
  scale_color_viridis()+labs(x = "position",
                             y= "normalized payoff",color="normalised\npayoff\n")
tmp<- exploitationDf %>% filter(userid == 0, level == 38)
B<- plotTrajectoryFromCoordinates1D(tmp %>% pull(position),
                                    tmp %>% pull(step),
                                    tmp %>% pull(payoff),
                                    tmp %>% pull(levelid) %>% unique())
C <- get_legend(B)

plot_grid(A+ theme(legend.position='none'),
          B+ theme(legend.position='none'),
          C,nrow = 1,rel_widths=c(3,3,.7),labels=c("A","B",NULL) )
ggsave("100_exploitation_example_trajectory_.pdf",height =4,width = 9)

exploitationBehaSimu <- bind_rows(exploitationDf,exploitationSimulationDf)%>%
  mutate(source = ifelse(source == "behavioral",
                  "experimental\ndata\n", "numerical\nsimulation"))
exploitationBehaSimu <- exploitationBehaSimu %>% group_by(userid,source,levelid) %>%
  arrange(source,levelid,step) %>%
  mutate(direction = position - lead(position),
         behavior = case_when(
           direction == 0 ~"exploit",
           direction < 0 ~ "explore",
           direction > 0 ~ "exploit")) 
tmp<-exploitationBehaSimu%>%
  group_by(step,source,rich) %>%
  summarise(meanPayoff=mean(normPayoff),se=sd(normPayoff)/sqrt(n()))
A <-  tmp%>%
  ggplot(aes(step,meanPayoff,color=rich,linetype = source)) +
  geom_errorbar(aes(ymin = meanPayoff - se, ymax=meanPayoff + se), data = tmp %>% filter(source =="experimental\ndata\n" ),alpha=.5)+
  geom_line(size=1.2)+
  labs(x="round", y="mean norm. payoff",linetype ="source",color="landscape")

B<-exploitationBehaSimu %>%  filter(step < 30)%>%
  group_by(source,step,behavior)  %>% tally()  %>% 
  group_by(source,step)%>% mutate(freq=n/sum(n,na.rm=T))%>% ungroup %>%
  complete(step,behavior,source, fill = list(freq=0))%>% ggplot(aes(step, freq, color=factor(behavior),linetype = source)) +
  geom_line(size=1.2)+labs(x="round", y="frequency",linetype ="source",color="behavior")+scale_color_viridis(discrete =T )
plot_grid(A,B,labels = c("A","B"))
ggsave("103_exploitation_behavior.pdf",height =4,width = 10)

bind_rows(exploitationDf,exploitationSimulationDf )%>%
  group_by(source,userid,levelid) %>%
  mutate(endPosition = position[31],
         endPayoff = payoff[31],
         endMaxPosition = positionMaxTill[31],
         endMaxPeriod = which(endMaxPosition==position)[1]) %>% 
  group_by(source,userid,levelid,endPosition,endMaxPosition,endMaxPeriod,rich) %>% 
  summarise(D_optimal = mean(floor((32 - endMaxPeriod)/2)),
            R = howFarFromEndPos1(position=position,endPosition = endMaxPosition),
            S = R / D_optimal,
            endPayoff = mean(endPayoff)) %>%
  ungroup() %>%
  filter(endPayoff > 0, S<=1) %>%
  mutate(endPayoff = cut(endPayoff,breaks = 10, labels = F) * 8.5,
         S = cut(S,breaks = 10, labels = F)*.1) %>% 
  group_by(source,endPayoff,S) %>% tally() %>% mutate(freq=n/sum(n))%>%complete(endPayoff,S,fill=list(freq=0))%>%ungroup()%>%
  mutate(source = ifelse(source == "behavioral", "A experimental data", "B numerical simulation")) %>% 
  ggplot(aes(endPayoff,S))+geom_tile(aes(fill=freq))+
  labs(x=expression('payoff P'[best]), y="safety level S", fill= "frequency")+ 
  scale_fill_viridis(option = "B")+facet_wrap(~source)+
  theme(panel.grid.major = element_blank(),strip.text = element_text(size=14),
        strip.background = element_rect(fill="white", colour="black"))
  
ggsave("111_exploitation_payoff_safety_density.pdf",height =4,width = 7)
  


###
# combined
###

combinedBehaSimu <- bind_rows(combinedDf,
                             combinedSimulationDf%>% mutate(source = "simulation"))%>%
  mutate(source = ifelse(source == "behavioral",
                         "experimental\ndata", "numerical\nsimulation"))

tmp<-combinedBehaSimu%>%
  group_by(step,source,rich) %>%
  summarise(meanPayoff=mean(normPayoff),se=sd(normPayoff)/sqrt(n()))
tmp%>%
  ggplot(aes(step,meanPayoff,color=rich,linetype = source)) +
  geom_errorbar(aes(ymin = meanPayoff - se, ymax=meanPayoff + se), data = tmp %>% filter(source =="experimental\ndata" ),alpha=.5)+
  geom_line(size=1.2)+
  labs(x="round", y="mean norm. payoff",linetype ="source",color="landscape")
ggsave("200_combined_payoff.pdf",height =4,width = 5)

levelidnumbers <- c(base::sample(seq(8001,12000,2),500),
                    base::sample(seq(8002,12000,2),500))
combinedBehaSimuMini <- filter(combinedBehaSimu,
                              source == "experimental\ndata" | levelid %in%levelidnumbers)
  
  
combinedBehaSimuMini %>%  
  group_by(userid,levelid,source) %>%
  mutate(duplicated = !duplicated(position)) %>% filter(duplicated)%>%
  group_by(rich,source,position) %>%
  summarise(n = log(n())) %>%
  complete(position = 0:3968, fill = list(n=0))%>%
  group_by(rich,source,position) %>%
  mutate(positionX = getX(position),positionY=getY(position)) %>%
  ggplot()+ geom_raster(aes(positionX,positionY,fill=n))+
  facet_grid(rich~source)+ scale_fill_viridis(option = "B")+
  labs(x = "position x", y= "position y", fill = "log(density)")+
  theme(panel.grid.major = element_blank(),strip.text = element_text(size=14),
        strip.background = element_rect(fill="white", colour="black"))
ggsave("201_combined_density.pdf",height =8,width = 11)
  