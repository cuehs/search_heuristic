library(here)
source(here("/data_analysis/_basic.R"))

PLOTFIGURES <- F

tmp1 <- exploitationDf %>% group_by(levelid)%>% 
  mutate(endMaxPayoff = maxTill[31],
         endMaxPosition = positionMaxTill[31],
         endMaxPeriod = which(endMaxPosition==position)[1],
         endPayoff = payoff[31]) %>% 
  group_by(userid,source,levelid,rich,endMaxPayoff,endMaxPosition) %>% 
  summarise(D_optimal = mean(floor((32 - endMaxPeriod)/2)),
            R = howFarFromEndPos1(position=position,endPosition = endMaxPosition),
            S = R / D_optimal,
            endPayoff = mean(endPayoff)) %>% 
  ungroup()  %>%mutate(stops = S==0, noReturn = S > 1) %>% mutate(stage = "exploit")

tmp2 <- combinedDf %>% group_by(levelid)%>% 
  mutate(endPositionX = positionX[31], endPositionY = positionY[31],
         endPayoff = payoff[31],endMaxPayoff = max(maxTill[31]))  %>% 
  group_by(userid,source,levelid,rich,endMaxPayoff) %>% 
  summarise(endMaxPeriod = which(endPayoff==payoff)[1],
            D_optimal = mean(floor((32 - endMaxPeriod)/2)),
            R = howFarFromEndPos2(positionX=positionX,positionY=positionY,
                                  endPositionX = endPositionX, endPositionY = endPositionY),
            S = R / D_optimal,
            endPayoff = mean(endPayoff)) %>% 
  ungroup() %>%mutate(stops = S==0,noReturn = S==1) %>% mutate(stage = "combine")

#individual difference ins safety level
tmp4 <- bind_rows(tmp1,tmp2) %>% group_by(userid,stage) %>%
  filter(S<=1)%>% summarise(meanS = mean(S,na.rm=T),seS = sd(S) / n() ) %>% unite(explorationExtendedDf,meanS,seS)%>%
  spread(stage,explorationExtendedDf) %>%
  separate(combine,c("combineS","combineSe"),sep = "_",convert=T)%>%
  separate(exploit,c("exploitS","exploitSe"),sep = "_",convert=T)


tmp4 %>% ggplot(aes(exploitS,combineS))+geom_point(size = 2)+ 
  geom_errorbar(aes(ymin = combineS-combineSe,ymax=combineS+combineSe),size = 1,alpha = .5)+
  geom_errorbarh(aes(xmin = exploitS-exploitSe,xmax=exploitS+exploitSe),size = 1,alpha = .5)+
  labs(x="safety level in exploitation phase ", y="safety level in combined phase")+
  geom_abline(aes(slope = 1, intercept = 0),size = 1,alpha = .5,linetype = "dashed")
if(PLOTFIGURES){
ggsave("A3.1.8.png",height =4,width = 5)
ggsave("A3.1.8.pdf",height =4,width = 5)
}

if(!exists("noTime") | !exists("easyPlus")){
  source(here("/data_analysis/review/propabilistic_model.R"))
}



easyPlus %>%  ggplot()+geom_line(aes(maxVisiblePayoff,maximizePayoff,group=userid),alpha =  .5) + 
  geom_jitter(aes(maxVisiblePayoff,isMax),alpha = .2,data=explorationExtendedDf%>%group_by(maxVisiblePayoff,userid) %>%
                summarise(isMax = mean(isMaximizingPayoff)))+
  geom_line(aes(maxVisiblePayoff,maximizePayoff),size= 1.2,color="red",data=noTime)+
  labs(x="best neighbouring solution",y="probability to use payoff cue")

if(PLOTFIGURES){
ggsave("315.png",height =4,width = 5)
ggsave("315.pdf",height =4,width = 5)
}
