tmp1 <- exploitationDf %>% group_by(levelid)%>%
  mutate(endMaxPayoff = maxTill[31],
         endMaxPosition = positionMaxTill[31],
         endMaxPeriod = which(endMaxPosition==position)[1],
         endPayoff = payoff[31]) %>% 
  group_by(userid,source,levelid,rich,endMaxPayoff,endMaxPosition ) %>% 
  summarise(D_optimal = mean(floor((32 - endMaxPeriod)/2)),
            R = howFarFromEndPos1(position=position,endPosition = endMaxPosition),
            S = R / D_optimal,
            endPayoff = mean(endPayoff),
            endMaxPeriod = min(endMaxPeriod)) %>% 
  ungroup()  %>%mutate(stops = S==0, noReturn = S > 1) %>% mutate(stage = "exploit") 


tmp2 <- exploitationSimulationDf %>% group_by(levelid)%>%
  mutate(endMaxPayoff = maxTill[31],
         endMaxPosition = positionMaxTill[31],
         endMaxPeriod = which(endMaxPosition==position)[1],
         endPayoff = payoff[31]) %>% 
  group_by(userid,source,levelid,rich,endMaxPayoff,endMaxPosition ) %>% 
  summarise(D_optimal = mean(floor((32 - endMaxPeriod)/2)),
            R = howFarFromEndPos1(position=position,endPosition = endMaxPosition),
            S = R / D_optimal,
            endPayoff = mean(endPayoff),
            endMaxPeriod = min(endMaxPeriod)) %>% 
  ungroup()  %>%mutate(stops = S==0, noReturn = S > 1) %>% mutate(stage = "exploit") 

 A <- tmp1  %>% ungroup() %>%
  filter(endPayoff > 0, S<=1 ) %>%
  mutate(endPayoff = cut(endPayoff,breaks = 10, labels = F) * 8.5,
         S = cut(S,breaks = 10, labels = F)*.1) %>%
  mutate(endMaxPeriod = if_else(endMaxPeriod < 10, "early",if_else(endMaxPeriod < 20 ,"intermediate", "late")))%>%
  group_by(endMaxPeriod,endPayoff,S) %>%
  tally() %>% mutate(freq=n/sum(n))%>%
  ungroup()%>%
  ggplot(aes(endPayoff,S,fill=freq))+geom_tile()+
  labs(x=expression('payoff P'[best]), y="safety level S", fill= "frequency")+ 
  scale_fill_viridis(option = "B")+
  theme(panel.grid.major = element_blank(),strip.text = element_text(size=14),
        strip.background = element_rect(fill="white", colour="black")) +facet_wrap(~endMaxPeriod)


B <- tmp2  %>% ungroup() %>%
  filter(endPayoff > 0, S<=1 ) %>%
  mutate(endPayoff = cut(endPayoff,breaks = 10, labels = F) * 8.5,
         S = cut(S,breaks = 10, labels = F)*.1) %>%
  mutate(endMaxPeriod = if_else(endMaxPeriod < 10, "early",if_else(endMaxPeriod < 20 ,"intermediate", "late")))%>%
  group_by(endMaxPeriod,endPayoff,S) %>%
  tally() %>% mutate(freq=n/sum(n))%>%
  ungroup()%>%
  ggplot(aes(endPayoff,S,fill=freq))+geom_tile()+
  labs(x=expression('payoff P'[best]), y="safety level S", fill= "frequency")+ 
  scale_fill_viridis(option = "B")+
  theme(panel.grid.major = element_blank(),strip.text = element_text(size=14),
        strip.background = element_rect(fill="white", colour="black")) +facet_wrap(~endMaxPeriod)
  
  
plot_grid(A,B,nrow=2,labels = c("A","B"))
ggsave("A3.1.9.png", height =8,width = 12)
ggsave("A3.1.9.pdf", height =8,width = 12)