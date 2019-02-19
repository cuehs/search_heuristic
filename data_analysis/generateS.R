k <- 0.005136518
mu <- 0.8076932721442703
o2 <- 0.364125988931807 

s <- vector("numeric",90*10000)
  for(p in seq(1,90)){
    print(paste(((p-1)*10000)+1,((p)*10000)))
    tmp <- if_else(runif(10000) <= ((p-1) * k),
                   -1,
                   rtruncnorm(10000,mean=mu,sd = o2,a=0,b=1))
    print(length(tmp))
    s[(((p-1)*10000)+1):((p)*10000)]<-tmp
  }
simulationFigDf <- tibble(p = numeric(),id = numeric()) %>%complete(p = seq(1,90),id=seq(1,10000)) %>%mutate(s = s)


simulationFigDf %>% filter(((s>0) & (s <= 1)) | (s == -1))%>% mutate(s = pmax(0,s))%>%
  mutate(p = cut(p,breaks = 9, labels = F) ,
         s = cut(s,breaks = 10, labels = F)) %>%
  group_by(p,s) %>% tally() %>%
  group_by(p)%>% mutate(freq=n/sum(n))%>%
  ggplot(aes(p,s))+geom_tile(aes(fill=freq))+
  labs(x="final payoff", y="safety level", fill= "frequency")+scale_fill_viridis(option = "B")
