
participant<-read_csv(here("/data/participant.csv"),col_names = c('time','userid','sex','age'))
movement <- read_csv(here("/data/movement.csv"),col_names= c('time','stage','level','step','userid','type','position','payoff'))
structure <-read_csv(here("/data/structure.csv"),
                     col_names=c('userid','level', 'nbRounds','isCoop', 'isRich','stage','levelid'),
                     col_types = "iiillii")
#fix wrong structure
structure<-mutate(structure, isRich = (!isRich))

landscapesInfo<-read_csv(here("/data/landscapesInfo.csv"),col_names = c('levelid','nbPeaks','stage'))

movement<-left_join(movement,structure)

landscapesInfo<-mutate(landscapesInfo,levelid=levelid+1)
movement<-mutate(movement,levelid=levelid+1,level = level+1)

#changeme
landscapes<-scan(here("/data/landscape2Dsearch.csv"),what='list')
landscapes<-lapply(strsplit(landscapes,","),as.numeric)
landscapesMatrix<-lapply(landscapes,FUN=matrix,nrow=63,byrow=F)

landscapes<-scan(here("/data/landscape1Dstop.csv"),what='list')
landscapes<-lapply(strsplit(landscapes,","),as.numeric)
landscapesMatrix<-c(landscapesMatrix,lapply(landscapes,FUN=matrix,ncol=63,byrow=F))

landscapes<-scan(here("/data/landscape2Dboth.csv"),what='list')
landscapes<-lapply(strsplit(landscapes,","),as.numeric)
landscapesMatrix<-c(landscapesMatrix,lapply(landscapes,FUN=matrix,nrow=63,byrow=F))
rm(landscapes)

maxValue <- unlist(lapply(landscapesMatrix,max))
movement$positionX<-sapply(movement$position,FUN=getX)
movement$positionY<-sapply(movement$position,FUN=getY)
movement<-mutate(movement, maxValue = maxValue[levelid],normPayoff = payoff / maxValue )
movement<-mutate(movement, rich = ifelse(isRich,"rich","poor"))
movement<- select(movement,-isCoop,-isRich,-type,-nbRounds)




