###
#convert one-number-positions to coordinates and vice versa
###
getX <- function(position) {
  x <- (position+1) %% 63
  if(x == 0)
  {
    return(x + 63)
  }
  else{
    return(x)
  }
}

getY <-
  function(position) {
    if((position+1) %% 63 ){
      return(floor((position+1) / 63)+1)}
    else{
      return(floor((position+1) / 63))
    }
  }

getPosition <- function(X,Y) {
  return (63*(Y-1) + (X-1))
}

###
# distance
###

euclidianDistance <- 
  function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

chessDistance <-
  function(x1, x2, y1, y2) {
    return(max(abs(x1 - x2), abs(y1 - y2)))
  }

###
# for search
###

maxVisiblePayoff <- function(levelid,positionX,positionY){
  landscape <- landscapesMatrix[[unique(levelid)]]
  notSeen <- matrix(T,nrow(landscape),ncol(landscape))
  maxPayoff <- NULL
  for(i in 1:length(positionX)){
    notSeen[positionX[i],positionY[i]] <- F
    visible <- ifelse(notSeen[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)], landscape[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)],-1)
    maxPayoff <- c(maxPayoff,max(visible))
    notSeen[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)] <- F
    
  }
  return(maxPayoff)
}
isPayoffInformativ <- function(levelid,positionX,positionY){
  landscape <- landscapesMatrix[[unique(levelid)]]
  notSeen <- matrix(T,nrow(landscape),ncol(landscape))
  isInformative <- NULL
  for(i in 1:length(positionX)){
    notSeen[positionX[i],positionY[i]] <- F
    visible <- ifelse(notSeen[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)],
                      landscape[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)],NA)
    isInformative <- c(isInformative,max(visible,na.rm=T) == min(visible,na.rm=T))
    notSeen[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)] <- F
    
  }
  return(isInformative)
}

maxVisiblePayoffPosition <- function(levelid,positionX,positionY){
  landscape <- landscapesMatrix[[unique(levelid)]]
  notSeen <- matrix(T,nrow(landscape),ncol(landscape))
  maxPayoff <- NULL
  for(i in 1:(length(positionX)-1)){
    notSeen[positionX[i],positionY[i]] <- F
    visible <- ifelse(notSeen[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)],
                      landscape[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)],-1)
    maxPosition <- which(visible==max(visible),arr.ind = T)
    closerToMax <- F
    for(n in nrow(maxPosition)){
      if((positionX[i+1] == (positionX[i] -2 + maxPosition[n,1])) |
         (positionY[i+1] == (positionY[i] -2 + maxPosition[n,2]))){
        closerToMax <- T
      }

      
    }
    maxPayoff <- c(maxPayoff, closerToMax)
    notSeen[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)] <- F
    
  }
  return(c(maxPayoff,NA))
}

isMaximizingUnvisitedPayoff <- function(levelid,positionX,positionY){
  #print(levelid)
  landscape <- landscapesMatrix[[unique(levelid)]]
  notSeen <- matrix(T,nrow(landscape),ncol(landscape))
  maxPayoff <- NULL
  for(i in 1:(length(positionX)-1)){
    notSeen[positionX[i],positionY[i]] <- F
    visible <- ifelse(notSeen[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)],
                      landscape[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)],NA)
    if (all(is.na(visible)))
    {
      maxPosition <- integer(0)
    }
    else{
      maxPosition <- which(visible == max(visible, na.rm = T), arr.ind = T)
    }
    if(length(maxPosition) == 0 || nrow(maxPosition) > 1 ){
      maxPayoff <- c(maxPayoff, NA)
    }else{
      #maxPayoff <- c(maxPayoff, T)
      if((positionX[i+1] == (positionX[i] -2 + maxPosition[1,1])) |
         (positionY[i+1] == (positionY[i] -2 + maxPosition[1,2]))){
          maxPayoff <- c(maxPayoff, T)
      }else{
        maxPayoff <- c(maxPayoff, F)
      }
    }
    notSeen[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)] <- F
  }
  return(c(maxPayoff,NA))
}

isMaximizingVisibility <- function(levelid,positionX,positionY){

  landscape <- landscapesMatrix[[unique(levelid)]]
  notSeen <- matrix(T,nrow(landscape),ncol(landscape))
  maxPayoff <- NULL
  for(i in 1:(length(positionX)-1)){
    notSeen[positionX[i],positionY[i]] <- F
    visible <- ifelse(notSeen[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)],
                      landscape[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)],NA)
    
    if (all(is.na(visible)))
    {
      maxPosition <- integer(0)
    }
    else{
      maxPosition <- which(visible == max(visible, na.rm = T), arr.ind = T)
    }
    if(length(maxPosition) == 0 || nrow(maxPosition) > 1){
      if((positionX[i] != positionX[i+1]) && (positionY[i] != positionY[i+1])){
        maxPayoff <- c(maxPayoff, T)
      }else{
        maxPayoff <- c(maxPayoff, F)
      }
    }else{
      maxPayoff <- c(maxPayoff, NA)
    }
    notSeen[positionX[i]+c(-1,0,1),positionY[i]+c(-1,0,1)] <- F
  }
  return(c(maxPayoff,NA))
}


####
# for stop
####

positionOfMaxTill <- function(x,position) {
  y<-c()
  for(i in 1:length(x)) {
    y<-c(y,position[which.max(x[1:(i)])])
  }
  
  return(y)
}

positionOfMaxTillLast <- function(x,position) {
  y<-c(0)
  for(i in 2:length(x)) {
    y<-c(y,position[which.max(x[1:(i-1)])])
  }
  
  return(y)
}

howFarFromEndPos1<-function(position,endPosition){
  endPositionFirstFound <- which(position == endPosition)[1]
  return(max(abs(position[endPositionFirstFound:length(position)]-position[endPositionFirstFound])))
}
###
# for combined
###

howFarFromEndPos2<-function(positionX,positionY,endPositionX,endPositionY){
  position <- getPosition(positionX,positionY)
  endPosition <- getPosition(endPositionX,endPositionY)
  endPositionFirstFound <- which(position == endPosition)[1]
  endPositionFirstFoundX <- getX(position[endPositionFirstFound])
  endPositionFirstFoundY <- getY(position[endPositionFirstFound])
  distance <- NULL
  for(i in endPositionFirstFound:length(positionX)){
    distance <-c(distance,chessDistance(positionX[i],endPositionFirstFoundX,positionY[i],endPositionFirstFoundY))
  }
  return(max(distance))
}

###
# visual stuff
###
            
fractionSeen<-function(x,y){
  seen<-matrix(0,nrow=65,ncol=65)
  for(i in 1:length(x)){
    seen[x[i]-1,y[i]-1]<-1
    seen[x[i]-1,y[i]]<-1
    seen[x[i]-1,y[i]+1]<-1
    seen[x[i],y[i]-1]<-1
    seen[x[i],y[i]]<-1
    seen[x[i],y[i]+1]<-1
    seen[x[i]+1,y[i]-1]<-1
    seen[x[i]+1,y[i]]<-1
    seen[x[i]+1,y[i]+1]<-1
  }
  seen<-seen[2:64,2:64]
  return(sum(seen))
}   


########
# PLOT #
########

plotTrajectory <- function(useridP,levelP,dataframe){
  
  tmp <- dplyr::filter(dataframe, userid == useridP, level == levelP)
  
  levelid <- unique(tmp %>% pull(levelid))
  X <- tmp %>% pull(positionX)
  Y <- tmp %>% pull(positionY)
  startX <- tmp %>% dplyr::filter(step == 1) %>% pull(positionX)
  startY<- tmp %>% dplyr::filter(step == 1) %>% pull(positionY)
  stopX<- tmp %>% dplyr::filter(step == 30) %>% pull(positionX)
  stopY<- tmp %>% dplyr::filter(step == 30) %>% pull(positionY)
  if(unique(tmp %>% pull(stage)) == 1){
    image.plot(x=c(1:63),y=c(1:3),z=base::matrix(rep(landscapesMatrix[[levelid]],3),nrow=63,ncol=3,byrow=F),col=viridis(100),useRaster=T)
    lines(X,rep(2,length(X)),col='red',lwd=2)
    points(startX,2,col="red",cex = 2,pch = 19)
    points(stopX,2,col="red",cex = 2,pch = 18)
  }else{
    image.plot(x=c(1:63),y=c(1:63),z=landscapesMatrix[[levelid]],col=viridis(100),useRaster=T)
    lines(X,Y,col='red',lwd=2)
    points(startX,startY,col="red",cex = 2,pch = 19)
    points(stopX,stopY,col="red",cex = 2,pch = 18)
  }
  
  for(i in 1:length(X)){
    positionXRadius <- pmax(pmin(c(X[i]-1,X[i],X[i]+1),63),1)
    positionYRadius <- pmax(pmin(c(Y[i]-1,Y[i],Y[i]+1),63),1)
    #    print(paste("step:",i,
    #                "hasgrad:", tmp$hasUsefulGradient[i],
    #                "maxpay:",tmp$willFollowGradient[i],
    #                "maxvis:",tmp$willMaximizeVisibility[i] ))
    #    print(landscapesMatrix[[levelid]][positionXRadius,positionYRadius])
  }
}



plotTrajectoryFromSimulation <- function(useridP,levelP,dataframe,safetyLevelP){
  library(fields)
  library(viridis)
  select <- dplyr::select
  filter <- dplyr::filter
  tmp <- dplyr::filter(dataframe, userid == useridP, levelid == levelP,safetyLevel == safetyLevelP)
  par(mar = c(2,2,1,1))
  levelid <- unique(tmp %>% pull(levelid))
  X <- tmp %>% pull(positionX)
  Y <- tmp %>% pull(positionY)
  startX <- tmp %>% dplyr::filter(step == 1) %>% pull(positionX)
  startY<- tmp %>% dplyr::filter(step == 1) %>% pull(positionY)
  stopX<- tmp %>% dplyr::filter(step == 30) %>% pull(positionX)
  stopY<- tmp %>% dplyr::filter(step == 30) %>% pull(positionY)

    image.plot(x=c(1:63),y=c(1:63),z=landscapesMatrix[[levelid]]/maxValue[[levelid]],col=viridis(100),useRaster=T)
    lines(X,Y,col='red',lwd=2)
    points(startX,startY,col="red",cex = 2,pch = 19)
    points(stopX,stopY,col="red",cex = 2,pch = 18)
}



plotTrajectoryFromCoordinates2D <- function(positionX,positionY,levelid){
  
  movementDf <- tibble(positionX = positionX, positionY =positionY)
  landscapeDf <- melt(landscapesMatrix[[levelid]]/maxValue[[levelid]]) %>% select(positionX=Var1,positionY = Var2,value = value)
  ggplot(landscapeDf) + geom_raster(aes(x=positionX,y=positionY,fill=value))+
    geom_point(aes(positionX,positionY),data=movementDf,color="#ca0020")+labs(x="X",y="Y",fill="")+
    scale_fill_viridis(option ="D",breaks = c(0,1),labels=c("0","1"))+
    labs(x="position x",y="position y",fill="norm.\npayoff\n")
}

plotTrajectoryFromCoordinates1D <- function(position,period,payoff,levelid){
movementDf <- tibble(position = position, period =period,maxTill = cummax(payoff),isNew = payoff >= maxTill)
#print(movementDf)
landscapeDf <- melt(base::matrix(rep(landscapesMatrix[[levelid]]/maxValue[[levelid]],31),nrow=63,ncol=31,byrow=F))%>%
  select(position=Var1,time = Var2,value=value)
ggplot(landscapeDf) + geom_raster(aes(x=position,y=time,fill=value))+
  geom_point(aes(position,period+1,shape=isNew),data = movementDf,color="#ca0020",size=2)+
  scale_fill_viridis(option ="D",breaks = c(0,1),labels=c("0","1"))+
  labs(x="position",y="round",fill="norm.\npayoff\n")+scale_shape(guide=FALSE)
}


