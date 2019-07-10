#########################
#        Exploration    #
#########################

oneExplorationSimulation <- function(levelidP,noiseLevel,step = 1:31 ) {
  
  levelid <- unique(levelidP)
  landscape <- landscapesMatrix[[levelid]]
  
  noiseLevel <- unique(noiseLevel)
  landscapeSize = nrow(landscape)
  positionX <- ceiling(landscapeSize/2)
  positionY <- ceiling(landscapeSize/2)
  currentPositionXL <- NULL
  currentPositionYL <- NULL
  currentBehaviorL <- NULL
  currentPayoffL <- NULL
  
  notSeen <- matrix(T, landscapeSize, landscapeSize)
  notVisited <- matrix(T, landscapeSize, landscapeSize)
  
  nextX<-NULL
  nextY<-NULL
  
  for (s in step) {
    # print(paste(levelid,s))
    # PREPERATION FOR DECISION
    currentPayoff <- landscape[positionX, positionY]
    
    positionXRadius <- pmax(pmin(c(positionX + seq(-1,1,1)), landscapeSize-1), 2)
    positionYRadius <- pmax(pmin(c(positionY + seq(-1,1,1)), landscapeSize-1), 2)
    
    # Rule 2: avoid already visited fields
    ## avoid to stay at the same place
    notVisited[positionX, positionY] <- F
    
    visibleNotVisitied <- notVisited[positionXRadius, positionYRadius]
    ## all next steps are visited, ignore the condition
    if(sum(visibleNotVisitied) == 0){visibleNotVisitied <- matrix(T, 3, 3)}
    visibleNotVisitiedCoordinates <- which(visibleNotVisitied, arr.ind = T)
    
    # Rule3: maximize payoff
    ##only considered fields which are not excluded in R1 or R2
    visiblePayoff <- matrix(-Inf, 3, 3)
    for (i in 1:nrow(visibleNotVisitiedCoordinates)) {
      visiblePayoff[visibleNotVisitiedCoordinates[i, 1], visibleNotVisitiedCoordinates[i, 2]] <-
        landscape[positionX - 2 + visibleNotVisitiedCoordinates[i, 1],
                  positionY - 2 + visibleNotVisitiedCoordinates[i, 2]]
    }
    
    bestPayoffMatrix <- visiblePayoff == max(visiblePayoff)
    bestPayoffCoordinates <-
      which((visiblePayoff == max(visiblePayoff, na.rm = T)), arr.ind = T)
    
    # Rule4: maximize visibility
    ## relevant for visiting calculation
    notSeen[positionXRadius, positionYRadius] <- F
    
    visibleExploration <- matrix(0, 3, 3)
    for (x in 1:nrow(visibleExploration)) {
      for (y in 1:ncol(visibleExploration)) {
        visibleExploration[x, y] <-
          sum(notSeen[positionXRadius[x] + seq(-1, 1), positionYRadius[y] + seq(-1, 1)])
      }
    }
    
    ##only considered fields which are not excluded in R3 (and as a extension in R2)
    visibleExploration<-ifelse(bestPayoffMatrix,visibleExploration,-Inf)
    
    bestVisibilityCoordinates <- 
      which(visibleExploration == max(visibleExploration, na.rm = T),arr.ind = T)
    
    # ACTUAL DECISION
    # just random movement on top
    if(runif(1) < noiseLevel){
      type <- "random"
      randomSelect <-
        base::sample(nrow(visibleNotVisitiedCoordinates), 1)
      nextX <- min(max(positionX - 2 + sample(1:3,1),2),62)
      nextY <- min(max(positionY - 2 + sample(1:3,1),2),62)
      #nextX <- positionX - 2 + visibleNotVisitiedCoordinates[randomSelect, "row"]
      #nextY <- positionY - 2 + visibleNotVisitiedCoordinates[randomSelect, "col"]
      # if only reachable field is not visitied
    } else if (nrow(visibleNotVisitiedCoordinates) == 1) {
      type <- "unvisited"
      nextX <- positionX - 2 + visibleNotVisitiedCoordinates[, "row"]
      nextY <- positionY - 2 + visibleNotVisitiedCoordinates[, "col"]
      # if only reachable field has the highest payoff
    } else if (nrow(bestPayoffCoordinates) == 1) {
      type <- "max payoff"
      nextX <- positionX - 2 + bestPayoffCoordinates[, "row"]
      nextY <- positionY - 2 + bestPayoffCoordinates[, "col"]
      # maximize visibility
    } else if (nrow(bestVisibilityCoordinates) < 9){
      type <-"max visibility"
      ## either one fields has the highest visibility
      if(nrow(bestVisibilityCoordinates) == 1){
        nextX <- positionX - 2 + bestVisibilityCoordinates[,"row"]
        nextY <- positionY - 2 + bestVisibilityCoordinates[,"col"]}
      ## or pick one of the possibilities randomly
      else{
        randomSelect <- base::sample(nrow(bestVisibilityCoordinates),1)
        nextX <- positionX - 2 + bestVisibilityCoordinates[randomSelect, "row"]
        nextY <- positionY - 2 + bestVisibilityCoordinates[randomSelect, "col"]
      }
    }
    
    # if T vizualizes the search
    if(s>100){
      par(mar = c(2,2,1,1))
      image(x=c(1:63),y=c(1:63),z=landscape,col=viridis(100),useRaster=T)
      points(positionX,positionY)
      readline(prompt=paste("allowed:",nrow(visibleNotVisitiedCoordinates),
                            "deci",type, "payoff",landscape[positionX, positionY]))
    }
    notVisited[positionXRadius, positionYRadius] <- F
    currentPayoffL <- c(currentPayoffL, currentPayoff)
    currentPositionXL <- c(currentPositionXL, unname(positionX))
    currentPositionYL <- c(currentPositionYL, unname(positionY))
    positionX <- unname(nextX)
    positionY <- unname(nextY)
    currentBehaviorL <- c(currentBehaviorL, type)
  }
  return(paste(currentPositionXL,currentPositionYL, currentPayoffL, currentBehaviorL, sep =","))
}

#########################
#         Exploitation  # 
#########################

oneExploitationSimulation <- function(levelidP, ignoreLevel = 1,stopLevel, noiseLevel, safetyLevelMean, safetyLevelSd, step = 1:31) {

  noiseLevel <- unique(noiseLevel)
  stopLevel<- unique(stopLevel)
  ignoreLevel<- unique(ignoreLevel)
  
  safetyLevelMean <- unique(safetyLevelMean)
  safetyLevelSd <- unique(safetyLevelSd)
  landscape <-landscapesMatrix[[unique(levelidP)]]
  landscapeSize <- length(landscapesMatrix[[unique(levelidP)]])
  allowedToVisit <- rep(T, landscapeSize)

  currentPosition <- 1
  maxPayoff <- 0
  maxPayoffPosition <- NULL
  
  currentPayoffL <- NULL
  currentPositionL <- NULL
  currentBehaviorL <- NULL
  
  stopLevel <- round(runif(1,1,stopLevel))
  ignoreLevel <- round(runif(1,0,ignoreLevel))
  print(stopLevel)
  print(ignoreLevel)
  safetyLevel <- min(1,max(0,rtruncnorm(1,a=0,b=1.1,safetyLevelMean,safetyLevelSd)))

  for(s in step){
    timeLeft <- length(step)-s
    currentPayoff <- landscape[currentPosition]
    
    if((currentPayoff >= maxPayoff)  ){
      maxPayoff <- currentPayoff
      maxPayoffPosition <- currentPosition
    }
    positionRadius <- pmin(pmax(1,seq(currentPosition-1,currentPosition+1)),landscapeSize)
    
    
    # Rule 1: stay inside circle
    areaMatrix <- getAreaMatrix1D(maxPayoffPosition)
    areaMatrix[currentPosition+1] <- areaMatrix[currentPosition+1]+1

    insideArea <- (areaMatrix) < floor(max(1,(timeLeft)*safetyLevelCalc(maxPayoff,ignoreLevel, stopLevel,safetyLevel)))
    #insideArea <- areaMatrix < floor(max(1,(timeLeft)*S))
    visibleInsideArea <- insideArea[positionRadius]
    #print(visibleInsideArea)
    ## all next steps are outside of the circle, ignore the condition
    if(landscape[currentPosition+1] > maxPayoff){
      visibleInsideArea[3]<-T
    }
    if(sum(visibleInsideArea) == 0){
      visibleInsideArea <- c(T,T,T)
      maxPayoff <-0
      maxPayoffPosition <- currentPosition
      }
    visibleInsideAreaCoordinates <- which(visibleInsideArea, arr.ind = T)
    #print(visibleInsideAreaCoordinates)
    
    
    # Rule 2: avoid already visited fields
    ## avoid to stay at the same place
    allowedToVisit[currentPosition] <- F
    visibleAllowedToVisit <- allowedToVisit[positionRadius]

    ## all next steps are visited, ignore the condition
    visibleAllowedToVisit <- visibleInsideArea & visibleAllowedToVisit
    if(sum(visibleAllowedToVisit) == 0){visibleAllowedToVisit <- visibleInsideArea}
    #visibleAllowedToVisit <- visibleInsideArea & visibleAllowedToVisit
    visibleAllowedToVisitCoordinates <- which(visibleAllowedToVisit, arr.ind = T)
    #print(visibleAllowedToVisitCoordinates)
    
    
    # Rule3: maximize payoff
    ##only considered fields which are not excluded in R1 or R2
    visiblePayoff <- rep(-Inf,3)
    for (i in 1:length(visibleAllowedToVisitCoordinates)) {
      visiblePayoff[visibleAllowedToVisitCoordinates[i]] <-
        landscape[max(currentPosition - 2 + visibleAllowedToVisitCoordinates[i],1)]
    }

    bestPayoffMatrix <- visiblePayoff == max(visiblePayoff)
    bestPayoffCoordinates <-
      which((visiblePayoff == max(visiblePayoff, na.rm = T)), arr.ind = T)
    #print(bestPayoffCoordinates)
    # Rule 4: maximize visibility
    # no need to calculate, just move to the most right allowed field
    
    ##search
    # just random movement on top
    if((runif(1) < noiseLevel) &
       (safetyLevelCalc(maxPayoff,ignoreLevel, stopLevel,safetyLevel) > .001)){
      type <- "random"
      randomSelect <-
        base::sample(length(visibleAllowedToVisitCoordinates), 1)
      nextPosition <- currentPosition -2 + visibleAllowedToVisitCoordinates[randomSelect]
      
      # if only one reachable field is inside the circle
    } else if(length(visibleInsideAreaCoordinates) == 1){
      type <- "stay inside"
      nextPosition <- currentPosition -2 + visibleInsideAreaCoordinates
      
      # if only one reachable field is not visitied
    } else if (length(visibleAllowedToVisitCoordinates) == 1) {
      type <- "unvisited"
      nextPosition <- currentPosition -2 + visibleAllowedToVisitCoordinates
    } else{
      
      type <- "payoff"
      # print(paste(levelidP,"payoff"))
      #print(bestPayoffCoordinates)
      randomSelect <-   base::sample(length(bestPayoffCoordinates),1)
      nextPosition <- currentPosition -2 + bestPayoffCoordinates[randomSelect]
    }
    
    if(F){
      par(mar = c(2,2,1,1))
      image(x=c(1:63),z=t(landscape),col=viridis(100),useRaster=T)
      image(x=c(1:63),z=t(t(insideArea)),col= grey.colors(2,alpha=.5),add=T,useRaster=T)
      points(currentPosition,0,col="red")
      readline(prompt=paste("inside:",length(visibleInsideAreaCoordinates), "visited", length(visibleAllowedToVisitCoordinates),
                            "deci",type, "payoff",landscape[currentPosition]))
    }
    
    allowedToVisit[positionRadius] <- F
    currentPayoffL <- c(currentPayoffL, currentPayoff)
    currentPositionL <- c(currentPositionL, unname(currentPosition))
    currentBehaviorL <- c(currentBehaviorL, type)
    currentPosition<-max(nextPosition,1)
  }
  return(paste(currentPositionL,currentPayoffL, currentBehaviorL, sep =","))
}

#########################
#        COMBINED       # combine3
#########################

oneCombinedSimulation <- function(levelidP, ignoreLevel, stopLevel, noiseLevel, safetyLevelMean,safetyLevelSd, step = 1:31) {
  
  levelid <- unique(levelidP)
  landscape <- landscapesMatrix[[levelid]]
  ignoreLevelP <-  unique(ignoreLevel)
  stopLevelP <- unique(stopLevel)
  safetyLevelMean<-unique(safetyLevelMean)
  safetyLevelSd<-unique(safetyLevelSd)
  noiseLevel <- unique(noiseLevel)
  landscapeSize = nrow(landscape)
  positionX <- ceiling(landscapeSize/2)
  positionY <- ceiling(landscapeSize/2)
  currentPositionXL <- NULL
  currentPositionYL <- NULL
  currentBehaviorL <- NULL
  currentPayoffL <- NULL
  
  notSeen <- matrix(T, landscapeSize, landscapeSize)
  notVisited <- matrix(T, landscapeSize, landscapeSize)
  
  goRandom <- F
  nextX<-NULL
  nextY<-NULL
  
  maxPayoff <- 0
  stopLevel <- runif(1,1,stopLevelP)
  ignoreLevel <- (runif(1,0,ignoreLevelP))
  
  while(stopLevel  < (ignoreLevel)){
    ignoreLevel <- (runif(1,0,ignoreLevelP))
    stopLevel <- runif(1,1,stopLevelP)
    #print(paste(stopLevel,ignoreLevel))
  }
  
  safetyLevel <- min(1,max(0,rtruncnorm(1,a=0,b=1,safetyLevelMean,safetyLevelSd)))
  
  for (s in step) {
    
    # PREPERATION FOR DECISION
    currentPayoff <- landscape[positionX, positionY]
    if(currentPayoff >= maxPayoff){
      maxPayoff <- currentPayoff
      maxPayoffPositionX <- positionX
      maxPayoffPositionY <- positionY
    }
    positionXRadius <- pmax(pmin(c(positionX + seq(-1,1,1)), landscapeSize-1), 2)
    positionYRadius <- pmax(pmin(c(positionY + seq(-1,1,1)), landscapeSize-1), 2)
    timeLeft <- length(step)-s
    
    # Rule 1: stay inside circle
    insideArea <- getAreaMatrix2D(maxPayoffPositionX,maxPayoffPositionY) < 
      floor(max(2,(timeLeft)*safetyLevelCalc(maxPayoff,ignoreLevel, stopLevel,safetyLevel)))
    visibleInsideArea <- insideArea[positionXRadius, positionYRadius]
    
    ## all next steps are outside of the circle, ignore the condition
    if(sum(visibleInsideArea) == 0){visibleInsideArea <- matrix(T,3,3)}
    visibleInsideAreaCoordinates <- which(visibleInsideArea, arr.ind = T)
    
    # Rule 2: avoid already visited fields
    ## avoid to stay at the same place
    
    notVisited[positionX, positionY] <- F
    visibleNotVisitied <- notVisited[positionXRadius, positionYRadius]
    visibleNotVisitied <- visibleInsideArea & visibleNotVisitied

    ## if satisifed staying is an option
    if(safetyLevelCalc(maxPayoff,ignoreLevel, stopLevel,safetyLevel) < .001 | (s+1 >= max(step))){
      visibleNotVisitied <- visibleInsideArea
    }

    ## all next steps are visited, ignore the condition
    if(sum(visibleNotVisitied) == 0){visibleNotVisitied <- visibleInsideArea}
    
    visibleNotVisitiedCoordinates <- which(visibleNotVisitied, arr.ind = T)
    
    # Rule3: maximize payoff
    ##only considered fields which are not excluded in R1 or R2
    visiblePayoff <- matrix(-Inf, 3, 3)
    for (i in 1:nrow(visibleNotVisitiedCoordinates)) {
      visiblePayoff[visibleNotVisitiedCoordinates[i, 1], visibleNotVisitiedCoordinates[i, 2]] <-
        landscape[positionX - 2 + visibleNotVisitiedCoordinates[i, 1],
                  positionY - 2 + visibleNotVisitiedCoordinates[i, 2]]
    }
    
    bestPayoffMatrix <- visiblePayoff == max(visiblePayoff)
    bestPayoffCoordinates <-
      which((visiblePayoff == max(visiblePayoff, na.rm = T)), arr.ind = T)
    
    # Rule4: maximize visibility
    ## relevant for visiting calculation
    notSeen[positionXRadius, positionYRadius] <- F
    
    visibleExploration <- matrix(0, 3, 3)
    for (x in 1:nrow(visibleExploration)) {
      for (y in 1:ncol(visibleExploration)) {
        visibleExploration[x, y] <-
          sum(notSeen[positionXRadius[x] + seq(-1, 1), positionYRadius[y] + seq(-1, 1)])
      }
    }
    
    ##only considered fields which are not excluded in R3 (and as a extension in R2 and R1)
    visibleExploration<-ifelse(bestPayoffMatrix,visibleExploration,-Inf)
    bestVisibilityCoordinates <- 
      which(visibleExploration == max(visibleExploration, na.rm = T),arr.ind = T)
    
    
    
    # ACTUAL DECISION
    ## just random movement on top
    ## if we are satisfied no reason to move random
    if(runif(1) < noiseLevel &
       (safetyLevelCalc(maxPayoff,ignoreLevel, stopLevel,safetyLevel) > .001) &
       (s+1 < max(step))){
      type <- "random"
      randomSelect <-
        base::sample(nrow(visibleNotVisitiedCoordinates), 1)
      nextX <- positionX - 2 + visibleNotVisitiedCoordinates[randomSelect, "row"]
      nextY <- positionY - 2 + visibleNotVisitiedCoordinates[randomSelect, "col"]
      
      # if only one reachable field is inside the circle
    } else if(nrow(visibleInsideAreaCoordinates) == 1){
      type <- "stay inside"
      nextX <- positionX - 2 + visibleInsideAreaCoordinates[, "row"]
      nextY <- positionY - 2 + visibleInsideAreaCoordinates[, "col"]
      # if only one reachable field is not visitied
    } else if (nrow(visibleNotVisitiedCoordinates) == 1) {
      type <- "unvisited"
      nextX <- positionX - 2 + visibleNotVisitiedCoordinates[, "row"]
      nextY <- positionY - 2 + visibleNotVisitiedCoordinates[, "col"]
      # if only one reachable field has the highest payoff
    } else if (nrow(bestPayoffCoordinates) == 1) {
      type <- "max payoff"
      nextX <- positionX - 2 + bestPayoffCoordinates[, "row"]
      nextY <- positionY - 2 + bestPayoffCoordinates[, "col"]
      # maximize visibility
    } else if (nrow(bestVisibilityCoordinates) < 9){
      type <-"max visibility"
      ## either one fields has the highest visibility
      if(nrow(bestVisibilityCoordinates) == 1){
        nextX <- positionX - 2 + bestVisibilityCoordinates[,"row"]
        nextY <- positionY - 2 + bestVisibilityCoordinates[,"col"]}
      ## or pick one of the possibilities randomly
      else{
        randomSelect <-   base::sample(nrow(bestVisibilityCoordinates),1)
        nextX <- positionX - 2 + bestVisibilityCoordinates[randomSelect, "row"]
        nextY <- positionY - 2 + bestVisibilityCoordinates[randomSelect, "col"]
      }
    }
    # if T vizualizes the search
    if(s>100){
      par(mar = c(2,2,1,1))
      image(x=c(1:63),y=c(1:63),z=landscape,col=viridis(100),useRaster=T)
      image(x=c(1:63),y=c(1:63),z=insideArea,col= grey.colors(2,alpha=.5),add=T,useRaster=T)
      points(positionX,positionY)
      readline(prompt=paste("allowed:",nrow(visibleInsideAreaCoordinates),
                            "deci",type, "payoff",landscape[positionX, positionY]))
    }
    notVisited[positionXRadius, positionYRadius] <- F
    currentPayoffL <- c(currentPayoffL, currentPayoff)
    currentPositionXL <- c(currentPositionXL, unname(positionX))
    currentPositionYL <- c(currentPositionYL, unname(positionY))
    positionX <- unname(nextX)
    positionY <- unname(nextY)
    currentBehaviorL <- c(currentBehaviorL, type)
    
  }
  return(paste(currentPositionXL,currentPositionYL, currentPayoffL, currentBehaviorL, sep =","))
}
