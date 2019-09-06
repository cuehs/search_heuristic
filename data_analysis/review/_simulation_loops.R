simulationExploit <- function(levelid,name){
  
  stopLevelBackup <- stopLevelP 
  ignoreLevelBackup <- ignoreLevelP 
  safetyLevelMeanBackup <- safetyLevelMean 
  safetyLevelSdBackup <- safetyLevelSd 
  
  levelid <- unique(levelid)
  name <- unique(name)
  if((levelid %% 100) == 1){
  print(paste(levelid,name,ignoreLevelP,stopLevelP))}
  
  if(name == "stop_first_peak"){
    ignoreLevelP <<- 1
    stopLevelP <<-2
    tmp <- oneCombinedSimulationN(levelid,T,T,T)
    ignoreLevelP <<- ignoreLevelBackup
    stopLevelP <<- stopLevelBackup
  }
  if(name == "aspiration"){
    ignoreLevelP <<- 1
    tmp <- oneCombinedSimulationN(levelid,T,T,T)
    ignoreLevelP <<- ignoreLevelBackup
    stopLevelP <<- stopLevelBackup
  }
  if(name == "aspiration_and_minimal"){
    tmp <- oneCombinedSimulationN(levelid,T,T,T)
  }
  if(name == "no_safety"){
    safetyLevelMean <<- .9
    safetyLevelSd <<- .001
    stopLevelP <<-1000
    tmp <- oneCombinedSimulationN(levelid,T,T,T)
    safetyLevelMean <<- safetyLevelMeanBackup 
    safetyLevelSd <<- safetyLevelSdBackup 
    stopLevelP <<- stopLevelBackup
  }
  return(tmp)
}


simulationExplore <- function(levelid,name){
  levelid <- unique(levelid)
  name <- unique(name)
  if((levelid %% 100) == 1){
    print(paste(levelid,name,ignoreLevelP,stopLevelP))
    print(paste(ignoreLevelP,stopLevelP,safetyLevelMean,safetyLevelSd))
    }
  if(name == "random"){
    return(oneCombinedSimulationN(levelid,F,F,F))
  }
  if(name == "full"){
    return(oneCombinedSimulationN(levelid,T,T,T))
  }
  if(name == "blind"){
    return(oneCombinedSimulationN(levelid,T,F,T))
  }
  if(name == "diagonal"){
    eps <<-noise
    noise <<-0 
    x<-(oneCombinedSimulationN(levelid,T,F,T))
    noise <<- eps
    return(x)
  }
  if(name == "directed_random"){
    return(oneCombinedSimulationN(levelid,T,F,F))
  }
  if(name == "follow_gradient"){
    return(oneCombinedSimulationN(levelid,T,T,F))
  }
  if(name == "probabalistic_time"){
    return(oneTradeoffSimulation(levelid,"time"))
  }
  if(name == "probabalistic_enviorment"){
    return(oneTradeoffSimulation(levelid,"poor"))
  }
  if(name == "probabalistic_simple"){
    return(oneTradeoffSimulation(levelid,F))
  }
}


oneCombinedSimulationN <- function(levelid, nonVisitiedCue, payoffCue, visibilityCue ) {
  
  landscape <- landscapesMatrix[[levelid]]
  
  positionX <- ceiling(landscapeSize/2)
  positionY <- ceiling(landscapeSize/2)
  
  stopLevel <- runif(1,1,stopLevelP)
  ignoreLevel <- (runif(1,0,ignoreLevelP))
  
  while(stopLevel  < (ignoreLevel)){
    ignoreLevel <- (runif(1,0,ignoreLevelP))
    stopLevel <- runif(1,1,stopLevelP)
    #print(paste(stopLevel,ignoreLevel))
  }
  
  safetyLevel <- min(1,max(0,rtruncnorm(1,a=0,b=1,safetyLevelMean,safetyLevelSd)))
  
  notSeen <- matrix(T, landscapeSize, landscapeSize)
  notVisited <- matrix(T, landscapeSize, landscapeSize)
  
  currentPositionXL <- NULL
  currentPositionYL <- NULL
  currentPayoffL <- NULL
  nextX<-NULL
  nextY<-NULL
  
  maxPayoff <- 0
  currentPayoff <- 0
  
  
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
    if(nonVisitiedCue){
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
    }else{
      visibleNotVisitied <- visibleInsideArea
      visibleNotVisitiedCoordinates <- visibleInsideAreaCoordinates
    }
    # Rule3: maximize payoff
    ##only considered fields which are not excluded in R1 or R2
    if(payoffCue){
      visiblePayoff <- matrix(-Inf, 3, 3)
      for (i in 1:nrow(visibleNotVisitiedCoordinates)) {
        visiblePayoff[visibleNotVisitiedCoordinates[i, 1], visibleNotVisitiedCoordinates[i, 2]] <-
          landscape[positionX - 2 + visibleNotVisitiedCoordinates[i, 1],
                    positionY - 2 + visibleNotVisitiedCoordinates[i, 2]]
      }
      
      bestPayoffMatrix <- visiblePayoff == max(visiblePayoff)
      bestPayoffCoordinates <-
        which((visiblePayoff == max(visiblePayoff, na.rm = T)), arr.ind = T)
    }else{
      bestPayoffMatrix <- visibleNotVisitied
      bestPayoffCoordinates <- visibleNotVisitiedCoordinates
    }
    # Rule4: maximize visibility
    ## relevant for visiting calculation
    
    notSeen[positionXRadius, positionYRadius] <- F
    if(visibilityCue){
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
    }else{
      bestVisibilityCoordinates <- bestPayoffCoordinates
    }
    
    
    # ACTUAL DECISION
    ## just random movement on top
    ## if we are satisfied no reason to move random
    if(runif(1) < noise &
       (safetyLevelCalc(maxPayoff,ignoreLevel, stopLevel,safetyLevel) > .001) &
       (s+1 < max(step))){
      
      randomSelect <-
        base::sample(nrow(visibleNotVisitiedCoordinates), 1)
      nextX <- positionX - 2 + visibleNotVisitiedCoordinates[randomSelect, "row"]
      nextY <- positionY - 2 + visibleNotVisitiedCoordinates[randomSelect, "col"]
      
      # if only one reachable field is inside the circle
    } else if(nrow(visibleInsideAreaCoordinates) == 1){
      
      nextX <- positionX - 2 + visibleInsideAreaCoordinates[, "row"]
      nextY <- positionY - 2 + visibleInsideAreaCoordinates[, "col"]
      # if only one reachable field is not visitied
    } else if (nrow(visibleNotVisitiedCoordinates) == 1) {
      
      nextX <- positionX - 2 + visibleNotVisitiedCoordinates[, "row"]
      nextY <- positionY - 2 + visibleNotVisitiedCoordinates[, "col"]
      # if only one reachable field has the highest payoff
    } else if (nrow(bestPayoffCoordinates) == 1) {
      
      nextX <- positionX - 2 + bestPayoffCoordinates[, "row"]
      nextY <- positionY - 2 + bestPayoffCoordinates[, "col"]
      # maximize visibility
    } else if (nrow(bestVisibilityCoordinates) <= 9){
      
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
    if(s>1000){
      par(mar = c(2,2,1,1))
      image(x=c(1:63),y=c(1:63),z=landscape,col=viridis(100),useRaster=T)
      image(x=c(1:63),y=c(1:63),z=insideArea,col= grey.colors(2,alpha=.5),add=T,useRaster=T)
      points(positionX,positionY)
      readline(prompt=paste("allowed:",nrow(visibleInsideAreaCoordinates), "payoff",landscape[positionX, positionY]))
    }
    notVisited[positionXRadius, positionYRadius] <- F
    currentPayoffL <- c(currentPayoffL, currentPayoff)
    currentPositionXL <- c(currentPositionXL, unname(positionX))
    currentPositionYL <- c(currentPositionYL, unname(positionY))
    positionX <- unname(nextX)
    positionY <- unname(nextY)
    
  }
  return(paste(currentPositionXL,currentPositionYL, currentPayoffL, sep =","))
}

oneTradeoffSimulation <- function(levelid, type ) {
  
  landscape <- landscapesMatrix[[levelid]]
  
  positionX <- ceiling(landscapeSize/2)
  positionY <- ceiling(landscapeSize/2)
  
  
  stopLevel <- runif(1,1,stopLevelP)
  ignoreLevel <- (runif(1,0,ignoreLevelP))
  
  while(stopLevel  < (ignoreLevel)){
    ignoreLevel <- (runif(1,0,ignoreLevelP))
    stopLevel <- runif(1,1,stopLevelP)
    #print(paste(stopLevel,ignoreLevel))
  }
  
  
  safetyLevel <- min(1,max(0,rtruncnorm(1,a=0,b=1,safetyLevelMean,safetyLevelSd)))
  
  notSeen <- matrix(T, landscapeSize, landscapeSize)
  notVisited <- matrix(T, landscapeSize, landscapeSize)
  
  currentPositionXL <- NULL
  currentPositionYL <- NULL
  currentPayoffL <- NULL
  nextX<-NULL
  nextY<-NULL
  
  maxPayoff <- 0
  currentPayoff <- 0
  
  
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
    
    if(type == "time"){
      if(runif(1) < filter(withTime,maxVisiblePayoff == max(visiblePayoff),step == s) %>% pull(maximizePayoff)){
        payoffCue <- T
        visibilityCue <- F
      }else{
        payoffCue <- F
        visibilityCue <- T
      }
    } else if(type == "poor"){
      leveltype <- ifelse(levelid%%2,"poor","rich")
      if(runif(1) < filter(poorRich,maxVisiblePayoff == max(visiblePayoff),rich == leveltype) %>% pull(maximizePayoff)){
        payoffCue <- T
        visibilityCue <- F
      }else{
        payoffCue <- F
        visibilityCue <- T
      }
    }else{
      if(runif(1) < filter(noTime,maxVisiblePayoff == max(visiblePayoff)) %>% pull(maximizePayoff)){
        payoffCue <- T
        visibilityCue <- F
      }else{
        payoffCue <- F
        visibilityCue <- T
      }
    }
    if(!payoffCue){
      bestPayoffMatrix <- visibleNotVisitied
      bestPayoffCoordinates <- visibleNotVisitiedCoordinates
    }
    # Rule4: maximize visibility
    ## relevant for visiting calculation
    
    notSeen[positionXRadius, positionYRadius] <- F
    if(visibilityCue){
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
    }else{
      bestVisibilityCoordinates <- bestPayoffCoordinates
    }
    
    
    # ACTUAL DECISION
    ## just random movement on top
    ## if we are satisfied no reason to move random
    if(runif(1) < noise &
       (safetyLevelCalc(maxPayoff,ignoreLevel, stopLevel,safetyLevel) > .001) &
       (s+1 < max(step))){
      
      randomSelect <-
        base::sample(nrow(visibleNotVisitiedCoordinates), 1)
      nextX <- positionX - 2 + visibleNotVisitiedCoordinates[randomSelect, "row"]
      nextY <- positionY - 2 + visibleNotVisitiedCoordinates[randomSelect, "col"]
      
      # if only one reachable field is inside the circle
    } else if(nrow(visibleInsideAreaCoordinates) == 1){
      
      nextX <- positionX - 2 + visibleInsideAreaCoordinates[, "row"]
      nextY <- positionY - 2 + visibleInsideAreaCoordinates[, "col"]
      # if only one reachable field is not visitied
    } else if (nrow(visibleNotVisitiedCoordinates) == 1) {
      
      nextX <- positionX - 2 + visibleNotVisitiedCoordinates[, "row"]
      nextY <- positionY - 2 + visibleNotVisitiedCoordinates[, "col"]
      # if only one reachable field has the highest payoff
    } else if (nrow(bestPayoffCoordinates) == 1) {
      
      nextX <- positionX - 2 + bestPayoffCoordinates[, "row"]
      nextY <- positionY - 2 + bestPayoffCoordinates[, "col"]
      # maximize visibility
    } else if (nrow(bestVisibilityCoordinates) <= 9){
      
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
    if(s>1000){
      par(mar = c(2,2,1,1))
      image(x=c(1:63),y=c(1:63),z=landscape,col=viridis(100),useRaster=T)
      image(x=c(1:63),y=c(1:63),z=insideArea,col= grey.colors(2,alpha=.5),add=T,useRaster=T)
      points(positionX,positionY)
      readline(prompt=paste("allowed:",nrow(visibleInsideAreaCoordinates), "payoff",landscape[positionX, positionY]))
    }
    notVisited[positionXRadius, positionYRadius] <- F
    currentPayoffL <- c(currentPayoffL, currentPayoff)
    currentPositionXL <- c(currentPositionXL, unname(positionX))
    currentPositionYL <- c(currentPositionYL, unname(positionY))
    positionX <- unname(nextX)
    positionY <- unname(nextY)
    
  }
  return(paste(currentPositionXL,currentPositionYL, currentPayoffL, sep =","))
}