
if(!file.exists(here("/data_analysis/distance_matrix2D.RData"))){
landscapeSize <- 63
resultArray2D <- array(dim=c(landscapeSize,landscapeSize,landscapeSize^2)) 
for( xFix in 1:landscapeSize){
  for(yFix in 1:landscapeSize){
    index <- 1
    for( x in 1:landscapeSize){
      for(y in 1:landscapeSize){
        resultArray2D[xFix,yFix,index] <- (max(abs(xFix - x), abs(yFix - y)))
        index <- index + 1
      }
    }
  }
}

save(resultArray2D,file=here("/data_analysis/distance_matrix2D.RData"))
rm(landscapeSize)
}

getAreaMatrix2D <- function(x,y){
  return(matrix(resultArray2D[x,y,],byrow = T, nrow=63,ncol=63))
}
load(here("/data_analysis/distance_matrix2D.RData"))


if(!file.exists(here("/data_analysis/distance_matrix1D.RData"))){
  landscapeSize <- 63
  resultArray1D <- array(dim=c(landscapeSize,landscapeSize)) 
  for( xFix in 1:landscapeSize){
    index <- 1
    for( x in 1:landscapeSize){
      resultArray1D[xFix,index] <- abs(xFix - x)
      index <- index + 1
    }
  }
   save(resultArray1D,file=here("/data_analysis/distance_matrix1D.RData"))
  rm(landscapeSize)
}

load(here("/data_analysis/distance_matrix1D.RData"))

getAreaMatrix1D <- function(x){
  return(resultArray1D[x,])
}

safetyLevelCalc <- function(maxPayoff, ignoreLevel, stopLevel, safetyLevel){
  
  if(maxPayoff <= ignoreLevel ){return(1000)}
  if(maxPayoff > stopLevel ){return(0)}
  return(safetyLevel)
}

