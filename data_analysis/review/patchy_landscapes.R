#generate 1000 patchy landscapes and save them in landscapeMatrix (id 12001-13000)
library(here)
library(spatialfil)
source(here("/data_analysis/_basic.R"))

patchyLandscape<-function(peaks,parameter,print=F)
{
  mapSize <- 63
  
  landscape<-matrix(0,nrow=mapSize,ncol=mapSize)
  
  position <- sample(seq(13,53),2,replace =T )
  X<-round(truncnorm::rtruncnorm(peaks,1,mapSize-1,as.numeric(position[1]),parameter))
  Y<-round(truncnorm::rtruncnorm(peaks,1,mapSize-1,as.numeric(position[2]),parameter))
  
  
  for(i in 1:peaks){
    tmpscape<-matrix(0,nrow=mapSize,ncol=mapSize)
    tmpscape[X[i],Y[i]]<-(10 * rnorm(1)^2)
    Mfil <- applyFilter(x = tmpscape, kernel = convKernel(sigma = 1, k ='gaussian'))
    landscape = pmax(landscape , Mfil)
    
  }
  if(print){
    image.plot(floor(landscape / max(landscape)* sample(50:90,1)))
  }
  return(floor(landscape/max(landscape)* sample(50:90,1)))
  
}



for(i in 1:1000){
  print(i)
  if(i%%2){
  landscapesMatrix[[12000+i]] <-  patchyLandscape(32,3)
  }else{
  landscapesMatrix[[12000+i]] <-  patchyLandscape(512,3)
  }
}