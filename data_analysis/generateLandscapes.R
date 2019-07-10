library(spatialfil)
setwd("C:/Users/yahosseini/workspace/code/ind-exploring-landscapes/stat/generator/")
source("optimizationFunctions.R")
mapSize = 63
smoothing = c(1);

peaks = c(32,512)
landscape<-matrix(0,nrow=mapSize,ncol=mapSize)
nbLandscapes <- 2000

par(mar = rep(.1, 4))
par(mfrow=c(1,4))

landscapeInfos<-data.frame(stringsAsFactors=F)
landscapeList<- vector("list",length = 2*nbLandscapes )
counter<-1
heightFunction <- function(peaks){return((10 * rnorm(peaks)^2))}
for(i in c(1:nbLandscapes)){
  print(i)
  for(p in peaks){
    landscape<-gaussLandscape(peaks = p,smoothing=smoothing,mapSize = mapSize,fun=heightFunction )
    landscapeList[[counter]] <-landscape[[1]]
    landscapeInfos<-rbind(landscapeInfos,list(counter-1,p,0))
    counter <- counter+1
  }
}
write(unlist(landscapeList),'landscape2Dsearch.csv',sep=',', ncolumns = mapSize^2)
counter<-1
for(i in c(1:nbLandscapes)){
  print(i)
  for(p in peaks){
    landscape<-gaussLandscape(peaks = p,smoothing=smoothing,mapSize = mapSize,fun=heightFunction )
    line<-1
    while(max(landscape[[1]][line,])<10){
      line<-line+1
    }
    landscapeList[[counter]] <- round((landscape[[1]][line,]/max(landscape[[1]][line,])) * sample(50:90,1))
    landscapeInfos<-rbind(landscapeInfos,list((2*nbLandscapes)-1+counter,p,1))
    counter <- counter+1
  }
}
write(unlist(landscapeList),'landscape1Dstop.csv',sep=',', ncolumns = mapSize)
counter<-1
for(i in c(1:nbLandscapes)){
  print(i)
  for(p in peaks){
    landscape<-gaussLandscape(peaks = p,smoothing=smoothing,mapSize = mapSize,fun=heightFunction )
    landscapeList[[counter]] <-landscape[[1]]
    landscapeInfos<-rbind(landscapeInfos,list((4*nbLandscapes)-1+counter,p,2))
    counter <- counter+1
  }
}
write(unlist(landscapeList),'landscape2Dboth.csv',sep=',', ncolumns = mapSize^2)


landscapeInfos <- data.frame(lapply(landscapeInfos, as.character), stringsAsFactors=FALSE)
write.table(landscapeInfos,'landscapesInfo.csv',sep=",",row.names = F,col.names = F,quote = F)

dummyL <- function(x,LL){
  landscape <- (x^4) * sample(55:85,1)
  LL = c(LL,round(as.vector(landscape)))
  LL = c(LL,round(as.vector(landscape[nrow(landscape):1,ncol(landscape):1])))
  return(LL)
}
dummyI <- function(x,LI,counter){
LI<-rbind(LI,list(counter,counter,0,0,counter,x))
LI<-rbind(LI,list(counter+1,counter,180,0,counter,x))
return(LI)
}



# landscapeList<-dummyL(wrapperOptCall(from=0,to=20,mapFun=levy),landscapeList)
# landscapeInfos<-dummyI("020levy",landscapeInfos,counter)
# counter<-counter+2
# 
# landscapeList<-dummyL(wrapperOptCall(from=-512,to=256,mapFun=egg ),landscapeList)
# landscapeInfos<-dummyI("-512256egg",landscapeInfos,counter)
# counter<-counter+2
# 
# landscapeList<-dummyL(wrapperOptCall(from=-256,to=512,mapFun=egg ),landscapeList)
# landscapeInfos<-dummyI("-256512egg",landscapeInfos,counter)
# counter<-counter+2
# 
# landscapeList<-dummyL(wrapperOptCall(from=-10,to=20,mapFun=crossit ),landscapeList)
# landscapeInfos<-dummyI("-1020crossit",landscapeInfos,counter)
# counter<-counter+2
# 
# landscapeList<-dummyL(wrapperOptCall(from=-20,to=10,mapFun=egg ),landscapeList)
# landscapeInfos<-dummyI("-2010crossit",landscapeInfos,counter)
# counter<-counter+2
# 
# landscapeList<-dummyL(wrapperOptCall(from=-7,to=7,mapFun=holder ),landscapeList)
# landscapeInfos<-dummyI("-77holder",landscapeInfos,counter)
# counter<-counter+2
# 
# landscapeList<-dummyL(wrapperOptCall(from=0,to=10,mapFun=langer ),landscapeList)
# landscapeInfos<-dummyI("010langer",landscapeInfos,counter)
# counter<-counter+2
# 
# landscapeList<-dummyL(wrapperOptCall(from=-2,to=2,mapFun=shubert ),landscapeList)
# landscapeInfos<-dummyI("-22shubert",landscapeInfos,counter)
# counter<-counter+2

