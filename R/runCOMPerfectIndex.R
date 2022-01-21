library(FLCore)
library(FLasher)
library(FLBRP)
library(ggplotFL)

library(plyr)
library(dplyr)
library(reshape)

library(mpb)
library(JABBA)

library(doParallel)
library(foreach)
cl=makeCluster(4)
registerDoParallel(cl)

if (osVersion=="Ubuntu 20.04.2 LTS"){
  dirMy ="/home/laurence-kell/Desktop/projects/fao/VoI/Report/FAO-VoI/results"
  dirFlr="~/Desktop/flr/mpb/R"
}else{
  dirMy="/rds/general/user/lkell/home/com"
  dirRes=dirMy
  dirFlr=dirMy
  source(file.path(dirMy,'jabba-coerce.R'))
  source(file.path(dirMy,'jabba-com.R'))}
dirRes=dirMy

load(file.path(dirMy,"ts.RData"))
load(file.path(dirMy,"pd.RData"))


for (scenario in unique(ts$assessid)){
    
  species =subset(ts,assessid==scenario)$species[1]
  
  ctrl=mdply(expand.grid(r=c("low","high"),bmsyk=c(0.5,0.37)), function(r,bmsyk){
    if (species%in%dimnames(pd)[[2]])
      if (r=="low") r=pd.low["r",species] else r=pd["r",species] else
        if (r=="low") r=apply(pd.low["r"],1,median) else r=apply(pd["r"],1,median)
        
        if (bmsyk<0.5) r=r/2
        
        c("r"=r)})
  
  catch=as.FLQuant(transmute(subset(ts,assessid==scenario),year=year,data=catch))
  catch=window(catch,end=max(dimnames(catch)$year[!is.na(catch)]))
  catch[is.na(catch)]=min(catch,na.rm=T)*0.001
  catch[catch==0]    =min(catch[catch>0],na.rm=T)*0.001
  index=as.FLQuant(transmute(subset(ts,assessid==scenario),year=year,data=stock))
  index=window(index,end=max(dimnames(catch)$year))
  
  if (scenario%in%c("NIWA-GSTRGZRSTA7-1964-2007-CORDUE","NWWG-GHALV-VI-XII-XIV-1960-2018-ICESIMP2018"))
    catch=catch/mean(catch)*1000
  
  ## Perfect fits
  r0.1<-foreach(j=seq(dim(ctrl)[1]), 
              .combine=rbind.fill,
              .multicombine=TRUE,
              .export =ls(globalenv()),
              .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
                
                source(file.path(dirFlr,'jabba-coerce.R'))
                source(file.path(dirFlr,'jabba-com.R'))
                
                ind=index
                ind[,seq(dim(ind)[2]-round(dim(index)[2]/2))]=NA
                rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                                  bfinal=FALSE,
                                  index=ind)
                
              rtn}
  save(r0.1,file=file.path(dirRes,paste(scenario,"r0.1.RData",sep="_")))
  }


withIndx=mdply(data.frame(scenario=unique(ts$assessid)), function(scenario) {
  load(file.path(dirRes,paste(scenario,"r0.1.RData",sep="_")))
  print(scenario)
  if (scenario=="WGHMM-ANGLVIIIc-IXa-1980-2013-ICESIMP2016") return(NULL)
  
  species =subset(ts,assessid==scenario)$species[1]
  
  ctrl=mdply(expand.grid(r=c("low","high"),bmsyk=c(0.5,0.37)), function(r,bmsyk){
    if (species%in%dimnames(pd)[[2]])
      if (r=="low") r=pd.low["r",species] else r=pd["r",species] else
        if (r=="low") r=apply(pd.low["r"],1,median) else r=apply(pd["r"],1,median)
        
        if (bmsyk<0.5) r=r/2
        
        c("r"=r)})
  
  data.frame(r0.1,bmsyk=rep(ctrl$bmsyk,each=dim(r0.1)[1]/4),
                  r    =rep(ctrl$r,    each=dim(r0.1)[1]/4))
  })

save(withIndx,file=file.path(dirMy,"withIndx.RData"))

