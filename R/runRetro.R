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
  dirMy ="/home/laurence-kell/Desktop/papers/COM"
  dirDat=file.path(dirMy,"data")
  dirRes=file.path(dirMy,"results")
  dirFlr="~/Desktop/flr/mpb/R"
  
  load(file.path(dirDat,"ts.RData"))
  load(file.path(dirRes,"pd.RData"))
  
}else{
  dirMy="/rds/general/user/lkell/home/com"
  dirRes=dirMy
  dirFlr=dirMy
  dirRes=dirMy
  
  load(file.path(dirMy,"ts.RData"))
  load(file.path(dirMy,"pd.RData"))
  
  source(file.path(dirMy,'jabba-coerce.R'))
  source(file.path(dirMy,'jabba-com.R'))}

i=as.numeric(commandArgs(trailingOnly=TRUE)[1])
#if (is.na(i)) i=4

scenario=unique(ts$assessid)[i]
species =subset(ts,assessid==scenario)$species[1]

ctrl=mdply(expand.grid(r=c("low","high"),bmsyk=c(0.5,0.37,0.2)), function(r,bmsyk){
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
retro0<-foreach(j=seq(dim(ctrl)[1]), 
                .combine=rbind.fill,
                .multicombine=TRUE,
                .export =ls(globalenv()),
                .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
                  
                  source(file.path(dirFlr,'jabba-coerce.R'))
                  source(file.path(dirFlr,'jabba-com.R'))
                  
                  retro=mdply(data.frame(tail=as.numeric(dimnames(catch)$year[dim(catch)[2]])), function(tail) {
                    catch=window(catch,end=tail)
                    index=window(index,end=tail)
                    rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                                 bfinal=c(index[,dim(index)[2]]),index=index)
                    
                    if (!is.null(rtn)) rtn=cbind(tail=tail,ctrl[j,],perfect=TRUE,rtn)
                    
                    rtn})
                  
                  retro}    
save(retro0,file=file.path(dirRes,paste(scenario,"retro0.RData",sep="_")))

retro1<-foreach(j=seq(dim(ctrl)[1]), 
                .combine=rbind.fill,
                .multicombine=TRUE,
                .export =ls(globalenv()),
                .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
                  
                  source(file.path(dirFlr,'jabba-coerce.R'))
                  source(file.path(dirFlr,'jabba-com.R'))
                  
                  retro=mdply(data.frame(tail=as.numeric(dimnames(catch)$year[dim(catch)[2]-0:8])), function(tail) {
                    catch=window(catch,end=tail)
                    index=window(index,end=tail)
                    rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],index=index)
                    
                    if (!is.null(rtn)) rtn=cbind(tail=tail,ctrl[j,],perfect=TRUE,rtn)
                    
                    rtn})
                  
                  
                  retro}    
save(retro1,file=file.path(dirRes,paste(scenario,"retro1.RData",sep="_")))

## Actual & Actual
retro2<-foreach(j=seq(dim(ctrl)[1]), 
                .combine=rbind.fill,
                .multicombine=TRUE,
                .export =ls(globalenv()),
                .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
                  
                  source(file.path(dirFlr,'jabba-coerce.R'))
                  source(file.path(dirFlr,'jabba-com.R'))
                  
                  retro=mdply(data.frame(tail=as.numeric(dimnames(catch)$year[dim(catch)[2]-0:8])), function(tail) {
                    catch=window(catch,end=tail)
                    index=window(index,end=tail)
                    rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                                 b0=ifelse(is.na(c(index[,1])),1,c(index[,1])),
                                 bfinal=c(index[,dim(index)[2]]))
                    
                    if (!is.null(rtn)) rtn=cbind(tail=tail,ctrl[j,],perfect=TRUE,rtn)
                    
                    rtn})
                  
                  
                  retro}    
save(retro2,file=file.path(dirRes,paste(scenario,"retro2.RData",sep="_")))

## Heuristic & Actual
retro3<-foreach(j=seq(dim(ctrl)[1]), 
                .combine=rbind.fill,
                .multicombine=TRUE,
                .export =ls(globalenv()),
                .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
                  
                  source(file.path(dirFlr,'jabba-coerce.R'))
                  source(file.path(dirFlr,'jabba-com.R'))
                  
                  retro=mdply(data.frame(tail=as.numeric(dimnames(catch)$year[dim(catch)[2]-0:8])), function(tail) {
                    catch=window(catch,end=tail)
                    index=window(index,end=tail)
                    rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                                 bfinal=c(index[,dim(index)[2]]))
                    
                    if (!is.null(rtn)) rtn=cbind(tail=tail,ctrl[j,],perfect=TRUE,rtn)
                    
                    rtn})
                  
                  
                  retro}    
save(retro3,file=file.path(dirRes,paste(scenario,"retro3.RData",sep="_")))

## Actual & Heuristic
retro4<-foreach(j=seq(dim(ctrl)[1]), 
                .combine=rbind.fill,
                .multicombine=TRUE,
                .export =ls(globalenv()),
                .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
                  
                  source(file.path(dirFlr,'jabba-coerce.R'))
                  source(file.path(dirFlr,'jabba-com.R'))
                  
                  retro=mdply(data.frame(tail=as.numeric(dimnames(catch)$year[dim(catch)[2]-0:8])), function(tail) {
                    catch=window(catch,end=tail)
                    index=window(index,end=tail)
                    rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                                 b0=ifelse(is.na(c(index[,1])),1,c(index[,1])))
                    
                    if (!is.null(rtn)) rtn=cbind(tail=tail,ctrl[j,],perfect=TRUE,rtn)
                    
                    rtn})
                  
                  retro}    
save(retro4,file=file.path(dirRes,paste(scenario,"retro4.RData",sep="_")))

## Heuristic & Heuristic
retro5<-foreach(j=seq(dim(ctrl)[1]), 
                .combine=rbind.fill,
                .multicombine=TRUE,
                .export =ls(globalenv()),
                .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
                  
                  source(file.path(dirFlr,'jabba-coerce.R'))
                  source(file.path(dirFlr,'jabba-com.R'))
                  
                  retro=mdply(data.frame(tail=as.numeric(dimnames(catch)$year[dim(catch)[2]-0:8])), function(tail) {
                    catch=window(catch,end=tail)
                    index=window(index,end=tail)
                    rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"])
                    
                    if (!is.null(rtn)) rtn=cbind(tail=tail,ctrl[j,],perfect=TRUE,rtn)
                    
                    rtn})
                  
                  
                  retro}    
save(retro5,file=file.path(dirRes,paste(scenario,"retro5.RData",sep="_")))

