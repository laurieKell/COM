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

i=as.numeric(commandArgs(trailingOnly=TRUE)[1])
if (is.na(i)) i=4

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
r0<-foreach(j=seq(dim(ctrl)[1]), 
            .combine=rbind.fill,
            .multicombine=TRUE,
            .export =ls(globalenv()),
            .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
              
            source(file.path(dirFlr,'jabba-coerce.R'))
            source(file.path(dirFlr,'jabba-com.R'))
            
            rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                         bfinal=c(index[,dim(index)[2]]),index=index)
              
            if (!is.null(rtn)) rtn=cbind(ctrl[j,],perfect=TRUE,rtn)
              
            rtn}
save(r0,file=file.path(dirRes,paste(scenario,"r0.RData",sep="_")))


## Perfect fits
r0.1<-foreach(j=seq(dim(ctrl)[1]), 
            .combine=rbind.fill,
            .multicombine=TRUE,
            .export =ls(globalenv()),
            .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
              
              source(file.path(dirFlr,'jabba-coerce.R'))
              source(file.path(dirFlr,'jabba-com.R'))
              
              rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                           b0=ifelse(is.na(c(index[,1])),1,c(index[,1])),
                           bfinal=c(index[,dim(index)[2]]),index=index)
            
              ind=index
              ind[,seq(dim(ind)[2]-round(dim(index)[2]/2))]=NA
              rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                           b0=ifelse(is.na(c(index[,1])),1,c(index[,1])),
                           bfinal=c(index[,dim(index)[2]]),
                           index=ind)
              rtn.5=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                                b0=ifelse(is.na(c(index[,1])),1,c(index[,1])),
                                bfinal=FALSE,
                                index=ind)
              
              rtn=rbind.fill(cbind(run="COM",rtn.10.),
                             cbind(run="+Index",rtn.5))
              
              rtn=merge(rtn,as.data.frame(index,drop=T),by="year")
              ggplot(rtn)+
                geom_point(aes(year,index))+
                geom_line(aes(year,stock,col=run))+
                geom_line(aes(year,data))
              
              rtn}
save(r0.1,file=file.path(dirRes,paste(scenario,"r0.1.RData",sep="_")))

r1<-foreach(j=seq(dim(ctrl)[1]), 
            .combine=rbind.fill,
            .multicombine=TRUE,
            .export =ls(globalenv()),
            .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
              
            source(file.path(dirFlr,'jabba-coerce.R'))
            source(file.path(dirFlr,'jabba-com.R'))
              
            rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],index=index)
                  
            if (!is.null(rtn)) rtn=cbind(ctrl[j,],perfect=TRUE,rtn)
                  
            rtn}
save(r1,file=file.path(dirRes,paste(scenario,"r1.RData",sep="_")))

## Actual & Actual
r2<-foreach(j=seq(dim(ctrl)[1]), 
            .combine=rbind.fill,
            .multicombine=TRUE,
            .export =ls(globalenv()),
            .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
                  
            source(file.path(dirFlr,'jabba-coerce.R'))
            source(file.path(dirFlr,'jabba-com.R'))
              
            rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                         b0=ifelse(is.na(c(index[,1])),1,c(index[,1])),
                         bfinal=c(index[,dim(index)[2]]))
                  
            if (!is.null(rtn)) rtn=cbind(ctrl[j,],initial="Actual",final="Actual",perfect=FALSE,rtn)
                  
            rtn}
save(r2,file=file.path(dirRes,paste(scenario,"r2.RData",sep="_")))

## Heuristic & Actual
r3<-foreach(j=seq(dim(ctrl)[1]), 
            .combine=rbind.fill,
            .multicombine=TRUE,
            .export =ls(globalenv()),
            .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
                  
            source(file.path(dirFlr,'jabba-coerce.R'))
            source(file.path(dirFlr,'jabba-com.R'))
              
            rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                         bfinal=c(index[,dim(index)[2]]))
                  
            if (!is.null(rtn)) rtn=cbind(ctrl[j,],initial="Heuristic",final="Actual",perfect=FALSE,rtn)
                  
            rtn}
save(r3,file=file.path(dirRes,paste(scenario,"r3.RData",sep="_")))

## Actual & Heuristic
r4<-foreach(j=seq(dim(ctrl)[1]), 
            .combine=rbind.fill,
            .multicombine=TRUE,
            .export =ls(globalenv()),
            .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
              
            source(file.path(dirFlr,'jabba-coerce.R'))
            source(file.path(dirFlr,'jabba-com.R'))
              
            rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"],
                         b0=ifelse(is.na(c(index[,1])),1,c(index[,1])))
                  
            if (!is.null(rtn)) rtn=cbind(ctrl[j,],initial="Actual",final="Heuristic",perfect=FALSE,rtn)
                  
            rtn}
save(r4,file=file.path(dirRes,paste(scenario,"r4.RData",sep="_")))

## Heuristic & Heuristic
r5<-foreach(j=seq(dim(ctrl)[1]), 
            .combine=rbind.fill,
            .multicombine=TRUE,
            .export =ls(globalenv()),
            .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
              
            source(file.path(dirFlr,'jabba-coerce.R'))
            source(file.path(dirFlr,'jabba-com.R'))
              
            rtn=jabbaCOM(catch,r=ctrl[j,"r"],bmsyk=ctrl[j,"bmsyk"])
              
            if (!is.null(rtn)) rtn=cbind(ctrl[j,],initial="Heuristic",final="Heuristic",perfect=FALSE,rtn)
              
            rtn}
save(r5,file=file.path(dirRes,paste(scenario,"r5.RData",sep="_")))


##Estimate shape
shape=list(assessment = "shape",
           BmsyK = 0.2, # mean of shape prior
           shape.CV = 0.3, # default is 0.3
           model.type="Pella",
           sigma.proc =TRUE,
           igamma = c(4,0.01), # That is default and fairly robust for COMs (also used in CMSY)
           proc.dev.all=TRUE,
           sigma.est =FALSE,
           fixed.obsE =0.05)

shape<-jabbaCOM(catch,r=ctrl[4,"r"],bmsyk=0.2,sa=shape,
                bfinal=c(index[,dim(index)[2]]),index=index,flag="biodyn")
save(shape,file=file.path(dirRes,paste(scenario,"shape.RData",sep="_")))

if (FALSE){
  
  ##Estimate shape
  shape<-foreach(scenario=scenario, 
                 .combine=rbind.fill,
                 .multicombine=TRUE,
                 .export =ls(globalenv()),
                 .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{
                   
                 source(file.path(dirFlr,'jabba-coerce.R'))
                 source(file.path(dirFlr,'jabba-com.R'))
                   
                 shape=list(assessment = "shape",
                            BmsyK = 0.2, # mean of shape prior
                            shape.CV = 0.3, # default is 0.3
                            model.type="Pella",
                            sigma.proc =TRUE,
                            igamma = c(4,0.01), # That is default and fairly robust for COMs (also used in CMSY)
                            proc.dev.all=TRUE,
                            sigma.est =FALSE,
                            fixed.obsE =0.05)
                   
                 species =subset(ts,assessid==scenario)$species[1]
                 if (species%in%dimnames(pd)[[2]])
                   r=pd["r",species] else
                   r=apply(pd["r"],1,median)
                   
                 r=r/2
                   
                 catch=as.FLQuant(transmute(subset(ts,assessid==scenario),year=year,data=catch))
                 index=as.FLQuant(transmute(subset(ts,assessid==scenario),year=year,data=stock))
                 index=window(index,end=max(dimnames(index[!is.na(index)])$year))
                 catch=window(catch,end=max(dimnames(index)$year))
                 catch[is.na(catch)]=min(catch,na.rm=T)*0.001
                 catch[catch==0]    =min(catch[catch>0],na.rm=T)*0.001
                 catch=catch/mean(catch)*100
                   
                 shape<-jabbaCOM(catch,r=ctrl[4,"r"],bmsyk=0.2,sa=shape,
                             bfinal=c(index[,dim(index)[2]]),index=index,flag="biodyn")
                 save(shape,file=file.path(dirRes,paste(scenario,"shape.RData",sep="_")))
                 data.frame("scenario"=scenario)}
}

#ggplot(r5)+geom_line(aes(year,stock,col=as.character(bmsyk),linetype=as.character(r)))+geom_line(aes(year,data),data=as.data.frame(index))
