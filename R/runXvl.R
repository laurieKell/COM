library(FLCore)
library(FLasher)
library(FLBRP)
library(ggplotFL)

library(plyr)
library(dplyr)
library(reshape)

library(mpb)
library(JABBA)

library(foreach)
library(doParallel)

dirRes="/home/laurence-kell/Desktop/projects/fao/VoI/results"
dirRs2="/home/laurence-kell/Desktop/projects/fao/VoI/Report/FAO-VoI/results"

load(file.path(dirRes,"ht.RData"))
load(file.path(dirRes,"lh.RData"))
load(file.path(dirRes,"pd.RData"))

source('~/Desktop/flr/xvl/R/jabba-com.R')

t3=subset(t3,!(is.na(catch)|is.na(stock)))
t3[is.na(t3$ssb),"ssb"]=t3[is.na(t3$ssb),"catch"]/t3[is.na(t3$ssb),"harvest"]
t3=subset(t3,catch>0)
t3=ddply(t3,.(assessid), with, {
  dat=merge(data.frame(year=min(year,na.rm=T):max(year,na.rm=T)),
            data.frame("assessid"=assessid,"stockid"=stockid,"year"=year,"ssb"=ssb,"stock"=stock,"catch"=catch,"harvest"=harvest),by="year",all.x=TRUE)
  if (any(is.na(dat$catch))) return(NULL) else dat})
t3=subset(t3,!(assessid=="NIWA-GSTRGZRSTA7-1964-2007-CORDUE"&year<1966))

run<-function(i,t3,pd,pd.low,control,saFlag=FALSE,min=NULL,n=20){
        print(i)
        
        sa=try(with(control[i,],setControl(assessid,species,model,r,dInitial,dFinal,t3,pd,pd.low,min=min,n=n)))
        if (saFlag) return(sa)
        
        if ("try-error"%in%is(sa)) {warning("sa fail"); return(NULL)}
        
        jb=do.call("build_jabba",sa[-seq(length(names(sa)))[names(sa)=="ts"]])
        jb=try(fit_jabba(jb,
                         #init.values=TRUE,
                         #init.r=sa$r.prior[1]*2,
                         #init.q=2,
                         ni =5500,
                         nt =1,
                         nb =500,
                         nc =2))
        
        if ("try-error"%in%is(jb)) return(NULL)
        
        bd=try(jabba2biodyn(jb)) 
        
        if ("try-error"%in%is(jb)) return(NULL)
        
        model.frame(mcf(FLQuants(bd,stock  =function(x) stock(  x)/refpts(x)["bmsy"],
                                    harvest=function(x) harvest(x)/refpts(x)["fmsy"],
                                    catch  =function(x) catch(  x)/refpts(x)["msy"],
                                    om     =function(x) as.FLQuant(data.frame(year=sa$ts$year,data=sa$ts$stock)))),drop=TRUE)}

cl=makeCluster(6)
registerDoParallel(cl)

  control=mdply(data.frame(assessid=unique(t3$assessid)), function(assessid,model)
    cbind(key=1:16,
          expand.grid(model   =c("Fox",       "Schaeffer"),
                      r       =c("Default",   "Low"),
                      dInitial=c("Heuristic", "Actual"),
                      dFinal  =c("Heuristic", "Actual"))))
  control$assessid=as.character(control$assessid)
  control=merge(control,transmute(assessment,assessid=assessid,species=species))
  
com1<-foreach(i=seq(dim(control)[1])[1:120], 
             .combine=rbind,
             .multicombine=TRUE,
             .export =c("t3","control","pd","pd.low"),#ls(globalenv()),
             .packages=c("FLCore","FLasher","FLBRP","ggplotFL","plyr","dplyr","reshape","mpb","JABBA")) %dopar%{cbind(i=i,run(i,t3,pd,pd.low,control))}
save(com1,file=file.path(dirRs2,"com1.RData"))
