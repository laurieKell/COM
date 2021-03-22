library(plyr)

if (osVersion=="Ubuntu 20.04.2 LTS"){
  dirMy ="/home/laurence-kell/Desktop/projects/fao/VoI/Report/FAO-VoI/com"
}else{
  dirMy="/rds/general/user/lkell/home/com"}
dirRes=dirMy

load(file.path(dirMy,"ts.RData"))

scenario=unique(ts$assessid)

com=mdply(data.frame(scenario=scenario), function(scenario){
  idx=subset(ts,assessid==scenario)[,c("year","stock")]
  names(idx)[2]="om"
  rtn=NULL;r0=NULL;r1=NULL;r2=NULL;r3=NULL;r4=NULL;r5=NULL;
  
  if (file.exists(file.path(dirRes,paste(scenario,"r0.RData",sep="_"))))
    t0<-try(load(file.path(dirRes,paste(scenario,"r0.RData",sep="_"))))
  if (file.exists(file.path(dirRes,paste(scenario,"r1.RData",sep="_"))))
    t1<-try(load(file.path(dirRes,paste(scenario,"r1.RData",sep="_"))))
  if (file.exists(file.path(dirRes,paste(scenario,"r2.RData",sep="_"))))
    t2<-try(load(file.path(dirRes,paste(scenario,"r2.RData",sep="_"))))
  if (file.exists(file.path(dirRes,paste(scenario,"r3.RData",sep="_"))))
    t3<-try(load(file.path(dirRes,paste(scenario,"r3.RData",sep="_"))))
  if (file.exists(file.path(dirRes,paste(scenario,"r4.RData",sep="_"))))
    t4<-try(load(file.path(dirRes,paste(scenario,"r4.RData",sep="_"))))
  if (file.exists(file.path(dirRes,paste(scenario,"r5.RData",sep="_"))))
    t5<-try(load(file.path(dirRes,paste(scenario,"r5.RData",sep="_"))))

  if (!is.null(r0)&!("try-error"%in%is(t0))) rtn=cbind(r0,depletion=TRUE)
  if (!is.null(r1)&!("try-error"%in%is(t1))) rtn=rbind.fill(rtn,cbind(r1,depletion=FALSE))
  if (!is.null(r2)&!("try-error"%in%is(t2))) rtn=rbind.fill(rtn,cbind(r2,depletion=FALSE))
  if (!is.null(r3)&!("try-error"%in%is(t3))) rtn=rbind.fill(rtn,cbind(r3,depletion=FALSE))
  if (!is.null(r4)&!("try-error"%in%is(t4))) rtn=rbind.fill(rtn,cbind(r4,depletion=FALSE))
  if (!is.null(r5)&!("try-error"%in%is(t5))) rtn=rbind.fill(rtn,cbind(r5))
  
  if (!is.null(rtn)) {
    rtn=merge(rtn,idx,by="year")
    print(as.character(scenario))
  }
  else {
    rtn=idx
    print(paste(as.character(scenario),"*"))}
    
  rtn})
save(com,file=file.path(dirMy,"com.RData"))
