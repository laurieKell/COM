library(plyr)

if (osVersion=="Ubuntu 20.04.2 LTS"){
  dirMy ="/home/laurence-kell/Desktop/projects/fao/VoI/Report/FAO-VoI/com"
}else{
  dirMy="/rds/general/user/lkell/home/com"}
dirRes=dirMy

load(file.path(dirMy,"ts.RData"))

scenario=unique(ts$assessid)

retro=mdply(data.frame(scenario=scenario), function(scenario){
  rtn=NULL;retro0=NULL;retro1=NULL;retro2=NULL;retro3=NULL;retro4=NULL;retro5=NULL;
  
  if (file.exists(file.path(dirRes,paste(scenario,"retro0.RData",sep="_"))))
    t0<-try(load(file.path(dirRes,paste(scenario,"retro0.RData",sep="_"))))
  if (file.exists(file.path(dirRes,paste(scenario,"retro1.RData",sep="_"))))
    t1<-try(load(file.path(dirRes,paste(scenario,"retro1.RData",sep="_"))))
  if (file.exists(file.path(dirRes,paste(scenario,"retro2.RData",sep="_"))))
    t2<-try(load(file.path(dirRes,paste(scenario,"retro2.RData",sep="_"))))
  if (file.exists(file.path(dirRes,paste(scenario,"retro3.RData",sep="_"))))
    t3<-try(load(file.path(dirRes,paste(scenario,"retro3.RData",sep="_"))))
  if (file.exists(file.path(dirRes,paste(scenario,"retro4.RData",sep="_"))))
    t4<-try(load(file.path(dirRes,paste(scenario,"retro4.RData",sep="_"))))
  if (file.exists(file.path(dirRes,paste(scenario,"retro5.RData",sep="_"))))
    t5<-try(load(file.path(dirRes,paste(scenario,"retro5.RData",sep="_"))))

  if (!is.null(retro0)&!("try-error"%in%is(t0))) rtn=cbind(retro0,               run=0)
  if (!is.null(retro1)&!("try-error"%in%is(t1))) rtn=rbind.fill(rtn,cbind(retro1,run=1))
  if (!is.null(retro2)&!("try-error"%in%is(t2))) rtn=rbind.fill(rtn,cbind(retro2,run=2))
  if (!is.null(retro3)&!("try-error"%in%is(t3))) rtn=rbind.fill(rtn,cbind(retro3,run=3))
  if (!is.null(retro4)&!("try-error"%in%is(t4))) rtn=rbind.fill(rtn,cbind(retro4,run=4))
  if (!is.null(retro5)&!("try-error"%in%is(t5))) rtn=rbind.fill(rtn,cbind(retro5,run=5))
  
  if (!is.null(rtn)) print(as.character(scenario)) else
                     print(paste(as.character(scenario),"*"))
    
  rtn})
save(retro,file=file.path(dirMy,"retro.RData"))

if (FALSE){
  load("/home/laurence-kell/Desktop/papers/COM/results/retro.RData")
  dat=merge(transform(subset(retro,tail==year),y=stock)[    ,c("scenario", "bmsyk","r","perfect","year","run","y")],
            transform(subset(retro,tail==max(year)),x=stock)[,c("scenario", "bmsyk","r","perfect","year","run","x")])
  
ret=ddply(dat,.(scenario,bmsyk,r,perfect,run),with, mean((y-x)/x))


ggplot(transform(subset(ret,run%in%c(1,2,4)),run=factor(run,labels=c("With Index","COM with Depletion Known","COM with Heuristic"))))+
  geom_histogram(aes(V1))+facet_grid(run~.)+theme_bw()+
  xlab("Mohn's rho")+ylab("")

}