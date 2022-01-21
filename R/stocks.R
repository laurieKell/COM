load("/home/laurence-kell/Desktop/papers/COM2/results/com.RData")
load("/home/laurence-kell/Desktop/papers/COM2/data/DBdata.RData")

id=unique(com[,1])
dat=subset(assessment, assessid%in%id)
