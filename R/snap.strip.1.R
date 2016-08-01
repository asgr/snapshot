snap.strip.1=function(snap,type=6){
allN=which(snap$head$Nall>0)
if(all(type %in% allN)==FALSE){stop(cat('Some particle types are missing in snapshot data! Only types',allN,'exist.\n'))}
Ntypes=length(snap$head$Nall)
ranges=cbind(rep(0,Ntypes),rep(0,Ntypes))
current=1
for(i in allN){
	ranges[i,1]=current
	current=current+snap$head$Nall[i]
	ranges[i,2]=current
	current=current+1
}

temp=list()
for(i in type){
	temp=c(temp,list(snap$part[snap$part[,'ID'] %in% ranges[i,1]:ranges[i,2],]))
}
names(temp)=paste('T',type,sep='')
return=temp
}