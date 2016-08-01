snap.write.head.2 <-
function(head,file){
cat("Writting Header in ",file,'\n')
data = file(file,'wb')
#first label block
writeBin(as.integer(8),data,size=4)
writeChar(as.character('HEAD'),data,eos=NULL)
writeBin(as.integer(264),data,size=4)
writeBin(as.integer(8),data,size=4)
#first header block
writeBin(as.integer(256),data,size=4)
writeBin(as.integer(head$Npart),data)
writeBin(as.numeric(head$Massarr),data,size=8)
writeBin(as.numeric(head$Time),data,size=8)
writeBin(as.numeric(head$z),data,size=8)
writeBin(as.integer(head$FlagSfr),data)
writeBin(as.integer(head$FlagFeedback),data)
writeBin(as.integer(head$Nall),data)
writeBin(as.integer(head$FlagCooling),data)
writeBin(as.integer(head$NumFiles),data)
writeBin(as.numeric(head$BoxSize),data,size=8)
writeBin(as.numeric(head$OmegaM),data,size=8)
writeBin(as.numeric(head$OmegaL),data,size=8)
writeBin(as.numeric(head$h),data,size=8)
writeBin(as.integer(head$FlagAge),data)
writeBin(as.integer(head$FlagMetals),data)
writeBin(as.integer(head$NallHW),data)
writeBin(as.integer(head$flag_entr_ics),data)
writeBin(as.integer(rep(0,length=as.integer(256-241))),data)
#last head block
writeBin(as.integer(256),data,size=4,useBytes=TRUE)

close(data)
}