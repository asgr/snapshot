.readBinThin=function(con, what, n=1L, size= NA_integer_, signed = TRUE, endian = .Platform$endian, thin=1, ndim=1){
    if(thin==1){
    out=readBin(con=con,what,n=n,size=size,signed=signed,endian=endian)
}else{
    count=1
    limit=floor(n/(ndim*thin))*ndim
    out=rep(NA,limit)
    while(count<=limit){
        out[count:(count+ndim-1)]=readBin(con,what,n=ndim,size=size,signed=signed,endian=endian)
        count=count+ndim
        if(count<=limit){
            seek(con, where=(thin-1)*size*ndim, origin='current')
        }
    }
    seek(con, where=(n-((limit-ndim)*thin)-ndim)*size, origin='current')
}
    return=out
}

snap.read.1=function(file, thin=1){
data = file(file,'rb')
#first header block
block=readBin(data,'integer',n=1)
Npart=readBin(data,'integer',n=6)
Massarr=readBin(data,'numeric',n=6,size=8)
Time=readBin(data,'numeric',n=1,size=8)
z=readBin(data,'numeric',n=1,size=8)
FlagSfr=readBin(data,'integer',n=1)
FlagFeedback=readBin(data,'integer',n=1)
Nall=readBin(data,'integer',n=6)
FlagCooling=readBin(data,'integer',n=1)
NumFiles=readBin(data,'integer',n=1)
BoxSize=readBin(data,'numeric',n=1,size=8)
OmegaM=readBin(data,'numeric',n=1,size=8)
OmegaL=readBin(data,'numeric',n=1,size=8)
h=readBin(data,'numeric',n=1,size=8)
FlagAge=readBin(data,'integer',n=1)
FlagMetals=readBin(data,'integer',n=1)
NallHW=readBin(data,'integer',n=6)
flag_entr_ics=readBin(data,'integer',n=1)
readBin(data,'integer',n=256-241)
#last head block
block=readBin(data,'integer',n=1)

#1 data block = Positions
block=readBin(data,'integer',n=1)
posall=.readBinThin(data,'numeric',n=block/4,size=4,thin=thin,ndim=3)
block=readBin(data,'integer',n=1)
#2 data block = Velocities
block=readBin(data,'integer',n=1)
print(block/4)
velall=.readBinThin(data,'numeric',n=block/4,size=4,thin=thin,ndim=3)
block=readBin(data,'integer',n=1)
#3 data block = IDs
block=readBin(data,'integer',n=1)
ID=.readBinThin(data,'integer',n=block/4,size=4,thin=thin,ndim=1)
block=readBin(data,'integer',n=1)
#4 data block = Masses
block=readBin(data,'integer',n=1)
if(length(block)>0){
    Mass=.readBinThin(data,'numeric',n=block/4,size=4,thin=thin,ndim=1)
}else{
    counter=1
    Mass=rep(NA,sum(Npart))
    whichmass=which(Npart>0)
        for(i in 1:length(whichmass)){
            N=Npart[whichmass[i]]
            Mass[ID>=counter & ID<=counter+N]=Massarr[whichmass[i]]
            counter=counter+N
        }
}
block=readBin(data,'integer',n=1)
#Extra blocks
extra=0
extramat={}
while(length(block)>0){
block=readBin(data,'integer',n=1)
	if(length(block)>0){
		extramat=cbind(extramat,.readBinThin(data,'numeric',n=block/4,size=4,thin=thin,ndim=1))
		block=readBin(data,'integer',n=1)
		extra=extra+1
	}
}

close(data)

extract=((1:floor(sum(Npart)/thin))*3)-2
part=data.frame(ID=ID,x=posall[extract],y=posall[extract+1],z=posall[extract+2],vx=velall[extract],vy=velall[extract+1],vz=velall[extract+2],Mass=Mass)

return(list(part=part,head=list(Npart = Npart, Massarr= Massarr, Time= Time, z= z, FlagSfr= FlagSfr, FlagFeedback= FlagFeedback, Nall= Nall, FlagCooling= FlagCooling, NumFiles= NumFiles, BoxSize= BoxSize, OmegaM= OmegaM, OmegaL= OmegaL,h=h, FlagAge= FlagAge, FlagMetals= FlagMetals, NallHW= NallHW,flag_entr_ics=flag_entr_ics),extra=extra,extramat=extramat))}
