snap.read.2 <-
function(file,what,ndim,type,debug,gas){
if(missing(what)) what="HEAD"
if(missing(debug)) debug=0
if(missing(ndim) && missing(type)){
  tmp=snap.select.type.2(what)
  ndim=tmp$ndim
  type=tmp$type
}else{
if(missing(ndim)) ndim=1
if(missing(type)) type=numeric()
}#from here on, there is always a type and ndim
#if(missing(which)) which=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
if(missing(gas)) gas=0
if(gas > 0) cat("Reading with GAS = ",gas,"\n")

data = file(file,'rb')
#first LABEL block
skip=readBin(data,'integer',n=1)
label=readChar(data,4,useBytes=TRUE)
block=readBin(data,'integer',n=1)
skip=readBin(data,'integer',n=1)
cat("Reading LABEL= ", label, " of ",block,'\n')
#first header block
skip=readBin(data,'integer',n=1)
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
skip=readBin(data,'integer',n=1)
if((block - skip - 8) != 0) {
  close(data)
  stop("Something wrong!")
}

skip=readBin(data,integer(),n=1)
while(length(skip)>0){
	label=readChar(data,4,useBytes=TRUE)
	block=readBin(data,integer(),n=1)
	skip=readBin(data,integer(),n=1)

	if(debug > 0)
	cat("Reading LABEL= ", label, " of ",block,'\n')
	skip=readBin(data,integer(),n=1)
	if(label == what){
		blo=readBin(data,type,n=skip/4,size=4)
	}else{
		seek(data,origin='current',where=block-8)
	}
	skip=readBin(data,integer(),n=1) #this ends the block
	if((block - skip - 8) != 0) print("Something wrong!")
	skip=readBin(data,integer(),n=1) #starts new block

}
close(data)

if(ndim == 3 ){
	extract=((1:sum(Npart))*3)-2
	blo=data.frame(	x=blo[extract],y=blo[extract+1],z=blo[extract+2])
}
if(gas > 0 && 'HEAD' != what){
	if(ndim == 3 ){
	   blo=data.frame(x=blo$x[1:Npart[1]], y=blo$y[1:Npart[1]], z=blo$z[1:Npart[1]] )
	} else {
	   blo=blo[1:Npart[gas]]
	}
}

if('HEAD' == what) 
return(list(Npart = Npart, Massarr= Massarr, Time= Time, z= z, FlagSfr= FlagSfr, FlagFeedback= FlagFeedback, 
       		 Nall= Nall, FlagCooling= FlagCooling, NumFiles= NumFiles, BoxSize= BoxSize, OmegaM= OmegaM, 
		 OmegaL= OmegaL, h=h , FlagAge= FlagAge, FlagMetals= FlagMetals, NallHW= NallHW,
		 flag_entr_ics=flag_entr_ics))
else
return(blo)
}