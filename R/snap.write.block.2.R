snap.write.block.2 <- function(file,label,inp,ndim,type){
if(missing(ndim) && missing(type)){
  tmp=snap.select.type.2(label)
  ndim=tmp$ndim
  type=tmp$type
}
if(missing(ndim)) ndim=1
if(missing(type)) type=numeric()

cat("Adding ",label," in ",file,'\n')
data = file(file,'ab')

if(ndim == 1) np=length(inp)*4
if(ndim == 3) {
   np=length(inp$x)*3*4
   inp=c(rbind(inp$x,inp$y,inp$z))
}

#first LABEL block
writeBin(as.integer(8),data,size=4)
writeChar(label,data,4,eos=NULL)
writeBin(as.integer(np+8),data,size=4)
writeBin(as.integer(8),data,size=4)
#first  block
writeBin(as.integer(np),data,size=4)
writeBin(inp,data,size=4)
writeBin(as.integer(np),data,size=4)

close(data)
}