snap.clean.fields.2 <-
function(file,gas){
if(missing(gas)) gas=0

head=snap.read.2(file,'HEAD',debug=1)
p=snap.read.2(file,'POS ',gas=gas)
v=snap.read.2(file,'VEL ',gas=gas)
id=snap.read.2(file,'ID  ',gas=gas)
m=snap.read.2(file,'MASS',gas=gas)
if (gas != 0) {
u=snap.read.2(file,'U   ',gas=gas)
r=snap.read.2(file,'RHO ',gas=gas)
h=snap.read.2(file,'HSML',gas=gas)
head$Npart[2:6] = 0
head$Nall[2:6] = 0
}

lfile=paste0(file,'_clean')

snap.write.head.2(lfile,head)
snap.write.block.2(lfile,'POS ',p)
snap.write.block.2(lfile,'VEL ',v)
snap.write.block.2(lfile,'ID  ',id)
snap.write.block.2(lfile,'MASS',m)
if (gas != 0) {
snap.write.block.2(lfile,'U   ',u)
snap.write.block.2(lfile,'RHO ',r)
snap.write.block.2(lfile,'HSML',h)
}

}