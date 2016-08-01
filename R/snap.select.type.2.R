snap.select.type.2 <-
function(what){
ndim = 0
type = FALSE 
if(what == "HEAD"){ 
	ndim = 0
	type = FALSE 
}
if(what == "POS " || what == "VEL " || "BFLD" == what){ 
	ndim = 3
        type = numeric()
}
if(what == 'MASS' || what == 'RHO ' || what == 'HSML' || what == 'U   '){ 
	ndim = 1
	type = numeric()
}
if(what == 'ID  '){ 
	ndim = 1
	type = integer()
}
return(list(ndim=ndim,type=type))
}