##this implements the clustering with convex conjugate functions 
##from the cba package.  

galileo2ClustConvFunc<- function(affinity, k = 10 , ...){
	data<- affinity$data
	require(cba)
	out<- ccfkms(data, n=k,...)
	clusts<- as.vector(out$cl)
	##detach('package:proxy')
	return(clusts)
	}
