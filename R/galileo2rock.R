##this function implements the ROCK clustering algorithm, from the CBA package

galileo2rock<- function(affinity, k=10, ...){
    require(cba)
   data<- affinity$data
   true_mat<- matrix(NA, nrow=nrow(data), ncol=ncol(data))
    for(i in 1:nrow(true_mat)){
	for(j in 1:ncol(true_mat)){
		if(data[i,j]>0){
             true_mat[i,j]<- T}
		if(data[i,j]==0){
		 true_mat[i,j]<- F}
		}
	 }
    out<- rockCluster(true_mat, n=k, beta=k)
    clusts<- c(out$cl)
    return(clusts)
	}
