##putting together the self-organizing map stuff

galileo2som<- function(affinity, k=10, topo='rectangular', ...){
  if(!('class' %in% .packages(all=TRUE)))
    install.packages('class')
require(class)
##defining the dims
	data<- affinity$data
	divs<- c()
	for(j in 1:k){
		divs[j]<- k%%j}
	dividers<- which(divs==0)
	others<- c()
	for(j in 1:len(dividers)){
		others[j]<- k/dividers[j]}
	diffs<- abs(dividers- others)
	vals<- which.min(diffs)
	x.dim<- dividers[vals]
	y.dim<- others[vals]


	gr<- somgrid(xdim=x.dim, ydim=y.dim, topo=topo)
	proc.som<- batchS(data, gr, c(4,4,2,2,1,1,1,0,0))
	clust<- as.numeric(knn1(proc.som$code, data, 0:(k-1)))

      return(clust)
	}



