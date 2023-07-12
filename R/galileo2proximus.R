##this function implements the proximus clustering method 
##from the cba package.  Proximus basically attempts to approximate the 
##incidence matrix of the term-document matrix with a simpler set of 
##representative vectors--where the number of representative vectors
##determined by the "radius"--tolerated error rate with a cluster center.  
##the greater the radius (the more terms a row and its center can disagree upon)
##the proximus method uses only incidence--not frequency information--to cluster
##the documents


galileo2proximus<- function(affinity, rad = 90, ...){
	if(!('cba' %in% .packages(all=TRUE)))
    install.packages('cba')
    require(cba)
    data<- affinity$data
    data_true<- matrix(NA, nrow=nrow(data), ncol=ncol(data))
    for(i in 1:nrow(data_true)){
	for(j in 1:ncol(data_true)){
		data_true[i,j]<- ifelse(data[i,j]>0, TRUE, FALSE)
		}
	}
    out<- proximus(data_true, max.radius=rad)
    clust<- c(fitted(out)$pl)
    return(clust)
	}
