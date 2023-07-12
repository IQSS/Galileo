##this is the file for large k-means clustering

galileo2lkmeans<- function(affinity, k=10,...){
  if(!('cluster' %in% .packages(all=TRUE)))
    install.packages('cluster')
  require(cluster)

  x <- affinity$data
  k <-k 
  out <- clara(x, k, ...)$clustering
  return(out)
	}

  

	
