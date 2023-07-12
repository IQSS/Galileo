##this function is for k-means clustering


galileo2kmeans<- function(affinity, k=10, metric='euclidean',...){
  require(amap)
  x <- affinity$data
  centers<- k
  method <- metric
  out <- Kmeans(x, centers, method=metric,...)$cluster
  return(out)
	} 
	
