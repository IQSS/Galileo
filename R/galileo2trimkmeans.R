##this is the wrapper for trimmedkmeans, which is a robust version of 
##k-means (similar to k-medoids and affinity propagation

galileo2trimkmeans<- function(affinity, k=10, trim=0.1,...){
  if(!('trimcluster' %in% .packages(all=TRUE)))
    install.packages('trimcluster')
  require(trimcluster)

  x <- affinity$data
  k <- k
  trim <- trim
  out <- trimkmeans(x, k, trim, ...)
  out <- out$classification
  return(out)
}
  
