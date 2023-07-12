##using the k-medoids from the cluster package


galileo2kmedoids<- function(affinity, k=10,metric='stand.euc',...){

 if(!('cluster' %in% .packages(all=TRUE)))
    install.packages('cluster')
  require(cluster)
 if(metric=='stand.euc'){
  x <- 1-affinity$cosine
  x <- as.dist(x)
  k <-k 
  out <- pam(x, k, ...)$clustering
  return(out)
}
 if(metric=='euclidean'){
   x <- affinity$data
   k <- k
   out <- pam(x, k=k, metric='euclidean')$clustering
   return(out)}
 if(metric=='manhattan'){
   x <- affinity$data
   k <- k
   out <- pam(x, k=k, metric='manhattan')$clustering
   return(out)
 }
}



