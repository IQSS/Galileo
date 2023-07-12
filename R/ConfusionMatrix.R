##this file puts together a set of comparison indices 


confusion_matrix<- function(cluster1, cluster2){
 temp1 <-  rep(0, len(cluster1))
 temp2<- rep(0, len(cluster2))
  uns1 <- unique(cluster1)
  uns2 <- unique(cluster2)
  for(j in 1:len(uns1)){
    temp1[which(cluster1==uns1[j])] <- j
  }
  for(j in 1:len(uns2)){
    temp2[which(cluster2==uns2[j])] <- j}
cluster1 <- temp1
  cluster2 <- temp2
  
  k <- length(unique(cluster1))
  k.prime <- length(unique(cluster2))

  clust1.uns <- unique(cluster1)
  clust2.uns <- unique(cluster2)
  
 
  confusion <- matrix(NA, nrow=k, ncol=k.prime)

  for(j in 1:k){
    for(i in 1:k.prime){
      confusion[j,i] <- len(which(cluster1==clust1.uns[j] & cluster2==clust2.uns[i]))
    }
  }
return(confusion)
}

