clustcomp<- function(cluster1, cluster2){
  len <- length
 temp1 <- rep(0, len(cluster1))
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

  
  p.k <- apply(confusion, 1, sum)/len(cluster1)
  p.k.prime <- apply(confusion, 2, sum)/len(cluster2)

  
  confusion2 <- confusion/sum(confusion)

 ##computing this based upon the definition of mutual information

 # cond.entropy <- 0
 # for(j in 1:len(p.k)){
 #   for(i in 1:len(p.k.prime)){
 #     if(confusion2[j,i]!=0 & p.k[j]!=0 & p.k.prime[i]!=0){
 #       cond.entropy <- cond.entropy +
 #          confusion2[j,i]*log((p.k.prime[i])/confusion2[j,i], base=2)
 #     }
 #   }
 # }
  

cond.entropy<- 0

for(j in 1:len(p.k)){
	for(i in 1:len(p.k.prime)){
		      if(confusion2[j,i]!=0 & p.k[j]!=0 & p.k.prime[i]!=0){
				cond.entropy<- cond.entropy - 
					confusion2[j,i]*log((confusion2[j,i])/p.k.prime[i])
				}
			}
		}


  entropy.1 <- entropy.2 <- 0
  for(k in 1:len(p.k)){
    if(p.k[k]!=0){
  entropy.1 <- -sum(p.k[k]*log(p.k[k])) + entropy.1
      }
  }

  for(k in 1:len(p.k.prime)){
     if(p.k.prime[k]!=0){
       entropy.2 <- -sum(p.k.prime[k]*log(p.k.prime[k])) + entropy.2
     }
   }

 clust.mutinf <- entropy.1 - cond.entropy
  
  VI <- entropy.1 + entropy.2 - 2 *clust.mutinf
  values <- list(VI, entropy.1, entropy.2, confusion)
  names(values) <- c('VI', 'entropy.1', 'entropy.2', 'confusion')
  return(values)
}
