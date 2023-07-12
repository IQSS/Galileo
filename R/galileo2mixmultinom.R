##this file uses mixtools to generate a mixture of multinomials
##although, it tends to be subject to underflows.

galileo2mixmult <- function(affinity, k=10, ...){
  data <- as.matrix(affinity$data)
  k = k
  require(mixtools)
  out <- multmixEM(data, k=k, ...)
  clusts <- apply(out$posterior, 1, which.max)
  return(clusts)
}
