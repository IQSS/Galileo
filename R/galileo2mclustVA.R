##this file uses mixture of normals computed using a variational
##approximation to the true posterior distribution


galileo2mclustVA <- function(affinity,k=10,nruns=1,prior=NA,...){

  S <- affinity$data
  ncat <- k
  nruns <- nruns
  prior <- prior
  require(vabayelMix)
  out <- vabayelMix(S, Ncat=ncat, prior=prior,nruns=nruns, npick=1)$probs[[1]]$wcl
  uns <- unique(out)
  temp <- rep(0, length(out))
  for(k in 1:length(uns)){
    ab <- which(out==uns[k])
    temp[ab] <- k}
  out <- temp
  return(out)
}
