galileo2spectralmelia<- function(affinity, metric='cosine', k=10, ...){
 if(metric=='cosine'){
  S <- affinity$cosine
  aff.max<- max(S)
  distance<- 1 - S/aff.max
  }
if(metric=='euclidean' | metric=='manhattan' | metric=='minkowski' | metric=='maximum' | metric=='canberra'){
	data<- affinity$data
	if(metric=='minkowski'){
      distance<- dist(data, metric, p=4)}
	if(metric!='minkowski'){
	distance<- dist(data, metric)}
	distance<- as.matrix(distance)
	diag(distance)<- 0
	}
  
    S<- max(distance) - distance
 
	
	degree<- apply(S, 1, sum)
	D<- matrix(0, nrow=nrow(S), ncol=ncol(S))
	diag(D)<- 1/degree
	P<- D%*%S
	vecs<- eigen(P)$vector[,1:k]
	out<- kmeans(vecs, center=k)$cluster
	return(out)}