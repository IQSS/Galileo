##this script allows analysis using Blei, Ng, and Jordan's (2003) spectral
##clustering algorithm

galileo2spectral<- function(affinity, k=10, metric='cosine',sigma=1,...){
 if(metric=='cosine'){
	  S <- affinity$cosine
	  aff.max<- max(S)
	  distance<- 1 - S/aff.max
	  }
if(metric=='euclidean' | metric=='binary'|metric=='manhattan' | metric=='minkowski'  | metric=='canberra'){
		data<- affinity$data
		if(metric=='minkowski'){
			p<- 4
			distance<- dist(data, metric, p=p)
			distance<- as.matrix(distance)
			distance<- distance/max(distance)
			diag(distance)<- 0
		}
	if(metric!='minkowski'){
		distance<- dist(data, metric)
		distance<- as.matrix(distance)
		distance<- distance/max(distance)
		diag(distance)<- 0
}
		
	}
  
	
  k <- k
  
  spectral<- function(distance, k){
	distance<- exp(-(distance^2)/sigma)
	diag(distance)<- 0
	D.mat<- matrix(0, nrow=nrow(distance), ncol=ncol(distance))
	sums<- apply(distance, 1, sum)
	diag(D.mat)<- 1/(sqrt(sums))
	L<- D.mat%*%distance%*%D.mat
	Y<- eigen(L)$vectors[,1:k]
	out<- apply(Y*Y, 1, sum)
	Y<- Y/sqrt(out)
	sol<- kmeans(Y, k)
	return(sol$cluster)
      }
  out <- spectral(distance, k)
  return(out)
	}




