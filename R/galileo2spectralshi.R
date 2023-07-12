##putting together the Shi Milk algorithm

##for text clustering

galileo2spectralshi<- function(affinity, k=10, metric ='cosine',...){
 if(metric=='cosine'){
      S <- affinity$cosine
     aff.max<- max(S)
     distance<- 1 - S/aff.max
     }
  if(metric=='euclidean' | metric=='manhattan' | metric=='minkowski' | metric=='maximum' | metric=='canberra'){
	data<- affinity$data
        distance<- matrix(0, nrow=nrow(affinity$cosine), ncol=ncol(affinity$cosine))
		for(m in 1:nrow(distance)){
			for(j in 1:m){
				distance[j,m]<- distance[m,j]<- dist(rbind(data[j,], data[m,]),metric)
         				}
		}
		}

  S<- max(distance) - distance

ncut<- function(set.A, set.B, degree){
	temp<- S[set.A, set.B]
	cut<- sum(temp)
	vol.A<- sum(degree[set.A])
	vol.B<- sum(degree[set.B])
	ncut<- cut*(1/vol.A + 1/vol.B)
	return(ncut)
		}

degree<- apply(S, 1, sum)
D<- matrix(0, nrow=nrow(S), ncol=ncol(S))


diag(D)<- 1/sqrt(degree)

P<- D%*%S





clust<- rep(0, nrow(P))
a<- 0
actual<- 1:nrow(S)
while(a<(k-1)){

a<- a + 1
##computing the second eigenvector
eigs<- eigen(P)$vector[,2]

##now ordering
ordered<- order(eigs)

##going through and computing the ncuts
nums<- nrow(P)-1

n.store<- c()
for(j in 1:nums){
	n.store[j]<- ncut(ordered[1:j], ordered[(j+1): len(eigs)],
			 degree)
	}

##finding the minimal ncut
clust1<- which.min(n.store)

##determining which segment has the lowest
##second eigenvector
temp1<- ordered[1:clust1]; temp2<- ordered[(clust1 + 1): len(eigs)]


p1<- P[temp1, temp1]; p2<- P[temp2, temp2]
val1<- eigen(p1)$val[2]; val2<- eigen(p2)$val[2]
if(is.na(val1)==T){
	clust[actual[temp1]]<-a
	actual<- actual[-temp1]
	P<- p2 }
if(is.na(val2)==T){
	clust[actual[temp2]]<- a
	actual<- actual[-temp2]
	P<- p1}
if(is.na(val1)==F & is.na(val2)==F){
if(val1>val2 ){
	clust[actual[temp2]]<- a
	actual<- actual[-temp2]
	P<- p1}
if(val2>val1){
	clust[actual[temp1]]<- a
	actual<- actual[-temp1]
	P<- p2}
}
}

clust[actual]<- k


return(clust)
}















