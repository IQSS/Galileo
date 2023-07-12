##this function is the harmonic k-means algorithm
##which generalizes the k-means algorithm to allow a more complex error
##function (specifically, all observations contribute to each cluster center
##with the amount of contribution regulated by the distance between the cluster
##center and the observation)

##this also needs the actual computing of a cluster added.  

galileo2harm_kmeans<- function(affinity, k = 10, ...){
   data<- affinity$data
   samps<- sample(1:nrow(data), k)
   for(i in 1:len(samps)){
   centers[i,]<- data[samps[i],] + rgamma(ncol(data), shape=1/100, rate=1/100)
	}
   z<- 0 
   count<- 0
   while(z==0){
	count <- count + 1
	if(count>1){
		centers.old<- centers}
	dist_mat<- matrix(NA, nrow=nrow(data), ncol=k)
	for(j in 1:k){
		for(m in 1:nrow(data)){
			dist_mat[m,j]<- dist(rbind(centers[j,], data[m,]))
			}
		}

	dist_mat2<- dist_mat^(-2)
	dist_mat4<- dist_mat^4
	d2<- apply(dist_mat2, 1, sum)
	d22<- d2^2
	for(j in 1:nrow(centers)){
		temp.a<- 0
		temp.b<- 0
		for(m in 1:nrow(data)){
			a<- (1/dist_mat4[m,j])*(1/d22[m])*data[m,]	
			b<- (1/dist_mat4[m,j])*(1/d22[m])
			temp.a <- temp.a + a
			temp.b<- temp.b + b
			}
		centers[j,]<- temp.a/temp.b
		}
	if(count>1){
		diff<- max(abs(centers- centers.old))
		if(diff<1e-6){
			z<- 1 }
		}
		}

}