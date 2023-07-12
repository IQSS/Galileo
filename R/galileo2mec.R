##this implements the maximum entropy clustering
##details are found on pg 175 of Gan, Ma, Wu



galileo2mec<- function(affinity, k=10, ...){

data<- affinity$data

samps<- sample(1:nrow(data), k)

centers<- data[samps,]

for(m in 1:nrow(centers)){
		centers[m,]<- centers[m,] + rgamma(ncol(centers), shape=1/10, rate=1/10)
	}

dist_mat<- matrix(NA, nrow=nrow(data), ncol=k)

	for(i in 1:nrow(data)){
	  for(j in 1:k){
		dist_mat[i,j]<- dist(rbind(centers[j,], data[i,]))^2
	     }
	}
	
	beta<- -.01
	temps<- exp(beta*dist_mat)
	temps<- apply(temps, 1, sum)
	nums<- exp(beta*dist_mat)
	probs<- nums
	for(j in 1:nrow(probs)){
		probs[j,]<- probs[j,]/temps[j]}
	a<- 0
	count<- 0
	while(a==0){
		count<- count + 1
		if(count>1){
			probs.old<- probs}
		##this is the annealing portion of the algorithm
		##as this gets more negative, we will get sharper 
		##distributions around the center.  
		beta<- -.001*count
		prop.cat<- apply(probs, 2, sum)
		for(j in 1:k){
			test<- 0
			for(m in 1:nrow(data)){
				test<- probs[m,j]*data[m,] + test}
			centers[j,]<- test/prop.cat[j]
			}
		dist_mat<- matrix(NA, nrow=nrow(data), ncol=k)

		for(i in 1:nrow(data)){
	 	 for(j in 1:k){
		   dist_mat[i,j]<- dist(rbind(centers[j,], data[i,]))^2
	      }
	   }
	  temps<- exp(beta*dist_mat)
	  temps<- apply(temps, 1, sum)
	  nums<- exp(beta*dist_mat)
	  probs<- nums
	  for(j in 1:nrow(probs)){
		probs[j,]<- probs[j,]/temps[j]}
	  if(count>1){
		diff<- abs(probs.old - probs)
		diff<- max(diff)
		if(diff<1e-4){
			a<- 1}
		}
		}
	clusters<- apply(probs, 1, which.max)
	return(clusters)
	}


