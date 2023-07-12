##extracting the important words for each doc
##and then extracting the important words for each cluster

##for the documents

func.labs<- function(data){

sums<- matrix(NA, nrow = nrow(data), ncol=ncol(data))
for(j in 1:nrow(data)){
	sums[j,]<- apply(data[-j,], 2, mean)
		}
	temps<- data - sums
	output<- apply(temps, 1, which.max)
	output<- colnames(data)[output]
	return(output)
	}

func.clusters<- function(data, cluster){
	uns<- unique(cluster)
	mean.in<- mean.out<- matrix(NA, nrow=len(uns), ncol=ncol(data))
	for(j in 1:len(uns)){
		abc<- which(cluster==uns[j])
		temps<- data[abc,]
		temps2<- data[-abc,]
		if(is.null(nrow(temps))){
			temps<- t(temps)}
		if(is.null(nrow(temps2))){
			temps2<- t(temps2)}
		mean.in[j,]<- apply(temps, 2, mean)
		mean.out[j,]<- apply(temps2, 2, mean)
		}
	temps<- mean.in - mean.out
	labs<- matrix(NA, nrow=len(uns), ncol=10)
	for(j in 1:len(uns)){
		labs[j,]<- colnames(data)[order(temps[j,], decreasing=T)[1:10]]
		}
	return(labs)
	}


list.create<- function(clust.list){
	cluster.store<- list()
	for(j in 1:len(clust.list)){
		uns<- unique(clust.list[[j]])
		mats<- matrix(0, nrow=len(clust.list[[1]]), ncol=len(uns))
		for(m in 1:len(uns)){
			temp<- which(clust.list[[j]]==uns[m])
			mats[temp,m]<- 1}
			mats<- mats%*%t(mats)
			cluster.store[[j]]<- mats
		}
	return(cluster.store)
	}

start.point<- function(point=c(0,0)){
	dists<- c()
	scales<- cmdscale(dist.mat)
	for(j in 1:nrow(scales)){
		dists[j]<- dist(rbind(point, scales[j,]))
		}
	weights<- dnorm(dists, sd=0.5)
	weights<- weights/sum(weights)
	#stumps<- weights/max(weights)
	#stumps<- 1- stumps
	
	ens<- list.mats[[1]]*weights[1]

	for(k in 1:nrow(scales)){
		ens<- ens + list.mats[[k]]*weights[k]
		}
	sims<- ens
	for(j in 1:nrow(sims)){
	sims[j,]<- sims[j,]/max(sims[j,])}

	sims<- 1 - sims
	tents<- kmeans(sims, center=11)	
	clusts<- tents$cluster
	#clusts<- hclust(as.dist(sims), method='complete')
	#clusts<- cutree(clusts, k=11)
	#dists.2<- c()
	#for(m in 1:93){
	#dists.2[m]<- clustcomp(clusts, clust.list[[m]])$VI
	#}
	#dists.2<- c(dists.2, 0)
	#dist.renew<- matrix(NA, nrow=94, ncol=94)
	#dist.renew[1:93,1:93]<- dist.mat
	#dist.renew[94,]<- dists.2; dist.renew[,94]<- dists.2
	#scales2<- cmdscale(dist.renew)
	#rownames(scales2)<- c(rownames(dist.mat), 'ensemble_test')
	#par(mfrow=c(2,1))
	#plot(scales, pch='')
	#text(scales, rownames(scales))
	#points(point[1], point[2], col='red', pch=20, cex=2)
	#plot(scales2, pch='')
	#text(scales2, rownames(scales2))
	#text(scales2[94,1], scales2[94,2], rownames(scales2)[94], col='red')
	scales3<- cmdscale(sims)
	return(scales3)
	}




	



