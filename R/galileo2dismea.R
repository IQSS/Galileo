##this the DISMEA algorithm, which 
##adds another divisive algorithm to the package
##this function goes through and successively splits the clusters
##that have the largest sum or squares (equivalently, the set of
##clusters that are the worst along each stage.)  
##A full description of this function can be found on pg 147 of Gan,
##Ma, Wu

galileo2dismea<- function(affinity, k=10, ...){
	data<- affinity$data
	clust_sol<- c()
	step_1<- kmeans(data, centers=2)
	clust_sol<- step_1$cluster
	a<- 1
	centers<- step_1$center
	while(a<k-1){
		a<- a + 1
		ssr<- c()
		for(j in 1:a){
			abc<- which(clust_sol==j)
			if(len(abc)>0){
			subs<- data[abc,]
			temp<- 0
		if(len(abc==1)){
				subs<- t(subs)}
			for(m in 1:nrow(subs)){
				temp <- temp + sum((subs[m,]- centers[j])^2)
				}
			ssr[j]<- temp
			}
			if(len(abc)==0){
			ssr[j]<- 0}
			}
		
		temps<- which.max(ssr)
		subs<- data[which(clust_sol==temps),]
		output<- kmeans(subs, centers=2)
		test2<- test<- output$cluster
		test2[test==1]<- temps[1]
		test2[test==2]<-  a + 1	
		clust_sol[clust_sol==temps]<- test2
		centers[temps,]<- output$center[1,]
		centers<- rbind(centers, output$center[2,])
		}
		return(clust_sol)
		}




	









