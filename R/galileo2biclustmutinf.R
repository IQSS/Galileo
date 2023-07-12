##rewriting this, but with functions (do to the large number of calls to the 
##same function


##writing a function to create the initial matrix

galileo2biclustmutinf<- function(affinity, k = 10, w.k= 20){

init.matrix<- affinity$data
init.matrix<- as.matrix(init.matrix)

margs<- apply(init.matrix, 1, sum)
for(j in 1:nrow(init.matrix)){
	init.matrix[j,]<- (1/nrow(init.matrix))*(init.matrix[j,]/margs[j])
	}

examp.mat<- init.matrix


examp.mat<- init.matrix<- as.matrix(examp.mat)



z<- a<- 0

verbose<-T
output<- list()

##may want to update this when you get a chance
##to allow for any general start values
output[[1]]<- kmeans(examp.mat, centers=10)$cluster
output[[2]]<- kmeans(t(examp.mat), centers=20)$cluster
n.clust<- k
n.cols<- w.k


q.approx<- function(){
	mat.approx<- matrix(NA, nrow=n.clust, ncol=n.cols)
	for(j in 1:nrow(mat.approx)){
		for(k in 1:ncol(mat.approx)){
			abc<- which(output[[1]]==j)
			if(len(abc)==1){
			temp.mat<- t(as.matrix(examp.mat[abc,]))}
			if(len(abc)>1){
			temp.mat<- examp.mat[abc,]}
			arch<- which(output[[2]]==k)
			mat.approx[j,k]<- sum(apply(temp.mat, 2, sum)[arch])
			}
		}
	return(mat.approx)
	}

q.x.xhat<- function(){
	clust<- output[[1]]
	mat.x.xhat<- matrix(0, nrow=len(unique(clust)), ncol=nrow(examp.mat))
	xs<- apply (examp.mat, 1, sum)
	for(j in 1:nrow(mat.x.xhat)){
		abc<- which(clust==j)
		tots<- sum(xs[abc])
		mat.x.xhat[j,abc]<- xs[abc]/tots
		}
	return(mat.x.xhat)
	}

q.y.yhat<- function(){
	cols<- output[[2]]
	mat.y.yhat<- matrix(0, nrow=len(unique(cols)), ncol=ncol(examp.mat))
	ys<- apply (examp.mat, 2, sum)
	for(j in 1:nrow(mat.y.yhat)){
		abc<- which(cols==j)
		tots<- sum(ys[abc])
		mat.y.yhat[j,abc]<- ys[abc]/tots
		}
	return(mat.y.yhat)
	}

q.y.xhat<- function(mat.approx){
	clust <- output[[1]]
	mat.y.xhat<- matrix(NA, nrow=len(unique(clust)), ncol=ncol(examp.mat))
	ys<- apply(examp.mat, 2, sum)
	for(j in 1:nrow(mat.y.xhat)){
		for(k in 1:ncol(mat.y.xhat)){
		abc<- cols[k]
		tent<- which(cols==abc)
		tents<- sum(ys[tent])
		part1<- ys[k]/tents
		part2<- mat.approx[j,abc]/sum(mat.approx[j,])
		mat.y.xhat[j,k]<- part1 *part2
	}
	}
	return(mat.y.xhat)
	}

q.x.yhat<- function(mat.approx){
	clust<- output[[1]]
	mat.x.yhat<- matrix(NA, nrow=len(unique(cols)), ncol=nrow(examp.mat))
	for(j in 1:nrow(mat.x.yhat)){
		for(k in 1:ncol(mat.x.yhat)){
		abc<- clust[k]
		tent<- which(clust==abc)
		xs<- apply(examp.mat, 1, sum)
		tents<- sum(xs[tent])
		part1<- xs[k]/tents
		part2<- mat.approx[abc,j]/sum(mat.approx[,j])
		mat.x.yhat[j,k]<- part1 *part2
	}
	}
	return(mat.x.yhat)
	}

distance.y.x<- function(x, x.hat, mat.y.xhat){
		p.y.x<- examp.mat[x,]/sum(examp.mat[x,])
		abc<- which(p.y.x>0)
		out<- sum(p.y.x[abc] * log(p.y.x[abc]/mat.y.xhat[x.hat,abc]))
		return(out)
		}
distance.x.y<- function(y, y.hat, mat.x.yhat){
		p.x.y<- examp.mat[,y]/sum(examp.mat[,y])
		abc<- which(p.x.y>0)
		out<- sum(p.x.y[abc] * log(p.x.y[abc]/mat.x.yhat[y.hat,abc]))
		return(out)
		}

obj.function<- function(mat.approx){
		clust<- output[[1]]
		out<- 0
		xs<- apply(examp.mat, 1, sum)
		for(k in 1:len(unique(clust))){
			abc<- which(output[[1]]==k)
			for(j in 1:len(abc)){
				out<- out + xs[abc[j]]*distance.y.x(abc[j], k, mat.approx)}}
			return(out)
		}


a<- 0
z<- 0

while(z==0){
	a<- a + 1
	clust<- output[[1]]
	cols<- output[[2]]
	if(a>1){
		test.old.objective<- test.objective}
	mat.approx.1<- q.approx()
	mat.x.xhat.1<- q.x.xhat()
	mat.y.yhat.1<- q.y.yhat()
	mat.y.xhat.1<- q.y.xhat(mat.approx=mat.approx.1)
	
	for(j in 1:nrow(examp.mat)){
		temp.dist<-c()
			for(k in 1:n.clust){
			temp.dist[k]<- distance.y.x(j,k, mat.y.xhat.1)}
			test<-min(temp.dist)
			test<- which(temp.dist==test)
			if(len(test)==1){
				clust[j]<- test}
			if(len(test)>1){
				 if(clust[j] %in% test){
					clust[j]<- clust[j]}
				 else{
					temp<- sample(1:len(test),1)
					clust[j]<- test[temp]
					}
				}
			}
	output[[1]]<- clust
	mat.approx.2<- q.approx()
	mat.x.xhat.2<- q.x.xhat()
	mat.y.yhat.2<- q.y.yhat()
	mat.x.yhat.2<- q.x.yhat(mat.approx.2)
	
	for(j in 1:ncol(examp.mat)){
		temp.dist<-c()
			for(k in 1:n.cols){
			temp.dist[k]<- distance.x.y(j,k, mat.x.yhat.2)}
			test<-min(temp.dist)
			test<- which(temp.dist==test)
			if(len(test)==1){
				cols[j]<- test}
			if(len(test)>1){
				 if(cols[j] %in% test){
					cols[j]<- cols[j]}
				 else{
					temp<- sample(1:len(test),1)
					cols[j]<- test[temp]
					}
				}
			}
	output[[2]]<- cols
	mat.approx.3<- q.approx()
	max.x.xhat.3<- q.x.xhat()
	mat.y.yhat.3<- q.y.yhat()
	mat.y.xhat.3<- q.y.xhat(mat.approx.3)
	test.objective<- obj.function(mat.approx=mat.y.xhat.3)
	if(a>1){
		emp<- abs(test.objective - test.old.objective)
		if(emp<1e-3){
			z<- 1}
		}
if(verbose==T){
	print(a)}
	}
	names(output)<- c('Documents', 'Words')
	return(output)
	}





