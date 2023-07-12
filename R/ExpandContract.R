##so, the idea is that we click on a point, 
##and the function moves over to that point, 
##then, when it arrives, the other points dim, 
##those points are extended out
##

##want to write a function, called "expand" that takes a point
##computes the close b



expand<- function(store,iters){
	a<- seq(0, 2*pi, len=15)
	circ<- cbind(cos(a), sin(a))*2
	shift<- min(store$top[,2]) - min(circ[,2])
	circ[,2]<- circ[,2] + shift
	tops<- store$top
	top.15<- order(store$weights, decreasing=T)[1:15]
	#stumps<- store$weights/max(store$weights)
	#stumps<- 1- stumps
	tops.15<- tops[top.15,]
	##so now, we just check one point to identify the closest
	dist.circs<- c()
	for(j in 1:15){
		dist.circs[j]<- t(tops.15[1,]- circ[j,])%*%(tops.15[1,] - circ[j,])}
	closest<- which.min(dist.circs)
	if(closest==1){
		parts<- c(1:15)}
	if(closest!=1){
	parts<- c(closest:nrow(circ), 1:(closest-1))}
	diffs<-  circ[parts, ] - tops.15
	travels <- diffs/iters
	x.dim<- c(min(store$bottom[,1], store$top[,1]), max(store$bottom[,1], store$top[,1]))
	y.dim<- c(min(store$bottom[,2], store$top[,2]), max(store$bottom[,2], store$top[,2]))
	clusts<- store$cluster
	uns<- unique(store$cluster)
	scales3<- store$bottom
	matrix.move<- matrix(NA, nrow=nrow(store$top[-top.15,]), ncol=iters)
	stuff<- seq(1, nrow(store$top), by=1)
	stuff<- stuff[-top.15]
	for(j in 1:nrow(matrix.move)){
		matrix.move[j,]<- seq(0, 0.9, len=iters)
		}
	matrix.move2<- matrix(NA, nrow=15, ncol=iters)
	stumps<- store$weights/max(store$weights)
	stumps<- 1 - stumps
	stumps<- stumps[top.15]
	for(j in 1:15){
		matrix.move2[j,]<- seq(0, stumps[j], len=iters)
		}
	cols<- c()
	for(k in 1:len(uns)){
		cols[k]<- rgb(k/len(uns), 0, 1-k/len(uns))
		}
	cols.final<- c()
	for(j in 1:len(uns)){
		cols.final[which(store$cluster==uns[j])]<- cols[j]
		}

	for(j in 1:iters){
		plot(rbind(store$bottom, store$top), pch='', xlim=x.dim, ylim = y.dim, axes=F, frame.plot=F, xlab='', ylab='')
		text(store$top[-top.15,], rownames(store$top)[-top.15], col=gray(matrix.move[,j]), cex=1)
		text(travels*j + store$top[top.15,], rownames(store$top)[top.15], cex=1, col=gray(matrix.move2[,j]))

		abline(h=min(store$top[,2]) - 0.2)
		text(scales3, names2, col=cols.final, cex=1)
		arrows(1.6 ,max(store$top[,2]) - 0.5, 1.6 , max(store$top[,2]) + 0.5,len=0)
		arrows(1.6, max(store$top[,2]) - 0.5, 3, max(store$top[,2]) - 0.5, 1.6, len=0)
		text(1.8,max(store$top[,2])-0.3, 'End')
		}
	means<- matrix(NA, nrow=k, ncol=2)
	for(k in 1:len(uns)){
		out<- which(clusts==uns[k])
		part1<- scales3[out,]
		if(is.null(nrow(part1))){
			part1<- t(part1)}
		means[k,]<- apply(part1, 2, mean)
		points(means[k,1], means[k,2], col=rgb(k/len(uns), 0, 1- k/len(uns)))
		for(j in 1:len(out)){
			arrows(part1[j,1], part1[j,2], means[k,1], means[k,2], len=0, col=rgb(k/len(uns), 0, 1- k/len(uns)))}
	}
	
points<- travels*iters + store$top[top.15,]
names<- rownames(store$top)[top.15]
cols <- gray(matrix.move2[,ncol(matrix.move2)])
lists<- list(points, names, cols)
names(lists)<- c("points", "names", "cols")
return(lists)


	}

contract<- function(store, iters){
	a<- seq(0, 2*pi, len=15)
	circ<- cbind(cos(a), sin(a))*2
	shift<- min(store$top[,2]) - min(circ[,2])
	circ[,2]<- circ[,2] + shift
	tops<- store$top
	top.15<- order(store$weights, decreasing=T)[1:15]
	tops.15<- tops[top.15,]
	##so now, we just check one point to identify the closest
	dist.circs<- c()
	for(j in 1:15){
		dist.circs[j]<- t(tops.15[1,]- circ[j,])%*%(tops.15[1,] - circ[j,])}
	closest<- which.min(dist.circs)
	if(closest==1){
		parts<- c(1:15)}
	if(closest!=1){
	parts<- c(closest:nrow(circ), 1:(closest-1))}

	diffs<-  circ[parts, ] - tops.15
	travels <- diffs/iters
	x.dim<- c(min(store$bottom[,1], store$top[,1]), max(store$bottom[,1], store$top[,1]))
	y.dim<- c(min(store$bottom[,2], store$top[,2]), max(store$bottom[,2], store$top[,2]))
	clusts<- store$cluster
	uns<- unique(store$cluster)
	scales3<- store$bottom
	matrix.move<- matrix(NA, nrow=nrow(store$top[-top.15,]), ncol=iters)
	stuff<- seq(1, nrow(store$top), by=1)
	stuff<- stuff[-top.15]
	for(j in 1:nrow(matrix.move)){
		matrix.move[j,]<- seq(0, .9, len=iters)
		}
	matrix.move2<- matrix(NA, nrow=15, ncol=iters)
	stumps<- store$weights/max(store$weights)
	stumps<- 1 - stumps
	stumps<- stumps[top.15]
	for(j in 1:15){
		matrix.move2[j,]<- seq(0, stumps[j], len=iters)
		}
	cols<- c()
	for(k in 1:len(uns)){
		cols[k]<- rgb(k/len(uns), 0, 1-k/len(uns))
		}
	for(j in iters:1){
		plot(rbind(store$bottom, store$top), pch='', xlim=x.dim, ylim = y.dim, axes=F, frame.plot=F, xlab='', ylab='')
		text(store$top[-top.15,], rownames(store$top)[-top.15], col=gray(matrix.move[,j]), cex=1)
		text(travels*j + store$top[top.15,], rownames(store$top)[top.15], cex=1, col=gray(matrix.move2[,j]))

		abline(h=min(store$top[,2]) - 0.2)
		text(scales3, names2, col=cols, cex=1)
		arrows(1.6 ,max(store$top[,2]) - 0.5, 1.6 , max(store$top[,2]) + 0.5,len=0)
		arrows(1.6, max(store$top[,2]) - 0.5, 3, max(store$top[,2]) - 0.5, 1.6, len=0)
		text(1.8,max(store$top[,2])-0.3, 'End')
		}
	means<- matrix(NA, nrow=k, ncol=2)
	for(k in 1:len(uns)){
		out<- which(clusts==uns[k])
		part1<- scales3[out,]
		if(is.null(nrow(part1))){
			part1<- t(part1)}
		means[k,]<- apply(part1, 2, mean)
		points(means[k,1], means[k,2], col=rgb(k/len(uns), 0, 1- k/len(uns)))
		for(j in 1:len(out)){
			arrows(part1[j,1], part1[j,2], means[k,1], means[k,2], len=0, col=rgb(k/len(uns), 0, 1- k/len(uns)))}
	}

	}



##ok, so now we want to do an expansion on the words.  
expand.words<- function(labels, x.dim, y.dim, no.words=10,iters=50, bottoms, names2, store){
	mean.x<- mean(x.dim)
	mean.y<- mean(y.dim)
	rads<- abs(max(x.dim) - mean.x)*0.8
	rads3<- abs(min(x.dim) - mean.x)*0.8
	rads2<- abs(max(y.dim) - mean.y)*0.8
	rads4<- abs(min(y.dim) - mean.y)*0.8
	rads<- min(rads, rads3)
	rads2<- min(rads2, rads4)
	#rads<- c(rads/(t(rads)%*%rads))
	a<- seq(0, 2*pi, len=11)
	circ<- cbind(cos(a)*rads, sin(a)*rads2)
	circs<- circ
	means<- c(mean(x.dim), mean(y.dim))
	means.mat<- matrix(NA, nrow=11, ncol=2)
	for(j in 1:nrow(means.mat)){
		means.mat[j,]<- means}
	circ<- circ + means.mat
	for(j in 1:nrow(circs)){
	circs[j,]<-  circ[j,]-means.mat[j,]}
	travels<-  circs/iters
	uns<- unique(store$cluster)
		cols<- c()
	for(k in 1:len(uns)){
		cols[k]<- rgb(k/len(uns), 0, 1-k/len(uns))
		}
	cols.final<- c()
	uns<- unique(store$cluster)
	for(j in 1:len(uns)){
		cols.final[which(store$cluster==uns[j])]<- cols[j]
		}
	for(j in 1:iters){
		plot(c(0,1)~c(0,1), ylim=y.dim, xlim=x.dim, axes=F, xlab='', ylab='', pch='')
		text(travels*j + means.mat, labels, col='cornflowerblue', cex=1)
		text(bottoms, names2, cex=1, col=cols.final)
		}
      }




