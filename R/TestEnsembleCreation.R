##i want to try this for a few points, so let's write a function


##need to do two things.  first, do the ensemble, then rescales


rotate<- function(scales4){
J<- diag(1,nrow(scales4)) - (1/nrow(scales4))*rep(1,nrow(scales4))%*%t(rep(1,nrow(scales4)))
C<- t(scales.point)%*%J%*%scales4
P<-svd(C)$u; Q<- svd(C)$v
T<- Q%*%t(P)
t<- (1/nrow(scales4))*t(scales.point - scales4%*%t(T))%*%rep(1,nrow(scales4))
X.final<- scales4%*%T + rep(1, nrow(scales4))%*%t(t)
return(X.final)
}




func.test<- function(point, noclust=25, plot=T){
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
	tents<- kmeans(sims, center=noclust)	
	clusts<- tents$cluster
	scales3<- cmdscale(sims)
	scales3<- rotate(scales3)

	comp<- c(min(scales[,1]), max(scales[,1]))
	comp2<- c(min(scales3[,1]), max(scales3[,1]))
	scale.x<- 4/abs(comp[1] - comp[2])
	scales[,1]<- scale.x*scales[,1]
	scale.x1<- 4/abs(comp2[1] - comp2[2])
	scales3[,1]<- scale.x1*scales3[,1]
	shift.x<-  - 2 - min(scales3[,1])
	scales3[,1] <- scales3[,1] + shift.x
	shift.x1<- -2 - min(scales[,1])
	scales[,1]<- scales[,1] + shift.x1
	comp3<- c(min(scales[,2]), max(scales[,2]))
	comp4<- c(min(scales3[,2]), max(scales3[,2]))
	scale.y<- 4/(abs(comp3[1] - comp3[2]))
	scales[,2]<- scale.y*scales[,2]
	scales.y1<- 4/(abs(comp4[1] - comp4[2]))
	scales3[,2]<- scales.y1*scales3[,2]
	shift<-  abs(min(scales[,2]) - max(scales3[,2]))
	scales3[,2]<- scales3[,2] -  shift-0.5
	x.dim<- c(min(scales[,1], scales3[,1]), max(scales[,1], scales3[,1]))
	uns<- unique(clusts)
	if(plot==T){
		plot(scales3, pch='', ylim=c(min(scales3[,2]), max(scales[,2])), xlim=x.dim, frame.plot=F, axes=F, xlab='', ylab='')
		text(scales3, names2, cex=1)
		text(scales, rownames(scales), cex=1) #col=gray(stumps))
		arrows(1.6 ,max(scales[,2]) - 0.5, 1.6 , max(scales[,2]) + 0.5,len=0)
		arrows(1.6, max(scales[,2]) - 0.5, 3, max(scales[,2]) - 0.5, 1.6, len=0)
		text(1.8, max(scales[,2])-0.3, 'End')
		lines<- min(scales[,2])
		abline(h=lines-0.2)
		points(point[1], point[2], col='red', pch=20, cex=2)
		for(k in 1:len(uns)){
			text(scales3[which(clusts==uns[k]),], names2[which(clusts==uns[k])], col=rgb(k/len(uns), 0, 1-k/len(uns)), cex=1)
			}}
		means<- matrix(NA, nrow=k, ncol=2)
		for(k in 1:len(uns)){
			out<- which(clusts==uns[k])
			part1<- scales3[out,]
			if(is.null(nrow(part1))){
				part1<- t(part1)}
			means[k,]<- apply(part1, 2, mean)
			if(plot==T){
			points(means[k,1], means[k,2], col=rgb(k/len(uns), 0, 1- k/len(uns)))
			for(j in 1:len(out)){
				arrows(part1[j,1], part1[j,2], means[k,1], means[k,2], len=0, col=rgb(k/len(uns), 0, 1- k/len(uns)))}
		}}
#	plot(dists~dists.2[-94])
#	abline(lm(dists~dists.2[-94]))
	list.output<- list(scales3, scales, point, means, clusts, weights)
	names(list.output)<- c('bottom', 'top', 'point', 'centers', 'cluster', 'weights')
	return(list.output)
	}
	
