##this function attempts to speed up 
##interact.func to allow for better interaction
##to do so, we strip out the animation at each step and just allow the points
##to move linearly from start to ending point



visualize<- function(circle=T, start=c(0,0), noclust=25){
 
z<- 0
orig.point<- start
scales<- top.part(dist.mat)
store<- func.test(orig.point,scales, noclust=noclust)
#Sys.sleep(0.1)
if(circle==T){
test<- expand(store, iters=50)}
part1<- store$bottom
labs<- func.clusters(data, store$cluster)
while(z==0){
	outs<- locator(n=1)
	new.point<- c(outs$x, outs$y)
	if(new.point[2]>max(store$top[,2])-0.5 & new.point[1]>1.6){
		return(store)}
	if(new.point[2]> (-2.2)){
	if(circle==T){
	contract(store, iters=50)}
	  vec<- new.point - orig.point
	  len.vec<- sqrt(vec%*%vec)
	  iters<- round(20*len.vec)
	  vec.quart<- vec/iters
	  store2<- func.test(new.point, scales, noclust=noclust, plot=F)
	  izzy<- store2$bottom
	  diff<- izzy - part1
	  x.dim<- c(min(store$top[,1], store$bottom[,1]), max(store$top[,1], store$bottom[,1]))
	  y.dim<- c(min(store$bottom[,2]), max(store$top[,2]))
	  vec.quart2<- diff/as.numeric(iters)
	  for(j in 1:iters){
	   travel<- orig.point + vec.quart*j
	   points<- part1 + vec.quart2*j
	   #store<- func.test(travel, noclust=noclust)
	   travel.points(store, points=points, travel=travel, x.dim=x.dim, y.dim=y.dim)
	   #part1<- store$bottom
	   #scales.point<- part1
	  }
	  store<- func.test(new.point, scales,noclust=noclust)
	 orig.point<- new.point
	part1<- store$bottom
	if(circle==T){
	test<- expand(store, iters=50)}
	labs<- func.clusters(data, store$cluster)
	 }
	if(new.point[2]< (min(store$top[,2])- 0.2)){
		a<- 0
		while(a==0){
			dists<-c()
			cent<- store$centers
			for(j in 1:nrow(store$centers)){
				diff<- new.point - cent[j,]
				diff<- sqrt(diff%*%diff)
				dists[j]<- diff}
			mins<- which.min(dists)
			print(labs[mins,])
			gtr<- which(store$cluster == unique(store$cluster)[mins])
			part1<- store$bottom
			x.rang<- c(min(part1[gtr,1]), max(part1[gtr,1]))
			y.rang<- c(min(part1[gtr,2]), max(part1[gtr,2]))
			parts<- rbind(store$bottom, store$top)
			init.x.rang<- c(min(parts[,1]), max(parts[,1]))
			init.y.rang<- c(min(parts[,2]), max(parts[,2]))
			seq.x<- cbind(seq(init.x.rang[1], x.rang[1], len=50), 
						seq(init.x.rang[2], x.rang[2], len=50))
			seq.y<- cbind(seq(init.y.rang[1], y.rang[1], len=50), 
						seq(init.y.rang[2], y.rang[2], len=50))
			clusts<- store$cluster
			uns<- unique(store$cluster)
			for(m in 1:nrow(seq.x)){
				if(circle==T){
				plot(rbind(test$points, store$top), pch='', xlim=seq.x[m,], ylim = seq.y[m,], axes=F, frame.plot=F, xlab='', ylab='')
				text(store$top, rownames(dist.mat), cex=1, col=gray(0.9))
				text(test$points, test$names, cex=1, col=test$cols)}
				if(circle==F){
					plot(store$top, pch='', xlim=seq.x[m,], ylim = seq.y[m,], axes=F, frame.plot=F, xlab='', ylab='')
					text(store$top, rownames(dist.mat), cex=1, col='black')}

				abline(h=min(store$top[,2]) - 0.2)
				for(k in 1:len(uns)){
					out<- which(clusts==uns[k])
					part2<- store$bottom[out,]
					if(is.null(nrow(part2))){
						part2<- t(part2)}
						text(part2, names2[out], col=rgb(k/len(uns), 0, 1-k/len(uns)), cex=1)
						points(store$centers[k,1], store$centers[k,2], col=rgb(k/len(uns), 0, 1- k/len(uns)), pch=20)}
						}
			expand.words(labs[mins,], x.dim=seq.x[nrow(seq.x),], y.dim=seq.y[nrow(seq.y),], bottoms=store$bottom, names2= names2, store=store)
			k<- unique(store$cluster)[mins]
			out<- which(clusts==k)
			part2<- part1[out,]
			for(j in 1:len(out)){
				arrows(part2[j,1], part2[j,2], store$centers[mins,1], store$centers[mins,2], len=0, col=rgb(mins/len(dists), 0, 1- mins/len(dists)))}
			a<- 1}
		tt<- 0
		sub.files<- files[gtr]
		corner<- (x.rang[2] - x.rang[1])/8
		corner2<- (y.rang[2]- y.rang[1])/8
		p1<- corner + x.rang[1]
		p2<- corner2 + y.rang[1]
		arrows(p1, p2, x.rang[1]-2, p2, len=0,col='black')
		arrows(p1, p2, p1, y.rang[1]-2, len=0, col='black')	
		text(x.rang[1], (p2 + y.rang[1])/2, 'Return')
		#arrows(x.rang[1]-2, p2, x.rang[1], y.rang[1], len=0, col='black')
		#arrows(x.rang[1]-2, y.rang[1], p1, y.rang[1],len=0,col='black')
		while(tt==0){
			frt<- locator(n=1)
			point.frt<- c(frt$x, frt$y)
		##start here, same remedy
		if(point.frt[1]<p1 & point.frt[2]<p2){
				tt<- 1
				for(m in nrow(seq.x):1){
				if(circle==T){
				plot(rbind(test$points, store$top), pch='', xlim=seq.x[m,], ylim = seq.y[m,], axes=F, frame.plot=F, xlab='', ylab='')
				text(store$top, rownames(dist.mat), cex=1, col=gray(0.9))
				text(test$points, test$names, cex=1, col=test$cols)}
				if(circle==F){
				plot(rbind(store$bottom, store$top), pch='', xlim=seq.x[m,], ylim = seq.y[m,], axes=F, frame.plot=F, xlab='', ylab='')
				text(store$top, rownames(dist.mat), cex=1, col='black')}

				abline(h=min(store$top[,2]) - 0.2)

			for(k in 1:len(uns)){
			out<- which(clusts==uns[k])
			part2<- part1[out,]
			if(is.null(nrow(part2))){
				part2<- t(part2)}
				text(part1[which(clusts==uns[k]),], names2[which(clusts==uns[k])], col=rgb(k/len(uns), 0, 1-k/len(uns)), cex=1)
					points(store$centers[k,1], store$centers[k,2], col=rgb(k/len(uns), 0, 1- k/len(uns)), pch=20)}
					}
			means<- matrix(NA, nrow=k, ncol=2)
	uns<- unique(store$cluster)
	for(k in 1:len(uns)){
		out<- which(store$cluster==uns[k])
		part3<- store$bottom[out,]
		if(is.null(nrow(part3))){
			part3<- t(part3)}
		means[k,]<- apply(part3, 2, mean)
		points(means[k,1], means[k,2], col=rgb(k/len(uns), 0, 1- k/len(uns)), pch=20)
		for(j in 1:len(out)){
			arrows(part3[j,1], part3[j,2], means[k,1], means[k,2], len=0, col=rgb(k/len(uns), 0, 1- k/len(uns)))}
	}
				#store<- func.test(orig.point)
		arrows(1.6 ,max(store$top[,2]) - 0.5, 1.6 , max(store$top[,2]) + 0.5,len=0)
		arrows(1.6, max(store$top[,2]) - 0.5, 3, max(store$top[,2]) - 0.5, 1.6, len=0)
		text(1.8,max(store$top[,2])-0.3, 'End')
		}
		if(point.frt[1]>p1 | point.frt[2]>p2){
			dists<- c()
			for(j in 1:nrow(part2)){
				diff<- point.frt- part2[j,]
				temp<- sqrt(t(diff)%*%(diff))
				dists[j]<- temp
				}
			mins<- which.min(dists)
			#ester<- file(sub.files[mins], 'r')
			local(file.show(sub.files[mins],header=sub.files[mins]))
			#temps<- suppressWarnings(readLines(ester))
			#print(temps)
			#close(ester)
			}
			}
		}##close the first if}
	}##close the big while	}
		}##close the function}

