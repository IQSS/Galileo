##puttting together the new plots





travel.points<- function(store, points, travel, x.dim, y.dim){
plot(store$bottom, pch='', ylim=y.dim, xlim=x.dim, frame.plot=F, axes=F, xlab='', ylab='')
text(points, names2, cex=1)
text(store$top, rownames(store$top), cex=1) #col=gray(stumps))
arrows(1.6 ,max(store$top[,2]) - 0.5, 1.6 , max(store$top[,2]) + 0.5,len=0)
arrows(1.6, max(store$top[,2]) - 0.5, 3, max(store$top[,2]) - 0.5, 1.6, len=0)
text(1.8, max(store$top[,2])-0.3, 'End')
lines<- min(store$top[,2])
abline(h=lines-0.2)
points(travel[1], travel[2], col='red', pch=20, cex=2)
clusts<- store$cluster
uns<- unique(clusts)
for(k in 1:len(uns)){
	text(points[which(clusts==uns[k]),], names2[which(clusts==uns[k])], col=rgb(k/len(uns), 0, 1-k/len(uns)), cex=1)
	}
	}
