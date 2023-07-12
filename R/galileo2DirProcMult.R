##this file implements a multinomial text clustering model with a dirichlet
##process prior

##this is based on Blei and Jordan's variational methods, were the Dirichlet
##Process Prior is represented by the "stick breaking" approach.  


##while the true posterior is an infinite mixture model, we approximate 
##that posterior with a truncated variational approximation.  Note, that only the 
##approximation (and not the true posterior) are approximated. We can end up 
##with "unused" clusters, which basically means that the process does not 
##grow to those clusters

galileo2DirProcMult<- function(affinity, k=50, iter = 10){
data<- affinity$data

##defining a lower bound for the Dirichlet process prior
##which we will use to assess the results


lower.bound.mix<- function(output, data){

alpha<- output$alpha
thetas<- output$taus
rs<- output$phis
gammas<- output$gammas
k<- ncol(rs)

gam.sum<- apply(gammas, 1, sum)
gams.gams<- digamma(gammas[,2]) - digamma(gam.sum)
part.a<- sum((alpha-1)*(gams.gams - digamma(gam.sum)))
sum.thetas<- apply(thetas, 1, sum)
digam.thetas<- digamma(thetas) - digamma(sum.thetas)
digam.thetas<- -0.9*sum(digam.thetas)
part.b<- k*lgamma(0.1*k) - (k^2)*lgamma(0.1)
mat.adds<- matrix(NA, nrow=nrow(rs), ncol=ncol(rs))
for(i in 1:nrow(mat.adds)){
	for(j in 1:(k-1)){
		mat.adds[i,j]<- sum(rs[i,(j+1):k])
		}
	}
mat.adds[,k]<- 0
sums.rs<- 0
for(i in 1:nrow(data)){
	for(j in 1:k){
		sums.rs<- sums.rs + mat.adds[i,j]*gams.gams[j] + 
				rs[i,j]*(digamma(gammas[j,1]) - gam.sum[j])
			}
		}
sum.etas<- 0
digam.thetas<- digamma(thetas) - digamma(sum.thetas)
y.inner<- data%*%t(digam.thetas)
for(i in 1:nrow(data)){
	sum.etas<- sum(rs[i,]*y.inner[i,]) + sum.etas
	}
rs.smooth<- rs
for(j in 1:nrow(rs)){
	rs.smooth[j,]<- (rs.smooth[j,] + 1e-08)/sum(rs.smooth[j,] + 1e-08)}
log.rs<- log(rs.smooth)
r.log.rs<- rs*log.rs
sum.rs.entropy<- sum(r.log.rs)

entropy.part1<- sum(lgamma(gam.sum)) - sum(lgamma(gammas)) + sum(gammas[,1]*(digamma(gammas[,1]) - digamma(gam.sum)) + gammas[,2]*(digamma(gammas[,2]) - digamma(gam.sum)))
entropy.part2<- sum(lgamma(sum.thetas)) - sum(lgamma(thetas))+ sum((thetas - 1)*digam.thetas)

bounds<- part.a + part.b + sums.rs + sum.etas - sum.rs.entropy - entropy.part1 - entropy.part2
	
return(bounds)
}

#require(Brobdingnag)

Temp<- k
data<- data

best.data<- -Inf
	for(v in 1:iter){
	gammas<- matrix(NA, nrow=Temp, ncol=2)
	for(j in 1:Temp){
		gammas[j,]<- rgamma(2, shape=10)
		}
	taus<- matrix(NA, nrow=Temp, ncol=ncol(data))
	for(j in 1:Temp){
		taus[j,]<- rgamma(ncol(data),shape=10 ) + 0.01
		}

	prior.taus<- .001

	a<- 0
	z<- 0
	phis<- matrix(NA, nrow=nrow(data), ncol=Temp)
	shape1<- shape2<- 1
	alpha<- rgamma(1, shape=shape1, rate=1/shape2)


	while(z==0){
	a<- a + 1
	if(a>1){
		gam.old<- gammas}
	gam.sum<- apply(gammas, 1, sum)
	tau.sum<- apply(taus, 1, sum)
	digam.sum<- digamma(gam.sum)
	digam.gam<- digamma(gammas[,1]) - digam.sum
	digam2<- digamma(gammas[,2])
	gam.rec<- c()
	gam.rec[1]<- 0
	di.tau<- digamma(taus)
	ditau.diff<- taus
	for(j in 1:Temp){
	ditau.diff[j,]<- di.tau[j,] - digamma(tau.sum[j])
		}

	for(j in 2:Temp){
		gam.rec[j]<- sum(digam2[1:(j-1)] -  digam.sum[1:(j-1)])
		}
	y.tau.inner<- data%*%t(ditau.diff)
	for(j in 1:nrow(data)){
		bc<- y.tau.inner[j,]  + digam.gam + gam.rec
        	bc <- bc- max(bc)
        	phis[j,] <- exp(bc)/sum(exp(bc))
		}

	sum.gam<- apply(phis, 2, sum)
	gam1<- 1 + sum.gam
	gam2<- c()
	gam2[Temp]<- alpha
	for(j in 1:(Temp-1)){
		gam2[j]<- alpha + sum(sum.gam[(j+1):k])
		}
	gammas<- cbind(gam1, gam2)

	for(m in 1:Temp){
		temp<- rep(prior.taus, ncol(data))
		for(j in 1:nrow(data)){
		temp<- temp + phis[j,m]*data[j,]
		}
	taus[m,]<- temp
	}
	w1<- shape1 + Temp-1
	diff<- sum(digamma(gammas[,2]) - digamma(apply(gammas, 1, sum)))
	w2<- shape2 - diff
	alpha<- w1/w2
	if(a>1){
		diff<- gammas- gam.old
		if(max(diff)<1e-5){
			z<- 1}
		}
	if(a%%10==0){
		print(a)}
	}
	w<- c(w1, w2)
	output<- list(phis,gammas, taus, w)
	names(output)<- c('phis', 'gammas', 'taus', 'w') 
	bounds<- lower.bound.mix(output, data)
	if(bounds>best.data){
		best.phis<- phis
		best.gammas<- gammas
		best.taus<- taus
		best.alpha<- alpha
		best.data<- bounds
		}
	}
	list.out<- output
	#names(list.out)<- c('rs', 'gamma', 'etas', 'w')
	#return(list.out)
	clust<- apply(list.out$phis, 1, which.max)
	uns<- unique(clust)
	clust2<- clust
	for(k in 1:len(uns)){
		clust2[which(clust==uns[k])]<- k
		}
	return(clust2)
	}

