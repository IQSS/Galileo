##this file implements a multinomial text clustering model with a dirichlet
##process prior

##this is based on Blei and Jordan's variational methods, were the Dirichlet
##Process Prior is represented by the "stick breaking" approach.  


##while the true posterior is an infinite mixture model, we approximate 
##that posterior with a truncated variational approximation.  Note, that only the 
##approximation (and not the true posterior) are approximated. We can end up 
##with "unused" clusters, which basically means that the process does not 
##grow to those clusters

galileo2DirProcMultUnder<- function(affinity, k=50, ...){
require(Brobdingnag)

Temp<- k
data<- affinity$data
gammas<- matrix(NA, nrow=Temp, ncol=2)
for(j in 1:Temp){
	gammas[j,]<- rgamma(2, shape=10)
	}
taus<- matrix(NA, nrow=Temp, ncol=ncol(data))
for(j in 1:Temp){
	taus[j,]<- rgamma(ncol(data),shape=10 ) + 0.01
	}



prior.taus<- 1

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
	gam2[j]<- alpha + sum(sum.gam[(j+1):T])
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
w2<- shape2 + sum(gammas[1:(T-1),2] - digamma(gammas[1:(Temp-1), 2]))
alpha<- w1/w2
if(a>1){
	diff<- gammas- gam.old
	if(max(diff)<1e-5){
		z<- 1}
	}
if(a%%10==0){
	print(a)}
}
clust<- apply(phis, 1, which.max)
uns<- unique(clust)
clust2<- clust
for(k in 1:len(uns)){
	clust2[which(clust==uns[k])]<- k
		}
return(clust2)
}
