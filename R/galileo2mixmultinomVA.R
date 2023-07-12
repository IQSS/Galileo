##this implements a fully Bayesian Mixture of Multinomial 
##using a mean-field approximation



galileo2mixmultinomVA<- function(affinity, k = 10,iter=10, prior.pi= 1, prior.theta = 0.01){
	
	lower.bound.mix<- function(output,data){
		k<- ncol(output$taus)
		alpha.par<- lgamma(k) - sum( lgamma(1))
		##setting priors on pis and thetas to be 1 simplifies things
		dig1<- digamma(output$pis) - digamma(sum(output$pis))
		rs1<- output$taus%*%dig1
		rs1<- sum(rs1)
		dig.thetas<- digamma(output$thetas) - digamma(apply(output$thetas, 1, sum))
		data.thetas<- data%*%dig.thetas
		sums<- rep(0, ncol(data.thetas))
		for(j in 1:nrow(data.thetas)){
			sums<- sums + output$taus[j,]*data.thetas[j,]
			}
		sums.r.thetas<- sum(sums)
		gamma.par<- lgamma(sum(output$pis)) - (output$pis - 1)*sum(digamma(output$pis) - digamma(sum(output$pis)))
		gamma.par<- sum(gamma.par)	
		smoothed<- output$taus + 1e-20
		smoothed<- smoothed/apply(smoothed, 1, sum)
		log.r<- log(smoothed)
		r.log.r<- smoothed* log.r
		rsr<- sum(r.log.r)
		eta.part<- suppressWarnings(lgamma(sum(output$thetas)) - sum(lgamma(output$thetas)))
		eta.part2<- (output$thetas-1)*(digamma(output$thetas) - digamma(sum(output$thetas)))
		etas<- eta.part + sum(eta.part2)
		bound<- alpha.par + rs1 + sums.r.thetas - gamma.par - rsr - etas
		return(bound)
		}
	data<- affinity$data
	data<- as.matrix(data)
	Y<- data
	require(MCMCpack)
	
	best.data<- -Inf
	for(v in 1:iter){
	pis<- rgamma(k, shape=1, rate=1)

	thetas<- matrix(NA, nrow=ncol(Y), ncol=k)
	
	for(j in 1:ncol(thetas)){
		thetas[,j]<- rgamma(ncol(Y), shape=1, rate=1)
		}

	z<- 0
	a<- 0
	taus<- matrix(NA, nrow=nrow(Y), ncol=k)

        alpha<- 1
        lambda <- 1
        
	while(z==0){
	a <- a + 1
	if(a>1){
		thetas.old<- thetas}
	pi.gam<- digamma(pis) - digamma(sum(pis))
	theta.gam<- digamma(thetas) - digamma(apply(thetas, 1, sum))
	y.theta<- Y%*%theta.gam
	y.theta<- y.theta + matrix(pi.gam, nrow=nrow(Y), ncol=k)
	for(j in 1:nrow(y.theta)){
		y.temp<- y.theta[j,] - max(y.theta[j,])
		exp.y<- exp(y.temp)
		taus[j,]<- exp.y/sum(exp.y)
		}
	
	pis<- alpha + apply(taus, 2, sum)
	thetas<- lambda + t(Y)%*%taus

	if(a>1){
	afr<- max(abs(thetas - thetas.old))
	if(afr<1e-4){
		z<- 1}
	}
	}
	output<- list(pis, taus, thetas)
	names(output)<- c('pis', 'taus', 'thetas')
	
	bounds<- lower.bound.mix(output, data)
	if(bounds>best.data){
		best.thetas<- thetas
		best.pis<- pis
		best.taus<- taus
		best.data<- bounds
		}
	}
	clusters<- apply(best.taus, 1, which.max)
	return(clusters)
	}


