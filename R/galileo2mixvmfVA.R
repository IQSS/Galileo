##this file will estimate a mixture of 
##vMF distributions.  


galileo2mixvmfVA<- function(affinity, k=10, kappa=100, prior.mu=NULL, 
				prior.alpha=NULL,tol=0.001,...){
	data<- affinity$data
	data<- as.matrix(data)
	lens<- diag(data%*%t(data))
	if(any(lens==0)){
	print("warning")
	abc<- which(lens==0)
	cat("document(s)", abc, "has no stems", '\n')
	lens[abc]<- 1}
	##normalizing to have unit length
	data2<- data/sqrt(lens)
	if(is.null(prior.mu)==T){
		prior.mu<- rep(1/sqrt(ncol(data2)), ncol(data2))
		}
	if(is.null(prior.mu)==F){
		prior.mu<- prior.mu}
	if(is.null(prior.alpha)==T){
		prior.alpha<- rep(1, k)
		}
	if(is.null(prior.alpha)==F){
		prior.alpha<- prior.alpha}
	taus<- matrix(NA, nrow=nrow(data), ncol=k)
	require(MCMCpack)
	pis<- rgamma(k, shape=5, rate=1/5)

	mus<- matrix(NA, nrow=ncol(data), ncol=k)
	for(j in 1:ncol(mus)){
	mus[,j]<- rgamma(ncol(data), shape=10)
	}
	for(j in 1:ncol(mus)){
	mus[,j]<- mus[,j]/sqrt(mus[,j]%*%mus[,j])
	}
	kappa<- kappa
	z<- 0
	a<- 1
	while(z==0){
	if(a>1){
	taus.old<- taus}
	pi.gam<- digamma(pis) - digamma(sum(pis))
	exp.pi<- exp(pi.gam)
	kernel<- exp(kappa*(data2%*%mus))
	for(j in 1:nrow(data2)){
	kernel[j,]<- exp.pi*kernel[j,]}
	taus<- kernel/apply(kernel, 1, sum)
	pis<- rep(1, k)
	mle<- apply(taus, 2, sum)
	pis<- (pis + mle)	
	mus<- rep(1/sqrt(ncol(data)), ncol(data))
	for(j in 2:k){
	mus<- cbind(mus, rep(1/sqrt(ncol(data)), ncol(data)))
	}
	for(i in 1:nrow(data)){
	mus<- mus + data2[i,]%o%taus[i,]
	}
	len.mus<- diag(t(mus)%*%(mus))
	for(j in 1:ncol(mus)){
	mus[,j]<- mus[,j]/sqrt(len.mus[j])}
	if(a>1){
	if(max(taus.old - taus)<tol){
		z<- 1}
	}
	a<- a + 1
	if(a%%10==0){
		print(a)}
	}
	clusters<- apply(taus, 1, which.max)
	return(clusters)
	}






