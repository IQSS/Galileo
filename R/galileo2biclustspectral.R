##this function implement's Dhillon's spectral bi-clustering algorithm


galileo2biclustspectral<- function(affinity, k = 30){
	A<- data <- affinity$data
	A<- t(A)	
	D1<- 1/apply(A, 1, sum)
	D2<- 1/apply(A, 2, sum)
	D1<- diag(D1)
	D2<- diag(D2)
	AN<- D1%*%A%*%D2
	l<- log(k, base=2)
	l<- round(l)
	svds<- svd(AN , nu = l+1, nv = l + 1)
	u<- svds$u
	v<- svds$v
	Z<- rbind(D1%*%u, D2%*%v)
	out<- kmeans(Z, centers=k)$cluster
	output<- list()
	output[[1]]<- 	out[(ncol(data)+1) : len(out)]
	output[[2]]<- out[1:ncol(data)]
	names(output)<- c('documents', 'data')
	return(output)
	}
