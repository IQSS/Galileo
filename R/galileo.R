##this file is the wrapper for each of galileo's functions

galileo<- function(affinity, model, ...){
	fn1<- paste('galileo2', model, sep='')
	mf<- galileo.call<- match.call(expand.dots= TRUE)
	
	mf<- do.call(fn1, list(affinity, ...))
	#res<- eval(as.call(mf))

	return(mf)
	}
