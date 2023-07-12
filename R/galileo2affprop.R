##this function call affinity propagation

galileo2affprop<- function(affinity, p=NULL, lambda=0.6,
                           metric='cosine', maxits=500, convits=50){
  S <- matrix(NA, nrow=nrow(affinity$data),
                ncol=nrow(affinity$data))
   data <- affinity$data
  data <- as.matrix(data)
  prior.orig <- p
  if(metric=='cosine'){
  S <- affinity$cosine
  p <- median(S)-1
}
  if(metric=='euclidean'){
    for(j in 1:nrow(S)){
      for(k in 1:j){
        S[j,k] <- S[k,j] <- sqrt((data[j,]-data[k,])%*%(data[j,]-
                                                        data[k,]))
      }
    }
    max.s <- max(S)
    S <- max.s - S
    p <- median(S)-1
  }
  if(metric=='manhattan'){
   for(j in 1:nrow(S)){
     for(k in 1:j){
       S[j,k] <- S[k,j] <- sum(abs(data[j,]- data[k,]))
     }
   }
   max.s <- max(S)
   S <- max.s-S
   p <- median(S)-1
 }
  if(metric=='info.costs'){
    for(j in 1:nrow(S)){
      for(k in 1:nrow(S)){
        S[j,k] <- siminf(data[j,], data[k,])
      }
    }
    p <- median(S)-120
  }
  if(metric=='maximum'){
    for(j in 1:nrow(S)){
      for(k in 1:j){
        S[j,k] <- S[k,j] <- max(data[j,]-data[k,])
      }
    }
    max.s <- max(S)
    S <- max.s-S
    p <- median(S)-1
  }
  if(metric=='correlation'){
    for(j in 1:nrow(S)){
      for(k in 1:j){
        S[j,k] <- S[k,j] <- c(cor(t(data[j,]), t(data[k,])))
      }
    }
    p <- median(S)-1
  }
  
 if(is.null(prior.orig)==F){
  p <- prior.orig}
  
  lambda <- lambda
  maxits <- maxits
  convits <- convits
  affprop<- function(S, p=-2, lambda=0.6,  maxits=500, convits=50){
    diag(S)<- p
    for(j in 1:nrow(S)){
      for(i in 1:nrow(S)){
        S[i,j]<- S[i,j] + runif(1, -1e-6, 1e-6)}}
    A<- matrix(0, nrow=nrow(S), ncol=nrow(S))
    R<- matrix(0, nrow=nrow(S), ncol=nrow(S))
##we already have the preferences placed along the diagonal
##
    e<- matrix(0, nrow=nrow(S), ncol=convits)
    i<-0
    lambda<- lambda
    netsim<-c()
    dn<-0
    N<- nrow(S)
    tmpidx<- 0


    while(dn==0 ){
      i<- i+1
      Rold<- R 
      AS<- A + S ; 
      Y<- apply(AS, 1, max)
      I<- apply(AS, 1, which.max)
      for(j in 1:nrow(S)){
	AS[j,I[j]]<- -Inf; 
      }
      Y2<- apply(AS, 1, max);
      I2<- apply(AS, 1, which.max);
      R<- S - matrix(Y, nrow=nrow(S), ncol=nrow(S))
      for(j in 1:nrow(S)){
        R[j,I[j]]<- S[j, I[j]] - Y2[j]; 
      }
      R = (1- lambda)*R + lambda*Rold

##now the availabilities
      Aold = A;
      Rp<- matrix(0, nrow=nrow(R), ncol=ncol(R))
      a<- which(R>0)
      Rp[a]<- R[a]
      diag(Rp)<- diag(R)
      at<- apply(Rp, 2, sum)
      A<- matrix(at, nrow=nrow(S), ncol=nrow(S), byrow=T)- Rp
      dA<- diag(A); 
      b<- which(A<0)
      AB<- matrix(0, nrow=nrow(S), ncol=nrow(S))
      AB[b]<- A[b]
      diag(AB)<- dA
      A<-AB
      A<- (1-lambda)*A + lambda*Aold


      E<- (diag(A) +diag(R))>0; 
      e[, (i-1)%%convits+1]<- E; K<- sum(E,na.rm=T);
      if( i>(convits-1)){
        se<- apply(e,1, sum,na.rm=T)
        unconverged<-(sum((se==convits) + (se==0)) != nrow(S))
        if(unconverged==F &&(K>0)| (i ==maxits)) {
          dn<-1}
      }


      if(K>0){ 
        I<- which(diag(A+R)>0); K<- length(I);
        if(K>1){
          tmp<- apply(S[,I], 2, max); c<- apply(S[,I], 1, which.max)
          c[I]<- 1:K ; tmpidx<- I[c]}
        if(K==1){
          tmp<- S[,I]; c<- rep(I,N)
          tmpidx<- c}
        ##want the original ids
        tmpnetsim<- sum(S[(tmpidx-1)*N+1:N])
        netsim[i]<- tmpnetsim
        abtest<- netsim[c(netsim>-Inf & netsim<Inf)]
        if(length(na.omit(abtest))>0){
          plot(abtest[1:length(abtest)], type="l", col="red", ylim=c(min(abtest[1:length(abtest)], na.rm=T)-1, max(abtest[1:length(abtest)], na.rm=T)+1.5))
          text(i, abtest[length(abtest)]+1, as.character(abtest[length(abtest)]), col="red")}


        I<- which(diag(A+R)>0); K<- length(I);
        if(K>1){
          tmp<- apply(S[,I], 1, max); c<- apply(S[,I], 1, which.max)
          c[I]<- 1:K};
        if(K==1){
          tmp<- S[,I]; c<- rep(I,N)}
        ##want the original ids
        for(k in 1:length(unique(I))){
          ii<- which(c==k);
          if(length(ii)>1){ 
            a<- apply(S[ii,ii], 2, sum)
            y<- max(a)
            j<- which(a==max(a))
            I[k]<- ii[j[1]]}}
        if(length(K)>1){
          c<-apply(S[,I], 1, which.max);
          c[I]<- 1:K; tmpidx<-I[c];}
        if(K==1){
          c<- I; tmpidx<- rep(I, N)}

      }
    }
    return(tmpidx)
  }
  out <- affprop(S, p,lambda, maxits, convits)
  uns <- unique(out)
  temp <- rep(0, length(out))
  for(k in 1:length(uns)){
    ab <- which(out==uns[k])
    temp[ab] <- k}
  out <- temp
  return(out)
	}
