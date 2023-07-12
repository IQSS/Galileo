########
##this program constructs the cosine 
##distribution from an undergrad object
##but we'd also like this to create a more general affinity matrix
##so call it affinity 


affinity <-
function(undergrad, tfidf=T, type='cosine', stop=T, extra=F, extra.dat=NULL,
			sphere=T, mult=F){

fr2<- undergrad$testset
fr2<- fr2[,4:ncol(fr2)]

name.cols<- colnames(fr2)

for(j in 1:length(name.cols)){
name.cols[j]<- strsplit(name.cols[j], split=".", fixed=T)[[1]][2]
}

colnames(fr2)<- name.cols

if(stop==T){
data(stop)
b<- 0
elims<- c()
for(j in 1:length(name.cols)){
test<- name.cols[j] %in% as.character(out[,1])
if(test==T){
b<- b+1
elims[b]<- j}
}
fr2<- fr2[,-elims]
name.cols<- name.cols[-elims]
}



if(extra==T){
  if(is.null(extra.dat)==T){
	stop('Must Supply Extra Words if extra=T', '\n')
	}
  else{
	b<- 0
	elims<- c()
	for(j in 1:length(name.cols)){
	test<- name.cols[j] %in% as.character(extra.dat[,1])
	if(test==T){
		b<- b+1
		elims[b]<- j}
	}
	fr2<- fr2[,-elims]
	name.cols<- name.cols[-elims]
	}
}

if(mult==T){
counts<- fr2}

if(tfidf==T){
stumps<- c()
for(a in 1:ncol(fr2)){
stumps[a]<- sum(fr2[,a]>0)}

fr2<- as.matrix(fr2)

idfs<- log(nrow(fr2)/stumps)
re.weight<- matrix(NA, nrow=nrow(fr2), ncol=ncol(fr2))
for(i in 1:nrow(re.weight)){
tt.st<- fr2[i,]*idfs
re.weight[i,]<- tt.st
}
}
if(tfidf==F){
re.weight<- as.matrix(fr2)}


A<- re.weight%*%t(re.weight)
a<-diag(A)
##so now we can create the representation 

if(sphere==T){
for(j in 1:nrow(re.weight)){
re.weight[j,]<- re.weight[j,]/a[j]
}
}


cos.mat<- matrix(NA, nrow=nrow(re.weight), ncol=nrow(re.weight))
for(i in 1:nrow(cos.mat)){
for(j in 1:i){
as<- a[i]; sa<- a[j]
cross<- A[i,j]
if(as>0 & sa>0){
cos.mat[i,j]<- cos.mat[j,i]<- cross/(sqrt(as*sa))
}
else{
cos.mat[i,j]<- cos.mat[j,i]<- 0}
}
}

test <- apply(cos.mat, 1, sum)
if(any(test==0)){
  warning('There are documents with no words.  Remove document from
analysis or include stop words')}

if(mult==F){
output<- list(cos.mat, re.weight)
names(output)<- c('cosine', 'data')}

if(mult==T){
output<- list(cos.mat, re.weight, counts)
names(output)<- c('cosine', 'data', 'counts')
}
return(output)
}








