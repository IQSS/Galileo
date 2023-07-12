`mutinf` <-
function(cluster, undergrad, stop=T, extra=F, extra.dat=NULL){

##temporary fix to allow for two different
  ##kinds of objects

 if(length(undergrad)==9){
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
}

name.cols<- name.cols[-elims]

colnames(fr2) <- name.cols
}
if(length(undergrad)==2){
  fr2<- undergrad$data}
 

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
        name.cols <- name.cols[-elims]
        colnames(fr2) <- name.cols
	}
}




uns<- unique(cluster)
mut.inf<- matrix(NA, nrow=length(uns), ncol=ncol(fr2))
for(i in 1:length(uns)){
swert<- which(cluster==uns[i])
in1<- fr2[swert,]
out1<- fr2[-swert,]
if(is.null(ncol(in1))==T){
	in1<- t(as.matrix(in1))
	}
zaw<- apply(in1, 2, sum)
for(j in 1:ncol(fr2)){
if(zaw[j]>0){
N11<- sum(in1[,j]>0) 
N<- nrow(fr2) 
N01<- sum(in1[,j]==0) 
N10<- sum(out1[,j]>0)
N00<- sum(out1[,j]==0) 
N1dot<- N11 + N10
Ndot1<- N01 + N11
Ndot0<- N10 + N00
N0dot<- N01 + N00
part1<- (N11/N)*(log((N*N11)/(N1dot*Ndot1), 2))
part2<- (N01/N)*(log((N*N01)/(N0dot*Ndot1), 2))
part3<- (N10/N)*(log((N*N10)/(N1dot*Ndot0), 2))
part4<- (N00/N)*(log((N*N00)/(N0dot*Ndot0), 2))
temp<- part1 + part2 + part3 + part4
if(length(na.omit(temp))==0){
mut.inf[i,j]<-0}
else{
mut.inf[i,j]<-temp }
}
else{
mut.inf[i,j]<- 0}
}
}

colnames(mut.inf)<- colnames(fr2)
return(mut.inf)
}

