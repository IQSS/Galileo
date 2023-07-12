##this function gathers a set of validation indices
##using some of the functions from the 
##clv packages
##so we will collect them all and then return them
##allowing the users to access each (because they all should be 
##relatively quick to compute

clustercomp<- function(clust1, clust2){
  require(clv)
  require(mclust)
  temp<- std.ext(clust1, clust2)
  ss<- temp$SS
  sd<- temp$SD
  ds<- temp$DS
  dd<- temp$DD
  un.vals1<- unique(clust1)
  un.vals2<- unique(clust2)
  counts1<- rep(0, len(un.vals1))
  counts2<- rep(0, len(un.vals2))
  for(j in 1:len(counts1)){
	counts1[j]<- len(which(clust1==un.vals1[j]))
	}
  for(k in 1:len(counts2)){
	counts2[k]<- len(which(clust2==un.vals2[k]))
	}
  temp1<- counts1 - 1
  temp2<- counts2 - 1
  temp11<- counts1*temp1
  temp22<- counts2*temp2
   part1<- sum(temp11/2)
   part2<- sum(temp22/2)
   fm<- ss/(sqrt(part1)*sqrt(part2))
   rand<- (ss + dd)/(ss + dd + ds + sd)
   adj.rand<- adjustedRandIndex(clust1, clust2)
   jacc<- (ss)/(ss + ds + sd)
   mirkin<- 2*(sd + ds)
   rr<- ss/(ss + ds + sd + dd)
   phi<- (ss*dd - sd*ds)/((ss + ds)*(ss + sd)*(sd+dd)*(ds + dd))
   conf_mat<- confusion.matrix(clust1, clust2)
   max.row<- apply(conf_mat, 1, max)
   max.col<- apply(conf_mat, 2, max)
   vd_metric<- 2*len(clust1) - sum(max.row) - sum(max.col)
   max.which.row<- apply(conf_mat, 1, which.max)
   max.which.col<- apply(conf_mat, 2, which.max)
   la_index1<- sum(2*max.row/(counts1 + counts2[max.which.row]) )/len(un.vals1)
   la_index2<- sum(2*max.col/(counts2 + counts1[max.which.col]) )/len(un.vals2)
   
  output<- list(fm, rand, adj.rand, jacc, mirkin, rr, phi, vd_metric, la_index1, la_index2)
  names(output)<- c('fm', 'rand', 'adj.rand', 'jacc', 'mirkin', 'rr', 'phi', 'vd_metric', 'la_index1', 'la_index2')
  return(output)
  }
