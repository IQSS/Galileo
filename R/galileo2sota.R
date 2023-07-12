##this file implements the self-organizing tree algorithm
##for clustering.  the original implentation is in the 
##clValid package.  


galileo2sota<- function(affinity, k=10, distance='correlation',...){
if(!('clValid' %in% .packages(all=TRUE)))
    install.packages('clValid')
  require(clValid)
   data<- affinity$data
   max<- k - 1
   out<- sota(data, maxCycles= max, distance = distance)
   return(out$clust)
   }


   
