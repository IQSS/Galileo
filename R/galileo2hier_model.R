##this file implements the "model based" hierarchical clustering 
##algorithm from Freeley's work on hierarchical clustering


##

galileo2hier_model<- function(affinity, k=10, model='EEE',...){
   data<- affinity$data
   require(mclust)
   out<- hc(model, data)
   out<- hclass(out, k)
   return(out)
   }
