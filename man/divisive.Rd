\name{divisive}
\alias{divisive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Divisive Hierarchical Clustering}
\description{
 Performs divisive hierarchical clustering on an object from affinity.  
}
\usage{
galileo(affinity, model ='divisive', k=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity}
  \item{model=divisive}{selects divisive hierarchical clustering}
  \item{k=NULL} {If set to NULL, then a hierarchical clustering solution
  is returned.  If set to a positive integer, the tree is cut to produce
a partion of the data with k clusters}
  \item{...} {parameters passed to the divisive algorithm, see the help
  file in the cluster package.}.
  }
\details{
  \code{hclust}  performs divisive hierarchical clustering of the
  data.  The clustering is divisive because all observations begin in
  the same cluster and then are removed from the initial cluster in
  order to minimize a cost function.  If
  k=NULL an object of class hclust is returned.  If k is set to some
  positive integer, the tree is cut to provide a clustering of the data
  with the requested number of categories.    
  
\value{
An object of class hclust if k=NULL, if k is some positive integer, then
a clustering of the data.  
}
\references{ ~put references to the literature/web site here ~ }
\author{Martin Maechler, Peter Rousseeuw, Anja Struyf, Mia Hubert, and
  Kurt Hornik originally implemented kmedoids clustering in the cluster
  package.  Justin Grimmer and Gary King added
  Galileo functionality. }
\note{ 
}
\seealso{hclust}
\examples{
##divisive hierarchical clustering of the senate press release data
##.
data(SenPress)
data(CommWords)

aff<- affinity(gt, extra=T, extra.dat=comm.words)

##return a partition of the data
divisive.flat<- galileo(aff, model='divisive', k=12)

##return an hclust object
divisive.object<- galileo(aff, model='divisive', k=NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the

