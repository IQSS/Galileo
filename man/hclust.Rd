\name{hclust}
\alias{hclust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Agglomerative Hierarchical Clustering}
\description{
 Performs agglomerative hierarchical clustering on an object from affinity.  
}
\usage{
galileo(affinity, model ='hclust', k=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity}
  \item{model=hclust}{selects agglomerative hierarchical clustering}
  \item{k=NULL} {If set to NULL, then a hierarchical clustering solution
  is returned.  If set to a positive integer, the tree is cut to produce
a partion of the data with k clusters}
  \item{...} {parameters passed to the hclust algorithm, see hclust help
  file in stats package.}.
  }
\details{
  \code{hclust}  performs agglomerative hierarchical clustering of the
  data.  The clustering is agglomerative because all observations start
  in their own clusters, then are joined together to minimize a cost
  function (which depends upon the method of clustering selected).  If
  k=NULL an object of class hclust is returned.  If k is set to some
  positive integer, the tree is cut to provide a clustering of the data
  with the requested number of categories.    
  
\value{
An object of class hclust if k=NULL, if k is some positive integer, then
a clustering of the data.  
}
\references{ ~put references to the literature/web site here ~ }
\author{X IMPLEMENTED hclust in R, Justin Grimmer and Gary King added
  Galileo functionality. }
\note{ 
}
\seealso{divisive}
\examples{
##hierarchical agglomerative clustering of the senate press release data
##.
data(SenPress)
data(CommWords)

aff<- affinity(gt, extra=T, extra.dat=comm.words)

##return a partition of the data
hclust.flat<- galileo(aff, model='hclust', k=12)

##return an hclust object
hclust.object<- galileo(aff, model='hclust', k=NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the

