\name{hybridHclust}
\alias{hybridHclust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Chipman and Tibshirani's (2006) hybrid hierarchical clustering algorithm.}
\description{
 Performs hybrid hierarchical clutering on an object from affinity.  
}
\usage{
galileo(affinity, model ='hybridHclust', k=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity}
  \item{model=hybridHclust}{selects hybrid hierarchical clustering}
  \item{k=NULL} {If k is set to a positive integer, then a partition of
    the data set is returned with k clusters.}
  \item{...} {parameters passed to the hybridHclust lgorithm, see hybridHclust help
  file in hybridHclust package.}.
  }
\details{
  \code{hybridHclust} is a hybrid of top-down and bottom up hierarchical
clusterin methods and is intended to exploit the strengths of both
approaches.  Top-down approaches are best able to identify a small
number of clusters, whereas bottom-up approaches are able to identify
a large number of small clusters.  To make this connection between two
methods, hybridHclust first identifies a group of very close together
observations (termed, mutual clusters).  Then, a top-down clustering is
run within each mutual cluster.  }
}
\value{
If k =NULL, then an object of class hclust is returned.  If k is a
positive integer, then a clustering of the data is returned with k
categories.   
}
\references{  Chipman, H. and Tibshirani, R (2006} "Hybrid Hierarchical
Clustering with Application to Microarray Data", Biostatistics, 7, 302-317.}
\author{Hugh Chipman implemented hybridHclust in R,
  Justin Grimmer and Gary King added
  Galileo functionality. }
\note{ 
}
\seealso{divisive, hclust}
\examples{
##hybridge hierarchical clustering of the senate press release data
##.
data(SenPress)
data(CommWords)

aff<- affinity(gt, extra=T, extra.dat=comm.words)

##compute a partition of the data
hybrid.Flat<- galileo(aff, model='hybridHclust', k=12)

##compute a dendrogram
hybrid.dendrogram<- galileo(aff, model='hybridHclust', k=NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the

