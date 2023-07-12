\name{kmeans}
\alias{kmeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clustering Using kmeans algorithm}
\description{
 Performs kmeans clustering on an object from affinity.  
}
\usage{
galileo(affinity, model ='kmeans', k=10,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity}
  \item{model=kmeans}{selects kmeans clustering}
  \item{k=10} {Number of clusters.}
  \item{...} {parameters passed to the kmeans algorithm, see kmeans help
  file in stats package.}.
  }
\details{
  \code{kmeans} is the primary clustering method used to discover groups
  in data.  The goal of k-means is to minimize the sum of squared errors
  between points in clusters and the cluster center.  The basic
  algorithm repeats two basic steps until converge (after an initial set of cluster
  centers are generated randomly).  First, observations are assigned to
  the cluster with the center that is closest--in terms of squared
  distance.  Conditional on the clusters assigned in the first step, the
  cluster center is recalculated.  Several restarts are usually necessary,
  as the kmeans algorithm will tend to get stuck in local maxima}
}
\value{
Returns a vector describing the cluster assignments for each document.  
}
\references{ ~put references to the literature/web site here ~ }
\author{X IMPLEMENTED KMEANS in R, Justin Grimmer and Gary King added
  Galileo functionality. }
\note{ 
}
\seealso{kmedoids, trimkmeans, lkmeans,fuzzy}
\examples{
##kmeans clustering of the senate press release data
##.
data(SenPress)
data(CommWords)

aff<- affinity(gt, extra=T, extra.dat=comm.words)
kmean<- galileo(aff, model='kmeans', k=12)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the

