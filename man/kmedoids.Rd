\name{kmedoids}
\alias{kmedoids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clustering Using kmedoids algorithm}
\description{
 Performs kmedoids clustering on an object from affinity.  
}
\usage{
galileo(affinity, model ='kmedoids', k=10,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity}
  \item{model=kmedoids}{selects kmedoids clustering}
  \item{k=10} {Number of clusters}
  \item{...} parameters passed to the various methods, see cluster library for details.
  }
\details{
  \code{kmedoids} is similar to kmeans, but cluster centers are
  restricted to be observations from the data set.  The objective
  function is the same as kmeans--minimize the distance between
  observations and cluster centers using a fixed number of clusters--but
  the domain of optimization on centers is restricted.
}
\value{
Returns a vector describing the cluster assignments for each document.  
}
\references{Kaufman and Rousseeuw (1990) Chapter 2. }
\author{Martin Maechler, Peter Rousseeuw, Anja Struyf, Mia Hubert, and
  Kurt Hornik originally implemented kmedoids clustering in the cluster
  package.  Justin Grimmer and Gary King added
  Galileo functionality. }
\note{ 
}
\seealso{kmeans, trimkmeans, lkmeans,fuzzy}
\examples{
##kmeans clustering of the senate press release data
##.
data(SenPress)
data(CommWords)

aff<- affinity(gt, extra=T, extra.dat=comm.words)
kmedoid<- galileo(aff, model='kmedoids', k=12)
}
