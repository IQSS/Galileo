\name{lkmeans}
\alias{lkmeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clustering using kmeans algorithm for large data sets}
\description{
 Performs kmeans clustering on an object from affinity.  Use only when
 the data set is prohibitively large, resulting in kmeans running too slowly  
}
\usage{
galileo(affinity, model ='lkmeans', k=10,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity}
  \item{model=lkmeans}{selects kmeans clustering for large data sets}
  \item{k=10} {Number of clusters}
  \item{...} {parameters passed to the lkmeans algorithm, see help file
    in cluster library for details}.
  }
\details{
  \code{lkmeans} is similar to kmeans, but estimates the number of
  clusters by subsampling the observations.  Should be used only if
  kmeans performs poorly on the data set.
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
\seealso{kmeans, trimkmeans, kmedoids,fuzzy}
\examples{
##lkmeans clustering of the senate press release data
##.
data(SenPress)
data(CommWords)

aff<- affinity(gt, extra=T, extra.dat=comm.words)
lkmeans<- galileo(aff, model='lkmeans', k=12)
}
