\name{spectral}
\alias{spectral}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Ng, Jordan, and Weiss's (2002) Spectral Clustering Algorithm}
\description{
 Computes a set of clusters using a two step algorithm--first the data
 are projected upon a lower dimensional space then clusters are
 uncovered using kmeans.  
}
\usage{
galileo(affinity, model ='spectral', k=10,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity}
  \item{model=spectral}{selects spectral clustering}
  \item{k=10} {selects the number of clusters in the data and the
  dimensionality of the space used for clustering.  }
  }
\details{
  \code{spectral}  Implements Ng, Jordan, and Weiss's (2002) algorithm
  for spectral clustering.  Suppose that we have a matrix of affinities
  A as output for affinity.  Define D to be the diagonal matrix where
  D_ii is the sum of the ith row of A.  Then we can define the
  'laplacian' of the affinity matrix as L = D^(-1/2) A D^(-1/2). 
  Using the laplacian spectral clustering proceeds in two steps.  First,
  the k largest eigenvectors of the laplacian are collected into a
  matrix and normalized to have unit length.  Then kmeans is performed
  on this representation of the data.  
}
\value{
Returns a partition of the data with k clusters.  
}
\references{  Ng, A. Jordan, MI. Weiss, Y  "On Spectral Clustering:
  Analysis and an Algorithm", Advances in Neural Information Processing Systems}
  \author{  Justin Grimmer and Gary King implementated spectral
    clustering in  R and added
  Galileo functionality. }
\note{ 
}
\seealso{kmeans}
\examples{
##spectral clustering of the senate press release data
##.
data(SenPress)
data(CommWords)

aff<- affinity(gt, extra=T, extra.dat=comm.words)

##compute a partition of the data
spectral<- galileo(aff, model='spectral',k=12 )


% Add one or more standard keywords, see file 'KEYWORDS' in the

