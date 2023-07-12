\name{fuzzy}
\alias{fuzzy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clustering using a 'fuzzy' algorithm for clusters}
\description{
 Performs kmeans clustering, but allows documents to have partial
 membership in different clusters.    
}
\usage{
galileo(affinity, model ='fuzzy', k=10, memb.exp=2,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity}
  \item{model=fuzzy}{selects fuzzy clustering}
  \item{k=10} {Number of clusters}
  \item{memb.exp=2}{Parameter for the 'membership' function.
  \item{...} {parameters passed to the fuzzy algorithm, see help file
    in cluster library for details}.
  }
\details{
  \code{fuzzy} is similar to kmeans, but allows observations to have
  partial cluster membership (and therefore is similar to mixture
  models).  
}
\value{
Returns a vector describing the cluster assignments for each document.  
}
\references{Kaufman and Rousseeuw (1990) Chapter 4. }
\author{Martin Maechler, Peter Rousseeuw, Anja Struyf, Mia Hubert, and
  Kurt Hornik originally implemented kmedoids clustering in the cluster
  package.  Justin Grimmer and Gary King added
  Galileo functionality. }
\note{ 
}
\seealso{kmeans, trimkmeans, kmedoids,lkmeans}
\examples{
##fuzzy clustering of the senate press release data
##.
data(SenPress)
data(CommWords)

aff<- affinity(gt, extra=T, extra.dat=comm.words)
fuzzy<- galileo(aff, model='fuzzy', k=12)
}
