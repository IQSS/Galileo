\name{trimkmeans}
\alias{trimkmeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Robust kmeans clustering by 'trimming' the data.}
\description{
 Performs robust kmeans clustering on an object from affinity by
 trimming outlying observations.    
}
\usage{
galileo(affinity, model ='trimkmeans', k=10, trim=0.1,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity}
  \item{model=trimkmeans}{selects trimmed k-means clustering}
  \item{k=10} {Number of clusters}
  \item{trim=0.1}{Proportion of points to be trimmed.  Between 0 and 1.
  \item{...} {parameters passed to trimkmeans, see help file
    in trimcluster library for details}.
  }
\details{
  \code{trimkmeans} is similar to kmeans, but is more robust to
  outlying observations that are removed from the data set (trimmed).
  Should be used if there are data that are highly discrepant from other
  observations.
}
\value{
Returns a vector describing the cluster assignments for each document.  
}
\references{Cuesta-Albertos, J.A., Grodaliza, A., and Matran C. (1997)
  Trimmed kmeans : An Attempt to Robustify Quantizers, Annals of
  Statistics, 25, 553-576}
\author{Christian Henning implementated
  the trimkmeans algorithm in the package 'trimcluster'.
  Justin Grimmer and Gary King added
  Galileo functionality. }
\note{ 
}
\seealso{kmeans, kmedoids, lkmeans,fuzzy}
\examples{
##trimkmeans clustering of the senate press release data
##.
data(SenPress)
data(CommWords)

aff<- affinity(gt, extra=T, extra.dat=comm.words)
timkmeans<- galileo(aff, model='trimkmeans', k=12)
}
