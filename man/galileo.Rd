\name{galileo}
\alias{galileo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for Topic Estimation}
\description{
  This function performs a variety of clustering algorithms for
  topic-discovery in text.  
}
\usage{
galileo(affinity, model ='hclust', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity, matrix describing the affinity
    between observations}
  \item{model}{The clustering method desired for analysis.  Currently,
    the following methods are available:
    agglomerative hierarchical clustering \code{ hclust},
     divisive hierarchical clustering \code{divisive},
     hybrid hierarchical clustering \code{hybridHclust},
     k-means \code{kmeans},
     k-medoids \code{kmedoids},
     affinity Propagation \code{affprop},
     trimmed k-means \code{trimkmeans},
     fuzzy k-means \code{fuzzy},
     k-means for large data sets \code{lkmeans}, hard competitive
     learning \code{hardcl}, neural gas clustering \code{neuralgas}, qt
     clustering \code{qtclust}, and spectral clustering (normalized)
     \code{spectral}.  See the help-files for each method for a full
     description about the strengths and weaknesses of each method of
     clustering. }
  \item{...} {parameters passed to the various methods, see the help
  files for each method}.
  }
\details{
  \code{galileo} is a function that provides several different
  clustering algorithms, all using the output from \code{affinity}.
}
\value{
Returns a vector describing the cluster assignments for each document.  
}
\references{ ~put references to the literature/web site here ~ }
\author{Justin Grimmer and Gary King implemented galileo in R }
\note{ 
}
\seealso{The help files for each method}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the

