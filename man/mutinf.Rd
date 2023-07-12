\name{mutinf}
\alias{mutinf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cluster Labels by Mutual Information}
\description{
  Computes a set of labels for each document based upon the additional
  information each word provides for predicting the members of each
  cluster. 
}
\usage{
mutinf(cluster, undergrad, stop = T, extra = F, extra.dat = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster}{Cluster is an object from galileo }
  \item{undergrad}{An undergrad object used to compute cluster}
  \item{stop}{Remove stop words?  Should agree with the call to
  galileo}
  \item{extra}{Remove additional words? Should agree with call to galileo}
  \item{extra.dat}{Additional words to be removed--Should agree with
    call
  to galileo}
}
\details{
  Takes an undergrad object and a clustering solution as arguments and
  returns the mutual information for each and cluster.  The mutual
  information identifies words that are common for topics within
  cluster, but rare outside of the cluster.  
}
\value{
If there are K topics and W stems, returns a K by W matrix of each
word's mutual information for each topic.  
}
\references{ Stanford Book }
\author{Justin Grimmer and Gary King}
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
