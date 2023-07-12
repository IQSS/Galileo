\name{clustTable}
\alias{clustTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Latex Table of Cluster Output }
\description{
 This function provides a formatted table to display the ouput from the
 clustering routine.  clustTable returns the proportion of total
 documents in each topic, along with a user specified number of stems to
 label each topic.  
}
\usage{
clustTable(cluster, mut.inf, num = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster}{ An object from galileo }
  \item{mut.inf}{ An object from the mutinf function  }
  \item{num}{ A user specified number of stems to label each category }
}
\details{
  
}
\value{
A LaTeX ready table to display the results of text classification
}
\references{  }
\author{Justin Grimmer and Gary King}
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
