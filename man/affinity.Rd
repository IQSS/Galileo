\name{affinity}
\alias{affinity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes Similarity (affinity) between documents }
\description{
  As currently implemented, takes an object from undergrad and
  computes the affinity between the documents.  Currenlty, only cosine
  is implemented.  
}
\usage{
affinity(undergrad, tfidf = T, type = "cosine", stop = T, extra = F, extra.dat = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{undergrad}{An object from the undergrad function }
  \item{tfidf}{If tfidf=T, calculate tf-idf weights to aid in topic
  detection}
\item{type}{Type of affinity to compute, currently only cosine is available
}
  \item{stop}{If stop=T, stop words are removed }
  \item{extra}{If extra=T affinity checks for an additional set of words
    to remove from the analysis}
  \item{extra.dat}{Used only if extra=T, the additional set of words to
    remove from the undergrad object}
}
\details{
 Computes the cosine between documents.  Its output it used by Galileo
 to find topics in the data.  
}
\value{
  If there are K- documents, returns a K-K matrix of affinities between
  documents
}
\references{ }
\author{Justin Grimmer and Gary King }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
