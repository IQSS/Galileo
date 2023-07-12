\name{affprop}
\alias{affprop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dueck and Frey's (2007) Affinity Propagation Clustering Algorithm}
\description{
 Computes a set of clusters from the data using affinity propagation.
 This algorithm automatically computes the number of clusters in the
 data and provides a more robust maximum than maximization using an EM
 algorithm.  Like k-medoids, affinity propagation selects
 a set of 'exemplar' observations that as cluster centers.  Affinity
 propagation can be used to compute an approximate mixture of
 von-Mises Fisher distributions, which is useful for text clustering.  
}
\usage{
galileo(affinity, model ='affprop', p=-2, lambda=0.6, maxits=500, convits=50,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{affinity}{Output from affinity}
  \item{model=affprop}{selects affinity propagation for clustering}
  \item{p=-2} {Essentially a prior on number of components in the
  document.  Can range from negative infinity to 1.  Details below.}
  \item{lambda =0.6} {Dampening parameter for maximization algorithm}.
  \item{maxits=500} {Maximum number of iterations to be performed.}
  \item{convits=50} {Number of iteration necessary to establish}
  convergence.  
  }
\details{
  \code{affprop}  Implements Dueck and Frey's (2007) clustering
  algorithm affinity propagation.  Affinity propagation identifies a set
of 'exemplar' observations that are highly likely to be cluster centers
(and therefore is similar in spirit to k-medoids).  Optimization is
performed using loopy belief propagation a recently developed algorithm
for approximate inference of NP-Hard problems that provides a much
better guaranteed maximum than can be guaranteed by an EM algorithm.\\

The number of clusters is estimated from the data and the p.  If using
the cosine distribution for document clustering, then p is equal to
log(Pr(Randomly chosen document is an exemplar)) + 1 and therefore can
range from 1 to negative infinity.  lambda is used to dampen the
messages, which is necessary to ensure convergence of the optimization
procedure.

Affinity propagation does not require an underlying statistical model
and will provide a reasonable set of clusters so long as there is a
function defined on the data that is increasing as two objects become
more similar.  It need not satisfy any of hte usual assumptions imposed
upon metrics.  

}
\value{
Returns a partition of the data with an estimated number of topics.  
}
\references{  Frey and Dueck (2007)  "Clustering by Passing Messages
  Between Data Points", Science, 972-976.}
  \author{  Justin Grimmer and Gary King ported Brendan Frey and Delbert
    Dueck's
    implementation of affinity propagation to
    R and added
  Galileo functionality. }
\note{ 
}
\seealso{kmedoids}
\examples{
##affinity propagation to cluster the senate press release data
##.
data(SenPress)
data(CommWords)

aff<- affinity(gt, extra=T, extra.dat=comm.words)

##compute a partition of the data
aff.prop<- galileo(aff, model='affprop',p= median(log(aff$cosine)) )


% Add one or more standard keywords, see file 'KEYWORDS' in the

