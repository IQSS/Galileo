##implements qt_clust from the flexclust package

galileo2qtclust<- function(affinity, radius=0.2, ...){
  if(!('flexclust' %in% .packages(all=TRUE)))
    install.packages('flexclust')
  require(flexclust)
  x <- affinity$data
  radius <- radius
  out <- qtclust(x, radius, ...)
  return(out)
	}
