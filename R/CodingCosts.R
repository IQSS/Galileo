##calculating the information theoretic costs 
##of coding document 1 with document 2


siminf<- function(doc1, doc2){
	abc<- which(doc1>0 & doc2>0)
	ba<- - len(abc) * log(sum(doc2))
	ca<- - (sum(doc1) - len(abc))*log(len(doc1))
	out<- ba + ca 
	return(out)
	}
