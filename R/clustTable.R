`clustTable` <-
function(cluster, mut.inf, num=5){

counts<- c()
uns<- unique(cluster)
for(j in 1:length(uns)){
counts[j]<- length(which(cluster==uns[j]))
}

prop<- round(counts/sum(counts),3)
orders<- order(prop,decreasing=T)

cat('\\begin{table}[hbt!]', '\n')
cat('\\begin{center}', '\n')
cat('\\begin{tabular}{ll}', '\n')
cat('\\hline\\hline', '\n')
cat('Prop. & Stems \\\\', '\n')
for(j in 1:length(uns)){
words<- order(mut.inf[orders[j], ], decreasing=T)[1:num]
words<- colnames(mut.inf)[words]
outtie<- paste(as.character(prop[orders[j]]), '&', sep='')
outtie<- paste(outtie, words[1], sep='')
for(k in 2:num){
outtie<- paste(outtie, paste(',', words[k], sep=''), sep='')
}
outtie<- paste(outtie, '\\\\', sep='')
cat(outtie, '\n')
}
cat('\\hline', '\n')
cat('\\end{tabular}', '\n')
cat('\\end{center}', '\n')
cat('\\end{table}', '\n')
}

