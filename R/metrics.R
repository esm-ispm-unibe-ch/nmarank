##################################################################################################
### Calculates the probability of a hierarchy based on a ranking metric ##########################
##################################################################################################
# precobject: An object of class precranking
# hierarchy: the hierarchy of which precision is to be measured: means, sucras, pBV, pretas

metrics=function(precobject,hierarchy){
  
  
  if (!inherits(precobject, "precranking"))
    stop("Argument 'precobject' must be an object of class \"precranking\"")

  if(precobject$random==T) {
    TE <- precobject$netmetaobject$TE.random
  }
  if(precobject$random==F) {
    TE <- precobject$netmetaobject$TE.fixed
  }
  
  #create table with hierarchies
  hierarchies <- matrix(NA, 5, ncol(TE), dimnames = list(c("pBV","sucras","pscores","means","pretas"), colnames(TE)))
  
  rankings=matrix(unlist(precobject$rankings), ncol = precobject$netmetaobject$n, byrow = TRUE, 
                  dimnames = list(1:length(precobject$rankings),names(precobject$rankings[[1]])))
  
  p.rank=matrix(NA,nrow(TE),ncol(TE),dimnames = list(rownames(TE), 1:precobject$netmetaobject$n))
  for (i in 1:nrow(p.rank)) {
    for (j in 1:ncol(p.rank)) {
      p.rank[i,j] <- sum(rankings[,i]==j)/length(precobject$rankings)
    }
  }
  
  p.rank_1st <- c()
  for (i in 1:nrow(p.rank)) {
    p.rank_1st[i]=p.rank[i,1]
  }
  
  #create vector of rankings based on prob of being the best
  hierarchies["pBV",]=(nrow(p.rank) + 1) - rank(p.rank_1st)
  
  ### calculate SUCRAs
  # first calculate cumulative probabilities
  p.rank.cum=matrix(NA,nrow(p.rank),ncol(p.rank),dimnames = list(rownames(p.rank), 1:precobject$netmetaobject$n))
  for (i in 1:nrow(p.rank)) {
    p.rank.cum[i,] <- cumsum(p.rank[i,])
  }
  # then calculate SUCRA for each treatment
  SUCRA <- vector(mode="logical", length = nrow(p.rank))
  names(SUCRA) <- rownames(p.rank)
  for (i in 1:nrow(p.rank.cum)) {
    SUCRA[i] <- round(sum(p.rank.cum[i,-length(p.rank.cum[i,])])/(nrow(p.rank.cum)-1), digits = 3)
  }
  #create vector of rankings based on SUCRA
  hierarchies["sucras",]=(nrow(p.rank) + 1) - rank(SUCRA)
  
  ##calculate vector of rankings based on P-score
  if(precobject$random==T) {
    hierarchies["pscores",]=(nrow(p.rank) + 1) - rank(netrank(precobject$netmetaobject, small.values = precobject$small.values)$Pscore.random)
  }
  if(precobject$random==F) {
    hierarchies["pscores",]=(nrow(p.rank) + 1) - rank(netrank(precobject$netmetaobject, small.values = precobject$small.values)$Pscore.fixed)
  }
  
  if (precobject$small.values=="good") hierarchies["means",] <- rank(TE[,1]) else hierarchies["means",] <- (precobject$netmetaobject$n + 1) - rank(TE[,1])
  
  altnetp1=alternativenma::alternativenma(netmetaobject=precobject$netmetaobject,random=precobject$random, small.values=precobject$small.values)
  
  averages=as.data.frame(altnetp1$averages)
  pretas=averages[match(colnames(rankings),averages$treat),]$Pscoreaverage
  
  hierarchies["pretas",]=(nrow(p.rank) + 1) - rank(pretas)
  
  res1=as.data.frame(apply(hierarchies, 1, paste, collapse=" "))
  colnames(res1)=c(paste(colnames(rankings),collapse = " "))
  
  resultsum=as.data.frame(prec1$resultsum)
  res2=as.data.frame(res1)
  res2$Metric=rownames(res2)
  answer=merge(resultsum,res2, by=colnames(res1))
  
  Output=answer[,c("Metric","Hierarchy","Probability")]
  
  Result=Output[Output$Metric==hierarchy,]

  Result
}
