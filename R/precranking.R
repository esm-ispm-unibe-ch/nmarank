###################################################################################################
#                   A function to measure precision in treatment hierarchy
###################################################################################################
#       Arguments:
#netmetaobject: An object of class netmeta
#random: A logical indicating whether a random effects meta-analysis should be conducted.
#no_most_prob: number of most probable hierarchies, only applicable if most_prob
#nsim: number of simulations
#small.values: A character string specifying whether small treatment effects indicate a "good" or "bad" effect
###################################################################################################

precranking = function (netmetaobject,random=T,no_most_prob=5,nsim=1000,small.values="good")
{
  require(netmeta)
  require(devtools)
  require(mvtnorm)

  if(random==T) {
    TE <- netmetaobject$TE.random
    simul <- rmvnorm(nsim, t(TE)[lower.tri(TE)], netmetaobject$Cov.random)
  }
  if(random==F) {
    TE <- netmetaobject$TE.fixed
    simul <- rmvnorm(nsim, t(TE)[lower.tri(TE)], netmetaobject$Cov.fixed)
  }
    
  resampling <- lapply(1:nsim,function(x) matrix(NA,nrow=nrow(TE),ncol=ncol(TE),dimnames = list(rownames(TE), colnames(TE))))
  
  rearrange <- function(x){
    resampling[[x]][lower.tri(resampling[[x]])] <- simul[x,]
    resampling[[x]] <- t(resampling[[x]])
    resampling[[x]][lower.tri(resampling[[x]])] <- -simul[x,]
    resampling
  }
  
  resampling <-lapply(1:nsim,function(x) rearrange(x)[[x]])

  if(small.values=="good"){rankings<-lapply(1:nsim,function(x) rank(rowSums(resampling[[x]]> 0, na.rm = T)))}
  if(small.values=="bad"){rankings<-lapply(1:nsim,function(x) (netmetaobject$n + 1)-rank(rowSums(resampling[[x]]> 0, na.rm = T)))}
  
  resultsall=sapply(1:nsim,function(x) rbind(paste(rankings[[x]],collapse=" ")))
  resultsum=as.data.frame(table(resultsall))

  resultsum$Probability=resultsum$Freq/sum(resultsum$Freq)
  colnames(resultsum)=c(paste(names(rankings[[1]]),collapse = " "),"N","Probability")
  
  mylist=lapply(resultsum[,1], function(x) unlist(strsplit(as.character(x), " ", fixed = TRUE)))
  resultsum$myresult=mylist
  
  treatnames=unlist(strsplit(names(resultsum)[1], " ", fixed = TRUE))
  mynameslist=lapply(1:nrow(resultsum), FUN=function(x) {return(as.character(treatnames[as.numeric(mylist[[x]])]))})

  resultsum$Hierarchy=mynameslist
  
  Output=resultsum[,c("Hierarchy","Probability")][order(-resultsum[,c("Hierarchy","Probability")]$Probability),]
  
  if (!is.na(no_most_prob)) Output=head(Output,no_most_prob)
  
  res <- list(resultsum=resultsum,Output=Output,rankings=rankings,netmetaobject=netmetaobject,random=random,small.values=small.values)
  class(res) <- "precranking"

  res  
  }
  
  
