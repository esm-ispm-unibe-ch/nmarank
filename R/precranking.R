#' Precision of treatment ranking
#'
#' The function \code{\link{precranking}} gives the probabilities of treatment
#' hierarchies of network meta-analysis. 
#'
#' @import netmeta mvtnorm devtools
#'
#' @param netmetaobject An object of class \code{\link{netmeta}}
#' @param random A logical indicating whether a random effects meta-analysis should be conducted.
#' @param no_most_prob number of most probable hierarchies, only applicable if most_prob
#' @param nsim number of simulations
#' @param small.values A character string specifying whether small treatment effects indicate a "good" or "bad" effect
#'
#' @return The main result is the \code{Output} is the list of rankings as lists of treatments and
#' their probabilites
#'

precranking = function( netmetaobject
                      , random=T
                      , no_most_prob=NA
                      , nsim=10000
                      , small.values="good")
{


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
  #print(resultsum)

  resultsum$Probability = resultsum$Freq/sum(resultsum$Freq)
  referenceRank = data.frame(treatments = names(rankings[[1]])) %>% mutate(id=1:n())
  
  colnames(resultsum)=c(paste(referenceRank$treatments,collapse = " "),"N","Probability")
  
  mylist=lapply(resultsum[,1], function(x) unlist(strsplit(as.character(x), " ", fixed = TRUE)))
  resultsum$myresult=mylist
  
  mynameslist=lapply(mylist, FUN=function(x) {
    positions = as.numeric(x)
    numpositions = as.numeric(c(1:length(positions)))
    treatments = Reduce(function(ac,i){
      pos = referenceRank$treatments[i]
      ac[positions[i]]=pos
      return(ac)}
      ,numpositions,c()
      )
    res = treatments
    return(paste(res,sep=","))})

  resultsum = resultsum %>% mutate(Hierarchy=mynameslist)
  
  Output=resultsum[,c("Hierarchy","Probability")][order(-as.numeric(resultsum[,c("Hierarchy","Probability")]$Probability)),]
  
  if (!is.na(no_most_prob)) Output=head(Output,no_most_prob)
  
  res <- list(resultsum=resultsum,Output=Output,rankings=rankings,netmetaobject=netmetaobject,random=random,small.values=small.values)
  class(res) <- "precranking"
 #print(res)
  res  
  }
