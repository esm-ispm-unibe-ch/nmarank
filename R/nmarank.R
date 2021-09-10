#' Checks wether the effects matrix \code{TE} and the variance-covariance matrix
#' have consistent names it 
#' @param TE a matrix with the NMA relative effects. Row and column names should
#' be present and correspond to the respective treatment label. See example
#' @param Cov variance-covariance matrix of the relative treatment effects
#' row and column names should refer to the respective comparison. Comparisons 
#' should be formatted as \code{treat1:treat2}
#' 
#' data("Woods2010")
#' see for example
#' p1 <- pairwise(treatment, event = r, n = N,
#'               studlab = author, data = Woods2010, sm = "OR")
#' Conduct network meta-analysis
#' net1 <- netmeta(p1)
#' TE <- net1$TE.random
#' Cov <- net1$Cov.random
#' prepareNMAInputs(TE, Cov)
#' 
#' @return an object of class \code{nmaEffects} with \code{TE} a tibble with the 
#' relative effects and \code{Cov} the variance-covariance matrix provided as 
#' argument.
#' @export 
prepareNMAEffects = function(TE, Cov) {
  if((!all(rownames(Cov)==colnames(Cov))) 
     | !isSymmetric.matrix(as.matrix(Cov),tol=10^(-6))){
    stop(paste("Variance-Covariance matrix Cov should be symmetric",
                  "\n  ",
                  "Please also check row and column names are the same",
                  "\n  ",
                  sep = ""))
  }
  comps <- rownames(Cov)
  REs <- TE %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      pivot_longer(cols = -rowname) %>%
      unite(name, rowname, name, sep = ":") %>%
      filter(name %in% comps)
  if((!all((REs$name) == rownames(Cov))) | is_empty(REs$value)){
    stop(paste("Relative Effects and Cov matrix do not match",
                  "\n  ",
                  "Please check treatment labels and dimensions",
                  "\n  ",
                  sep = ""))
  }else{
    res <- list(TE=TE, RE=REs$value, Cov = Cov)
    class(res) <- "nmaEffects"
    return(res)
  }
}

#' Precision of treatment ranking
#'
#' The main function of the package\code{\link{nmarank}} gives the probabilities 
#' of treatment hierarchies of network meta-analysis.
#'
#' @import netmeta mvtnorm tidyverse 
#'
#' @param x either an object of class \code{\link{netmeta}} or a list with the 
#' effects and the variance-covariance list(TE=TE, Cov=Cov)
#' @param nsim number of simulations
#' @param small.values A character string specifying whether small treatment 
#' effects indicate a "good" or "bad" effect
#' @param predicate defines the condition that a possible relative effects 
#' vector should comply to. See \code{select-ranks}
#' 
#' @return The main result is the \code{Output} is the list of rankings as lists 
#' of treatments and their probabilities
#'
nmarank = function( x
                  , nsim=10000
                  , small.values="bad"
                  , predicate = list(fn = "alwaysTrue", args = list())
                  )
{
  if (!meta:::is.installed.package("mvtnorm", stop = FALSE)) {
    warning(paste("Package 'mvtnorm' missing.",
                  "\n  ",
                  "Please use the following R command for installation:",
                  "\n  ",
                  "install.packages(\"mvtnorm\")",
                  sep = ""),
            call. = FALSE)
    return(invisible(NULL))
  }
  
  if (!meta:::is.installed.package("tidyverse", stop = FALSE)) {
    warning(paste("Package 'tidyverse' missing.",
                  "\n  ",
                  "Please use the following R command for installation:",
                  "\n  ",
                  "install.packages(\"tidyverse\")",
                  sep = ""),
            call. = FALSE)
    return(invisible(NULL))
  }
  
  meta:::chkclass(x, c("netmeta", "nmaEffects"))
  
  if(inherits(x,"netmeta")){
    if((x$comb.fixed==F) | (x$comb.fixed==T & x$comb.random==T)){
      nmaTE <- x$TE.random
      nmaCovs <- x$Cov.random
    }else{
      nmaTE <- x$TE.fixed
      nmaCovs <- x$Cov.fixed
    }
    effects <- prepareNMAEffects(nmaTE, nmaCovs)
    TEs <- effects$TE
    REs <- effects$RE
    Covs <- effects$Cov
  }else{
    TEs <- x$TE
    REs <- x$RE
    Covs <- x$Cov
  }
  
  leagueTableFromRelatives <- function(rels){
    lgtbl = matrix(0
                  ,nrow=nrow(TEs)
                  ,ncol=ncol(TEs)
                  ,dimnames = list( rownames(TEs)
                                  , colnames(TEs))
    )
    lgtbl[lower.tri(lgtbl)] <- rels
    lgtbl <- t(lgtbl)
    lgtbl[lower.tri(lgtbl)] <- -rels
    lgtbl
  }
  
  if(is.null(predicate$root)){
    predicate = makeNode(predicate)
  }
  
  rels <- mvtnorm:::rmvnorm(nsim, REs, Covs,checkSymmetry=F)
  
  hitsranks <- Reduce(function(acc, i){
    x <- rels[i,]
    leagueT <- leagueTableFromRelatives(x)
    if(selectionHolds(predicate, small.values, leagueT)){
      newhits <- acc$hits + 1
    }else{
      newhits = acc$hits
    }
    thisrank <- rankFromLeagueTable(small.values,leagueT) %>% 
      paste(collapse=", ")
    newranks = acc$ranks
    if(is.null(acc$ranks[thisrank])){ #for the first time
      newranks[thisrank] <- 1
    }else{
      if(is.na.data.frame(acc$ranks[thisrank])){
        newranks[thisrank] <- 1
      }else{
        newranks[thisrank] <- newranks[thisrank] + 1
      }
    }
    newacc <- list(hits=newhits, ranks=newranks)
    return(newacc)
  },1:nsim,list(hits=0, ranks=c()))
  
  hitsranks$ranks <- sort(hitsranks$ranks, decreasing = T)
  ranks <- tibble( Hierarchies = hitsranks$ranks %>% names()
                 , Probability = hitsranks$ranks / nsim
                 )
  res <- list(x=x
            ,probabilityOfSelection = hitsranks$hits / nsim
            ,hierarchies = ranks
  )
  return(res)
}

#' Checks if a selection on relative treatment effects is true
#'
#' Calculates the value of a predicate statement predicate tree given 
#' a league table
#'
#' @import data.tree
#'
#' @param node of a predicate tree containing the name of a function
#' @param small.values "good" or "bad" defines rank order
#' @param leagueTable the \code{TE} of a \code{nmaEffects} object produced by the 
#' \code{prepareNMAEffects} function
#'
#' @return True/False
#'
selectionHolds = function(node, small.values, leagueTable){
  if (!meta:::is.installed.package("data.tree", stop = FALSE)) {
    warning(paste("Package 'data.tree' missing.",
                  "\n  ",
                  "Please use the following R command for installation:",
                  "\n  ",
                  "install.packages(\"data.tree\")",
                  sep = ""),
            call. = FALSE)
    return(invisible(NULL))
  }
   if(node$operation == "function"){
     return(get(node$fn)(node$args, small.values, leagueTable))
   }else{
     if(node$fn=="AND"){
       return(
         all(sapply(node$children, 
                    function(x){
                      return(selectionHolds(x,small.values,leagueTable))
                    })
         )
       )
     }else{
       if(node$fn=="OR"){
         return(
           any(sapply(node$children, 
                      function(x){
                        return(selectionHolds(x,small.values, leagueTable))
                      }
                     )
              )
           )
       }
       if(node$fn=="XOR"){
         return(
           xor( selectionHolds(node$children[[1]], small.values,leagueTable)
              , selectionHolds(node$children[[2]], small.values,leagueTable))
           )
       }
       if(node$fn=="NOT"){
         return(
           ! ( selectionHolds(node$children[[1]], small.values,leagueTable))
           )
       }
     }
   }
}

#' Internall functions for constructing a predicate tree
#' 
#' 
makeID = function(op) {
  return(paste(op, as.character(floor(runif(1,0,1)*10000)), sep="_"))
}

makeNode = function(selectionList) {
  if(is.null(selectionList$root)){
  selectionList$id = makeID(as.character(selectionList$fn))
  selectionList$operation = "function"
  out = FromListExplicit(selectionList,nameName="id")
  }
  else{
    out = selectionList
    }
  return(out)
}

#' Combine selections with AND
#'
#' @param selections as list. Each selection comprises of a list with the
#' function and its arguments
#'
#' @return data.tree
#'
#' @examples
#' A = list(fn = "retainOrder", args = c("Placebo", "Salmeterol", "SFC"))
#' B = list(fn = "retainOrder", args = c("Placebo", "Salmeterol", "SFC"))
#' A %AND% B
#'
#' @export 
`%AND%` = function(A,B) {
  nl = list( id = makeID("AND")
             , fn ="AND"
             , arguments = {}
             , operation = "combinator"
             )
  nodeout = FromListExplicit(nl,nameName="id")
  nodeA = makeNode(A)
  nodeB = makeNode(B)
  nodeout$AddChildNode(nodeA)
  nodeout$AddChildNode(nodeB)
  return(nodeout)
}

`%OR%` = function(A,B) {
  nl = list( id = makeID("OR")
             , fn ="OR"
             , arguments = {}
             , operation = "combinator"
             )
  nodeout = FromListExplicit(nl,nameName="id")
  nodeA = makeNode(A)
  nodeB = makeNode(B)
  nodeout$AddChildNode(nodeA)
  nodeout$AddChildNode(nodeB)
  return(nodeout)
}

`%XOR%` = function(A,B) {
  nl = list( id = makeID("XOR")
           , fn ="XOR"
           , arguments = {}
           , operation = "combinator"
           )
  nodeout = FromListExplicit(nl,nameName="id")
  nodeA = makeNode(A)
  nodeB = makeNode(B)
  nodeout$AddChildNode(nodeA)
  nodeout$AddChildNode(nodeB)
  return(nodeout)
}

#' The NOT function for a selection statement
#' It simply reverses predicate 
#' 
#' @param A selection list
#' @examples
#' A = list(fn = "retainOrder", args = c("Placebo", "Salmeterol", "SFC"))
#' opposite(A)
#'  @export 
opposite = function(A) {
  nl = list( id = makeID("NOT")
           , fn ="NOT"
           , arguments = {}
           , operation = "reversor"
           )
  nodeout = FromListExplicit(nl,nameName="id")
  nodeA = makeNode(A)
  nodeout$AddChildNode(nodeA)
  return(nodeout)
}
