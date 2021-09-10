
#' returnAlways true for default predicate
#' @export
alwaysTrue = function(args, small.values, leagueTable){
  return(T)
}

#' Treatment difference bigger than a given Clinically Important Value
#'
#' Checks whether the effect of the first treatment is bigger than the second by
#' more than the given CIV.
#' The function does not take into account the \code{small.values=bad/good} 
#' argument. 
#'
#' @param civ (Clinicaly Important Value) is an optional argument 
#' of type list(treatBig, treatSmall, threshold) where
#' hierarchies fulfilling condition \code{treatBig - treatSmall > civ}  will
#' be recorded. This is a prerequisite for using the \code{isbiggerCIV}
#' helper function in the \code{propabilityOfSelection}
#' @param treatBigger the name of the treatment with the bigger absolute effect
#' irrespective of whether it is considered bad or good
#'
#' @return True / False
#' 
#' @param civ (Clinicaly Important Value) is an optional argument 
#' of type list(treatBig, treatSmall, threshold) where
#' hierarchies fulfilling condition \code{treatBig - treatSmall > civ}  will
#' be recorded. 
#' 
isbiggerCIV = function(args, small.values, leagueTable){
  thecheck <- leagueTable[args[[2]],args[[1]]] > args[[3]]
  return(thecheck)
}

#' Treatment hierarchy from smaller to bigger from the league table
#' 
#' @param leagueTable is a table like the one exported by the 
#' \code{prepareNMAEffects} {$TE} object
#' @return a vector of treatment names
#' 
#' @export
rankFromLeagueTable = function(small.values, leagueTable){
  if(small.values == "good"){
    res <- sort(leagueTable[1,], decreasing = T) %>% names() 
  }else{
    res <- sort(leagueTable[1,]) %>% names() 
  }
  return(res)
}

#' Hierarchies are equal
#'
#' Checks whether two list of treatments are the same
#'
#' @param ranklist the hierarchy input
#' @param rankProbabilityRow the hierarchy from the \code{\link{nmarank}} row 
#' output
#'
#' @return True / False
isthesamehierarchy = function(ranklist, small.values, leagueTable){
 rank <- rankFromLeagueTable(small.values, leagueTable)
 out = all(rank==ranklist)
 return(out)
}

#' Treatment in position
#'
#' Checks whether a treatment is in specified position
#'
#' @param treatpos a  list with the treatment and posistion
#' @param rankProbabilityRow the rank from the \code{\link{nmarank}} row output
#'
#' @return True / False
#'
#' @export 
treatementInSpecificPosition = function(treatpos, small.values, leagueTable){
  rank <- rankFromLeagueTable(small.values, leagueTable)
  treat = treatpos[[1]]
  position = treatpos[[2]]
  out = rank[[position]] == treat
 return(out)
}

#' Treatments in specified order
#'
#' Checks whether the treatments specified as a vector are in the same order. 
#' Other treatments may be between them.
#'
#' @param treatments a vector of treatments
#' @param rankProbabilityRow the rank from the \code{\link{nmarank}} row output
#'
#' @return True / False
#'
#' @examples
#'  retainOrder(list("Placebo", "SFC", "salmeterol"), precisionsRow)
#'
#' @export 
retainOrder = function(treatments, small.values, leagueTable){
  rank <- rankFromLeagueTable(small.values, leagueTable)
  indexes = match(treatments, rank)
  return(!is.unsorted(indexes))
}

#' Treatment is in better or equal position
#'
#' @param treatpos a  list with the treatment and posistion
#' @export 
betterEqual = function(treatpos, small.values, leagueTable){
  treat = treatpos[[1]]
  position = treatpos[[2]]
  rank <- rankFromLeagueTable(small.values, leagueTable)
  out = match(treat, rank)
  return(out <= position)
}
