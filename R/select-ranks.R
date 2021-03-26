#' Probability of selected rankings
#'
#' Functions for quering treatment hierarchies probability
#'
#' @import data.tree
#'
#' @param rankPrecisions the output of \code{\link{precranking}}
#' @param predicateTree is the tree representing the criteria specified by
#' logical functions.
#'
#' @return probability of selected hierarchies
#'
#' @examples
#' data("Woods2010")
#' 
#' p1 <- pairwise(treatment, event = r, n = N,
#'  studlab = author, data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1)
#' net1
#' 
#' prec1=precranking(netmetaobject=net1, random=F, no_most_prob=NA, nsim=10000, small.values = "bad")
#'
#' A = list(fn = "retainOrder", args = c("Placebo", "SFC", "Salmeterol"))
#' p1 = probabilityOfSelection(prec1, A)
#' p1 
#' B = list(fn = "retainOrder", args = c("Placebo",  "Salmeterol", "SFC"))
#' p2 = probabilityOfSelection(prec1, B)
#' p2
#' p3 = probabilityOfSelection(prec1, (B %AND% A))
#' p3
#'
#' @export 
probabilityOfSelection = function(rankPrecisions, predicateTree){
  ranks = rankPrecisions$Output;
  if(is.null(predicateTree$root)){
    predicateTree = makeNode(predicateTree)
  }
  probs = Reduce(function(acc, i){
      holds = selectionHolds(predicateTree,ranks$Hierarchy[[i]])
      if(holds){
        out = acc+ranks$Probability[[i]]
      }else{
        out = acc
      }
      return(out)
    }, 1:nrow(ranks), 0)
  return (probs)
}

#' Probability of selected rankings
#'
#' Functions for quering treatment hierarchies probability
#'
#' @import data.tree
#'
#' @param rankPrecisions the output of \code{\link{precranking}}
#' @param predicateTree is the tree representing the criteria specified by
#' logical functions.
#'
#' @return probability of selected hierarchies
#'
#' @export 
selectionHolds = function(node, rankprobsrow){
   if(node$operation == "function"){
     return(get(node$fn)(node$args, rankprobsrow))
   }else{
     if(node$fn=="AND"){
       return(
         all(sapply(node$children, 
                    function(x){
                      return(selectionHolds(x, rankprobsrow))
                    })
         )
       )
     }else{
       if(node$fn=="OR"){
         return(
           any(sapply(node$children, 
                      function(x){
                        return(selectionHolds(x, rankprobsrow))
                      }
                     )
              )
           )
       }
       if(node$fn=="XOR"){
         return(
           xor( selectionHolds(node$children[[1]], rankprobsrow)
              , selectionHolds(node$children[[2]], rankprobsrow))
           )
       }
     }
   }
}

#' Ranks are equal
#'
#' Checks whether two list of treatments are the same
#'
#' @param ranklist the rank input
#' @param rankProbabilityRow the rank from the \code{\link{precranking}} row output
#'
#' @return True / False
isthesamerank = function(ranklist, rankProbabilityRow){
 out = all(rankProbabilityRow==ranklist)
 return(out)
}

#' Treatment in position
#'
#' Checks whether a treatment is in specified position
#'
#' @param treatpos a  list with the treatment and posistion
#' @param rankProbabilityRow the rank from the \code{\link{precranking}} row output
#'
#' @return True / False
#'
#' @export 
treatementInSpecificPosition = function(treatpos,rankProbabilityRow){
  treat = treatpos[[1]]
  position = treatpos[[2]]
  out = rankProbabilityRow[[position]] == treat
 return(out)
}

#' Treatments in specified order
#'
#' Checks whether the treatments specified as a vector are in the same order. 
#' Other treatments may be between them.
#'
#' @param treatments a vector of treatments
#' @param rankProbabilityRow the rank from the \code{\link{precranking}} row output
#'
#' @return True / False
#'
#' @examples
#'  retainOrder(list("Placebo", "SFC", "salmeterol"), precisionsRow)
#'
#' @export 
retainOrder = function(treatments,rankProbabilityRow){
  indexes = match(treatments,rankProbabilityRow)
  return(!is.unsorted(indexes))
}

#' Treatment is in better or equal position
#'
#' @param treatpos a  list with the treatment and posistion
#' @export 
betterEqual = function(treatpos,rankProbabilityRow){
  treat = treatpos[[1]]
  position = treatpos[[2]]
  out = match(treat,rankProbabilityRow)
  return(out <= position)
}

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
