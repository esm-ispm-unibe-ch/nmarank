#' Define condition
#' 
#' @param fn Character string specifiying type of condition.
#' @param \dots Function arguments.
#'
#' @details
#' Argument \code{fn} can be any of the following character strings
#' which can be abbreviated: "alwaysTRUE", "betterEqual",
#' "sameHierarchy", "specificPosition", "retainOrder", "biggerCIV"
#'
#' TODO: add more details on various functions (especially, function
#' arguments)
#' 
#' @return A list.
#'
#' @export

condition <- function(fn, ...) {
  avails <- c("alwaysTRUE",
              "betterEqual", "sameHierarchy",
              "specificPosition", "retainOrder",
              "biggerCIV") 
  fn <- setchar(fn, avails)
  ##
  list(fn = fn, args = list(...))
}


#' Combine selections with AND
#'
#' @param selections as list. Each selection comprises of a list with the
#' function and its arguments
#'
#' @return data.tree
#'
#' @examples
#' A <- condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' B <- condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' A %AND% B
#'
#' @export

`%AND%` <- function(A, B) {
  nl <- list(id = makeID("AND"),
             fn = "AND",
             arguments = {},
             operation = "combinator"
             )
  nodeout <- FromListExplicit(nl,nameName="id")
  nodeA <- makeNode(A)
  nodeB <- makeNode(B)
  nodeout$AddChildNode(nodeA)
  nodeout$AddChildNode(nodeB)
  ##
  nodeout
}


#' Combine selections with OR
#' 
#' @export 

`%OR%` <- function(A, B) {
  nl <- list(id = makeID("OR"),
             fn ="OR",
             arguments = {},
             operation = "combinator")
  ##
  nodeout <- FromListExplicit(nl, nameName = "id")
  nodeA <- makeNode(A)
  nodeB <- makeNode(B)
  nodeout$AddChildNode(nodeA)
  nodeout$AddChildNode(nodeB)
  ##
  nodeout
}


#' Combine selections with XOR
#' 
#' @export 

`%XOR%` <- function(A, B) {
  nl <- list(id = makeID("XOR"),
             fn ="XOR",
             arguments = {},
             operation = "combinator")
  ##
  nodeout <- FromListExplicit(nl, nameName = "id")
  nodeA <- makeNode(A)
  nodeB <- makeNode(B)
  nodeout$AddChildNode(nodeA)
  nodeout$AddChildNode(nodeB)
  ##
  nodeout
}


#' The NOT function for a selection statement It simply reverses
#' condition
#' 
#' @param A selection list
#' 
#' @examples
#' A = condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' opposite(A)
#' 
#' @export

opposite <- function(A) {
  nl <- list(id = makeID("NOT"),
            fn ="NOT",
            arguments = {},
            operation = "reversor"
            )
  nodeout = FromListExplicit(nl,nameName="id")
  nodeA = makeNode(A)
  nodeout$AddChildNode(nodeA)
  ##
  nodeout
}
