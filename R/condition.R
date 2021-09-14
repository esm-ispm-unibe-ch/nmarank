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
#' @examples
#' data("Woods2010", package = "netmeta")
#' p1 <- pairwise(treatment, event = r, n = N, studlab = author,
#'                data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "bad")
#'
#' criterionA <-
#'  condition("sameHierarchy",
#'            c("SFC", "Salmeterol", "Fluticasone", "Placebo"))
#' nmarank(net1, criterionA, nsim = 100)
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
#' @param cond1 First \code{\link{condition}}.
#' @param cond2 Second \code{\link{condition}}.
#'
#' @return Object of class 'data.tree'.
#'
#' @examples
#' data("Woods2010", package = "netmeta")
#' p1 <- pairwise(treatment, event = r, n = N, studlab = author,
#'                data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "bad")
#'
#' A <- condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' B <- condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#'
#' nmarank(net1, A %AND% B)
#'
#' @export

`%AND%` <- function(cond1, cond2) {
  nl <- list(id = makeID("AND"),
             fn = "AND",
             arguments = {},
             operation = "combinator")
  ##
  nodeout <- FromListExplicit(nl, nameName = "id")
  node1 <- makeNode(cond1)
  node2 <- makeNode(cond2)
  nodeout$AddChildNode(node1)
  nodeout$AddChildNode(node2)
  ##
  nodeout
}


#' Combine selections with OR
#'
#' @param cond1 First \code{\link{condition}}.
#' @param cond2 Second \code{\link{condition}}.
#'
#' @return Object of class 'data.tree'.
#'
#' @examples
#' data("Woods2010", package = "netmeta")
#' p1 <- pairwise(treatment, event = r, n = N, studlab = author,
#'                data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "bad")
#'
#' A <- condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' B <- condition("retainOrder", c("Placebo", "SFC", "Salmeterol"))
#'
#' nmarank(net1, A %OR% B)
#' 
#' @export 

`%OR%` <- function(cond1, cond2) {
  nl <- list(id = makeID("OR"),
             fn ="OR",
             arguments = {},
             operation = "combinator")
  ##
  nodeout <- FromListExplicit(nl, nameName = "id")
  node1 <- makeNode(cond1)
  node2 <- makeNode(cond2)
  nodeout$AddChildNode(node1)
  nodeout$AddChildNode(node2)
  ##
  nodeout
}


#' Combine selections with XOR
#'
#' @param cond1 First \code{\link{condition}}.
#' @param cond2 Second \code{\link{condition}}.
#'
#' @return Object of class 'data.tree'.
#'
#' @examples
#' data("Woods2010", package = "netmeta")
#' p1 <- pairwise(treatment, event = r, n = N, studlab = author,
#'                data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "bad")
#'
#' A <- condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' B <- condition("retainOrder", c("Placebo", "SFC", "Salmeterol"))
#'
#' nmarank(net1, A %XOR% B)
#' 
#' @export 

`%XOR%` <- function(cond1, cond2) {
  nl <- list(id = makeID("XOR"),
             fn ="XOR",
             arguments = {},
             operation = "combinator")
  ##
  nodeout <- FromListExplicit(nl, nameName = "id")
  node1 <- makeNode(cond1)
  node2 <- makeNode(cond2)
  nodeout$AddChildNode(node1)
  nodeout$AddChildNode(node2)
  ##
  nodeout
}


#' The NOT function for a selection statement It simply reverses
#' condition
#' 
#' @param cond \code{\link{condition}}.
#'
#' @return Object of class 'data.tree'.
#'
#' @examples
#' data("Woods2010", package = "netmeta")
#' p1 <- pairwise(treatment, event = r, n = N, studlab = author,
#'                data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "bad")
#' 
#' A = condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' nmarank(net1, opposite(A), text.condition = "NOT order P-S-S")
#' 
#' @export

opposite <- function(cond) {
  nl <- list(id = makeID("NOT"),
             fn ="NOT",
             arguments = {},
             operation = "reversor")
  ##
  nodeout = FromListExplicit(nl, nameName="id")
  node = makeNode(cond)
  nodeout$AddChildNode(node)
  ##
  nodeout
}
