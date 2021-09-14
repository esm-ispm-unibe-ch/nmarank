#' Define which hierarchies to select
#' 
#' @description
#' Defines a condition that is of interest to be satisfied involving a set of
#' treatments in the network. 
#' 
#' @param fn Character string specifiying type of condition.
#' @param \dots Function arguments.
#'
#' @details
#' Each implemented condition function needs a specific set of arguments. 
#' Available conditions and their accompanying arguments are:
#' \itemize{
#'  \item{"sameHierarchy": }{Testing that a specified hierarchy occurs with arguments a vector of length equal to the number of treatments in the network }
#' \item{"specificPosition": }{A treatment ranks in a specific position with
#' arguments a list including the name of the treatment and its position}
#' \item{"retainOrder": }{The order of two or more treatments is retained in the
#' hierarchy with arguments a vector of length smaller or equal to the number of
#' treatments in the network with the specified order to be retained}
#' \item{"betterEqual": }{A treatment has a position better or equal to a
#' specific rank with arguments a list including the name of the treatment and
#' the set position} 
#' \item{"biggerCIV": }{The effect of the first treatment is bigger than that of
#' the second by more than a given clinically important value with arguments: a
#' vector of length 3 with the first and second treatment and the clinically
#' important value in an additive scale (e.g. log odds ratio, log risk ratio,
#' mean difference). Note that the actual value of the relative effect is considered
#' independently of whether \code{small.values} is \code{"good"} or \code{"bad"} }
#' \item{"alwaysTRUE": }{All hierarchies are included. No arguments required}
#'}
#' 
#' @return A list with the defined function and its arguments.
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
#' B <- condition("betterEqual", "Fluticasone", 2)
#'
#' nmarank(net1, A %AND% B, nsim=5000)
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
#' B <- condition("betterEqual", "Fluticasone", 2)
#'
#' nmarank(net1, A %OR% B, nsim=5000)
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
#' B <- condition("betterEqual", "Fluticasone", 2)
#'
#' nmarank(net1, A %XOR% B, nsim=3000)
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
#' nmarank(net1, opposite(A), text.condition = "NOT order P-S-S", nsim=5000)
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
