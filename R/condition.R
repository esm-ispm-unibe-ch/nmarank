#' Define which hierarchies to select
#' 
#' @description
#' Defines a condition that is of interest to be satisfied involving a
#' set of treatments in the network.
#' 
#' @param fn Character string specifiying type of condition.
#' @param \dots Function arguments.
#'
#' @details
#' The following types of conditions are available.
#'
#' The condition \code{fn = "sameHierarchy"} checks whether a specific
#' hierarchy occurs. One additional unnamed argument has to be
#' provided in '\dots': a vector with a permutation of all treatment
#' names in the network.
#'
#' The condition \code{fn = "specificPosition"} checks whether a
#' treatment ranks in a specific position. Two additional unnamed
#' arguments have to be provided in '\dots': (1) name of the treatment
#' of interest and (2) a single numeric specifying the rank position.
#'
#' The condition \code{fn = "betterEqual"} checks whether a treatment
#' has a position better or equal to a specific rank. Two additional
#' unnamed arguments have to be provided in '\dots': (1) name of the
#' treatment of interest and (2) a single numeric specifying the rank
#' position.
#'
#' The condition \code{fn = "retainOrder"} checks whether a specific
#' order of two or more treatments is retained anywhere in the
#' hierarchy. One additional unnamed argument has to be provided in
#' '\dots': a vector with two or more treatment names providing the
#' order of treatments.
#'
#' The condition \code{fn = "biggerCIV"} checks whether the effect of
#' a treatment is bigger than that of a second treatment by more than
#' a given clinically important value (CIV) on an additive scale
#' (e.g. log odds ratio, log risk ratio, mean difference). Three
#' additional unnamed arguments have to be provided in '\dots': (1)
#' name of the first treatment, (2) name of the second treatment and
#' (3) a numerical value for the CIV. Note that the actual value of
#' the relative effect is considered independently of whether
#' \code{small.values} is \code{"desirable"} or \code{"undesirable"}.
#' 
#' \bold{Composition of conditions for more complex queries:}
#' 
#' Conditions can be combined to express more complex decision
#' trees. This can be done by using the special operators \%AND\%,
#' \%OR\%, \%XOR\% and the \code{opposite} function. The combination
#' should be defined as a binary tree with the use of parentheses. If
#' A, B, C and D are conditions, we can for example combine them into
#' a complex condition E:
#'
#' \code{E = A \%AND\% (B \%OR\% (opposite(C) \%XOR\% D))}
#' 
#' @return
#' A list with the defined function and its arguments.
#'
#' @examples
#' data("Woods2010", package = "netmeta")
#' p1 <- pairwise(treatment, event = r, n = N, studlab = author,
#'   data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "good")
#'
#' # criterionA if all treatments are in the exact defined order
#' criterionA <-
#' condition("sameHierarchy",
#'   c("SFC", "Salmeterol", "Fluticasone", "Placebo"))
#'
#' # criterionB respects the relative order of two or more treatments
#' criterionB <-
#'   condition("retainOrder",
#'     c("SFC", "Fluticasone", "Placebo"))
#'
#' # Below we define the condition that SFC and Salmeterol are in the
#' # first two positions.
#'
#' # We first define conditions that each one of them is in position 1
#' # or 2 
#' criterionC1 <- condition("betterEqual", "SFC", 2)
#' criterionC2 <- condition("betterEqual", "Salmeterol", 2)
#' # We then combine them with operator %AND%
#' criterionC <- criterionC1 %AND% criterionC2
#' 
#' # Next we can feed the condition into nmarank to get the
#' # probability of the selection
#' nmarank(net1, criterionC,
#'   text.condition =
#'     "SFC and Salmeterol are the two best options", nsim = 100)
#'
#' # We can further combine criteria
#' criterionD <- criterionA %AND% (criterionB %OR% opposite(criterionC))
#'
#' @seealso \code{\link{nmarank}}
#' 
#' @export

condition <- function(fn, ...) {
  available.conditions <-
    c("sameHierarchy", "retainOrder",
      "specificPosition", "betterEqual",
      "biggerCIV")
  ##
  if (missing(fn))
    fn <- "not.any.function"
  ##
  fn2 <- setchar(fn, "alwaysTRUE", stop.at.error = FALSE)
  if (length(fn2) == 0)
    fn <- setchar(fn, available.conditions)
  else
    fn <- fn2
  ##
  if (fn %in% c("sameHierarchy", "retainOrder")) {
    if (length(list(...)) < 1)
      stop("One additional argument must be provided ",
           "for condition \"", fn, "\".",
           call. = FALSE)
    else {
      if (length(list(...)) > 1)
        warning("Only first argument provided in '...' considered ",
                "for condition \"", fn, "\".")
      ##
      if (!is.vector(list(...)[[1]]))
        stop("Argument provided in '...' must be a vector with ",
             if (fn == "sameHierarchy") "all ", "treatment names ",
             "for condition \"", fn, "\".",
             call. = FALSE)
    }
  }
  else if (fn %in% c("specificPosition", "betterEqual")) {
    if (length(list(...)) < 2)
      stop("Two additional arguments must be provided ",
           "for condition \"", fn, "\".",
           call. = FALSE)
    else {
      if (length(list(...)) > 2)
        warning("Only first and second argument ",
                "provided in '...' considered ",
                "for condition \"", fn, "\".")
      ##
      if (length(list(...)[[1]]) != 1)
        stop("First argument provided in '...' ",
             "must be a single treatment name ",
              "for condition \"", fn, "\".",
            call. = FALSE)
      ##
      chknumeric(list(...)[[2]], min = 1, single = TRUE,
                 text = paste0("Second argument in '...' ",
                              "must be a single numeric ",
                              "for condition \"", fn, "\"."))
    }
  }
  else if (fn == "biggerCIV") {
    if (length(list(...)) < 3)
      stop("Three additional arguments must be provided ",
           "for condition \"", fn, "\".",
           call. = FALSE)
    else {
      if (length(list(...)) > 3)
        warning("Only first three arguments ",
                "provided in '...' considered ",
                "for condition \"", fn, "\".")
      ##
      if (length(list(...)[[1]]) != 1)
        stop("First argument provided in '...' ",
             "must be a single treatment name ",
             "for condition \"", fn, "\".",
             call. = FALSE)
      ##
      if (length(list(...)[[2]]) != 1)
        stop("Second argument provided in '...' ",
             "must be a single treatment name ",
             "for condition \"", fn, "\".",
             call. = FALSE)
      ##
      chknumeric(list(...)[[3]], single = TRUE,
                 text = paste0("Third argument in '...' ",
                              "must be a single numeric ",
                              "for condition \"", fn, "\"."))
    }
  }
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
#'   data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "good")
#'
#' A <- condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' B <- condition("betterEqual", "Fluticasone", 2)
#'
#' nmarank(net1, A %AND% B, nsim = 500)
#'
#' @seealso \code{\link{condition}}, \code{\link{nmarank}}
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
#'   data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "good")
#'
#' A <- condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' B <- condition("betterEqual", "Fluticasone", 2)
#'
#' nmarank(net1, A %OR% B, nsim = 500)
#'
#' @seealso \code{\link{condition}}, \code{\link{nmarank}}
#' 
#' @export 

`%OR%` <- function(cond1, cond2) {
  nl <- list(id = makeID("OR"),
             fn = "OR",
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
#'   data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "good")
#'
#' A <- condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' B <- condition("betterEqual", "Fluticasone", 2)
#'
#' nmarank(net1, A %XOR% B, nsim = 3000)
#'
#' @seealso \code{\link{condition}}, \code{\link{nmarank}}
#' 
#' @export 

`%XOR%` <- function(cond1, cond2) {
  nl <- list(id = makeID("XOR"),
             fn = "XOR",
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
#'   data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "good")
#' 
#' A = condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
#' nmarank(net1, opposite(A), text.condition = "NOT order P-S-S", nsim = 5000)
#'
#' @seealso \code{\link{condition}}, \code{\link{nmarank}}
#' 
#' @export

opposite <- function(cond) {
  nl <- list(id = makeID("NOT"),
             fn = "NOT",
             arguments = {},
             operation = "reversor")
  ##
  nodeout <- FromListExplicit(nl, nameName = "id")
  node <- makeNode(cond)
  nodeout$AddChildNode(node)
  ##
  nodeout
}
