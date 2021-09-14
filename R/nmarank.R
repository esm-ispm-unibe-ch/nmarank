#' Check input data for \code{nmarank}
#'
#' @description
#' Checks whether the network meta-analysis treatment effects TE and their
#' variance-covariance matrix Cov have consistent names.
#' 
#' @param TE A matrix with the network meta-analysis relative treatment effects.
#'   Rows and columns should be labeled according to the treatment names as
#'   shown in the example.
#' @param Cov Variance-covariance matrix of the network meta-analysis relative
#' treatment effects. Row and column names should refer to the respective
#' treatment comparisons as shown in the example.
#' 
#' @return
#' An object of class \code{nmaEffects} with \code{TE} a tibble with
#' the relative effects and \code{Cov} the variance-covariance matrix
#' provided as argument.
#'
#' @examples
#' 
#' data("Woods2010")
#' p1 <- pairwise(treatment, event = r, n = N,
#'                studlab = author, data = Woods2010, sm = "OR")
#' # Conduct network meta-analysis
#' net1 <- netmeta(p1)
#' TE <- net1$TE.random
#' Cov <- net1$Cov.random
#' nmaEffects(TE, Cov)
#'
#' @importFrom tidyr pivot_longer unite
#' @importFrom stats runif
#' @importFrom rlang is_empty 
#' @importFrom rlang .data
#' 
#' @export 

nmaEffects <- function(TE, Cov) {
  
  if (!all(rownames(Cov) == colnames(Cov))) {
    warning(paste("Variance-Covariance matrix Cov should be symmetric",
                  "\n  ",
                  "Please also check row and column names are the same",
                  "\n  ",
                  sep = ""))
  }
  ##
  comps <- rownames(Cov)
  ##
  REs <- TE %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    pivot_longer(cols = -"rowname") %>%
    unite("compname", "rowname", "name", sep = ":") %>%
    filter(.data$compname %in% comps)
  ##
  if ((!all((REs$compname) == rownames(Cov))) | is_empty(REs$value)) {
    stop(paste("Relative Effects and Cov matrix do not match",
                  "\n  ",
                  "Please check treatment labels and dimensions",
                  "\n  ",
                  sep = ""))
  }
  ##
  res <- list(TE = TE, RE = REs$value, Cov = Cov)
  class(res) <- "nmaEffects"
  
  res
}


#' Probabilities of treatment hierarchies
#'
#' @description
#' Specifies the frequencies of hierarchies along with their estimated
#' probabilities and the probability that a specified criterion holds.
#'
#' @details
#' A simulation method is used to derive the relative frequency of all possible
#' hierarchies in a network of interventions. Users can also define the set of all
#' possible hierarchies that satisfy a specified criterion, for example that a
#' specific order among treatments is retained in the network and/or a treatment
#' is in a specific position, and the sum of their frequencies constitute the
#' certainty around the criterion. 
#'
#' @param TE.nma Either a \code{\link{netmeta}} object or a matrix
#'   with network estimates.
#' @param condition Defines the conditions that should be satisfied
#' by the treatments in the network. Multiple conditions can be combined with
#' special operators into any decision tree. See \code{condition}.
#' @param text.condition Optional descriptive text for the condition.
#' @param VCOV.nma Variance-covariance matrix for network estimates
#'   (only considered if argument \code{TE.nma} isn't a
#'   \code{\link{netmeta}} object).
#' @param pooled A character string indicating whether the hierarchy
#'   is calculated for the fixed effects (\code{"fixed"}) or random
#'   effects model (\code{"random"}). Can be abbreviated.
#' @param nsim Number of simulations.
#' @param small.values A character string specifying whether small
#'   treatment effects indicate a "good" or "bad" effect.
#' @param x A \code{\link{nmarank}} object.
#' @param nrows Number of hierarchies to print.
#' @param digits Minimal number of significant digits for proportions,
#'   see \code{print.default}.
#' @param \dots Additional arguments.
#' 
#' @return
#'
#' An object of class \code{"nmarank"} with corresponding
#' \code{print} function. The object is a list containing the
#' following components:
#'
#' \item{hierarchies}{A list of the most frequent hierarchies along with their
#' estimated probability of occurrence} 
#' \item{probabilityOfSelection}{combined probability of all hierarchies that
#' satisfy the defined condition}
#' \item{TE.nma, condition, VCOV.nma,}{As defined above.}
#' \item{pooled, nsim, small.values}{As defined above.}
#'
#' @import netmeta mvtnorm dplyr tibble data.tree
#' 
#' @examples
#' data("Woods2010", package = "netmeta")
#' p1 <- pairwise(treatment, event = r, n = N, studlab = author,
#'                data = Woods2010, sm = "OR")
#' net1 <- netmeta(p1, small.values = "bad")
#'
#' nmarank(net1, condition("always"), nsim = 100)
#'
#' criterionA <-
#'  condition("sameHierarchy",
#'            c("SFC", "Salmeterol", "Fluticasone", "Placebo"))
#' nmarank(net1, criterionA, nsim = 100)
#' 
#' @export


nmarank <- function(TE.nma, condition, text.condition = "",
                    VCOV.nma = NULL, pooled,
                    nsim = 10000, small.values) {
  
  
  if (inherits(TE.nma, "netmeta")) {
    if (!is.null(VCOV.nma))
      warning("Argument 'VCOV.nma' ignored for objects of type 'netmeta'.",
              call. = FALSE)
    ##
    if (missing(small.values))
      small.values <- TE.nma$small.values
    ##
    if (missing(pooled))
      if ((TE.nma$comb.fixed == FALSE) |
          (TE.nma$comb.fixed == TRUE & TE.nma$comb.random == TRUE)) {
        pooled <- "random"
        VCOV.nma <- TE.nma$Cov.random
        TE.nma <- TE.nma$TE.random
      }
      else {
        pooled <- "fixed"
        VCOV.nma <- TE.nma$Cov.fixed
        TE.nma <- TE.nma$TE.fixed
      }
  }
  else {
    if (is.null(VCOV.nma))
      warning("Argument 'VCOV.nma' must be provided as ",
              "'TE.nma' isn't a 'netmeta' object.",
              call. = FALSE)
    ##
    if (missing(small.values))
      small.values <- "bad"
    ##
    if (missing(pooled))
      pooled <- ""
  }
  ##
  effects <- nmaEffects(TE.nma, VCOV.nma)
  ##
  TEs <- effects$TE
  REs <- effects$RE
  Covs <- effects$Cov
  ##
  small.values <- setchar(small.values, c("bad", "good"))
  pooled <- setchar(pooled, c("fixed", "random", ""))
  
  
  leagueTableFromRelatives <- function(rels) {
    lgtbl <- matrix(0, nrow = nrow(TEs), ncol = ncol(TEs),
                    dimnames = list(rownames(TEs), colnames(TEs)))
    ##
    lgtbl[lower.tri(lgtbl)] <- rels
    lgtbl <- t(lgtbl)
    lgtbl[lower.tri(lgtbl)] <- -rels
    lgtbl
  }
  
  
  if (is.null(condition$root))
    condition <- makeNode(condition)
  
  
  rels <- mvtnorm::rmvnorm(nsim, REs, Covs, checkSymmetry = FALSE)
  
  
  hitsranks <-
    Reduce(function(acc, i) {
      x <- rels[i, ]
      leagueT <- leagueTableFromRelatives(x)
      if (selectionHolds(condition, small.values, leagueT))
        newhits <- acc$hits + 1
      else
        newhits <- acc$hits
      ##
      thisrank <- getRank(leagueT, small.values) %>% paste(collapse = ", ")
      newranks <- acc$ranks
      if (is.null(acc$ranks[thisrank]))
        newranks[thisrank] <- 1
      else {
        if (is.na.data.frame(acc$ranks[thisrank]))
          newranks[thisrank] <- 1
        else
          newranks[thisrank] <- newranks[thisrank] + 1
      }
      ##
      list(hits = newhits, ranks = newranks)
    },
    1:nsim, list(hits = 0, ranks = c()))
  ##
  hitsranks$ranks <- sort(hitsranks$ranks, decreasing = TRUE)
  ##
  ranks <- data.frame(Hierarchy = hitsranks$ranks %>% names(),
                      Probability = hitsranks$ranks / nsim,
                      row.names = seq_along(hitsranks$ranks))
  
  
  res <- list(hierarchies = ranks,
              probabilityOfSelection = hitsranks$hits / nsim,
              TE.nma = TE.nma, VCOV.nma = VCOV.nma,
              pooled = pooled, nsim = nsim, small.values = small.values,
              condition = condition, text.condition = text.condition)
  ##
  class(res) <- "nmarank"
  
  res
}


#' @rdname nmarank
#' @method print nmarank
#'
#' @importFrom meta gs
#' 
#' @export
#' @export print.nmarank

print.nmarank <- function(x, text.condition = x$text.condition,
                          nrows = 10,
                          digits = gs("digits.prop"), ...) {
  if (x$pooled == "random")
    text.pooled <- " (random effects model)"
  else if (x$pooled == "fixed")
    text.pooled <- " (fixed effects model)"
  else
    text.pooled <- ""
  ##
  cat(paste0("Treatment hierarchy", text.pooled, "\n\n"))

  if (x$condition$fn == "alwaysTRUE") {
    x$hierarchies$Probability %>% round(digits = digits)
    print(x$hierarchies[seq_len(min(nrow(x$hierarchies), nrows)), ], ...)
    if (nrows < nrow(x$hierarchies))
      cat("...\n")
  }
  else if (x$condition$fn == "sameHierarchy") {
    cat(paste0("Probability of hierarchy '",
               paste(x$condition$args[[1]], collapse = ", "),
               "': ",
               x$probabilityOfSelection %>% round(digits = digits),
               "\n"))
  }
  else if (x$condition$fn == "retainOrder") {
    cat(paste0("Probability that order '",
               paste(x$condition$args[[1]], collapse = ", "),
               "' is retained anywhere in the hierarchy: ",
               x$probabilityOfSelection %>% round(digits = digits),
               "\n"))
  }
  else if (x$condition$fn == "betterEqual") {
    cat(paste0("Probability that '",
               x$condition$args[[1]],
               "' is among the best ", x$condition$args[[2]],
               " options: ",
               x$probabilityOfSelection %>% round(digits = digits),
               "\n"))
  }
  else {
    cat(paste0("Probability", if (text.condition != "") " that ",
               text.condition,
               ": ",
               x$probabilityOfSelection %>% round(digits = digits),
               "\n"))
  }
  ##
  invisible(NULL)
}
