#' Checks whether the effects matrix \code{TE} and the
#' variance-covariance matrix have consistent names it
#' 
#' @param TE A matrix with the network estimates. Row and column names
#'   should be present and correspond to the respective treatment
#'   label. See example
#' @param Cov variance-covariance matrix of the relative treatment
#'   effects row and column names should refer to the respective
#'   comparison. Comparisons should be formatted as
#'   \code{treat1:treat2}
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
    unite("name", "rowname", "name", sep = ":") %>%
    filter(name %in% comps)
  ##
  if ((!all((REs$name) == rownames(Cov))) | is_empty(REs$value)) {
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


#' Hierarchy of treatment rankings
#'
#' @details
#' This function gives the probabilities of treatment hierarchies of
#' network meta-analysis.
#'
#' @param TE.nma Either a \code{\link{netmeta}} object or a matrix
#'   with network estimates.
#' @param condition Defines the condition that a possible relative
#'   effects vector should comply to. See \code{select-ranks}.
#' @param text.condition Descriptive text for the condition.
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
#' @param \dots Additional arguments.
#' 
#' @return
#'
#' An object of class \code{"nmarank"} with corresponding
#' \code{print} function. The object is a list containing the
#' following components:
#'
#' \item{hierarchy}{...} 
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
#' @export
#' @export print.nmarank

print.nmarank <- function(x, text.condition = x$text.condition,
                          nrows = 10,
                          digits = 4, ...) {
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
