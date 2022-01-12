data("Woods2010")

#Old relative effects multivariate sampling
nmarank2 <- function(TE.nma, condition=NULL, text.condition = "",
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
  if (is.null(condition)){
    condition <- condition("alwaysTRUE")
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
  ##
  trts <- rownames(TEs)
  ##
  if (condition$fn == "sameHierarchy") {
    condition$args[[1]] <-
      setseq(condition$args[[1]], trts,
             error.text =
               paste0("first argument of condition \"",
                      condition$fn, "\""))
  }
  else if (condition$fn == "retainOrder")
    condition$args[[1]] <-
      setref(condition$args[[1]], trts, length = 0,
             error.text =
               paste0("first argument of condition \"",
                      condition$fn, "\""))
  else if (condition$fn %in%
           c("specificPosition", "betterEqual", "biggerCIV")) {
    condition$args[[1]] <-
      setref(condition$args[[1]], trts, length = 1,
             error.text =
               paste0("first argument of condition \"",
                      condition$fn, "\""))
    ##
    if (condition$fn %in% c("specificPosition", "betterEqual"))
      chknumeric(condition$args[[2]], min = 1, max = length(trts),
                 text = paste0("Second argument of condition \"",
                               condition$fn, "\" ",
                               "must be a single numeric ",
                               "between 1 and ", length(trts),
                               "."))
    ##
    if (condition$fn == "biggerCIV") {
      condition$args[[2]] <-
        setref(condition$args[[2]], trts, length = 1,
               error.text =
                 paste0("second argument of condition \"",
                        condition$fn, "\""))
    }
  }
  
  
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

#' @export

p1 <- pairwise(treatment, event = r, n = N,
               studlab = author, data = Woods2010, sm = "OR")
# Conduct network meta-analysis
net1 <- netmeta(p1, small.values = "good")

effs <- nmarank:::nmaEffects(net1$TE.random, net1$Cov.random)

test_that("arm gives correct ranks", {
  A = condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
  rank1 = nmarank(net1, A, nsim=3000,small.values="bad")
  rank2 = nmarank2(net1, A, nsim=3000,small.values="bad")
  rank3 = nmarank(net1, A, nsim=3000,small.values="good")
  rank4 = nmarank2(net1, A, nsim=3000,small.values="good")
  p1 <- rank1$hierarchies[1:3,]
  p2 <- rank2$hierarchies[1:3,]
  p3 <- rank3$hierarchies[1:3,]
  p4 <- rank4$hierarchies[1:3,]
  expect_true(all(p1$Hierarchy==p2$Hierarchy) &
               all(p3$Hierarchy==p4$Hierarchy))
})

test_that("arm gives correct probs", {
  A = condition("biggerCIV", "SFC", "Fluticasone", 0.1)
  rank1 = nmarank(net1, A, nsim=10000)
  rank2 = nmarank2(net1, A, nsim=10000)
  p1 <- rank1$probabilityOfSelection
  p2 <- rank2$probabilityOfSelection
  expect_lte(abs(p1-p2),0.02)
})

