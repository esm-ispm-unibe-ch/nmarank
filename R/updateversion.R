updateversion <- function(x, verbose = FALSE) {
  
  if (is.null(x$version)) {
    major <- 0
    minor <- 0
  }
  else {
    version <- as.numeric(unlist(strsplit(x$version, "-")))
    major <- version[1]
    minor <- version[2]
  }
  
  
  ##
  ##  Update netmeta object
  ##
  if (inherits(x, "netmeta")) {
    ##
    update.1.3.0 <- update_needed(x$version, 1, 3, verbose)
    update.2.0.0 <- update_needed(x$version, 2, 0, verbose)
    update.2.5.0 <- update_needed(x$version, 2, 5, verbose)
    update.2.8.0 <- update_needed(x$version, 2, 8, verbose)
    ##
    if (update.1.3.0) {
      x$statistic.fixed <- x$zval.fixed
      x$statistic.random <- x$zval.random
      x$statistic.direct.fixed <- x$zval.direct.fixed
      x$statistic.direct.random <- x$zval.direct.random
      x$statistic.indirect.fixed <- x$zval.indirect.fixed
      x$statistic.indirect.random <- x$zval.indirect.random
      x$statistic.nma.fixed <- x$zval.nma.fixed
      x$statistic.nma.random <- x$zval.nma.random
      ##
      x$zval.fixed <- x$zval.random <-
        x$zval.nma.fixed <- x$zval.nma.random <-
          x$zval.direct.fixed <- x$zval.direct.random <-
            x$zval.indirect.fixed <- x$zval.indirect.random <- NULL
      ##
      if (any(x$narms > 2)) {
        tdata1 <- data.frame(studlab = x$studlab,
                             .order = seq(along = x$studlab))
        tdata2 <- data.frame(studlab = as.character(x$studies),
                             narms = x$narms)
        ##
        tdata12 <- merge(tdata1, tdata2,
                         by = "studlab", all.x = TRUE, all.y = FALSE,
                         sort = FALSE)
        tdata12 <- tdata12[order(tdata12$.order), ]
        ##
        x$n.arms <- tdata12$narms
        x$multiarm <- tdata12$narms > 2
      }
      else {
        x$n.arms <- rep(2, length(x$studlab))
        x$multiarm <- rep(FALSE, length(x$studlab))
      }
    }
    ##
    if (update.2.0.0) {
      x$fixed <- x$comb.fixed
      x$random <- x$comb.random
      x$level.ma <- x$level.comb
      ##
      x$comb.fixed <- x$comb.random <- x$level.comb <- NULL
    }
    ##
    if (update.2.5.0) {
      x$common <- x$fixed
      ##
      x$seTE.adj.common <- x$seTE.adj.fixed
      ##
      if (!inherits(x, "netmetabin")) {
        x$TE.nma.common <- x$TE.nma.fixed
        x$seTE.nma.common <- x$seTE.nma.fixed
        x$lower.nma.common <- x$lower.nma.fixed
        x$upper.nma.common <- x$upper.nma.fixed
        x$statistic.nma.common <- x$statistic.nma.fixed
        x$pval.nma.common <- x$pval.nma.fixed
        x$leverage.common <- x$leverage.fixed
        x$w.common <- x$w.fixed
        x$Q.common <- x$Q.fixed
      }
      ##
      x$TE.common <- x$TE.fixed
      x$seTE.common <- x$seTE.fixed
      x$lower.common <- x$lower.fixed
      x$upper.common <- x$upper.fixed
      x$statistic.common <- x$statistic.fixed
      x$pval.common <- x$pval.fixed
      ##
      x$prop.direct.common <- x$prop.direct.fixed
      ##
      x$TE.direct.common <- x$TE.direct.fixed
      x$seTE.direct.common <- x$seTE.direct.fixed
      x$lower.direct.common <- x$lower.direct.fixed
      x$upper.direct.common <- x$upper.direct.fixed
      x$statistic.direct.common <- x$statistic.direct.fixed
      x$pval.direct.common <- x$pval.direct.fixed
      ##
      x$TE.indirect.common <- x$TE.indirect.fixed
      x$seTE.indirect.common <- x$seTE.indirect.fixed
      x$lower.indirect.common <- x$lower.indirect.fixed
      x$upper.indirect.common <- x$upper.indirect.fixed
      x$statistic.indirect.common <- x$statistic.indirect.fixed
      x$pval.indirect.common <- x$pval.indirect.fixed
      ##
      if (!inherits(x, "netmetabin")) {
        x$L.matrix.common <- x$L.matrix.fixed
        x$Lplus.matrix.common <- x$Lplus.matrix.fixed
        x$H.matrix.common <- x$H.matrix.fixed
      }
      x$P.common <- x$P.fixed
      x$Cov.common <- x$Cov.fixed               
    }
    ##
    if (update.2.8.0)
      x$small.values <- setsv(x$small.values)
    ##
    return(x)
  }
}


update_needed <- function(version, major = 0, minor = 0,
                          verbose = FALSE) {
  if (is.null(version)) {
    version <- 0.1
    major.cur <- 0
    minor.cur <- 1
  }
  else {
    version <- unlist(strsplit(version, "-")[1])
    major.cur <-
      as.numeric(unlist(strsplit(version, ".", fixed = TRUE))[1])
    minor.cur <-
      as.numeric(unlist(strsplit(version, ".", fixed = TRUE))[2])
  }
  ##
  res <-
    ifelse(major.cur < major,
           TRUE, ifelse(major.cur > major,
                        FALSE, minor.cur < minor))
  if (res & verbose)
    message(paste0("Update to netmeta, version ", major, ".", minor))
  ##
  res
}
