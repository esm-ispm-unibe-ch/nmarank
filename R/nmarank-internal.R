#' @importFrom tidyr pivot_longer unite
#' @importFrom stats runif
#' @importFrom rlang is_empty 
#' @importFrom rlang .data
#' 
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
makeID <- function(op)
  paste(op, as.character(floor(runif(1, 0, 1) * 10000)), sep = "_")


makeNode <- function(selectionList) {
  if (is.null(selectionList$root)) {
    selectionList$id <- makeID(as.character(selectionList$fn))
    selectionList$operation <- "function"
    out <- FromListExplicit(selectionList, nameName = "id")
  }
  else
    out <- selectionList
  
  out
}


setchar <- function(x, val, text, list = FALSE, name = NULL,
                    stop.at.error = TRUE) {
  if (is.null(name))
    name <- deparse(substitute(x))
  nval <- length(val)
  ##
  if (is.numeric(x)) {
    numeric.x <- TRUE
    idx <- x
    idx[idx < 1] <- NA
    idx[idx >= nval + 1] <- NA
  }
  else {
    numeric.x <- FALSE
    ##
    if (length(unique(tolower(x))) != length(unique(x)) |
        length(unique(tolower(val))) != length(unique(val)))
      idx <- charmatch(x, val, nomatch = NA)
    else
      idx <- charmatch(tolower(x), tolower(val), nomatch = NA)
  }
  ##
  if (anyNA(idx) || any(idx == 0)) {
    if (list)
      first <- "List element '"
    else
      first <- "Argument '"
    ##
    if (missing(text)) {
      if (numeric.x) {
        if (nval == 1)
          vlist <- "1"
        else if (nval == 2)
          vlist <- "1 or 2"
        else
          vlist <- paste("between 1 and", nval)
      }
      else {
        if (nval == 1)
          vlist <- paste0('"', val, '"')
        else if (nval == 2)
          vlist <- paste0('"', val, '"', collapse = " or ")
        else
          vlist <- paste0(paste0('"', val[-nval], '"', collapse = ", "),
                          ', or ', '"', val[nval], '"')
      }
      ##
      if (stop.at.error)
        stop(first, name, "' must be ", vlist, ".", call. = FALSE)
      else
        return(NULL)
    }
    else {
      if (stop.at.error)
        stop(first, name, "' ", text, ".", call. = FALSE)
      else
        return(NULL)
    }
  }
  ##
  val[idx]
}


selectionHolds <- function(node, small.values, leagueTable) {
  
  if (node$operation == "function")
    return(get(node$fn)(node$args, small.values, leagueTable))
  ##
  if (node$fn == "AND")
    return(
      all(sapply(
        node$children, 
        function(x)
          return(selectionHolds(x, small.values, leagueTable))
      )))
  ##
  if (node$fn == "OR")
    return(
      any(sapply(
        node$children, 
        function(x)
          return(selectionHolds(x,small.values, leagueTable))
      )))
  ##
  if (node$fn == "XOR")
    return(
      xor(selectionHolds(node$children[[1]], small.values, leagueTable),
          selectionHolds(node$children[[2]], small.values, leagueTable)))
  ##
  if (node$fn == "NOT")
    return(
      !(selectionHolds(node$children[[1]], small.values,leagueTable))
    )

  invisible(NULL)
}


#' @importFrom stats runif
