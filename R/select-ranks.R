getRank <- function(leagueTable, small.values) {
  res <- sort(leagueTable[1, ],
              decreasing = small.values == "desirable") %>% names() 
  ##
  res
}


alwaysTRUE <- function(args, small.values, leagueTable)
  TRUE


betterEqual <- function(treatpos, small.values, leagueTable){
  treat <- treatpos[[1]]
  position <- treatpos[[2]]
  rank <- getRank(leagueTable, small.values)
  ##
  match(treat, rank) <= position
}


sameHierarchy <- function(ranklist, small.values, leagueTable)
  all(getRank(leagueTable, small.values) == ranklist[[1]])


specificPosition <- function(treatpos, small.values, leagueTable) {
  rank <- getRank(leagueTable, small.values)
  treat <- treatpos[[1]]
  position = treatpos[[2]]
  ##
  rank[[position]] == treat
}


retainOrder <- function(treatments, small.values, leagueTable){
  !is.unsorted(match(unlist(treatments), getRank(leagueTable, small.values)))
}


biggerCIV <- function(args, small.values, leagueTable)
  leagueTable[args[[2]], args[[1]]] > args[[3]]
