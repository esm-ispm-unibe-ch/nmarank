##
## (1) Make R packages available
##
library(devtools)
library(roxygen2)


##
## (2) Create documentation file(s)
##
document("../nmarank")


##
## (3) Build R package and PDF file with help pages
##
build("../nmarank")
build_manual("../nmarank")


##
## (4) Install R package
##
install("../nmarank")


##
## (5) Check R package
##
check(env_vars = c(NOT_CRAN = "FALSE","_R_CHECK_CRAN_INCOMING_"=TRUE))


##
## (6) Check R package (with dontrun examples)
##
check("../nmarank", run_dont_test = TRUE)
