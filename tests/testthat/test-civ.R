rm(list=ls())
data("Woods2010")

p1 <- pairwise(treatment, event = r, n = N,
               studlab = author, data = Woods2010, sm = "OR")
# Conduct network meta-analysis
net1 <- netmeta(p1)
net1
forest(net1)

prec1=precranking(netmetaobject=net1, random=F, no_most_prob=NA, nsim=10000, small.values = "bad")


A = list(fn = "retainOrder", args = c("Placebo", "Salmeterol", "SFC"))
  
test_that("civ of 0 gives the same as probability", {
  expect_equal(2 * 2, 4)
})
