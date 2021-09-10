rm(list=ls())
data("Woods2010")

p1 <- pairwise(treatment, event = r, n = N,
               studlab = author, data = Woods2010, sm = "OR")
# Conduct network meta-analysis
net1 <- netmeta(p1, comb.fixed=T)

effs <- prepareNMAEffects(net1$TE.random
                         ,net1$Cov.random)

test_that("bigger civ gives smaller probability", {
  A = list(fn = "isbiggerCIV", args = list("SFC", "Fluticasone", 2.2))
  B = list(fn = "isbiggerCIV", args = list("SFC", "Fluticasone", 0.2))
  p1 = nmarank(x=net1,predicate=A)$probabilityOfSelection
  p2 = nmarank(x=net1,predicate=B)$probabilityOfSelection
  expect_lt(p1, p2)
})


test_that("civ of 0 gives the same probability as A > B", {
  C = list(fn = "retainOrder", args = list("SFC", "Fluticasone"))
  D = list(fn = "isbiggerCIV", args = list("SFC", "Fluticasone", 0))
  p1 = nmarank(x=net1,small.values="good", predicate=(C %XOR% D))$probabilityOfSelection
  expect_equal(p1, 0)
})
