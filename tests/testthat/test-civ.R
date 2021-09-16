rm(list=ls())
data("Woods2010")

p1 <- pairwise(treatment, event = r, n = N,
               studlab = author, data = Woods2010, sm = "OR")
# Conduct network meta-analysis
net1 <- netmeta(p1, small.values = "good")

effs <- nmarank:::nmaEffects(net1$TE.random, net1$Cov.random)

test_that("bigger civ gives smaller probability", {
  A = condition("biggerCIV", "SFC", "Fluticasone", 2.2)
  B = condition("biggerCIV", "SFC", "Fluticasone", 0.2)
  p1 = nmarank(net1, A)$probabilityOfSelection
  p2 = nmarank(net1, B)$probabilityOfSelection
  expect_lt(p1, p2)
})


test_that("civ of 0 gives the same probability as A > B", {
  C = condition("retainOrder", "SFC", "Fluticasone")
  D = condition("biggerCIV", "SFC", "Fluticasone", 0)
  p1 = nmarank(net1, C %XOR% D)
  expect_equal(p1$probabilityOfSelection, 0)
})
