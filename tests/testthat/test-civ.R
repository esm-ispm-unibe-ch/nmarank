data("Woods2010")

p1 <- pairwise(treatment, event = r, n = N,
               studlab = author, data = Woods2010, sm = "OR")
# Conduct network meta-analysis
net1 <- netmeta(p1, small.values = "good")

effs <- nmarank:::nmaEffects(net1$TE.random, net1$Cov.random)

test_that("bigger civ gives smaller probability", {
  A = condition("biggerCIV", "SFC", "Fluticasone", 2.2)
  B = condition("biggerCIV", "SFC", "Fluticasone", 0.2)
  pA = nmarank(net1, A)$probabilityOfSelection
  pB = nmarank(net1, B)$probabilityOfSelection
  expect_lt(pA, pB)
})


test_that("civ of 0 gives the same probability as C > D", {
  C = condition("retainOrder", c("SFC", "Fluticasone"))
  D = condition("biggerCIV", "SFC", "Fluticasone", 0)
  set.seed(1909)
  pC = nmarank(net1, C)$probabilityOfSelection
  set.seed(1909)
  pD = nmarank(net1, D)$probabilityOfSelection
  expect_equal(pC, pD)
})
