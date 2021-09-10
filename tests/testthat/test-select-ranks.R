rm(list=ls())
data("Woods2010")

p1 <- pairwise(treatment, event = r, n = N,
               studlab = author, data = Woods2010, sm = "OR")
# Conduct network meta-analysis
net1 <- netmeta(p1)
net1

A = condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
B = condition("specificPosition", "Placebo", 1)
C = condition("sameHierarchy", c("Placebo", "Fluticasone", "Salmeterol", "SFC"))
D = condition("betterEqual", "Fluticasone", 2)
G = condition("betterEqual", "Placebo", 2)

test_that("Build selection tree", {
  st = (A %AND% (B %OR% (C %XOR% D)))
  expect_equal( st$root$isRoot
               , T
               )
})

test_that("check Selection tree", {
  st = (A %OR% (B %XOR% (C %OR% (D %AND% G))))
  st1 = (B %XOR% (C %OR% (D %AND% G))) %OR% A
  effs <- nmaEffects(net1$TE.random, net1$Cov.random)
  ranksrow = effs$TE
  holds = selectionHolds(st, small.values="good", ranksrow)
  expect_true(holds)
  expect_equal(selectionHolds(st, small.values="good", ranksrow),
               selectionHolds(st1, small.values="good", ranksrow))
})

test_that("Commutative selections", {
  st = (B %XOR% (C %OR% (D %AND% G))) %OR% A
  st1 = st %AND% G
  p1 = nethierarchy(net1, st1)$probabilityOfSelection
  expect_type(p1, "double")
})

test_that("test opposite (not) function", {
  p1 = nethierarchy(net1, B %AND% opposite(B))$probabilityOfSelection
  expect_equal(p1, 0)
})
