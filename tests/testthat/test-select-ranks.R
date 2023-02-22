data("Woods2010")

p1 <- pairwise(treatment, event = r, n = N,
               studlab = author, data = Woods2010, sm = "OR")
# Conduct network meta-analysis
net1 <- netmeta(p1)
net1

test_that("Default execution of nmarank without condition", {
  p1 = nmarank(net1)$probabilityOfSelection
  expect_type(p1, "double")
})

A = condition("retainOrder", c("Placebo", "Salmeterol", "SFC"))
B = condition("specificPosition", "Placebo", 1)
C = condition("sameHierarchy",
              c("Placebo", "Fluticasone", "Salmeterol", "SFC"))
D = condition("betterEqual", "Fluticasone", 2)
G = condition("betterEqual", "Placebo", 2)

test_that("Build selection tree", {
  st = (A %AND% (B %OR% (C %XOR% D)))
  expect_equal(st$root$isRoot, TRUE)
})

test_that(paste("small.values = 'desirable' should give",
                "Placebo last for this mortality outcome"), {
  st1 <- condition("specificPosition", "Placebo", 1)
  placeboFirst = nmarank(net1, condition = st1,
                         small.values = "desirable")$probabilityOfSelection
  st2 <- condition("specificPosition", "Placebo", 4)
  placeboLast = nmarank(net1, condition = st2,
                        small.values = "desirable")$probabilityOfSelection
  expect_true(placeboLast > placeboFirst)
})

test_that("check Selection tree", {
  st = (A %OR% (B %XOR% (C %OR% (D %AND% G))))
  st1 = (B %XOR% (C %OR% (D %AND% G))) %OR% A
  effs <- nmarank:::nmaEffects(net1$TE.random, net1$Cov.random)
  ranksrow = effs$TE
  holds = selectionHolds(st, small.values = "undesirable", ranksrow)
  expect_true(holds)
  expect_equal(selectionHolds(st, small.values = "undesirable", ranksrow),
               selectionHolds(st1, small.values = "undesirable", ranksrow))
})

test_that("Commutative selections", {
  st = (B %XOR% (C %OR% (D %AND% G))) %OR% A
  st1 = st %AND% G
  p1 = nmarank(net1, st1)$probabilityOfSelection
  expect_type(p1, "double")
})

test_that("test opposite (not) function", {
  st = B %AND% opposite(B)
  p1 = nmarank(net1, st)$probabilityOfSelection
  expect_equal(p1, 0)
  
})
