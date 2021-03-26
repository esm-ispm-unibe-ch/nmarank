rm(list=ls())
library(devtools)
load_all()
data("Woods2010")

p1 <- pairwise(treatment, event = r, n = N,
               studlab = author, data = Woods2010, sm = "OR")
# Conduct network meta-analysis
net1 <- netmeta(p1)
net1

prec1=precranking(netmetaobject=net1, random=T, no_most_prob=NA, nsim=10000, small.values = "good")

A = list(fn = "retainOrder", args = c("Placebo", "Salmeterol", "SFC"))
B = list(fn = "treatementInSpecificPosition", args = list("Placebo", 1))
C = list(fn = "isthesamerank", args = c("Placebo", "Fluticasone", "Salmeterol", "SFC"))
D = list(fn = "betterEqual", args = list("Fluticasone", 2))
G = list(fn = "betterEqual", args = list("Placebo", 2))
H = list(fn = "isthesamerank", args = c("SFC", "Salmeterol", "Placebo", "Fluticasone"))
I = list(fn = "retainOrder", args = c("SFC", "Fluticasone", "Placebo"))
J = list(fn = "treatementInSpecificPosition", args = list("Fluticasone", 2))
K = list(fn = "betterEqual", args = list("Fluticasone", 2))

test_that("Build selection tree", {
  st = (A %AND% (B %OR% (C %XOR% D)))
  expect_equal( st$root$isRoot
               , T
               )
})

test_that("Build selection tree", {
  p1 = probabilityOfSelection(prec1, H)
  expect_true(abs(p1-0.0904)<0.01)
})

test_that("check Selection", {
  st = (A %OR% (B %XOR% (C %OR% (D %AND% G))))
  st1 = (B %XOR% (C %OR% (D %AND% G))) %OR% A
  ranksrow = c("Placebo", "Salmeterol", "Fluticasone", "SFC")
  holds = selectionHolds(st, ranksrow)
  expect_true(holds)
  expect_equal(selectionHolds(st,ranksrow),selectionHolds(st1,ranksrow))
})

test_that("Commutative selections", {
  st1 = (A %OR% (B %XOR% (C %OR% (D %AND% G))))
  st2 = (B %XOR% (C %OR% (D %AND% G))) %OR% A
  p1 = probabilityOfSelection(prec1, st1)
  p2 = probabilityOfSelection(prec1, st2)
  expect_equal(p1, p2)
})
