rm(list=ls())
pbestData = data.frame(studlab=(1:3)
                       ,TE=c(0,0,0)
                       # ,seTE=c(1,1,1)
                       ,seTE=c(1.1,0.4,0.1)
                       ,treat1=c("A","B","C")
                       ,treat2=c("B","C","A")
                       )
net1 = netmeta(studlab=studlab,TE=TE,seTE=seTE,treat1=treat1,treat2=treat2,
               data=pbestData)

prec1=precranking(netmetaobject=net1,random=F, no_most_prob=NA, nsim=10000,small.values = "bad")

A = list(fn = "retainOrder", args = c("A", "B"))
p1 = probabilityOfSelection(prec1, A)
B = list(fn = "retainOrder", args = c("A", "C"))
p2 = probabilityOfSelection(prec1, B)
C = list(fn = "retainOrder", args = c("C", "B"))
p3 = probabilityOfSelection(prec1, C)

test_that("probability of A > B should be the same as A < C",{
  print(p1,p2)
  expect_lt(p1-p2, 0.01)
  expect_lt(p1-p3, 0.01)
})