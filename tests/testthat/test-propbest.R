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

A = list(fn = "retainOrder", args = c("A", "B"))
p1 = nmarank(x=net1,predicate=A)$probabilityOfSelection
B = list(fn = "retainOrder", args = c("A", "C"))
p2 = nmarank(x=net1,predicate=B)$probabilityOfSelection
C = list(fn = "retainOrder", args = c("C", "B"))
p3 = nmarank(x=net1,predicate=C)$probabilityOfSelection

#Needs rewrite since p1 p2 and p3 are not part of the same simulation
test_that("probability of A > B should be the same as A < C",{
  expect_lt(p1-p2, 0.1)
  expect_lt(p1-p3, 0.1)
})
