#library(devtools)
#load_all() - for testing in devmode
library(nmarank)
library(netmeta)

rm(list=ls())
pbestData = data.frame(studlab=(1:3)
                       ,TE=c(0,0,0)
                       # ,seTE=c(1,1,1)
                       ,seTE=c(1.1,0.7,0.1)
                       ,treat1=c("A","B","C")
                       ,treat2=c("B","C","A")
                       )
net1 = netmeta(studlab=studlab,TE=TE,seTE=seTE,treat1=treat1,treat2=treat2,
               data=pbestData)

prec1=precranking(netmetaobject=net1,random=F, no_most_prob=NA, nsim=10000,small.values = "bad")
print(prec1$Output)

A = list(fn = "retainOrder", args = c("A", "B"))
p1 = probabilityOfSelection(prec1, A)
p1
B = list(fn = "retainOrder", args = c("A", "C"))
p2 = probabilityOfSelection(prec1, B)
p2
C = list(fn = "retainOrder", args = c("C", "B"))
p3 = probabilityOfSelection(prec1, C)
p3
