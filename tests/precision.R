rm(list=ls())
library(nmarank)
library(netmeta)


#Senn
data("Senn2013")
net1 <- netmeta(TE, seTE, treat1, treat2, studlab,
                data=Senn2013, sm="MD")

#Linde
data("Linde2015")
p1 <- pairwise(list(treatment1, treatment2, treatment3),
               event = list(resp1, resp2, resp3),
               n = list(n1, n2, n3),
               studlab = id, data = Linde2015, sm = "OR")

# Define order of treatments
trts <- c("TCA", "SSRI", "SNRI", "NRI",
          "Low-dose SARI", "NaSSa", "rMAO-A", "Hypericum",
          "Placebo")

# Conduct network meta-analysis
net1 <- netmeta(p1, comb.fixed = FALSE, comb.random = TRUE,
                reference = "Placebo",
                seq = trts)

#parkinson
data("parkinson")
p1 <- pairwise(list(Treatment1, Treatment2, Treatment3),
               n=list(n1, n2, n3),
               mean=list(y1, y2, y3),
               sd=list(sd1, sd2, sd3),
               data=parkinson, studlab=Study)
p1

# Conduct network meta-analysis
net1 <- netmeta(p1)
net1

#Woods
data("Woods2010")

p1 <- pairwise(treatment, event = r, n = N,
               studlab = author, data = Woods2010, sm = "OR")
p1

# Conduct network meta-analysis
net1 <- netmeta(p1)
net1

#netmetaobject=net1
#random=T
#no_most_prob=5
#nsim=1000
#small.values="good"

prec1=precranking(netmetaobject=net1,random=F, no_most_prob=NA, nsim=10000,small.values = "bad")

prec1$Output


#metrics
precobject=prec1
hierarchy="pBV"


m1=metrics(precobject=prec1,hierarchy="pscores")



#Hierarchy: means, sucras, pBV, pretas, most_prob, sygekrimenh 
#no_most_prob: number of most probable hierarchies, only applicable if most_prob
#implementation: analytically, simulation (only simulation with most_prob)
#number of simulations: only applicable if simulation

#precision
#dataformatter
#main_simulation
#summary
#plot

##### issues to fix
#treatment labels
#check with arm/contrast level, binary/continuous etc
#change names, fors
#define hierarchy
#small values are good or bad
#complicated questions, use reduce
#check sampling

