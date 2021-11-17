rm(list=ls())
library(devtools)
load_all()

#example 2: depression
# Load data

data("depression")

# Prepare data
p1=pairwise(treat=drug_name,event=Responders
           ,n=Ntotal,data=depression,studlab = studyID, sm="OR")

# Conduct network meta-analysis
netp1=netmeta(p1)

# Calculate probabilities of all hierarchies (Table 3)
ranks = nmarank(netp1,nsim=10000,small.values = "bad")
View(ranks$hierarchies)

# Calculate probabilities of hierarchies that satisfy a set of criteria

#criterion A and its probability
sel1 = condition("specificPosition", "Vortioxetine", 1)
sel2 = condition("specificPosition", "Bupropion", 2)
sel3 = condition("specificPosition", "Escitalopram", 3)

criterionA = (sel1 %AND% (sel2 %AND% sel3))
ranksA = nmarank(netp1,nsim=10000,small.values = "bad", condition=criterionA)
print(c("probability of Vor in 1st, Bup in 2nd and Esci in 3"
        ,ranksA$probabilityOfSelection))

#criterion B and its probability
sel4 = condition("betterEqual", "Vortioxetine", 3)
sel5 = condition("betterEqual", "Bupropion", 3)
sel6 = condition("betterEqual", "Escitalopram", 3)

criterionB = (sel4 %AND% (sel5 %AND% sel6))
ranksB = nmarank(netp1,nsim=10000,small.values = "bad", condition=criterionB)
print(c("probability of Vor, Bup and Esci being the first 3"
        ,ranksB$probabilityOfSelection))

#criterion C and its probability
criterionC = condition("retainOrder"
                      ,c("Vortioxetine", "Bupropion", "Escitalopram"))
ranksC = nmarank(netp1,nsim=10000,small.values = "bad", condition=criterionC)
print(c("probability that Vor better than Bupr better than Escit"
        ,ranksC$probabilityOfSelection))

#criterion D and its probability
sel7 = condition("biggerCIV", "Fluoxetine", "Vortioxetine", 0.2231436)
sel8 = condition("biggerCIV", "Fluoxetine", "Bupropion", 0.2231436)
sel9 = condition("biggerCIV", "Fluoxetine", "Escitalopram", 0.2231436)
criterionD = (sel7 %AND% (sel8 %AND% sel9))
ranksD = nmarank(netp1,nsim=10000,small.values = "bad", condition=criterionD)
print(paste("probability that Vor, Bupr & Escit"
           ,"have OR against Fluo more than 1.25: ",
        ranksD$probabilityOfSelection, sep=""))

#criterion E and its probability
sel10 = condition("retainOrder", c("Vortioxetine", "Fluoxetine"))
sel11 = condition("retainOrder", c("Bupropion", "Fluoxetine"))
sel12 = condition("retainOrder", c("Escitalopram", "Fluoxetine"))
criterionE = (sel10 %AND% (sel11 %AND% sel12))
ranksE = nmarank(netp1,nsim=10000,small.values = "bad", condition=criterionE)
print(c("probability that Vor, Bupr & Escit are better than Fluo"
        ,ranksE$probabilityOfSelection))





