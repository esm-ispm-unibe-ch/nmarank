library(alternativenma)
rm(list=ls())
data(depression)
p1=pairwise(treat=drug_name,event=Responders,n=Ntotal,data=depression,studlab = studyID, sm="OR")
netp1=netmeta(p1)
ranks = precranking(netp1,nsim=1000,small.values = "bad")
View(ranks$Output)
write.csv(head(ranks$Output,10) %>% mutate(Hierarchy = sapply(Hierarchy,function(h){return(paste(unlist(h),collapse=", "))})), "depressionRanks.csv")

A = list(fn = "treatementInSpecificPosition", args = list("Vortioxetine", 1))
B = list(fn = "treatementInSpecificPosition", args = list("Bupropion", 2))
C = list(fn = "treatementInSpecificPosition", args = list("Escitalopram", 3))
D = list(fn = "retainOrder", args = c("Vortioxetine", "Bupropion", "Escitalopram"))
E = list(fn = "retainOrder", args = c("Vortioxetine", "Sertraline"))

selection1 = (A %AND% (B %AND% C ))
prob1 = probabilityOfSelection(ranks, selection1)
print(c("probability of Vor in 1st, Bup in 2nd and Esci in 3",prob1))

prob2 = probabilityOfSelection(ranks, D)
print(c("probability that Vor better than Bupr better than Ecit",prob2))

prob3 = probabilityOfSelection(ranks, E)
print(c("probability that Vortio better than Sertraline",prob3))