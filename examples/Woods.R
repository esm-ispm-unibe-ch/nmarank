library(devtools)
load_all()

#example 1: Woods2010
# Load data
data("Woods2010")

# Prepare data
p1 = pairwise( treatment, event = r, n = N, studlab = author
             , data = Woods2010, sm = "OR")

# Conduct network meta-analysis
net1 = netmeta(p1)

# Calculate probabilities of all hierarchies (Table 2)
ranks = nmarank(net1, nsim=10000, small.values = "good")
View(ranks$hierarchies)

# Calculate probabilities of hierarchies that satisfy a set of criteria

#criterion A and its probability
criterionA = list( fn = "isthesamehierarchy"
                 , args = c("SFC", "Salmeterol", "Fluticasone", "Placebo"))
ranksA = nmarank(net1,nsim=10000,small.values = "good", predicate=criterionA)
print(paste("probability that hierarchy is "
           ,"SFC, Salmeterol, Fluticasone, Placebo’: "
           , ranksA$probabilityOfSelection
           , sep=""))

#criterion B and its probability
criterionB = list(fn = "retainOrder"
                 , args = c("SFC", "Fluticasone", "Placebo"))
ranksB = nmarank(net1,nsim=10000,small.values = "good", predicate=criterionB)
print(paste("probability that order "
   ,"‘SFC, Fluticasone, Placebo’ is retained anywhere in the hierarchy: ",
        ranksB$probabilityOfSelection,
        sep=""))

#criterion C and its probability
criterionC = list(fn = "betterEqual", args = list("Fluticasone", 2))
ranksC = nmarank(net1,nsim=10000,small.values = "good", predicate=criterionC)
print(paste("probability that Fluticasone is among the best two options: ",
        ranksC$probabilityOfSelection
        ,sep=""))

#criterion D and its probability
seD1 = list(fn = "betterEqual", args = list("SFC", 2))
seD2 = list(fn = "betterEqual", args = list("Salmeterol", 2))
criterionD = (seD1 %AND% seD2)
ranksD = nmarank(net1,nsim=10000,small.values = "good", predicate=criterionD)
print(paste("probability that SFC and Salmeterol are the two best options: ",
        ranksD$probabilityOfSelection,sep=""))

#criterion E and its probability
criterionE = criterionB %AND% criterionC
ranksE = nmarank(net1,nsim=10000,small.values = "good", predicate=criterionE)
print(paste("probability that criteria B and C are satisfied: ",
        ranksE$probabilityOfSelection, sep=""))

