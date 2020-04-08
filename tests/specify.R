#########################################################################
####### Calculating the precision of a specific research question ####### 
#########################################################################
# This calculates the precision of a specific research question
# precobject: An object of class precranking
# question: a function that ...

prec1$rankings[[1]]
prec1$resultsum
c("Fluticasone", "Placebo", "Salmeterol", "SFC")
prec1$Output$Hierarchy[[1]]
prec1$Output$Hierarchy[[1]][prec1$Output$Hierarchy[[1]]=="Placebo"]
prec1$Output$Hierarchy[[1]][prec1$Output$Hierarchy[[1]]=="Placebo"]

#one better or worse than the other
which(prec1$Output$Hierarchy[[1]]=="Placebo") < which(prec1$Output$Hierarchy[[1]]=="Fluticasone")
which(prec1$Output$Hierarchy[[10]]=="Placebo") < which(prec1$Output$Hierarchy[[10]]=="Fluticasone")
which(prec1$Output$Hierarchy[[11]]=="Placebo") < which(prec1$Output$Hierarchy[[11]]=="Fluticasone")

#one at a particular position (first)
prec1$Output$Hierarchy[[1]][1]=="Placebo"
prec1$Output$Hierarchy[[10]][1]=="Placebo"
prec1$Output$Hierarchy[[11]][1]=="Placebo"

#one better than two others
(which(prec1$Output$Hierarchy[[1]]=="Placebo") < which(prec1$Output$Hierarchy[[1]]=="Fluticasone")) & (which(prec1$Output$Hierarchy[[1]]=="Salmeterol") < which(prec1$Output$Hierarchy[[1]]=="SFC"))

#several at particular positions
prec1$Output$Hierarchy[[1]][1]=="Placebo" & prec1$Output$Hierarchy[[1]][2]=="Fluticasone" & prec1$Output$Hierarchy[[1]][3]=="Salmeterol"

#a sequence A, B, C
which(prec1$Output$Hierarchy[[1]]=="Fluticasone") > which(prec1$Output$Hierarchy[[1]]=="Placebo") & which(prec1$Output$Hierarchy[[1]]=="Fluticasone") < which(prec1$Output$Hierarchy[[1]]=="Salmeterol")




