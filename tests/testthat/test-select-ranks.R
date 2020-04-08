rm(list=ls())

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

prec1=precranking(netmetaobject=net1, random=F, no_most_prob=NA, nsim=10000, small.values = "bad")

selectionTree = FromListExplicit(
  list(operation = "op1"
       , fn="AND"
       , status="U"
       , arguments = {}
       , children = 
         list( list(operation ="fn1", fn = retainOrder, args = c("Placebo", "Salmeterol", "SFC"),  status="U")
               , list(operation = "op2", fn="OR", status = "U", args ={}
                      , children=
                        list( list(operation = "op3", fn="AND", status = "U"
                                     , children=
                                       list( list(operation="fn4", fn = treatementInSpecificPosition, args=list("Placebo", 1), status="U")
                                           , list(operation="fn5", fn = isthesamerank, args = c("Placebo", "Fluticasone", "Salmeterol", "SFC"), status="U")
                                       )
                        )
                        , list(operation="fn2", fn = treatementInSpecificPosition, args=list("Placebo", 2), status="U")
                        )
               )
         )
  )
, nameName="operation")

print(selectionTree)

print(selectionTree$Get("status","fn"))


#print(selectionTree$Get("status"))

lkj = probabilityOfSelection(prec1, selectionTree)
 
print(lkj)

# skj = probabilityOfSelection(prec1, list(list("Placebo",3), treatementInSpecificPosition))
# 
# print(skj)
# 
# kkj = probabilityOfSelection(prec1, list(c("SFC","Placebo"), retainOrder))
# 
# print(kkj)

