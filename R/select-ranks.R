isthesamerank = function(ranklist, rankProbabilityRow){
 out = all(rankProbabilityRow==ranklist)
 return(out)
}

treatementInSpecificPosition = function(treatpos,rankProbabilityRow){
  treat = treatpos[[1]]
  position = treatpos[[2]]
  out = rankProbabilityRow[[position]] == treat
 return(out)
}

retainOrder = function(treatments,rankProbabilityRow){
  indexes = match(treatments,rankProbabilityRow)
  return(!is.unsorted(indexes))
}


#arguments: rankPrecision is the output of precranking function and the predicate is the logical function for selecting ranks 
probabilityOfSelection = function(rankPrecisions, predicateTree){
  setStatus = function(node, rankprobsrow){
   if(is.function(node$fn)){
     node$status = node$fn(node$args, rankprobsrow)
   }else{
     if(node$fn=="AND"){
       node$status = 
         all(sapply(node$children, 
                    function(x){
                      return(setStatus(x,rankprobsrow))
                    }
                    )
       )
     }else{
       if(node$fn=="OR"){
         node$status = 
           any(sapply(node$children, 
                      function(x){
                        return(setStatus(x,rankprobsrow))
                      }
                     )
              )
       }
    }
   }
  }
  ranks = rankPrecisions$Output;
  probs = Reduce(function(acc, i){
  print(predicateTree$Get("status","fn"))
  pred = setStatus(predicateTree, ranks$Hierarchy[[i]])
  print(pred)
  print(predicateTree$Get("status","fn"))
    if(pred){
      out = acc+ranks$Probability[[i]]
    }else{
      out = acc
    }
    return(out)
    }, 1:nrow(ranks), 0)
  return (probs)
}
