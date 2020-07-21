CatchWeightsEachChoice <- function(catch.lengths.stock.choice,   # Kept and discarded fish by length
                                   length.weight.params.stock) { # Length-weight parameters
    #' CatchWeightsEachChoice
    #'
    #' CatchWeightsEachChoice
    #'
    #' @param catch.lengths.stock.choice Kept and discarded fish by length
    #'
    #' @return choice.weights choice.weights
    #' @export
    #' @examples
    #' 
    
  # Calculate weight of kept fish
  if(length(catch.lengths.stock.choice$kept.lengths) > 0) {
    kept.weights <- mapply(LengthWeight, length=catch.lengths.stock.choice$kept.lengths, 
                           sex=catch.lengths.stock.choice$kept.sexes, 
                           a.1=length.weight.params.stock['a.1'], b.1=length.weight.params.stock['b.1'],
                           a.2=length.weight.params.stock['a.2'], b.2=length.weight.params.stock['b.2'],
                           a.3=length.weight.params.stock['a.3'], b.3=length.weight.params.stock['b.3'])
  } else { # if not fish kept, NULL kept.weights
    kept.weights <- NULL
  } # END if else are there kept fish
  
  # Calculate weight of released fish
  if(length(catch.lengths.stock.choice$released.lengths) > 0) {
    released.weights <- mapply(LengthWeight, length=catch.lengths.stock.choice$released.lengths, 
                               sex=catch.lengths.stock.choice$released.sexes, 
                               a.1=length.weight.params.stock['a.1'], b.1=length.weight.params.stock['b.1'],
                               a.2=length.weight.params.stock['a.2'], b.2=length.weight.params.stock['b.2'],
                               a.3=length.weight.params.stock['a.3'], b.3=length.weight.params.stock['b.3'])
  } else { # if not fish released, NULL released.weights
    released.weights <- NULL
  } # END if else are there released fish
  
  # Return kept and released weights
  choice.weights <- list(kept.weights=kept.weights, released.weights=released.weights)
  return(choice.weights)
} # END CatchWeightsEachChoice function
