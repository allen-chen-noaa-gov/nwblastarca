CalculateUtility <- function(own.boat,                                # Own a boat? 0=no, 1=yes
                             opt.out,                                 # Opted out of fishing (T/F)
                             utility.coefs,                           # Utility coefficients for economic model
                             area = NULL,                             # OR or WA
                             sub.area = NULL,                         # Sub-area within area
                             type = NULL,                             # Trip type bottomfish or salmon
                             catch.weights = NULL,                    # List of kept and discarded fish by weight
                             utility.catch.weight.coefs = NULL,       # Utility coefficients for economic model
                             opt.out.prob = NULL,                     # Probabilty of opting out of fishing
                             catch.weight.binned = TRUE,              # Utility depends on weight bin (T/F)
                             trip.cost = NULL,                        # Costs of each potential trip type
                             trip.type.utility.coefs.subarea = NULL,  # Trip type utility for this sub-area
                             utlity.catch.term.names = NULL,          # Stock categories for lineral utility
                             utlity.catch.squared.term.names = NULL) {# Stock categories for quadratic utility
    #' CalculateUtility
    #'
    #' CalculateUtility
    #'
    #' @param own.boat Own a boat? 0=no, 1=yes
    #'
    #' @return utility utility
    #' @export
    #' @examples
    #' 
    
  # Create vector to hold the utility multipliers
  utility.multipliers <- rep(0, nrow(utility.coefs))
  
  # If the person opted out of fish, utility is just calculated based on the opt coefficient
  # Set the utility coefficient for all trips that aren't marine fishing
  if(opt.out==TRUE) { 
    
    # Get the proportion of people who choose the different opt out activities
    # The proportion are different based on whether or not they own a boat
    utility.multipliers[which(utility.coefs$Variable1=='Opt' & utility.coefs$Variable2=='Shore')] <- 
      opt.out.prob$Shore[which(opt.out.prob$BoatType==own.boat)]
    utility.multipliers[which(utility.coefs$Variable1=='Opt' & utility.coefs$Variable2=='Freshwater')] <- 
      opt.out.prob$Freshwater[which(opt.out.prob$BoatType==own.boat)]
    utility.multipliers[which(utility.coefs$Variable1=='Opt' & utility.coefs$Variable2=='Otherstate')] <- 
      opt.out.prob$OtherState[which(opt.out.prob$BoatType==own.boat)]
    
  } else { # Set utility coefficients for all trips that result in marine fishing
    
    # Set the trip cost based on the trip type and state
    utility.multipliers[which(utility.coefs$Variable1=='Price')] <- 
      trip.cost$Cost[which(trip.cost$BoatType==own.boat)]
    
    # Set the trip type coefficient
    utility.multipliers[which(utility.coefs$Variable1=='Type')] <- 
      trip.type.utility.coefs.subarea$Coefficient[which(trip.type.utility.coefs.subarea$TripType==type &
                                                          trip.type.utility.coefs.subarea$BoatType==own.boat)]
    
    # Calculate contribution to utility from catch term
    for(stock in names(catch.weights)) {
      
      # Get the weights of all catch for this stock, but kept and released
      catch.weights.stock <- c(catch.weights[[stock]]$kept.weights, catch.weights[[stock]]$released.weights)
      
      if(length(catch.weights.stock) > 0){ # if at least one fish of this stock was caught
        
        # Calculate contribution to utility from catch term
        for(weight in 1:length(catch.weights.stock)){
          
          # Get the index of utility.multipliers that corresponds to catch of a fish of this stock and weight, 
          # where utilty depends on a weight bin
          if(catch.weight.binned==TRUE){
            
            index <- which(utility.coefs$Variable1=='Catch' & utility.coefs$Variable2==utlity.catch.term.names[[stock]] & 
                             utility.coefs$weight.min < catch.weights.stock[weight] & utility.coefs$weight.max > catch.weights.stock[weight])
            
            utility.multipliers[index] <- utility.multipliers[index] + 1
            
          }else{
            # Estimate the utility coefficient for this stock and weight using the Michaelis-Menten relationship
            weight.coef <- as.numeric(MichaelisMenten(catch.weights.stock[weight], 
                                                      utility.catch.weight.coefs[which(rownames(utility.catch.weight.coefs)==utlity.catch.term.names[[stock]]), 1], 
                                                      utility.catch.weight.coefs[which(rownames(utility.catch.weight.coefs)==utlity.catch.term.names[[stock]]), 2]))
            
            # Save the utility coefficient
            index <- which(utility.coefs$Variable1=='ContinuousCatchCoefs')
            utility.multipliers[index] <- utility.multipliers[index] + weight.coef
            
          }
        }
        
        # Caculate the catch squared term for this stock
        utility.multipliers[which(utility.coefs$Variable1=='CatchSquared' & 
                                    utility.coefs$Variable2==utlity.catch.squared.term.names[[stock]])] <- length(catch.weights.stock)^2
        
      }
      
      # Calculation contribution to utility of released fish
      utility.multipliers[which(utility.coefs$Variable1=='PoundsReleased' & 
                                  utility.coefs$Variable2==utlity.catch.term.names[[stock]])] <- sum(catch.weights[[stock]]$released.weights)
    }
    
  }
  
  # Calculate total estimated utility by multiplying the utility coefficients by the utility multipliers set above
  utility <- sum(utility.coefs$Coef*utility.multipliers)
  
  # Return the calculated utility
  return(utility)
  
} 
