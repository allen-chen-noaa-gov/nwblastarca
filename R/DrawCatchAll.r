DrawCatchAll <- function(catch.per.trip, compliant, num.keep.legal, 
    num.keep.legal.group, length.min.legal.all, length.max.legal.all, 
    rec.sel.at.length, lengths, len.pop.female, len.pop.male, 
    modeled.stock.rec.catch.source.subarea, wave) {
    #' DrawCatchAll
    #'
    #' Draw catch information
    #'
    #' @param catch.per.trip Number of fish caught
    #' @param compliant Is the trip compliant with regulations (T/F)
    #' @param num.keep.legal Bag limit for each stock
    #' @param num.keep.legal.group Bag limit for each species group
    #' @param length.min.legal.all Minimum legal length for all stocks
    #' @param length.max.legal.all Maximum legal length for all stocks
    #' @param rec.sel.at.length Recreational selectivity at length for all 
    #' stocks
    #' @param lengths Length bins for all stocks
    #' @param len.pop.female Number of females by length bin for all stocks
    #' @param len.pop.male Number of males by length bin for all stocks
    #' @param modeled.stock.rec.catch.source.subarea Subarea for modeled stock
    #' @param wave Current wave
    #'
    #' @return catch.info Catch information
    #' @export
    #' @examples
    #'
    
    # Create a list to hold the kept and release catch for each stock
    catch.info <- sapply(names(catch.per.trip), 
        function(x) x=list(kept.lengths=NULL, kept.sexes=NULL, 
            released.lengths=NULL, released.sexes=NULL),
            simplify=FALSE, USE.NAMES=TRUE)
  
    # Calculate the total number of fish caught on this trip
    total.catch <- Reduce("+", catch.per.trip)

    # If nothing was cuaght, no need to simulate the catch order
    if(total.catch==0) return(catch.info)

    # Draw the order of the fish caught by stock  
    catch.order <- sample(rep(names(catch.per.trip), unlist(catch.per.trip)), 
        total.catch, replace=FALSE) 
  
    for(i in 1:total.catch) { 
    # In order, assessing whether each fish is caught or discarded

    # identify the ith stock caught
    stock <- catch.order[i]

    # Identify the correpsonding modeled population for this stock
    # If there is not a corresponding modeled populuation, this will be NA
    model.pop.name <- modeled.stock.rec.catch.source.subarea[[stock]]

    ##### Draw the information for this caught fish    
    # If this is a static stock, probabilty distribution of the lengths of 
    # caught fish are just based on catch from recent years
    if(is.na(model.pop.name)) {
      
        # Draw fish catch from the probability distribution
        catch.length <- sample(x=lengths[[stock]], size=1, 
            prob=rec.sel.at.length[[stock]]) 
        catch.sex <- 3
      
    } else { 
    # If it's a dynamic stock, draw length and sex based on rec selectivity and 
    # current population

        # Estimate the probability mass function of the length and sex of caught 
        # fish based on the current stock structure and recreational selectivity
        rec.total.fishable.pop <- sum(rec.sel.at.length[[stock]]$Male * 
            len.pop.male[[stock]]) + sum(rec.sel.at.length[[stock]]$Female * 
            len.pop.female[[stock]])
        length.caught.prob.male <- (rec.sel.at.length[[stock]]$Male * 
            len.pop.male[[stock]]) / rec.total.fishable.pop
        length.caught.prob.female <- (rec.sel.at.length[[stock]]$Female * 
            len.pop.female[[stock]]) / rec.total.fishable.pop
      
        # Draw fish catch from the probability distribution
        catch.length.sex <- sample(x=(1:(length(lengths[[stock]])*2)), size=1, 
            prob=c(length.caught.prob.female, length.caught.prob.male))  
      
        if(catch.length.sex > length(lengths[[stock]])){ 
            catch.sex <- 2 # Male
            catch.length <- lengths[[stock]][catch.length.sex-
                length(lengths[[stock]])]
        } else { 
            catch.sex <- 1 # Female
            catch.length <- lengths[[stock]][catch.length.sex]
        }
    } # END if else dynamic or static stock

    # Determine if any groups the stock is part of are at their catch limit
    at.group.limit <- FALSE
    groups.part <- sapply(num.keep.legal.group, function(x) stock %in% x[[1]], 
        simplify = TRUE, USE.NAMES = TRUE)
    for(group in names(which(groups.part==TRUE))){
        group.stocks <- num.keep.legal.group[[group]][[1]]
        group.total <- sum(sapply(group.stocks, 
            function(x) length(catch.info[[x]]$kept.lengths), simplify = TRUE, 
            USE.NAMES = TRUE))
        if(group.total >= num.keep.legal.group[[group]][[2]]) 
            at.group.limit=TRUE
    } # END species group loop

    # Determine if this fish should be kept or released
    # Release if it's below the minimum size limit, above the maximum size 
    # limit, at the catch limit for the stock, or at the catch limit for a group 
    # the stock part of
    if(catch.length < length.min.legal.all[[stock]] ||
        catch.length > length.max.legal.all[[stock]] ||
        length(catch.info[[stock]]$kept.lengths) >= 
        num.keep.legal[[stock]][[wave]] || at.group.limit) { 
      
        catch.info[[stock]]$released.lengths <- c(
            catch.info[[stock]]$released.length, catch.length)
        catch.info[[stock]]$released.sexes <- c(
            catch.info[[stock]]$released.sexes, catch.sex)
      
    } else { # otherwise keep
      
        catch.info[[stock]]$kept.lengths <- c(catch.info[[stock]]$kept.lengths, 
            catch.length)
        catch.info[[stock]]$kept.sexes <- c(catch.info[[stock]]$kept.sexes, 
            catch.sex)
            
    } # END keep or discard loop
    } # END each fish in catch loop

    return(catch.info)
  
} 
