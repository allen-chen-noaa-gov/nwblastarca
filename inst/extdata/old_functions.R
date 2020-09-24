##############################################################################################################
##############################################################################################################
####                                           ARCA Functions.r                                           ####
## These functions support the ARCA Tool                                                                    ##
##                                                                                                          ##
## Creator:   Josh Nowlis                                                                                   ##
## Created:   16 May 2018                                                                                   ##
## Version:   1.0                                                                                           ##
## ModDate:   13 Aug 2018                                                                                   ##
##  Tested:   Y                                                                                             ##
##  Issues:   Need to test TripDetails                                                                                          ##
####                                                                                                      ####
##############################################################################################################
##############################################################################################################

##############################################################################################################
####  Generate length-to-age keys (based on Dave Records' code)
GenLengthAgeKey <- function(numbersatage,    # Vector of current numbers at age
                            ALKey) {         # DF of age-length-conversion w/C1=LBins, C2=first age
  # Organize the numbersatage and age-length-key
  cols <- c("age",unlist(ALKey[1]))
  ALKey <- data.frame(as.numeric(names(ALKey)[-1]),
                      t(ALKey[,-1]))
  names(ALKey) <- cols
  
  tmp1 <- data.frame(age=ALKey[1],
                     count=numbersatage)
  
  # Now merge counts with age-length key for ease of multiplying
  tmp2 <- merge(tmp1,ALKey, by.x="age", by.y="age", all.x=FALSE, all.y=FALSE)
  
  # Now multiply age counts times length distribution to get numbers at length for each age
  for (i in names(tmp2[3:ncol(tmp2)])) {
    tmp2[,i]<-tmp2[,"count"]*tmp2[,i]
  }
  
  # Now drop the counts column and reshape the data to long
  tmp3 <- tmp2[,-which(names(tmp2)=="count")]
  tmp4 <- reshape2:::melt.data.frame(tmp3,variable.name="length",id.vars="age")
  
  # Now generate probability distribution of ages at length.  
  #   Note: convert to data.table for fast and easy manipulation.
  tmp5 <- data.table(tmp4)
  tmp5[, sumCount:=sum(value), by=length]    # sum counts over length classes
  tmp5$prob <- (tmp5$value/tmp5$sumCount)    # divide count for each age at len by total len count
  tmp5$prob[which(tmp5$prob=="NaN")] <- 0    # address cases where certain sizes not observed
  tmp5 <- tmp5[,.(age,length,prob)]
  tmp5[,"prob"][tmp5[,"prob"]<1e-15]<-0      # Zero out very small probabilities
  
  tmp5 <- as.data.frame(tmp5)                # Convert back to data frame
  
  # Now reshape to wide so that rows contain a probability distribution of ages for each length class.
  tmp6 <- reshape2:::dcast(tmp5, length~age, value.var="prob")
  tmp6[,-1] <- tmp6[,-1][,order(as.numeric(names(tmp6)[-1]))]
  
  return(tmp6)
} # END GenLengthAgeKey function
##############################################################################################################

##############################################################################################################
####  Graphing functions, must be setup with x col called xvar and y cols called var1, var2, etc
TwoLineGraph <- function(dat,           # Data source
                         #xlim=NULL,     # x limits c(min,max)
                         xlabel,        # Label for x axis
                         ycolors,       # List of colors for each y
                         ynames,        # Legend names
                         #ylim=NULL,     # y limits c(min,max)
                         ylabel,        # Label for y axis
                         title,         # Graph title
                         theme=NULL) {  # Graph theme
  graph <- ggplot() +
    geom_line(data=dat, aes(x=xvar, y=var1, color="var1"), size=2) +
    geom_line(data=dat, aes(x=xvar, y=var2, color="var2"), size=2) +
    scale_color_manual(values=ycolors, labels=ynames) +
    #xlim(xlim) + ylim(ylim) +
    xlab(xlabel) + ylab(ylabel) + 
    ggtitle(title) +
    mytheme + topright
} # END TwoLineGraph Function

ThreeLineGraph <- function(dat,           # Data source
                           #xlim=NULL,     # x limits c(min,max)
                           xlabel,        # Label for x axis
                           ycolors,       # List of colors for each y
                           ynames,        # Legend names
                           #ylim=NULL,     # y limits c(min,max)
                           ylabel,        # Label for y axis
                           title,         # Graph title
                           theme=NULL) {  # Graph theme
  graph <- ggplot() +
    geom_line(data=dat, aes(x=xvar, y=var1, color="var1"), size=2) +
    geom_line(data=dat, aes(x=xvar, y=var2, color="var2"), size=2) +
    geom_line(data=dat, aes(x=xvar, y=var3, color="var3"), size=2) +
    scale_color_manual(values=ycolors, labels=ynames) +
    #xlim(xlim) + ylim(ylim) +
    xlab(xlabel) + ylab(ylabel) + 
    ggtitle(title) +
    mytheme + topright
} # END ThreeLineGraph Function
##############################################################################################################

##############################################################################################################
#### Pauses script until user presses enter. Used as part of validation, only works when code Sourced
pause <- function() {
  prompt <- "Press <Enter> to continue > "
  if(interactive()) {
    invisible(readline(prompt))
  } else {
    cat(prompt)
    invisible(readLines(file("stdin"),1))
  }
} # END pause function
##############################################################################################################

##############################################################################################################
#### FUNCTION TripDetails
# Workhorse function used to identify choices and, subsequently, catches within a wave-area combination
TripDetails <- function(curryear,
                        currmonth,
                        wave,
                        area,                               # Larger area definitions, OR or WA
                        subarea,
                        boattype,
                        triptype,
                        catch.per.trip.prob.all,            # Probability of number of fish caught per trip
                        dynamic.stocks,
                        fishing.area.subareas,              #
                        length.max.legal.all,               # Maximum length of retention (by stock)
                        length.min.legal.all,               # Minimum length of retention (by stock)
                        length.weight.params,
                        lengths,                            # Length bins for dynamic stocks
                        modeled.stock.rec.catch.source,     # Mapping of sub-area stocks to dynamic models
                        NAL,
                        noncompliance.rate,                 # Rate of non-compliance with retention limits
                        num.keep.legal.all,                 # Max number of fish retained by stock
                        num.keep.legal.group.by.area,       # Max number of fish retained by species group
                        opt.out.prob.by.area,
                        rec.sel.at.length,                  # Recreational selectivity at length
                        release.mortality.rate,             # Release mortality rate by stock
                        stocks.model.pop.this.area,
                        stocks.rec.catch.by.area,           # Names of the dynamic stocks
                        trip.cost.by.area,
                        trip.type.utility.coefs,
                        trip.types.by.area,                 # Trip options (bottomfish, salmon) by subarea
                        utility.catch.weight.coefs,
                        utility.coefs,                      # Coefficients for utility model
                        utlity.catch.squared.term.names,    # Categories for quadratic utility terms
                        utlity.catch.term.names,            # Categories for linear utility terms
                        or.lingcod.bottomfish.catch.per.trip.model,
                        wa.lingcod.bottomfish.catch.per.trip.model,
                        wa.lingcod.salmon.catch.per.trip.model) {

  ## Initialize
  # Create a list to hold the number of fish available to the recreational fishery and NAL for dynamic stocks
  num.available.to.rec.fishery <- list()
  
  # Summarize length distributions
  dynamic.stocks.this.area <- unique(unlist(lapply(modeled.stock.rec.catch.source[[area]], 
                                                   function(x) names(which(x==dynamic.stocks)))))
  len.pop.female <- len.pop.male <- vector("list", length(dynamic.stocks.this.area))
  names(len.pop.female) <- names(len.pop.male) <- dynamic.stocks.this.area
  
  # Determine compliance
  compliant <- sample(c(TRUE,FALSE), size=1, prob=c((1-noncompliance.rate), noncompliance.rate))
  
  # Get the number of fish by stock group that can be kept legally for this sub-area wave
  num.keep.legal.group <- sapply(names(num.keep.legal.group.by.area[[area]][[subarea]]), 
                                 function(x) list(num.keep.legal.group.by.area[[area]][[subarea]][[x]][[1]],
                                             num.keep.legal.group.by.area[[area]][[subarea]][[x]][[2]][wave]),
                                 simplify = FALSE, USE.NAMES = TRUE)
  
  # Subset catch per trip probabilities
  catch.per.trip.prob.subsetted <- catch.per.trip.prob.all[stocks.rec.catch.by.area[[area]][[subarea]]]
  catch.per.trip.prob.subsetted <- catch.per.trip.prob.subsetted[
                                                         !sapply(catch.per.trip.prob.subsetted, is.null)]
  catch.per.trip.prob.subsetted <- lapply(catch.per.trip.prob.subsetted, function(x)
    subset(x, Wave==wave & BoatType==boattype & TripType==triptype, select=X0:ncol(x)))
  
  # Add dynamic stocks (MAKE THIS GENERAL)
  # Add the catch per trip probabilities for dynamic stocks the population dynamics of (ADD BLACK ROCKFISH)
  # NOTE: no Puget.Sound because stock assessments don't apply to that sub-area, thus is never dynamic
  for(stock in dynamic.stocks) {
    # Get the names of the sub-area stocks that correspond to this dynamic stock to get the selectivity info
    stock.name.for.selectivity <- unique(unlist(lapply(modeled.stock.rec.catch.source[[area]], 
                                                       function(x) names(which(x==stock)))))
    
    # Subsetted numbers at length
    len.pop.female[[stock.name.for.selectivity]] <-
      unlist(NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & NAL[[stock]]$Month==currmonth &
                                  NAL[[stock]]$Sex==1)])
    len.pop.male[[stock.name.for.selectivity]] <-
      unlist(NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & NAL[[stock]]$Month==currmonth & 
                                  NAL[[stock]]$Sex==2)])
    
    # Calculate numbers available to the recreational fishery to adjust catch per trip probabilities
    for(i in 1:length(stock.name.for.selectivity)) {
      num.available.to.rec.fishery[[stock.name.for.selectivity[i]]] <- 
        sum(rec.sel.at.length[[stock.name.for.selectivity]]$Female * 
              len.pop.female[[stock.name.for.selectivity]]) +
        sum(rec.sel.at.length[[stock.name.for.selectivity]]$Male * 
              len.pop.male[[stock.name.for.selectivity]])
    } # End sub-area stock loop
  } # END dynamic stock loop
  
  # Now run appropriate catch per trip model
  if(area=='WA' & subarea=='WA.Coast') {
    if(triptype=='bottomfish') {
      if(wave %in% 2:5) {
        wave.use <- wave
      } else if(wave==1) { # For wave 1, model as if wave 2
        wave.use <- 2
      } else if(wave==6) { # For wave 6, model as if wave 5
        wave.use <- 5
      } else warning('Wave mis-specified')
      
      new.data <- data.frame(BoatType=factor(boattype, levels=boat.types), 
                             Anglers=1, 
                             Wave=factor(wave.use, levels=2:5), 
                             LingcodAvailable=num.available.to.rec.fishery$WA.lingcod)
      lingcod.probs <- predict(wa.lingcod.bottomfish.catch.per.trip.model, new.data, type='prob')
      catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, list(WA.lingcod=lingcod.probs))
      
    } else if(triptype=='salmon') {
      new.data <- data.frame(BoatType=factor(boattype, levels=boat.types), 
                             Anglers=1, 
                             Wave=factor(wave, levels=3:5), 
                             LingcodAvailable=num.available.to.rec.fishery$WA.lingcod)
      lingcod.probs <- predict(wa.lingcod.salmon.catch.per.trip.model, new.data, type='prob')
      catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, list(WA.lingcod=lingcod.probs))
      
    } else warning('trip type not specified correctly in RecAnglerChoice') # END if else trip types
  } else if(area=='OR' & subarea=='OR.Ocean') {
    if(triptype=='bottomfish') {
      new.data <- data.frame(BoatType=factor(boattype, levels=boat.types), 
                             Anglers=1, 
                             Wave=factor(wave, levels=1:6), 
                             LingcodAvailable=num.available.to.rec.fishery$OR.lingcod,
                             OcnEst=FALSE)
      lingcod.probs <- predict(or.lingcod.bottomfish.catch.per.trip.model, new.data, type='prob')
      catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, list(OR.lingcod=lingcod.probs))
    } else if(triptype=='salmon') {
      catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, list(OR.lingcod=as.matrix(1)))
    } else warning('trip type not specified correctly in RecAnglerChoice') # END if else trip types
  } else if(area=='OR' & subarea=='OR.Estuary') {
    if(triptype=='bottomfish') {
      new.data <- data.frame(BoatType=factor(boattype, levels=boat.types), 
                             Anglers=1, 
                             Wave=factor(wave, levels=1:6), 
                             LingcodAvailable=num.available.to.rec.fishery$OR.lingcod,
                             OcnEst=TRUE)
      lingcod.probs <- predict(or.lingcod.bottomfish.catch.per.trip.model, new.data, type='prob')
      catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, list(OR.lingcod=lingcod.probs))
    }else if(triptype=='salmon'){
      catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, list(OR.lingcod=as.matrix(1)))
    }else warning('trip type not specified correctly in RecAnglerChoice')
  } # END if else sub-areas
  ## END dynamic stock catch per trip
  
  ## Generate catch info
  # Draw the catch for this trip for each stock
  catch.per.trip <- sapply(stocks.rec.catch.by.area[[area]][[subarea]],
                           function(x) sample(0:(ncol(catch.per.trip.prob.subsetted[[x]])-1),
                                             size=1,prob=catch.per.trip.prob.subsetted[[x]][1,],replace=TRUE),
                           simplify=FALSE, USE.NAMES=TRUE)
  
  # Draw the lengths of the fish caught (kept and released) for this choice occasion    
  catch.info <- DrawCatchAll(catch.per.trip = catch.per.trip, 
                             compliant = compliant, 
                             num.keep.legal = num.keep.legal.all, 
                             num.keep.legal.group = num.keep.legal.group, 
                             length.min.legal.all = length.min.legal.all, 
                             length.max.legal.all = length.max.legal.all,
                             rec.sel.at.length = rec.sel.at.length, 
                             lengths = lengths, 
                             len.pop.female = len.pop.female, 
                             len.pop.male = len.pop.male,
                             modeled.stock.rec.catch.source.subarea = 
                               modeled.stock.rec.catch.source[[area]][[subarea]])
  
  # Calculate the weights of fish caught (kept and released) using the length-weight relationship
  catch.weights <- mapply(CatchWeightsEachChoice, 
                          catch.lengths.stock.choice = catch.info, 
                          length.weight.params.stock = 
                            length.weight.params[stocks.rec.catch.by.area[[area]][[subarea]]],
                          SIMPLIFY = FALSE, 
                          USE.NAMES = TRUE)    
  
  # Calculate the utility of this trip
  trip.utility <- CalculateUtility(opt.out = FALSE, 
                                   area = area, 
                                   sub.area = subarea,
                                   type = triptype, 
                                   own.boat = boattype, 
                                   catch.weights = catch.weights, 
                                   utility.coefs = utility.coefs, 
                                   utility.catch.weight.coefs = utility.catch.weight.coefs, 
                                   opt.out.prob = opt.out.prob.by.area[[area]],
                                   catch.weight.binned = TRUE,
                                   trip.cost = trip.cost.by.area[[area]][[subarea]],
                                   trip.type.utility.coefs.subarea=trip.type.utility.coefs[[area]][[subarea]],
                                   utlity.catch.term.names = utlity.catch.term.names,
                                   utlity.catch.squared.term.names = utlity.catch.squared.term.names)
  
  return(list(catch=catch.info, utility=trip.utility))
} # END TripDetails function
##############################################################################################################

##############################################################################################################
#### FUNCTION DrawCatchAll
# Draw the simulated catch for all stock for this choice occasion. Return the kept and released fish
# from this stock by length and sex
#
# Tested?: 14-Nov-2017
# Issues:

DrawCatchAll <- function(catch.per.trip,                         # Number of fish caught
                         compliant,                              # Is the trip compliant with regulations (T/F)
                         num.keep.legal,                         # Bag limit for each stock
                         num.keep.legal.group,                   # Bag limit for each species group
                         length.min.legal.all,                   # Minimum legal length for all stocks
                         length.max.legal.all,                   # Maximum legal length for all stocks
                         rec.sel.at.length,                      # Recreational selectivity at length for all stocks
                         lengths,                                # Length bins for all stocks
                         len.pop.female,                         # Number of females by length bin for all stocks
                         len.pop.male,                           # Number of males by length bin for all stocks
                         modeled.stock.rec.catch.source.subarea) { # 
  
  # Create a list to hold the kept and release catch for each stock
  catch.info <- sapply(names(catch.per.trip), function(x) x=list(kept.lengths=NULL, kept.sexes=NULL, 
                                                                 released.lengths=NULL, released.sexes=NULL),
                       simplify=FALSE, USE.NAMES=TRUE)
  
  # Calculate the total number of fish caught on this trip
  total.catch <- Reduce("+", catch.per.trip)
  
  # If nothing was cuaght, no need to simulate the catch order
  if(total.catch==0) return(catch.info)
  
  # Draw the order of the fish caught by stock  
  catch.order <- sample(rep(names(catch.per.trip), unlist(catch.per.trip)), total.catch, replace=FALSE) 
  
  for(i in 1:total.catch) { # In order, assessing whether each fish is caught or discarded
    
    # identify the ith stock caught
    stock <- catch.order[i]
    
    # Identify the correpsonding modeled population for this stock
    # If there is not a corresponding modeled populuation, this will be NA
    model.pop.name <- modeled.stock.rec.catch.source.subarea[[stock]]
    
    ##### Draw the information for this caught fish    
    # If this is a static stock, probabilty distribution of the lengths of caught fish
    # are just based on catch from recent years
    if(is.na(model.pop.name)) {
      
      # Draw fish catch from the probability distribution
      catch.length <- sample(x=lengths[[stock]], size=1, prob=rec.sel.at.length[[stock]]) 
      catch.sex <- 3
      
    } else { # If it's a dynamic stock, draw length and sex based on rec selectivity and current population
      
      # Estimate the probability mass function of the length and sex of caught fish
      # based on the current stock structure and recreational selectivity
      rec.total.fishable.pop <- sum(rec.sel.at.length$Male * len.pop.male[[stock]]) + 
        sum(rec.sel.at.length[[stock]]$Female * len.pop.female[[stock]])
      length.caught.prob.male <- (rec.sel.at.length[[stock]]$Male * len.pop.male[[stock]]) / 
        rec.total.fishable.pop
      length.caught.prob.female <- (rec.sel.at.length[[stock]]$Female * len.pop.female[[stock]]) / 
        rec.total.fishable.pop
      
      # Draw fish catch from the probability distribution
      catch.length.sex <- sample(x=(1:(length(lengths[[stock]])*2)), size=1, 
                                 prob=c(length.caught.prob.female, length.caught.prob.male))  
      
      if(catch.length.sex > length(lengths[[stock]])){ 
        catch.sex <- 2 # Male
        catch.length <- lengths[[stock]][catch.length.sex-length(lengths[[stock]])]
      }else{ 
        catch.sex <- 1 # Female
        catch.length <- lengths[[stock]][catch.length.sex]
      }
    } # END if else dynamic or static stock
    
    # Determine if any groups the stock is part of are at their catch limit
    at.group.limit <- FALSE
    groups.part <- sapply(num.keep.legal.group, function(x) stock %in% x[[1]], simplify = TRUE, USE.NAMES = TRUE)
    for(group in names(which(groups.part==TRUE))){
      group.stocks <- num.keep.legal.group[[group]][[1]]
      group.total <- sum(sapply(group.stocks, function(x) length(catch.info[[x]]$kept.lengths), simplify = TRUE, 
                                USE.NAMES = TRUE))
      if(group.total >= num.keep.legal.group[[group]][[2]]) at.group.limit=TRUE
    } # END species group loop
    
    ### Determine if this fish should be kept or released
    ### Release if it's below the minimum size limit, above the maximum size limit,
    ### at the catch limit for the stock, or at the catch limit for a group the stock part of
    if(catch.length < length.min.legal.all[[stock]] ||
       catch.length > length.max.legal.all[[stock]] ||
       length(catch.info[[stock]]$kept.lengths) >= num.keep.legal[[stock]] ||
       at.group.limit) { 
      
      catch.info[[stock]]$released.lengths <- c(catch.info[[stock]]$released.length, catch.length)
      catch.info[[stock]]$released.sexes <- c(catch.info[[stock]]$released.sexes, catch.sex)
      
    } else { # otherwise keep
      
      catch.info[[stock]]$kept.lengths <- c(catch.info[[stock]]$kept.lengths, catch.length)
      catch.info[[stock]]$kept.sexes <- c(catch.info[[stock]]$kept.sexes, catch.sex)
    } # END keep or discard loop
  } # END each fish in catch loop
  
  return(catch.info)
  
} # END DrawCatchAll function
##############################################################################################################

##############################################################################################################
#### FUNCTION CatchWeightsEachChoice
# Calculates the weights of fish from a single choice occasion and a single stock. Returns kept and
# released weights.
#
# Tested?: 14-Nov-2017
# Issues:

CatchWeightsEachChoice <- function(catch.lengths.stock.choice,   # Kept and discarded fish by length
                                   length.weight.params.stock) { # Length-weight parameters
  
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
##############################################################################################################

##############################################################################################################
#### FUNCTION LengthWeight
# Returns the weight in pounds for a fish of the specified length and sex using length-weight
# coefficients, W=aL^b.
#
# Tested?: 14-Nov-2017
# Issues:

LengthWeight <- function(length,   # Fork length in cm
                         sex,      # Sex, 1=female, 2=male
                         a.1,      # a for females
                         b.1,      # b for females
                         a.2,      # a for males
                         b.2,      # b for males
                         a.3,      # a for unsexed fish
                         b.3){     # b for unsexed fish
  
  if(sex==1) { # parameters for females
    a <- a.1
    b <- b.1
  } else if(sex==2) { # parameters for males
    a <- a.2
    b <- b.2
  } else if(sex==3) { # parameters for unsexed fish
    a <- a.3
    b <- b.3
  }
  
  # calcualte weight in kg
  weight <- a*length^b
  
  # convert weight from kg to lbs
  weight <- weight*2.2046
  
  # return weight in lbs
  return(weight)
} # END LengthWeight function
##############################################################################################################

##############################################################################################################
#### FUNCTION CalculateUtility
# Calculate the utility of a specific choice occasion. Returns the calculated utility for this trip.
# NOTE: Need default values for when function is called for opt-out choice.
#
# Tested?: 14-Nov-2017
# Issues:

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
  
} # END CalculateUtility function
##############################################################################################################

##############################################################################################################
#### FUNCTION MichaelisMenten
# Estimate utility when not based on weight bins, using U = aW/(b+W)
#
# Tested?: 14-Nov-2017
# Issues:

MichaelisMenten <- function(x,     # Weight
                            a,     # M-M a parameter
                            b) {   # M-M b parameter
  y.est <- (a*x)/(b+x)
  return(y.est)
} # END MichaelisMenten function
##############################################################################################################

##############################################################################################################
#### FUNCTION CombineLists
# Combine lists based on their common names.
#
# Tested?: 14-Nov-2017
# Issues:

CombineLists <- function(key,                # Combined names from both lists
                         catch.info.all,     # Fist list
                         catch.info) {       # Second list
  
  l1 <- catch.info.all[[key]]
  l2 <- catch.info[[key]]
  len <- max(length(l1), length(l2))
  
  lapply(seq(len), function(i) c(l1[[i]], l2[[i]]))
} # END CombineLists function
##############################################################################################################

######## END FUNCTIONS
##############################################################################################################
##############################################################################################################