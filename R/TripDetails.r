TripDetails <- function(curryear, currmonth, wave, area, subarea, boattype, 
    boat.types, triptype, catch.per.trip.prob.all, dynamic.stocks, 
    length.max.legal.all, length.min.legal.all, 
    length.weight.params, lengths, modeled.stock.rec.catch.source, NAL,
    noncompliance.rate, num.keep.legal.all, num.keep.legal.group.by.area, 
    opt.out.prob.by.area, rec.sel.at.length, release.mortality.rate, 
    stocks.model.pop.this.area, stocks.rec.catch.by.area, trip.cost.by.area,
    trip.type.utility.coefs, trip.types.by.area, utility.catch.weight.coefs,
    utility.coefs, utlity.catch.squared.term.names, utlity.catch.term.names,
    or.lingcod.bottomfish.catch.per.trip.model, 
    wa.lingcod.bottomfish.catch.per.trip.model, 
    wa.lingcod.salmon.catch.per.trip.model) {
    #' TripDetails
    #'
    #' Workhorse function used to identify choices and, subsequently, catches 
    #' within a wave-area combination
    #'
    #' @param curryear Current year
    #' @param currmonth Current month
    #' @param wave Current wave
    #' @param area Larger area definitions, OR or WA
    #' @param subarea Smaller area definitions (e.g. Puget Sound)
    #' @param boattype Current boat type charter or private
    #' @param boat.types All boat types charter and private
    #' @param triptype Current trip type (salmon or groundfish)
    #' @param catch.per.trip.prob.all Probability of number of fish caught per 
    #' trip
    #' @param dynamic.stocks Dynamic stocks to model
    #' @param length.max.legal.all Maximum length of retention (by stock)
    #' @param length.min.legal.all Minimum length of retention (by stock)
    #' @param length.weight.params Length-weight conversion parameters
    #' @param lengths Length bins for dynamic stocks
    #' @param modeled.stock.rec.catch.source Mapping of sub-area stocks to 
    #' dynamic models
    #' @param NAL Numbers at length
    #' @param noncompliance.rate Rate of non-compliance with retention limits
    #' @param num.keep.legal.all Max number of fish retained by stock
    #' @param num.keep.legal.group.by.area Max number of fish retained by 
    #' species group
    #' @param opt.out.prob.by.area Opt out probability by area
    #' @param rec.sel.at.length Recreational selectivity at length
    #' @param release.mortality.rate Release mortality rate by stock
    #' @param stocks.model.pop.this.area Data loaded for stock population for
    #' this area
    #' @param stocks.rec.catch.by.area Recreational catches by area
    #' @param trip.cost.by.area Trip cost by area
    #' @param trip.type.utility.coefs Previously modeled utility coefficients
    #' for each trip type
    #' @param trip.types.by.area Trip options (bottomfish, salmon) by subarea
    #' @param utility.catch.weight.coefs Utility coefficients for catch weight
    #' @param utility.coefs Coefficients for utility model
    #' @param utlity.catch.squared.term.names Categories for quadratic utility 
    #' terms
    #' @param utlity.catch.term.names Categories for linear utility terms
    #' @param or.lingcod.bottomfish.catch.per.trip.model Catch model for Oregon
    #' lingcod that depends on stock status
    #' @param wa.lingcod.bottomfish.catch.per.trip.model Catch model for WA
    #' lingcod that depends on stock status
    #' @param wa.lingcod.salmon.catch.per.trip.model Jointcatch model for WA
    #' lingcod and salmon that depends on stock status
    #'
    #' @return List of catch info and trip utility
    #' @import pscl
    #' @export
    #' @examples
    #'
    
    # Initialize
    # Create a list to hold the number of fish available to the recreational 
    # fishery and NAL for dynamic stocks
    num.available.to.rec.fishery <- list()

    # Summarize length distributions
    dynamic.stocks.this.area <- unique(unlist(lapply(
        modeled.stock.rec.catch.source[[area]], 
        function(x) names(which(x==dynamic.stocks)))))
    len.pop.female <- len.pop.male <- vector("list", 
        length(dynamic.stocks.this.area))
    names(len.pop.female) <- names(len.pop.male) <- dynamic.stocks.this.area

    # Determine compliance
    compliant <- sample(c(TRUE,FALSE), size=1, prob=c((1-noncompliance.rate), 
        noncompliance.rate))

    # Get the number of fish by stock group that can be kept legally for this 
    # sub-area wave
    num.keep.legal.group <- sapply(names(
        num.keep.legal.group.by.area[[area]][[subarea]]), 
        function(x) list(
            num.keep.legal.group.by.area[[area]][[subarea]][[x]][[1]],
            num.keep.legal.group.by.area[[area]][[subarea]][[x]][[2]][wave]),
        simplify = FALSE, USE.NAMES = TRUE)

    # Subset catch per trip probabilities
    catch.per.trip.prob.subsetted <- catch.per.trip.prob.all[
        stocks.rec.catch.by.area[[area]][[subarea]]]
    catch.per.trip.prob.subsetted <- catch.per.trip.prob.subsetted[
        !sapply(catch.per.trip.prob.subsetted, is.null)]
    catch.per.trip.prob.subsetted <- lapply(catch.per.trip.prob.subsetted, 
        function(x)
    subset(x, Wave==wave & BoatType==boattype & TripType==triptype, 
        select=X0:ncol(x)))

    # Add dynamic stocks (MAKE THIS GENERAL)
    # Add the catch per trip probabilities for dynamic stocks the population 
    # dynamics of (ADD BLACK ROCKFISH)
    # NOTE: no Puget.Sound because stock assessments don't apply to that 
    # sub-area, thus is never dynamic
    for(stock in dynamic.stocks) {
        # Get the names of the sub-area stocks that correspond to this dynamic
        # stock to get the selectivity info
        stock.name.for.selectivity <- unique(unlist(lapply(
            modeled.stock.rec.catch.source[[area]], 
            function(x) names(which(x==stock)))))

        # Subsetted numbers at length
        len.pop.female[[stock.name.for.selectivity]] <-
            unlist(NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & 
            NAL[[stock]]$Month==currmonth &
            NAL[[stock]]$Sex==1)])
        len.pop.male[[stock.name.for.selectivity]] <-
            unlist(NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & 
            NAL[[stock]]$Month==currmonth & 
            NAL[[stock]]$Sex==2)])

        # Calculate numbers available to the recreational fishery to adjust
        # catch per trip probabilities
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
            } else if(wave==1) { 
                # For wave 1, model as if wave 2
                wave.use <- 2
            } else if(wave==6) { 
                # For wave 6, model as if wave 5
                wave.use <- 5
            } else warning('Wave mis-specified')

            new.data <- data.frame(BoatType=factor(boattype, levels=boat.types), 
                Anglers=1, 
                Wave=factor(wave.use, levels=2:5), 
                LingcodAvailable=num.available.to.rec.fishery$WA.lingcod)
            lingcod.probs <- predict(wa.lingcod.bottomfish.catch.per.trip.model, 
                new.data, type='prob')
            catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, 
                list(WA.lingcod=lingcod.probs))
        } else if(triptype=='salmon') {
            new.data <- data.frame(BoatType=factor(boattype, levels=boat.types), 
                Anglers=1, 
                Wave=factor(wave, levels=3:5), 
                LingcodAvailable=num.available.to.rec.fishery$WA.lingcod)
            lingcod.probs <- predict(wa.lingcod.salmon.catch.per.trip.model, 
                new.data, type='prob')
            catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, 
                list(WA.lingcod=lingcod.probs))
        } else warning('trip type not specified correctly in RecAnglerChoice') 
    # END if else trip types
    } else if(area=='OR' & subarea=='OR.Ocean') {
        if(triptype=='bottomfish') {
            new.data <- data.frame(BoatType=factor(boattype, levels=boat.types), 
                Anglers=1, 
                Wave=factor(wave, levels=1:6), 
                LingcodAvailable=num.available.to.rec.fishery$OR.lingcod,
                OcnEst=FALSE)
            lingcod.probs <- predict(or.lingcod.bottomfish.catch.per.trip.model, 
                new.data, type='prob')
            catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, 
                list(OR.lingcod=lingcod.probs))
        } else if(triptype=='salmon') {
            catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, 
            list(OR.lingcod=as.matrix(1)))
        } else warning('trip type not specified correctly in RecAnglerChoice') 
        # END if else trip types
    } else if(area=='OR' & subarea=='OR.Estuary') {
        if(triptype=='bottomfish') {
            new.data <- data.frame(BoatType=factor(boattype, levels=boat.types), 
                Anglers=1, 
                Wave=factor(wave, levels=1:6), 
                LingcodAvailable=num.available.to.rec.fishery$OR.lingcod,
                OcnEst=TRUE)
            lingcod.probs <- predict(or.lingcod.bottomfish.catch.per.trip.model, 
                new.data, type='prob')
            catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, 
                list(OR.lingcod=lingcod.probs))
        } else if(triptype=='salmon') {
            catch.per.trip.prob.subsetted <- c(catch.per.trip.prob.subsetted, 
                list(OR.lingcod=as.matrix(1)))
        } else warning('trip type not specified correctly in RecAnglerChoice')
    } # END if else sub-areas
    ## END dynamic stock catch per trip

    ## Generate catch info
    # Draw the catch for this trip for each stock
    catch.per.trip <- sapply(stocks.rec.catch.by.area[[area]][[subarea]],
        function(x) sample(0:(ncol(catch.per.trip.prob.subsetted[[x]])-1),
            size=1,prob=catch.per.trip.prob.subsetted[[x]][1,],
            replace=TRUE),
        simplify=FALSE, USE.NAMES=TRUE)

    # Draw the lengths of the fish caught (kept and released) for this choice 
    # occasion    
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
        modeled.stock.rec.catch.source[[area]][[subarea]],
        wave = wave)

    # Calculate the weights of fish caught (kept and released) using the 
    # length-weight relationship
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
        trip.type.utility.coefs.subarea=
        trip.type.utility.coefs[[area]][[subarea]],
        utlity.catch.term.names = utlity.catch.term.names,
        utlity.catch.squared.term.names = utlity.catch.squared.term.names)

    return(list(catch=catch.info, utility=trip.utility))
} 
