blast_arca_wrapper <- function(MCMC = 1, firstyear, forecastyrs, 
    stocks.rec.catch.by.area, datain, dynamic.stocks = NULL, dynstock,
    parallelr = FALSE, ...) {
    #' blast_arca_wrapper
    #'
    #' Wrapper function for recreational bioeconomic model
    #'
    #' @param MCMC Number of monte carlo runs per scenario
    #' @param firstyear First year of simulation
    #' @param forecastyrs Number of forecast years
    #' @param stocks.rec.catch.by.area 
    #' @param datain Data needed to run model
    #' @param dynamic.stocks Names of stocks that change over time
    #' @param dynstock Stock information for dynamic species (if exists)
    #' @param parallelr Whether to run iterations in parallel (default FALSE)
    #'
    #' @return tsout Time series of fisher catches, stock status, over forecast
    #' years
    #' @export
    #' @importFrom dplyr summarise group_by n
    #' @importFrom magrittr %>%
    #' @importFrom foreach foreach %dopar% getDoParWorkers
    #' @examples
    #'

    stockvars <- Init_dy_var(dynamic.stocks, indat = dynstock)
    list2env(stockvars, environment())

    areas <- names(stocks.rec.catch.by.area)
    subareas <- vector("list",length(areas))
    subareas <- lapply(areas, function(x) names(stocks.rec.catch.by.area[[x]]))
    names(subareas) <- areas

    simyears <- (firstyear):(firstyear+forecastyrs-1)
    simyearsplus <- (firstyear):(firstyear+forecastyrs)

    NAA <- NAL <- SSB <- Recr <- CALcomm <- 
        vector("list", length(dynamic.stocks))
    names(NAA) <- names(NAL) <- names(SSB) <- names(Recr) <- 
        names(CALcomm) <- dynamic.stocks
    
    recrdev <- vector("list", MCMC)
    for (nn in 1:MCMC) {
    recrdev[[nn]] <- vector("list", length(dynamic.stocks))
    names(recrdev[[nn]]) <- dynamic.stocks
    }
    
    CALrec <- vector("list",length(areas))
    names(CALrec) <- areas
    for (area in areas) {
        CALrec[[area]] <- vector("list",length(subareas[[area]]))
        names(CALrec[[area]]) <- subareas[[area]]
        for (subarea in subareas[[area]]) {
            CALrec[[area]][[subarea]] <- 
                vector("list",
                length(stocks.rec.catch.by.area[[area]][[subarea]]))
            names(CALrec[[area]][[subarea]]) <- 
                stocks.rec.catch.by.area[[area]][[subarea]]
        }
    }

    num.available.to.rec.fishery <- len.pop.male <- len.pop.female <- 
        vector("list", length(dynamic.stocks))
    names(num.available.to.rec.fishery) <- names(len.pop.female) <- 
        names(len.pop.male) <- dynamic.stocks

    # list2env(lingcod.dynstock, environment())
    sexes <- c(1,2)
    list2env(datain, environment())
    list2env(Modelinfo, environment())
    list2env(Regulations, environment())
    list2env(Catchinfo, environment())
    list2env(Utility, environment())
    list2env(Stockinfo, environment())
    spawnmonth <- recrmonths
    ## Initialize population data structures
    # Mostly for each dynamic species
    for (stock in dynamic.stocks) {
        NAA[[stock]] <- expand.grid(Year=simyearsplus,
            Month=1:12,
            Sex=sexes,
            Age=ages[[stock]],
            N=NA)
        NAL[[stock]] <- expand.grid(Year=simyearsplus,
            Month=1:12,
            Sex=sexes,
            LBin=lbins[[stock]]$Mean_Size,
            N=NA)
        SSB[[stock]] <- expand.grid(Year=simyears,
            SSB=NA)
        Recr[[stock]] <- expand.grid(Year=simyears,
            Recr=NA)
        NAA[[stock]] <- NAA[[stock]][order(NAA[[stock]]$Year,
            NAA[[stock]]$Month,NAA[[stock]]$Sex,NAA[[stock]]$Age),]
        NAL[[stock]] <- NAL[[stock]][order(NAL[[stock]]$Year,
            NAL[[stock]]$Month,NAL[[stock]]$Sex,NAL[[stock]]$LBin),]
      
    # Set initial population sizes
    for(sex in sexes) {
        NAA[[stock]]$N[which(NAA[[stock]]$Year==firstyear & 
            NAA[[stock]]$Sex==sexes[sex] & NAA[[stock]]$Month==1)] <- 
            popsize[[stock]][which(popsize[[stock]]$Time==firstyear & 
            popsize[[stock]]$Sex==sexes[sex]), 11:ncol(popsize[[stock]])]           
            # 11-end cols are actual age-structured popsize
    }
      
    # Draw random recruitment deviations
    for (devit in 1:MCMC) {
    recrdev[[devit]][[stock]] <- data.frame(Year=simyears,
        Dev={if(MCMC>1) {                 
        # ONLY USE IF RUNNING REPLICATE RUNS
        rnorm(length(simyears),mean=0,sd=sigmaR[[stock]])
        } else {rep(0,length(simyears))}})
    }
    # END recrdevs draw
    
    # Set up commercial catches
    CALcomm[[stock]] <- expand.grid(Year=simyears,
        Month=1:12,
        Fleet=Fleets$FleetName,
        Sex=sexes,
        LBin=lbins[[stock]]$Mean_Size,
        catch=NA,
        land=NA)
    CALcomm[[stock]] <- CALcomm[[stock]][order(CALcomm[[stock]]$Year,
        CALcomm[[stock]]$Month,
        CALcomm[[stock]]$Fleet,CALcomm[[stock]]$Sex,
        CALcomm[[stock]]$LBin),]
      
    } # END dynamic stocks loop

    if (parallelr == TRUE && getDoParWorkers() > 1) {
        print("Running in parallel")
        tsout <- foreach(nn = 1:MCMC, 
            .packages = c("nwblastarca")) %dopar% Bio_loop(nn, simyears, firstyear, 
            dynamic.stocks, 
            spawnmonth, SSB, SSB0, NAA, ageinfo, recrmonths, Recr, BH_steep, 
            recrdev, recrfrac,  NAL, ALK, Mortalities, sexes, CALcomm, ages, 
            Fleets, past.trips.by.yearwaveareaboattype, areas, boat.types, 
            subareas, trip.types.by.area, catch.per.trip.prob.all, 
            length.max.legal.all, length.min.legal.all, length.weight.params, 
            lengths, modeled.stock.rec.catch.source, noncompliance.rate, 
            num.keep.legal.all, num.keep.legal.group.by.area, 
            opt.out.prob.by.area, rec.sel.at.length, release.mortality.rate, 
            stocks.model.pop.this.area, stocks.rec.catch.by.area, 
            trip.cost.by.area, trip.type.utility.coefs, 
            utility.catch.weight.coefs, utility.coefs, 
            utlity.catch.squared.term.names, utlity.catch.term.names,
            or.lingcod.bottomfish.catch.per.trip.model, 
            wa.lingcod.bottomfish.catch.per.trip.model, 
            wa.lingcod.salmon.catch.per.trip.model)
    } else {
        print("Running sequentially")
        tsout <- list()
        for (nn in 1:MCMC) {    
        tsout[[nn]] <- Bio_loop(nn, simyears, firstyear, dynamic.stocks, 
            spawnmonth, SSB, SSB0, NAA, ageinfo, recrmonths, Recr, BH_steep, 
            recrdev, recrfrac,  NAL, ALK, Mortalities, sexes, CALcomm, ages, 
            Fleets, past.trips.by.yearwaveareaboattype, areas, boat.types, 
            subareas, trip.types.by.area, catch.per.trip.prob.all, 
            length.max.legal.all, length.min.legal.all, length.weight.params, 
            lengths, modeled.stock.rec.catch.source, noncompliance.rate, 
            num.keep.legal.all, num.keep.legal.group.by.area, 
            opt.out.prob.by.area, rec.sel.at.length, release.mortality.rate, 
            stocks.model.pop.this.area, stocks.rec.catch.by.area, 
            trip.cost.by.area, trip.type.utility.coefs, 
            utility.catch.weight.coefs, utility.coefs, 
            utlity.catch.squared.term.names, utlity.catch.term.names,
            or.lingcod.bottomfish.catch.per.trip.model, 
            wa.lingcod.bottomfish.catch.per.trip.model, 
            wa.lingcod.salmon.catch.per.trip.model)
        }
    } 
    
    return(tsout)
    
}
