Bio_loop <- function(MCMC, simyears, firstyear, dynamic.stocks, spawnmonth, SSB, 
    SSB0, NAA, ageinfo, recrmonths, Recr, BH_steep, recrdev, recrfrac, NAL, ALK, 
    Mortalities, sexes, CALcomm, ages, Fleets, 
    past.trips.by.yearwaveareaboattype, areas, boat.types, subareas, 
    trip.types.by.area, catch.per.trip.prob.all, length.max.legal.all, 
    length.min.legal.all, length.weight.params, lengths, 
    modeled.stock.rec.catch.source, noncompliance.rate, num.keep.legal.all, 
    num.keep.legal.group.by.area, opt.out.prob.by.area, rec.sel.at.length, 
    release.mortality.rate, stocks.model.pop.this.area, 
    stocks.rec.catch.by.area, trip.cost.by.area, trip.type.utility.coefs, 
    utility.catch.weight.coefs, utility.coefs, 
    utlity.catch.squared.term.names, utlity.catch.term.names,
    or.lingcod.bottomfish.catch.per.trip.model, 
    wa.lingcod.bottomfish.catch.per.trip.model, 
    wa.lingcod.salmon.catch.per.trip.model) {
    #' Bio_loop
    #'
    #' Simulate bioeconomic model across years 
    #'
    #' @param MCMC Monte Carlo iteration
    #' @param simyears Years for simulation
    #' @param firstyear First year of simulation
    #' @param dynamic.stocks Names of stocks that change over time
    #' @param spawnmonth List of spawning months for each stock
    #' @param SSB Initialized spawning biomass for each stock
    #' @param SSB0 Unfished spawning biomass parameter for each stock
    #' @param NAA Numbers at age to be simulated for each stock
    #' @param ageinfo Age information for each stock
    #' @param recrmonths Months of recruitment for each stock
    #' @param Recr Recruitment for each stock
    #' @param BH_steep Recruitment steepness parameter
    #' @param recrdev Recruitment deviations for Monte Carlo runs
    #' @param recrfrac Recruitment fraction for sexes
    #' @param NAL Numbers at length to be simulated for each stock
    #' @param ALK Age length key
    #' @param Mortalities Mortalities for each stock
    #' @param sexes Sexes
    #' @param CALcomm Commercial catches to be simulated
    #' @param ages Ages that exist for each stock
    #' @param Fleets Fleets that exist
    #' @param past.trips.by.yearwaveareaboattype Historical trip data
    #' @param areas List of areas to fish
    #' @param boat.types List of boat types (charter or private)
    #' @param subareas List of subareas
    #' @param trip.types.by.area List of trip types by area
    #' @param catch.per.trip.prob.all Probability of number of fish caught per 
    #' trip
    #' @param length.max.legal.all Maximum length of retention (by stock)
    #' @param length.min.legal.all Minimum length of retention (by stock)
    #' @param length.weight.params Length-weight conversion parameters
    #' @param lengths Length bins for dynamic stocks
    #' @param modeled.stock.rec.catch.source Mapping of sub-area stocks to 
    #' dynamic models
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
    #' @return outlist List of stock status and catches
    #' @export
    #' @examples
    #' 
  
    # Create objects to hold the sub-areas and trip types associated with each 
    # utility vector and trips taken
    utility.all.subareas <- NULL
    utility.labels <- NULL
    dynremoveout <- NULL
    
    ## Sub loop: time
    #Define time references
    for(timestep in 1:(12*length(simyears))) {
        curryear <- firstyear+floor((timestep-1)/12)
        nextyear <- firstyear+floor(timestep/12)
        currmonth <- timestep - 12*floor((timestep-1)/12)
        nextmonth <- timestep - 12*floor(timestep/12) + 1
        waveT <- ceiling(currmonth/2)

    ## Branch: calculate SSB if spawning season (for each dynamic species)
    for(stock in dynamic.stocks) {
        if(currmonth==spawnmonth[[stock]]) {
            SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] <- 0
            for (age in ages[[stock]]) {
                SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] <- 
                    SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] +
                    unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==curryear & 
                    NAA[[stock]]$Month==currmonth & 
                    NAA[[stock]]$Age==age & NAA[[stock]]$Sex==sexes[1])]) *
                    ageinfo[[stock]]$"Mat*Fecund"[
                    which(ageinfo[[stock]]$Sex==1 & 
                    ageinfo[[stock]]$Real_Age==age)]
            } # END age loop
        } # END if spawning month
      } # END dynamic stock loop
    ## End spawning season branch

    ## Branch: add recruits if recruitment season (for each dynamic species)
    for(stock in dynamic.stocks) {
        if(currmonth %in% recrmonths[[stock]] & timestep!=1) {
            
            LnR0 <- NULL
            #double check this is correct. Get rid of hard code.
            LnR0[[stock]] <- 9.0669
            
            recrage <- ifelse(currmonth>=spawnmonth[[stock]],0,1)            
            # THIS CODE SPECIFIC TO SS 3.30!
            Recr[[stock]]$Recr[which(Recr[[stock]]$Year==(curryear-recrage))] <-
                (4*BH_steep[[stock]]*exp(LnR0[[stock]])*
                SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] / 
                (SSB0[[stock]]*(1-BH_steep[[stock]])+
                SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] * 
                (5*BH_steep[[stock]]-1))) *
                exp(recrdev[[MCMC]][[stock]]$Dev[
                which(recrdev[[MCMC]][[stock]]$Year==curryear)])
            for (sex in sexes) {
                NAA[[stock]]$N[which(NAA[[stock]]$Year==curryear & 
                NAA[[stock]]$Month==currmonth & 
                NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==recrage)] <- 
                    recrfrac[[stock]]$Prop[which(recrfrac[[stock]]$Sex==sex)] * 
                    Recr[[stock]]$Recr[
                    which(Recr[[stock]]$Year==(curryear-recrage))] / 
                    length(recrmonths[[stock]]) + 
                    unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==curryear & 
                    NAA[[stock]]$Month==currmonth & 
                    NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==recrage)])
            } # END sex loop
        } # END if recruitment month
    } # END dynamic stock loop
    ## End recruitment season branch

    ## Convert population numbers from age- to length-classes 
    # (for each dynamic species)
    for(stock in dynamic.stocks) {
        for(sex in sexes) {
          NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & 
          NAL[[stock]]$Month==currmonth & 
          NAL[[stock]]$Sex==sexes[sex])] <- 
            unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==curryear & 
            NAA[[stock]]$Month==currmonth & 
            NAA[[stock]]$Sex==sexes[sex])]) %*% 
            t(ALK[[stock]][which(ALK[[stock]]$Subseason==2 & 
            ALK[[stock]]$Sex==sex),-c(1:3)])
        } # END sex loop
    } # END dynamic stock loop
    ## End population conversion age to length

    ## Calculate commercial fishing and natural mortality, adjust NAL 
    # (for each dynamic species)
    for(stock in dynamic.stocks) {
        for (sex in sexes) {
            # Natural mortality
            NAL[[stock]]$N[which(NAL[[stock]]$Year==nextyear & 
            NAL[[stock]]$Month==nextmonth & 
            NAL[[stock]]$Sex==sexes[sex])] <-
                unlist(NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & 
                NAL[[stock]]$Month==currmonth & 
                NAL[[stock]]$Sex==sexes[sex])]) *
                exp(-Mortalities[[stock]][which(Mortalities[[stock]]$Type=="M" & 
                Mortalities[[stock]]$Sex==sexes[sex]),-c(1:4)]/12)
          
            # Commercial fishing
            NAL[[stock]]$N[which(NAL[[stock]]$Year==nextyear & 
            NAL[[stock]]$Month==nextmonth & 
            NAL[[stock]]$Sex==sexes[sex])] <-
                unlist(NAL[[stock]]$N[which(NAL[[stock]]$Year==nextyear & 
                NAL[[stock]]$Month==nextmonth & 
                NAL[[stock]]$Sex==sexes[sex])]) *
                exp(-colSums(Mortalities[[stock]][
                which(Mortalities[[stock]]$Type=="Fdead" & 
                Mortalities[[stock]]$Fleet %in% 
                Fleets$FleetName[which(Fleets$FleetType=="C")] &
                Mortalities[[stock]]$Year==curryear & 
                Mortalities[[stock]]$Sex==sexes[sex]), -c(1:4)])/12)

            # Record catches
            for (fleet in Fleets$FleetName) {
                Zdead <- (Mortalities[[stock]][which(
                Mortalities[[stock]]$Type=="M" & 
                Mortalities[[stock]]$Sex==sexes[sex]),-c(1:4)]) +
                (colSums(Mortalities[[stock]][
                which(Mortalities[[stock]]$Type=="Fdead" & 
                Mortalities[[stock]]$Fleet %in% 
                Fleets$FleetName[which(Fleets$FleetType=="C")] &
                Mortalities[[stock]]$Year==curryear & 
                Mortalities[[stock]]$Sex==sexes[sex]), -c(1:4)]))
                
                CALcomm[[stock]]$catch[which(CALcomm[[stock]]$Year==curryear & 
                CALcomm[[stock]]$Month==currmonth &
                CALcomm[[stock]]$Fleet==fleet & 
                CALcomm[[stock]]$Sex==sexes[sex])] <- 
                    NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & 
                    NAL[[stock]]$Month==currmonth & 
                    NAL[[stock]]$Sex==sexes[sex])] *
                    Mortalities[[stock]][
                    which(Mortalities[[stock]]$Type=="Fctch" & 
                    Mortalities[[stock]]$Fleet==fleet & 
                    Mortalities[[stock]]$Year==curryear &
                    Mortalities[[stock]]$Sex==sexes[sex]), -c(1:4)]/12 * 
                    (1-exp(-Zdead/12))/(Zdead/12)
                
                CALcomm[[stock]]$land[which(CALcomm[[stock]]$Year==curryear & 
                CALcomm[[stock]]$Month==currmonth &
                CALcomm[[stock]]$Fleet==fleet & 
                CALcomm[[stock]]$Sex==sexes[sex])] <- 
                    NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & 
                    NAL[[stock]]$Month==currmonth & 
                    NAL[[stock]]$Sex==sexes[sex])] *
                    Mortalities[[stock]][
                    which(Mortalities[[stock]]$Type=="Fland" & 
                    Mortalities[[stock]]$Fleet==fleet &
                    Mortalities[[stock]]$Year==curryear &
                    Mortalities[[stock]]$Sex==sexes[sex]), -c(1:4)]/12 * 
                    (1-exp(-Zdead/12))/(Zdead/12)
            } # END fleet loop and catch recording
        } # END sex loop
    } # END dynamic stock loop
    ## End commercial fishing and natural mortality

    init.choices.by.waveareaboattype <- 100       
browser()
    ## At the beginning of each year, calculate recreational trips by wave, 
    # area, boattype, subarea, and triptype
    if(currmonth==1) {

    # Create data frame to hold the number of choice occasions by Year, Wave,  
    # Area, and BoatType as well as number of trips
    choice.occasions <- cbind(past.trips.by.yearwaveareaboattype, 
    Choices=rep(0,nrow(past.trips.by.yearwaveareaboattype)))
    
    # Adjust the year so 1 is most recent, etc
    choice.occasions$Year <- max(choice.occasions$Year) - 
        choice.occasions$Year + 1
    
    predsave <- NULL
    
    for (wave in 1:6) {
        for (area in areas) {
            for (boattype in boat.types) {
            
                utility.all.subareas <- NULL
                utility.labels <- NULL

                for(subarea in subareas[[area]]) {
                                    
                    for (triptype in 
                        trip.types.by.area[[area]][[subarea]][[wave]]) {
                        
                        utility.all.choices <- NULL
                        
                        # #fortesting
                        # print(paste(wave, area, boattype, subarea, triptype, 
                        # sep = " "))
                        for (choice in 1:init.choices.by.waveareaboattype) {
                        # Call function to draw catch from trip and report trip 
                        #characteristics
                        trip.info <- TripDetails(choice,curryear,
                            currmonth,
                            wave,
                            area,
                            subarea,
                            boattype,
                            boat.types,
                            triptype,
                            catch.per.trip.prob.all,
                            dynamic.stocks,
                            length.max.legal.all,
                            length.min.legal.all,
                            length.weight.params,
                            lengths,
                            modeled.stock.rec.catch.source,
                            NAL,
                            noncompliance.rate,
                            num.keep.legal.all,
                            num.keep.legal.group.by.area,
                            opt.out.prob.by.area,
                            rec.sel.at.length,
                            release.mortality.rate,
                            stocks.model.pop.this.area,
                            stocks.rec.catch.by.area,
                            trip.cost.by.area,
                            trip.type.utility.coefs,
                            trip.types.by.area,
                            utility.catch.weight.coefs,
                            utility.coefs,
                            utlity.catch.squared.term.names,
                            utlity.catch.term.names,
                            or.lingcod.bottomfish.catch.per.trip.model,
                            wa.lingcod.bottomfish.catch.per.trip.model,
                            wa.lingcod.salmon.catch.per.trip.model)
                        
                        # Save utility for this trip
                        utility.all.choices <- c(utility.all.choices, 
                            trip.info$utility)

                        } # END choice loop

                    utility.all.subareas[[subarea]][[triptype]] <- 
                        utility.all.choices
                    utility.labels[[subarea]][[triptype]] <- 
                        c(subarea, triptype)

                    } # END triptype loop

                } # END subarea loop
            
            utility.all.subareas <- matrix(unlist(utility.all.subareas), 
                init.choices.by.waveareaboattype, 
                length(utility.all.subareas))

            opt.out.utility.all <- sapply(boattype, CalculateUtility, 
                opt.out = TRUE, 
                utility.coefs = utility.coefs, 
                opt.out.prob = opt.out.prob.by.area[[area]],
                simplify = TRUE, USE.NAMES = FALSE)
             
            utility.all.subareas <- cbind(utility.all.subareas,
                opt.out.utility.all)
            
            utility.labels <- matrix(unlist(utility.labels), 
                2, 
                length(utility.labels))
                
            utility.labels <- cbind(utility.labels, 'Opt.out')
            trip.probs <- exp(utility.all.subareas)/
                rowSums(exp(utility.all.subareas))
                
            total.trips.predicted <- setNames(as.list(rep(NA, 
                length(subareas[[area]])+1)), 
                c(subareas[[area]],"Opt.Out"))
            fishing.trips <- 0
            
            for(i in subareas[[area]]) { # for each sub-area
    
            # Create a list within the list to hold the trips taken by each trip 
            #type for this sub-area
            total.trips.predicted[[i]] <- setNames(as.list(
                rep(NA, length(trip.types.by.area[[area]][[subarea]][[wave]]))), 
                trip.types.by.area[[area]][[subarea]][[wave]])
    
            for(j in trip.types.by.area[[area]][[subarea]][[wave]]) { 
            # for each trip type
            # Identify the column of trip.probs with data for this sub-area and 
            # trip type
            trip.probs.col <- which(utility.labels[1,]==i & 
                utility.labels[2,]==j)
      
            # Save the predicted number of trips
            total.trips.predicted[[i]][[j]] <- round(sum(trip.probs[, 
                trip.probs.col]), digits=0)
            fishing.trips <- fishing.trips + total.trips.predicted[[i]][[j]]

            } # END trip type loop
            } # END sub-area loop
  
            total.trips.predicted$Opt.Out <- init.choices.by.waveareaboattype - 
                fishing.trips
            #use 2014 most recent year to extrapolate
            year <- 1
            choice.index <- which(choice.occasions$Year==year & 
                choice.occasions$Wave==wave & 
                choice.occasions$Area==area & 
                choice.occasions$BoatType==boattype)
            choice.occasions$Choices[choice.index] <- 
                (sum(unlist(total.trips.predicted)) - 
                total.trips.predicted$Opt.Out)/
                sum(unlist(total.trips.predicted))
            
            predsave[[as.character(wave)]][[area]][[boattype]] <- 
                total.trips.predicted
            
            } # END boattype loop
        } # END area loop
    } # END wave loop
    
    #This end block finds number of choice occasions given that some end up in
    #opting out, e.g. there are more choices than trips because some ended up
    #opting out
    choice.years <- 1
    # Convert proportion of each activity into numbers using total trips taken
    choice.occasions$Choices[which(choice.occasions$Year %in% 
        1:choice.years)] <- 
    round(choice.occasions$Trips[which(choice.occasions$Year %in% 
        1:choice.years)]/
        choice.occasions$Choices[
        which(choice.occasions$Year %in% 1:choice.years)],0)
    
    choice.occasions <- choice.occasions[
        which(choice.occasions$Year<=choice.years),
        -which(names(choice.occasions)=="Trips")]

    choice.occasions <- aggregate(list(Choices=choice.occasions$Choices), 
        by=list(Wave=choice.occasions$Wave, 
        Area=choice.occasions$Area, 
        BoatType=choice.occasions$BoatType), 
        FUN=mean, na.rm=TRUE)

    choice.occasions.choices <- choice.occasions[order(choice.occasions$Wave, 
        -xtfrm(choice.occasions$Area), 
        choice.occasions$BoatType),]
    } # END beginning of year branch
    ## End trip calculations

    savedyn <- NULL
    saveall <- NULL
    dyncatch <- NULL
    dynremove <- NULL
    dynremovesave <- NULL

    ## Calculate recreational catches (for all species) and mortality 
    #(for each dynamic species) MORE EFFICIENT?
    # Only at end of wave to avoid duplication
    if(waveT == (currmonth/2)) {
        for(area in areas) {
        
        savedyncount <- 1

            for (boattype in boat.types) {
            
                choiceiters <- choice.occasions$Choices[
                    choice.occasions$Wave == waveT & 
                    choice.occasions$Area == area &
                    choice.occasions$BoatType == boattype]
                
                for (subarea in subareas[[area]]) {
                    for (triptype in 
                    trip.types.by.area[[area]][[subarea]][[waveT]]) {
                    
                        savecatch <- NULL
                        saveutil <- NULL
                        
                        choiceitersn <- round(choiceiters*predsave[[waveT]][[
                            area]][[boattype]][[subarea]][[triptype]]/(100-
                            predsave[[waveT]][[area]][[boattype]]$Opt.Out))
    
                        if (choiceitersn != 0) {
print(choiceitersn)                        
                        for (choice in 1:choiceitersn) { 
                        # Call function to draw catch from trip and report trip 
                        #characteristics
                        trip.info <- TripDetails(choice,curryear,
                            currmonth,
                            waveT,
                            area,
                            subarea,
                            boattype,
                            boat.types,
                            triptype,
                            catch.per.trip.prob.all,
                            dynamic.stocks,
                            length.max.legal.all,
                            length.min.legal.all,
                            length.weight.params,
                            lengths,
                            modeled.stock.rec.catch.source,
                            NAL,
                            noncompliance.rate,
                            num.keep.legal.all,
                            num.keep.legal.group.by.area,
                            opt.out.prob.by.area,
                            rec.sel.at.length,
                            release.mortality.rate,
                            stocks.model.pop.this.area,
                            stocks.rec.catch.by.area,
                            trip.cost.by.area,
                            trip.type.utility.coefs,
                            trip.types.by.area,
                            utility.catch.weight.coefs,
                            utility.coefs,
                            utlity.catch.squared.term.names,
                            utlity.catch.term.names,
                            or.lingcod.bottomfish.catch.per.trip.model,
                            wa.lingcod.bottomfish.catch.per.trip.model,
                            wa.lingcod.salmon.catch.per.trip.model)
                        
                            if (choice == 1) {
                            savecatch <- unlist(trip.info$catch, 
                                recursive = FALSE)
                            } else {
                            savecatch <- mapply(c, savecatch, 
                                unlist(trip.info$catch, recursive = FALSE),
                                SIMPLIFY=FALSE)   
                            }
                            
                            if (savedyncount == 1) {
                            savedyn[[area]] <- unlist(trip.info$catch, 
                                recursive = FALSE)[
                                grepl(paste(sub('.*\\.', '', dynamic.stocks), 
                                "kept", sep = "."), names(savecatch))]
                            } else {  
                            savedyn[[area]] <- mapply(c, savedyn[[area]], 
                                unlist(trip.info$catch, recursive = FALSE)[
                                grepl(paste(sub('.*\\.', '', dynamic.stocks), 
                                "kept", sep = "."), 
                                names(unlist(trip.info$catch, 
                                    recursive = FALSE)))],
                                SIMPLIFY=FALSE)
                            }

                            saveutil <- cbind(saveutil, trip.info$utility)
                            savedyncount <- savedyncount + 1
                            
                        } # END choice loop
                        } 
                    
                saveall[[area]][[boattype]][[subarea]][[triptype]] <- 
                    list(savecatch, saveutil)
                    
                    } # END triptype loop
                } # END subarea loop
            } # END boattype loop
        } # END area loop
        # Update NAL for dynamic stocks

    for(area in areas) {
    tempcat <- list()
    tempcat[[area]] <- savedyn[[area]]
    if (is.null(do.call(cbind, unlist(tempcat, recursive = FALSE))) == TRUE) {
    dyncatch[[area]] <- NULL    
    } else{
    dyncatch[[area]] <- data.frame(do.call(cbind, unlist(tempcat, 
        recursive = FALSE)))
    colnames(dyncatch[[area]]) <- c("LBin", "Sex")
    }
    }
    dyncatch <- do.call(rbind,dyncatch)

    #NAL is in thousands! Because from SS
    dynremove <- data.frame(dyncatch %>%
        group_by(LBin, Sex) %>%
        summarise(count = n()/1000))    
        
    } # END end of wave branch
    ## End recreational catch section
    
    if (length(dynremove) > 0) {
    
    dynremovesave <- dynremove
    dynremovesave$Year <- curryear
    dynremovesave$Month <- currmonth
    dynremovesave$count <- dynremovesave$count*1000
    
    #already in nextmonth    
    dynremove$Year <- nextyear
    dynremove$Month <- nextmonth
         
    NAL[[stock]] <- merge(NAL[[stock]], dynremove, 
        by = c("Year", "Month", "Sex", "LBin"),
        all.x = TRUE, all.y = FALSE)
  
    NAL[[stock]]$count[is.na(NAL[[stock]]$count)] <- 0
    
    NAL[[stock]]$N <- as.list(unlist(NAL[[stock]]$N) - NAL[[stock]]$count)
    
    NAL[[stock]]$count <- NULL
    
    }
    
    # if (currmonth == 12) {
    # browser()
    # }
    # print(paste(curryear, currmonth))
    
    ## Convert population numbers from length- to age-classes 
    #(for each dynamic species)
    for(stock in dynamic.stocks) {
        for (sex in sexes) {
            NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & 
                NAA[[stock]]$Month==nextmonth & 
            NAA[[stock]]$Sex==sexes[sex])] <- 
                unlist(NAL[[stock]]$N[which(NAL[[stock]]$Year==nextyear & 
                NAL[[stock]]$Month==nextmonth & 
                NAL[[stock]]$Sex==sexes[sex])]) %*% 
                as.matrix(GenLengthAgeKey(numbersatage=
                    unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==curryear & 
                NAA[[stock]]$Month==currmonth & NAA[[stock]]$Sex==sexes[sex])]),
                ALKey=ALK[[stock]][which(ALK[[stock]]$Subseason==2 & 
                ALK[[stock]]$Sex==sexes[sex]),-c(1:2)])[,-1])
        } # END sex loop
      } # END dynamic stock loop
    ## End population conversion length to age

    ## Branch: age if end of year (for each dynamic species)
    for(stock in dynamic.stocks) {
        if(currmonth==12) {
            for (sex in sexes) {
            # Address plus age group
            NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & 
            NAA[[stock]]$Month==nextmonth & 
            NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==max(ages[[stock]]))] <- 
                unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & 
                NAA[[stock]]$Month==nextmonth & 
                NAA[[stock]]$Sex==sex & 
                NAA[[stock]]$Age==max(ages[[stock]]))]) +
                unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & 
                NAA[[stock]]$Month==nextmonth & 
                NAA[[stock]]$Sex==sex & 
                NAA[[stock]]$Age==(max(ages[[stock]])-1))])
            for (age in rev(ages[[stock]][-c((length(ages[[stock]])-1),
                length(ages[[stock]]))])) {
            NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & 
            NAA[[stock]]$Month==nextmonth & 
            NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==(age+1))] <- 
            unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & 
            NAA[[stock]]$Month==nextmonth &
            NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==age)])
            } # END age loop
            # Zero out 0-age fish
            NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & 
            NAA[[stock]]$Month==nextmonth & 
            NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==ages[[stock]][1])] <- 0
            } # END sex loop
        } # END end of year branch
    } # END dynamic stock loop
    ## End ageing branch

    dynremoveout[[as.character(curryear)]][[as.character(currmonth)]] <- 
        dynremovesave
    
    } ## End time loop
    
    NAA[[dynamic.stocks]]$N <- unlist(NAA[[dynamic.stocks]]$N)
    NAA[[dynamic.stocks]] <- NAA[[dynamic.stocks]][NAA[[dynamic.stocks]]$Year <= 
        simyears[length(simyears)],]

    outlist <- list(NAA, dynremoveout, CALcomm)
    
    return(outlist)

}
