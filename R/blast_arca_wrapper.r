blast_arca_wrapper <- function(MCMC = 1, firstyear, forecastyrs, 
    dynamic.stocks) {
    #' blast_arca_wrapper
    #'
    #' Wrapper function for recreational bioeconomic model
    #'
    #' @param MCMC Number of monte carlo runs per scenario
    #' @param firstyear First year of simulation
    #' @param forecastyrs Number of forecast years
    #' @param dynamic.stocks Names of stocks that change over time
    #'
    #' @return tsout Time series of fisher catches, stock status, over forecast
    #' years
    #' @export
    #' @importFrom dplyr summarise group_by n
    #' @importFrom magrittr %>%
    #' @examples
    #'
    
    #Likely these should go as inputs, leave for now.
    # 1=female, 2=male
    sexes <- c(1,2)
    fishing.areas <- c("WA","OR")
    # C=commercial, R=recreational
    Fleets <- data.frame(FleetType=c("C","C","R","R"),          
        FleetArea=c("WA.OR","WA.OR","WA","OR"),
        FleetNum=1:4,
        FleetName=c("1_N_TRAWL","2_N_FIX","3_WA_REC","4_OR_REC"),
        stringsAsFactors=FALSE)
    
    #### Set up and read in data
    # Initialize dynamic stock variables

    stockvars <- init_dy_var(dynamic.stocks)
    list2env(stockvars, environment())
    
    # Load and process key data
    load(system.file("extdata", "Regulations.RData", package = "nwblastarca"))
    load(system.file("extdata", "CPT CAL.RData", package = "nwblastarca"))
    load(system.file("extdata", "Utility.RData", package = "nwblastarca"))
    load(system.file("extdata", "Structural.RData", package = "nwblastarca"))
    
    #sel here is wrong, need to change data
    rec.sel.at.length$PS.lingcod <- c(rec.sel.at.length$PS.lingcod, rep(0,6))

    areas <- names(stocks.rec.catch.by.area)
    subareas <- vector("list",length(areas))
    subareas <- lapply(areas, function(x) names(stocks.rec.catch.by.area[[x]]))
    names(subareas) <- areas

    simyears <- (firstyear):(firstyear+forecastyrs-1)
    simyearsplus <- (firstyear):(firstyear+forecastyrs)

    NAA <- NAL <- SSB <- Recr <- recrdev <- CALcomm <- 
        vector("list", length(dynamic.stocks))
    names(NAA) <- names(NAL) <- names(SSB) <- names(Recr) <- names(recrdev) <- 
        names(CALcomm) <- dynamic.stocks

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
    recrdev[[stock]] <- data.frame(Year=simyears,
        Dev={if(MCMC>1) {                 
        # ONLY USE IF RUNNING REPLICATE RUNS
        rnorm(length(simyears),mean=0,sd=sigmaR[[stock]])
        } else {rep(0,length(simyears))}})
    # END recrdevs draw
      
    # Set up commercial catches
    CALcomm[[stock]] <- expand.grid(Year=simyears,
        Month=1:12,
        Fleet=Fleets$FleetName[which(Fleets$FleetType=="C")],
        Sex=sexes,
        LBin=lbins[[stock]]$Mean_Size,
        catch=NA,
        land=NA)
    CALcomm[[stock]] <- CALcomm[[stock]][order(CALcomm[[stock]]$Year,
        CALcomm[[stock]]$Month,
        CALcomm[[stock]]$Fleet,CALcomm[[stock]]$Sex,
        CALcomm[[stock]]$LBin),]
      
    } # END dynamic stocks loop

    # But also for each stock's recreational catch
    for(area in areas) {
        for(subarea in subareas[[area]]) {
            for (allstock in stocks.rec.catch.by.area[[area]][[subarea]]) {
            # Set up rec catches for all species, dynamic and static
            CALrec[[area]][[subarea]][[allstock]] <- 
                expand.grid(Year=simyears,
                Month=1:12,
                Fleet=Fleets$FleetName[which(Fleets$FleetType=="R" & 
                    Fleets$FleetArea==area)],
                Area=area,
                Subarea=subarea,
                Sex=sexes,
                LBin=lengths[[allstock]],
                catch=NA,
                land=NA)
            } # END all stock loop
        } # END subarea loop
    } # END area loop

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
            #double check this is correct
            LnR0[[stock]] <- 8.11666
            
            recrage <- ifelse(currmonth>=spawnmonth[[stock]],0,1)            
            # THIS CODE SPECIFIC TO SS 3.30!
            Recr[[stock]]$Recr[which(Recr[[stock]]$Year==(curryear-recrage))] <-
                (4*BH_steep[[stock]]*exp(LnR0[[stock]])*
                SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] / 
                (SSB0[[stock]]*(1-BH_steep[[stock]])+
                SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] * 
                (5*BH_steep[[stock]]-1))) *
                exp(recrdev[[stock]]$Dev[
                which(recrdev[[stock]]$Year==curryear)])
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
                Mortalities[[stock]]$Sex==sexes[sex]),
                -c(1:4)])/12)
          
            # Record catches
            for (fleet in Fleets$FleetName) {
                Zdead <- Mortalities[[stock]][
                    which(Mortalities[[stock]]$Type=="Zdead" & 
                    Mortalities[[stock]]$Year==curryear & 
                    Mortalities[[stock]]$Sex==sexes[sex]),-c(1:4)]/12
                
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
                    Mortalities[[stock]]$Sex==sexes[sex]),
                    -c(1:4)]/12 * 
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
                    Mortalities[[stock]]$Sex==sexes[sex]),
                    -c(1:4)]/12 * (1-exp(-Zdead/12))/(Zdead/12)
            } # END fleet loop and catch recording
        } # END sex loop
    } # END dynamic stock loop
    ## End commercial fishing and natural mortality

    init.choices.by.waveareaboattype <- 100       

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
        
    for (wave in 1:6) {
        for (area in areas) {
            for (boattype in boat.types) {
            
                utility.all.subareas <- NULL
                utility.labels <- NULL

                for(subarea in subareas[[area]]) {
                                    
                    for (triptype in 
                        trip.types.by.area[[area]][[subarea]][[wave]]) {
                        
                        utility.all.choices <- NULL
                        
                        for (choice in 1:init.choices.by.waveareaboattype) {
                        # Call function to draw catch from trip and report trip 
                        #characteristics
                        trip.info <- TripDetails(curryear,
                            currmonth,
                            wave,
                            area,
                            subarea,
                            boattype,
                            boat.types,
                            triptype,
                            catch.per.trip.prob.all,
                            dynamic.stocks,
                            fishing.area.subareas,
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
                length(fishing.areas.subareas[[area]])+1)), 
                c(fishing.areas.subareas[[area]],"Opt.Out"))
            fishing.trips <- 0
            
            for(i in fishing.areas.subareas[[area]]) { # for each sub-area
    
            # Create a list within the list to hold the trips taken by each trip 
            #type for this sub-area
            total.trips.predicted[[i]] <- setNames(as.list(
                rep(NA, length(trip.types.by.area[[area]][[subarea]][[wave]]))), 
                trip.types.by.area[[area]][[subarea]][[wave]])
    
            for(j in trip.types.by.area[[area]][[subarea]][[wave]]) { 
            # for each trip type
      
            # Identify the column of trip.probs with data for this sub-area and 
            #trip type
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
            
            } # END boattype loop
        } # END area loop
    } # END wave loop
    
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
    ## Calculate recreational catches (for all species) and mortality 
    #(for each dynamic species) MORE EFFICIENT?
    # Only at end of wave to avoid duplication
    if(waveT == (currmonth/2)) {
        for(area in fishing.areas) {
        
        savedyncount <- 1

            for (boattype in boat.types) {
            
                choiceiters <- round(choice.occasions$Choices[
                    choice.occasions$Wave == waveT & 
                    choice.occasions$Area == area &
                    choice.occasions$BoatType == boattype]/
                    length(sapply(trip.types.by.area[[area]], "[[", waveT)))
                
                for (subarea in subareas[[area]]) {
                    for (triptype in 
                    trip.types.by.area[[area]][[subarea]][[waveT]]) {
                    
                        savecatch <- NULL
                        saveutil <- NULL
                        
                        if (choice.occasions$Choices[
                        choice.occasions$Wave == waveT & 
                        choice.occasions$Area == area &
                        choice.occasions$BoatType == boattype] != 0) {                   
                        for (choice in 1:choiceiters) { 
                        # Call function to draw catch from trip and report trip 
                        #characteristics
                        trip.info <- TripDetails(curryear,
                            currmonth,
                            waveT,
                            area,
                            subarea,
                            boattype,
                            boat.types,
                            triptype,
                            catch.per.trip.prob.all,
                            dynamic.stocks,
                            fishing.area.subareas,
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
    
    dyncatch <- data.frame(do.call(cbind, unlist(savedyn, recursive = FALSE)))
    colnames(dyncatch) <- c("LBin", "Sex")

    #NAL is in thousands! Because from SS
    dynremove <- data.frame(dyncatch %>%
        group_by(LBin, Sex) %>%
        summarise(count = n()/1000))    
        
    } # END end of wave branch
    ## End recreational catch section
    
    if (length(dynremove) > 0) {
    
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
    print(paste(curryear, currmonth))
    
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
        dynremove
    
    } ## End time loop
    
    tsout <- list(NAA, dynremoveout)
    
    return(tsout)
    
}
