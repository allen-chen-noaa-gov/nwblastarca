##############################################################################################################
##############################################################################################################
####                          Angler Regulation-to-Catch Assessment (ARCA) Tool                           ####
## Description ##
##                                                                                                          ##
## Creator:   Josh Nowlis                                                                                   ##
## Created:   16 May 2018                                                                                   ##
## Version:   1.0                                                                                           ##
## ModDate:   13 Aug 2018                                                                                   ##
##  Tested:   Y                                                                                             ##
##  Issues:   in development...                                                                             ##
##                                                                                                          ##
## Inputs (all in source files):                                                                            ##
##         * Catch-per-trip probability distributions for static species                                    ##
##         * Catch-per-rtip models, one for each dynamic species                                            ##
##         * Current regulations                                                                            ##
##         * Utility parameters from discrete choice surveys                                                ##
##         * A few biological data (e.g., length-weight conversions) for all species                        ##
##         * Complete biological data from SS for each dynamic species                                      ##
##                                                                                                          ##
## Outputs (graphs as png files and tables as csv files):                                                   ##
##         * DETAILS                                                                                        ##
####                                                                                                      ####
##############################################################################################################
##############################################################################################################

##############################################################################################################
#### User defined parameters
# Choose management scenarios
##@@DO IT
# scenarios <- 1

# Identify dynamic species
dynamic.stocks <- c('WA.OR.lingcod')              # c(..., "WA.black.rockfish", "OR.black.rockfish")

# Other model choices and structure
MCMC <- 1                                         # Monte Carlo runs per scenario, 1 = not stochastic
firstyear <- 2017                                 # First year of forecasting (>= assessment year+1)
forecastyrs <- 5                                  # Number of years of forecasting
sexes <- c(1,2)                                             # 1=female, 2=male
#Sexes <- data.frame(num=c(1,2),
#                    name=c("F","M"))
fishing.areas <- c("WA","OR")
Fleets <- data.frame(FleetType=c("C","C","R","R"),          # C=commercial, R=recreational
                     FleetArea=c("WA.OR","WA.OR","WA","OR"),
                     FleetNum=1:4,
                     FleetName=c("1_N_TRAWL","2_N_FIX","3_WA_REC","4_OR_REC"),
                     stringsAsFactors=FALSE)

# Set working directory
rootdir <- "U:\\NWFSC_data_code\\BLAST\\" 
#rootdir <- "C:/Users/jsnow/Documents/NWFSC"       # JOSH'S LAPTOP
#rootdir <- "G:"                                   # JOSH'S MEMORY STICK / HOME DESKTOP
# rootdir <- file.path(rootdir, "Angler regulation to catch assessment tool/NW Code")
setwd(rootdir)

# Set random number seed
set.seed(16052018)

# Update user on process and set beginning time
print("Initializing program and gathering data from data files...")
elapsed.time <- proc.time()        # start timer for runs
split.time <- elapsed.time         # split timer set to 0 for each scenario

##############################################################################################################
#### Libraries
library(pscl)                 # used for zeroinfl catch per trip stats models >> install.packages("pscl") <<
library(ggplot2)              # handy graphing package >> install.packages("ggplot2") <<
library(plyr)                 # handy reshaping package used to generate ALK >> install.packages("plyr") <<
library(data.table)           # handy data table package (ALK) >> install.packages("data.table") <<

##############################################################################################################
#### Set up and read in data
# Initialize dynamic stock variables

ageinfo <- vector("list", length(dynamic.stocks))
names(ageinfo) <- dynamic.stocks
ages <- vector("list", length(dynamic.stocks))
names(ages) <- dynamic.stocks
ALK <- vector("list", length(dynamic.stocks))
names(ALK) <- dynamic.stocks
assessmentyear <- vector("list", length(dynamic.stocks))
names(assessmentyear) <- dynamic.stocks
BH_steep <- vector("list", length(dynamic.stocks))
names(BH_steep) <- dynamic.stocks
lbins <- vector("list", length(dynamic.stocks))
names(lbins) <- dynamic.stocks
Mortalities <- vector("list", length(dynamic.stocks))
names(Mortalities) <- dynamic.stocks
popsize <- vector("list", length(dynamic.stocks))
names(popsize) <- dynamic.stocks
recrfrac <- vector("list", length(dynamic.stocks))
names(recrfrac) <- dynamic.stocks
recrmonths <- vector("list", length(dynamic.stocks))
names(recrmonths) <- dynamic.stocks
sigmaR <- vector("list", length(dynamic.stocks))
names(sigmaR) <- dynamic.stocks
spawnmonth <- vector("list", length(dynamic.stocks))
names(spawnmonth) <- dynamic.stocks
SSB0 <- vector("list", length(dynamic.stocks))
names(SSB0) <- dynamic.stocks
WeightConversion <- vector("list", length(dynamic.stocks))
names(WeightConversion) <- dynamic.stocks

# Load and process key data
load("Structural.RData")
areas <- names(stocks.rec.catch.by.area)
subareas <- vector("list",length(areas))
subareas <- lapply(areas, function(x) names(stocks.rec.catch.by.area[[x]]))
names(subareas) <- areas

# load("CPT CAL.RData")
# load("Data/Regulations.RData")
# load("Data/Utility.RData")

# Read in and reformat data for dynamic stocks
for(stock in dynamic.stocks) {
  load(paste(stock, ".RData", sep=""))
  # Convert dynamic stock data into common lists
  ageinfo[[stock]] <- get(stock)$ageinfo
  ages[[stock]] <- get(stock)$ages
  ALK[[stock]] <- get(stock)$ALK
  assessmentyear[[stock]] <- get(stock)$assessmenthyear
  BH_steep[[stock]] <- get(stock)$BH_steep
  lbins[[stock]] <- get(stock)$lbins
  Mortalities[[stock]] <- get(stock)$Mortalities
  popsize[[stock]] <- get(stock)$popsize
  recrfrac[[stock]] <- get(stock)$recrfrac
  recrmonths[[stock]] <- get(stock)$recrmonths
  sigmaR[[stock]] <- get(stock)$sigmaR
  spawnmonth[[stock]] <- get(stock)$spawnmonth
  SSB0[[stock]] <- get(stock)$SSB0
  WeightConversion[[stock]] <- get(stock)$WeightConversion
} # END dynamic stock loop

##############################################################################################################
#### Read in functions
source("ARCA Functions.r")

##############################################################################################################
#### Initialize derived data structures, including results
# mytheme <- theme(aspect.ratio=2/2,
                 # plot.title=element_text(size=21, face="bold"),
                 # axis.title.x=element_text(size=18),
                 # axis.title.y=element_text(size=18),
                 # axis.text.x=element_text(size=15),
                 # axis.text.y=element_text(size=15),
                 # legend.background=element_rect(fill="#EEEEEE",linetype="solid",color="black"),
                 # legend.title=element_blank(),
                 # legend.text=element_text(size=15))
# topright <- theme(legend.position=c(0.85,0.85))
# bottomleft <- theme(legend.position=c(0.2,0.2))

simyears <- (firstyear):(firstyear+forecastyrs-1)
simyearsplus <- (firstyear):(firstyear+forecastyrs)

NAA <- NAL <- SSB <- Recr <- recrdev <- CALcomm <- vector("list", length(dynamic.stocks))
names(NAA) <- names(NAL) <- names(SSB) <- names(Recr) <- names(recrdev) <- names(CALcomm) <- dynamic.stocks

CALrec <- vector("list",length(areas))
names(CALrec) <- areas
for (area in areas) {
  CALrec[[area]] <- vector("list",length(subareas[[area]]))
  names(CALrec[[area]]) <- subareas[[area]]
  for (subarea in subareas[[area]]) {
    CALrec[[area]][[subarea]] <- vector("list",length(stocks.rec.catch.by.area[[area]][[subarea]]))
    names(CALrec[[area]][[subarea]]) <- stocks.rec.catch.by.area[[area]][[subarea]]
  }
}

num.available.to.rec.fishery <- len.pop.male <- len.pop.female <- vector("list", length(dynamic.stocks))
names(num.available.to.rec.fishery) <- names(len.pop.female) <- names(len.pop.male) <- dynamic.stocks

##############################################################################################################
#### Main loop: scenarios
#for (scenario in scenarios) {
#  print("Starting a scenario...")
  ##@@!!@@## ADD SCENARIOS ##@@!!@@##
  
####################@@..................................................................@@####################
## Sub loop: MCMC. To turn off, set MCMC to 1. Otherwise MCMC is number of runs
#  for(mcrun in 1:MCMC) {
#    print(paste("Starting Monte Carlo run",mcrun,"..."))

##@@......................................................................................................@@##
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
      NAA[[stock]] <- NAA[[stock]][order(NAA[[stock]]$Year,NAA[[stock]]$Month,NAA[[stock]]$Sex,NAA[[stock]]$Age),]
      NAL[[stock]] <- NAL[[stock]][order(NAL[[stock]]$Year,NAL[[stock]]$Month,NAL[[stock]]$Sex,NAL[[stock]]$LBin),]
      
      # Set initial population sizes
      for(sex in sexes) {
        NAA[[stock]]$N[which(NAA[[stock]]$Year==firstyear & NAA[[stock]]$Sex==sexes[sex] & 
                               NAA[[stock]]$Month==1)] <- 
          popsize[[stock]][which(popsize[[stock]]$Time==firstyear & popsize[[stock]]$Sex==sexes[sex]),
                           11:ncol(popsize[[stock]])]           # 11-end cols are actual age-structured popsize
      }
      
      # Draw random recruitment deviations
      recrdev[[stock]] <- data.frame(Year=simyears,
                                     Dev={if(MCMC>1) {                 # ONLY USE IF RUNNING REPLICATE RUNS
                                       rnorm(length(simyears),mean=0,sd=sigmaR[[stock]])}
                                     else {rep(0,length(simyears))}})
      # END recrdevs draw
      
      # Set up commercial catches
      CALcomm[[stock]] <- expand.grid(Year=simyears,
                             Month=1:12,
                             Fleet=Fleets$FleetName[which(Fleets$FleetType=="C")],
                             Sex=sexes,
                             LBin=lbins[[stock]]$Mean_Size,
                             catch=NA,
                             land=NA)
      CALcomm[[stock]] <- CALcomm[[stock]][order(CALcomm[[stock]]$Year,CALcomm[[stock]]$Month,
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

# Create objects to hold the sub-areas and trip types associated with each utility vector and trips taken
    utility.all.subareas <- NULL
    utility.labels <- NULL
    # trips!!!!
    
#############################################fine up to here   
 
##########@@......................................................................................@@##########
## Sub loop: time
# Define time references
    for(timestep in 1:(12*length(simyears))) {
      curryear <- firstyear+floor((timestep-1)/12)
      nextyear <- firstyear+floor(timestep/12)
      currmonth <- timestep - 12*floor((timestep-1)/12)
      nextmonth <- timestep - 12*floor(timestep/12) + 1
      wave <- ceiling(currmonth/2)

##@@......................................................................................................@@##
## Branch: calculate SSB if spawning season (for each dynamic species)
      for(stock in dynamic.stocks) {
        if(currmonth==spawnmonth[[stock]]) {
          SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] <- 0
          for (age in ages[[stock]]) {
            SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] <- 
              SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] +
              unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==curryear & NAA[[stock]]$Month==currmonth & 
                                            NAA[[stock]]$Age==age & NAA[[stock]]$Sex==sexes[1])]) *
              ageinfo[[stock]]$"Mat*Fecund"[which(ageinfo[[stock]]$Sex==1 & ageinfo[[stock]]$Real_Age==age)]
          } # END age loop
        } # END if spawning month
      } # END dynamic stock loop
## End spawning season branch
##@@......................................................................................................@@##

##@@......................................................................................................@@##
## Branch: add recruits if recruitment season (for each dynamic species)
      for(stock in dynamic.stocks) {
        if(currmonth %in% recrmonths[[stock]] & timestep!=1) {
          recrage <- ifelse(currmonth>=spawnmonth[[stock]],0,1)            # THIS CODE SPECIFIC TO SS 3.30!
          Recr[[stock]]$Recr[which(Recr[[stock]]$Year==(curryear-recrage))] <-
            (4*BH_steep[[stock]]*exp(LnR0[[stock]])*SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] / 
               (SSB0[[stock]]*(1-BH_steep[[stock]])+SSB[[stock]]$SSB[which(SSB[[stock]]$Year==curryear)] * 
                  (5*BH_steep[[stock]]-1)))*exp(recrdev[[stock]]$Dev[which(recrdev[[stock]]$Year==curryear)])
          for (sex in sexes) {
            NAA[[stock]]$N[which(NAA[[stock]]$Year==curryear & NAA[[stock]]$Month==currmonth & 
                                   NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==recrage)] <- 
              recrfrac[[stock]]$Prop[which(recrfrac[[stock]]$Sex==sex)] * Recr[[stock]]$Recr[
                which(Recr[[stock]]$Year==(curryear-recrage))] / length(recrmonths[[stock]]) + 
              unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==curryear & NAA[[stock]]$Month==currmonth & 
                                            NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==recrage)])
          } # END sex loop
        } # END if recruitment month
      } # END dynamic stock loop
## End recruitment season branch
##@@......................................................................................................@@##

##@@......................................................................................................@@##
## Convert population numbers from age- to length-classes (for each dynamic species)
      for(stock in dynamic.stocks) {
        for(sex in sexes) {
          NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & NAL[[stock]]$Month==currmonth & 
                                 NAL[[stock]]$Sex==sexes[sex])] <- 
            unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==curryear & NAA[[stock]]$Month==currmonth & 
                                          NAA[[stock]]$Sex==sexes[sex])]) %*% 
            t(ALK[[stock]][which(ALK[[stock]]$Subseason==2 & ALK[[stock]]$Sex==sex),-c(1:3)])
        } # END sex loop
      } # END dynamic stock loop
## End population conversion age to length
##@@......................................................................................................@@##

##@@......................................................................................................@@##
## Calculate commercial fishing and natural mortality, adjust NAL (for each dynamic species)
      for(stock in dynamic.stocks) {
        for (sex in sexes) {
          # Natural mortality
          NAL[[stock]]$N[which(NAL[[stock]]$Year==nextyear & NAL[[stock]]$Month==nextmonth & 
                                 NAL[[stock]]$Sex==sexes[sex])] <-
            unlist(NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & NAL[[stock]]$Month==currmonth & 
                                          NAL[[stock]]$Sex==sexes[sex])]) *
            exp(-Mortalities[[stock]][which(Mortalities[[stock]]$Type=="M" & 
                                              Mortalities[[stock]]$Sex==sexes[sex]),-c(1:4)]/12)
          
          # Commercial fishing
          NAL[[stock]]$N[which(NAL[[stock]]$Year==nextyear & NAL[[stock]]$Month==nextmonth & 
                                 NAL[[stock]]$Sex==sexes[sex])] <-
            unlist(NAL[[stock]]$N[which(NAL[[stock]]$Year==nextyear & NAL[[stock]]$Month==nextmonth & 
                                          NAL[[stock]]$Sex==sexes[sex])]) *
            exp(-colSums(Mortalities[[stock]][which(Mortalities[[stock]]$Type=="Fdead" & 
                                             Mortalities[[stock]]$Fleet %in% 
                                               Fleets$FleetName[which(Fleets$FleetType=="C")] &
                                             Mortalities[[stock]]$Year==curryear & 
                                             Mortalities[[stock]]$Sex==sexes[sex]),
                                     -c(1:4)])/12)
          
          # Record catches
          for (fleet in Fleets$FleetName) {
            Zdead <- Mortalities[[stock]][which(Mortalities[[stock]]$Type=="Zdead" & 
                                                Mortalities[[stock]]$Year==curryear & 
                                                Mortalities[[stock]]$Sex==sexes[sex]),-c(1:4)]/12
            CALcomm[[stock]]$catch[which(CALcomm[[stock]]$Year==curryear & CALcomm[[stock]]$Month==currmonth &
                                       CALcomm[[stock]]$Fleet==fleet & CALcomm[[stock]]$Sex==sexes[sex])] <- 
              NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & NAL[[stock]]$Month==currmonth & 
                                     NAL[[stock]]$Sex==sexes[sex])] *
              Mortalities[[stock]][which(Mortalities[[stock]]$Type=="Fctch" & 
                                         Mortalities[[stock]]$Fleet==fleet & 
                                         Mortalities[[stock]]$Year==curryear &
                                         Mortalities[[stock]]$Sex==sexes[sex]),
                          -c(1:4)]/12 * (1-exp(-Zdead/12))/(Zdead/12)
            CALcomm[[stock]]$land[which(CALcomm[[stock]]$Year==curryear & CALcomm[[stock]]$Month==currmonth &
                                        CALcomm[[stock]]$Fleet==fleet & CALcomm[[stock]]$Sex==sexes[sex])] <- 
              NAL[[stock]]$N[which(NAL[[stock]]$Year==curryear & NAL[[stock]]$Month==currmonth & 
                                     NAL[[stock]]$Sex==sexes[sex])] *
              Mortalities[[stock]][which(Mortalities[[stock]]$Type=="Fland" & 
                                         Mortalities[[stock]]$Fleet==fleet &
                                         Mortalities[[stock]]$Year==curryear &
                                         Mortalities[[stock]]$Sex==sexes[sex]),
                          -c(1:4)]/12 * (1-exp(-Zdead/12))/(Zdead/12)
          } # END fleet loop and catch recording
        } # END sex loop
      } # END dynamic stock loop
## End commercial fishing and natural mortality
##@@......................................................................................................@@##

##@@......................................................................................................@@##
## At the beginning of each year, calculate recreational trips by wave, area, boattype, subarea, and triptype
      if(currmonth==1) {
        # Initialize storage variables
        utility.by.waveareaboattype <- vector("list",length())
#        utility.by.area <- vector("list",length(areas))
#        names(utility.by.area) <- areas
#        for(area in areas) {
#          utility.by.area[[area]] <- vector("list",length(subareas[[area]]))
#          names(utility.by.area[[area]]) <- subareas[[area]]
#          for(subarea in subareas[[area]]) {
#            for(wave in 1:6)
#              utility.by.area[[area]][[subarea]] <- rbind(utility.by.area[[area]][[subarea]],
#                expand.grid(Wave=wave,        # Just averaging for now, var later?
#                            BoatType=boat.types,
#                            TripType=trip.types.by.area[[area]][[subarea]][[wave]],
#                            Trip=1:init.choices.by.waveareaboattype,
#                            Utility=0))
#          } # END subarea loop
#        } # END area loop

        for (wave in 1:6) {
          for (area in areas) {
            for (boattype in boat.types) {
              for(subarea in subareas[[area]]) {
                for (triptype in trip.types.by.area[[area]][[subarea]][[wave]]) {
                  for (choice in 1:init.choices.by.waveareaboattype) {
                    # Call function to draw catch from trip and report trip characteristics
                      trip.info <- TripDetails(curryear,
                                               currmonth,
                                               wave,
                                               area,
                                               subarea,
                                               boattype,
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
                    utility.by.area[[area]][[subarea]]$Utility[
                      which(utility.by.area[[area]][[subarea]]$Wave==wave & 
                            utility.by.area[[area]][[subarea]]$BoatType==boattype & 
                              utility.by.area[[area]][[subarea]]$TripType==triptype)] <= trip.info$utility +
                      utility.by.area[[area]][[subarea]]$Utility[
                        which(utility.by.area[[area]][[subarea]]$Wave==wave & 
                                utility.by.area[[area]][[subarea]]$BoatType==boattype & 
                                utility.by.area[[area]][[subarea]]$TripType==triptype)]
                  } # END choice loop
                  # Save utility for this triptype
                } # END triptype loop
                # Average utility for this trip type x subarea
              } # END subarea loop
              # Compare trip utilies to opt-out, get probabilities of each trip type x subarea (by wave, area, boattype)
            } # END boattype loop
          } # END area loop
        } # END wave loop
      } # END beginning of year branch
## End trip calculations
##@@......................................................................................................@@##

##@@......................................................................................................@@##
## Calculate recreational catches (for all species) and mortality (for each dynamic species) MORE EFFICIENT?
# Only at end of wave to avoid duplication
      if(wave == (currmonth/2)) {
        for(area in fishing.areas) {
          for (boattype in boat.types) {
            for (subarea in subareas[[area]]) {
              for (triptype in trip.types.by.area[[area]][[subarea]][[wave]]) {
                for (choice in 1:...) { # DO TOTAL TRIPS with max value...
                  # Call function to draw catch from trip and report trip characteristics
                  trip.info <- TripDetails(curryear,
                                           currmonth,
                                           wave,
                                           area,
                                           subarea,
                                           boattype,
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
                  # Save catches
                } # END choice loop
                # 
              } # END triptype loop
            } # END subarea loop
          } # END boattype loop
        } # END area loop
        # Update NAL for dynamic stocks
      } # END end of wave branch
## End recreational catch section
##@@......................................................................................................@@##

##@@......................................................................................................@@##
## Convert population numbers from length- to age-classes (for each dynamic species)
      for(stock in dynamic.stocks) {
        for (sex in sexes) {
          NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & NAA[[stock]]$Month==nextmonth & 
                                 NAA[[stock]]$Sex==sexes[sex])] <- 
            unlist(NAL[[stock]]$N[which(NAL[[stock]]$Year==nextyear & NAL[[stock]]$Month==nextmonth & 
                                          NAL[[stock]]$Sex==sexes[sex])]) %*% 
            as.matrix(GenLengthAgeKey(numbersatage=unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==curryear & 
                                              NAA[[stock]]$Month==currmonth & NAA[[stock]]$Sex==sexes[sex])]),
                                      ALKey=ALK[[stock]][which(ALK[[stock]]$Subseason==2 & 
                                                                 ALK[[stock]]$Sex==sexes[sex]),-c(1:2)])[,-1])
        } # END sex loop
      } # END dynamic stock loop
## End population conversion length to age
##@@......................................................................................................@@##

##@@......................................................................................................@@##
## Branch: age if end of year (for each dynamic species)
      for(stock in dynamic.stocks) {
        if(currmonth==12) {
          for (sex in sexes) {
            # Address plus age group
            NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & NAA[[stock]]$Month==nextmonth & 
                                   NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==max(ages[[stock]]))] <- 
              unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & NAA[[stock]]$Month==nextmonth & 
                                            NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==max(ages[[stock]]))]) +
              unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & NAA[[stock]]$Month==nextmonth & 
                                          NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==(max(ages[[stock]])-1))])
            for (age in rev(ages[[stock]][-c((length(ages[[stock]])-1),length(ages[[stock]]))])) {
              NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & NAA[[stock]]$Month==nextmonth & 
                                     NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==(age+1))] <- 
                unlist(NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & NAA[[stock]]$Month==nextmonth &
                                              NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==age)])
            } # END age loop
            # Zero out 0-age fish
            NAA[[stock]]$N[which(NAA[[stock]]$Year==nextyear & NAA[[stock]]$Month==nextmonth & 
                                   NAA[[stock]]$Sex==sex & NAA[[stock]]$Age==ages[[stock]][1])] <- 0
          } # END sex loop
        } # END end of year branch
      } # END dynamic stock loop
## End ageing branch
##@@......................................................................................................@@##
      
    } ## End time loop
##########@@......................................................................................@@##########
    
#  } ## End MCMC conditional loop
####################@@..................................................................@@####################
  
# Print total run time, this scenario and reset split timer
  print(paste("Total run time, this scenario: ", proc.time() - split.time))
  split.time <- proc.time()
#} ## End scenario loop
##############################################################################################################

##############################################################################################################
#### Summarize and save output
# Need to decide on details here

# Print total run time, all scenarios
print(paste("Total run time, all scenarios: ", proc.time() - elapsed.time))

#####
##############################################################################################################

#### END PROGRAM
##############################################################################################################