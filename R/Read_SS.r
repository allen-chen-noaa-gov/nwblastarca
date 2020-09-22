Read_SS <- function(filename) {
    #' Read_SS
    #'
    #' Read in a Report.sso file from Stock Synthesis for use in BLAST
    #'
    #' @param filename File path where Report.sso is located
    #'
    #' @return Stockinfo Information about stock used in BLAST model
    #' @importFrom r4ss SS_output
    #' @export
    #' @examples
    #' 
    
tmp <- SS_output(dir = filename, forecast = FALSE, 
    warn = FALSE, covar=FALSE, readwt = FALSE, checkcor = FALSE, 
    verbose = FALSE, printstats=FALSE, hidewarn = TRUE, NoCompOK = TRUE)

sexes <- c(1,2)
# 1=female, 2=male
Fleets <- data.frame(FleetType=c("C","C","R","R"),
    FleetNum=1:4,
    FleetName=c("1_N_TRAWL","2_N_FIX","3_WA_REC","4_OR_REC"),
    stringsAsFactors=FALSE)
    
Stockinfo <- list()
Stockinfo$ageinfo <- tmp$endgrowth
Stockinfo$ages <- as.numeric(dimnames(tmp$ALK)$TrueAge)

ALK <- NULL
for (i in 1:dim(tmp$ALK)[3]) {

    Subseason <- matrix(as.numeric(sub(" Morph:.*", "", 
        sub(".*Sub_Seas: ", "", dimnames(tmp$ALK)$Matrix[i]))), 
        dim(tmp$ALK[,,i])[1], )
        
    Sex <- matrix(sub(".*Morph: ", "", dimnames(tmp$ALK)$Matrix[i]),
        dim(tmp$ALK[,,i])[1])

    ALK <- rbind(ALK, cbind(data.frame(Subseason = Subseason), 
        data.frame(Sex = Sex),
        data.frame(LBin = as.numeric(rev(dimnames(tmp$ALK)$Length))),
        apply(tmp$ALK[,,i],2,rev)))
        
}

Stockinfo$ALK <- ALK
Stockinfo$assessmentyear <- tmp$endyr
 
Stockinfo$BH_steep <- tmp$parameters$Value[
    tmp$parameters$Label == "SR_BH_steep"]

lbins <- data.frame(Low = as.numeric(rev(dimnames(tmp$ALK)$Length)))
lbins$Mean_Size <- lbins$Low + (tmp$Lbin_method)/2
Stockinfo$lbins <- lbins 

Lsel <- tmp$sizeselex[tmp$sizeselex$Factor == "Lsel" & 
    tmp$sizeselex$Fleet <= 4 & tmp$sizeselex$Yr == tmp$endyr,]

Keep <- tmp$sizeselex[tmp$sizeselex$Factor == "Dead" & 
    tmp$sizeselex$Fleet <= 4 & tmp$sizeselex$Yr == tmp$endyr,]

Dead <- tmp$sizeselex[tmp$sizeselex$Factor == "Keep" & 
    tmp$sizeselex$Fleet <= 4 & tmp$sizeselex$Yr == tmp$endyr,]

Fs <- tmp$exploitation[tmp$exploitation$Yr >= tmp$endyr,]

Mortalities <- expand.grid(Type="M",
                           Fleet=NA,
                           Year=NA,
                           Sex=sexes,
                           stringsAsFactors=FALSE)
MortLbins <- matrix(0,nrow=nrow(Mortalities),ncol=nrow(lbins))
MortLbins <- as.data.frame(MortLbins)
names(MortLbins) <- lbins$Mean_Size
Mortalities <- cbind(Mortalities,MortLbins)
                           
Mortalities[,5:dim(Mortalities)[2]] <- rowMeans(tmp$M_at_age[
    tmp$M_at_age$Yr == tmp$endyr, 5:dim(tmp$M_at_age)[2]], na.rm = TRUE)     

years <- Fs$Yr
    
for (fleet in Fleets$FleetNum) {
  for (sex in sexes) {
    Fvals <- Fs[which(Fs$Yr %in% years),
                which(names(Fs) %in% c("Yr", Fleets$FleetName[which(Fleets$FleetNum==fleet)]))]
    for (year in years) {
Mortalities <- rbind.data.frame(Mortalities, 
cbind.data.frame(Type="Fctch",
               Fleet=Fleets$FleetName[which(Fleets$FleetNum==fleet)],
               Year=year,
               Sex=sex,
               Fvals[which(Fvals$Yr==year),2] * 
                 Lsel[which(Lsel$Fleet==fleet & Lsel$Sex==sex),6:ncol(Lsel)],
               stringsAsFactors=FALSE),
cbind.data.frame(Type="Fdead",
               Fleet=Fleets$FleetName[which(Fleets$FleetNum==fleet)],
               Year=year,
               Sex=sex,
               Fvals[which(Fvals$Yr==year),2] * 
                 Dead[which(Dead$Fleet==fleet & Dead$Sex==sex),6:ncol(Dead)],
               stringsAsFactors=FALSE),
cbind.data.frame(Type="Fland",
               Fleet=Fleets$FleetName[which(Fleets$FleetNum==fleet)],
               Year=year,
               Sex=sex,
               Fvals[which(Fvals$Yr==year),2] * 
                 Keep[which(Keep$Fleet==fleet & Keep$Sex==sex),6:ncol(Keep)],
               stringsAsFactors=FALSE),
stringsAsFactors=FALSE)
    } # END year loop
  } # END sex loop
} # END fleet loop
        
Stockinfo$Mortalities <- Mortalities
        
Stockinfo$popsize <- tmp$natage[tmp$natage$Yr >= tmp$endyr & 
    tmp$natage$`Beg/Mid` == "B", 
    -which(names(tmp$natage) %in% c("Beg/Mid","Era"))]

Stockinfo$recrfrac <- data.frame(Sex=sexes,                                                # 1=female, 2=male
    Prop=c(tmp$parameters$Value[tmp$parameters$Label == "FracFemale_GP_1"],
    1- tmp$parameters$Value[tmp$parameters$Label == "FracFemale_GP_1"]))

Stockinfo$recrmonths <- tmp$parameters$Value[
    tmp$parameters$Label == "RecrDist_month_1"]
Stockinfo$sigmaR <- tmp$parameters$Value[
    tmp$parameters$Label == "SR_sigmaR"] 
Stockinfo$SSB0 <- tmp$SBzero
Stockinfo$WeightConversion <- tmp$sprseries$SSBzero[
    tmp$sprseries$Yr == tmp$endyr]
    
return(Stockinfo)

}
