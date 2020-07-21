init_dy_var <- function(dynamic.stocks) {
    #' init_dy_var
    #'
    #' Initialize dynamic variables for stocks
    #'
    #' @param dynamic.stocks Names of dynamic stocks
    #'
    #' @return outlist List of stock characteristics
    #' @export
    #' @examples
    #'
    
    outlist <- list()
    
    outlist$ageinfo <- vector("list", length(dynamic.stocks))
    names(outlist$ageinfo) <- dynamic.stocks
    outlist$ages <- vector("list", length(dynamic.stocks))
    names(outlist$ages) <- dynamic.stocks
    outlist$ALK <- vector("list", length(dynamic.stocks))
    names(outlist$ALK) <- dynamic.stocks
    outlist$assessmentyear <- vector("list", length(dynamic.stocks))
    names(outlist$assessmentyear) <- dynamic.stocks
    outlist$BH_steep <- vector("list", length(dynamic.stocks))
    names(outlist$BH_steep) <- dynamic.stocks
    outlist$lbins <- vector("list", length(dynamic.stocks))
    names(outlist$lbins) <- dynamic.stocks
    outlist$Mortalities <- vector("list", length(dynamic.stocks))
    names(outlist$Mortalities) <- dynamic.stocks
    outlist$popsize <- vector("list", length(dynamic.stocks))
    names(outlist$popsize) <- dynamic.stocks
    outlist$recrfrac <- vector("list", length(dynamic.stocks))
    names(outlist$recrfrac) <- dynamic.stocks
    outlist$recrmonths <- vector("list", length(dynamic.stocks))
    names(outlist$recrmonths) <- dynamic.stocks
    outlist$sigmaR <- vector("list", length(dynamic.stocks))
    names(outlist$sigmaR) <- dynamic.stocks
    outlist$spawnmonth <- vector("list", length(dynamic.stocks))
    names(outlist$spawnmonth) <- dynamic.stocks
    outlist$SSB0 <- vector("list", length(dynamic.stocks))
    names(outlist$SSB0) <- dynamic.stocks
    outlist$WeightConversion <- vector("list", length(dynamic.stocks))
    names(outlist$WeightConversion) <- dynamic.stocks
    
    for(stock in dynamic.stocks) {
        load(system.file("extdata", paste(stock, ".RData", sep=""), 
            package = "nwblastarca"))
        # Convert dynamic stock data into common lists
        outlist$ageinfo[[stock]] <- get(stock)$ageinfo
        outlist$ages[[stock]] <- get(stock)$ages
        outlist$ALK[[stock]] <- get(stock)$ALK
        outlist$assessmentyear[[stock]] <- get(stock)$assessmenthyear
        outlist$BH_steep[[stock]] <- get(stock)$BH_steep
        outlist$lbins[[stock]] <- get(stock)$lbins
        outlist$Mortalities[[stock]] <- get(stock)$Mortalities
        outlist$popsize[[stock]] <- get(stock)$popsize
        outlist$recrfrac[[stock]] <- get(stock)$recrfrac
        outlist$recrmonths[[stock]] <- get(stock)$recrmonths
        outlist$sigmaR[[stock]] <- get(stock)$sigmaR
        outlist$spawnmonth[[stock]] <- get(stock)$spawnmonth
        outlist$SSB0[[stock]] <- get(stock)$SSB0
        outlist$WeightConversion[[stock]] <- get(stock)$WeightConversion
    } # END dynamic stock loop
    
    return(outlist)
    
}
    