Init_dy_var <- function(dynamic.stocks, indat) {
    #' Init_dy_var
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
        # Convert dynamic stock data into common lists
        outlist$ageinfo[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$ageinfo
        outlist$ages[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$ages
        outlist$ALK[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$ALK
        outlist$assessmentyear[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$assessmentyear
        outlist$BH_steep[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$BH_steep
        outlist$lbins[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$lbins
        outlist$Mortalities[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$Mortalities
        outlist$popsize[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$popsize
        outlist$recrfrac[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$recrfrac
        outlist$recrmonths[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$recrmonths
        outlist$sigmaR[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$sigmaR
        outlist$spawnmonth[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$spawnmonth
        outlist$SSB0[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$SSB0
        outlist$WeightConversion[[stock]] <- indat[[paste0(dynamic.stocks, 
            ".dynstock")]]$WeightConversion
    } # END dynamic stock loop
    
    return(outlist)
    
}
    