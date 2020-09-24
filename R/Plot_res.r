Plot_res <- function(blastres, MCMC, wd = NULL, savep = FALSE, 
    samplei = FALSE) {
    #' Plot_res
    #'
    #' Plot results
    #'
    #' @param blastres Results from running the blast_arca_wrapper
    #' @param MCMC Number of Monte Carlo iterations
    #' @param wd Working directory to save plots to
    #' @param savep Whether to save plots or not
    #' @param samplei Whether to draw a random single iteration or take the mean
    #' of all the results (default)
    #'
    #' @return plotout Numbers at age plot and rec catches plot
    #' @export
    #' @importFrom ggplot2 ggplot geom_bar aes facet_wrap scale_fill_continuous
    #' guides guide_colorbar ylab theme element_text scale_x_continuous
    #' scale_fill_manual xlab guide_legend ggsave
    #' @importFrom RColorBrewer brewer.pal
    #' @examples
    #' 
    
    if (samplei == TRUE) {
    NAAforplot <- as.data.frame(lapply(lapply(blastres, `[[`, 1), `[[`, 1)[
        sample(1:MCMC, 1)])
    } else {
    NAAforplot <- as.data.frame(apply(simplify2array(lapply(lapply(lapply(
        blastres, `[[`, 1), `[[`, 1), as.matrix)), 1:2, mean))
    }
    
    NAAout <- ggplot(NAAforplot) + 
        geom_bar(aes(x=Age, y = N, fill = Month), 
            stat="identity", position = "identity", alpha=1) +
        facet_wrap(~Year) +
        scale_fill_continuous(limits=c(1, 12), breaks= seq(1,12,by=1)) +
        guides(fill = guide_colorbar(reverse = TRUE, barwidth = 0.5, 
            barheight = 10)) +
        ylab("Total number Lingcod at age (thousands)") +
        theme(text = element_text(size=20))

    if (savep == TRUE) {
    ggsave(NAAout, file = paste0(wd, "NAA", ".png"), width = 16, height = 9)
    }
    
    bindcatch <- list()
    for (mc in 1:MCMC) {
    bindcatch[[mc]] <- as.matrix(rbind(do.call(rbind, blastres[[mc]][[2]][[1]]), 
        do.call(rbind, blastres[[mc]][[2]][[2]])))
    }
            
    if (samplei == TRUE || MCMC == 1) {
        catchforplot <- as.data.frame(bindcatch[sample(1:MCMC, 1)])
    } else {
        catchforplot <- bindcatch[[1]]
        for (mc in 2:MCMC) {
        catchforplot <- merge(catchforplot, bindcatch[[mc]], by = c("LBin",
            "Sex", "Year", "Month"), all.x = TRUE, all.y = TRUE)
        
        catchforplot$count.x[is.na(catchforplot$count.x)] <- 0
        catchforplot$count.y[is.na(catchforplot$count.y)] <- 0

        catchforplot$count <- catchforplot$count.x + catchforplot$count.y
        catchforplot$count.x <- NULL
        catchforplot$count.y <- NULL
        }
        
        catchforplot$count <- catchforplot$count/MCMC
    }

    my.cols <- brewer.pal(7, "Set1")
    my.cols <- my.cols[-6]

    reccatches <- ggplot(catchforplot) + 
        geom_bar(aes(x=(LBin), y = count, fill = as.factor(Month)), 
            stat="identity", alpha=1) +
        facet_wrap(~Year) +
        scale_x_continuous(limits=c(min(catchforplot$LBin)-2, 
            max(catchforplot$LBin)+2), breaks = seq(min(catchforplot$LBin), 
            max(catchforplot$LBin)+2,by=4))+
        scale_fill_manual(values = my.cols) +
        ylab("Total number of recreationally-caught Lingcod") +
        xlab("Length bin")+
        guides(fill=guide_legend(title="Month")) +
        theme(text = element_text(size=20), 
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    if (savep == TRUE) {
    ggsave(reccatches, file = paste0(wd, "reccatches", ".png"), width = 16, 
        height = 9)
    }
    
    plotout <- list(NAAout, reccatches)
    return(plotout)
} 
