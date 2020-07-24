GenLengthAgeKey <- function(numbersatage,    # Vector of current numbers at age
                            ALKey) {         # DF of age-length-conversion w/C1=LBins, C2=first age
    #' GenLengthAgeKey
    #'
    #' Organize the numbersatage and age-length-key
    #'
    #' @param numbersatage Vector of current numbers at age
    #'
    #' @return tmp6 tmp6
    #' @export
    #' @importFrom reshape2 melt dcast
    #' @importFrom data.table data.table :=
    #' @examples
    #'

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
  tmp3 <- data.frame(tmp2[,-which(names(tmp2)=="count")])
  tmp4 <- melt(tmp3,variable.name="length",id.vars="age")
  
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
  tmp6 <- dcast(tmp5, length~age, value.var="prob")
  tmp6[,-1] <- tmp6[,-1][,order(as.numeric(names(tmp6)[-1]))]
  
  return(tmp6)
}
