#'
#' 
oddsRatioDS <- function(xvect, yvect, alpha){

  # Tabulate the two vectors
  Table <- table(xvect, yvect)
  
  # Calculate the odds ratio
  OR <- (Table[1,1] * Table[2,2]) / (Table[1,2] * Table[2,1])

  # Compute the Wald confidence intervals:
  siglog <- sqrt((1/Table[1,1]) + (1/Table[1,2]) + (1/Table[2,1]) + (1/Table[2,2]))
  zalpha <- qnorm(1 - alpha/2)
  log.OR <- log(OR)
  log.lower <- log.OR - zalpha * siglog
  log.higher <- log.OR + zalpha * siglog
  
  OR.lower <- exp(log.lower)
  OR.higher <- exp(log.higher)
  
  # Return a dataframe that includes the odds ratio, its (1-alpha) confidence interval and the alpha value
  output <- data.frame(LowerCI = OR.lower, OR = OR, UpperCI = OR.higher, alpha = alpha)
  return(output)

}

datashield.aggregate(opals.cm, paste0("oddsRatioDS(x=", 'D$LAB_TSC', ", y=D$LAB_HDL", ", alpha=", '0.05', ")"))

datashield.aggregate(opals.cm, paste0("oddsRatioDS(x=", 'D$DIS_DIAB', ", y=D$GENDER", ", alpha=", '0.05', ")"))

