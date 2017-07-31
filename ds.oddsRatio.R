#'
#' @title Calculates the odds ratio between two dichotomous variables
#' @description Calculates the odds ratio and its confidence interval between two binary variables
#' @details The function calculates the odds ratio between an exposure and an outcome (both of which are binary variables).
#' The odds ratio with their confidence intervals are calculated for each single study. Also, an overall ('pooled') odds ratio
#' is estimated by Mantel-Haenszel method assuming a fixed effects model. 
#' @param x is the outcome variable. Parameter \code{x} must be a binary variable 
#' @param y is the exposure variable. Parameter \code{y} must be a binary variable
#' @param alpha is a value from 0 to 1 that indicates the level of the confidence interval. By default \code{alpha} is set to 
#' 0.05 and therefore a (1-alpha)*100=95% confidence interval is calculated.
#' @param forest.plot a boolean that determines whether a forest plot of odds ratios will be shown. If this argument is set to
#' TRUE (default) then the function loads the 'forestplot' package and a forest plot is created.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return the odds ratios, their (1-alpha)% confidence interval and the alpha value for each single value and the pooled odds
#' ratio with (1-alpha)% confidence interval as estimated by the Mantel-Haenszel method. A forest plot is also return to an R 
#' graphics window if the argument \code(forest.plot) is set to TRUE.
#' @author Avraam, D.
#' @export 
#' @examples {
#'
#'   # load that contains the login details
#'   data(logindata)
#'
#'   # login and assign specific variable(s)
#'   myvar <- list('DIS_DIAB', 'GENDER')
#'   opals <- datashield.login(logins=logindata, assign=TRUE, variables=myvar)
#'
#'   # Example 1: Calculate the odds ratio between GENDER and DIS_DIAB variables
#'   ds.oddsRatio(x='D$DIS_DIAB', y='D$GENDER')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#'
#' }
#' 
ds.oddsRatio <- function(x=NULL, y=NULL, alpha=0.05, forest.plot=TRUE, datasources=NULL){ 
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the x vector!", call.=FALSE)
  }
  
  if(is.null(y)){
    stop("Please provide the name of the y vector!", call.=FALSE)
  }

  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  ynames <- extract(y)
  xvarname <- xnames$elements
  yvarname <- ynames$elements
  xobj2lookfor <- xnames$holders
  yobj2lookfor <- ynames$holders
  xvariable <- xvarname
  yvariable <- yvarname
  
  # check if the input object(s) is(are) defined in all the studies
  if(is.na(xobj2lookfor)){
    defined <- isDefined(datasources, xvarname)
  }else{
    defined <- isDefined(datasources, xobj2lookfor)
  }
  if(is.na(yobj2lookfor)){
    defined <- isDefined(datasources, yvarname)
  }else{
    defined <- isDefined(datasources, yobj2lookfor)
  }

  if(is.null(alpha)){
    alpha <- 0.05
  }
  if(alpha < 0 || alpha > 1){
    stop("alpha should be a value from 0 to 1", call.=FALSE)
  }

  # call the internal function that checks the input object is of the same class in all studies.
  typ1 <- checkClass(datasources, x)
  typ2 <- checkClass(datasources, y)
  
  # names of the studies 
  stdnames <- names(datasources)
  
  # call the server side function that calculates the odds ratio for each study
  cally <- paste0("table2dDS(", x, ",", y, ")")
  output <- datashield.aggregate(datasources, as.symbol(cally))

  Table <- list()
  OR <- c()
  sigma.log <- c()
  log.OR <- c()
  log.lower <- c()
  log.higher <- c()
  OR.lower <- c()
  OR.higher <- c()
  n <- c()
  P <- c()
  Q <- c()
  R <- c()
  S <- c()
  out <- list()

  for (i in 1:length(datasources)){
    Table[[i]] <- output[[i]][[1]]
   
    # Calculate the odds ratio
    OR[i] <- (Table[[i]][1,1] * Table[[i]][2,2]) / (Table[[i]][1,2] * Table[[i]][2,1])

    # Compute the Wald confidence intervals:
    sigma.log[i] <- sqrt((1/Table[[i]][1,1]) + (1/Table[[i]][1,2]) + (1/Table[[i]][2,1]) + (1/Table[[i]][2,2]))
    zalpha <- qnorm(1 - alpha/2)
    log.OR[i] <- log(OR[i])
    log.lower[i] <- log.OR[i] - zalpha * sigma.log[i]
    log.higher[i] <- log.OR[i] + zalpha * sigma.log[i]
  
    OR.lower[i] <- exp(log.lower[i])
    OR.higher[i] <- exp(log.higher[i])

    out[[i]] <- c(LowerCI = OR.lower[i], OR = OR[i], UpperCI = OR.higher[i], alpha = alpha)

    n[i] <- Table[[i]][1,1] + Table[[i]][1,2] + Table[[i]][2,1] + Table[[i]][2,2]
    P[i] <- (Table[[i]][1,1] + Table[[i]][2,2]) / n[i]
    Q[i] <- (Table[[i]][1,2] + Table[[i]][2,1]) / n[i]
    R[i] <- (Table[[i]][1,1] * Table[[i]][2,2]) / n[i]
    S[i] <- (Table[[i]][1,2] * Table[[i]][2,1]) / n[i]

  }
  
  # Mantel-Haenszel method is used to estimate the pooled odds ratio assuming a fixed effects model
  sum.PR <- 0
  sum.R <- 0
  sum.PS.plus.QR <- 0
  sum.S <- 0
  sum.QS <- 0
  for (i in 1:length(datasources)){
    sum.PR <- sum.PR + P[i]*R[i]
    sum.R <- sum.R + R[i]
    sum.PS.plus.QR <- sum.PS.plus.QR + (P[i]*S[i]+Q[i]*R[i])
    sum.S <- sum.S + S[i]
    sum.QS <- sum.QS + Q[i]*S[i]
  }
  pooled.OR <- sum.R/sum.S
  pooled.sigma <- sqrt(((sum.PR)/(2*(sum.R)^2))+((sum.PS.plus.QR)/(2*sum.R*sum.S))+((sum.QS)/(2*(sum.S)^2)))
  zalpha <- qnorm(1 - alpha/2)
  CI <- (1-alpha)*100
  pooled.CI.lower <- pooled.OR * exp(-zalpha * pooled.sigma)
  pooled.CI.higher <- pooled.OR * exp(zalpha * pooled.sigma)

  out[[(length(datasources)+1)]] <- c(LowerCI = pooled.CI.lower, OR = pooled.OR, UpperCI = pooled.CI.higher, alpha = alpha)
  
  names <- c(stdnames, paste0('pooled odds ratio'))
  names(out) <- paste0(names)

  if (forest.plot==TRUE){
    
    # turn warnings of
    options(warn=-1)

    # load the 'forestplot' package that allows the plot of a forestplot
    library(forestplot)

    # turn warnings on
    options(warn=0)
    
    labeltext <- cbind(c("Study", stdnames, NA, "Overall"), c(paste0("OR (", CI, "% CI)"), paste0(signif(OR,3), "  (", signif(OR.lower,3), "-", signif(OR.higher,3), ")"), NA, paste0(signif(pooled.OR,3), " (", signif(pooled.CI.lower,3), "-", signif(pooled.CI.higher,3), ")")))
    mean=c(NA, OR, NA, pooled.OR)
    lower=c(NA, OR.lower, NA, pooled.CI.lower)
    higher=c(NA, OR.higher, NA, pooled.CI.higher)
    is.summary.vector <- c(TRUE, rep(FALSE, (length(mean)-2)), TRUE)
    forestplot(labeltext, hrzl_lines = gpar(col="#444444"), mean, lower, higher, vertices=TRUE, new_page = TRUE, xlog=TRUE, is.summary=is.summary.vector)   
  }

  # Return the odds ratios, their (1-alpha)% confidence interval and the alpha value
  return(out)
 
}
