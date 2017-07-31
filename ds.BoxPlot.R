#' 
#' @title Generates a boxplot for one numeric variable
#' @description This function plots a boxplot without showing the outliers.
#' @details The function calls a datashield server side function that produce the 
#' information required to plot the boxplot. The function allows the user to plot 
#' disctinct boxplots one for each single study (plotted in the same panel) or one 
#' boxplot for combined data.
#' @param x the input (numeric) vector for which the boxplot is desired.
#' @param type a character which represent the type of graph to display.
#' If \code{type} is set to 'combine' (default), a boxplot for combined data is displayed. 
#' If \code{type} is set to 'split', a boxplot for each study is displayed and the 
#' boxplots are plotted in one single graph.
#' @param notch a boolean, that determines whether the notch of the boxplot is shown. 
#' If FALSE (default) the notch is not presented. If TRUE, the notch is drawn in each side
#' of the boxplot indicating the 95% confidence interval of the median.  
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return one or more boxplots for the numeric variable depending on the argument \code{type}
#' @author Avraam, D.
#' @export
#' @examples {
#'
#'   # load that contains the login details
#'   data(logindata)
#'
#'   # login and assign specific variable(s)
#'   myvar <- list('LAB_TSC', 'LAB_HDL')
#'   opals <- datashield.login(logins=logindata, assign=TRUE, variables=myvar)
#'
#'   # Example 1: Plot a combined boxplot of the variable 'LAB_TSC' (default behaviour)
#'   ds.BoxPlot(x='D$LAB_TSC')
#'
#'   # Example 2: Plot distinct bloxplots of 'LAB_TSC' (one per study)
#'   ds.BoxPlot(x='D$LAB_TSC', type='split')
#'
#'   # Example 3: Plot a combined histogram of the variable 'LAB_HDL' showing the notch
#'   ds.BoxPlot(x='D$LAB_HDL', notch=TRUE)
#'
#'   # Example 4: Plot the distinct boxplots of 'LAB_HDL' (one per study) showing the notches
#'   ds.BoxPlot(x='D$LAB_HDL', type='split', notch=TRUE)
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#'
#' }
#'
ds.BoxPlot <- function (x=NULL, type='combine', notch=FALSE, datasources=NULL){

  # if no opal login details are provided look for 'opal' objects in the environment
    if(is.null(x)){
        stop("Please provide the input variable", call.=FALSE)
    }

    if(is.null(datasources)){
        datasources <- findLoginObjects()
    }

  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders

  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(varnames)){
    if(is.na(obj2lookfor[i])){
      defined <- isDefined(datasources, varnames[i])
    }else{
      defined <- isDefined(datasources, obj2lookfor[i])
    }
  }
  
  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  typ <- checkClass(datasources, x)

  # the input object must be a numeric or an integer vector
  if(typ != 'integer' & typ != 'numeric'){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The input object must be an integer or numeric vector.", call.=FALSE)
  }
  
  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)
  
  # call the server-side function that gives the information required to plot a boxplot
  call <- paste0("BoxPlotDS.b(", x, ")")
  output <- datashield.aggregate(datasources, call)
  
  # create a list with the infromation required to plot a box plot for each study
    split.stats <- matrix(0, nrow = 5, ncol = length(datasources))
    split.conf <- matrix(0, nrow = 2, ncol = length(datasources))
    split.out <- matrix(0, nrow = 2, ncol =length(datasources))
    split.n <- matrix(0, nrow = 1, ncol =length(datasources)) 
    split.group <- c()
    split.names <- stdnames
    split.range <- c()
    for (i in 1:length(datasources)){
        split.stats[, i] <- output[[i]][[1]][1:5]
        split.n[, i] <- output[[i]][[2]]
        split.conf[, i] <- output[[i]][[3]]
        split.out[, i] <- output[[i]][[4]][1:2]
        split.group[[i]] <- cbind(rep(i, (output[[i]][[4]][1]+output[[i]][[4]][2])))
        split.range[[i]] <- output[[i]][[5]]
    }
    split.group <- rbind(split.group)
    split.z <- list(stats=split.stats, n=split.n, conf=split.conf, out=split.out, group=split.group, names=split.names)

  # create a list with the infromation required to plot a box plot for pooled data
    pooled.stats <- matrix(0, nrow = 5, ncol = 1)
    pooled.n <- 0
    pooled.conf <- matrix(0, nrow = 2, ncol = 1)
    pooled.out <- rep(0, 2)
    for (i in 1:length(datasources)){
       pooled.stats <- pooled.stats + (output[[i]][[1]][1:5] * output[[i]][[2]])
       pooled.n <- pooled.n + output[[i]][[2]]
       pooled.conf <- pooled.conf + (output[[i]][[3]][1:2] * output[[i]][[2]])
       pooled.out <- pooled.out + output[[i]][[4]]
    }
    pooled.stats <- pooled.stats/pooled.n
    pooled.conf <- pooled.conf/pooled.n
    pooled.group <- rep(1, (pooled.out[1]+pooled.out[2]))
    pooled.name <- paste0("Combined Data")
    pooled.range <- (pooled.stats[5]-pooled.stats[4])/(pooled.stats[4]-pooled.stats[2])
    pooled.z <- list(stats=pooled.stats, n=pooled.n, conf=pooled.conf, out=pooled.out, group=pooled.group, names=pooled.name)
 
  # For argument 'type' call the bxp function which creates the box plots
  if(type=="combine"){
    do.call("bxp", list(pooled.z, range=pooled.range, outline = FALSE, notch=notch, main=paste("Box Plot of", varnames, "(Combined Data)")))
    list(stats=pooled.stats, n=pooled.n, conf=pooled.conf, out=pooled.out)
  }else{
    if(type=="split"){
      do.call("bxp", list(split.z, range=split.range, outline = FALSE, notch=notch, main=paste("Box Plot of", varnames, "(Split Data)")))
      list(stats=split.stats, n=split.n, conf=split.conf, out=split.out)   
    }else{
    stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
}
