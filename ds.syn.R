ds.syn = function(x=NULL, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # throw a message and stop if input is not table structure
  if(typ != 'data.frame' & typ!= 'matrix'){
    stop("The input object must be a table structure!", call.=FALSE)
  }
  
  cally <- paste0('synDS(', x, ')')
  output <- datashield.aggregate(datasources, as.symbol(cally))
  
  return(output)

}