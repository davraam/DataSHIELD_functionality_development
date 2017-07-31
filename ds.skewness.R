ds.skewness = function(x=NULL, formula=3, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  if (!(formula %in% c(1:3))){ 
    stop("Invalid 'formula' argument.")
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  }else{
    defined <- isDefined(datasources, obj2lookfor)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # the input object must be a numeric or an integer vector
  if(typ != 'integer' & typ != 'numeric'){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The input object must be an integer or numeric vector.", call.=FALSE)
  }
   
  cally <- paste0("skewnessDS(x=", x, ",", "formula='", formula, "')")
  output <- datashield.aggregate(datasources, as.symbol(cally))
   
  return(output)
  
}
