# Function from synthpop library to create synthetic data
synDS <- function(data){

  # load the 'synthpop' package
  library(synthpop)

  # create synthetic data using the 'syn' function
  out <- syn(data)

  # return the output
  return(out)

}
