skewnessDS <- function (x, formula = formula){

  if (any(ina <- is.na(x))){
    x <- x[!ina]
  }

  n <- length(x)
  y <- sqrt(n) * sum((x - mean(x))^3)/(sum((x - mean(x))^2)^(3/2))

  if (!(formula %in% (1:3))){ 
    stop("Invalid 'formula' argument.")
  }else{
    if (formula == 1){
      skewness <- y   
    }
    if (formula == 2){
      if (n < 3){ 
        stop("Need at least 3 complete observations.")
      }else{
        skewness <- y * sqrt(n * (n - 1))/(n - 2)
      }
    }
    if (formula == 3){ 
        skewness <- y * ((1 - 1/n))^(3/2)
    }
  }

  return(skewness)
}