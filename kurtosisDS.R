kurtosisDS <- function (x, formula = formula){

  if (any(ina <- is.na(x))){
    x <- x[!ina]
  }

  n <- length(x)
  r <- n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)

  if (!(formula %in% (1:3))){ 
    stop("Invalid 'formula' argument.")
  }else{
    if (formula == 1){
      kurtosis <- r - 3   
    }
    if (formula == 2){
      if (n < 4){ 
        stop("Need at least 4 complete observations.")
      }else{
        kurtosis <- ((n + 1) * (r - 3) + 6) * (n - 1)/((n - 2) * (n - 3))
      }
    }
    if (formula == 3){ 
        kurtosis <- r * (1 - 1/n)^2 - 3
    }
  }

  return(kurtosis)
}