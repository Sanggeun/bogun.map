p_function <- function(p_value) {
  if (p_value <0.001) {
    k <- "***"
  } else if (p_value < 0.01) {
    k <- "**" 
  }
  else if (p_value < 0.05) {
    k <- "*"
  } else {
    k <-""
  }
  
  return(k)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

underbar_change <- function(x) gsub("[[:punct:]]+", "_", gsub("[[:punct:]]$", "", x))

rank2group <- function (y,k=4){
  count=length(y)
  z=rank(y,ties.method="min")
  return(floor((z-1)/(count/k))+1)
}
                                    
round_new <- function(x, digits=3) {
  formatC(round(x,digits),digits,format="f")
}

Mean_CI<- function(x, digits=3) {
  result <- paste0(round_new(x[,1],digits ),
                   "(",
                   round_new(x[,2], digits),
                   "-",
                   round_new(x[,3], digits),
                   ")"
  )
  
  return(result)
}