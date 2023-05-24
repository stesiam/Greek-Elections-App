library(shiny)


nobonus <- function(p, inparl){
  if (p >= 3){
    p_adj = p/inparl
    seats = floor(p_adj * 300)
  }
  else{
    seats = 0
  }
  return(seats)
}



