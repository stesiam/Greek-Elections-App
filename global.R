library(shiny)

first_party <- function(...){
  first_party_perc = max(...)
  return(first_party_perc)
}

calculate_bonus <- function(...){
  max = do.call("first_party", list(...))
  if (max >= 25 && max <= 40){
    bonus = 20 + floor((max - 25)/0.5)
  } else if (max > 40){
    bonus = 50
  }
  else{
    bonus = 0
  }
  return(bonus) 
}


calc_seats <- function(p){
  if(p<3){
    seats = 0
  }
  else{
   seats = floor(p/0.5)
  }
  return(seats)
}

df = data.frame(
  "Party" =c("ND", "SYRIZA"),
  "Perc" =c(29,18)
)

find_max_party = function(){
  max = -1
  for (i in 1:2){
   if (df$Perc[i] > max){
      max = df$Perc[i]
      max_party = df$Party[i]
    }
  }
  return(list(max, max_party))
}
