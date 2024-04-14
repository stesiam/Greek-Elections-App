
a = data.frame(
  "Party" = c("ND", "SYRIZA", "PASOK","KKE","Ellisi", "Niki", "Plefsi", "Mera", "Other1", "Other2"),
  "Perc" = c(18, 30, 11, 8, 7, 3, 3, 4, 3, 2)
)


calculate_seats = function(a){
  
  
  inparl = inparliament(a)
  pr = allocated_prop_seats(max(a$Perc))
  
 s = a |>
    mutate(
      id = 1:nrow(a),
      AdjPerc = ifelse(Perc >= 3, Perc / inparl, 0),
           Seats = pr * AdjPerc,
           IntSeats = floor(Seats),
           RestSeats = Seats - IntSeats) |>
   arrange(-IntSeats)
  
   s$IntSeats[1] <- s$IntSeats[1] + (300 - pr)
  
  unallocated_seats = 300 - sum(s$IntSeats)
  
  s = s |>
    arrange(-RestSeats)
  
  for (i in 1:unallocated_seats){
    s$intSeats[i] = s$intSeats[i] +1
  }
  
  s = s|>
    arrange(id) |>
    select(-c(AdjPerc, Seats, RestSeats))
  
  return(s)
}



allocated_prop_seats = function(first_party_pct){
#' A function to calculate the seats that will be allocated proportionally
#' It is based on the percentage of the first party
  
  
  if (first_party_pct < 25){
    prop_seats_aloc = 300
  } else if (first_party_pct > 25 & first_party_pct < 40){
    prop_seats_aloc = 300 - 20 - floor((first_party_pct - 25)/0.5)
  } else{
    prop_seats_aloc = 250
  }
  
  return(prop_seats_aloc)
}

unallocated = function(data){
  
  
  return( 100 - sum(data$Perc))
}

inparliament = function(data){
  
  data = data  |>
    dplyr::filter(Perc >=3)
  
  
  return( sum(data$Perc))
}

bonus40 = function(data){
  
  
  bi = data |>
    dplyr::mutate(
      id = 1:nrow(data),
      AdjProp = ifelse(Perc >= 3, Perc/inparliament(data), 0),
      Seats = 260*AdjProp,
      IntSeats = floor(Seats),
      digitSeats = Seats - IntSeats) |>
    dplyr::arrange(-digitSeats)
  
  ipol = sum(bi$Seats) - sum(bi$IntSeats)
  
  for (i in 1:(260-sum(bi$IntSeats))){
    bi$IntSeats[i] = bi$IntSeats[i] + 1 
  }
  
  bi = bi |>
    arrange(-IntSeats)
  
  bi$IntSeats[1] = bi$IntSeats[1] + 40
  
  
  bi = bi |>
    arrange(id)
  
  return(bi)
  
}

bonus50 = function(data){
  
  
  bi = data |>
    dplyr::mutate(
      id = 1:nrow(data),
      AdjProp = ifelse(Perc >= 3, Perc/inparliament(data), 0),
      Seats = 250*AdjProp,
      IntSeats = floor(Seats),
      digitSeats = Seats - IntSeats) |>
    dplyr::arrange(-digitSeats)
  
  ipol = sum(bi$Seats) - sum(bi$IntSeats)
  
  for (i in 1:(250-sum(bi$IntSeats))){
    bi$IntSeats[i] = bi$IntSeats[i] + 1 
  }
  
  bi = bi |>
    arrange(-IntSeats)
  
  bi$IntSeats[1] = bi$IntSeats[1] + 50
  
  
  bi = bi |>
    arrange(id)
  
  return(bi)
  
}

nobonus = function(data){
  
  
  bi = data |>
    dplyr::mutate(
      id = 1:nrow(data),
      AdjProp = ifelse(Perc >= 3, Perc/inparliament(data), 0),
      Seats = 300*AdjProp,
      IntSeats = floor(Seats),
      digitSeats = Seats - IntSeats) |>
    dplyr::arrange(-digitSeats)
  
  ipol = sum(bi$Seats) - sum(bi$IntSeats)
  
  for (i in 1:(300-sum(bi$IntSeats))){
    bi$IntSeats[i] = bi$IntSeats[i] + 1 
  }
  
  bi = bi |>
    arrange(id)
  
  return(bi)
  
}
