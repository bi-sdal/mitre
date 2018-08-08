library(httr)
library(stringr)

#' Geocode an address using the Google API
#' 
#' Takes an address and returns the latitude and longidude coordinates
#' 
#' @param address: string, the address to geocode
#' @param api_key: string, google maps API key
#' 
#' @return named numeric vector of latiude and longitude
#' 
#' @export
#' 
#' @example 
#' geocode_address('1600 Amphitheatre Parkway, Mountain View, CA')
geocode_address <- function(address, api_key = sdalr::get_google_maps_key()) {
  address <- stringr::str_replace_all(string = address, pattern = '\\s', replacement = '+')
  res <- httr::GET(sprintf('https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s',
                     address,
                     api_key))
  con <- httr::content(res)
  return(c(lat = con$results[[1]]$geometry$location$lat,
           lon = con$results[[1]]$geometry$location$lng)
         )
}
