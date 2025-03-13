my_shares_border <- function(country1 = "ireland", country2 = "united kingdom") {
  world <- plantR::world
  w <- world

  if (!country1 %in% w$name) 
    country1 <- plantR::prepLoc(plantR::prepCountry(country1))
  if (!country2 %in% w$name) 
    country2 <- plantR::prepLoc(plantR::prepCountry(country2))
  if (!country1 %in% w$name | !country2 %in% w$name) 
    stop(paste0("Country name(s) '", c(country1, 
                                       country2)[!c(country1,
                                                    country2) %in% w$name], "' not found"))
  v <- w[w$name %in% c(country1), ]
  z <- w[w$name %in% c(country2), ]
  intersects <- sf::st_intersects(v, z, sparse = FALSE)[1, 1]
  return(intersects)
}

my_shares_border(country1 = 'united kingdom', country2 = 'ireland')
