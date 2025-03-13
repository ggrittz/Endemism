my_checkBorders <- function (x, geo.check = "geo.check", country.shape = "NAME_0", 
          country.gazetteer = "loc.correct", output = "new.col") 
{
  if (!class(x)[1] == "data.frame") 
    stop("Input object needs to be a data frame!")
  if (!all(c(geo.check, country.shape, country.gazetteer) %in% 
           colnames(x))) 
    stop("One or more column names declared do not match those of the input object: please rename or specify the correct names")
  if (any(grepl("_", x[, country.shape], fixed = TRUE))) {
    country.shp <- sapply(strsplit(x[, country.shape], "_", 
                                   fixed = TRUE), function(x) x[1])
  }
  else {
    country.shp <- x[, country.shape]
  }
  if (any(grepl("_", x[, country.gazetteer], fixed = TRUE))) {
    country.gazet <- sapply(strsplit(x[, country.gazetteer], 
                                     "_", fixed = TRUE), function(x) x[1])
  }
  else {
    country.gazet <- x[, country.gazetteer]
  }
  check_these <- grepl("bad_country", x[, geo.check], perl = TRUE)
  if (any(check_these)) {
    shares_bord <- Vectorize(my_shares_border)
    share_border <- suppressMessages(suppressWarnings(shares_bord(country.shp[check_these], 
                                                                  country.gazet[check_these])))
    if (output == "new.col") {
      x$border.check <- NA
      x$border.check[check_these] <- share_border
    }
    if (output == "same.col") {
      x[check_these, geo.check][share_border] <- "bad_country[border]"
      x[check_these, geo.check][!share_border] <- "bad_country[inverted?]"
    }
  }
  else {
    if (output == "new.col") 
      x$border.check <- FALSE
  }
  return(x)
}
