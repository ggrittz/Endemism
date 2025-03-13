my_checkInverted <- function (x, check.names = c("geo.check", "border.check", "shore.check"), 
          country.gazetteer = "country.gazet", lat = "decimalLatitude.new", 
          lon = "decimalLongitude.new", output = "new.col") 
{
  # worldMap <- worldMap
  if (!inherits(x, "data.frame")) 
    stop("Input object needs to be a data frame!")
  if (dim(x)[1] == 0) 
    stop("Input data frame is empty!")
  if (!all(c(lat, lon, country.gazetteer) %in% colnames(x))) 
    stop("One or more column names declared do not match those of the input object: please rename or specify the correct names")
  if (!check.names[1] %in% colnames(x)) 
    stop("The column with the results from the coordinate checking was not found in the input data")
  if (any(grepl("_", x[, country.gazetteer], fixed = TRUE))) {
    country.gazet <- sapply(strsplit(x[, country.gazetteer], 
                                     "_", fixed = TRUE), function(x) x[1])
  }
  else {
    country.gazet <- x[, country.gazetteer]
  }
  x$tmp.order <- 1:nrow(x)
  x$tmp.country.gazet <- country.gazet
  cols <- check.names[check.names %in% names(x)]
  if (check.names[1] %in% cols) {
    check_these <- grepl("inverted|bad_country|open_sea", 
                         x[, check.names[1]], perl = TRUE)
    check_inv1 <- x[check_these, ]
  }
  else {
    check_inv1 <- x[FALSE, ]
  }
  if (check.names[2] %in% cols) {
    check_inv2 <- x[x[, check.names[2]] %in% FALSE, ]
  }
  else {
    check_inv2 <- x[FALSE, ]
  }
  if (check.names[3] %in% cols) {
    check_inv3 <- x[x[, check.names[3]] %in% FALSE, ]
  }
  else {
    check_inv3 <- x[FALSE, ]
  }
  check_inv <- do.call(rbind.data.frame, list(check_inv1, check_inv2, 
                                              check_inv3))
  check_inv <- check_inv[!duplicated(check_inv$tmp.order), 
  ]
  check_inv$check_inv <- NULL
  if (dim(check_inv)[1] == 0) {
    tmp.cols <- c("tmp.order", "tmp.country.gazet")
    x <- x[, -which(names(x) %in% tmp.cols)]
    return(x)
  }
  else {
    cols1 <- c("tmp.order", check.names, "tmp.country.gazet", 
               lon, lat)
    cols1 <- cols1[cols1 %in% names(x)]
    tmp <- check_inv[, cols1]
    tmp$inv_lon <- -tmp[, lon]
    tmp$inv_lat <- -tmp[, lat]
    inv_lon <- "inv_lon"
    inv_lat <- "inv_lat"
    types <- list(invert_lon = c(inv_lon, lat), invert_lat = c(lon, 
                                                               inv_lat), invert_both = c(inv_lon, inv_lat), transposed = c(lat, 
                                                                                                                           lon), transp_inv_lon = c(lat, inv_lon), transp_inv_lat = c(inv_lat, 
                                                                                                                                                                                      lon), transp_inv_both = c(inv_lat, inv_lon))
    trans.data <- vector("list", length(types))
    names(trans.data) <- names(types)
    for (i in seq_along(types)) trans.data[[i]] <- sf::st_as_sf(tmp, 
                                                                coords = types[[i]])
    check <- dplyr::bind_rows(trans.data, .id = "types")
    bad.lat <- abs(sf::st_coordinates(check)[, 2]) > 90
    if (any(bad.lat)) 
      check <- check[!bad.lat, ]
    check <- sf::st_set_crs(check, sf::st_crs(worldMap))
    check1 <- suppressMessages(sf::st_join(check, worldMap, 
                                           join = sf::st_intersects))
    check1 <- check1[!is.na(check1$NAME_0), ]
    if (dim(check1)[1] > 0) {
      check_these <- check1$tmp.country.gazet == check1$NAME_0
      
      ##### Adding a na.rm = TRUE #####
      
      if (any(check_these, na.rm = TRUE)) {
        new.coords <- sf::st_coordinates(check1)
        colnames(new.coords) <- c(lon, lat)
        new.coords <- new.coords[check_these %in% TRUE, 
                                 , drop = FALSE]
        sf::st_geometry(check1) <- NULL
        cols2 <- c("types", "tmp.order", check.names)
        cols2 <- cols2[cols2 %in% names(check1)]
        check1 <- check1[check_these %in% TRUE, cols2]
        check1[, check.names[1]] <- paste0("ok_country[", 
                                           check1$types, "]")
        if (length(cols2) >= 4) 
          check1[, check.names[2]] <- NA_character_
        if (length(cols2) == 5) 
          check1[, check.names[3]] <- NA_character_
        check1 <- cbind.data.frame(check1[, -1], new.coords, 
                                   stringsAsFactors = FALSE)
      }
      else {
        check1 <- NULL
      }
    }
    else {
      check1 <- NULL
    }
    if (is.null(check1)) {
      if (output == "new.col") {
        x[, paste0(check.names[1], ".new")] <- gsub("bad_country[inverted?]", 
                                                    "bad_country", x[, check.names[1]], fixed = TRUE)
        if (check.names[2] %in% colnames(x)) 
          x[, paste0(check.names[2], ".new")] <- x[, 
                                                   check.names[2]]
        if (check.names[3] %in% colnames(x)) 
          x[, paste0(check.names[3], ".new")] <- x[, 
                                                   check.names[3]]
      }
      if (output == "same.col") {
        x[, check.names[1]] <- gsub("bad_country[inverted?]", 
                                    "bad_country", x[, check.names[1]], fixed = TRUE)
      }
      tmp.cols <- c("tmp.order", "tmp.country.gazet")
      x <- x[, -which(names(x) %in% tmp.cols)]
      return(x)
    }
    else {
      y <- dplyr::left_join(x, check1, by = "tmp.order", 
                            suffix = c("", ".new"))
      tmp.cols <- c("tmp.order", "tmp.country.gazet")
      sub.cols <- names(check1)[!names(check1) %in% tmp.cols]
      sub.cols.new <- paste0(sub.cols, ".new")
      if (output == "new.col") {
        sub.rows <- x$tmp.order %in% check1$tmp.order
        x[sub.rows, sub.cols.new] <- y[sub.rows, sub.cols.new]
        x[!sub.rows, sub.cols.new] <- y[!sub.rows, sub.cols]
        x[, paste0(check.names[1], ".new")] <- gsub("bad_country[inverted?]", 
                                                    "bad_country", x[, paste0(check.names[1], ".new")], 
                                                    fixed = TRUE)
        if (check.names[2] %in% colnames(x)) 
          x[, paste0(check.names[2], ".new")] <- as.logical(x[, 
                                                              paste0(check.names[2], ".new")])
        if (check.names[3] %in% colnames(x)) 
          x[, paste0(check.names[3], ".new")] <- as.logical(x[, 
                                                              paste0(check.names[3], ".new")])
      }
      if (output == "same.col") {
        sub.rows <- x$tmp.order %in% check1$tmp.order
        x[sub.rows, sub.cols] <- y[sub.rows, sub.cols.new]
        x[, check.names[1]] <- gsub("bad_country[inverted?]", 
                                    "bad_country", x[, check.names[1]], fixed = TRUE)
      }
      x <- x[order(x$tmp.order), ]
      tmp.cols <- c("tmp.order", "tmp.country.gazet")
      x <- x[, -which(names(x) %in% tmp.cols)]
      return(x)
    }
  }
}
