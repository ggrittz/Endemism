my_checkCoord <- function (x, lon = "decimalLongitude.new", lat = "decimalLatitude.new", 
    str.name = "loc.correct", orig.coord = "origin.coord", low.map = "plantR", 
    high.map = "plantR", res.gazet = "resolution.gazetteer", 
    dist.center = FALSE, lon.gazet = "longitude.gazetteer", lat.gazet = "latitude.gazetteer", 
    keep.cols = c("geo.check", "distCentroid_m")) 
{
    NAME_0 <- pais_latam <- pais_wo <- NULL
    worldMap <- plantR::worldMap
    latamMap <- plantR::latamMap
    if (!class(x)[1] == "data.frame") 
        stop("Input object needs to be a data frame!")
    if (dim(x)[1] == 0) 
        stop("Input data frame is empty!")
    if (!all(c(lat, lon, str.name, orig.coord) %in% colnames(x))) 
        stop("One or more column names declared do not match those of the input object: please rename or specify the correct names")
    if (dist.center) 
        if (any(!c(lon.gazet, lat.gazet) %in% names(x))) 
            stop("If 'dist.center' is TRUE, the input must contain the longitude/latitude obtained from a gazetteer: please rename or specify the correct names")
    cols.x <- names(x)
    x$tmp.order <- 1:nrow(x)
    x[, str.name][x[, str.name] %in% "no_loc"] <- NA
    x <- tidyr::separate(data = x, col = str.name, sep = "_", 
        into = c("country.gazet", "state.gazet", "county.gazet", 
            "locality.gazet", "sublocality.gazet"), remove = FALSE, 
        fill = "right")
    geo.check <- rep(NA_character_, dim(x)[1])
    ids.gazet <- x[, orig.coord] %in% "coord_gazet"
    if (any(ids.gazet)) 
        geo.check[ids.gazet] <- paste0("ok_", x[ids.gazet, res.gazet], 
            "_gazet")
    
    # Adding the OR condition for cases where origin.coord is coord_original
    # but the lon OR lat are still NA even after using the gazetteer. 
    # These ones need to be no_cannot_check too
    ids.no.coord <- x[, orig.coord] %in% "no_coord" |
      (x[, orig.coord] %in% "coord_original" & (is.na(x[lon]) | is.na(x[lat])))
      
    if (any(ids.no.coord)) 
        geo.check[ids.no.coord] <- "no_cannot_check"
    tmp <- x[is.na(geo.check), ]
    tmp <- sf::st_as_sf(tmp, coords = c(lon, lat))
    prj <- sf::st_crs(4326)
    tmp <- sf::st_set_crs(tmp, prj)
    class.low.map <- class(low.map)[1]
    low_map <- NULL
    if (class.low.map == "character") {
        if (any(low.map %in% c("plantR", "plantr"))) {
            low_map <- worldMap
        }
        else {
            stop("Please chose between the default map or a user-provided map")
        }
    }
    if (class.low.map == "list") {
        if (all(sapply(low.map, function(x) class(x)[1]) %in% 
            "sf")) {
            if (!"NAME_0" %in% names(low.map[[1]])) 
                stop("The sf objects must have a column 'NAME_0': the lowest administrative level")
            low_map <- dplyr::bind_rows(low.map)
        }
        else {
            stop("The user-provided map must be an sf object or a list of sf objects")
        }
    }
    if (class.low.map == "sf") {
        if (!"NAME_0" %in% names(low.map)) 
            stop("The global map must have a column 'NAME_0': the lowest administrative level")
        low_map <- low.map
    }
    if (is.null(low_map)) 
        stop("The user-provided map must be an sf object or a list of sf objects")
    tmp <- suppressMessages(sf::st_join(tmp, low_map, join = sf::st_intersects))
    names(tmp)[which(names(tmp) == "NAME_0")] <- "pais_wo"
    check_these <- grepl("\\.[0-9]", rownames(tmp))
    if (any(check_these)) {
        tmp$keep_these <- rep(TRUE, dim(tmp)[1])
        dup.orders <- tmp$tmp.order[check_these]
        for (i in seq_along(dup.orders)) {
            dups.i <- tmp[tmp$tmp.order %in% dup.orders[i], ]
            dups.i$keep_these[dups.i$country.new != dups.i$pais_wo] <- FALSE
            if (all(dups.i$keep_these)) 
                dups.i$keep_these[-1] <- FALSE
            tmp$keep_these[tmp$tmp.order %in% dup.orders[i]] <- dups.i$keep_these
        }
        tmp <- tmp[tmp$keep_these, ]
    }
    geo.check[is.na(geo.check)][is.na(tmp$pais_wo)] <- "sea"
    class.high.map <- class(high.map)[1]
    high_map <- NULL
    if (class.high.map == "character") {
        if (any(high.map %in% c("plantR", "plantr"))) {
            high_map <- dplyr::bind_rows(latamMap)
        }
        else {
            stop("Please chose between the default map or a user-provided map")
        }
    }
    if (class.high.map == "list") {
        if (all(sapply(high.map, function(x) class(x)[1]) %in% 
            "sf")) {
            if (!all(c("NAME_0", "NAME_1") %in% names(high.map[[1]]))) 
                stop("Ideally, the high resolution map should have the columns 'NAME_0', 'NAME_1', 'NAME_2' and 'NAME_3'")
            high_map <- dplyr::bind_rows(high.map)
        }
        else {
            stop("The user-provided map must be an sf object or a list of sf objects")
        }
    }
    if (class.high.map == "sf") {
        if (!all(c("NAME_0", "NAME_1") %in% names(high.map[[1]]))) 
            stop("Ideally, the high resolution map should have the columns 'NAME_0', 'NAME_1', 'NAME_2' and 'NAME_3'")
        high_map <- high.map
    }
    if (is.null(high_map)) 
        stop("The user-provided map must be an sf object or a list of sf objects")
    x2 <- suppressMessages(sf::st_join(tmp, high_map, join = sf::st_intersects))
    check_these <- grepl("\\.[0-9]", rownames(x2))
    if (any(check_these)) {
        x2$keep_these <- rep(TRUE, dim(x2)[1])
        dup.orders <- x2$tmp.order[check_these]
        for (i in seq_along(dup.orders)) {
            dups.i <- x2[x2$tmp.order %in% dup.orders[i], ]
            dups.i$keep_these[dups.i$country.new != dups.i$pais_wo] <- FALSE
            if (all(dups.i$keep_these)) 
                dups.i$keep_these[-1] <- FALSE
            x2$keep_these[x2$tmp.order %in% dup.orders[i]] <- dups.i$keep_these
        }
        x2 <- x2[x2$keep_these, ]
    }
    x2 <- dplyr::rename(x2, pais_latam = NAME_0)
    x2 <- dplyr::mutate(x2, NAME_0 = dplyr::if_else(is.na(pais_wo) & 
        !is.na(pais_latam), pais_latam, pais_wo))
    x2$loc.coord <- paste(x2$NAME_0, x2$NAME_1, x2$NAME_2, sep = "_")
    x2$loc.coord[x2$loc.coord %in% "NA_NA_NA"] <- NA_character_
    x2$loc.coord <- gsub("_NA_NA$", "", x2$loc.coord, perl = TRUE)
    x2$loc.coord <- gsub("_NA$", "", x2$loc.coord, perl = TRUE)
    x3 <- suppressMessages(dplyr::left_join(x, x2[, c("tmp.order", 
        "NAME_0", "NAME_1", "NAME_2", "NAME_3", "loc.coord")]))
    x3$country.check <- dplyr::if_else(x3$country.gazet == x3$NAME_0, 
        "ok_country", "bad_country", missing = "no_country")
    x3$state.check <- dplyr::if_else(x3$state.gazet == x3$NAME_1, 
        "ok_state", "bad_state", missing = "no_state")
    x3$county.check <- dplyr::if_else(x3$county.gazet == x3$NAME_2, 
        "ok_county", "bad_county", missing = "no_county")
    tmp1 <- apply(x3[, c("country.check", "state.check", "county.check")], 
        1, paste, collapse = "/")
    geo.check[is.na(geo.check)] <- tmp1[is.na(geo.check)]
    repl.check <- plantR:::simpGeoCheck
    geo.check <- stringr::str_replace_all(geo.check, repl.check)
    if (dist.center) {
        x3$distCentroid_m <- NA
        if (dim(tmp)[1] > 0) {
            ids.dist <- !grepl("ok_county|ok_locality|no_cannot_check", 
                x3$geo.check) & !(is.na(x3[, lon.gazet]) | is.na(x3[, 
                lat.gazet]))
            tmp2 <- x3[ids.dist, c(lat, lon, lat.gazet, lon.gazet, 
                "distCentroid_m"), ]
            tmp2$distCentroid_m <- spatialrisk::haversine(tmp2[, 
                1], tmp2[, 2], tmp2[, 3], tmp2[, 4])
            x3$distCentroid_m[ids.dist] <- tmp2$distCentroid_m
        }
    }
    x3 <- x3[order(x3$tmp.order), ]
    x3$geo.check <- geo.check
    x3 <- x3[, -which(names(x3) %in% c("tmp.order"))]
    new.cols <- names(x3)[!names(x3) %in% cols.x]
    if (!is.null(keep.cols)) 
        new.cols <- new.cols[new.cols %in% keep.cols]
    tmp.cols <- c("tmp.order", "country.gazet", "state.gazet", 
        "county.gazet", "locality.gazet", "sublocality.gazet")
    x <- x[, -which(names(x) %in% tmp.cols)]
    if (length(new.cols) == 0) {
        return(x)
    }
    else {
        x4 <- x3[, which(names(x3) %in% new.cols), drop = FALSE]
        return(cbind.data.frame(x, x4))
    }
}