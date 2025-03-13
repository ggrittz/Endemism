my_getTombo <- function (collection = NULL, accession = NULL, by.coll = TRUE, 
          to.lower = FALSE) 
{
  if (is.null(collection) | is.null(accession)) 
    stop("Please provide at least one collection and one accession")
  if (length(collection) != length(accession)) 
    stop("Collection codes and accession numbers must have the same size")
  collection[collection %in% c("", " ", NA)] <- NA_character_
  accession[accession %in% c("", " ", NA)] <- NA_character_
  nas.col <- is.na(collection)
  nas.acc <- is.na(accession)
  col <- plantR::rmLatin(collection[!nas.acc])
  tmb <- plantR::rmLatin(accession[!nas.acc])
  ids <- grepl("\\D", tmb, perl = TRUE)
  tmb1 <- tmb[ids]
  if (length(tmb1) > 0) {
    col1 <- col[ids]
    tmb1[!grepl("\\d", tmb1)] <- NA
    tmb1 <- gsub("-|\\!", "", tmb1, perl = TRUE)
    tmb1 <- gsub(" ", "", tmb1, fixed = TRUE)
    tmb1 <- gsub("([a-z])\\.([0-9])", "\\1\\2", tmb1, perl = TRUE)
    if (!by.coll) {
      pos <- as.double(regexpr("\\d", tmb1, perl = TRUE))
      pos[is.na(pos)] <- 0
      if (any(pos > 1)) 
        tmb1[pos > 1] <- substring(tmb1[pos > 1], pos[pos > 
                                                        1])
      pos <- as.double(regexpr("[1-9]", tmb1, perl = TRUE))
      pos[is.na(pos)] <- 0
      if (any(pos > 1)) 
        tmb1[pos > 1] <- substring(tmb1[pos > 1], pos[pos > 
                                                        1])
      tmb[ids] <- tmb1
    }
    else {
      pos <- as.double(regexpr("\\d", tmb1, perl = TRUE))
      pos[is.na(pos)] <- 0
      tmb2 <- tmb1[pos > 1]
      
      # Escape special characters in col1
      col1_escaped <- gsub("([][{}()*+?.\\^$|])", "\\\\\\1", col1[pos > 1])
      col2 <- paste0("^", col1_escaped, "+")

      tmb2 <- mapply(function(x, y) {
        gsub(y, "", x, perl = TRUE)
      }, tmb2, col2)
      tmb2[grepl(pattern = "\\d", tmb2, perl = TRUE) & 
             !grepl(pattern = "\\D", tmb2, perl = TRUE)] <- as.numeric(tmb2[grepl(pattern = "\\d", 
                                                                                  tmb2, perl = TRUE) & !grepl(pattern = "\\D", 
                                                                                                              tmb2, perl = TRUE)])
      tmb1[pos > 1] <- tmb2
      pos <- as.double(regexpr("\\d", tmb1, perl = TRUE))
      pos[is.na(pos)] <- 0
      if (any(pos > 1)) 
        tmb1[pos > 1] <- gsub("([a-z])0+([1-9])", "\\1\\2", 
                              tmb1[pos > 1], perl = TRUE)
      pos <- as.double(regexpr("^0", tmb1, perl = TRUE))
      pos[is.na(pos)] <- 0
      if (any(pos == 1)) 
        tmb1[pos == 1] <- gsub("^(0+)([1-9])", "\\2", 
                               tmb1[pos == 1], perl = TRUE)
      tmb[ids] <- tmb1
    }
  }
  tmb[!grepl("\\D", tmb, perl = TRUE)] <- as.numeric(tmb[!grepl("\\D", 
                                                                tmb, perl = TRUE)])
  tmb[!is.na(tmb) & stringr::str_count(tmb, "-") >= 3] <- NA
  numTombo <- paste(col, tmb, sep = "_")
  numTombo1 <- paste(collection[nas.acc & !nas.col], accession[nas.acc & 
                                                                 !nas.col], sep = "_")
  if (to.lower) {
    ids <- grepl("^NA_", numTombo, perl = TRUE)
    numTombo[!ids] <- tolower(numTombo[!ids])
    numTombo[ids] <- paste0("NA_", tolower(gsub("NA_", "", 
                                                numTombo[ids], fixed = TRUE)))
    numTombo1 <- paste0(tolower(gsub("_NA", "", numTombo1, 
                                     fixed = TRUE)), "_NA")
  }
  result <- rep(NA_character_, length(collection))
  result[!nas.acc] <- numTombo
  result[nas.acc & !nas.col] <- numTombo1
  return(result)
}