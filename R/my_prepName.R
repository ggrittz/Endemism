my_prepName <- function (x, fix.names = TRUE, output = "all", treat.prep = NULL, 
          add.treat = TRUE, sep.in = c(";", "&", "|", " e ", " y ", 
                                       " and ", " und ", " et "), sep.out = "|", special.char = FALSE, 
          format = "last_init", pretty = TRUE, get.prep = FALSE, get.initials = TRUE) 
{
  
  # All NA cases...it can happen
  if(all(is.na(x)))
    return(x)
  
  tmp.ordem <- NULL
  if (is.null(x) | !class(x)[1] %in% "character") 
    stop("Input need to be a string or vector of names")
  bracks <- grepl("^\\[", x, perl = TRUE) & grepl("\\]$", x, 
                                                  perl = TRUE)
  if (any(bracks)) 
    x[bracks] <- gsub("^\\[|\\]$|^\\(|\\)$", "", x[bracks], 
                      perl = TRUE)
  
  # Remove all parenthesis if they are not, at the same time, in the beginning and end
  # of the string: e.g., 'Sobrinho, J. de P.L. (no. 1441)'
  # I couldn't find a more straightforward solution that also keeps the info inside the 
  # parenthesis '(no. 1441)' in the case above
  x <- ifelse(grepl("(?<!^)\\(", x, perl = TRUE) | 
                grepl("\\)(?!$)", x, perl = TRUE), gsub("[()]", "", x), x)
  
  parent <- grepl("^\\(", x, perl = TRUE) & grepl("\\)$", x, 
                                                  perl = TRUE)
  if (any(parent)) 
    x[parent] <- gsub("^\\[|\\]$|^\\(|\\)$", "", x[parent], 
                      perl = TRUE)
  if (fix.names) 
    x <- plantR:::fixName(x, sep.in = sep.in, sep.out = sep.out, special.char = special.char)
  if (!is.null(treat.prep)) {
    if (add.treat) {
      treat.prep.final <- unique(c(plantR:::treatPreps, treat.prep))
    }
    else {
      treat.prep.final <- unique(as.character(treat.prep))
    }
  }
  else {
    treat.prep.final <- plantR:::treatPreps
  }
  patt.treat <- gsub("\\.", "\\\\.", paste(treat.prep.final, 
                                           collapse = " |"))
  if (!grepl("\\|", patt.treat)) 
    patt.treat <- paste0(patt.treat, " ")
  x <- gsub(patt.treat, "", x, perl = TRUE)
  sep.in1 <- gsub("(\\|)", "\\\\\\1", sep.in, perl = TRUE)
  sep.in1 <- gsub("(\\+)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\$)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\.)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\?)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.in1 <- gsub("(\\*)", "\\\\\\1", sep.in1, perl = TRUE)
  sep.out1 <- gsub("(\\|)", "\\\\\\1", sep.out, perl = TRUE)
  sep.out1 <- gsub("(\\+)", "\\\\\\1", sep.out1, perl = TRUE)
  sep.out1 <- gsub("(\\$)", "\\\\\\1", sep.out1, perl = TRUE)
  sep.out1 <- gsub("(\\.)", "\\\\\\1", sep.out1, perl = TRUE)
  sep.out1 <- gsub("(\\?)", "\\\\\\1", sep.out1, perl = TRUE)
  sep.out1 <- gsub("(\\*)", "\\\\\\1", sep.out1, perl = TRUE)
  if (fix.names) {
    patt.split <- paste(sep.out1, paste0(gsub("^ | $", "", 
                                              sep.out1, perl = TRUE), "(?=\\p{Lu})"), sep = "|")
  }
  else {
    patt.split <- paste0(sep.in1, collapse = "|")
  }
  split <- strsplit(x, patt.split, perl = TRUE)
  DT <- data.table::setDT(data.table::transpose(split, fill = NA))
  dt <- as.data.frame(DT)
  cols <- colnames(dt)
  if (output %in% c("all", "first")) {
    dt$V1 <- my_prepTDWG(dt$V1, format = format, pretty = pretty, 
                      get.prep = get.prep, get.initials = get.initials, 
    )
  }
  if (output %in% c("all", "aux") & length(cols) > 1) {
    cols1 <- cols[!cols %in% c("V1", "tmp.ordem")]
    for(i in cols1)
      dt[, i] <- my_prepTDWG(dt[, i],
                                  format = format, pretty = pretty, 
                                  get.prep = get.prep, get.initials = get.initials)
    
  }
  if (output == "first") {
    names.out <- as.character(dt$V1)
  }
  if (output == "aux" & length(cols) == 1) {
    names.out <- rep(NA_character_, dim(dt)[1])
  }
  if (output == "aux" & length(cols) > 1) {
    cols1 <- cols[!cols %in% c("V1", "tmp.ordem")]
    if (length(cols1) == 1) {
      names <- dt[, cols1]
    }
    else {
      names <- apply(dt[, cols1], 1, paste, collapse = sep.out)
      names <- gsub(paste0(sep.out1, "NA"), "", names, 
                    perl = TRUE)
    }
    names <- gsub("NA", NA_character_, names, fixed = TRUE)
    names.out <- as.character(names)
  }
  if (output == "all" | !output %in% c("first", "aux")) {
    if (length(cols) > 1) {
      names <- apply(dt[, cols], 1, paste, collapse = sep.out)
      names <- gsub(paste0(sep.out1, "NA"), "", names, 
                    perl = TRUE)
      names <- gsub("NA", NA_character_, names, fixed = TRUE)
      names.out <- as.character(names)
    }
    else {
      names.out <- as.character(dt$V1)
    }
  }
  if (any(bracks)) 
    names.out[bracks] <- paste("[", names.out[bracks], "]", 
                               sep = "")
  if (any(parent)) 
    names.out[parent] <- paste("(", names.out[parent], ")", 
                               sep = "")
  return(names.out)
}
