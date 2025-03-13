#' 
#' @title Remove Unwanted Spaces
#' 
#' @param x a character or vector
#'
#' @return the character `x` without trailing or double spaces
#'  
#' @keywords internal
#'
#' @noRd
#' 
.squish <- function (x) {
  x <- gsub("\\s\\s+", " ", as.character(x), perl = TRUE)
  x <- gsub("^ | $", "", x, perl = TRUE)
  return(x)
}
#' @title Convert Columns Stored as List or Logical to Characters
#' 
#' @description Simple function to solve some colunms storing characters as list
#'   or logical during import or download
#' 
#' @param x a data frame.
#'
#' @return The value of `x` with columns stored as list/logical converted to
#'   characters.
#' 
#' @keywords internal
#'
#' @noRd
#' 
.cols_to_char <- function(x) {
  
  if (!class(x)[1] %in% "data.frame")
    x <- as.data.frame(x)
  
  check_these <- sapply(x, class) %in% "list"
  if (any(check_these)) {
    colunas <- colnames(x)[check_these]
    for(i in seq_along(colunas)) {
      info <- as.character(x[[colunas[i]]])
      info[info %in% "NULL"] <- NA_character_
      x[[colunas[i]]] <- info
    }
  }
  
  check_these <- sapply(x, class) %in% "logical"
  if (any(check_these)) {
    colunas <- colnames(x)[check_these]
    for(i in seq_along(colunas)) {
      info <- as.character(x[[colunas[i]]])
      info[info %in% "NULL"] <- NA_character_
      x[[colunas[i]]] <- info
    }
  }
  
  return(x)  
}

#' @title Remove ponctuation, special characters, numbers and spaces
#' 
#' @description Simple function to prepare vectors of characters for
#'   exact and fuzzy matching
#' 
#' @param x a vector
#'
#' @keywords internal
#'
#' @importFrom tm removePunctuation
#' @importFrom plantR rmLatin
#' 
#' @noRd
#' 
.clean_char <- function(x) {
  
  x1 <- tolower(gsub(" ", "", x, fixed =  TRUE))
  x1 <- tm::removePunctuation(x1)
  x1 <- plantR:::rmLatin(x1)
  x1 <- gsub("×", "", x1, fixed = TRUE)
  x1 <- gsub("[0-9]", "", x1, perl = TRUE)
  return(x1)
  
}
#' 
#' @title Build Organism Name
#' 
#' @description Combine different columns with species name
#'   information (i.e. genus, epiteth, infra-epiteth, author) into a
#'   single organism name
#' 
#' @param x the data frame with the taxonomic information
#' @param col.names the name of the columns containing the information
#'   to be combined in the desired order of output
#'
#' @return a vector of names combining the information in 'col.names' 
#'  
#' @keywords internal
#' 
#' @noRd
#' 
.build_name <- function(x, col.names = c("genus", "species")) {
  
  if (any(!col.names %in% colnames(x)))
    stop("One or more names in 'col.names' were not found in 'x'")
  
  cols <- names(x)[match(col.names, names(x), nomatch = 0)]
  
  if (length(cols) > 1) {
    
    organismName <- do.call(paste, x[, cols])
    organismName[organismName %in% 
                   c("NA NA", "NA NA NA",
                     "NULL NULL", "NULL NULL NULL")] <- NA_character_
    organismName <- gsub(" NA NA$| NULL NULL$", "", organismName, 
                         perl = TRUE)
    organismName <- gsub(" NA$| NULL$", "", organismName, 
                         perl = TRUE)
    organismName <- gsub(" NA ", " ", organismName, 
                         fixed = TRUE)
    organismName <- .squish(organismName)
    return(organismName)
  } else {
    return(.squish(x[[cols]]))
  }    
}
#' 
#' @title Remove Unwanted Spaces
#' 
#' @param x a character or vector
#'
#' @return the character `x` without trailing or double spaces
#'  
#' @keywords internal
#'
#' @noRd
#' 
.squish <- function (x) {
  x <- gsub("\\s\\s+", " ", as.character(x), perl = TRUE)
  x <- gsub("^ | $", "", x, perl = TRUE)
  return(x)
}
#'
#' @title Remove punctuation, special characters, numbers and spaces
#'
#' @description Simple function to prepare vectors of taxon names for
#'   increasing the chances of exact and fuzzy name matching
#'
#' @param x a vector of characters (i.e. names)
#'
#' @return the vector \code{x} without punctuation, special
#'   characters, numbers and spaces
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#'
#' \dontrun{
#' taxa <- c("Genus \u00d7 epiteth (Jos\u00e9 AB.) C.")
#' print(taxa)
#' cleanName(taxa)
#' }
#'
#' @keywords internal
#'
#' @importFrom stringr str_to_lower
#'
.cleanName <- function(x) {
  
  x1 <- gsub(" ", "", x, fixed =  TRUE)
  x1 <- stringr::str_to_lower(x1)
  x1 <- gsub("[[:punct:]]", "", x1, perl =  TRUE)
  x1 <- plantR::rmLatin(x1)
  x1 <- gsub("\u00d7", "", x1, fixed = TRUE)
  x1 <- gsub("[0-9]", "", x1, perl = TRUE)
  
  return(x1)
}