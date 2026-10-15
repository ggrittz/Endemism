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

cell_coverage <- function(species, perms = 100, base.n = 5, base.richness = 5) {
  n <- length(species)
  s_obs <- length(unique(species))
  
  if (n < base.n || s_obs < base.richness) {
    return(list(N = as.numeric(n), 
             S_obs = as.numeric(s_obs), 
             S_pred = NA_real_, 
             completeness = NA_real_, 
             slope = NA_real_, 
             model = NA_character_))
  }
  
  # Accumulation curve with n permutations
  acc <- vapply(seq_len(perms), function(p) {
    ord <- sample(species)
    cumsum(!duplicated(ord))
  }, numeric(n))
  
  # Extracting the perm mean
  mean_curve <- rowMeans(acc)
  x <- seq_len(n)
  
  models <- list()
  fits <- list()
  
  # Clench/Michaelis-Menten
  fits$clench <- tryCatch(
    nls(mean_curve ~ (A * x) / (1 + B * x),
        start = list(A = max(mean_curve), B = 0.01)),
    error = function(e) NULL
  )
  
  # Negative exponential
  fits$exp <- tryCatch(
    nls(mean_curve ~ A * (1 - exp(-B * x)),
        start = list(A = max(mean_curve), B = 0.001)),
    error = function(e) NULL
  )
  
  # Weibull
  fits$weibull <- tryCatch(
    nls(mean_curve ~ A * (1 - exp(-(B * x)^C)),
        start = list(A = max(mean_curve), B = 0.001, C = 0.8),
        control = nls.control(maxiter = 500)),
    error = function(e) NULL
  )
  
  # Logistic
  fits$logistic <- tryCatch(
    nls(mean_curve ~ A / (1 + exp(-B * (x - C))),
        start = list(A = max(mean_curve), B = 0.01, C = n/2),
        control = nls.control(maxiter = 500)),
    error = function(e) NULL
  )
  
  # Gompertz
  fits$gompertz <- tryCatch(
    nls(mean_curve ~ A * exp(-B * exp(-C * x)),
        start = list(A = max(mean_curve) * 1.1, B = 2, C = 0.01),
        control = nls.control(maxiter = 500)),
    error = function(e) NULL
  )
  
  # Remove failed fits
  fits <- fits[!sapply(fits, is.null)]
  
  if (length(fits) == 0) {
    return(list(N = as.numeric(n), 
             S_obs = as.numeric(s_obs), 
             S_pred = NA_real_, 
             completeness = NA_real_, 
             slope = NA_real_, 
             model = NA_character_))
  }
  
  # Calculate AIC for each model
  aics <- sapply(fits, AIC)
  
  # Select best model (lowest AIC)
  best_model <- names(aics)[which.min(aics)]
  best_fit <- fits[[best_model]]
  
  # Extract asymptote and calculate slopes, which are the first derivative of
  # each model's function, aka, the exact rate of change
  if (best_model == "clench") {
    A <- coef(best_fit)["A"]; B <- coef(best_fit)["B"]
    s_pred <- A/B
    slope_now <- A/(1 + B*n)^2
  } else if (best_model == "exp") {
    A <- coef(best_fit)["A"]; B <- coef(best_fit)["B"]
    s_pred <- A
    slope_now <- A*B*exp(-B*n)
  } else if (best_model == "weibull") {
    A <- coef(best_fit)["A"]; B <- coef(best_fit)["B"]; C <- coef(best_fit)["C"]
    s_pred <- A
    slope_now <- A*C*B^C*n^(C-1)*exp(-(B*n)^C)
  } else if (best_model == "logistic") {
    A <- coef(best_fit)["A"]; B <- coef(best_fit)["B"]; C <- coef(best_fit)["C"]
    s_pred <- A
    slope_now <- A*B*exp(-B*(n-C))/(1+exp(-B*(n-C)))^2
  } else if (best_model == "gompertz") {
    A <- coef(best_fit)["A"]; B <- coef(best_fit)["B"]; C <- coef(best_fit)["C"]
    s_pred <- A
    slope_now <- A*B*C*exp(-C*n)*exp(-B*exp(-C*n))
  }
  
  slope_now <- max(0, slope_now)
  
  # Adjusting S pred
  if (s_pred < s_obs) {
    s_pred <- s_obs
  }
  
  # Calculate completeness as obs/pred
  completeness <- 100 * s_obs / s_pred
  completeness <- min(100, completeness)
  
  # A list keeps each value with its own type (c() would turn all into character)
  result <- list(N = as.numeric(n), 
              S_obs = as.numeric(s_obs), 
              S_pred = as.numeric(s_pred),
              completeness = as.numeric(completeness), 
              slope = as.numeric(slope_now), 
              model = as.character(best_model))
  
  return(result)
}
