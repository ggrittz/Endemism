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
