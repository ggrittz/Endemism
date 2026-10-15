#' Get Species Accumulation
#' 
#' @description 
#' This is a simple script to get the year of description of each
#' species and the rate of species accumulation across time
#' 
#' @author Renato Lima \email{raflima@usp.br}
#' 
#' @importFrom plantR fixAuthors
#' @importFrom dplyr left_join
#' 
#' @date 2025/10/13
#' 
# Load and preparing the species list -----------------------------------------------------------------
# Neotropical endemics saved by 11_preparing_checklist.R
check_file <- here::here("data", "derived-data", "nt_species.rds")
# check <- readRDS(check_file)
check <- as.data.frame(readRDS(check_file))
names(check) <- "scientificName"
# isolating taxon and author names for name matching
sci_name <- "scientificName"
# tmp <- plantR::fixAuthors(check[[sci_name]])
tmp <- plantR::fixAuthors(check$scientificName)
names(tmp) <- c(sci_name, "scientificName.new", 
                "scientificNameAuthorship.new")
check <- dplyr::left_join(check, tmp, by = sci_name)
# name matching to get the WCVP ID using plantR
data("wcvpNames", package = "plantRdata")
tmp <- plantR::prepSpecies(check, 
                           tax.names = c("scientificName.new", "scientificNameAuthorship.new"),
                           db = wcvpNames, 
                           replace.names = FALSE, 
                           split.letters = TRUE, drop.cols = c(""))
# inspecting the match to make sure all is fine
table(tmp$tax.notes)
col2check <- c("scientificName", "scientificNameFull", "match_type")
head(tmp[tmp$tax.notes %in% "author misspelled", col2check], 10)
tail(tmp[tmp$tax.notes %in% "author misspelled", col2check], 10)          # looks fine
head(tmp[tmp$tax.notes %in% "name misspelled", col2check], 10)            # looks fine
head(tmp[tmp$tax.notes %in% "author misspelled|synonym", col2check], 10)  # looks fine
head(tmp[tmp$tax.notes %in% "check +1 name", col2check], 10)              # cannot solve (get wcvp ID manually?)
head(tmp[tmp$tax.notes %in% "check not resolved", col2check], 10)         # looks fine (not reslved in WCVP)
head(tmp[tmp$tax.notes %in% "orthographic variant", col2check], 10)       # looks weird (get pub year elsewhere)
head(tmp[tmp$tax.notes %in% "synonym", col2check], 10)                    # looks fine
check$plant_name_id <- gsub("wcvp-", "", tmp$id)
check$plant_name_id[tmp$tax.notes %in% c("check +1 name", "orthographic variant")] <- NA

# Some few manual replacements
# scientificNames in the plantR::checkList function do not have the authorship!
# For now, I will not modify here since there are only a few species...(27/02/2026)
check$plant_name_id[check$scientificName %in% "Schinus microphyllus I.M.Johnst."] <- 2480197
check$plant_name_id[check$scientificName %in% "Miconia theizans (Bonpl.) Cogn."] <- 3021066
check$plant_name_id[check$scientificName %in% "Meriania paniculata Cogn."] <- 3018623 # ok! double-checked
check$plant_name_id[check$scientificName %in% "Blakea cuatrecasii Gleason"] <- 3021378  # ok! double-checked
check$plant_name_id[check$scientificName %in% "Guarea pubescens A.Juss."] <- 2834135
check$plant_name_id[check$scientificName %in% "Ficus crassinervia Desf."] <- 2810070
check$plant_name_id[check$scientificName %in% "Ouratea parviflora Engl."] <- 2400519 # ok! double-checked
check$plant_name_id[check$scientificName %in% "Cleyera theaeoides (Sw.) Choisy"] <- 2727806
check$plant_name_id[check$scientificName %in% "Pouteria macrocarpa D.Dietr."] <- 164246
check$plant_name_id[check$scientificName %in% "Solanum campaniforme Roem. & Schult."] <- 3031602
check$plant_name_id[check$scientificName %in% "Solanum laevigatum Dunal"] <- 3032083 # ok! double-checked
check$plant_name_id[check$scientificName %in% "Solanum arboreum Dunal"] <- 3031502
check$plant_name_id[check$scientificName %in% "Sebastiania brevifolia Klotzsch"] <- 189982 # ok! double-checked
check$plant_name_id[check$scientificName %in% "Ficus ulei Warb."] <- 2812688


## Getting the year of first publication ---------------------------------------------------------------------
wcvp <- as.data.frame(rWCVPdata::wcvp_names)

# standardizing dates
wcvp$first_published <- gsub("\\)\\(", " ", wcvp$first_published, perl = TRUE)
wcvp$first_published <- gsub("\\(|\\)", "", wcvp$first_published, perl = TRUE)
wcvp$first_published <- gsub("^ | $", "", wcvp$first_published, perl = TRUE)
wcvp$first_published <- gsub(".* ", "", wcvp$first_published, perl = TRUE)
wcvp$first_published <- gsub(".*-", "", wcvp$first_published, perl = TRUE)
wcvp$first_published <- gsub(".*\\.", "", wcvp$first_published, perl = TRUE)
wcvp$first_published <- gsub(".*\\/", "", wcvp$first_published, perl = TRUE)
wcvp$first_published <- gsub("^\\*", "", wcvp$first_published, perl = TRUE)
wcvp$first_published <- gsub("^I", "", wcvp$first_published, perl = TRUE)

# merging the two information
wcvp1 <- unique(wcvp[, c("plant_name_id", "first_published", "accepted_plant_name_id",
                         "basionym_plant_name_id")])
wcvp1$plant_name_id <- as.character(wcvp1$plant_name_id) 
wcvp1$accepted_plant_name_id <- as.character(wcvp1$accepted_plant_name_id)
check <- dplyr::left_join(check, wcvp1, by = "plant_name_id")

# getting missing publication year due to bad matches (mostly due to authorship format between WFO and WCVP)
wcvp2 <- wcvp[, c("taxon_name", "taxon_authors", "plant_name_id","first_published", 
                  "accepted_plant_name_id", "basionym_plant_name_id")]
wcvp2 <- wcvp2[!is.na(wcvp2$first_published), ]
wcvp2 <- wcvp2[!duplicated(wcvp2$taxon_name), ]
names(wcvp2)[1] <- "scientificName.new"
tmp <- dplyr::left_join(check, wcvp2, by = "scientificName.new", 
                        suffix = c(".x", ""))
rep_these <- is.na(check$first_published) & !is.na(tmp$first_published)
if (any(rep_these)) {
  # col2show <- c("scientificName", "scientificName.new", "taxon_authors")
  # cbind(check, tmp)[rep_these, col2show] # visually inspection shows that most if not all differences due to missing authors in one or the other DB
  auth1 <- strsplit(check$scientificNameAuthorship.new[rep_these], " ")
  auth1 <- lapply(auth1, function(x) gsub("\\(|\\)|,$", "", x, perl = TRUE))
  auth2 <- strsplit(tmp$taxon_authors[rep_these], " ")
  auth2 <- lapply(auth2, function(x) gsub("\\(|\\)|,$", "", x, perl = TRUE))
  f1 <- function(x, y) return(any(x %in% y | y %in% x))
  name_diff <- mapply(f1, auth1, auth2, SIMPLIFY = FALSE)
  # cbind(check, tmp)[rep_these, col2show][!unlist(name_diff),] # visually inspection shows that most if not all differences due to missing authors in one or the other DB
  cols2rep <- c("plant_name_id", "first_published", "accepted_plant_name_id", 
                "basionym_plant_name_id")
  rep_these[rep_these][!unlist(name_diff)] <- FALSE
  check[rep_these, cols2rep] <- tmp[rep_these, cols2rep]
}
check$first_published.new <- check$first_published

# Getting year of publication in the case of basionyms
rep_these <- !is.na(check$basionym_plant_name_id)
if (any(rep_these)) {
  tmp1 <- check[rep_these, ]
  tmp1$plant_name_id <- as.character(tmp1$basionym_plant_name_id)
  tmp1 <- dplyr::left_join(tmp1, wcvp1, by = "plant_name_id")
  new.years <- tmp1$first_published.y
  check_these <- as.numeric(new.years) > as.numeric(check$first_published.new[rep_these])
  check_these[is.na(check_these)] <- FALSE
  if (any(check_these))
    new.years[check_these] <- tmp1$first_published.x[check_these]
  check$first_published.new[rep_these] <- new.years
}

# Getting the older year of publication from all accepted names
check$first_published.new1 <- check$first_published.new 
all_ids <- unique(check$plant_name_id)
all_ids <- all_ids[!is.na(all_ids)]
wcvp2 <- wcvp1[wcvp1$plant_name_id %in% all_ids | 
                 wcvp1$accepted_plant_name_id %in% all_ids,]
for (i in seq_along(all_ids)) {
  cat(i, "")
  id.i <- all_ids[i]
  data.i <- wcvp2[wcvp2$plant_name_id %in% id.i | 
                   wcvp2$accepted_plant_name_id %in% id.i, ]
  if (dim(data.i)[1] == 1) next
  
  if (dim(data.i)[1] > 1) {
    min.yr.i <- data.i$first_published[which.min(data.i$first_published)]
    check$first_published.new1[check$plant_name_id %in% id.i] <- 
      min.yr.i
  }
}
check$first_published.new <- as.double(check$first_published.new)
check$first_published.new1 <- as.double(check$first_published.new1)

# removing infra specific levels (about 1400 taxa)
rm_these <- !grepl(" var. | subsp. | f. | fo. ", check$scientificName, 
                  perl = TRUE) 
check1 <- check[rm_these, ]

# NAs publication years (about 232 taxon without publication year)
check_these <- (is.na(check1$first_published.new1) | is.na(check1$first_published.new))
table(check_these)
check1[check_these & is.na(check1$plant_name_id), ]

# inspecting values
plot(check1$first_published.new1, check1$first_published.new)
ids <- !is.na(check1$first_published.new) & 
  check1$first_published.new1 < check1$first_published.new
head(check1[ids,])

# hist(check1$first_published.new, nclass = 80)
# hist(check1$first_published.new1, nclass = 80)
table(check1$first_published.new1, useNA = "always")
table(check1$first_published.new, useNA = "always")
table(check1$first_published, useNA = "always")


file.out <- "outputs/results_species_accumulation.txt"
writeLines("#### MAIN METHODS AND RESULTS FROM THE EVALUATION OF THE SPECIES ACCUMULATION CURVE (from code: 12_species_accumulation.r) ####\n", 
           file.out)
desc <- "We obtained from the WCVP backbone (version X) the date of first publication for each species. 
This date corresponded to the oldest date of publication when accepted names contained basionyms or synonyms published prior to the accepted name.\n"
write(desc, file=file.out, append=TRUE)

# Getting the data ready for downstream analyses
anos <- seq(from=min(check1$first_published.new, na.rm=T), 
            to= max(check1$first_published.new, na.rm=T), by=1)
anos.pub <- check1$first_published.new1
species <- aggregate(check1$scientificName, list(anos.pub), 
                     function(x) length(unique(x))) 
dados.anos = merge (as.data.frame(anos), species, 
                    by.x="anos", by.y="Group.1", all=TRUE)
dados.anos[is.na(dados.anos)] <- 0
dados.anos <- dados.anos[-length(dados.anos[[1]]),] # removing last year studies (many species are probably not published yet)
# dados.anos <- dados.anos[dados.anos$anos >= 1760, ] # avoiding Species Plantarum biased influence

fut.year <- 250 # The choice of the final year of projection matters
dados.anos1 <- dados.anos
predict.years <- (max(dados.anos1$anos)+1):((max(dados.anos1$anos)+1+fut.year))


## Describing the species accumulation through time -----------------------------------------------------
DATA1 <- data.frame(ano = c(dados.anos1$anos, predict.years),
                    sp.acum =  c(cumsum(dados.anos1$x), rep(NA, length(predict.years))))
# adding some older years before any description to force a zero intercept
prev.ys <- 15
DATA1 <- rbind.data.frame(data.frame(ano=seq(min(DATA1$ano) - prev.ys, min(DATA1$ano)-1),
                                     sp.acum = rep(0, prev.ys)), 
                          DATA1)
# max_sp1 <- max(as.double(DATA1[[2]]), na.rm = TRUE)
# factor <- 1.7
# DATA1$sp <- DATA1[[2]]/(max_sp1 * factor)
DATA1$sp <- DATA1[[2]]
DATA1$ys <- DATA1[[1]] - min(DATA1[[1]], na.rm = TRUE) + 1
desc <- "We then calculated the cumulative number of known species each year until 2022.\n"
write(desc, file=file.out, append=TRUE)

# Fitting different models to the data of number of species described per year
models <- c("linear", 
            "exponential", 
            "quadratic",
            # "logistic",
            "general_logistic",
            "saturating",
            "ricker",
            "general_ricker"
)
model.ls1 <- vector("list", length(models))
names(model.ls1) <- models
if (any("linear" %in% models)) {
  f <- sp ~ a + b * ys
  sts <- as.numeric(stats::coef(stats::lm(sp ~ ys, data = stats::na.omit(DATA1))))
  lin <- try(stats::nls(f, data = stats::na.omit(DATA1), 
                        start = list(a = sts[1], b = sts[2])), TRUE)
  model.ls1[["linear"]] = lin
}
if (any("quadratic" %in% models)) {
  f <- sp ~ a + b * ys + c * (ys^2)
  sts <- as.numeric(stats::coef(stats::lm(sp ~ ys + I(ys^2), 
                                          data = stats::na.omit(DATA1))))
  quad <- try(stats::nls(f, data = stats::na.omit(DATA1), 
                         start = list(a = sts[1], b = sts[2], c = sts[3]), 
                         stats::nls.control(maxiter = 500)), TRUE)
  model.ls1[["quadratic"]] = quad
}
if (any("exponential" %in% models)) {
  f <- sp ~ a * exp(b * ys)
  exp <- try(stats::nls(f, data = stats::na.omit(DATA1), 
                        start = list(a = 100, b = 0.001)), TRUE)
  model.ls1[["exponential"]] = exp
}
if (any("logistic" %in% models)) {
  f <- sp ~ exp(a + b * ys)/(1 + exp(a + b * ys))
  logis <- try(stats::nls(f, data = stats::na.omit(DATA1), 
                          start = list(a = -3.38, b = 0.015)), TRUE)
  
  glm(sp ~ ys, family=binomial(link='logit'), data=stats::na.omit(DATA1))
  
  model.ls1[["logistic"]] = logis
}
if (any("general_logistic" %in% models)) {
  f <- sp ~ a + (1 - a)/(1 + exp(-b * (ys - m)))
  gen.logis <- try(stats::nls(f, data = stats::na.omit(DATA1), 
                              start = list(a = 2.302e+04, b = -0.01, 
                                           m = stats::median(stats::na.omit(DATA1)$ys))), TRUE)
  # nls.multstart::nls_multstart(f,
  #                              data = stats::na.omit(DATA1),
  #                              start_lower = c(a = -1, b = -1, m = 100),
  #                              start_upper = c(a = 100000, b = 1, m = 500),
  #                              iter = 500, supp_errors = "Y", convergence_count = 100,
  #                              na.action = na.omit)
  model.ls1[["general_logistic"]] = gen.logis
}
if (any("saturating" %in% models)) {
  f <- sp ~ a * (ys/(b + ys))
  sat <- try(stats::nls(f, data = stats::na.omit(DATA1), 
                        start = list(a = mean(stats::na.omit(DATA1)$sp), 
                                     b = -1.3)), TRUE)
  model.ls1[["saturating"]] = sat
}
if (any("ricker" %in% models)) {
  f.rick <- sp ~ a * ys * exp(b*ys)
  ricker <- try(stats::nls(f.rick, data = stats::na.omit(DATA1), 
                           start = list(a = 50, b = 0.01)), TRUE)
  model.ls1[["ricker"]] = ricker
}
if (any("general_ricker" %in% models)) {
  f.gen <- sp ~ b * (ys/a * exp(1 - ys/a))^alpha
  gen_ricker <- try(stats::nls(f.gen, data = stats::na.omit(DATA1), 
                               start = list(a = 600, b = max(stats::na.omit(DATA1)$sp), 
                                            alpha= 2.4)), TRUE)
  # nls.multstart::nls_multstart(f.gen,
  #                              data = stats::na.omit(DATA1),
  #                              start_lower = c(a = -1, b = -1, alpha = 0),
  #                              start_upper = c(a = 1, b = 1, alpha = 2),
  #                              iter = 500, supp_errors = "Y", convergence_count = 100,
  #                              na.action = na.omit)
  model.ls1[["general_ricker"]] = gen_ricker
}

# Chosing the best model to describe the data
md.sel <- ConR:::ICtab.mod.select(model.ls1)
(AICtab <- md.sel$ICtab[order(md.sel$ICtab$dAIC),]) # inspect the best models
best.name <- "general_ricker"
best1 <- model.ls1[[best.name]]
DATA1$est.prop <- predict(best1, data.frame(ys = DATA1$ys))
DATA1$est.prop[DATA1$est.prop < 0] <- 0
DATA1$predicted <- floor(DATA1$est.prop)
head(DATA1, 25); tail(DATA1[!is.na(DATA1$sp), ], 10); tail(DATA1)

desc <- cat("Next we fitted different candidate non-linear function to describe the species accumulation curve using the function nls() from base R (cite R).
The candidate models were: ", models,"\n")
write(desc, file=file.out, append=TRUE)
desc <- "The non-linear Generalized Ricker model (i.e. species ~ b * (years/a * exp(1 - years/a))^alpha), was the one that by far presented the vest fit to the species accumulation curve.\n"
write(desc, file=file.out, append=TRUE)
write(paste(rownames(AICtab), rep(c("dAIC:", "df:"), each = length(models)), unlist(AICtab)), file=file.out, append=TRUE)
desc <- "\nThis hump-shaped model has a parameter b which estimates the peak of the curve, which in our context is a good estimate for the maximum number of species that will be described in our study area in the future.\n"
write(desc, file=file.out, append=TRUE)

# Get the confidence intervals for the best model
# anos_sel <- seq(min(DATA$ano), max(DATA$ano),  by = 5)
# anos_ci <- DATA$ys[which(DATA$ano %in% anos_sel)]
anos_ci <- DATA1$ys
pred_ci1 <-  propagate:::predictNLS(best1, interval = "prediction",
                                    newdata = data.frame(ys = anos_ci),
                                    nsim = 100000)
DATA1$pred.low.95 <- pred_ci1$summary$`Prop.2.5%`
DATA1$pred.high.95 <- pred_ci1$summary$`Prop.97.5%`

desc <- "Based on the fitted model, we obtained the predicted y-value at the peak (i.e the estimated number of species) and the x-value at the peak (the estimated year when all species will be described).
Using the function predictNLS() of the package propagate (Spiess, A. (2025). propagate: Propagation of Uncertainty. R package version 1.0-7, <https://CRAN.R-project.org/package=propagate>.), we obtained the 95% prediction intervals based on uncertainty propagation.\n"
write(desc, file=file.out, append=TRUE)


## Describing the species rate of discovery (NO LONGER NECESSARY) --------------------------------------------------
# Fitting different models to the data of number of species described per year
# DATA <- data.frame (pop.size = c(dados.anos1[[2]], rep(NA, length(predict.years))),
#                     ano = c(dados.anos1[[1]], predict.years))
# max_sp <- max(as.double(dados.anos[[2]]), na.rm = TRUE)
# DATA$sp <- DATA[[1]]/max_sp
# DATA$ys <- DATA[[2]] - min(DATA[[2]], na.rm = TRUE) + 1
# 
# models <- c("linear", 
#             "exponential", 
#             "quadratic",
#             "logistic",
#             #"general_logistic",
#             #"saturating"
#             "ricker",
#             "general_ricker"
#             )
# model.ls <- vector("list", length(models))
# names(model.ls) <- models
# if (any("linear" %in% models)) {
#   f <- sp ~ a + b * ys
#   sts <- as.numeric(stats::coef(stats::lm(sp ~ ys, data = stats::na.omit(DATA))))
#   lin <- try(stats::nls(f, data = stats::na.omit(DATA), 
#                         start = list(a = sts[1], b = sts[2])), TRUE)
#   model.ls[["linear"]] = lin
# }
# if (any("quadratic" %in% models)) {
#   f <- sp ~ a + b * ys + c * (ys^2)
#   sts <- as.numeric(stats::coef(stats::lm(sp ~ ys + I(ys^2), 
#                                           data = stats::na.omit(DATA))))
#   quad <- try(stats::nls(f, data = stats::na.omit(DATA), 
#                          start = list(a = sts[1], b = sts[2], c = sts[3]), 
#                          stats::nls.control(maxiter = 500)), TRUE)
#   model.ls[["quadratic"]] = quad
# }
# if (any("exponential" %in% models)) {
#   f <- sp ~ a * exp(b * ys)
#   exp <- try(stats::nls(f, data = stats::na.omit(DATA), 
#                         start = list(a = 0.1, b = 0.001)), TRUE)
#   model.ls[["exponential"]] = exp
# }
# if (any("logistic" %in% models)) {
#   f <- sp ~ exp(a + b * ys)/(1 + exp(a + b * ys))
#   logis <- try(stats::nls(f, data = stats::na.omit(DATA), 
#                           start = list(a = -1, b = 0.1)), TRUE)
#   model.ls[["logistic"]] = logis
# }
# if (any("general_logistic" %in% models)) {
#   f <- sp ~ a + (1 - a)/(1 + exp(-b * (ys - m)))
#   gen.logis <- try(stats::nls(f, data = stats::na.omit(DATA), 
#                               start = list(a = min(stats::na.omit(DATA)$sp), b = 0.01, 
#                                            m = stats::median(stats::na.omit(DATA)$ys))), TRUE)
#   model.ls[["general_logistic"]] = gen.logis
# }
# if (any("saturating" %in% models)) {
#   f <- sp ~ a * (ys/(b + ys))
#   sat <- try(stats::nls(f, data = stats::na.omit(DATA), 
#                        start = list(a = 0.5, b = 1)), TRUE)
#   model.ls[["saturating"]] = sat
# }
# if (any("ricker" %in% models)) {
#   f.rick <- sp ~ a * ys * exp(b*ys)
#   ricker <- try(stats::nls(f.rick, data = stats::na.omit(DATA), 
#                         start = list(a = 0.1, b = -0.01)), TRUE)
#   model.ls[["ricker"]] = ricker
# }
# if (any("general_ricker" %in% models)) {
#   f.gen <- sp ~ b * (ys/a * exp(1 - ys/a))^alpha
#   gen_ricker <- try(stats::nls(f.gen, data = stats::na.omit(DATA), 
#                            start = list(a = 150, b = 0.35, alpha= 1.1)), TRUE)
#   # nls.multstart::nls_multstart(f,
#   #                              data = stats::na.omit(DATA),
#   #                              start_lower = c(a = -1, b = -1, alpha = 0),
#   #                              start_upper = c(a = 1, b = 1, alpha = 2),
#   #                              iter = 500, supp_errors = "Y", convergence_count = 100,
#   #                              na.action = na.omit)
#   model.ls[["general_ricker"]] = gen_ricker
# }
# 
# # Chosing the best model to describe the data
# md.sel <- ConR:::ICtab.mod.select(model.ls)
# md.sel$ICtab # inspect the best models
# best.name <- "ricker"
# best <- model.ls[[best.name]]
# DATA$est.prop <- predict(best, data.frame(ys = DATA$ys))
# DATA$est.prop[DATA$est.prop < 0] <- 0
# DATA$predicted <- round(DATA$est.prop * max_sp, 1)
# # tail(DATA)
# 
# # Get the confidence intervals for the best model
# # anos_sel <- seq(min(DATA$ano), max(DATA$ano),  by = 5)
# # anos_ci <- DATA$ys[which(DATA$ano %in% anos_sel)]
# anos_ci <- DATA$ys
# pred_ci <-  propagate:::predictNLS(best, interval = "prediction",
#                                    newdata = data.frame(ys = anos_ci),
#                                    nsim = 50000)
# DATA$pred.low.95 <- pred_ci$summary$`Prop.2.5%` * max_sp
# DATA$pred.high.95 <- pred_ci$summary$`Prop.97.5%` * max_sp
# 
# # Get the confidence intervals for the quadratic model (minimum estimate, 2nd best)
# quad.mod <- stats::lm(sp ~ ys + I(ys^2), data = stats::na.omit(DATA))
# pred_ci1 <- predict(quad.mod, newdata = data.frame(ys = anos_ci),
#             interval = "prediction", se.fit = TRUE, level = 0.95)
# DATA$predicted_quadratic <- pred_ci1$fit[,1] * max_sp
# DATA$pred.low.95_quadratic <- pred_ci1$fit[,2] * max_sp
# DATA$pred.high.95_quadratic <- pred_ci1$fit[,3] * max_sp
# DATA$predicted_quadratic[DATA$predicted_quadratic<0] <- NA
# DATA$pred.low.95_quadratic[DATA$pred.low.95_quadratic<0] <- NA
# DATA$pred.high.95_quadratic[DATA$pred.high.95_quadratic<0] <- NA


## Number of species to be discovered ------------------------------------------------------------------
## From the species accumulation
# Gen. ricker
conf95 <- confint(best1)
pico <- paste0(round(stats::coef(best1)[2], 0), " tree species [95% CI: ", 
               floor(conf95[2,1]), "-", 
               ceiling(conf95[2,2]), "]")
toto <- range(check1$first_published.new, na.rm = TRUE) # species described between 1753 and 2023
ano.pico <- paste0(seq(toto[1], toto[1]+1000, 1)[round(stats::coef(best1)[1], 0)], " [95% CI: ",
                   seq(toto[1], toto[1]+1000, 1)[floor(conf95[1,1])], "-", 
                   seq(toto[1], toto[1]+1000, 1)[ceiling(conf95[1,2])], "]")
check1[check1$first_published %in% toto[1], ]
desc <- paste0("The earliest species in our study area were described in ", toto[1],". The rate of species described each year increased until the early 20s of the last century, when the rate started to decrease. \n", sep="")
write(desc, file=file.out, append=TRUE)
desc <- paste0("However, we estimated that there still are ", 
               round(stats::coef(best1)[2], 0) - dim(check1)[1], 
               " tree species to be discovered in tropical America, a feat that will result in a total of ", pico,
              " and that will not be achieved before the year of ", 
               ano.pico,", given the historical rate of species discovery.\n", sep="")
write(desc, file=file.out, append=TRUE)


## From the rate of species discovery
# future <- which(is.na(DATA$sp)) 
# pred.cols <- c("predicted", "pred.low.95", "pred.high.95",
#                "predicted_quadratic", "pred.low.95_quadratic", "pred.high.95_quadratic")
# max_sp1 + colSums(DATA[future, pred.cols], na.rm = TRUE)



## Plotting the results --------------------------------------------------------------------------------
# Plotting the fit of the best model
pdf(file = "./outputs/FigureXX.pdf", width = 7.24, height = 7.1, pointsize = 12, useKerning = TRUE, 
    compress = FALSE, useDingbats = FALSE)

## species accumulation
DATA2 <- DATA1[-c(1:prev.ys), ]
ylim <- range(DATA1[grepl("sp|est.prop|pred", names(DATA2))], 
              na.rm = TRUE)
xlim <- range(DATA2$ys, na.rm = TRUE)
preds1 <- predict(best1, data.frame(ys = seq(range(DATA2$ys)[1], 
                                           range(DATA2$ys)[2], by = 1)))
graphics::par(mfrow = c(1, 1), mgp = c(3, 0.45, 0), 
              mar = c(4.5, 4.5, 1, 1))
graphics::plot(DATA2$sp ~ DATA2$ys, pch = 19, cex = 1.2, col = adjustcolor("black", alpha.f = 0.15), 
               ylim = ylim, xlim = xlim, xlab = "Years", ylab = "New species described", 
               xaxt = "n", yaxt = "n", cex.lab = 1.4)
graphics::axis(1, at = seq(min(DATA2$ys), max(DATA2$ys), 10), 
               labels = seq(min(DATA2$ano), max(DATA2$ano), 10), 
               tcl = -0.3)
ats <- pretty(seq(min(ylim), max(ylim), 0.1))
axis(2, at = ats, labels = ats, 
     las = 1, tcl = -0.3)
range1 <- (range(stats::na.omit(DATA2)$ys)[1] - 1)
range2 <- (range(stats::na.omit(DATA2)$ys)[2] + 1)
leg.pos <- "bottomright"

if (best.name == "general_ricker") {
  graphics::curve(stats::coef(best1)[2] * (x/stats::coef(best1)[1] * 
                    exp(1 - x/stats::coef(best1)[1]))^stats::coef(best1)[3], 
                  add = TRUE, lwd = 2, lty = 1, col = "red")
  # graphics::curve(stats::coef(best1)[2] * (x/stats::coef(best1)[1] * 
  #                   exp(1 - x/stats::coef(best1)[1]))^stats::coef(best1)[3], 
  #                 from = range1, to = range2, add = TRUE, lwd = 2, 
  #                 col = "red")
  # Plot the profile with specified confidence levels
  anos_ci1 <- anos_ci[-c(1:prev.ys)]
  lines(anos_ci1, DATA2$pred.low.95, col = 2, lwd = 2, lty = 2)
  lines(anos_ci1, DATA2$pred.high.95, col = 2, lwd = 2, lty = 2)

  legend(leg.pos, c("Ricker model"), lwd = 3,
         col = "red", bty = "n", cex = 1.4)
  pico <- stats::coef(best1)[2]
  ano.pico <- seq(1753, 1753+1000, 1)[round(stats::coef(best1)[1], 0)]
                     
  abline(h = pico, lty = 2)
  abline(v = ano.pico, lty = 2)

  legend("topleft",
         legend = paste("Value at peak =", round(pico,0), "species. Year at peak =", ano.pico),
         bty = "n",)
}

## species discovery
# ylim <- range(DATA[grepl("sp|est.prop", names(DATA))], 
#               na.rm = TRUE)
# xlim <- range(DATA$ys, na.rm = TRUE)
# preds <- predict(best, data.frame(ys = seq(range(DATA$ys)[1], 
#                                            range(DATA$ys)[2], by = 1)))
# graphics::par(mfrow = c(1, 1), mgp = c(2.5, 0.5, 0), 
#               mar = c(4, 4, 1, 1))
# graphics::plot(DATA$sp ~ DATA$ys, pch = 19, cex = 1.2, 
#                ylim = ylim, xlim = xlim, xlab = "Years", ylab = "New species described", 
#                xaxt = "n", yaxt = "n", cex.lab = 1.4)
# graphics::axis(1, at = seq(min(DATA$ys), max(DATA$ys), 10), 
#                labels = seq(min(DATA$ano), max(DATA$ano), 10), 
#                tcl = -0.3)
# ats <- pretty(seq(min(ylim), max(ylim), 0.1))
# axis(2, at = ats, labels = round(ats * max(DATA$pop.size, na.rm = TRUE), 0), 
#      las = 1, tcl = -0.3)
# range1 <- (range(stats::na.omit(DATA)$ys)[1] - 1)
# range2 <- (range(stats::na.omit(DATA)$ys)[2] + 1)
# leg.pos <- "topright"
# 
# if (best.name == "linear") {
#   graphics::curve(stats::coef(mod)[1] + stats::coef(mod)[2] * 
#                     x, add = TRUE, lwd = 2, lty = 2, col = "#D55E00")
#   graphics::curve(stats::coef(mod)[1] + stats::coef(mod)[2] * 
#                     x, from = range1, to = range2, add = TRUE, lwd = 2, 
#                   col = "#D55E00")
#   graphics::legend(leg.pos, c("Linear model"), lwd = 3, 
#                    col = "#D55E00", bty = "n", cex = 1.4)
# }
# if (best.name == "quadratic") {
#   mod1 <- quad.mod
#   graphics::curve(stats::coef(mod1)[1] + stats::coef(mod1)[2] * 
#                     x + stats::coef(mod1)[3] * (x^2), add = TRUE, 
#                   lwd = 2, lty = 2, col = "#56B4E9")
#   graphics::curve(stats::coef(mod1)[1] + stats::coef(mod1)[2] * 
#                     x + stats::coef(mod1)[3] * (x^2), from = range1, 
#                   to = range2, add = TRUE, lwd = 2, col = "#56B4E9")
#   lines(anos_ci, pred_ci1$fit[, 2], col = "#56B4E9", lwd = 1, lty = 2)
#   lines(anos_ci, pred_ci1$fit[, 3], col = "#56B4E9", lwd = 1, lty = 2)
#   # legend(leg.pos, c("Quadratic model"), lwd = 3, col = "#56B4E9", 
#   #        bty = "n", cex = 1.4)
#   
# }
# if (best.name == "exponential") {
#   graphics::curve(stats::coef(mod)[1] * exp(stats::coef(mod)[2] * 
#                                               x), add = TRUE, lwd = 2, lty = 2, col = "#009E73")
#   graphics::curve(stats::coef(mod)[1] * exp(stats::coef(mod)[2] * 
#                                               x), from = range1, to = range2, add = TRUE, lwd = 2, 
#                   col = "#009E73")
#   legend(leg.pos, c("Exponential model"), lwd = 3, 
#          col = "#009E73", bty = "n", cex = 1.4)
# }
# if (best.name == "ricker") {
#   graphics::curve(stats::coef(mod)[1] * x * exp(stats::coef(mod)[2] * x), add = TRUE, lwd = 2, lty = 2, col = "red")
#   graphics::curve(stats::coef(mod)[1] * x * exp(stats::coef(mod)[2] * x), from = range1, to = range2, add = TRUE, lwd = 2, 
#                   col = "red")
#   # Plot the profile with specified confidence levels
#   # lines(anos_ci, pred_ci$summary[, 5], col = 2, lwd = 1, lty = 2)
#   # lines(anos_ci, pred_ci$summary[, 6], col = 2, lwd = 1, lty = 2)
#   lines(anos_ci, pred_ci$summary[, 11], col = 2, lwd = 1, lty = 2)
#   lines(anos_ci, pred_ci$summary[, 12], col = 2, lwd = 1, lty = 2)
#   
#   
#   # legend(leg.pos, c("Ricker model"), lwd = 3, 
#   #        col = "red", bty = "n", cex = 1.4)
#   pico <- (stats::coef(mod)[1]/-stats::coef(mod)[2])*exp(-1)
#   ano.pico <- 1/-stats::coef(mod)[2]
#   
#   #abline(h = pico, lty = 2)
#   abline(v = ano.pico, lty = 2)
#   
#   legend("right", 
#          legend = paste("Year of peak =", DATA$ano[round(ano.pico, 0)]),
#          bty = "n",)
# }
# legend(leg.pos, c("Ricker model", "Quadratic model"), lwd = 3, 
#        col = c("red", "#56B4E9"), bty = "n", cex = 1.4)
dev.off()

