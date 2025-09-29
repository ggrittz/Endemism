
# Adding more locations to plantR data ------------------------------------
devtools::load_all()

# Read all
file_path <- here::here("data", "derived-data", "Cleaned_occs")
folders <- list.files(file_path, full.names = TRUE)

# Load gazetteer from Drive
options(gargle_oauth_cache = ".secrets")
# googlesheets4::gs4_auth() # is this needed?
googlesheets4::gs4_auth(cache = ".secrets",
                        email = "ggrittz@usp.br",
                        scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
# Download from Google Drive
link <- "https://docs.google.com/spreadsheets/d/"
link1 <- "1qOyD7qDzgqEz6Xdbf5GcrjDChjJbJApfWk_P4pgCOAg/edit?gid=1266085842#gid=1266085842"
gaz <- as.data.frame(googlesheets4::read_sheet(paste0(link, link1)))
# Load families
# family_names <- readRDS(here::here("data", "derived-data", "family_names.rds"))

# Only ok here? Last time I used everything (22/09)
# gaz <- gaz[gaz$status %in% "ok", ]
# gaz_plantR1 <- plantR:::gazetteer


# Adding localities with more than 100 registers to plantR ----------------

# Needed columns: status, source, order, loc, loc.correct (?), lat, lon, res.gaz
# NAME_01 = country; NAME_02 = state; NAME_03 = municipality; NAME_04 = locality

loc_list <- list()

for (i in family_names) {
  rds_file <- file.path(file_path, i, paste0(i, ".rds"))
  
  if (file.exists(rds_file)) {
    
    df <- readRDS(rds_file)
    df$geo.reg <- countrycode::countrycode(df$country.new,
                                           origin = "country.name",
                                           destination = "region")
    
    # Filtering only tropical American registers
    df <- df[df$geo.reg %in% c("Latin America & Caribbean"), ]
    df <- df[df$loc.check1 %in% c("check_local.2country", "check_local.2state") | 
               df$geo.check1 %in% "check_gazetteer", ]
    
    # Select only the specified columns
    df <- df[, c("country", "stateProvince", "municipality", "locality",
                 "country.new", "stateProvince.new", "municipality.new",
                 "locality.new", "loc", "loc.correct",
                 "loc.check", "geo.check",
                 "decimalLatitude.new", "decimalLongitude.new",
                 "resolution.gazetteer", "geo.reg", "basisOfRecord", "data_source")]
    
    # Add the filtered and selected dataframe to the list
    loc_list[[i]] <- df
  } else {
    cat("File not found:", i, "\n")
  }
}

gc()

loc_list <- do.call(rbind, loc_list)
loc_list$family <- rownames(loc_list)
rownames(loc_list) <- NULL
loc_list$family <- gsub("\\..*", "", loc_list$family)
loc_list$loc <- gsub("___", "", loc_list$loc)
loc_list$loc <- gsub("__", "_", loc_list$loc)
loc_list$loc <- gsub("_$", "", loc_list$loc)
# rm(list=ls())

# saveRDS(loc_list, here::here("data", "derived-data", "loc_list_new_fixLoc.rds"))

# Post-preparing ----------------------------------------------------------
# file_path2 <- here::here("data", "derived-data")
# file_name2 <- "loc_list_new_fixLoc.rds"
# loc_list <- readRDS(file.path(file_path2, file_name2))
to_remove <- c("family", "geo.reg", "loc.check", "geo.check", "resolution.gazetteer",
               "basisOfRecord", "data_source", "country.new", "stateProvince.new",
               "municipality.new", "locality.new", "decimalLatitude.new", "decimalLongitude.new")
loc_list <- loc_list[, -which(names(loc_list) %in% to_remove)]
# loc_list$decimalLatitude.new <- as.numeric(loc_list$decimalLatitude.new)
# loc_list$decimalLongitude.new <- as.numeric(loc_list$decimalLongitude.new)
# loc_list$recID <- NULL

loc_new_table <- data.frame(table(loc_list$loc))
loc_new_table <- loc_new_table[loc_new_table$Freq >= 100, ]

# From these, remove cases where the locality assigned came from a country/state/municipality
# Country
loc_new_table <- loc_new_table[!tolower(loc_new_table$Var1) %in% tolower(gaz$NAME_0), ]
# State
loc_new_table <- loc_new_table[!tolower(loc_new_table$Var1) %in% tolower(gaz$NAME_1), ]
# Remove numbers that indicate a distance from x place, but not a precise locality
# loc_new_table$Var1 <- gsub("[0-9]", "", loc_new_table$Var1)
# loc_new_table <- loc_new_table[!loc_new_table$Var1 %in% "", ]

# Filter these on loc_list
loc_list <- loc_list[loc_list$loc %in% loc_new_table$Var1, ]
loc_list <- loc_list[!duplicated(loc_list$loc), ]
loc_list <- dplyr::left_join(loc_list, loc_new_table,
                             by = c("loc" = "Var1"))

loc_list$resolution.gazetteer <- NA
loc_list$resolution.gazetteer <- as.character(loc_list$resolution.gazetteer)
loc_list$latitude.gazetteer <- NA
loc_list$latitude.gazetteer <- as.character(loc_list$latitude.gazetteer)
loc_list$longitude.gazetteer <- NA
loc_list$longitude.gazetteer <- as.character(loc_list$longitude.gazetteer)

# Populate new fixLoc probable (or not) issues
gaz$new_fixLoc <- NA
gaz[gaz == "NA"] <- NA
gbif_gsg_indices <- which(gaz$source %in% "gbif_gsg")

# Which of these gbif_gsg locations still exist?
still_exist_mask <- gaz$loc[gbif_gsg_indices] %in% loc_list$loc
gaz$new_fixLoc[gbif_gsg_indices[still_exist_mask]] <- "still_exists"
solved_mask <- !still_exist_mask  # The ones that don't exist anymore
gaz$new_fixLoc[gbif_gsg_indices[solved_mask]] <- "solved"

# Which one of these are new issues?
new_fixLoc_issues <- setdiff(loc_list$loc, gaz$loc)
# Subset these
loc_list <- loc_list[loc_list$loc %in% new_fixLoc_issues, ]
# Can't solve cases with / separating unknown adm levels unfortunately
slash_cases <- which(grepl("/", loc_list$loc))
loc_list <- loc_list[!grepl("/", loc_list$loc), ]

# Make NAME_0-4 columns
loc_list$loc <- as.character(loc_list$loc)
splitted <- strsplit(loc_list$loc, split = "_")
loc_list$NAME_0 <- sapply(splitted, function(x) x[1])
loc_list$NAME_1 <- sapply(splitted, function(x) x[2])
loc_list$NAME_2 <- sapply(splitted, function(x) x[3])
loc_list$NAME_3 <- sapply(splitted, function(x) x[4])
loc_list$NAME_4 <- sapply(splitted, function(x) x[5])

# Adding and editing some columns before saving
loc_list$source <- "gbif_gsg_new"
loc_list$order <- max(gaz$order, na.rm = TRUE)+1 : dim(loc_list)[1]
names(loc_list)[names(loc_list) == "Freq"] <- "status"
loc_list$status <- as.character(loc_list$status)

# Bind with plantR's gaz and save
file_name3 <- "editted_gaz_new_fixLoc.xlsx"
gaz1 <- dplyr::bind_rows(gaz, loc_list)
writexl::write_xlsx(gaz1, file.path(file_path2, file_name3))
rm(list=ls())


##### NOS NOVOS #####


