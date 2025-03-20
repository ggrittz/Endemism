
# Adding more locations to plantR data ------------------------------------
devtools::load_all()

# Read all
file_path <- here::here("data", "derived-data", "Cleaned_occs")
folders <- list.files(file_path, full.names = TRUE)

# Load gazetteer
gaz <- file.path("D:", "plantR", "data-raw", "raw_dictionaries", "gazetteer.csv")
gaz <- read.csv(gaz)
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
    df <- df[, c("country.new", "stateProvince.new", "municipality.new",
                 "locality.new", "loc", "locality", "loc.correct",
                 "loc.check", "geo.check",
                 "decimalLatitude.new", "decimalLongitude.new",
                 "resolution.gazetteer", "geo.reg")]
    
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
rm(list=ls())



# Post-preparing ----------------------------------------------------------
file_path2 <- here::here("data", "derived-data")
file_name2 <- "localities_new.parquet"
# arrow::write_parquet(loc_list, sink = file.path(file_path2, file_name2))
loc_list <- as.data.frame(arrow::read_parquet(file.path(file_path2, file_name2)))
to_remove <- c("family", "geo.reg", "resolution.gazetteer", "geo.check")
loc_list <- loc_list[, -which(names(loc_list) %in% to_remove)]
loc_list <- loc_list[, c(1, 2, 3, 6, 5, 4, 7:length(loc_list))]

# Count each locality.new
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

loc_new_table$resolution.gazetteer <- NA
loc_new_table$latitude.gazetteer <- NA
loc_new_table$longitude.gazetteer <- NA

# Get missing country
get_these <- grepl("^[^_]*$", loc_new_table$Var1)
loc_new_table$resolution.gazetteer[get_these] <- "country"
# Get missing states
get_these <- grepl("^[^_]*_[^_]*$", loc_new_table$Var1)
loc_new_table$resolution.gazetteer[get_these] <- "state"  
# Get missing county
get_these <- grepl("^[^_]*_[^_]*_[^_]*$", loc_new_table$Var1)
loc_new_table$resolution.gazetteer[get_these] <- "county"


# Which one of these are not in the gazetteer?
missing_gaz <- setdiff(loc_new_table$Var, gaz$loc)
# Subset these
loc_new_table <- loc_new_table[loc_new_table$Var1 %in% missing_gaz, ]
# Can't solve / cases
loc_new_table <- loc_new_table[!grepl("/", loc_new_table$Var1), ]
# Make NAME_0-4 columns
loc_new_table$Var1 <- as.character(loc_new_table$Var1)
splitted <- strsplit(loc_new_table$Var1, split = "_")
loc_new_table$NAME_0 <- sapply(splitted, function(x) x[1])
loc_new_table$NAME_1 <- sapply(splitted, function(x) x[2])
loc_new_table$NAME_2 <- sapply(splitted, function(x) x[3])
loc_new_table$NAME_3 <- sapply(splitted, function(x) x[4])
loc_new_table$NAME_4 <- sapply(splitted, function(x) x[5])

# Remove duplicated bad cases such as argentina_argentina
loc_new_table <- loc_new_table[!( !is.na(loc_new_table$NAME_0) & 
                                    !is.na(loc_new_table$NAME_1) & 
                                    loc_new_table$NAME_0 == loc_new_table$NAME_1 ), ]


# Adding and editing some columns before saving
loc_new_table$source <- "gbif_gsg"
loc_new_table$order <- max(gaz$order)+1 : dim(loc_new_table)[1]
names(loc_new_table)[names(loc_new_table) == "Freq"] <- "status"
names(loc_new_table)[names(loc_new_table) == "Var1"] <- "loc"
loc_new_table$status <- as.character(loc_new_table$status)

# Bind with plantR's gaz and save
file_name3 <- "editted_gaz.xlsx"
gaz <- dplyr::bind_rows(gaz, loc_new_table)
writexl::write_xlsx(gaz, file.path(file_path2, file_name3))
rm(list=ls())





