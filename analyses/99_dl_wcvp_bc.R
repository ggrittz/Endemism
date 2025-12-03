
# Checking distribution ---------------------------------------------------

# # Loading WCVP botanical countries
# wcvp_global <- rWCVP::wcvp_checklist()
# wcvp_global <- wcvp_global[, c("plant_name_id", "taxon_name", "taxon_authors", "area")]

# Download botanical countries (level3)
url <- "https://github.com/tdwg/wgsrpd/raw/master/109-488-1-ED/2nd%20Edition/tblLevel3.txt"
file_path <- here::here("data", "raw-data")
file_name1 <- "wgsrpd_level3.txt"
download.file(url = url,
              destfile = file.path(file_path, file_name1))  

# Download subdivision of botanical countries (level4)
url <- "https://github.com/tdwg/wgsrpd/raw/master/109-488-1-ED/2nd%20Edition/tblLevel4.txt"
file_name2 <- "wgsrpd_level4.txt"
download.file(url = url,
              destfile = file.path(file_path, file_name2))

# Load subdivisions and prepare them
# Level 3 = botanical countries
level3 <- read.table(file.path(file_path, file_name1), 
                     sep = "*", 
                     fileEncoding = "Latin1", 
                     header = TRUE, 
                     stringsAsFactors = FALSE,
                     quote = "",
                     fill = TRUE)

# Level 4 = basic recording units (which sometimes will be equal to level3)
level4 <- read.table(file.path(file_path, file_name2), 
                     sep = "*", 
                     fileEncoding = "Latin1", 
                     header = TRUE, 
                     stringsAsFactors = FALSE,
                     quote = "",
                     fill = TRUE)

# Merge both informations
level_all <- dplyr::left_join(level4, level3[, c("L3.code", "L3.area")],
                              by = c("L3.code"))

level_all1 <- aggregate(L4.code ~ L3.area + L4.country,
                        FUN = function(x) paste(unique(x), collapse = "|"),
                        data = level_all)

names(level_all1) <- c("taxon.distribution.bc", "taxon.distribution.bru", "taxon.distribution.bru.code")

# Editingt some names before merging
# Kirgizstan case
level_all1$taxon.distribution.bc <- gsub("Kirgizistan", "Kyrgyzstan", level_all1$taxon.distribution.bc)
level_all1$taxon.distribution.bru <- gsub("Kirgizistan", "Kyrgyzstan", level_all1$taxon.distribution.bru)

# Gambia case
level_all1$taxon.distribution.bc <- gsub("Gambia, The", "Gambia", level_all1$taxon.distribution.bc)
level_all1$taxon.distribution.bru <- gsub("Gambia, The", "Gambia", level_all1$taxon.distribution.bru)

# Brasília case
level_all1$taxon.distribution.bc <- gsub("^brazilia distrito federal$",
                                         "distrito federal",
                                         level_all1$taxon.distribution.bc)
level_all1$taxon.distribution.bru <- gsub("^brazilia distrito federal$",
                                         "distrito federal",
                                         level_all1$taxon.distribution.bru)


# Prepare island names, remove latin, and make everything lowercase in wcvp and our data
level_all1$taxon.distribution.bc <- plantR::prepCountry(level_all1$taxon.distribution.bc)
level_all1$taxon.distribution.bru <- plantR::prepCountry(level_all1$taxon.distribution.bru)

# Dealing with India NA cases due to countrycode routine
get_these <- which(level_all1$taxon.distribution.bru.code %in% "IND-DI")
level_all1$taxon.distribution.bru[get_these] <- "diu"
get_these <- which(level_all1$taxon.distribution.bru.code %in% "IND-GO")
level_all1$taxon.distribution.bru[get_these] <- "goa"

# Add this to Sys.Data!
file_name3 <- "wcvp_lookupDist.rds"
saveRDS(level_all1, file.path(file_path, file_name3))


rm(list=ls())
