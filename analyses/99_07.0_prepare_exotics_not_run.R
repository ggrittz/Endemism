
# Removing exotics/introduced/alien species -------------------------------
# Using 10.1038/s41597-025-06379-6 data set

# Download from Zenodo
# raw_path <- here::here("data", "raw-data", "SInAS")
# url <- "https://zenodo.org/api/records/18220953/files-archive"
# file_name <- "aliens_dataset.zip"
# if(!dir.exists(raw_path))
#   dir.create(raw_path)
# options(timeout = 10000)
# download.file(url = url, destfile = file.path(raw_path, file_name), mode = "wb")
# # Unzip everything
# unzip(file.path(raw_path, file_name), exdir = raw_path)
# # Now unzip the wanted output files
# file_name1 <- "All_Output_Files_SInAS_v3.1.1.zip"
# unzip(file.path(raw_path, file_name1), exdir = raw_path)

# Load
raw_path <- here::here("data", "raw-data", "SInAS")
main <- data.table::fread(file.path(raw_path, "SInAS_3.1.1.csv"))
raw_path1 <- here::here("data", "raw-data", "SInAS", "Output")
supl <- data.table::fread(file.path(raw_path1, "SInAS_3.1.1_FullTaxaList.csv"))
# Get neotropical countries
nt <- terra::vect(here::here("data", "derived-data", "maps", "latamMap_classified.gpkg"))
nt_countries <- sort(unique(nt$NAME_0))
rm(nt); gc()

# Keep only vascular plants
supl <- supl[supl$taxaGroup %in% "Vascular plants", ]
vasc_names <- unique(supl$taxon)
# Filter these names in the main
main <- main[main$taxon %in% vasc_names, ]

# Keep only neotropical countries in the main data
main$location_edited <- main$location
# Caribbean Netherlands...three municipalities
main$location_edited[which(main$location_edited == "Sint Eustatius")] <- "Caribbean Netherlands"
main$location_edited[which(main$location_edited == "Saba")] <- "Caribbean Netherlands"
main$location_edited[which(main$location_edited == "Bonaire")] <- "Caribbean Netherlands"
main$location_edited <- plantR::prepCountry(main$location_edited)
main$location_edited[which(main$location_edited == "virgin islands (british)")] <- "british virgin islands"
main$location_edited[which(main$location_edited == "virgin islands (u.s.)")] <- "united states virgin islands"

# Now I can filter only neotropical countries
main_nt <- main[main$location_edited %in% nt_countries, ]

# First, remove cases where the species is indicated to be native to all neotropical
# countries available in the df
# eg Buddleja cordata is only indicated to be native to three neotropical countries
has_non_native <- aggregate(establishmentMeans ~ taxon, 
                            data = main_nt, 
                            FUN = function(x) any(tolower(x) != "native"))
taxa_to_keep <- has_non_native$taxon[has_non_native$establishmentMeans]
main_nt <- main_nt[main_nt$taxon %in% taxa_to_keep, ]

# Now, which species are 100% introduced when info is available?
all_intro <- aggregate(establishmentMeans ~ taxon,
                       data = main_nt,
                       FUN = function(x) all(tolower(x) == "introduced"))
exotics <- all_intro[all_intro$establishmentMeans %in% TRUE, "taxon"]

# Testing with my data
nt_rich <- readxl::read_xlsx(here::here("outputs", "tables", "richness_by_nt.xlsx"))
nt_ende <- readxl::read_xlsx(here::here("outputs", "tables", "endemics_by_nt.xlsx"))
# table(exotics %in% nt_rich$Species)
# table(exotics %in% nt_ende$Species)

nt_rich_exo <- intersect(exotics, nt_rich$Species)
# Only need rich exo here see below
# nt_ende_exo <- intersect(exotics, nt_ende$Species)
# all(nt_ende_exo %in% nt_rich_exo)














