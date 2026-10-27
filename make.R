#' Endemism: A Research Compendium
#' 
#' @description 
#' Runs the full workflow of the paper "A biome lens is critical to
#' understanding tree endemism across tropical America": cleaning of herbarium
#' records (00-02), biome maps and protected areas (03-04), endemism levels (05), weighted
#' endemism and null model (06), interpolation (07), figures (08), checklists
#' (09-10), species description curve (11), manuscript numbers (12) and the
#' Zenodo export (13).
#' 
#' @author Guilherme S. Grittz \email{ggrittz@usp.br}
#' 
#' @date 2024/07/15



## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())


## Global Variables ----

# Google authentication (token cached in .secrets)
options(gargle_oauth_cache = ".secrets")
googlesheets4::gs4_auth(cache = ".secrets",
                        email = "ggrittz@usp.br",
                        scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")


## Run Project ----

# Ecoregion -> IUCN EFG classification (sts_ecoregion_efg_conversionTable_Morrone2022)
sts_class <- googlesheets4::read_sheet("1G19dSMno_AuRRV7QCBRe9bza8kQIW88Mvk8BO6Y9LBI",
                                       sheet = "Sheet1", col_types = "c")
writexl::write_xlsx(sts_class, here::here("data", "derived-data", "maps",
                                          "final_classification.xlsx"))
rm(sts_class)

source(here::here("analyses", "00_prepare_ffb_vouchers.R"))
source(here::here("analyses", "01_prepare_gbif_data.R"))
source(here::here("analyses", "01_prepare_jabot_data.R"))
source(here::here("analyses", "01_prepare_reflora_data.R"))
source(here::here("analyses", "01_prepare_splink_data.R"))
source(here::here("analyses", "02_removing_duplicates.R"))
source(here::here("analyses", "03_prepare_TAmap.R"))
source(here::here("analyses", "00_prepare_wdpa.R"))
source(here::here("analyses", "04_crossing_maps.R"))
source(here::here("analyses", "05_endemism_levels.R"))        # ~7.5 h
source(here::here("analyses", "06_calculate_wee.R"))
source(here::here("analyses", "07_rarefy_interpolate.R"))
source(here::here("analyses", "08_preparing_plots.R"))
source(here::here("analyses", "09_preparing_extra_records.R"))
source(here::here("analyses", "10_preparing_checklist.R"))
source(here::here("analyses", "11_species_accumulation.R"))
source(here::here("analyses", "12_manuscript_numbers.R"))
source(here::here("analyses", "13_zenodo_export.R"))

