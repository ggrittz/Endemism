#' Endemism: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
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

source(here::here("analyses", "01_prepare_gbif_data.R"))
source(here::here("analyses", "01_prepare_jabot_data.R"))
source(here::here("analyses", "01_prepare_reflora_data.R"))
source(here::here("analyses", "01_prepare_splink_data.R"))
source(here::here("analyses", "02_removing_duplicates.R"))

