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

# You can list global variables here (or in a separate R script)


## Run Project ----

# List all R scripts in a sequential order and using the following form:
source(here::here("analyses", "01_prepare_gbif_data.R"))
source(here::here("analyses", "01_prepare_jabot_data.R"))
source(here::here("analyses", "01_prepare_reflora_data.R"))
source(here::here("analyses", "01_prepare_splink_data.R"))
source(here::here("analyses", "02_removing_duplicates.R"))

