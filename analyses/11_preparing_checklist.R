

# Checklist of tropical American trees ------------------------------------

# Load data
file_path <- here::here("data", "derived-data", "rds")
file_name <- "final_df.rds"
all_data <- readRDS(file.path(file_path, file_name))

my_names <- file.path("data", "derived-data", "checklist_standardized.rds")
my_names <- readRDS(my_names)
my_names <- my_names[!duplicated(my_names$our_acceptedID), ]
# Make checklist based on taxonomically verified registers
# table(all_data$tax.check1)
checklist <- plantR::checkList(x = all_data,
                               n.vouch = 30,
                               type = "short",
                               rm.dup = FALSE) # already removed
checklist1 <- dplyr::left_join(checklist, my_names[, c("our_accepted_scientificName",
                                                      "lifeForm",
                                                      "source")],
                              by = c("scientificName" = "our_accepted_scientificName"))




