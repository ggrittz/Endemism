# Manuscript numbers ------------------------------------------------------
# Joins the numbers saved by each script (Taxonomy and Endemism) into one table,
# which is what the values in the manuscript are updated from
num_files <- list.files(here::here("outputs", "numbers"), pattern = "\\.csv$", full.names = TRUE)
ms_numbers <- dplyr::bind_rows(lapply(num_files, function(f) {
  x <- utils::read.csv(f, colClasses = "character")
  x$script <- sub("\\.csv$", "", basename(f))
  x$updated <- format(file.mtime(f), "%Y-%m-%d %H:%M")
  x
}))
ms_numbers <- ms_numbers[, c("script", "key", "value", "description", "updated")]
writexl::write_xlsx(ms_numbers, here::here("outputs", "tables", "manuscript_numbers.xlsx"))
rm(num_files, ms_numbers)
