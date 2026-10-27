# Download JABOT data -----------------------------------------------------

# https://ipt.jbrj.gov.br/jabot/resource?r= is the base url.

# Load the list of herbaria available in JABOT 
file_path <- here::here("data", "derived-data")
file_name <- "jabot_herbarium_list.txt" # EDIT: updated on 25/09/26
herbaria <- data.table::fread(file.path(file_path, file_name))
herbaria <- as.data.frame(herbaria)
herbaria <- herbaria[order(herbaria$herbarium), ]
# Path to save
path_to_save <- here::here("data", "raw-data", "JABOT")
if(!dir.exists(path_to_save))
  dir.create(path_to_save)

# From JABOT IPT, get each herbarium and the most updated version of the data

for(i in seq_along(herbaria)){
  
  cat("Downloading", herbaria[i], "\n")
  
  # Get JABOT IPT url from each herbarium
  url <- paste0("https://ipt.jbrj.gov.br/jabot/resource?r=", herbaria[i])
  
  tryCatch({
    
    # Get its html content
    html_content <- rvest::read_html(url, encoding = "ISO-8859-1")
    
    # Find the node containing the most updated version (SelectorGadget addon is useful here)
    node <- rvest::html_node(html_content, css = "td a")
    
    # Get href attribute
    link <- rvest::html_attr(node, "href")
    
    # Download and save
    name_to_save <- paste0(herbaria[i], ".zip")
    download.file(url = link, destfile = file.path(path_to_save, name_to_save), mode = "wb")
    
  }, error = function(e) cat("  FAILED:", herbaria[i], "-", conditionMessage(e), "\n"))  # EDIT
  
}

# Extract each zip file before binding it all
for(h in herbaria){
  zip_file <- file.path(path_to_save, paste0(h, ".zip"))
  if(file.exists(zip_file))
    utils::unzip(zip_file, exdir = file.path(path_to_save, h))
}

occ_files <- file.path(path_to_save, herbaria, "occurrence.txt")
has_data <- file.exists(occ_files)
cat("Herbaria without occurrence data:", herbaria[!has_data], "\n")

# Load each occurrence.txt from each collection and bind it
herbarios <- lapply(seq_along(occ_files[has_data]), function(i){
  cat("Reading herbaria", herbaria[has_data][i], "\n")
  data.table::fread(file = occ_files[has_data][i],
                    sep = "\t", quote = "",
                    encoding = "UTF-8",
                    colClasses = "character")
})
names(herbarios) <- herbaria[has_data]

# Remove empty data frames (herbarium removed from JABOT or something)
herbarios <- herbarios[sapply(herbarios, function(x) x[, .N] > 0)]

# dplyr::bind_rows also fills missing columns with NA
all_data <- dplyr::bind_rows(herbarios, .id = 'my_id')  # my_id now holds the herbarium code

# Save
data.table::fwrite(all_data, file = file.path(path_to_save, "jabot_occurrences.txt"))
