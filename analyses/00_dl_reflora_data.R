# Download REFLORA data ---------------------------------------------------

# https://ipt.jbrj.gov.br/reflora/resource?r= is the base url.

# Load the list of herbaria available in REFLORA 
file_path <- here::here("data", "derived-data")
file_name <- "reflora_herbarium_list.txt"
herbaria <- data.table::fread(file.path(file_path, file_name))
herbaria <- as.data.frame(herbaria)
herbaria <- herbaria[order(herbaria$herbarium), ]
# Path to save
path_to_save <- here::here("data", "raw-data", "REFLORA")

# From REFLORA IPT, get each herbarium and the most updated version of the data
# 
# for(i in seq_along(herbaria)){
#   
#   cat("Downloading", herbaria[i], "\n")
#   
#   # Get REFLORA IPT url
#   url <- paste0("https://ipt.jbrj.gov.br/reflora/resource?r=", herbaria[i])
#   
#   # Get its html content
#   html_content <- rvest::read_html(url, encoding = "ISO-8859-1")
#   
#   # Find the node containing the most updated version (SelectorGadget addon is useful here)
#   node <- rvest::html_node(html_content, css = "td a")
#   
#   # Get href attribute
#   link <- rvest::html_attr(node, "href")
#   
#   # Download and save
#   name_to_save <- paste0(herbaria[i], ".zip")
#   download.file(url = link, destfile = file.path(path_to_save, name_to_save), mode = "wb")
# 
# }
# 
# # Extract each zip file before binding it all
# zip_files <- list.files(path_to_save, full.names = TRUE)
# 
# for(i in seq_along(herbaria)){
#   utils::unzip(zip_files[i], exdir = file.path(path_to_save, herbaria[i]))
# }

# Load each occurrence.txt from each collection and bind it
folders <- list.files(path_to_save, full.names = TRUE)
# folders <- folders[-grep("\\.zip", folders)]
# remove acam since it does not have a DwC
folders <- folders[!grepl("acam$", folders)]
herbarios <- vector("list", length(folders))
names(herbarios) <- herbaria[!grepl("acam", herbaria)]

for(i in seq_along(folders)){
  herbarios[[i]] <- data.table::fread(file.path(folders[i], "occurrence.txt"))
}

# Bad formatting in REFLORA, so I am making all columns as.character
herbarios <- lapply(herbarios, function(x) {
  x[] <- lapply(x, as.character)
  return(x)
})

# dplyr::bind_rows also fills missing columns with NA
all_data <- dplyr::bind_rows(herbarios, .id = 'my_id')

# Save to my D:/
data.table::fwrite(all_data, file = "D:/reflora_occurrences.txt")



