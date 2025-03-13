# Making bfoNames with taxon distribution

#Creating directory
file_path <- here::here("data", "raw-data", "flora_do_brasil")
if (!dir.exists(file_path)) 
  dir.create(file_path)

#Downloading
flora_version <- "393.406"
file_name <- "flora_do_brasil.zip"
url <- paste0("https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil&v=", flora_version)
download.file(url, file.path(file_path, file_name), mode = "wb")

#Extracting zip
unzip(file.path(file_path, file_name),
      exdir = file_path)

# Getting dist and merging
flora_dist <- readr::read_tsv(file.path(file_path, "distribution.txt"))
flora_dist$id <- paste0("bfo-", flora_dist$id)

# Assuming your data is in a data frame called 'df'
flora_dist <- aggregate(locationID ~ id, data = flora_dist, 
                        FUN = function(x) paste(unique(x), collapse = "|"))

# View the resulting data frame
df_grouped

bfoNames_test <- dplyr::left_join(bfoNames, flora_dist[, c("id", "locationID")],
                                  by = 'id')

# Prepare to save
file_path1 <- here::here("data")
file_name1 <- "bfoNames_test.rds"
saveRDS(bfoNames_test, file.path(file_path1, file_name1))
