# CNCFlora: Livro Vermelho 2013 -------------------------------------------
file_path <- here::here("data", "raw-data", "CNCFlora")
if (!dir.exists(file_path)) 
 dir.create(file_path)

# Downloading occurrences from Livro Vermelho da Flora de 2013
cncflora_version <- "v=2.23"
file_name <- "cncflora2013_occurrences.zip"
url <- paste0("https://ipt.jbrj.gov.br/jbrj/archive.do?r=redlist_2013_occs&", cncflora_version)
download.file(url, file.path(file_path, file_name), mode = "wb")

# Extracting zip
unzip(file.path(file_path, file_name),
      exdir = file_path)