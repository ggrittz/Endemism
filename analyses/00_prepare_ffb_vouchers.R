
# Prepare FFB vouchers ----------------------------------------------------

# Only useful ids (trees already kinda prepared)
fbo <- file.path("..", "Taxonomy", "data", "raw-data", 
                 "trees_and_shrubs_flora_do_brasil.rds")
fbo <- readRDS(fbo)
# All of the accepted ids already contain all necessary vouchers
fbo <- fbo[fbo$taxonomicStatus %in% "NOME_ACEITO", ]
# Types here
types <- file.path("..", "Taxonomy", "data", "raw-data", "flora_do_brasil",
                   "typesandspecimen.txt")
types <- as.data.frame(readr::read_tsv(types))

# Subset only what I have as names
types <- types[types$id %in% fbo$id_fbo, ]

# Remove s.n. vouchers; these cannot be confidently considered
types <- types[!grepl(" s\\.n\\.", types$recordedBy), ]
types <- types[!is.na(types$recordedBy), ]
# # Clean the recordedBy " s.n." (just remove from the string)
# # since plantR will add it automatically if it is NA
# types$recordedBy <- gsub(" s\\.n\\.", "", types$recordedBy)
types$recordedBy.new <- plantR::prepName(types$recordedBy,
                                         output = "first",
                                         sep.out = "; ")
types$recordNumber.new <- plantR::colNumber(types$recordedBy, 
                                            noNumb = "s.n.")
# Also remove from here...
types <- types[!grepl("s\\.n\\.", types$recordNumber.new), ] 

# Get scientific name
types <- dplyr::left_join(types, fbo[, c("id_fbo",
                                               "taxonName",
                                               "taxonAuthorship")], 
                             by = c("id" = "id_fbo"))
types$scientificName <- plantR:::buildName(types,
                                           col.names = c("taxonName",
                                                         "taxonAuthorship"))
types$voucher_these <- paste(types$recordedBy.new,
                             types$recordNumber.new,
                             sep = "_")
# Save
save_path <- here::here("data", "derived-data", "rds")
save_name <- "ffb_vouchers.rds"
saveRDS(types, file.path(save_path, save_name))


