my_prepTDWG <- function (x, sep = ", ", format = "last_init", pretty = TRUE, 
    get.prep = FALSE, get.initials = TRUE, max.initials = 4) 
{
    patt <- paste0("\\p{L}", sep, "\\p{L}")
    commas <- grepl(patt, x, perl = TRUE)
    NAs <- x %in% c("", " ", NA)
    words <- grepl(" ", x, fixed = TRUE)
    oa <- grepl("O'[A-Z]|O' [A-Z]", x, perl = TRUE)
    if (any(oa)) 
        x[oa] <- gsub("(O') ([A-Z])", "\\1\\2", x[oa], perl = TRUE)
    mac <- grepl("Mac[A-Z]|Mac [A-Z]", x, perl = TRUE)
    if (any(mac)) 
        x[mac] <- gsub("(Mac) ([A-Z])", "\\1\\2", x[mac], perl = TRUE)
    mc <- grepl("Mc[A-Z]|Mc [A-Z]", x, perl = TRUE)
    if (any(mc)) 
        x[mc] <- gsub("(Mc) ([A-Z])", "\\1\\2", x[mc], perl = TRUE)
    nomes <- x
    if (any(commas & words & !NAs)) {
        split <- matrix(c(my_lastName(x[commas & words & !NAs], 
            first.capital = FALSE), my_lastName(x[commas & words & 
            !NAs], invert = TRUE, initials = FALSE, first.capital = FALSE)), 
            ncol = 2)
        split <- plantR:::getPrep(split, rm.prep = FALSE)
        if (get.initials) {
            remove_these <- grepl(" \\([A-Z]", split[, 2]) & 
                grepl("[A-Z]\\)", split[, 2])
            if (any(remove_these)) 
                split[, 2][remove_these] <- gsub(" \\([A-Z].*", 
                  "", split[, 2][remove_these], perl = TRUE)
            check_caps <- split[, 1] == toupper(split[, 1]) & 
                split[, 2] == toupper(split[, 2]) & !split[, 
                2] %in% ""
            if (any(check_caps)) {
                split[, 2][check_caps] <- gsub("(*UCP)[^;\\&\\-\\\\'\\s](?<!\\b\\p{L})", 
                  "", split[, 2][check_caps], perl = TRUE)
                split[, 2][check_caps] <- gsub("(\\p{L})", "\\1.", 
                  split[, 2][check_caps], perl = TRUE)
                split[, 2][check_caps] <- gsub("\\.,\\.|\\.\\.", 
                  ".", split[, 2][check_caps], perl = TRUE)
            }
            split[, 2][!check_caps] <- plantR:::getInit(split[, 2][!check_caps], 
                max.initials = 10, rm.spaces = pretty)
        }
        else {
            split[, 2] <- gsub("\\s+", " ", split[, 2], perl = TRUE)
            split[, 2] <- gsub("^ | $", "", split[, 2], perl = TRUE)
        }
        if (pretty) {
            if (get.initials) {
                split[, 2] <- gsub(" ", "", split[, 2], fixed = TRUE)
            }
            else {
                not_empty <- !split[, 2] %in% ""
                init_caps <- split[, 2] == toupper(split[, 2])
                is_inits <- nchar(gsub("(*UCP)[^;](?<!\\p{L})", 
                  "", split[, 2], perl = TRUE)) < max.initials
                check_inits <- init_caps & is_inits & not_empty
                split[, 2][check_inits] <- gsub("(\\p{Lu})(\\s)(\\p{Lu})+", 
                  "\\1.\\2\\3.", split[, 2][check_inits], perl = TRUE)
                split[, 2][check_inits] <- gsub("(\\p{Lu})(\\p{Lu})+", 
                  "\\1.\\2.", split[, 2][check_inits], perl = TRUE)
                split[, 2][check_inits] <- gsub("(\\p{Lu})(\\.\\s)(\\p{Lu}$)", 
                  "\\1\\2\\3.", split[, 2][check_inits], perl = TRUE)
                split[, 2][check_inits] <- gsub("(^\\p{Lu}$)", 
                  "\\1.", split[, 2][check_inits], perl = TRUE)
                w_abbrev <- grepl("\\.", split[, 2], perl = TRUE)
                init_low <- split[, 2] == tolower(split[, 2])
                check_these <- !w_abbrev & (init_caps | init_low) & 
                  not_empty
                if (any(check_these)) 
                  split[, 2][check_these] <- plantR:::capName(split[, 
                    2][check_these])
                split[, 2] <- gsub("(\\.) (\\p{L}\\.)", "\\1\\2", 
                  split[, 2], perl = TRUE)
                split[, 2] <- gsub("(\\.) (\\p{L}\\.)", "\\1\\2", 
                  split[, 2], perl = TRUE)
                split[, 2][init_low] <- gsub("(\\p{Ll})(\\.)+", 
                  "\\U\\1\\2", split[, 2][init_low], perl = TRUE)
            }
            split[, 1] <- plantR:::capName(split[, 1])
            split[, 3] <- tolower(split[, 3])
        }
        if (get.prep) {
            if (format %in% c("last_init", "last_init_prep")) 
                names <- paste(paste(split[, 1], split[, 2], 
                  sep = sep), split[, 3])
            if (format %in% c("prep_last_init")) 
                names <- paste(split[, 3], paste(split[, 1], 
                  split[, 2], sep = sep))
            if (format %in% c("init_last")) 
                names <- paste(split[, 2], split[, 3], split[, 
                  1])
        }
        else {
            if (format %in% c("last_init", "last_init_prep", 
                "prep_last_init")) 
                names <- paste(split[, 1], split[, 2], sep = sep)
            if (format %in% c("init_last")) 
                names <- paste(split[, 2], split[, 1])
        }
        names <- gsub("\\s+", " ", names, perl = TRUE)
        names <- gsub("^ | $", "", names, perl = TRUE)
        names <- gsub("\\.\\s\\.", ".", names, perl = TRUE)
        names <- gsub(",$", "", names, perl = TRUE)
        nomes[commas & words & !NAs] <- names
    }
    if (any(!commas & words & !NAs)) {
        remove_these <- grepl(" \\([A-Z]", x[!commas & words & 
            !NAs]) & grepl("[A-Z]\\)$", x[!commas & words & !NAs])
        if (any(remove_these)) 
            x[!commas & words & !NAs][remove_these] <- gsub(" \\([A-Z].*", 
                "", x[!commas & words & !NAs][remove_these], 
                perl = TRUE)
        split <- matrix(c(my_lastName(x[!commas & words & !NAs], 
            first.capital = FALSE), my_lastName(x[!commas & words & 
            !NAs], invert = TRUE, initials = FALSE, first.capital = FALSE)), 
            ncol = 2)
        split <- plantR:::getPrep(split, rm.prep = FALSE)
        if (get.initials) {
            check_caps <- split[, 1] == toupper(split[, 1]) & 
                split[, 2] == toupper(split[, 2]) & !split[, 
                2] %in% ""
            if (any(check_caps)) {
                split[, 2][check_caps] <- gsub("(*UCP)[^;\\&\\-\\\\'\\s](?<!\\b\\p{L})", 
                  "", split[, 2][check_caps], perl = TRUE)
                split[, 2][check_caps] <- gsub("(\\p{L})", "\\1.", 
                  split[, 2][check_caps], perl = TRUE)
                split[, 2][check_caps] <- gsub("\\.,\\.|\\.\\.", 
                  ".", split[, 2][check_caps], perl = TRUE)
            }
            split[, 2][!check_caps] <- plantR:::getInit(split[, 2][!check_caps], 
                max.initials = 10, rm.spaces = pretty)
            split[, 2][split[, 2] %in% "character(0)."] <- ""
        }
        else {
            split[, 2] <- gsub("\\s+", " ", split[, 2], perl = TRUE)
            split[, 2] <- gsub("^ | $", "", split[, 2], perl = TRUE)
        }
        if (pretty) {
            if (get.initials) {
                split[, 2] <- gsub(" ", "", split[, 2], fixed = TRUE)
            }
            else {
                not_empty <- !split[, 2] %in% ""
                init_caps <- split[, 2] == toupper(split[, 2])
                is_inits <- nchar(gsub("(*UCP)[^;](?<!\\p{L})", 
                  "", split[, 2], perl = TRUE)) < max.initials
                check_inits <- init_caps & is_inits & not_empty
                split[, 2][check_inits] <- gsub("(\\p{Lu})(\\s)(\\p{Lu})+", 
                  "\\1.\\2\\3.", split[, 2][check_inits], perl = TRUE)
                split[, 2][check_inits] <- gsub("(\\p{Lu})(\\p{Lu})+", 
                  "\\1.\\2.", split[, 2][check_inits], perl = TRUE)
                split[, 2][check_inits] <- gsub("(\\p{Lu})(\\.\\s)(\\p{Lu}$)", 
                  "\\1\\2\\3.", split[, 2][check_inits], perl = TRUE)
                split[, 2][check_inits] <- gsub("(^\\p{Lu}$)", 
                  "\\1.", split[, 2][check_inits], perl = TRUE)
                w_abbrev <- grepl("\\.", split[, 2], perl = TRUE)
                init_low <- split[, 2] == tolower(split[, 2])
                check_these <- !w_abbrev & (init_caps | init_low) & 
                  not_empty
                if (any(check_these)) 
                  split[, 2][check_these] <- plantR:::capName(split[, 
                    2][check_these])
                split[, 2] <- gsub("(\\.) (\\p{L}\\.)", "\\1\\2", 
                  split[, 2], perl = TRUE)
                split[, 2] <- gsub("(\\.) (\\p{L}\\.)", "\\1\\2", 
                  split[, 2], perl = TRUE)
                split[, 2][init_low] <- gsub("(\\p{Ll})(\\.)+", 
                  "\\U\\1\\2", split[, 2][init_low], perl = TRUE)
            }
            split[, 1] <- plantR:::capName(split[, 1])
            split[, 3] <- tolower(split[, 3])
        }
        if (get.prep) {
            if (format %in% c("last_init", "last_init_prep")) 
                names <- paste(paste(split[, 1], split[, 2], 
                  sep = sep), split[, 3])
            if (format %in% c("prep_last_init")) 
                names <- paste(split[, 3], paste(split[, 1], 
                  split[, 2], sep = sep))
            if (format %in% c("init_last")) 
                names <- paste(split[, 2], split[, 3], split[, 
                  1])
        }
        else {
            if (format %in% c("last_init", "last_init_prep", 
                "prep_last_init")) 
                names <- paste(split[, 1], split[, 2], sep = sep)
            if (format %in% c("init_last")) 
                names <- paste(split[, 2], split[, 1])
        }
        names <- gsub("\\s+", " ", names, perl = TRUE)
        names <- gsub("^ | $", "", names, perl = TRUE)
        names <- gsub(",$", "", names, perl = TRUE)
        names <- gsub("\\.\\s\\.", ".", names, perl = TRUE)
        nomes[!commas & words & !NAs] <- names
    }
    if (any(!words & !NAs)) 
        nomes[!words & !NAs] <- plantR:::capName(nomes[!words & !NAs])
    if (any(NAs)) 
        nomes[NAs] <- NA_character_
    if (any(oa)) 
        nomes[oa] <- gsub("(O')([a-z])", "\\1\\U\\2", nomes[oa], 
            perl = TRUE)
    if (any(mac)) 
        nomes[mac] <- gsub("(Mac)([a-z])", "\\1\\U\\2", nomes[mac], 
            perl = TRUE)
    if (any(mc)) 
        nomes[mc] <- gsub("(Mc)([a-z])", "\\1\\U\\2", nomes[mc], 
            perl = TRUE)
    return(as.character(nomes))
}