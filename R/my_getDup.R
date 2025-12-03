my_getDup <- function (df = NULL, flag.ind = TRUE) 
{
  if (!inherits(df, "data.frame")) 
    stop("Input object needs to be a data frame!")
  if (dim(df)[1] == 0) 
    stop("Input data frame is empty!")
  tmp.ordem <- numTombo <- value <- i.memb <- new_id <- dup.ind.test <- . <- NULL
  rec.ID <- names(df)[1]
  str_names <- names(df)[-1]
  changeCols <- sapply(df, class) == "factor"
  if (any(changeCols)) {
    changeCols <- names(changeCols)[changeCols]
    for (i in 1:length(changeCols)) df[, changeCols[i]] <- as.character(df[, 
                                                                           changeCols[i]])
  }
  df$dup.check <- NA
  df$dup.numb <- NA
  df$dup.prop <- NA
  df$dup.ID <- NA
  for (i in 1:length(str_names)) {
    col_name <- paste0("dup.check", i)
    forward <- duplicated(df[, str_names[i]], incomparables = NA)
    reverse <- duplicated(df[, str_names[i]], incomparables = NA, 
                          fromLast = TRUE)
    df[, col_name] <- forward | reverse
  }
  df$dup.numb[apply(df[, names(df) %in% str_names, drop = FALSE], 
                    1, function(x) all(is.na(x)))] <- "cc"
  df$dup.numb[apply(df[, grepl("dup.check[0-9]", names(df)), 
                       drop = FALSE], 1, function(x) all(x))] <- length(str_names)
  df$dup.numb[is.na(df$dup.numb)] <- apply(df[is.na(df$dup.numb), 
                                              grepl("dup.check[0-9]", names(df)), drop = FALSE], 1, 
                                           sum, na.rm = TRUE)
  tmp <- df[, grepl("dup.check[0-9]", names(df)), drop = FALSE]
  tmp1 <- apply(tmp, 1, function(x) sum(!is.na(x)))
  tmp1[!df$dup.numb %in% "cc"] <- as.double(df$dup.numb[!df$dup.numb %in% 
                                                          "cc"])/tmp1[!df$dup.numb %in% "cc"]
  tmp1[df$dup.numb %in% "cc"] <- "cc"
  df$dup.prop <- tmp1
  df$dup.check[!df$dup.numb %in% "cc"] <- df$dup.numb[!df$dup.numb %in% 
                                                        "cc"] > 0
  df$dup.check[df$dup.numb %in% "cc"] <- "cannot_check"
  df1 <- df[df$dup.check %in% "TRUE", ]
  if (dim(df1)[1] > 0) {
    dt <- data.table::data.table(df1)
    dt[, `:=`(tmp.ordem, .I), ]
    data.table::setnames(dt, rec.ID, "numTombo")
    dt[duplicated(numTombo), `:=`(numTombo, paste(numTombo, 
                                                  1:.N, sep = "_dup")), by = numTombo]
    cols <- c(rec.ID, str_names)
    d <- data.table::melt.data.table(dt[, 1:(length(cols)), 
    ], id.vars = "numTombo", na.rm = TRUE)
    
    ##### Removing leftover NAs for some reason #####
    d <- d[!is.na(d$numTombo), ]
    g <- igraph::graph_from_data_frame(d[, .(numTombo, value)])
    memb <- igraph::components(g)$membership
    memb <- data.table::data.table(numTombo = names(memb), 
                                   memb = memb)
    data.table::setDT(dt)[memb, `:=`(memb, i.memb), on = c(numTombo = "numTombo")]
    `%like.ic%` <- function(x, pattern) {
      grepl(pattern, x, perl = TRUE, ignore.case = TRUE)
    }
    dt[numTombo %like.ic% "_dup[0-9]", `:=`(numTombo, gsub("_dup[0-9]", 
                                                           "", numTombo, perl = TRUE))]
    data.table::setkeyv(dt, c("memb"))
    dt[!is.na(memb), `:=`(new_id, as.character(paste0(sort(unique(numTombo)), 
                                                      collapse = "|"))), by = memb]
    if (flag.ind) {
      dup.cols <- data.table::copy(names(dt))
      dup.cols <- dup.cols[grepl("dup.check[1-9]", dup.cols, 
                                 perl = TRUE)]
      for (i in seq_along(str_names)) {
        data.table::setkeyv(dt, c(str_names[i]))
        dt[NA_character_, `:=`((dup.cols[i]), NA)]
      }
      data.table::setkey(dt, new_id)
      dt[, `:=`(dup.ind.test, lapply(.SD, function(x) any(sum(x) == 
                                                            0, na.rm = TRUE))), by = new_id, .SDcols = c(dup.cols)]
      dt[!is.na(new_id), `:=`(new_id, ifelse(dup.ind.test, 
                                             paste0("[", new_id, "]", collapse = ""), new_id)), 
         by = new_id]
    }
    dt[dt[, .I[.N == 1], by = memb]$V1, `:=`(new_id, NA)]
    data.table::setkeyv(dt, c("new_id"))
    dt[!is.na(new_id) & !new_id %like.ic% "\\|", `:=`(new_id, 
                                                      paste0(new_id, "[virtual]", collapse = "")), by = new_id]
    data.table::setkeyv(dt, c("tmp.ordem"))
    df$dup.ID[df$dup.check %in% "TRUE"] <- as.character(dt$new_id)
  }
  drop.cols <- names(df)[grepl("dup.check", names(df))]
  df1 <- df[, -which(names(df) %in% drop.cols)]
  return(df1)
}