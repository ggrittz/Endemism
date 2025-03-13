my_lastName <- function (name, noName = "s.n.", invert = FALSE, initials = FALSE, 
          first.capital = TRUE) 
{
  miss.name <- name %in% c("NA", NA, "", " ", noName)
  ##### My addition to remove the trailing comma #####
  name <- gsub("^,", "", name)
  name <- gsub("[.]", ". ", name, perl = TRUE)
  name <- gsub("\\s+", " ", name, perl = TRUE)
  name <- gsub("^ | $", "", name, perl = TRUE)
  comma <- grepl("\\p{L},", name[!miss.name], perl = TRUE)
  no.first <- !grepl(" ", name[!miss.name], perl = TRUE)
  outros <- !no.first & !comma
  last.name <- name[!miss.name]
  if (any(comma)) 
    last.name[comma] <- gsub(",.*", "", last.name[comma], 
                             perl = TRUE)
  if (any(outros)) 
    last.name[outros] <- gsub(".*\\s", "", last.name[outros], 
                              perl = TRUE)
  probs <- grepl("^\\p{L}\\.$", last.name, perl = TRUE)
  if (any(probs)) 
    last.name[probs] <- gsub(" \\p{L}\\..*", "", name[!miss.name][probs], 
                             perl = TRUE)
  probs1 <- last.name == toupper(last.name) & !grepl("\\.|,", 
                                                     name[!miss.name], perl = TRUE) & grepl(" ", name[!miss.name], 
                                                                                            fixed = TRUE)
  if (any(probs1)) {
    nome1 <- gsub(" .*", "", name[!miss.name][probs1], perl = TRUE)
    nome2 <- gsub(".* ", "", name[!miss.name][probs1], perl = TRUE)
    nome2[nchar(nome1) > nchar(nome2)] <- nome1[nchar(nome1) > 
                                                  nchar(nome2)]
    last.name[probs1] <- nome2
  }
  cp.nome <- rep("", length(last.name))
  cp <- c("Filho", "Neto", "Jr.", "Junior", "Sobrinho")
  cp <- c(cp, tolower(cp))
  gen.suf <- last.name %in% cp
  if (any(gen.suf)) {
    cp.nome[gen.suf] <- last.name[gen.suf]
    last.name[gen.suf] <- name[!miss.name][gen.suf]
    last.name[gen.suf] <- mapply(function(x, y) {
      gsub(x, "", y, fixed = TRUE)
    }, cp.nome[gen.suf], last.name[gen.suf])
    last.name[gen.suf] <- gsub(" $", "", last.name[gen.suf], 
                               perl = TRUE)
    last.name[gen.suf] <- gsub(".*\\s", "", last.name[gen.suf], 
                               perl = TRUE)
    double.cp <- last.name[gen.suf] %in% ""
    last.name[gen.suf][!double.cp] <- paste(last.name[gen.suf][!double.cp], 
                                            cp.nome[gen.suf][!double.cp], sep = " ")
    last.name[gen.suf][double.cp] <- cp.nome[gen.suf][double.cp]
  }
  if (invert) {
    other.names <- mapply(function(x, y) {
      if (x == "") {
        return(y)  # Return the original string if `last.name` is empty
      } else {
        return(gsub(x, "", y, fixed = TRUE))  # Otherwise, perform the replacement
      }
    }, last.name, name[!miss.name])
    check_these <- other.names == name[!miss.name] & grepl(" ", 
                                                           last.name, fixed = TRUE)
    if (any(check_these)) {
      last.name1 <- sapply(strsplit(last.name[check_these], 
                                    " ", fixed = TRUE), stringr::str_c, collapse = "|")
      last.name1 <- gsub("(^\\p{Lu})(\\|)", " \\1$\\2", 
                         last.name1, perl = TRUE)
      last.name1 <- gsub("(^\\p{Lu})\\.(\\|)", " \\1\\\\.$\\2", 
                         last.name1, perl = TRUE)
      other.names1 <- other.names[check_these]
      other.names2 <- mapply(function(x, y) {
        gsub(x, "", y, perl = TRUE)
      }, last.name1, other.names1)
      other.names[check_these] <- other.names2
    }
    other.names <- gsub("\\s+", " ", other.names, perl = TRUE)
    other.names <- gsub("^ | $", "", other.names, perl = TRUE)
    other.names <- gsub(", ", "", other.names, fixed = TRUE)
    seps <- c(";|&|\\|| e | y | and | und | et ")
    mult.names <- grepl(seps, other.names, perl = TRUE)
    if (any(mult.names)) 
      other.names[mult.names] <- gsub(";.*|&.*|\\|.*| e .*| y .*| and .*| und .*| et .*", 
                                      "", other.names[mult.names], perl = TRUE)
    if (initials) {
      inits <- grepl("\\p{Lu}", other.names, perl = TRUE)
      abrev.inits <- grepl("(\\p{L}\\.)(\\p{L}\\.)+|(\\p{L}\\.)\\s(\\p{L}\\.)+", 
                           other.names, perl = TRUE)
      combo <- inits | abrev.inits
      inits[abrev.inits] <- FALSE
      if (any(inits)) {
        w_spaces <- grepl(" ", other.names[inits], fixed = TRUE)
        other.names[inits][w_spaces] <- gsub("\\b(\\p{Lu})|.", 
                                             "\\1", other.names[inits][w_spaces], perl = TRUE)
        other.names[inits][!w_spaces] <- gsub("[^\\p{Lu}]+", 
                                              "", other.names[inits][!w_spaces], perl = TRUE)
      }
      if (any(abrev.inits)) {
        other.names[abrev.inits] <- gsub("[^\\p{L}]+", 
                                         "", other.names[abrev.inits], perl = TRUE)
        other.names[abrev.inits] <- toupper(gsub("(?<=\\p{Lu})\\p{Ll}.*", 
                                                 "", other.names[abrev.inits], perl = TRUE))
      }
      if (any(!combo)) 
        other.names[!combo] <- toupper(substr(other.names[!combo], 
                                              1, 1))
      other.names <- gsub("(\\p{Lu})", "\\1.", other.names, 
                          perl = TRUE)
    }
    name[!miss.name] <- other.names
  }
  else {
    last.name <- gsub("([A-Z]\\.) ([A-Z]\\.).", "\\1\\2", 
                      last.name, perl = TRUE)
    if (first.capital) 
      last.name <- capName(last.name)
    last.name <- gsub("(^\\p{Lu})(\\s\\p{Lu})", "\\1.\\2", 
                      last.name, perl = TRUE)
    name[!miss.name] <- last.name
  }
  if (any(miss.name)) 
    name[miss.name] <- noName
  return(name)
}