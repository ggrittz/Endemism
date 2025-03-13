################################################################################################################################################################
################################################################################################################################################################
### OTHER FUNCTIONS ###

## Vector to remove accents and special characters
unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                      'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='S', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                      'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                      'ö'='o', 'ø'='o', 'ü'='u', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

## Quick function to correct capitalized names ##
#Already in the package as 'capName'
simpleCap <- function(x) {
	if(grepl('-',x)) {
		s <- strsplit(x, "-")[[1]]
		nome = paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = "-")
	} else {
		s <- strsplit(x, " ")[[1]]
		nome = paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
	}
	return(nome) 
}

## Adapted Hans' function for name fingerprint ##
fingerprint = function(name, asciify = F) {
	name = gsub("[^[:alpha:]]", "", tolower(name))     # remove non a-z, incl spaces
	fp   = sort(strsplit(name,split=character(0))[[1]])    # count & sort characters
 	return(paste(fp, collapse = ""))
}

## Hans' original function for name fingerprint ##
fingerprint1 = function(name, asciify = F) {
  name = gsub("[^[:alpha:]]", "", tolower(name))     # remove non a-z, incl spaces

  #if(asciify == T){ 
  #  input = unlist(strsplit(name,""))  
  #  output = character(length(input))    
  #  for(i in seq_along(input)) output[i] = funny2ascii[match(input[i],funny2ascii[,1]),2]
  #  name = paste(output, collapse="") 
  #}
 fp   = table(strsplit(name,split=character(0)))    # count & sort characters
 return(paste(names(fp), collapse = ""))
}

## Hans' function to find 1st last name
lastname1 = function(name, name.length = 5, asciify = F){
  name = tolower(name)                                # all chars to lower
  name = gsub("[.]", ". ", name)                      # 
  name = gsub("[,]", ". ", name)                      # 
  name = gsub("[;]", ". ", name)                      # 
  name = gsub(" ", "Z", name)                         # spaces -> Z
  name = gsub("ZZ", "Z", name)                        # ZZ -> Z
  name = gsub("ZZ", "Z", name)                        # ZZ -> Z
  name = gsub("[^[:alpha:]]", "", name)               # remove non a-z
  if(name == "" | is.na(name)) return("NA")

  #if(asciify == T){ 
  #  input = unlist(strsplit(name,""))  
  #  output = character(length(input))    
  #  for(i in seq_along(input)) output[i] = funny2ascii[match(input[i],funny2ascii[,1]),2]
  #  name = paste(output, collapse="") 
  #}

  names = unlist(strsplit(name,"Z"))                  # split o names and initials
  if (length(names) < 2) return(names)

  lastname = "a"; i = 1
  while((nchar(lastname) < name.length) & (i <= length(names))){
    lastname = names[i]
    i = i + 1
  }
  return(lastname)
}

name="Cesar Sandro, Esteves, F"

## Adptation of Hans' function to get the last name of the author
#Already in the package as 'lastName'
lastname = function(name) {
  name = gsub("[.]", ". ", name)                         # adding a space between points 
  name = gsub("  ", " ", name)                           # removing double spaces

  names = unlist(strsplit(name," "))                     # split o names and initials
  names = as.character(unlist(sapply(names,simpleCap)))  # capitalizing first letter of each name
  if (length(names) < 2) return(tolower(names))		    # stop if there is only one name

  if(any(grepl(",",names))) { 
	lastname = head(names[grep(",",names)],1) 
  } else { 
	lastname = names[[length(names)]]  
  }  

  cp = c("Filho","Filho,","Neto","Neto,","Júnior","Júnior,","Jr.","Jr.,","Junior","Junior,","Sobrinho","Sobrinho,") #compound names
  if (sum(names %in% cp)>0) {
	lastname = tail(names[!names %in% cp],1)
	cp.nome = names[names %in% cp]
	other.names = names[!names %in% cp & !names %in% lastname]
  } else {
	lastname = lastname
	other.names = names[1:(length(names)-1)]
  }

  i = 1
  while((nchar(lastname) < 3) & (i <= length(names))){ #putting last names in the good order
    lastname = names[i]
    other.names = names[!names %in% c(lastname)]
    i = i + 1
  }

  lastname = gsub("\\.$","",lastname)
  if(sum(names %in% cp)==0) { 
	  lastname = lastname 
	  other.names = other.names
	} else {
       cp.nome = gsub("\\.$","",cp.nome)
	  lastname = paste(lastname,cp.nome,sep=" ")
	  other.names = other.names[!other.names %in% cp.nome]  
  }

  if (length(lastname)>1) { 
		lastname = lastname[grepl(",",lastname)] 
		lastname = head(lastname,1)
  } else { lastname = lastname }
  lastname = tolower(gsub('\\.|,',"",lastname))	
  return(lastname)
}

#lastname("Lima, I.B.")
#lastname("Lima, I. B. ")
#lastname("Lima, Isabel B. ")
#lastname("Lima, Isabel Benedictes")
#lastname("Isabel Benedictes Lima")
#lastname("R.A.F. Lima")
#lastname("R.A. F. Lima  ")
#lastname("R.A. F. Lima ")
#lastname("Renato Augusto Ferreira de Lima")
#lastname("RENATO A. FERREIRA DE LIMA")
#lastname("Renato Augusto Ferreira de Lima Filho")
#lastname("Lima R.A.")
#lastname("Lima R.A ")
#lastname("Lima R.A.F.")
#lastname("Lima R. A. F. ")
#lastname("LIMA R. A. F. ") 
#lastname("LIMA R. A. F. S.") 
#lastname("A. Stival-Santos.")
#lastname("Leitão Filho, H.F.")
#lastname("Hermogenes Leitão Filho")
#lastname("Leitão Filho, Hermogenes F.")
#lastname("Lucie Tiphane Zinger")
#lastname("Cesar Sandro, Esteves, F")
#lastname("Filho Neto, S.J.")
#lastname("Mendonca Filho, C.V. Neto, F.C.C.")

## Adptation of Hans' function to get unabbreviated names in the good format
#Already in the package as 'tdwgName'
format.name = function(name) {
  name = gsub("[.]", ". ", name)                         # adding a space between points 
  name = gsub("  ", " ", name)                           # removing double spaces

  # removing unwanted characters
  name = gsub(", --$| --, --|^--", "", name)

  # removing treatment prepositions (e.g. Dr., Prof., Pe., ...)
  name = gsub("^Dr\\. |Pe\\. |Prof\\. ","",name)

  names = unlist(strsplit(name," "))                     # split o names and initials
  names = as.character(unlist(sapply(names,simpleCap)))  # capitalizing first letter of each name

  if (length(names) < 2) return(names)				    # stop if there is only one name
    
  lastname = names[[length(names)]]
  
  cp = c("Filho","Filho,","Neto","Neto,","Jr.","Jr.,","Junior","Junior,","Júnior","Júnior,","Sobrinho","Sobrinho,") #compound names
    
  if (sum(names %in% cp)>0) {
  	lastname = tail(names[!names %in% cp],1)
  	cp.nome = names[names %in% cp]
  	other.names = names[!names %in% cp & !names %in% lastname]
  } else {
  	lastname = lastname
  	other.names = names[1:(length(names)-1)]
  }

  i = 1
  while((nchar(lastname) < 3) & (i <= length(names))){ #putting last names in the good order
    lastname = names[i]
    other.names = names[!names %in% c(lastname)]
    i = i + 1
  }

  lastname = gsub("\\.$","",lastname)
  if(sum(names %in% cp)==0) { 
	  lastname = lastname 
	  other.names = other.names
	} else {
    cp.nome = gsub("\\.$","",cp.nome)
	  lastname = paste(lastname,cp.nome,sep=" ")
	  other.names = other.names[!other.names %in% cp.nome]  
  }

  #Creating and editing the name initials
  initials = sapply(other.names, function(x) toupper(strsplit(x,"")[[1]][1]))

  #Editing the name initials
  if(any(initials=="-")) initials[initials=="-"] = substr(names(initials[initials=="-"]),1,2)
  initials = initials[!grepl("^De$|^Dos$|^Do$|^Da$|^Das$|^Von$|^Van$|^Van Der$|^Van Den$|^Ter$",names(initials))] 	
  initials = paste(initials,collapse=".")
  initials = paste(initials,".",sep="")

  # Creating the name in the TDWG format
  name.correct = paste(lastname,initials,sep=", ")
  # Final edits (removing duplicated commas and bad name endings)
  name.correct = gsub(",,", ",",name.correct)
  name.correct = gsub(", \\.$", "",name.correct)

  return(name.correct)
}

format.name("V. Bittrich")

#format.name("R.A.F. Lima")
#format.name("R.A. F. Lima  ")
#format.name("R.A. F. Lima ")
#format.name("Renato Augusto Ferreira de Lima")
#format.name("RENATO A. FERREIRA DE LIMA")
#format.name("Renato Augusto Ferreira de Lima Filho")
#format.name("Lima R.A.")
#format.name("Lima R.A ")
#format.name("Lima R.A.F.")
#format.name("Lima R. A. F. ")
#format.name("LIMA R. A. F. ") 
#format.name("LIMA R. A. F. S.") 
#format.name("A. Stival-Santos.")
#format.name("Leitão Filho H.F.")
#format.name("Lucie Tiphane Zinger")


## Function to standardize name notation
#Already in the package as function fixName
format.name1 = function(name) {
  	if(!grepl("[a-z;A-Z], ",name)) {
		#name.correct = format.name(name)
		name.correct = name
	  } else {
		names = strsplit(name,", ")[[1]]
		lastname = simpleCap(names[1])
		initials = strsplit(names[2],"")[[1]]
		if(any(grepl('[A-Z]',initials))) {
  		initials = initials[grepl('[A-Z]',initials)]
		} else {
		  if(nchar(initials)==1) { 
		    initials = toupper(initials)
		  } else {
		    initials = initials 
		  }  
		}	
		initials = paste(paste(initials,collapse="."),".",sep="")
  	name.correct = paste(lastname,initials,sep=", ")
		name.correct = gsub("\\.\\.\\.",".",name.correct)
		name.correct = gsub("\\.\\.",".",name.correct)
	}
  return(name.correct)
}

#format.name1("Soares, CRA")
#format.name1("Lyra-Lemos, R.P.")
#format.name1("Soares R.P.")
#format.name1("Amorim, AMADL")
#format.name1("YABE, RS")
#format.name1("Ziffer Berger, J")                                       
#format.name1("Ziffer Berger, Richter")
#format.name1("Zandoval, Jose Anibal")
#format.name1("Zartman, C E")
#format.name1("Lima, Isabel Benedictes")

################################################################################################################################################################
################################################################################################################################################################
### Adapted from: https://gist.github.com/erinshellman/7405574

#funtion to convert dictionary lists into a data frame
dict_to_df = function(dict) {
  
  require(plyr)
  df = data.frame()
  df_temp = list()
  store = list()
  
  for (i in 1:length(dict)) {
    cat(i,"\n")
    if(!is.na(dict[i])) {
     # Split up the dictionary entry
     split = unlist(strsplit(dict[i], '\\"\\],'))	
     split = gsub('\\{|\\[', '', split)
     split = gsub('\\}|\\]', '', split)
     #split = noquote(split)	
     values = unlist(strsplit(split, ':'))

     # Parse out what will be the df headers
     headers = values[seq(1, length(values), 2)]       
     headers = gsub('\"', '', headers) # Remove quotes
     headers = gsub(' ', '', headers)  # and whitespace

     # Parse out what will be the df values
     row_values = values[seq(0, length(values), 2)]
     row_values = gsub('\"', '', row_values) # Remove quotes
     row_values = gsub(' ', '', row_values)  # and whitespace

     # Construct a dataframe with 1 row
     out = data.frame(t(row_values))
     colnames(out) = headers

     store[i] = list(out)

    } else {
     toto = data.frame(t(rep(NA,dim(store[[1]])[2])))
     colnames(toto) = colnames(store[[1]])
     store[[i]] = toto
    }	
  }
  df = rbind.fill(store)
  return(df)
}

################################################################################################################################################################
################################################################################################################################################################
# Author: Tony Breyal
# Date: 2011-11-18
# Modified: 2011-11-18
# Description: Extracts all text from a webpage (aims to extract only the text you would see in a web browser)
# Packages Used: RCurl, XML   
# Blog Reference: Not published

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}


# ###--- EXAMPLES ---###
# # Example 1: url input
# input <- "http://www.google.co.uk/search?gcx=c&sourceid=chrome&ie=UTF-8&q=r+project#pq=%22hello+%3C+world%22&hl=en&cp=5&gs_id=3r&xhr=t&q=phd+comics&pf=p&sclient=psy-ab&source=hp&pbx=1&oq=phd+c&aq=0&aqi=g4&aql=&gs_sm=&gs_upl=&bav=on.2,or.r_gc.r_pw.r_cp.,cf.osb&fp=27ff09b2758eb4df&biw=1599&bih=904"
# txt <- htmlToText(input)
# txt