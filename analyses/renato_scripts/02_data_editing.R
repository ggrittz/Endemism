############################################# -
############################################# -
### DATA EDITING AND PRELIMINARY CLEANING ###
############################################# -
############################################# -

memory.limit(size=60000)
rm(list=ls())
########################################################################################### -
#### Finding and standardizing collector's and deteminer's names and collection number ####
########################################################################################### -

### Loading packages and functions  ###
require(RCurl)
require(stringr)
source("functions.R")
require(rgdal)
require(utils)
require(finch)
#require(devtools)
require(dplyr)
require(countrycode)
require(rgeos)
require(raster)

#### 1- Loading and editing speciesLink, speciesLink_new and JABOT data ####

#Finding the data - speciesLink, speciesLink_new and JABOT
paths = c(dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE),
          dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new",full.names=TRUE),
		      dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot",full.names=TRUE),
		      dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//sndb_mincyt_arg",full.names=TRUE))

#Looping for each collection - speciesLink, speciesLink_new and JABOT
for(i in 1:length(paths)) {
    myfiles0 = list.files(paths[i], full.names = TRUE)
		myfiles0 = myfiles0[grep("-edited.csv",myfiles0)]
		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA")) 
	##Removing unwanted columns
		cols = c("collectionCode","collectioncode","siglacolecao","catalogNumber","catalognumber","numtombo",
		         "recordNumber","collectornumber","codespecime","numcoleta",
		         "recordedBy","recordedBy1","collector","coletor","coletor1",
		         "year","month","day","yearcollected","monthcollected","daycollected","diacoleta","mescoleta","anocoleta",
		         "country","pais","stateProvince","stateprovince","estado_prov","municipality","county","cidade",
		         "locality","descrlocal",
		         "decimalLatitude","decimalLongitude","latitude","longitude",
		         "identifiedBy","dateIdentified","typeStatus","identifiedby","yearidentified","typestatus","determinador","diadeterm","mesdeterm","anodeterm","nat_typus",	
		         "family","familia","genus","genero","scientificName","scientificname",
		         "datasetkey","datelastmodified","occurrenceRemarks","notes","notes.x","notas")
		cls =  cols[cols %in% names(herb.data)]
		herb.data = herb.data[,cls] 
	##Getting the vector with authors' names
		coluna = names(herb.data)[names(herb.data) %in% c("recordedBy","recordedby","coletor","collector")]
		nomes = herb.data[,coluna] 
	##Editing collectors names
	  #Separation between multiple authors		
		nomes = gsub("  "," ",nomes) 
		nomes = gsub(' & | e | \\| ',"; ",nomes) #### INCLUDE HERE: ' et '
		nomes = gsub("&|\\|",";",nomes)
		nomes = gsub("; ;",";",nomes)
		nomes = gsub(" ;",";",nomes)
		nomes = gsub('^\\;','',nomes)
		nomes = gsub('\\( ','\\(',nomes)
		nomes = gsub(' \\)','\\)',nomes)
		nomes = gsub(' <U+','; ',nomes)
		nomes = gsub(" s.n. ",";",nomes)
		nomes = gsub("Collector\\(s\\):","",nomes)
		nomes = gsub("\\(Coll.",";",nomes)
		nomes = gsub(" \\(\\?\\) ","; ",nomes)
		nomes = as.character(unlist(sapply(strsplit(nomes,";"), head, n=1)))
	   #Removing unwanted character
		nomes = gsub("[0-9]","",nomes)
		nomes = gsub('\\(\\)|\\( \\)',"",nomes)
		nomes = gsub('\\[|\\]',"",nomes)
		nomes = gsub(' - | -|- ',"-",nomes)
		nomes = gsub('^-- ',"",nomes)
		nomes = gsub(' --$',"",nomes)
		nomes = gsub('^-\\.$','SemColetor',nomes)
		nomes = gsub('^-\\. ','',nomes)
		nomes[grepl('^<',nomes)&grepl('>$',nomes)] = "EncodingError"
		nomes = gsub('!','',nomes)
		nomes = gsub('^\\* ','',nomes)
	   #Removing "et al." expression
		nomes = gsub(" et alii| Et alii| Et Alii| et alli| Et alli| et all$|et alii$","",nomes)
		nomes = gsub(" et\\. al\\.$| et\\.al\\.$| et al\\.$","",nomes)
		nomes = gsub(" at al\\.$| etal\\.$| et,al\\.$| et, al\\.$|et. al\\.$","",nomes)
		nomes = gsub(" et\\. al$| et\\.al$| et\\. al\\.\\.$","",nomes)
		nomes = gsub(" et\\. al,\\.$| et\\.a l\\.$| et\\. al,\\.$| et\\. a\\.$","",nomes)
		nomes = gsub(" et\\.al,\\.$| et\\.al\\.,$","",nomes)
		nomes = gsub(" et al $| et al\\. $","",nomes)
		nomes = gsub(" el al\\.$| Et al\\.$| Et Al\\.$| Et\\. Al\\.$| at al\\.$| et Al\\.$","",nomes)
		nomes = gsub(" et al$|et al$","",nomes)
	   #Compound names
		nomes = gsub("Jr\\.|jr\\.| jr$| Jr$"," Junior",nomes)
		nomes = gsub(" - Júnior| - Junior"," Junior",nomes)
		nomes = gsub(", Júnior,|, Junior,"," Junior,",nomes)
		nomes = gsub("-Júnior|-Junior"," Junior",nomes)
		nomes = gsub("JAºnior|jAºnior","Junior",nomes)
		nomes = gsub(" F°| Fº"," Filho",nomes)
		nomes = gsub(' f\\.,'," Filho",nomes)
		nomes = gsub(" - Filho"," Filho",nomes)
		nomes = gsub("-Filho"," Filho",nomes)
		nomes = gsub("- Filho"," Filho",nomes)
		nomes = gsub(" Filho\\.,"," Filho,",nomes)
		nomes = gsub(" Sobr°| Sobrº"," Sobrinho",nomes) 
		nomes = gsub(" Sobrinho\\.,| Sobr\\.,"," Sobrinho,",nomes)
	   #Formatting names without abbreviation
		nomes = gsub("     "," ",nomes)
		nomes = gsub("    "," ",nomes)
		nomes = gsub("   "," ",nomes)
		nomes = gsub("  "," ",nomes)
		nomes = str_trim(nomes)
		nomes = gsub('^\\.|^,','',nomes)
		nomes = gsub(',$','',nomes)
		nomes = gsub('-$','',nomes)
		nomes = str_trim(nomes)
		nomes[!grepl("\\.$",nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!grepl("\\.$",nomes)&!grepl(",",nomes)],format.name))
	   #Editing prefixs or prepositions
		nomes = gsub(' de$| do$| dos$| da$| van den$| van der$| van$| den$| von$| ter$',"",nomes, ignore.case=TRUE)
		nomes = gsub('\\.de$|\\.do$|\\.dos$|\\.da$|\\.van den$|\\.van der$|\\.von$|\\.ter$',"",nomes, ignore.case=TRUE)
		nomes = gsub(' de la |\\. de |\\. do |\\. dos |\\. da |\\. van den |\\. van der |\\. von |\\. ter ',"",nomes)
		nomes = gsub(', de |, do |, dos |, da |, van den |, van der |, von |, ter ',"",nomes)
	   #Standardizing division between last and initials
		nomes = gsub("  "," ",nomes)
		nomes = gsub(",",", ",nomes)
		nomes = gsub("  "," ",nomes)
		nomes = gsub(" ,",",",nomes)
		nomes = gsub("  "," ",nomes)
	   #Standardizing separation within initials
		nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)] = gsub("\\. ","\\.",nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)])
	   #Encoding problems
		nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
		nomes = as.character(iconv(nomes, from="UTF-8", to="windows-1252//TRANSLIT"))
		nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
		nomes = str_trim(nomes)
	   #No collector information
		nomes[is.na(nomes)|nomes %in% ""] = "SemColetor"
		nomes[nomes %in% c("s/col.","s/col","s/c","s/coletor"," sem col.","s.col.")] = "SemColetor"
		nomes[nomes %in% c("Collector unspecified","Collector unknown")] = "SemColetor" 
		nomes[nomes %in% c("?")] = "SemColetor"
		nomes = gsub("NANA","SemColetor",nomes)
	   #Last stardardization
		nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1] = as.character(unlist(sapply(nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1],format.name1)))
	   #Final corrections
		nomes = gsub('\\. \\.',".",nomes)
		nomes = gsub('\\(\\.\\)',".",nomes)
		nomes = gsub("\\.\\'\\.",".",nomes)
		nomes = gsub('\\.\\.\\.',".",nomes)
		nomes = gsub('\\.\\.',".",nomes)
		nomes = gsub('\\/,',",",nomes)
		nomes = gsub('\\/-,',',',nomes)
		nomes = gsub(',,',",",nomes)
		nomes = str_trim(nomes)
		#final edits
		nomes = gsub('a³',"o",nomes)
		nomes = gsub('a©|aª',"e",nomes)
		nomes = gsub('a³',"o",nomes)
		nomes = gsub("a¡","a",nomes)
		nomes = gsub('aº|a¼|a¼',"u",nomes)
		nomes = gsub("a§","c",nomes)
		nomes = gsub("a´","o",nomes)
		nomes = gsub("a¢|a¡|a£|a¤","a",nomes)
	 #Saving the editions and new name columns
		herb.data$coletor.name = nomes
		herb.data$coletor.last.name = as.vector(unlist(sapply(nomes,lastname)))
		herb.data$coletor.fingerprint = as.vector(sapply(nomes,fingerprint))
		 #Replacing names to decrease file size
		herb.data$coletor.last.name[herb.data$coletor.name %in% "SemColetor"] = NA
		herb.data$coletor.fingerprint[herb.data$coletor.name %in% "SemColetor"] = NA
		
	##Getting the vector with determiners' names
		coluna = names(herb.data)[names(herb.data) %in% c("identifiedBy","identifiedby","determinador")]
		nomes = herb.data[,coluna]
	   #Separation between multiple authors		
		nomes = gsub("  "," ",nomes) 
		nomes = gsub(' & | e | \\| ',"; ",nomes)
		nomes = gsub("&|\\|",";",nomes)
		nomes = gsub("; ;",";",nomes)
		nomes = gsub(" ;",";",nomes)
		nomes = gsub('^\\;','',nomes)
		nomes = gsub('\\( ','\\(',nomes)
		nomes = gsub(' \\)','\\)',nomes)
		nomes = gsub(' <U+','; ',nomes)
		nomes = gsub(" s.n. ",";",nomes)
		nomes = gsub('Data:|Date:',";",nomes, ignore.case=T)
		#preparing to remove dates of determination
		nomes = gsub(', [0-9][0-9]|\\. [0-9][0-9]',";",nomes, ignore.case=T)
		nomes = gsub(', [0-9]/|, [0-9]-|\\. [0-9]/',";",nomes, ignore.case=T)
		nomes = gsub(" [0-9][0-9]| \\'[0-9][0-9]",";",nomes, ignore.case=T)
		#preparing to remove months of determination
		meses = paste(paste(", ",month.name,"/",sep="",collapse="|"),paste(", ",month.abb,"/",sep="",collapse="|"),collapse="|")
		nomes = gsub(meses,";",nomes)
		meses = paste(paste(", ",month.name,"-",sep="",collapse="|"),paste(", ",month.abb,"-",sep="",collapse="|"),collapse="|")
		nomes = gsub(meses,";",nomes)
		meses = paste(paste(", ",month.name," ",sep="",collapse="|"),paste(", ",month.abb," ",sep="",collapse="|"),collapse="|")
		nomes = gsub(meses,";",nomes)
		meses = paste(paste("/",month.name,sep="",collapse="|"),paste("/",month.abb,sep="",collapse="|"),paste("/",month.abb,".",sep="",collapse="|"),collapse="|")
		nomes = gsub(meses,";",nomes)
		nomes[grep("\\([A-Z]|[A-Z]\\)",nomes)] = gsub(" \\(","; ",nomes[grep("\\([A-Z]|[A-Z]\\)",nomes)])
		nomes = gsub(" \\(\\?\\) ","; ",nomes)
		nomes[nomes==""] = "SemDeterminador"
		#final detection of years
		nomes = gsub("/[0-9][0-9]",";",nomes, ignore.case=T)
		#splitting the cetor to get only names
		nomes = as.character(unlist(sapply(strsplit(nomes,";|/"), head, n=1)))
	   
	 #Removing unwanted character/expressions
		nomes = gsub("in dupl\\.: ","",nomes) 
		nomes = gsub("[0-9]","",nomes)
		nomes = gsub('\\(\\)|\\( \\)',"",nomes)
		nomes = gsub(' - | -|- ',"-",nomes)
		nomes = gsub('\\[|\\]',"",nomes)
		nomes = gsub(', date unknown',"",nomes, ignore.case=T)
		nomes = gsub('Determiner unknown|Determiner unspecified|Incógnito','SemDeterminador',nomes,ignore.case=T)
		nomes = gsub('^det\\. by|',"",nomes, ignore.case=T)
		nomes = gsub('^det\\.:|^det:',"",nomes, ignore.case=T)
		nomes = gsub('^det\\.|^det\\. ',"",nomes, ignore.case=T)
		nomes = gsub('^-- ',"",nomes)
		nomes = gsub(' --$',"",nomes)
		nomes = gsub('^-\\.$','SemDeterminador',nomes)
		nomes = gsub('^-\\. ','',nomes)
		nomes[grepl('^<',nomes)&grepl('>$',nomes)] = "EncodingError"
		nomes = gsub('!','',nomes)
		nomes = gsub('^\\* ','',nomes)
	   #Removing the type indication
		nomes = gsub("-Paratype|, Paratype|, paratype|-TYPE|-Lectotype| (Lectotype)| (Paratype)| ,(Isotype:MO)|, Syntype|-Syntype","",nomes)
		nomes = gsub("Isotype: MO\\/ |Isotype:MO| Isolectotype|Isotype|, US - Syntype","",nomes)
	   #Removing "et al." expression
		nomes = gsub(" et alii| Et alii| Et Alii| et alli| Et alli| et all$|et alii$","",nomes)
		nomes = gsub(" et\\. al\\.$| et\\.al\\.$| et al\\.$","",nomes)
		nomes = gsub(" at al\\.$| etal\\.$| et,al\\.$| et, al\\.$|et. al\\.$","",nomes)
		nomes = gsub(" et\\. al$| et\\.al$| et\\. al\\.\\.$","",nomes)
		nomes = gsub(" et\\. al,\\.$| et\\.a l\\.$| et\\. al,\\.$| et\\. a\\.$","",nomes)
		nomes = gsub(" et\\.al,\\.$| et\\.al\\.,$","",nomes)
		nomes = gsub(" et al $| et al\\. $","",nomes)
		nomes = gsub(" el al\\.$| Et al\\.$| Et Al\\.$| Et\\. Al\\.$| at al\\.$| et Al\\.$","",nomes)
		nomes = gsub(" et al$|et al$","",nomes)
	   #Compound names
		nomes = gsub("Jr\\.|jr\\.| jr$| Jr$"," Junior",nomes)
		nomes = gsub(" - Júnior| - Junior"," Junior",nomes)
		nomes = gsub(", Júnior,|, Junior,"," Junior,",nomes)
		nomes = gsub("-Júnior|-Junior"," Junior",nomes)
		nomes = gsub("JAºnior|jAºnior","Junior",nomes)
		nomes = gsub(" F°| Fº"," Filho",nomes)
		nomes = gsub(' f\\.,'," Filho",nomes)
		nomes = gsub(" - Filho"," Filho",nomes)
		nomes = gsub("-Filho"," Filho",nomes)
		nomes = gsub("- Filho"," Filho",nomes)
		nomes = gsub(" Filho\\.,"," Filho,",nomes)
		nomes = gsub(" Sobr°| Sobrº"," Sobrinho",nomes) 
		nomes = gsub(" Sobrinho\\.,| Sobr\\.,"," Sobrinho,",nomes)
	   #Formatting names without abbreviation
		nomes = gsub("     "," ",nomes)
		nomes = gsub("    "," ",nomes)
		nomes = gsub("   "," ",nomes)
		nomes = gsub("  "," ",nomes)
		nomes = str_trim(nomes)
		nomes = gsub('^\\.|^,','',nomes)
		nomes = gsub(',$','',nomes)
		nomes = gsub('-$','',nomes)
		nomes = str_trim(nomes)
		nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)],format.name))
	   #Editing prefixs or prepositions
		nomes = gsub(' de$| do$| dos$| da$| van den$| van der$| van$| den$| von$| ter$',"",nomes, ignore.case=TRUE)
		nomes = gsub('\\.de$|\\.do$|\\.dos$|\\.da$|\\.van den$|\\.van der$|\\.von$|\\.ter$',"",nomes, ignore.case=TRUE)
		nomes = gsub(' de la |\\. de |\\. do |\\. dos |\\. da |\\. van den |\\. van der |\\. von |\\. ter ',"",nomes)
		nomes = gsub(', de |, do |, dos |, da |, van den |, van der |, von |, ter ',"",nomes)
	   #Standardizing division between last and initials
		nomes = gsub("  "," ",nomes)
		nomes = gsub(",",", ",nomes)
		nomes = gsub("  "," ",nomes)
		nomes = gsub(" ,",",",nomes)
		nomes = gsub("  "," ",nomes)
	   #Standardizing separation within initials
		nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)] = gsub("\\. ","\\.",nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)])
	   #Encoding problems
		nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
		nomes = as.character(iconv(nomes, from="UTF-8", to="windows-1252//TRANSLIT"))
		nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
		nomes = str_trim(nomes)
	   #Last stardardization
		nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1] = as.character(unlist(sapply(nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1],format.name1)))
	   #No determiner information
		nomes[is.na(nomes)|nomes %in% ""] = "SemDeterminador"
		nomes[nomes %in% c("s/det.","s/det","s/d","s/determinador"," sem det.","s.det.")] = "SemDeterminador"
		nomes[nomes %in% c("Determiner unspecified","Determiner unknown")] = "SemDeterminador" 
		nomes[nomes %in% c("?")] = "SemDeterminador"
		nomes = gsub("NANA","SemDeterminador",nomes)
		if(any(grepl('\\(\\?\\)',nomes))) nomes[grep('\\(\\?\\)',nomes)] = 	sapply(strsplit(nomes[grep('\\(\\?\\)',nomes)],"\\(\\?\\)"), function(x) format.name(x[1]))
	 #Final edits
		nomes = gsub(", --$","",nomes)
		nomes = gsub(', //$','',nomes)
		nomes = gsub(', /$','',nomes)
		nomes = gsub(', \'$','',nomes)
		nomes = gsub('\\.\\.$','\\.',nomes)
		nomes = gsub('\\.\\.$','\\.',nomes)
		nomes = gsub('\\. /$','\\.',nomes)
		nomes = gsub('^Disponible, N\\.$','SemDeterminador',nomes)
		nomes = gsub('^Sin$','SemDeterminador',nomes)
		nomes = gsub('^Pjm, M\\.$','Maas, P.J.M.',nomes)
		nomes = gsub('AraAºjo|Araaºjo','Araujo',nomes)
		nomes = gsub('a£o|aƒo','ao',nomes)
		nomes = gsub('ra£es','raes',nomes)
	   #Super final starndardization
		nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)],format.name))
		nomes[!is.na(nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!is.na(nomes)&!grepl(",",nomes)],format.name))
	   #Super extra final corrections
		nomes = gsub('\\. \\.',".",nomes)
		nomes = gsub('\\(\\.\\)',".",nomes)
		nomes = gsub("\\.\\'\\.",".",nomes)
		nomes = gsub('\\.\\.\\.',".",nomes)
		nomes = gsub('\\.\\.',".",nomes)
		nomes = gsub('\\/,',",",nomes)
		nomes = gsub('\\/-,',',',nomes)
		nomes = gsub(',,',",",nomes)
		nomes = str_trim(nomes)
		#Uber Super extra final corrections
		nomes = gsub('a³',"o",nomes)
		nomes = gsub('a©|aª',"e",nomes)
		nomes = gsub('a³',"o",nomes)             
		nomes = gsub("a¡","a",nomes)
		nomes = gsub('aº|a¼|a¼',"u",nomes)
		nomes = gsub("a§","c",nomes)
		nomes = gsub("a´","o",nomes)
		nomes = gsub("a¢|a¡|a£|a¤","a",nomes)
  #Saving the editions and new name columns
		herb.data$determinador.name = nomes
		herb.data$determinador.last.name = as.vector(unlist(sapply(nomes,lastname)))
		herb.data$determinador.fingerprint = as.vector(sapply(nomes,fingerprint))
		 #Replacing names to descrease file size
		herb.data$determinador.last.name[herb.data$determinador.name %in% "Semdeterminador"] = NA
		herb.data$determinador.fingerprint[herb.data$determinador.name %in% "Semdeterminador"] = NA

	##Getting the vector with collectors numbers
		coluna = names(herb.data)[names(herb.data) %in% c("recordNumber","recordnumber","numcoleta","collectornumber")]
		numbs = herb.data[,coluna] 
	  #Editing collectors numbers
		numbs = gsub('s\\.n\\.|s\\.n|s/nº|S/N|S\\.N\\.|s/nº|s/n°|^s/n$|^s/n\\.$',"SemNumero",numbs)
		numbs[numbs %in% "sn"] = "SemNumero"
		numbs[is.na(numbs)|numbs %in% c("")] = "SemNumero"
		numbs[numbs %in% c("Number unspecified")] = "SemNumero" 
		numbs[is.na(numbs)&grepl(" s.n. ",numbs)] = "SemNumero"
		#Removing names of collectors from the numbers
		numbs[!is.na(numbs)&grepl("[a-z][a-z][a-z] ",numbs,ignore.case = TRUE)] = as.character(sapply(strsplit(numbs[!is.na(numbs)&grepl("[a-z][a-z][a-z] ",numbs,ignore.case = TRUE)], " "),function(x) x[grepl('[0-9]|SemNumero',x)]))
		numbs[!is.na(numbs)&grepl("SemNumero",numbs)] = "SemNumero" 
		numbs[!is.na(numbs)&grepl("character\\(0\\)",numbs)] = "SemNumero"  
		#Removing unwanted characters
		numbs = gsub(' - ',"-",numbs)
		numbs = gsub(' \\(',"\\(",numbs)
		numbs = gsub(' ',"-",numbs)
		numbs = gsub(' e ',", ",numbs)
		numbs = gsub('#|\\?|\\!|\\.',"",numbs)
		numbs = gsub(", ",",",numbs)
		numbs = gsub("Collector Number:","",numbs)
		numbs = gsub("NANA","SemNumero",numbs)
		numbs = gsub('^--$',"SemNumero",numbs)
		numbs = gsub('^-',"",numbs)
		numbs = gsub('-$',"",numbs)
		numbs = gsub("SemNumero","s.n.",numbs)
		herb.data$coletor.number = str_trim(numbs)		
		
	##Editing the determination date
	##TAKE CODED FROM THE: CREATING COLLECTION AND SPECIMEN UNIQUE IDENTIFIERS AND PREPARING DATA FOR MERGE (03_data_validation.R) 	
		
	##Saving the edited/filtered tree records
		path.csv = gsub("-edited.csv","-edited-cleaned.csv",myfiles0)
		write.csv(x=herb.data,file=path.csv, row.names = FALSE)
		cat(i,"\n")
}


#### 2- Loading and editing GBIF data ####
#Editing the data
paths = list.files("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE)
paths = paths[grep("-edited.csv",paths)]

#Looping for each collection - GBIF
for(i in 1:length(paths)) {
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,header=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns
		cols = c("collectionCode","catalogNumber","recordNumber","recordedBy", 
			"collectioncode","catalognumber","recordnumber","recordedby",		
			"year","month","day","countrycode","stateProvince","municipality","county","locality",
			"decimallatitude","decimallongitude","identifiedby","dateIdentified","typestatus",	
			"family","genus","species","scientificName","taxonrank","datasetkey","collectioncode","license","rightsholder","occurrenceRemarks")
		cls =  unique(cols[cols %in% names(herb.data)])
		herb.data = herb.data[,cls] 
	##Getting the vector with authors' names
		coluna = names(herb.data)[names(herb.data) %in% c("recordedBy","recordedby","coletor")]
		nomes = herb.data[,coluna] 
	##Editing collectors names
	   #Separation between multiple authors
		nomes = gsub("  "," ",nomes) 
		nomes = gsub(' & | e | \\| ',"; ",nomes)
		nomes = gsub("&|\\|",";",nomes)
		nomes = gsub("; ;",";",nomes)
		nomes = gsub(" ;",";",nomes)
		nomes = gsub('^\\;','',nomes)
		nomes = gsub('\\( ','\\(',nomes)
		nomes = gsub(' \\)','\\)',nomes)
		nomes = gsub(' <U+','; ',nomes)
		nomes = gsub(" s.n. ",";",nomes)
		nomes = gsub("Collector\\(s\\):","",nomes)
		nomes = gsub("\\(Coll.",";",nomes)
		nomes = gsub(" \\(\\?\\) ","; ",nomes)
		nomes[nomes%in%""] = "SemDeterminador"
		nomes = as.character(unlist(sapply(strsplit(nomes,";"), head, n=1)))
	   #Removing unwanted character
		nomes = gsub("[0-9]","",nomes)
		nomes = gsub('\\(\\)|\\( \\)',"",nomes)
		nomes = gsub('\\[|\\]',"",nomes)
		nomes = gsub(' - | -|- ',"-",nomes)
		nomes = gsub('^-- ',"",nomes)
		nomes = gsub(' --$',"",nomes)
		nomes = gsub('^-\\.$','SemColetor',nomes)
		nomes = gsub('^-\\. ','',nomes)
		nomes[grepl('^<',nomes)&grepl('>$',nomes)] = "EncodingError"
		nomes = gsub('!','',nomes)
		nomes = gsub('^\\* ','',nomes)
	   #Removing "et al." expression
		nomes = gsub(" et alii| Et alii| Et Alii| et alli| Et alli| et all$|et alii$","",nomes)
		nomes = gsub(" et\\. al\\.$| et\\.al\\.$| et al\\.$","",nomes)
		nomes = gsub(" at al\\.$| etal\\.$| et,al\\.$| et, al\\.$|et. al\\.$","",nomes)
		nomes = gsub(" et\\. al$| et\\.al$| et\\. al\\.\\.$","",nomes)
		nomes = gsub(" et\\. al,\\.$| et\\.a l\\.$| et\\. al,\\.$| et\\. a\\.$","",nomes)
		nomes = gsub(" et\\.al,\\.$| et\\.al\\.,$","",nomes)
		nomes = gsub(" et al $| et al\\. $","",nomes)
		nomes = gsub(" el al\\.$| Et al\\.$| Et Al\\.$| Et\\. Al\\.$| at al\\.$| et Al\\.$","",nomes)
		nomes = gsub(" et al$|et al$","",nomes)
		nomes = gsub("^et al\\.|et al\\.$","SemDeterminador",nomes)
	   #Compound names
		nomes = gsub("Jr\\.|jr\\.| jr$| Jr$"," Junior",nomes)
		nomes = gsub(" - Júnior| - Junior"," Junior",nomes)
		nomes = gsub(", Júnior,|, Junior,"," Junior,",nomes)
		nomes = gsub("-Júnior|-Junior"," Junior",nomes)
		nomes = gsub("JAºnior|jAºnior","Junior",nomes)
		nomes = gsub(" F°| Fº"," Filho",nomes)
		nomes = gsub(' f\\.,'," Filho",nomes)
		nomes = gsub(" - Filho"," Filho",nomes)
		nomes = gsub("-Filho"," Filho",nomes)
		nomes = gsub("- Filho"," Filho",nomes)
		nomes = gsub(" Filho\\.,"," Filho,",nomes)
		nomes = gsub(" Sobr°| Sobrº"," Sobrinho",nomes) 
		nomes = gsub(" Sobrinho\\.,| Sobr\\.,"," Sobrinho,",nomes)
	   #Formatting names without abbreviation
		nomes = gsub("     "," ",nomes)
		nomes = gsub("    "," ",nomes)
		nomes = gsub("   "," ",nomes)
		nomes = gsub("  "," ",nomes)
		nomes = str_trim(nomes)
		nomes = gsub('^\\.|^,','',nomes)
		nomes = gsub(',$','',nomes)
		nomes = gsub('-$','',nomes)
		nomes = str_trim(nomes)
		nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)],format.name))
	   #Editing prefixs or prepositions
		nomes = gsub(' de$| do$| dos$| da$| van den$| van der$| van$| den$| von$| ter$',"",nomes, ignore.case=TRUE)
		nomes = gsub('\\.de$|\\.do$|\\.dos$|\\.da$|\\.van den$|\\.van der$|\\.von$|\\.ter$',"",nomes, ignore.case=TRUE)
		nomes = gsub(' de la |\\. de |\\. do |\\. dos |\\. da |\\. van den |\\. van der |\\. von |\\. ter ',"",nomes)
		nomes = gsub(', de |, do |, dos |, da |, van den |, van der |, von |, ter ',"",nomes)
	   #Standardizing division between last and initials
		nomes = gsub("  "," ",nomes)
		nomes = gsub(",",", ",nomes)
		nomes = gsub("  "," ",nomes)
		nomes = gsub(" ,",",",nomes)
		nomes = gsub("  "," ",nomes)
	   #Standardizing separation within initials
		nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)] = gsub("\\. ","\\.",nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)])
	   #Encoding problems
		nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
		nomes = as.character(iconv(nomes, from="UTF-8", to="windows-1252//TRANSLIT"))
		nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
		nomes = str_trim(nomes)
	   #Last stardardization
		nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1] = as.character(unlist(sapply(nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1],format.name1)))
	   #No collector information
		nomes[is.na(nomes)|nomes %in% ""] = "SemColetor"
		nomes[nomes %in% c("s/col.","s/col","s/c","s/coletor"," sem col.","s.col.")] = "SemColetor"
		nomes[nomes %in% c("?")] = "SemColetor"
		nomes[nomes %in% c("Collector unspecified","Collector unknown")] = "SemColetor" 
		nomes = gsub("NANA","SemColetor",nomes)
	   #Final corrections
		nomes = gsub('\\. \\.',".",nomes)
		nomes = gsub('\\(\\.\\)',".",nomes)
		nomes = gsub("\\.\\'\\.",".",nomes)
		nomes = gsub('\\.\\.\\.',".",nomes)
		nomes = gsub('\\.\\.',".",nomes)
		nomes = gsub('\\/,',",",nomes)
		nomes = gsub('\\/-,',',',nomes)
		nomes = gsub(',,',",",nomes)
		nomes = str_trim(nomes)
		#Super final corrections
		nomes = gsub('a³',"o",nomes)
		nomes = gsub('a©|aª',"e",nomes)
		nomes = gsub('a³',"o",nomes)             
		nomes = gsub("a¡","a",nomes)
		nomes = gsub('aº|a¼|a¼',"u",nomes)
		nomes = gsub("a§","c",nomes)
		nomes = gsub("a´","o",nomes)
		nomes = gsub("a¢|a¡|a£|a¤","a",nomes)
		
		#Saving the editions and new name columns
		herb.data$coletor.name = nomes
		herb.data$coletor.last.name = as.vector(unlist(sapply(nomes,lastname)))
		herb.data$coletor.fingerprint = as.vector(sapply(nomes,fingerprint))
		#Replacing names to descrease file size
		herb.data$coletor.last.name[herb.data$coletor.name %in% "SemColetor"] = NA
		herb.data$coletor.fingerprint[herb.data$coletor.name %in% "SemColetor"] = NA
		
	##Getting the vector with determiners' names
		coluna = names(herb.data)[names(herb.data) %in% c("identifiedBy","identifiedby","determinador")]
		nomes = herb.data[,coluna]
	   #Separation between multiple authors		
		nomes = gsub("  "," ",nomes) 
		nomes = gsub(' & | e | \\| ',"; ",nomes)
		nomes = gsub("&|\\|",";",nomes)
		nomes = gsub("; ;",";",nomes)
		nomes = gsub(" ;",";",nomes)
		nomes = gsub('^\\;','',nomes)
		nomes = gsub('\\( ','\\(',nomes)
		nomes = gsub(' \\)','\\)',nomes)
		nomes = gsub(' <U+','; ',nomes)
		nomes = gsub(" s.n. ",";",nomes)
		nomes = gsub('Data:|Date:',";",nomes, ignore.case=T)
		#preparing to remove dates of determination
		nomes = gsub(', [0-9][0-9]|\\. [0-9][0-9]',";",nomes, ignore.case=T)
		nomes = gsub(', [0-9]/|, [0-9]-|\\. [0-9]/',";",nomes, ignore.case=T)
		nomes = gsub(" [0-9][0-9]| \\'[0-9][0-9]",";",nomes, ignore.case=T)
		#preparing to remove months of determination
		meses = paste(paste(", ",month.name,"/",sep="",collapse="|"),paste(", ",month.abb,"/",sep="",collapse="|"),collapse="|")
		nomes = gsub(meses,";",nomes)
		meses = paste(paste(", ",month.name,"-",sep="",collapse="|"),paste(", ",month.abb,"-",sep="",collapse="|"),collapse="|")
		nomes = gsub(meses,";",nomes)
		meses = paste(paste(", ",month.name," ",sep="",collapse="|"),paste(", ",month.abb," ",sep="",collapse="|"),collapse="|")
		nomes = gsub(meses,";",nomes)
		meses = paste(paste("/",month.name,sep="",collapse="|"),paste("/",month.abb,sep="",collapse="|"),paste("/",month.abb,".",sep="",collapse="|"),collapse="|")
		nomes = gsub(meses,";",nomes)
		nomes[grep("\\([A-Z]|[A-Z]\\)",nomes)] = gsub(" \\(","; ",nomes[grep("\\([A-Z]|[A-Z]\\)",nomes)])
		nomes = gsub(" \\(\\?\\) ","; ",nomes)
		nomes[nomes==""] = "SemDeterminador"
		#final detection of years
		nomes = gsub("/[0-9][0-9]",";",nomes, ignore.case=T)
		#splitting the cetor to get only names
		nomes = as.character(unlist(sapply(strsplit(nomes,";|/"), head, n=1)))
		
		#Removing unwanted character/expressions
		nomes = gsub("in dupl\\.: ","",nomes) 
		nomes = gsub('\\(\\)|\\( \\)',"",nomes)
		nomes = gsub(' - | -|- ',"-",nomes)
		nomes = gsub('\\[|\\]',"",nomes)
		nomes = gsub(', date unknown',"",nomes, ignore.case=T)
		nomes = gsub('Determiner unknown|Determiner unspecified|Incógnito','SemDeterminador',nomes,ignore.case=T)
		nomes = gsub('^det\\. by|',"",nomes, ignore.case=T)
		nomes = gsub('^det\\.:|^det:',"",nomes, ignore.case=T)
		nomes = gsub('^det\\.|^det\\. ',"",nomes, ignore.case=T)
		nomes = gsub('^-- ',"",nomes)
		nomes = gsub(' --$',"",nomes)
		nomes = gsub('^-\\.$','SemDeterminador',nomes)
		nomes = gsub('^-\\. ','',nomes)
		nomes[grepl('^<',nomes)&grepl('>$',nomes)] = "EncodingError"
		nomes = gsub('!','',nomes)
		nomes = gsub('^\\* ','',nomes)
	   #Removing the type indication
		nomes = gsub("-Paratype|, Paratype|, paratype|-TYPE|-Lectotype| (Lectotype)| (Paratype)| ,(Isotype:MO)|, Syntype|-Syntype","",nomes)
		nomes = gsub("Isotype: MO\\/ |Isotype:MO| Isolectotype|Isotype|, US - Syntype","",nomes)
	   #Removing "et al." expression
		nomes = gsub(" et alii| Et alii| Et Alii| et alli| Et alli| et all$|et alii$","",nomes)
		nomes = gsub(" et\\. al\\.$| et\\.al\\.$| et al\\.$","",nomes)
		nomes = gsub(" at al\\.$| etal\\.$| et,al\\.$| et, al\\.$|et. al\\.$","",nomes)
		nomes = gsub(" et\\. al$| et\\.al$| et\\. al\\.\\.$","",nomes)
		nomes = gsub(" et\\. al,\\.$| et\\.a l\\.$| et\\. al,\\.$| et\\. a\\.$","",nomes)
		nomes = gsub(" et\\.al,\\.$| et\\.al\\.,$","",nomes)
		nomes = gsub(" et al $| et al\\. $","",nomes)
		nomes = gsub(" el al\\.$| Et al\\.$| Et Al\\.$| Et\\. Al\\.$| at al\\.$| et Al\\.$","",nomes)
		nomes = gsub(" et al$|et al$","",nomes)
	   #Compound names
		nomes = gsub("Jr\\.|jr\\.| jr$| Jr$"," Junior",nomes)
		nomes = gsub(" - Júnior| - Junior"," Junior",nomes)
		nomes = gsub(", Júnior,|, Junior,"," Junior,",nomes)
		nomes = gsub("-Júnior|-Junior"," Junior",nomes)
		nomes = gsub("JAºnior|jAºnior","Junior",nomes)
		nomes = gsub(" F°| Fº"," Filho",nomes)
		nomes = gsub(' f\\.,'," Filho",nomes)
		nomes = gsub(" - Filho"," Filho",nomes)
		nomes = gsub("-Filho"," Filho",nomes)
		nomes = gsub("- Filho"," Filho",nomes)
		nomes = gsub(" Filho\\.,"," Filho,",nomes)
		nomes = gsub(" Sobr°| Sobrº"," Sobrinho",nomes) 
		nomes = gsub(" Sobrinho\\.,| Sobr\\.,"," Sobrinho,",nomes)
	   #Formatting names without abbreviation
		nomes = gsub("     "," ",nomes)
		nomes = gsub("    "," ",nomes)
		nomes = gsub("   "," ",nomes)
		nomes = gsub("  "," ",nomes)
		nomes = str_trim(nomes)
		nomes = gsub('^\\.|^,','',nomes)
		nomes = gsub(',$','',nomes)
		nomes = gsub('-$','',nomes)
		nomes = str_trim(nomes)
		nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)],format.name))
	   #Editing prefixs or prepositions
		nomes = gsub(' de$| do$| dos$| da$| van den$| van der$| van$| den$| von$| ter$',"",nomes, ignore.case=TRUE)
		nomes = gsub('\\.de$|\\.do$|\\.dos$|\\.da$|\\.van den$|\\.van der$|\\.von$|\\.ter$',"",nomes, ignore.case=TRUE)
		nomes = gsub(' de la |\\. de |\\. do |\\. dos |\\. da |\\. van den |\\. van der |\\. von |\\. ter ',"",nomes)
		nomes = gsub(', de |, do |, dos |, da |, van den |, van der |, von |, ter ',"",nomes)
	   #Standardizing division between last and initials
		nomes = gsub("  "," ",nomes)
		nomes = gsub(",",", ",nomes)
		nomes = gsub("  "," ",nomes)
		nomes = gsub(" ,",",",nomes)
		nomes = gsub("  "," ",nomes)
	   #Standardizing separation within initials
		nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)] = gsub("\\. ","\\.",nomes[grep("[A-Z]\\. [A-Z]\\.",nomes)])
	   #Encoding problems
		nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
		nomes = as.character(iconv(nomes, from="UTF-8", to="windows-1252//TRANSLIT"))
		nomes = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), nomes)
		nomes = str_trim(nomes)
	   #Last stardardization
		nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1] = as.character(unlist(sapply(nomes[!is.na(nomes)&!grepl('\\.',nomes)&str_count(nomes, ", ")==1],format.name1)))
	   #No collector information
		nomes[is.na(nomes)|nomes %in% ""] = "SemDeterminador"
		nomes[nomes %in% c("s/det.","s/det","s/d","s/determinador"," sem det.","s.det.")] = "SemDeterminador"
		nomes[nomes %in% c("Determiner unspecified","Determiner unknown")] = "SemDeterminador" 
		nomes[nomes %in% c("?")] = "SemDeterminador"
		nomes = gsub("NANA","SemDeterminador",nomes)
		nomes[grep('\\(\\?\\)',nomes)] = 	sapply(strsplit(nomes[grep('\\(\\?\\)',nomes)],"\\(\\?\\)"), function(x) format.name(x[1]))
		nomes = gsub(", --$","",nomes)
		nomes = gsub(', //$','',nomes)
		nomes = gsub(', /$','',nomes)
		nomes = gsub(', \'$','',nomes)
		nomes = gsub('\\.\\.$','\\.',nomes)
		nomes = gsub('\\.\\.$','\\.',nomes)
		nomes = gsub('^Disponible, N\\.$','SemDeterminador',nomes)
		nomes = gsub('^Sin$','SemDeterminador',nomes)
		nomes = gsub('^Pjm, M\\.$','Maas, P.J.M.',nomes)
		nomes = gsub('AraAºjo|Araaºjo','Araujo',nomes)
		nomes = gsub('a£o|aƒo','ao',nomes)
		nomes = gsub('ra£es','raes',nomes)
	   #Super final starndardization
		nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!is.na(nomes)&!grepl("\\.$",nomes)&!grepl(",",nomes)],format.name))
		nomes[!is.na(nomes)&!grepl(",",nomes)] = as.character(sapply(nomes[!is.na(nomes)&!grepl(",",nomes)],format.name))
	   #Super extra final corrections
		nomes = gsub('\\. \\.',".",nomes)
		nomes = gsub('\\(\\.\\)',".",nomes)
		nomes = gsub("\\.\\'\\.",".",nomes)
		nomes = gsub('\\.\\.\\.',".",nomes)
		nomes = gsub('\\.\\.',".",nomes)
		nomes = gsub('\\/,',",",nomes)
		nomes = gsub('\\/-,',',',nomes)
		nomes = gsub(',,',",",nomes)
		nomes = str_trim(nomes)
		#Uber Super extra final corrections
		nomes = gsub('a³',"o",nomes)
		nomes = gsub('a©|aª',"e",nomes)
		nomes = gsub('a³',"o",nomes)             
		nomes = gsub("a¡","a",nomes)
		nomes = gsub('aº|a¼|a¼',"u",nomes)
		nomes = gsub("a§","c",nomes)
		nomes = gsub("a´","o",nomes)
		nomes = gsub("a¢|a¡|a£|a¤","a",nomes)
	 #Saving the editions and new name columns
		herb.data$determinador.name = nomes
		herb.data$determinador.last.name = as.vector(unlist(sapply(nomes,lastname)))
		herb.data$determinador.fingerprint = as.vector(sapply(nomes,fingerprint)) 
 		 #Replacing names to descrease file size
		herb.data$determinador.last.name[herb.data$determinador.name %in% "Semdeterminador"] = NA
		herb.data$determinador.fingerprint[herb.data$determinador.name %in% "Semdeterminador"] = NA
		
	##Getting the vector with collectors numbers
		coluna = names(herb.data)[names(herb.data) %in% c("recordNumber","recordnumber","numcoleta")]
		numbs = herb.data[,coluna] 
	  #Editing collectors numbers
		numbs = gsub('s\\.n\\.|s\\.n|s/nº|S/N|S\\.N\\.|s/nº|s/n°|^s/n$|^s/n\\.$',"SemNumero",numbs)
		numbs[numbs %in% "sn"] = "SemNumero"
		numbs[is.na(numbs)|numbs %in% c("")] = "SemNumero"
		numbs[is.na(numbs)&grepl(" s.n. ",herb.data$recordedby)] = "SemNumero"
		#Removing names of collectors from the numbers
		numbs[!is.na(numbs)&grepl("[a-z][a-z][a-z] ",numbs,ignore.case = TRUE)] = as.character(sapply(strsplit(numbs[!is.na(numbs)&grepl("[a-z][a-z][a-z] ",numbs,ignore.case = TRUE)], " "),function(x) x[grepl('[0-9]|SemNumero',x)]))
		numbs[!is.na(numbs)&grepl("SemNumero",numbs)] = "SemNumero" 
		numbs[!is.na(numbs)&grepl("character\\(0\\)",numbs)] = "SemNumero"  
		#Removing unwanted characters
		numbs = gsub(' - ',"-",numbs)
		numbs = gsub(' \\(',"\\(",numbs)
		numbs = gsub(' ',"-",numbs)
		numbs = gsub(' e ',", ",numbs)
		numbs = gsub('#|\\?|\\!|\\.',"",numbs)
		numbs = gsub(", ",",",numbs)
		numbs = gsub("Collector Number:","",numbs)
		numbs = gsub("NANA","SemNumero",numbs)
		numbs = gsub('^--$',"SemNumero",numbs)
		numbs = gsub('^-',"",numbs)
		numbs = gsub('-$',"",numbs)
		numbs = gsub("SemNumero","s.n.",numbs)
		herb.data$coletor.number = str_trim(numbs)		
		
		##Editing the determination date
		##TAKE CODED FROM THE: CREATING COLLECTION AND SPECIMEN UNIQUE IDENTIFIERS AND PREPARING DATA FOR MERGE (03_data_validation.R) 	
		
	##Saving the edited/filtered tree records
		path.csv = gsub("-edited.csv","-edited-cleaned.csv",myfiles0)
		write.csv(x=herb.data,file=path.csv, row.names = FALSE)
		cat(i,sep="\n")
}

#### 2- Saving collection information from GBIF data ####
#Getting the file paths for the herbarium data
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE)
paths = paths[grepl("-edited.csv",paths)] 

#Loading and merging the different collections
lista = vector("list",length(paths))
for (i in 1:length(paths)) {
  path.csv = paths[i]
  tmp = read.csv(path.csv)
  lista[[i]] = tmp
}
dados.gbif = do.call(rbind,lista)
dados.gbif[] <- lapply(dados.gbif, as.character)
rm(lista)

#Getting number of records per collection 
dados.gbif1 = dados.gbif[,c("catalognumber","institutioncode","collectioncode","datasetkey","license","rightsholder")]
rm(dados.gbif)
tmp = aggregate(dados.gbif1$catalognumber, list(dados.gbif1$collectioncode, dados.gbif1$institutioncode), function(x) length(x))
names(tmp) = c("collectioncode","institutioncode","records")
#Getting metadata available from each collection
tmp1 = dados.gbif1[!duplicated(dados.gbif1$collectioncode, incomparables = NA)&!duplicated(dados.gbif1$institutioncode, incomparables = NA),]
tmp1 = tmp1[order(tmp1$collectioncode),]
tmp1 = tmp1[order(tmp1$collectioncode),c("institutioncode","collectioncode","datasetkey","license","rightsholder")]
tmp2 = left_join(tmp,tmp1,by="institutioncode")
tmp2$name = NA; tmp2$pub.date = NA; tmp2$citation = NA
tmp2 = tmp2[,c("collectioncode.x","collectioncode.y","institutioncode","name","records","pub.date","citation","license","datasetkey","rightsholder")]
#write.csv(tmp2,"gbif_per_collectioncode.csv")
write.csv(tmp2,"gbif_per_collectioncode_new.csv")

#aggregating info per dataset key
tmp3 = aggregate(tmp2$records,list(tmp2$datasetkey), sum, na.rm=TRUE)
names(tmp3) = c("datasetkey","records")
tmp3$collectioncode = aggregate(tmp2$collectioncode,list(tmp2$datasetkey), function(x) paste(unique(x),collapse="|"))$x
tmp3$license = aggregate(tmp2$license,list(tmp2$datasetkey), function(x) paste(unique(x),collapse="|"))$x
tmp3$rightsholder = aggregate(tmp2$rightsholder,list(tmp2$datasetkey), function(x) paste(unique(x),collapse="|"))$x
tmp3$name = NA; tmp3$pub.date = NA; tmp3$citation = NA
tmp3 = tmp3[,c("collectioncode","name","records","pub.date","citation","license","datasetkey","rightsholder")]
#Renaming datasets
names(tmp2) = c("acronym","name","records","pub.date","citation","rights","gbif_UUID","rightsholder")
names(tmp3) = c("acronym","name","records","pub.date","citation","rights","gbif_UUID","rightsholder")
#saving dataframes
write.csv(tmp3,"gbif_per_datasetkey.csv")


################################################################################################################################################################################-		
################################################################################################################################################################################-
#################################################################### -
#### Towards a dictionary of collectors' and specialists' name  ####
#################################################################### -
## Following th standards of the TDWG (International Working Group on Taxonomic Databases for Plant Sciences)
## Standard notation:
#1-General: "Lima, R.A.F."
#2-Compound last names: "González-Medrano, F." or "Leitão Filho, H.F."
#3-Prefixs or prepositions. Better without, but if needed use: "Aart, J. H. van" or "Lima, R.A.F. de" or "De Nardi, Vanderheide" or "O'Conner, S."

## SPECIES LINK AND JABOT
#Finding the data
paths = c(dir("D://Documentos//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE),
		dir("D://Documentos//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot",full.names=TRUE))

coletores = vector("list",length(paths))
#Looping for each collection
for(i in 1:length(paths)) {
		myfiles0 = list.files(paths[i], full.names = TRUE)
		myfiles0 = myfiles0[grep("edited-cleaned.csv",myfiles0)]
		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns and re-naming 
		cols = c("recordedBy","coletor","coletor.name","coletor.last.name","coletor.fingerprint",
				"identifiedBy","determinador","determinador.name","determinador.last.name","determinador.fingerprint")
		cls =  cols[cols %in% names(herb.data)]
		herb.data = herb.data[,cls] 
		names(herb.data) = c("recordedBy","TDWG.name","last.name","fingerprint","identifiedBy","TDWG.name","last.name","fingerprint")
	##Saving the filtered data
		coletores[[i]] = herb.data
}
coletores = do.call(rbind.data.frame,coletores)
coletores = droplevels(coletores)
coletores$source= "splink_jabot"

## GBIF ##
#Finding the data
paths = list.files("D://Documentos//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE)
paths = paths[grep("-edited-cleaned.csv",paths)]

coletores1 = vector("list",length(paths))
#Looping for each collection
for(i in 1:length(paths)) {
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns and re-naming 
		cols = c("recordedby","coletor","coletor.name","coletor.last.name","coletor.fingerprint",
				"identifiedby","determinador","determinador.name","determinador.last.name","determinador.fingerprint")
		cls =  cols[cols %in% names(herb.data)]
		herb.data = herb.data[,cls] 
		names(herb.data) = c("recordedBy","TDWG.name","last.name","fingerprint","identifiedBy","TDWG.name","last.name","fingerprint")
	##Saving the filtered data
		coletores1[[i]] = herb.data
}
coletores1 = do.call(rbind.data.frame,coletores1)
coletores1 = droplevels(coletores1)
coletores1$source= "gbif"

#Merging the two databases
determinadores = rbind.data.frame(coletores[,c(5:9)],coletores1[,c(5:9)])
determinadores = determinadores[!duplicated(determinadores$identifiedBy),]
determinadores = determinadores[order(determinadores$TDWG.name),]

coletores = rbind.data.frame(coletores[,c(1:4,9)],coletores1[,c(1:4,9)])
coletores = coletores[!duplicated(coletores$recordedBy),]
coletores = coletores[order(coletores$TDWG.name),]

#sort(table(coletores$TDWG.name))
#length(unique(coletores$TDWG.name))
#head(sort(table(coletores$TDWG.name)),200)

## Rank of the greatest determiner - splink and jabot
paths = c(dir("D://Documentos//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE),
		dir("D://Documentos//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot",full.names=TRUE))

tmp = vector("list",length(paths))
#Looping for each collection
for(i in 1:length(paths)) {
		myfiles0 = list.files(paths[i], full.names = TRUE)
		myfiles0 = myfiles0[grep("edited-cleaned.csv",myfiles0)]
		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns and re-naming 
		cols = c("family","familia","determinador.name","determinador.fingerprint")
		cls =  cols[cols %in% names(herb.data)]
		herb.data = herb.data[,cls] 
	if(nrow(herb.data)==0) { 
		toto = rep(NA,ncol(herb.data)+1)
		names(toto) = c("fingerprint","determinador.name","family","count")
	} else {
	##Determiners per family
		toto = aggregate(herb.data[,1],list(herb.data$determinador.fingerprint,herb.data$determinador.name,herb.data[,1]),function(x)length(x))
		names(toto) = c("fingerprint","determinador.name","family","count")
	}
	##Saving the filtered data
		tmp[[i]] = toto
		#cat(i,sep="\n")
}
tmp1 = do.call(rbind.data.frame,tmp)
tmp1 = droplevels(tmp1)

#Finding the data - GBIF
paths = list.files("D://Documentos//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE)
paths = paths[grep("-edited-cleaned.csv",paths)]

tmp = vector("list",length(paths))
#Looping for each collection
for(i in 1:length(paths)) {
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns and re-naming 
		cols = c("family","determinador.name","determinador.fingerprint")
		cls =  cols[cols %in% names(herb.data)]
		herb.data = herb.data[,cls] 
	##Determiners per family
		toto = aggregate(herb.data[,1],list(herb.data$determinador.fingerprint,herb.data$determinador.name,herb.data[,1]),function(x)length(x))
		names(toto) = c("fingerprint","determinador.name","family","count")
	##Saving the filtered data
		tmp[[i]] = toto
}
tmp2 = do.call(rbind.data.frame,tmp)
tmp2 = droplevels(tmp2)
tmp3 = rbind.data.frame(tmp1,tmp2)

toto = aggregate(tmp3$count,list(tmp3$fingerprint,tmp3$determinador.name,tmp3$family),sum,na.rm=TRUE)
toto = toto[order(toto$x,decreasing=T),]
toto = toto[order(toto$Group.3),]
write.csv(toto,"determinadores.csv")

#List of families
tmp4 = sort(table(tmp3$family),decreasing=T)
write.csv(names(tmp4),"fam.csv")

############################################################################################################################################################################-
##Downloading the list of especialists from the Rede de Herbários do Brasil
#Getting the weg page content
#base_url <- "http://www.botanica.org.br/buscar_herbario_pub.php?opcoes=todos&result_por_pagi=500"
#base_html <- getURLContent(base_url)[[1]]

##Extracting the specialists names
#txt = strsplit(base_html,"\\\n")[[1]]
#txt1 = txt[grep("Especialistas:",txt)+1]
#txt1 = strsplit(txt1,"'>")
#txt1 = sapply(txt1,function(x) x[3]) 
#txt1 = gsub(' </span></td>',"",txt1)
#rhb = data.frame(especialista=txt1)
#txt1 = txt[grep("sigla_herbario",txt)]
#txt1 = strsplit(txt1,"'>")
#txt1 = sapply(txt1,function(x) x[3]) 
#txt1 = gsub('</div><div class=',"",txt1)
#txt1 = gsub("'tx_bold_herb","",txt1)
#rhb$herbario = txt1
#rhb$especialista = gsub('\r',"",rhb$especialista)
#write.csv(rhb,"rede_herbarios.csv")

#American Society of Plant Taxonomists
#aspt = read.csv("am_soc_plant_taxon.csv",as.is=T)
#tmp = as.vector(sapply(aspt$name,format.name))
#tmp = gsub(",,",",",tmp)
#aspt$tdwg = tmp
#aspt = aspt[!is.na(aspt$speciality),]
#write.csv(aspt,"am_soc_plant_taxon1.csv")

##Editing the taxonomist list
#Creating the new columns
#toto = read.csv("taxonomists.csv",as.is=T)

#toto$tdwg.name = as.vector(sapply(toto$full.name,format.name))
#toto$tdwg.name = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), toto$tdwg.name)
#toto$last.name = tolower(sapply(strsplit(toto$tdwg.name,","),function(x)x[1]))
#toto$last.name = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), toto$last.name)
#toto$finger.print = as.vector(sapply(toto$tdwg.name,fingerprint))
#toto$finger.print = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), toto$finger.print)

#Denormalizing multi-family specialists
#tmp = toto[grep(";",toto$family),]
#tmp1= toto[grep(";",toto$family,invert=TRUE),]

#tmp2= NULL
#for(i in 1:dim(tmp)[1]) {
#	linha = tmp[i,]
#	res = NULL
#	for (j in 1:str_count(linha$family, ";")) 
#		if(is.null(res))  { res = rbind.data.frame(linha,linha) } else {
#						 res = rbind.data.frame(res,linha) }
#		res$family = str_trim(strsplit(tmp[i,"family"],";")[[1]])
#	if(is.null(tmp2)) tmp2 = res else tmp2 = rbind.data.frame(tmp2,res)
#	cat(i,"\n")
#}

#toto1 = rbind.data.frame(tmp2,tmp1)
#toto1$family.obs = as.vector(sapply(strsplit(toto1$family,"\\("),function(x) x[2]))
#toto1$family.obs = gsub(" and ",", ",toto1$family.obs)
#toto1$family.obs = gsub("\\)","",toto1$family.obs)
#toto1$family1 = as.vector(sapply(strsplit(toto1$family," \\("),function(x) x[1]))
#write.csv(toto,"tmp5.csv")


##########################################################################################-
#### Finding a "barcoding gap" (minimum distance accpeted within authors fingerpints) ####
##########################################################################################-

#Condiçoes para a checagem
#1- TODOS OS AUTORES COM FINGERPRINT TEM QUE TER UM FULL NAME
#2- REMOVER '-' DA DIVISÃO DO FULL NAME
#3- PADRONIZAR O FULL.NAME1 PARA O MESMO AUTOR COM FINGERPRINTS DIFERENTES
library(qualV)
#rm(list=ls())

nomes = autores$tdwg.name
autores$last.name1 = as.vector(unlist(sapply(nomes,lastname)))
autores$fingerprint1 = as.vector(sapply(nomes,fingerprint)) 


## Reading the data
autores = read.csv("autores.csv",as.is=T,na.string=c("",NA," "))
autores1 = autores[!is.na(autores$full.name1),]
autores1 = autores1[!is.na(autores1$finger.print),]
autores1$full.name1= gsub("-"," ",autores1$full.name1)
toto = paste(autores1$finger.print,autores1$full.name1,sep="")
autores1 = autores1[!duplicated(toto),] # SOME AUTHORS WITH DUPLICATED FINGER.PRINT ARE BEING REMOVED 

##Creating the data frame with all combinations of fingerprints
tmp = data.frame(t(combn(autores1$finger.print,2)))
tmp$X1 = as.character(tmp$X1)
tmp$X2 = as.character(tmp$X2)

#calculating the number of charcaters in each fingerprint
tmp$nc1 = nchar(tmp$X1)
tmp$nc2 = nchar(tmp$X2)

#longest common substring
tmp1 = head(tmp,100000) #test object
tmp1$lcs = NA
tmp1$adist = NA

a = dim(tmp1)[1]
for (i in 1:a) {
	#x = substring(tmp1$X1[i], seq(1, tmp$nc1[i]), seq(1, tmp$nc1[i]))
	#y = substring(tmp1$X2[i], seq(1, tmp$nc2[i]), seq(1, tmp$nc2[i]))
	#tmp1$lcs[i] = paste(LCS(x,y)$LCS,collapse= "")
	tmp1$adist[i] = adist(tmp1$X1[i],tmp1$X2[i],counts=FALSE)
}

#Calculating the number of characters in common
#tmp1$nc.lcs = nchar(tmp1$lcs)
tmp1$nc.lcs = NA

#Calculating the distance between finger.prints
#tmp1$dist.lcs = 1 - apply(tmp1[,c(3,4,7)],1,function(x) x[3]/min(c(x[1],x[2])))
tmp1$dist.lcs = NA
tmp1$dist.adist = apply(tmp1[,c(3,4,6)],1,function(x) x[3]/max(c(x[1],x[2])))

#Getting authors names for each finger.print
tmp1 = merge(tmp1,autores[,c("full.name1","finger.print")],by.x="X2",by.y="finger.print",all.x=T)
tmp1 = merge(tmp1,autores[,c("full.name1","finger.print")],by.x="X1",by.y="finger.print",all.x=T)
tmp1$check = tmp1$full.name1.x == tmp1$full.name1.y

#plotting the values of distance within and across authors 
par(mfrow=c(1,2))
boxplot(tmp1$dist.lcs ~ tmp1$check,notch = TRUE)
abline(h=c(0.2,0.25,0.3),lty=2,col=2)
100*table(tmp1$check==FALSE&tmp1$dist.lcs<=0.25)/dim(tmp1)[1]
table(tmp1$check==FALSE&tmp1$dist.lcs<=0.25)

boxplot(tmp1$dist.adist ~ tmp1$check,notch = TRUE)
abline(h=c(0.4,0.45,0.5,0.55),lty=2,col=2)
100*table(tmp1$check==FALSE&tmp1$dist.adist<=0.2)/dim(tmp1)[1]
table(tmp1$check==FALSE&tmp1$dist.adist<=0.2)
#plot(tmp1$dist.lcs ~ tmp1$dist.adist)

#checking the differences between distance methods
tmp1[tmp1$dist.adist>=0.9&tmp1$dist.lcs<0.5,]
tmp1[tmp1$dist.adist>=0.6&tmp1$dist.lcs<0.1,]
#better to use adist intead of lcs

############################################################-
#### Finding missing variantes for the specialist names ####
############################################################-

#Reading the determinators variants from the herbarium labels
dets = read.csv("determinadores.csv", as.is=TRUE) 
dets1 = aggregate(dets$x, list(dets$Group.1,dets$Group.2,dets$Group.3), sum, na.rm=T)
dets1 = dets1[order(dets1$x, decreasing=TRUE),]
dets1 = dets1[!dets1$Group.2 %in% c("SemDeterminador","Disponible, N.","Sin","Flag, R.I."),]
names(dets1) = c("finger.print","tdwg.name","family","counts")

#Removing those finger.prints from authors already in the dictionary
dets1 = dets1[!as.character(dets1$tdwg.name) %in% as.character(autores1$tdwg.name),]
dets1 = dets1[dets1$counts>5,]

##Grepping the finger.prints closer to the one already in our dictionary (based on a min adist)
lista = vector("list",length(dets1$finger.print))
names(lista) = dets1$tdwg.name

for(i in 1:length(dets1$tdwg.name)) {
	det = dets1[i,]
	id = agrep(det$finger.print, autores1$finger.print, max.distance = 1)
	df = autores1[id,c("tdwg.name","finger.print"),]
	if (dim(df)[1]==0) { res = cbind.data.frame(det,data.frame(tdwg.name=NA,finger.print=NA)) } else {
		if(dim(df)[1]==1) {
			res = cbind.data.frame(det,df)
		} else {
			df$nc = nchar(df$finger.print)
			df$nc1 = nchar(det$finger.print)
			df$dist = as.vector(adist(det$finger.print,df$finger.print))
			df$st.dist = apply(df[,c(3:5)],1,function(x) x[3]/max(c(x[1],x[2])))
			#df = df[which(df$st.dist<0.25),]
			df = df[which(df$st.dist==min(df$st.dist)),]
			#colnames(df) = c("finger.print","tdwg.name","counts","tdwg.name","finger.print")
			res = cbind.data.frame(det,df)[,1:6]
		}
	}
	cat(i,"\n")
	colnames(res) = gsub("\\.1","",colnames(res)) 
	lista[[i]] = res
}
dets2 = do.call(rbind.data.frame,lista)
dets2 = droplevels(dets2)
rownames(dets2) = NULL
names(dets2)[5:6] = c("tdwg.name1","finger.print1")
toto = aggregate(dets2$counts,list(dets2$tdwg.name1),sum,na.rm=T)
names(toto) = c("tdwg.name1","total")
toto1 = left_join(dets2,toto,by="tdwg.name1")
write.csv(toto1,"autores_check.csv")
#dets2 = read.csv("autores_check.csv")


##############################################-
#### Finding closely related fingerprints ####
##############################################-
dic = cbind.data.frame(full.name1=autores1$full.name1,last.name=autores1$last.name,finger.print=autores1$finger.print, nc.fp = nchar(autores1$finger.print))
dic$nc=NA
dic$dist=NA
dic$st.dist=NA

lista1 = vector("list",length(determinadores$fingerprint))
for (i in 1:length(determinadores$fingerprint)) {
#for (i in 1:50) {
	det = determinadores[i,]
	dic$nc = nchar(det$fingerprint)
	dic$dist = as.vector(adist(det$fingerprint,dic$finger.print))
	dic$st.dist = apply(dic[,c(4,5,6)],1,function(x) x[3]/max(c(x[1],x[2])))
	dic1 = try(dic[dic$st.dist<0.25,],TRUE)
	if(dim(dic1)[1]==0) { dic1 = tail(dic,1) } else { dic1 = dic1 }
	lista1[[i]] = cbind.data.frame(det[,2:4],dic1[,1:3])
}


#################################################################################################################################################################################################-		
#################################################################################################################################################################################################-
##############################################################-
#### FINDING MISSING COORDINATES BASED ON COUNTY OR STATE ####
##############################################################-

#loading packages
#require(rgdal)
#require(raster)
#require(stringr)

#Path and lists for South American countries' data
#path0 = "E://ownCloud//W_GIS//Am_Lat_ADM_GADM_v3.6//"
#countries = list.files(path0)
#countries = countries[grep("\\.ini|\\.txt",countries,invert=TRUE)]
#countries1 = countries[grep('_1_sp.rds',countries)]
#countries2 = countries[grep('_2_sp.rds',countries)]
#paises = c("ARG","BRA","PRY","BLZ","JAM","PRI","ATG","BHS","BRB","BMU","BES","VGB","CYM","DMA","GRD","GLP","MTQ","MSR","KNA","LCA","VCT","TTO","TCA","VIR")
#paises = paste(paises,"_",sep="",collapse = "|")
#countries1 = countries1[!grepl(paste(paises,collapse="|"),countries1)]
#countries2 = countries2[grep(paises,countries2)]

### Building a gazetter for South America (state level) ###
#gazet1 = vector("list",length(countries1)) 
#for (i in 1:length(gazet1)) {
#	path = paste(path0,countries1[i],sep="")
#	tmp = readRDS(path)
#	tmp1 = cbind.data.frame(coord=coordinates(tmp),tmp@data,stringsAsFactors=FALSE)
#	tmp1 = tmp1[,c("coord.1","coord.2","NAME_0","NAME_1")]
#	tmp1$NAME_2 = NA
#	gazet1[[i]] = tmp1
#}
#gazet1 = do.call(rbind.data.frame,gazet1)
#gazet1 = droplevels(gazet1)
#names(gazet1)[1:2] = c("longitude","latitude")

##Defining a unique code for each state/province
#gazet1$loc = paste(		tolower(gazet1$NAME_0),
#					tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), gazet1$NAME_1)),sep="_")

#Building a gazetter for South America (county level)
#gazet = vector("list",length(countries2)) 
#for (i in 1:length(gazet)) {
#	path = paste(path0,countries2[i],sep="")
#	tmp = readRDS(path)
#	tmp1 = cbind.data.frame(coord=coordinates(tmp),tmp@data,stringsAsFactors=FALSE)
#	tmp1 = tmp1[,c("coord.1","coord.2","NAME_0","NAME_1","NAME_2")]
#	gazet[[i]] = tmp1
#}
#gazet = do.call(rbind.data.frame,gazet)
#gazet = droplevels(gazet)
#names(gazet)[1:2] = c("longitude","latitude")

##Definig a unique code for each county/commune
#gazet$loc = paste(	tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), gazet$NAME_0)),
#				tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), gazet$NAME_1)),
#				tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), gazet$NAME_2)), sep="_")

## Complementary database for Brazilian counties
#path = "C://Users//RenatoLima//ownCloud//W_GIS"
#municipios <- readOGR(dsn=paste(path,"//BR_municipios_IBGE",sep=""),layer="BRMUE250GC_SIR")
#municipios@data$NM_MUNICIP = as.character(iconv(municipios@data$NM_MUNICIP, from="UTF-8", to="windows-1252//TRANSLIT"))
#municipios = cbind.data.frame(coordinates(municipios),municipios@data)
#municipios$NAME_0 = "Brazil"
#municipios$NAME_1 = as.factor(substr(municipios$CD_GEOCMU,0,2))
#levels(municipios$NAME_1) = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins","Maranhão","Piauí","Ceará",
#						"Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais",
#						"Espírito Santo","Rio de Janeiro","São Paulo","Paraná","Santa Catarina","Rio Grande do Sul",
#						"Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal")
#municipios$NAME_1 = as.character(municipios$NAME_1)
#names(municipios) = c("longitude","latitude","NAME_2","CD_2","NAME_0","NAME_1")
#municipios = municipios[,c("longitude","latitude","NAME_0","NAME_1","NAME_2")]

##Defining a unique code for each county/commune
#municipios$loc = paste(	tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), municipios$NAME_0)),
#				tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), municipios$NAME_1)),
#				tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), municipios$NAME_2)), sep="_")



#Merging all gazetteers at state and county/commune levels
#names(gazet1) == names(gazet)
#gazet = rbind.data.frame(gazet1,gazet)
#names(municipios) == names(gazet)
#gazet = rbind.data.frame(gazet,municipios)
#gazet = gazet[!duplicated(gazet$loc),]

#final edits of the unique location code to symplify the coordinates checking proccess
#gazet$loc = gsub("-"," ",gazet$loc)
#gazet$loc = gsub(" - "," / ",gazet$loc)
#gazet$loc = gsub(" do | da | de "," ",gazet$loc)
#gazet$loc = gsub(" dos | das | des "," ",gazet$loc)
#gazet$loc = gsub(' d\' | d\'| d´| d\"'," ",gazet$loc)
#gazet$loc = gsub("\\.$","",gazet$loc)
#gazet = gazet[!duplicated(gazet$loc),]


#### BUILDING THE GAZETTEER AT LOCALITY AND SUBLOCALITY LEVELS ####

## Loading points
#cnc = read.csv("PontosLocalidades_CNCFlora.csv",as.is=TRUE,na.string=c("",NA," "))
#refs = read.csv("C://Users//renato//Documents//raflima\\Pos Doc\\Databases\\References\\references.csv",as.is=TRUE,na.string=c("",NA," "))

#Getting most precise coordinates
#refs$lat1 = NA; refs$long1 = NA

##getting the working coordinates for all surveys
#for (i in 1:dim(refs)[1]) {
#  if (!is.na(refs$lat_correct[i])) {
#    refs$lat1[i] = refs$lat_correct[i]; refs$long1[i] = refs$long_correct[i] 
#  } else { 
#    if (!is.na(refs$lat[i])) {
#      refs$lat1[i] = refs$lat[i]; refs$long1[i] = refs$long[i]
#    } else {
#      refs$lat1[i] = refs$lat_municipio[i]; refs$long1[i] = refs$long_municipio[i]
#    }		
#  }
#}
#refs$lat1 = as.numeric(refs$lat1); refs$long1 = as.numeric(refs$long1)
#refs$state = as.character(refs$state); refs$county = as.character(refs$county); refs$locality = as.character(refs$locality)

## Filtering only the surveys with valid coordinates, with locality names and with good precision
#treeco = refs[!is.na(refs$lat1)&!is.na(refs$long1), c("refID","status","SiteCode","country","state","county","locality","locality1","lat1","long1","confiabilidade","coord_check","ordem")]
#treeco1 = treeco[!is.na(treeco$locality)|!is.na(treeco$locality1),]
#treeco1 = treeco1[treeco1$confiabilidade %in% c("boa","precisa","exata"),]


## Getting data frame with spatial coordinates (points)
#coordinates(cnc) <- c("long", "lat")  # set spatial coordinates
#coordinates(treeco1) <- c("long1", "lat1")  # set spatial coordinates
#crs.geo <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +no_defs")  # geographical, datum WGS84
#proj4string(cnc) <- crs.geo  # define projection system of our data
#proj4string(treeco1) <- crs.geo  # define projection system of our data

## Loading shapefiles for Brazil at different levels
#path = "E://ownCloud//W_GIS"
#municipios <- readOGR(dsn=paste(path,"//BR_ADM_limits",sep=""),layer="BRA_adm2")
#municipios.gadm <- readRDS(paste(path,"//Am_Lat_ADM_GADM_v3.6//gadm36_BRA_2_sp.rds",sep=""))
#municipio.ibge = readOGR(dsn=paste(path,"//BR_municipios_IBGE",sep=""),layer="BRMUE250GC_SIR")
#distritos <- readOGR(dsn=paste(path,"//BR_ADM_limits",sep=""),layer="BRA_adm3")
#localidades  <- readOGR(dsn=paste(path,"//BR_municipios_IBGE",sep=""),layer="locali")

# Homogenizing projections
#pj = crs(municipio.ibge) 
#cnc2 = spTransform(cnc, pj)
#treeco2 = spTransform(treeco1, pj)
#municipios = spTransform(municipios, pj)
#municipios.gadm = spTransform(municipios.gadm, pj)

#convertendo shape file (UFT-8) characters para ISO-8859-1
#require(textclean)
#municipios@data$NAME_0 = as.character(iconv(municipios@data$NAME_0, from="UTF-8", to="windows-1252//TRANSLIT"))
#municipios@data$NAME_1 = as.character(iconv(municipios@data$NAME_1, from="UTF-8", to="windows-1252//TRANSLIT"))
#municipios@data$NAME_2 = as.character(iconv(municipios@data$NAME_2, from="UTF-8", to="windows-1252//TRANSLIT"))
#municipios.gadm@data$NAME_0 = as.character(iconv(municipios.gadm@data$NAME_0, from="UTF-8", to="windows-1252//TRANSLIT"))
#municipios.gadm@data$NAME_1 = as.character(iconv(municipios.gadm@data$NAME_1, from="UTF-8", to="windows-1252//TRANSLIT"))
#municipios.gadm@data$NAME_2 = as.character(iconv(municipios.gadm@data$NAME_2, from="UTF-8", to="windows-1252//TRANSLIT"))
#municipio.ibge@data$NM_MUNICIP = as.character(iconv(municipio.ibge@data$NM_MUNICIP, from="UTF-8", to="windows-1252//TRANSLIT"))
#name = municipio.ibge@data$NM_MUNICIP
#municipio.ibge@data$NM_MUNICIP = paste(toupper(substr(name, 1, 1)), tolower(substr(name, 2, nchar(name))), sep="")

## Extracting country, state and municipality 
#tmp0 = over(cnc2,municipios, fn=NULL)
#tmp1 = over(cnc2,municipios.gadm, fn=NULL)
#tmp2 = over(cnc2,municipio.ibge, fn=NULL)

## Exracting the biome per county 
# path = "E://ownCloud//W_GIS"
# municipio.ibge = readOGR(dsn=paste(path,"//BR_municipios_IBGE",sep=""),layer="BRMUE250GC_SIR")
# pj = crs(municipio.ibge) 
# lei = readOGR(dsn="E://ownCloud//W_GIS//AF_limites_milton",layer="merge_limites_MA11428_TNC_ARG_BR_PAR")
# mma = readOGR(dsn="E://ownCloud//W_GIS//BR_Biomas_MMA//Biomas_5000mil",layer="Biomas5000")
# lei = spTransform(lei, pj)
# crs(mma) = pj
# 
# toto = gArea(municipio.ibge, byid = TRUE)
# result = cbind.data.frame(NM_MUNICIP = municipio.ibge$NM_MUNICIP, CD_GEOCMU = municipio.ibge$CD_GEOCMU, cod.mun = names(toto), area.mun = toto)
# #Lei MA
# toto1 = gIntersection(municipio.ibge, lei, byid = TRUE)
# toto2 = gArea(toto1, byid = TRUE)
# result1 = cbind.data.frame(cod.mun = names(toto2), area.lei = as.double(toto2))
# result1$cod.mun = gsub(" 0","",result1$cod.mun)
# result2 = aggregate(result1$area.lei,list(result1$cod.mun), sum, na.rm = TRUE)
# result3 = merge(result, result2, by.x="cod.mun", by.y="Group.1", all.x = TRUE, sort=FALSE)
# result3$leiMA.prop = result3$x/result3$area.mun
# #Biomas MMA
# mma1 = mma[c(3,5:10),]
# toto1 = gIntersection(municipio.ibge, mma1, byid = TRUE)
# toto2 = gArea(toto1, byid = TRUE)
# nomes = names(toto2)
# cod.mun = sapply(strsplit(nomes," "), function(x) x[1])
# bioma = sapply(strsplit(nomes," "), function(x) x[2])
# bioma = as.factor(bioma)
# levels(bioma)[rownames(mma1@data) %in% levels(bioma)] = as.character(mma1@data$COD_BIOMA[rownames(mma1@data) %in% levels(bioma)])
# bioma = as.character(bioma)
# toto3 = as.data.frame.matrix(xtabs(as.double(toto2) ~ cod.mun + bioma))
# toto3 = toto3
# toto3$cod.mun = row.names(toto3)
# result4 = merge(result3, toto3, by = "cod.mun", all.x = TRUE, sort= FALSE)
# result4[,7:13] = result4[,7:13]/result4$area.mun
# result5 = read.csv("gazetteer.csv")
# result5 = merge(result5, result4, by.x = "code", by.y = "CD_GEOCMU", all.x = TRUE, sort = FALSE)
# write.csv(result5,"leiMA_biomas_por_municipio.csv")


##unindo os resultados
#cnc3 = cbind.data.frame(ordem=as.double(cnc2@data$ordem),
#                       latitude=coordinates(cnc2)[,2],longitude=coordinates(cnc2)[,1],
#                       pais0=as.character(tmp0$NAME_0),pais=as.character(tmp1$NAME_0),
#                       estado0=as.character(tmp0$NAME_1),estado=as.character(tmp1$NAME_1),
#                       municipio0=as.character(tmp0$NAME_2),
#                       municipio=as.character(tmp1$NAME_2),
#                       municipio1=as.character(tmp2$NM_MUNICIP),
#                       check0= replace_non_ascii(tolower(as.character(tmp0$NAME_2))),
#                       check1= replace_non_ascii(tolower(as.character(tmp1$NAME_2))),
#                       check2= replace_non_ascii(tolower(as.character(tmp2$NM_MUNICIP))), 
#                       stringsAsFactors=FALSE)

## Extracting country, state and municipality 
#tmp0 = over(treeco2,municipios, fn=NULL)
#tmp1 = over(treeco2,municipios.gadm, fn=NULL)
#tmp2 = over(treeco2,municipio.ibge, fn=NULL)

##unindo os resultados
#treeco3 = cbind.data.frame(ordem=as.double(treeco2@data$ordem),
#                       latitude=coordinates(treeco2)[,2],longitude=coordinates(treeco2)[,1],   
#                       pais0=as.character(tmp0$NAME_0),pais=as.character(tmp1$NAME_0),
#                       estado0=as.character(tmp0$NAME_1),estado=as.character(tmp1$NAME_1),
#                       municipio0=as.character(tmp0$NAME_2),
#                       municipio=as.character(tmp1$NAME_2),
#                       municipio1=as.character(tmp2$NM_MUNICIP),
#                       check0= replace_non_ascii(tolower(as.character(tmp0$NAME_2))),
#                       check1= replace_non_ascii(tolower(as.character(tmp1$NAME_2))),
#                       check2= replace_non_ascii(tolower(as.character(tmp2$NM_MUNICIP))), 
#                       stringsAsFactors=FALSE)

##Distritos
#distritos1 = cbind.data.frame(latitude=coordinates(distritos)[,2],longitude=coordinates(distritos)[,1],
#                             pais=as.character(distritos$NAME_0),
#                             estado=as.character(distritos$NAME_1),
#                             municipio=as.character(distritos$NAME_2),
#                             distrito=as.character(distritos$NAME_3))
#distritos1$pais = as.character(iconv(distritos1$pais, from="UTF-8", to="windows-1252//TRANSLIT"))
#distritos1$estado = as.character(iconv(distritos1$estado, from="UTF-8", to="windows-1252//TRANSLIT"))
#distritos1$municipio = as.character(iconv(distritos1$municipio, from="UTF-8", to="windows-1252//TRANSLIT"))
#distritos1$distrito = as.character(iconv(distritos1$distrito, from="UTF-8", to="windows-1252//TRANSLIT"))
#distritos2 = distritos1[!tolower(distritos1$municipio) %in% tolower(distritos1$distrito),]
#distritos2$country_code =countrycode(distritos2$pais, 'country.name', 'iso2c')

##Vilas IBGE
#crs(localidades) = pj
#tmp = over(localidades,municipios, fn=NULL)
#localidades$NM_MUNICIP = tmp$NAME_2
#vilas = cbind.data.frame(latitude=coordinates(localidades)[,2],longitude=coordinates(localidades)[,1],
#                              pais="Brazil",
#                             estado=as.character(localidades$ESTADO5),
#                             municipio=as.character(localidades$NOMEMUN1),
#                             municipio1=as.character(localidades$NM_MUNICIP),
#                             vila=as.character(localidades$NOMELOC3),
#                             tipo=as.character(localidades$TIPO2))
#vilas1 = vilas[vilas$tipo %in% "Vila",]
#vilas1$estado = as.character(iconv(vilas1$estado, from="UTF-8", to="windows-1252//TRANSLIT"))
#vilas1$municipio = as.character(iconv(vilas1$municipio, from="UTF-8", to="windows-1252//TRANSLIT"))
#vilas1$municipio1 = as.character(iconv(vilas1$municipio1, from="UTF-8", to="windows-1252//TRANSLIT"))
#vilas1$vila = as.character(iconv(vilas1$vila, from="UTF-8", to="windows-1252//TRANSLIT"))
#vilas1$country_code =countrycode(vilas1$pais, 'country.name', 'iso2c')

#GeoNames
#downloading LatinAmerican localities directly from the website
#tmp = read.csv("LatinAmericaCountries.csv")$country_code

#for(i in 1:length(tmp)) {
#  link = paste("http://download.geonames.org/export/dump/",tmp[i],".zip",sep="")
#  path = paste("E://ownCloud//W_GIS//WO_GeoNames//",tmp[i],".zip",sep="")
#  download.file(link, path)
#  unzip(path,exdir="E://ownCloud//W_GIS//WO_GeoNames")
#}

##headers of the geoname table
#header = c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature_class","feature_code","country_code","cc2",
#           "admin1_code","admin2_code","admin3_code","admin4_code","population","elevation","dem","timezone","modification_date")

##reading and binding the gazetters from all LA countries
#lista = vector("list",length(tmp))
#for(i in 1:length(tmp)) {
#  path = paste("E://ownCloud//W_GIS//WO_GeoNames//",tmp[i],".txt",sep="")
#  lista[[i]] = read.delim(path,header=FALSE)
#}
#geonames = do.call(rbind.data.frame,lista)
#geonames = droplevels(geonames)
#names(geonames) = header
#geonames$name = as.character(iconv(geonames$name, from="UTF-8", to="windows-1252//TRANSLIT"))

##remove any of the unecessary fields
#geonames1 = geonames[,c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature_class","feature_code","country_code","cc2",
#                        "admin1_code","admin2_code","admin3_code","admin4_code","population","modification_date")]
#write.csv(geonames1,"E://ownCloud//W_GIS//WO_GeoNames//LatinAmerica.csv")

##Final edits and saving the new localities
#cnc3$check = !cnc3$check0==cnc3$check2  
#cnc3 = cnc3[,c("ordem","pais","estado","municipio","municipio1","check","latitude","longitude")]
#cnc4 = merge(cnc@data,cnc3,by="ordem",all.x=TRUE)
#cnc4$country_code =countrycode(cnc4$pais, 'country.name', 'iso2c')

#treeco3$check = !treeco3$check0==treeco3$check2
#treeco3 = treeco3[,c("ordem","pais","estado","municipio","municipio1","check","latitude","longitude")]
#treeco4 = merge(treeco2@data,treeco3,by="ordem",all.x=TRUE)
#treeco4$country_code = countrycode(treeco4$pais, 'country.name', 'iso2c')

#write.csv(distritos2,"check5.csv")
#write.csv(vilas1,"check6.csv")
#write.csv(cnc4,"check7.csv")
#write.csv(treeco4,"check8.csv")

## Definig a unique code for each county, state/province or county/commune for the gazetter ##
#tmp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)
#tmp1 = tmp[tmp$loc.correct %in% "",]
#tmp1$loc.new = paste(	tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_0)),
#				tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_1)),
#				tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_2)),
#				tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_3)),sep="_")
#final edits of the unique location code to symplify the coordinates checking proccess
#tmp1$loc.new = gsub("-"," ",tmp1$loc.new)
#tmp1$loc.new = gsub(" - "," / ",tmp1$loc.new)
#tmp1$loc.new = gsub(" do | da | de "," ",tmp1$loc.new)
#tmp1$loc.new = gsub(" dos | das | des "," ",tmp1$loc.new)
#tmp1$loc.new = gsub(' d\' | d\'| d´| d\"'," ",tmp1$loc.new)
#tmp1$loc.new = gsub("\\.$","",tmp1$loc.new)
#tmp$loc.new = NA
#tmp$loc.new[tmp$loc.correct %in% ""] = str_trim(tmp1$loc.new)
#tmp1 = tmp1[!duplicated(tmp1$loc.new),]


##################################################################################################################################################################
##################################################################################################################################################################
#### 1 - Finding and editing locality data - speciesLink, speciesLink_new and JABOT #### 
#######################################################################
#SO FAR, COORDINATES AT COUNTY/CITY LEVEL (EXAUSTIVE), AND SOME MORE COMMON LOCALITIES (parks, farms, etc.) 
#LOCATITIES ADDED SO FAR ARE MAINLY FOR BRAZIL, ARGENTINA AND PARAGUAY AND COME FROM: GADM LEVEL 3, TreeCo and CNCFlora locality databases
#TO DO: ADD UCS CENTROIDES PER COUNTY.
### REVER FORMA DE REMOVER LOCALIDADES COM COORDENADAS DIFERENTES...

## Uploading the final/edited version of the gazetter, with missing counties and ortographical variants of some names (not exaustive)
dic = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)
dic = dic[dic$status %in% "ok",]
#gazet$loc[!gazet$loc %in% dic$loc]

## Creating the dictionary of state and state code names (for the Atlantic Forest countries)
tmp = dic[dic$country_code%in%c("AR","BR","PY"),]
tmp = dic[!is.na(dic$state_code),]
states = aggregate(tmp$NAME_1,list(tmp$NAME_0,tmp$state_code),function(x) paste(unique(as.character(x)),collapse="|"))
names(states) = c("NAME_0","state_code","NAME_1")
states = states[order(states$state_code),]
states = states[order(states$NAME_0),]
#dictionay for each country
arg.states = states[states$NAME_0%in%"Argentina",]$NAME_1
names(arg.states) = c("buenos aires","catamarca","cordoba","chaco","corrientes","chubut","entre rios","formosa",
                      "jujuy","la pampa","la rioja","misiones","mendoza","neuquen","rio negro","salta","santa cruz",
                      "santiago del estero","santa fe","san juan","san luis","tierra del fuego","tucuman")
bra.states = paste(states$NAME_1[states$NAME_0%in%"Brazil"],"|, ",states$state_code[states$NAME_0%in%"Brazil"],"\\.",sep="")
names(bra.states) = c("acre","alagoas","amazonas","amapa","bahia","ceara","distrito federal",   
                      "espirito santo","goias","maranhao","minas gerais","mato grosso sul", 
                      "mato grosso","para","paraiba","pernambuco","piaui","parana","rio janeiro",
                      "rio grande norte","rondonia","roraima","rio grande sul","santa catarina","sergipe",
                      "sao paulo","tocantins")
par.states = states[states$NAME_0%in%"Paraguay",]$NAME_1
names(par.states) = c("amambay","alto parana","alto paraguay","asuncion","boqueron","caazapa","canindeyu",
                      "caaguazu","central","cordillera","concepcion","guaira","itapua","misiones","neembucu",
                      "paraguari","presidente hayes","san pedro")

#Including some other state variants
bra.states = gsub("Amapá","Amapá|Amapa",bra.states)
bra.states = gsub("Ceará","Ceará|Ceara",bra.states)
bra.states = gsub("Espírito Santo","Espírito Santo|Espirito Santo",bra.states)
bra.states = gsub("Goiás","Goiás|Goias",bra.states)
bra.states = gsub("Maranhão","Maranhão|Maranhao",bra.states)
bra.states = gsub("Mato Grosso","Mato Grosso|Matto Grosso",bra.states)
bra.states = gsub("Minas Gerais","Minas Gerais|Minas Geraes",bra.states)
bra.states = gsub("Piauí","Piauí|Piaui|Piauy|Piauhy",bra.states)
bra.states = gsub("Rondônia","Rondônia|Rondónia|Rondonia",bra.states)
bra.states = gsub("São Paulo","São Paulo|Sao Paulo",bra.states)

## Finding the occurrence data
paths = c(dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE),
          dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new",full.names=TRUE),
          dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot",full.names=TRUE),
          dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//sndb_mincyt_arg",full.names=TRUE))

## Looping for each collection
for(i in 1:length(paths)) {
		myfiles0 = list.files(paths[i], full.names = TRUE)
		myfiles0 = myfiles0[grep("edited-cleaned.csv",myfiles0)]
		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns and re-naming 
		cols = c("decimalLatitude","decimalLongitude","latitude","longitude","decimallatitude","decimallongitude",  
		         "country","pais","countrycode", #"countryCode",
		         "stateProvince","stateprovince","estado_prov",
		         "municipality","county","cidade",
		         "locality", "descrlocal","locality")
		cls = unique(cols[cols %in% names(herb.data)])
		herb.data1 = herb.data[,cls]
	##Removing duplicated county columns from SNDB	
		if (sum(names(herb.data1) %in% c("county","municipality"))==2) {
		  herb.data1$county[is.na(herb.data1$county) & !is.na(herb.data1$municipality)] = herb.data1$municipality[is.na(herb.data1$county) & !is.na(herb.data1$municipality)]
		  herb.data1$municipality[!is.na(herb.data1$county) & is.na(herb.data1$municipality)] = herb.data1$county[!is.na(herb.data1$county) & is.na(herb.data1$municipality)]
		  herb.data1 = herb.data1[,-which(names(herb.data1)=="county")]
		} 
	##Extracting the locality fields	  
		n0 = herb.data1[,3] #country 
		n1 = herb.data1[,4] #state, province, etc.
		n2 = herb.data1[,5] #county, departamiento, commune, etc.
		n3 = herb.data1[,6] #locality: park, farm, etc.
	 # Nivel0: Country #
		n0[nchar(n0)==2 & !is.na(n0)] = countrycode(as.character(n0[nchar(n0)==2 & !is.na(n0)]), 'iso2c', 'country.name')
		n0 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n0))
		n0 = gsub("\\?|s\\/localidade|inderminada|inderminado|^s\\.d\\.|^desconhecido|^ignorado|^sem informacao|n\\.i\\.",NA,n0)
		n0 = gsub("nao especificado|^não informado",NA,n0)
		n0[n0 %in% c("")] = NA
		n0 = gsub("a¡","a",n0)
		n0 = gsub("a©|aª","e",n0)
		n0 = gsub("aº","u",n0)
		n0 = gsub("a§","c",n0)
		n0 = gsub("a´","o",n0)
		n0 = gsub("a¢|a¡|a£","a",n0)
		n0 = gsub("^bolavia","bolivia",n0)
		#n0 = gsub("^bola?via?","bolivia",n0)
		#n0 = gsub("^suaca?","suica",n0)
		n0 = gsub("^paraguai","paraguay",n0)
		n0 = gsub("^brasil","brazil",n0)
		n0 = gsub("^equador","ecuador",n0)
		n0 = gsub("^uruguai","uruguay",n0)
		n0 = gsub("^guiana inglesa","guyana",n0)
		n0 = gsub("^guiana","guyana",n0)
		n0 = gsub("^estados unidos","usa",n0)
		n0 = gsub("^portugual","portugal",n0)
		n0 = gsub("^franca","france",n0)
		n0 = gsub("^alemanha","germany",n0)
		n0 = gsub("^guiana francesa","french guiana",n0)
		n0 = gsub("^franca","france",n0)
		n0 = gsub("^japao","japan",n0)
		n0 = gsub("^republica dominicana","dominican republic",n0)
		n0 = gsub("^peru oriental","peru",n0)
		n0 = gsub("^inglaterra","england",n0)
		n0 = str_trim(n0)
		#sort(unique(n0))	
		#Missing country for non missing states and counties (only for uniquivocal Brazilian states)
		n0[is.na(n0) & !is.na(n2) & n1 %in% 
			c("Acre","Alagoas","Amapá","Bahia","Ceará","Espírito Santo",
			"Goiás","Maranhão","Minas Gerais","Mato Grosso do Sul","Pará","Paraíba",
			"Pernambuco","Piauí","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul",
               "Rondônia","Roraima","Santa Catarina","Sergipe","São Paulo","Tocantins"		
			)] = "brazil" 
	 # Nivel1: States, Provinces #
		n1[n0%in%"brazil" & n1%in%"AC"] = "Acre"
		n1[n0%in%"brazil" & n1%in%"AL"] = "Alagoas"
 		n1[n0%in%"brazil" & n1%in%c("AM","Amozonas","Territorio Federal Amazonas")] = "Amazonas"
		n1[n0%in%"brazil" & n1%in%c("AP","Território do Amapá")] = "Amapá"
		n1[n0%in%"brazil" & n1%in%"BA"] = "Bahia"
		n1[n0%in%"brazil" & n1%in%c("DF","Brasília","Brasília (DF)","Distrito Fedreal","Distrito Fedral")] = "Distrito Federal"
		n1[n0%in%"brazil" & n1%in%c("CE","Ceára")] = "Ceará"
		n1[n0%in%"brazil" & n1%in%c("ES")] = "Espírito Santo"
		n1[n0%in%"brazil" & n1%in%c("EspÃ\u00ADrito Santo")] = "Espírito Santo"
		n1[n0%in%"brazil" & n1%in%c("GO","Goais","Goiais")] = "Goiás"
		n1[n0%in%"brazil" & n1%in%c("MA")] = "Maranhão"
		n1[n0%in%"brazil" & n1%in%c("MG")] = "Minas Gerais"
		n1[n0%in%"brazil" & n1%in%c("MS","MS.")] = "Mato Grosso do Sul"
		n1[n0%in%"brazil" & n1%in%c("MT")] = "Mato Grosso"
		n1[n0%in%"brazil" & n1%in%c("PA")] = "Pará"
		n1[n0%in%"brazil" & n1%in%c("PI","Paiuí")] = "Piauí"
		n1[n0%in%"brazil" & n1%in%c("PaiuÃ\u00ADba")] = "Piauí"
		n1[n0%in%"brazil" & n1%in%c("PE","Permambuco","Pernammbuco","Pernanbuco","Pernmabuco","Prenambuco")] = "Pernambuco"
		n1[n0%in%"brazil" & n1%in%c("PB")] = "Paraíba"
		n1[n0%in%"brazil" & n1%in%c("ParaÃ\u00ADba")] = "Paraíba"
		n1[n0%in%"brazil" & n1%in%c("PR")] = "Paraná"
		n1[n0%in%"brazil" & n1%in%c("RJ","Ri de Janeiro","Rio de Janiero","Rio de Janejro","Rio de Janiro","Rio de Jnaeiro")] = "Rio de Janeiro"
		n1[n0%in%"brazil" & n1%in%c("RN","Rio Gande do Norte","Rio Grade do Norte")] = "Rio Grande do Norte"
		n1[n0%in%"brazil" & n1%in%c("RS","Rio Grando do Sul")] = "Rio Grande do Sul"
		n1[n0%in%"brazil" & n1%in%c("RO","Rodonia")] = "Rondonia"
		n1[n0%in%"brazil" & n1%in%c("RR","Roraima T.F.")] = "Roraima"
		n1[n0%in%"brazil" & n1%in%c("SC","Santa Caratina","Santa Catarinaa","Santa Cataria")] = "Santa Catarina"
		n1[n0%in%"brazil" & n1%in%c("SE","Segipe","Serjipe")] = "Sergipe"
		n1[n0%in%"brazil" & n1%in%c("SP")] = "São Paulo"
		n1[n0%in%"brazil" & n1%in%c("TO","Tocantis","Tocatins")] = "Tocantins"

		#find missing states for states that may create problems by using grep (e.g. Mato Grosso do Sul and Mato Grosso)
		n1[is.na(n1) & n0%in%"brazil" & grepl("Mato Grosso do Sul|Mato Grosso Sul|, MS\\.",n3) & !grepl("Divisa ",n3, ignore.case = TRUE)] = "Mato Grosso do Sul"
		n1[is.na(n1) & n0%in%"brazil" & grepl("Acre|, AC\\.",n3) & 
		     !grepl("Rio Acre|Novo Acre|Boca do Acre|Brasilia-Acre|Brasilica-Acre|Mato Grosso-Acre|Brasilia Acre Highway|Divisa ",n3, ignore.case = TRUE)] = "Acre"
		n1[is.na(n1) & n0%in%"brazil" & grepl("Amazonas|, AM\\.",n3) & 
		     !grepl("Rio Amazonas|Alto Amazonas|Rondonia|Rondônia|Roraima|Planta do Amazonas|Maria do Amazonas|Divisa ",n3, ignore.case = TRUE)] = "Amazonas"
		n1[is.na(n1) & n0%in%"brazil" & grepl("Bahia|, BA\\.",n3) & !grepl("Igarape Bahia|Victoria and Bahia|Bahia and Pernambuco|Coco-da Bahia|Bahia - Espirito Santo|Divisa ",n3, ignore.case = TRUE)] = "Bahia"
		n1[is.na(n1) & n0%in%"brazil" & grepl("Paraíba|Paraiba|, PB\\.",n3) & 
		     !grepl("fluv. Paraiba|Rio Paraiba|Río Paraiba|Rio Paraíba|Paraiba do Sul|Paraíba do Sul|Paraiba da Sul|Paraiba River|Além Paraiba|Além-Paraiba|Alem Paraiba|Alem-Paraiba|Divisa ",n3, ignore.case = TRUE)] = "Paraíba"
		n1[is.na(n1) & n0%in%"brazil" & grepl("Paraná|, PR\\.|, Parana\\.",n3) & 
		     !grepl("Rio Paraná|Mirití-Paraná|Alto Paraná|del Paraná|Ati Paraná|Autai Paraná|Mineruá|Mamupina|Juamí-Japurá|Anacho|Tefé|Xiborena|Paraná d|Divisa ",n3, ignore.case = TRUE)] = "Paraná"
		#some prep edits
		n3 = gsub("Brasilia DF,|Brasília DF,|Brasília DF\\.","Brasilia, DF.",n3) 
		n3 = gsub("Brasilia DF","Brasilia, DF",n3)
		#some list of expressions to avoid mismatches between states
		avoid = c("Alto Amazonas|Divisa |Brasilia-Acre|Mato Grosso-Acre|Estados Pará, Goiás, Mato Grosso|Rio Maranhão|to Maranhão km|Igarapé Espírito Santo|Fazenda Espírito Santo|Fazenda São Paulo|Distrito Federal or Goiás|Distrito Federal ou Goiás|Sao Paulo\\/Minas Gerais|Goias\\/Minas Gerais|Rio de Janeiro and São Paulo| of Rondônia state line| of Rondonia state line|Rondonia-Amazonas|Amazonas e Roraima|São Paulo de Oliven|Sao Paulo de Oliven|Sao Paulo d'Olivenc|Sao Paulode Olivenca|Linha Sao Paulo|Linha São Paulo|Universidade de São Paulo|Rio Tocantin")
		#Looping for missing state information in the 3 Atlantic Forest countries (All other states)
		bra.states1 = bra.states[!grepl("Mato Grosso do Sul|Acre|Amazonas|Bahia|Paraíba|Paraná",bra.states)]
		if (any(n0%in%"brazil")) for (j in 1:length(bra.states1)) { n1[is.na(n1) & n0%in%"brazil" & grepl(bra.states1[j],n3, ignore.case = TRUE) & !grepl(avoid,n3, ignore.case = TRUE)] = names(bra.states1[j]) }
		if (any(n0%in%"argentina")) for (j in 1:length(arg.states)) { n1[is.na(n1) & n0%in%"argentina" & grepl(arg.states[j],n3)] = names(arg.states[j]) }
		if (any(n0%in%"paraguay")) for (j in 1:length(par.states)) { n1[is.na(n1) & n0%in%"paraguay" & grepl(par.states[j],n3)] = names(par.states[j]) }

		#Further editing and standardization		
		n1 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n1))
		n1 = gsub("\\?|s\\/localidade|inderminada|inderminado|^s\\.d\\.|^desconhecido|^ignorado|^sem informacao|n\\.i\\.|estado nao informado!|Sin loc\\.|Sin\\. loc\\.",NA,n1)
		n1 = gsub("nao especificado|^não informado",NA,n1)
		n1[n1 %in% c("")] = NA
		n1 = gsub("a¡","a",n1)
		n1 = gsub("a©|aª","e",n1)
		n1 = gsub("aº","u",n1)
		n1 = gsub("a§","c",n1)
		n1 = gsub("a´","o",n1)
		n1 = gsub("a¢|a¡|a£","a",n1)
		#n1 = gsub("\u00AD","",n1) # removing soft hyphen (hidden as a breaking space)
		#n1 = gsub("\u00AO","",n1) # removing hidden breaking space
		n1 = gsub("^prov\\. |^dep\\. ","",n1)
		n1 = gsub("^depto\\. |^prov\\.|^mun\\. ","",n1)
		n1 = gsub("^dept\\.|^dpto\\.|^depto\\.|^dept.","",n1)
		n1 = gsub("^departamento |^departamento de ","",n1)
		n1 = gsub("^provincia de |^província de ","",n1)
		n1 = gsub("^segipe","sergipe",n1)
		n1 = gsub("^mato gosso","mato grosso",n1)
		n1 = gsub("^santa cataria","santa catarina",n1)
		n1 = gsub("^rondonica","rondonia",n1)
		n1 = gsub("^minas gerias|^mnas gerais","minas gerais",n1)
		n1 = gsub("^canendeyu","canindeyu",n1)
		n1 = gsub("^espiritu santo|^espirito sanrto|^espirito santos|^espirito sato|^espitiro santo|^esparito santo","espirito santo",n1)
		n1 = gsub("^missiones","misiones",n1)
		n1 = gsub("^Alagaoas","Alagoas",n1)
		n1 = gsub("\\.$","",n1)
		n1 = str_trim(n1)
		#sort(unique(n1))
		
	 # Nivel2: County, Departament, Commune #
		#n2 = gsub("Ã\u00AD","",n2) # removing soft hyphen (hidden as a breaking space)
		#n2 = gsub("Ã\u00AO","",n2) # removing hidden breaking space
		n2 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n2))
		n2 = gsub("\\?|s\\/localidade|inderminada|inderminado|^s\\.d\\.|^desconhecido|^ignorado|bloqueado|^sem informacao|n\\.i\\.|Sin loc\\.|Sin\\. loc\\.",NA,n2)
		n2 = gsub("nao especificado|^não informado",NA,n2)
		n2[!is.na(n2)&n2 %in% c("")] = NA
		n2 = gsub("a¡","a",n2)
		n2 = gsub("a©|ã©|aª","e",n2)
		n2 = gsub("aº","u",n2)
		n2 = gsub("a§","c",n2)
		n2 = gsub("a´","o",n2)
		n2 = gsub("a¢|a¡|a£","a",n2)
		n2 = gsub("^dep\\. |^depto\\. |^prov\\.|^mun\\. ","",n2)
		n2 = gsub("^dept\\.|^dpto\\.|^depto\\.|^dept.","",n2)
		n2 = gsub("^municipio de","",n2)
		n2 = gsub("^municipio ","",n2)
		n2 = gsub("^gral\\.","general",n2)
		n2 = gsub("^rod\\. ","rodovia",n2)
		n2 = gsub("sma\\.","santissima",n2)
		n2 = gsub("st\\. antonio","santo antonio",n2)
		n2 = gsub("s\\.jose","sao jose",n2)
		n2 = gsub("sta\\. elena","santa elena",n2)
		n2 = gsub("sta\\. rita","santa rita",n2)
		n2 = gsub("s\\. jose","sao jose",n2)
		n2 = gsub("s\\. joao","sao joao",n2)
		n2 = gsub("s\\. bento|s\\.bento","sao bento",n2)
		n2 = gsub("s\\. luis","sao luis",n2)
		n2 = gsub("s\\. miguel","sao miguel",n2)
		n2 = gsub("s\\. f\\. paula","sao francisco paula",n2)
		n2 = gsub("nª sª|nossa sra\\.","nossa senhora",n2)
		n2 = gsub(" s\\. luiz"," sao luiz",n2)
		n2 = gsub(" s\\. mateus"," sao mateus",n2)
		n2 = gsub(" s\\. francisco"," sao francisco",n2)
		n2 = gsub(" dr\\. "," doutor ",n2)
		n2 = gsub("sta\\. maria","santa maria",n2)
		n2 = gsub('\\.$','',n2)
		n2[!is.na(n2)&!is.na(n2)&(grepl("\\.",n2)|grepl(",",n2)|grepl(":",n2))] = 
		  as.character(sapply(strsplit(n2[!is.na(n2)&!is.na(n2)&(grepl("\\.",n2)|grepl(",",n2)|grepl(":",n2))],",|:"), function(x) x[1]))
		n2 = str_trim(n2)
		#sort(unique(n2))	
	# Nivel 3: locality: park, farm, etc.
		n3 = gsub("Ã\u00AD","í",n3)		
		n3 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n3))
		n3 = gsub("\\?|s\\/localidade|inderminada|inderminado|^s\\.d\\.|^desconhecido|^ignorado|bloqueado|^sem informacao|n\\.i\\.|Sin loc\\.|Sin\\. loc\\.",NA,n3)
		n3 = gsub("nao especificado|^não informado|^no specific\\.$|^no specific$",NA,n3)
		n3[!is.na(n3)&n3 %in% c("")] = NA
		n3 = gsub("a¡","a",n3)
		n3 = gsub("a©|aª","e",n3)
		n3 = gsub("aº","u",n3)
		n3 = gsub("a§","c",n3)
		n3 = gsub("a´|a³","o",n3)
		n3 = gsub("a¢|a¡|a£|a'","a",n3)
		n3 = gsub("a\u0081","a",n3)
		n3 = gsub("  "," ",n3)
		n3 = str_trim(n3)
		n3 = gsub('\\.$|,$|:$','',n3)
		n3 = gsub("^mun\\. |^mun\\.: |^mpio\\. |^mpio: |^mpio\\.: ","municipio de ",n3)
		n3 = gsub(" mun\\. | mun\\.: | mpio\\. | mpio: | mpio\\.: "," municipio de ",n3)
		n3 = gsub("\\(mun\\. ","(municipio de ",n3)
		n3 = gsub("^mun\\.|^mun\\.:|^mpio |^mpio\\.|^mpio:|^mpio\\.:","municipio",n3)
		n3 = gsub("mun\\. |mpio |mpio\\. ","municipio de ",n3)
		n3 = gsub("mun\\.:|mpio:|mpio\\.:","municipio de",n3)
		n3 = gsub("municipio de :","municipio de",n3)	
		n3 = gsub("municipio de de ","municipio de ",n3)
		n3 = gsub("municipio de do ","municipio do ",n3)
		n3 = gsub("municipio de sto\\.","municipio de santo",n3)
		n3 = gsub("municipio de sta\\.","municipio de santa",n3)
		n3 = gsub("municipio de :","municipio de ",n3)	
		n3 = gsub("municipio de de ","municipio de ",n3)
		n3 = gsub("municipio de do ","municipio do ",n3)
		n3 = gsub("municipio de of ","municipio of ",n3)
    		n3 = gsub("^prov\\. ","provincia de ",n3)
		n3 = gsub(" prov\\. "," provincia de ",n3)
		n3 = gsub("^dep\\. |^dept\\. |^dpto\\. |dtto\\. |^depto\\. ","departamento de ",n3)
		n3 = gsub(" dep\\. | dept\\. | dpto\\. "," departamento de ",n3)
		n3 = gsub("provincia de :","provincia de ",n3)	
		n3 = gsub("provincia de de ","provincia de ",n3)
		n3 = gsub("provincia de do ","provincia do ",n3)
		n3 = gsub("provincia de of ","provincia of ",n3)
		n3 = gsub("departamento de :","departamento de ",n3)	
		n3 = gsub("departamento de de ","departamento de ",n3)
		n3 = gsub("departamento de do ","departamento do ",n3)
		n3 = gsub("departamento de of ","departamento of ",n3)
		#n3 = gsub("^dist\\.","distrito",n3)
		n3 = gsub("^gral\\.","general",n3)
		n3 = gsub("^rod\\. ","rodovia",n3)
		n3 = gsub("^est\\. ","estado ",n3)
		n3 = gsub("^dr\\. ","doutor ",n3)
		n3 = gsub("sma\\.","santissima",n3)
		n3 = gsub("st\\. antonio","santo antonio",n3)
		n3 = gsub("sta\\. elena","santa elena",n3)
		n3 = gsub("sta\\. rita","santa rita",n3)
		n3 = gsub("s\\.jose","sao jose",n3)
		n3 = gsub("s\\. jose","sao jose",n3)
		n3 = gsub("s\\. joao","sao joao",n3)
		n3 = gsub("s\\. bento|s\\.bento","sao bento",n3)
		n3 = gsub("s\\. luis","sao luis",n3)
		n3 = gsub("s\\. miguel","sao miguel",n3)
		n3 = gsub("s\\. f\\. paula","sao francisco paula",n3)
		n3 = gsub("nª sª|nossa sra\\.","nossa senhora",n3)
		n3 = gsub(" s\\. luiz"," sao luiz",n3)
		n3 = gsub(" s\\. mateus"," sao mateus",n3)
		n3 = gsub(" s\\. francisco"," sao francisco",n3)
		n3 = gsub(" dr\\. "," doutor ",n3)
		n3 = gsub(" do sr\\. "," do senhor ",n3)
		n3 = gsub("faz\\. ","fazenda ",n3)
		n3 = gsub("estr\\. ","estrada ",n3)
		n3 = gsub("sta\\. maria","santa maria",n3)
		n3 = gsub("p\\.e\\.serra do mar,|p\\.e\\.serra do mar\\.","parque estadual serra mar",n3)
		n3 = gsub("p\\.e\\.serra do mar","parque estadual serra mar",n3)
		n3 = gsub("parque estadual da serra do mar,|parque estadual da serra do mar\\.","parque estadual serra mar",n3)
		n3 = gsub("serra do mar, nucleo |serra do mar \\(nucleo","serra do mar nucleo",n3) 
		n3 = gsub("e\\.f\\.b\\. |e\\. f\\. b\\. ","estacao de ferro de braganca",n3)
	     n3 = gsub("r\\.f\\.ducke","reserva florestal ducke",n3)
    		n3 = gsub("r\\.f\\.bananal","reserva florestal bananal",n3)
		n3 = gsub("r\\.f\\. ","reserva florestal",n3)
		n3 = gsub("parque est\\.","parque estadual",n3)
		n3 = gsub("p\\. est\\.|p\\. \\e.|p\\.\\e.","parque estadual",n3)
		n3 = gsub("parque nac\\.","parque nacional",n3)		
		n3 = gsub("p\\. nac\\.|p\\. \\n.|p\\.\\n.","parque nacional",n3)
		n3 = gsub("est\\. ecologica|est\\. ecol\\.|estacao ecol\\.","estacao ecologica",n3)
		n3 = gsub("e\\. ecologica|e\\.ecologica|e\\.ecol\\.|e\\. ecol\\.|e\\.ecologica|e\\.ec\\.","estacao ecologica",n3)
		n3 = gsub("est\\. experimental|est\\. exp\\.|estacao exp\\.","estacao experimental",n3)
		n3 = gsub("e\\. experimental|e\\.experimental|e\\.exp\\.|e\\. exp\\.","estacao experimental",n3)
		n3 = gsub("e\\.b\\. |e\\. b\\. |e\\. biol\\. |est\\. biol\\. |est\\. biologica |e\\. biologica ","estacao biologica ",n3)
		n3 = gsub("e\\.e\\.|e\\. e\\.|e\\. biol\\. |est\\. biol\\. |est\\. biologica |e\\. biologica ","estacao biologica ",n3)
		n3 = gsub("^ca\\. ","circa ",n3)
		n3 = gsub(", ca\\. ",", circa ",n3)
		n3 = gsub('c\\. 1','circa 1',n3); n3 = gsub('c\\. 2','circa 2',n3); n3 = gsub('c\\. 3','circa 3',n3) 
		n3 = gsub('c\\. 4','circa 4',n3); n3 = gsub('c\\. 5','circa 5',n3); n3 = gsub('c\\. 6','circa 6',n3) 
		n3 = gsub('c\\. 7','circa 7',n3); n3 = gsub('c\\. 8','circa 8',n3); n3 = gsub('c\\. 9','circa 9',n3) 
		n3 = gsub("^s\\. of","south of",n3)
		n3 = gsub(" s\\. of"," south of",n3)
		n3 = gsub("^n\\. of","north of",n3)
		n3 = gsub(" n\\. of"," north of",n3)
		n3 = gsub("^e\\. of","east of",n3)
		n3 = gsub(" e\\. of"," east of",n3)
		n3 = gsub("^w\\. of","west of",n3)
		n3 = gsub(" w\\. of"," west of",n3)
		n3 = gsub("^s\\.e\\. of","southeast of",n3)
		n3 = gsub(" s\\.e\\. of"," southeast of",n3)
		n3 = gsub("^n\\.e\\. of","northeast of",n3)
		n3 = gsub(" n\\.e\\. of"," northeast of",n3)
		n3 = gsub("^s\\.w\\. of","southwest of",n3)
		n3 = gsub(" s\\.w\\. of"," soutwest of",n3)
		n3 = gsub("^n\\.w\\. of","northwest of",n3)
		n3 = gsub(" n\\.w\\. of"," northwest of",n3)
		n3 = gsub("^s\\.t\\. ","saint ",n3)
		n3 = gsub("^st\\. ","saint ",n3)
		n3 = gsub(", s\\.t\\. ",", saint ",n3)
		n3 = gsub("u\\.s\\. virgin islands","us virgin islands",n3)
		n3 = gsub("prop\\. sr\\.","propriedade senhor",n3)
		n3 = gsub("p\\. n\\. d","parque nacional d",n3)
		n3 = gsub("p\\.n\\. d","parque nacional d",n3)
		n3 = gsub("^p\\.n\\. ","parque nacional ",n3)
		n3 = gsub("^p\\. n\\. ","parque nacional ",n3)
		n3 = gsub("^p\\.n\\.n\\. ","parque nacional natural ",n3)
		n3 = gsub("p\\. e\\. d","parque estadual d",n3)
		n3 = gsub("p\\.e\\. d","parque estadual d",n3)
		n3 = gsub("^p\\.e\\.","parque estadual",n3)
		n3 = gsub("p\\. i\\. ","posto indigena",n3)
		n3 = gsub("p\\.i\\. ","posto indigena",n3)
		n3 = gsub("i\\.n\\.p\\.","instituto nacional pinho",n3)
		n3 = gsub("i\\. n\\. p\\.","instituto nacional pinho",n3)
		n3 = gsub("distr\\. ","distrito ",n3)
		n3 = gsub("^dist\\. ","distrito ",n3)
		n3 = gsub("vic\\. of","vicinity of",n3)
		n3 = gsub("vic\\. ","vicinity ",n3)
		n3 = gsub("r\\. jari","rio jari",n3)
		n3 = gsub("r\\. xingu","rio xingu",n3)
		n3 = gsub("r\\. juruena","rio juruena",n3)
		n3 = gsub("r\\. uaupes","rio uaupes",n3)
		n3 = gsub("r\\. negro","rio negro",n3)
		n3 = gsub("r\\. trombetas","rio trombetas",n3)
		n3 = gsub("r\\. tapajoz|r\\. tapajos","rio tapajos",n3)
		n3 = gsub("\\(esec\\) ","",n3)
		n3 = gsub("\\(flona\\) ","",n3)
		n3 = gsub("\\(horto florestal\\) ","horto florestal",n3)
		n3 = gsub("^s\\. ","s ",n3)
		n3 = gsub("^r\\. ","r ",n3)
		n3 = gsub("^prox\\. ","proximo ",n3)
		n3 = gsub("^prox\\. ","proximo ",n3)
		n3 = gsub("^p\\.n\\.i\\.","pni",n3)
		n3 = gsub("^p\\.n\\.s\\.o\\.","pnso",n3)
		n3 = gsub("^p\\. ","p ",n3)
		n3 = gsub("^loc\\. |^loc: ","loc ",n3)
    #spliting the locality vector
		n4 = strsplit(n3,",|\\.|:|\\(|)|;")
		n4 = sapply(n4,str_trim)
	 # trying to get missing counties from the locality description (e.g. "Pico das Almas, municipio de Rio de Contas")
		n4.2 = as.character(sapply(n4, function(x) paste(unique(str_trim(x[grepl("municipio|municipality|county|provincia|village",x)])),collapse = "|")))
		n4.2 = str_trim(n4.2)
		n4.2 = gsub("municipio de |municipality of |municipio do |^county of |^provincia de|^provincia of|village of","",n4.2)
		n4.2 = gsub("municipio |municipality |municipio |^county |^provincia |village ","",n4.2)
		n4.2[n4.2%in%""] = NA
		# getting missing counties that may be the first part of the locality description (e.g. "Rio de Contas, Pico das Almas")
    n4.2.1 = as.character(sapply(n4, function(x) x[1]))
		n4.2.1 = str_trim(n4.2.1)
		n4.2.1[n4.2.1%in%""] = NA
		# isolating localities possibily in the gazetter (e.g. parks, serras, farms)
		locais = "parque|reserva|reserve|fazenda|nacional|estadual|parna|flona|rebio|rppn|e\\.e\\.|biologica|ecologica|extrativista|park|farm|estrada|rodovia|road|^sitio|^mata|^horto|^jardim|campus|^pico|^serra|^sierra|^morro|^chapada"
		n4.3 = as.character(sapply(n4, function(x) paste(unique(str_trim(x[grepl(locais,x)])),collapse = ", ")))
		n4.3 = str_trim(n4.3)
		n4.3[n4.3%in%""] = NA
		#other localities, than the ones in 'locais' and counties (first sentence, prior to the first comma when n4.3 is empty)
		n4.3[is.na(n4.3)&!is.na(n3)] = as.character(sapply(n4[is.na(n4.3)&!is.na(n3)],function(x) head(x[!grepl("municipio|municipality|county|provincia|village",x)],1)))
  ## Replacing missing counties and localities
		n2[is.na(n2)&!is.na(n4.2)] = str_trim(n4.2[is.na(n2)&!is.na(n4.2)])
		n3[!is.na(n4.3)] = str_trim(n4.3[!is.na(n4.3)])
		n3[is.na(n4.3)] = str_trim(as.character(sapply(n4[is.na(n4.3)],function(x) x[1])))
		
	## Definig a unique code for each county, state/province or county/commune ##
		loc = rep(NA,dim(herb.data1)[1])
		#county-level
		loc[!is.na(n0)&!is.na(n1)&!is.na(n2)] = 
		  paste(n0[!is.na(n0)&!is.na(n1)&!is.na(n2)],n1[!is.na(n0)&!is.na(n1)&!is.na(n2)],n2[!is.na(n0)&!is.na(n1)&!is.na(n2)],sep="_")   
		#state-level
		loc[is.na(loc)&!is.na(n0)&!is.na(n1)] = 
		  paste(n0[is.na(loc)&!is.na(n0)&!is.na(n1)],n1[is.na(loc)&!is.na(n0)&!is.na(n1)],sep="_")
		#country-level
		loc[is.na(loc)&!is.na(n0)] = n0[is.na(loc)&!is.na(n0)]
 	## Definig a unique code for each locality (if available)
		loc1 = rep(NA,dim(herb.data1)[1])
		#locality-level
		loc1[!is.na(n0)&!is.na(n1)&!is.na(n2)&!is.na(n2)&!is.na(n3)] = 
		  paste(n0[!is.na(n0)&!is.na(n1)&!is.na(n2)&!is.na(n3)],n1[!is.na(n0)&!is.na(n1)&!is.na(n2)&!is.na(n3)],n2[!is.na(n0)&!is.na(n1)&!is.na(n2)&!is.na(n3)],n3[!is.na(n0)&!is.na(n1)&!is.na(n2)&!is.na(n3)],sep="_")   
	## Definig a unique code for an alternative way of getting missing countis from the locality field
		loc2 = rep(NA,dim(herb.data1)[1])
		#county-level when county is not given
		loc2[!is.na(n0)&!is.na(n1)&is.na(n2)&!is.na(n4.2.1)] = 
		  paste(n0[!is.na(n0)&!is.na(n1)&is.na(n2)&!is.na(n4.2.1)], n1[!is.na(n0)&!is.na(n1)&is.na(n2)&!is.na(n4.2.1)], n4.2.1[!is.na(n0)&!is.na(n1)&is.na(n2)&!is.na(n4.2.1)],sep="_")   
		
	# Final edits to simplify the coordinate cross checking process
		loc = gsub("-"," ",loc); loc1 = gsub("-"," ",loc1); loc2 = gsub("-"," ",loc2)
		loc = gsub(" - "," / ",loc); loc1 = gsub(" - "," / ",loc1); loc2 = gsub(" - "," / ",loc2)
		loc = gsub(" do | da | de "," ",loc); loc1 = gsub(" do | da | de "," ",loc1); loc2 = gsub(" do | da | de "," ",loc2)
		loc = gsub(" dos | das | des "," ",loc); loc1 = gsub(" dos | das | des "," ",loc1); loc2 = gsub(" dos | das | des "," ",loc2)
		loc = gsub(' d\' | d\'| d´| d\"| d’'," ",loc); loc1 = gsub(' d\' | d\'| d´| d\"| d’'," ",loc1); loc2 = gsub(' d\' | d\'| d´| d\"| d’'," ",loc2)
		loc = gsub(' d\' | d\'| d´| d\"| d’'," ",loc); loc1 = gsub(' d\' | d\'| d´| d\"| d’'," ",loc1); loc2 = gsub(' d\' | d\'| d´| d\"| d’'," ",loc2)
		loc = gsub('\\.$',"",loc); loc1 = gsub('\\.$',"",loc1); loc2 = gsub('\\.$',"",loc2)
		loc = gsub(" dx "," ",loc); loc1 = gsub(" dx "," ",loc1); loc2 = gsub(" dx "," ",loc2)
	# Final corrections
		loc = gsub("esao pauloirito santo","espirito santo",loc)
		loc = gsub ('alagoas_s\\. j\\. lage|alagoas_s\\. j\\. laje|alagoas_s\\.j lage|alagoas_s\\.j\\. lage|alagoas_s\\.j\\. laje|alagoas_s\\.j\\.da laje|alagoas_s\\.j\\.da lage',"alagoas_sao jose laje",loc)
		loc = gsub ('vila \\.b\\.s\\. trindade|vila bela s\\. trindade|vila bela santis\\. trindade|vila bela santis\\.trindade|vila bela sant\\. trindade|vila bela sm\\. trindade|vila bela stma\\. trindade',"vila bela santissima trindade",loc)
		loc[loc%in%"brazil_sao paulo_sjrpreto"] = "brazil_sao paulo_sao jose rio preto"
		loc = gsub ('brazil_guanabara_rio janeiro',"brazil_rio janeiro_rio janeiro",loc)
		loc1 = gsub("esao pauloirito santo","espirito santo",loc1)
		loc1 = gsub ('alagoas_s\\. j\\. lage|alagoas_s\\. j\\. laje|alagoas_s\\.j lage|alagoas_s\\.j\\. lage|alagoas_s\\.j\\. laje|alagoas_s\\.j\\.da laje|alagoas_s\\.j\\.da lage',"alagoas_sao jose laje",loc1)
		loc1 = gsub ('vila \\.b\\.s\\. trindade|vila bela s\\. trindade|vila bela santis\\. trindade|vila bela santis\\.trindade|vila bela sant\\. trindade|vila bela sm\\. trindade|vila bela stma\\. trindade',"vila bela santissima trindade",loc1)
		loc1[loc1%in%"brazil_sao paulo_sjrpreto"] = "brazil_sao paulo_sao jose rio preto"
		loc1 = gsub ('brazil_guanabara_rio janeiro',"brazil_rio janeiro_rio janeiro",loc1)
		loc2 = gsub("esao pauloirito santo","espirito santo",loc2)
		loc2 = gsub ('alagoas_s\\. j\\. lage|alagoas_s\\. j\\. laje|alagoas_s\\.j lage|alagoas_s\\.j\\. lage|alagoas_s\\.j\\. laje|alagoas_s\\.j\\.da laje|alagoas_s\\.j\\.da lage',"alagoas_sao jose laje",loc2)
		loc2 = gsub ('vila \\.b\\.s\\. trindade|vila bela s\\. trindade|vila bela santis\\. trindade|vila bela santis\\.trindade|vila bela sant\\. trindade|vila bela sm\\. trindade|vila bela stma\\. trindade',"vila bela santissima trindade",loc2)
		loc2[loc2%in%"brazil_sao paulo_sjrpreto"] = "brazil_sao paulo_sao jose rio preto"
		loc2 = gsub ('brazil_guanabara_rio janeiro',"brazil_rio janeiro_rio janeiro",loc2)

	## Getting coordinates from the gazetteer - county level or lower (provided in the occuurrence labels)
		tmp = left_join(data.frame(loc=loc, stringsAsFactors= FALSE),dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")],by="loc")
	 # downgrading the locality resolution for the localities not found in the gazetteer
		tmp1 = tmp[!is.na(tmp$loc)&is.na(tmp$loc.correct),]$loc
		if(length(tmp1)>0) {
			if(any(str_count(tmp1,"_")==1)) tmp1[str_count(tmp1,"_")==1] = sapply(strsplit(tmp1[str_count(tmp1,"_")==1],"_"), function(x) x[1]) 
			if(any(str_count(tmp1,"_")==2)) tmp1[str_count(tmp1,"_")==2] = sapply(strsplit(tmp1[str_count(tmp1,"_")==2],"_"), function(x) paste(x[1],x[2],sep="_")) 
			tmp2 = left_join(data.frame(loc=tmp1, stringsAsFactors= FALSE),dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")],by="loc")
			tmp[!is.na(tmp$loc)&is.na(tmp$loc.correct),c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")] = 
			       tmp2[,c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")]
		} else { 	}

	## Getting coordinates from the gazetteer - locality level
		### REVER FORMA DE REMOVER LOCALIDADES COM COORDENADAS DIFERENTES...
		#dic1 = dic[!duplicated(dic$loc)&!duplicated(dic$loc.correct)&dic$resolution.gazetteer%in%c("localidade","localidade|sublocalidade","distrito|vila","distrito|bairro","cachoeira","mina","vila"),]
		dic1 = dic[!duplicated(dic$loc)&!is.na(dic$loc.correct)&dic$resolution.gazetteer%in%c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra"),]
		tmp3 = left_join(data.frame(loc=loc1, stringsAsFactors= FALSE),dic1[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")],by="loc")
		# replacing coordinates at county level by those found at locality level
    tmp[tmp$resolution.gazetteer%in%"county"&!is.na(tmp3$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] = 
      tmp3[tmp$resolution.gazetteer%in%"county"&!is.na(tmp3$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
    # replacing coordinates at state level by those found at locality level (rare but can happen due to missing counties)
    tmp[tmp$resolution.gazetteer%in%"state"&!is.na(tmp3$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")] = 
      tmp3[tmp$resolution.gazetteer%in%"state"&!is.na(tmp3$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")]
    tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra")] = "locality"
    
  ## getting coordinates from the gazetteer - county level extracted from the locality (not 100% sure? needs validation...)
    tmp4 = left_join(data.frame(loc=loc2, stringsAsFactors= FALSE),dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")],by="loc")
    # replacing coordinates at state level by those found at county level
    tmp[tmp$resolution.gazetteer%in%"state"&!is.na(tmp4$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")] = 
      tmp4[tmp$resolution.gazetteer%in%"state"&!is.na(tmp4$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")]
    tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra")] = "locality"    

  ## Saving the filtered data
		herb.data = cbind.data.frame(herb.data,tmp[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")],loc1)
		path.csv = gsub("cleaned.csv","cleaned-loc.csv",myfiles0)
		write.csv(x=herb.data,file=path.csv, row.names = FALSE)
		cat(i,"\n")
}

##Finding missing counties in the gazetteer with lots of specimens in speciesLink and JABOT
#paths = c(dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE),
#          dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE),
#		      dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot",full.names=TRUE))

##Looping for each collection
#tmp = vector("list",length(paths))
#for(i in 1:length(paths)) {
#		myfiles0 = list.files(paths[i], full.names = TRUE)
#		myfiles0 = myfiles0[grep("edited-cleaned-loc.csv",myfiles0)]
#		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
#	##Removing unwanted columns and re-naming 
#		cols = c("collectionCode","collectioncode","siglacolecao",
#		         "country","pais","stateProvince","stateprovince","estado_prov",
#		         "municipality","county","cidade","loc","loc.correct","resolution.gazetteer","loc1") 
#				#"countrycode","locality","loc")				#GBIF
#		cls = unique(cols[cols %in% names(herb.data)])
#		if(cls[1]=="loc") { cls = c(cls[2:8],cls[1]) } else { cls = cls }
#		herb.data1 = herb.data[,cls]
#		names(herb.data1) = c("collection","country","state","municipality","loc","loc.correct","resolution.gazetteer","loc1")
#		tmp[[i]] = herb.data1 
#}
#tmp1 = do.call(rbind.data.frame,tmp)
#tmp1 = droplevels(tmp1)

###Table of missing counties
#tmp3 = tmp1[!tmp1$loc %in% dic$loc,]
#dim(tmp3)
#sort(table(tmp3$collection))

#tmp2 = data.frame(locs= names(table(tmp3$loc)), count= as.vector(table(tmp3$loc)))
#tmp2 = tmp2[grep("^brazil_|^argentina_|^paraguay_",tmp2$locs),]
#tmp2 = tmp2[grep(",",tmp2$locs,invert=TRUE),]
#toto = strsplit(as.character(tmp2$locs),"_")
#tmp2$country = sapply(toto,function(x) x[1])  
#tmp2$state = sapply(toto,function(x) x[2])
#tmp2$county = sapply(toto,function(x) x[3]) 
#tmp2 = tmp2[order(tmp2$count,decreasing = TRUE),]
#write.csv(tmp2,"tmp3.csv")

###Table of missing localities
#tmp3 = tmp1[!is.na(tmp1$loc1)&tmp1$resolution.gazetteer%in%c("county"),]
#dim(tmp3)
#sort(table(tmp3$collection))

#tmp2 = data.frame(locs= names(table(tmp3$loc1)), count= as.vector(table(tmp3$loc1)))
#tmp2 = tmp2[grep("^brazil_|^argentina_|^paraguay_",tmp2$locs),]
#tmp2 = tmp2[grep(",",tmp2$locs,invert=TRUE),]
#toto = strsplit(as.character(tmp2$locs),"_")
#tmp2$country = sapply(toto,function(x) x[1])  
#tmp2$state = sapply(toto,function(x) x[2])
#tmp2$county = sapply(toto,function(x) x[3]) 
#tmp2$loc = sapply(toto,function(x) x[4]) 
#tmp2 = tmp2[order(tmp2$count,decreasing = TRUE),]
#write.csv(tmp2[1:6000,],"tmp4.csv")


#####################################################################################################################################################################################################-
##########################################################################################################################
#### 1.1 Editing coordinates data and defining the working coordinates  - from speciesLink, speciesLink_new and JABOT ####
##########################################################################################################################
#Path to the edited files from speciesLink and JABOT
paths = c(dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE),
          dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new",full.names=TRUE),
		      dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot",full.names=TRUE),
		      dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//sndb_mincyt_arg",full.names=TRUE))
paths = list.files(paths, full.names = TRUE)
paths = paths[grep("edited-cleaned-loc.csv",paths)]

#Looping for each collection
#tmp = vector("list",length(paths))
for(i in 1:length(paths)) {
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
		if(dim(herb.data)[1]==0) next 
	##Removing unwanted columns and re-naming 
		cols = c("decimalLatitude","decimalLongitude", 		#speciesLink
				#"verbatimLatitude","verbatimLongitude", 	    #speciesLink
				"latitude","longitude",					              #speciesLink_new + JABOT
				"decimallatitude","decimallongitude")		      #GBIF
		cls = unique(cols[cols %in% names(herb.data)])
		herb.data1 = herb.data[,cls]
	##Obtaining the columns with coordinates
		lat = herb.data1[,1]; long = herb.data1[,2]
	##Preliminary editing
		  #coordinates without numbers
		lat[!grepl('\\d',lat)|!grepl('\\d',long)] = NA
		long[!grepl('\\d',lat)|!grepl('\\d',long)] = NA
	    #zero coordinates as missing
		lat[lat%in%0 & long%in%0] = NA 
		long[(is.na(lat)|lat%in%0) & long%in%0] = NA   
	    #one of the coordinates missing as missing
		lat[lat%in%0 & is.na(long)] = NA; long[long%in%0 & is.na(lat)] = NA
		  #blocked coordinates
		lat[lat %in% c("bloqueada","Bloqueada")] = NA
		long[long %in% c("bloqueada","Bloqueada")] = NA
			 #possible problems with decimal division  
		lat = gsub(',','\\.',lat); long = gsub(',','\\.',long) 
	##Any coordinate on non-decimal degrees format?	
		toto = sum(table(lat)) - sum(table(as.double(lat)))
		if(toto>0) {
			lat.ref = grepl("s|S",lat)&!grepl("n|N",lat); long.ref = grepl("w|W|o|O",long)&!grepl("e|E|l|L",long)
			lat = gsub('\'|\"|°|º|\\*|\\|'," ",lat); long = gsub('\'|\"|°|º|\\*|\\|'," ",long)
			lat = gsub('[a-z]'," ",lat, ignore.case=T); long = gsub('[a-z]'," ",long, ignore.case=T) 
			lat = gsub('   ',' ',lat); long = gsub('   ',' ',long)
			lat = gsub('  ',' ',lat); long = gsub('  ',' ',long)
			lat = str_trim(lat); long = str_trim(long)
			lat[!is.na(lat)&lat%in%c("0 0 0","0 0")] = NA; long[!is.na(long)&long%in%c("0 0 0","0 0")] = NA 
			lat[!is.na(lat)&lat%in%""] = NA; long[!is.na(long)&long%in%""] = NA 
			#unique(lat)
			lat1 = cbind.data.frame(grau = as.double(sapply(strsplit(lat," "),function(x)x[1])),
				  				min = as.double(sapply(strsplit(lat," "),function(x)x[2]))/60,
			       				sec = as.double(sapply(strsplit(lat," "),function(x)x[3]))/3600)
			lat2 = round(apply(lat1,1,sum,na.rm=TRUE),8)
			long1 = cbind.data.frame(grau = as.double(sapply(strsplit(long," "),function(x)x[1])),
				  				min = as.double(sapply(strsplit(long," "),function(x)x[2]))/60,
			       				sec = as.double(sapply(strsplit(long," "),function(x)x[3]))/3600)
			long2 = round(apply(long1,1,sum,na.rm=TRUE),8)
			lat2[lat.ref] = -lat2[lat.ref]; long2[long.ref] = -long2[long.ref] 
			lat2[!is.na(lat2)&lat2==0] = NA; long2[!is.na(long2)&long2==0] = NA 
		} else {
			lat2 = lat
			long2 = long
		}
	##Creating the working coodinates and defining the origin of the information
	if(all(is.na(lat))|all(is.na(long))) {
	  herb.data$latitude.work = NA
	  herb.data$longitude.work = NA
	  herb.data$origin.coord = NA
	  herb.data$resolution.coord = NA
	}	else {
		herb.data$latitude.work = lat2
		herb.data$longitude.work = long2
		herb.data$origin.coord = NA
		herb.data$origin.coord[!is.na(herb.data$latitude.work)] = "coord_original"
	##Assessing the resolution of the original coordinates
		herb.data$resolution.coord = NA
		lat3 = herb.data$latitude.work[!is.na(herb.data$latitude.work)]
		long3 = herb.data$latitude.work[!is.na(herb.data$latitude.work)]
	   #No minutes
		tmp1 = strsplit(lat3,"\\.")
		lat3[sapply(tmp1,length)==1] = "degrees_only"
		#if(sapply(tmp1,length)==1) lat3 = "degrees_only"
		tmp2 = strsplit(long3,"\\.")
		long3[sapply(tmp2,length)==1] = "degrees_only"
		#if(sapply(tmp2,length)==1) long3 = "degrees_only"
		
	   #No seconds
		mins= sapply(strsplit(as.character(round(1:59/60,6)),"\\."),function(x)x[2])
		#lat3[!lat3%in%"degrees_only" & sapply(tmp1,function(x)x[2])%in%c(mins,0)] = "minutes_only"
		#long3[!long3%in%"degrees_only" & sapply(tmp2,function(x)x[2])%in%c(mins,0)] = "minutes_only"
		mins1 = c(sapply(strsplit(as.character(round(1:59/60,6)),"\\."),function(x)x[2]),
		         sapply(strsplit(as.character(round(1:59/60,4)),"\\."),function(x)x[2]),
		         sapply(strsplit(as.character(round(1:59/60,2)),"\\."),function(x)x[2]))
		lat3[!lat3%in%"degrees_only" & sapply(tmp1,function(x)x[2])%in%c(mins1,0)] = "minutes_only"
		long3[!long3%in%"degrees_only" & sapply(tmp2,function(x)x[2])%in%c(mins1,0)] = "minutes_only"
	   #Minutes
		mins = as.double(mins)/10^(nchar(mins))
		secs = as.character(apply(cbind.data.frame(as.double(rep(c(mins,0),each=60)),as.double(rep(round(1:60/3600,6),60))),1,sum))
		secs = sapply(strsplit(secs,"\\."),function(x)x[2])
		secs = secs[!is.na(secs)]
		lat3[!lat3%in%c("degrees_only","minutes_only") & sapply(tmp1,function(x)x[2])%in%secs] = "seconds"
		long3[!lat3%in%c("degrees_only","minutes_only") & sapply(tmp2,function(x)x[2])%in%secs] = "seconds"
	   #Rounding issues	
		lat3[!lat3%in%c("degrees_only","minutes_only","seconds")] = "seconds?"
		long3[!long3%in%c("degrees_only","minutes_only","seconds")] = "seconds?"
	   #Saving the resolution
		tmp = herb.data$resolution.coord[!is.na(herb.data$latitude.work)]
		tmp[lat3%in%"degrees_only" & long3%in%"degrees_only"] = "degrees_only"
		tmp[lat3%in%"minutes_only" & long3%in%"minutes_only"] = "minutes_only"
		tmp[lat3%in%"seconds" & long3%in%"seconds"] = "seconds"
		tmp[lat3%in%"degrees_only" & long3%in%"minutes_only"] = "minutes_only"		
		tmp[long3%in%"degrees_only" & lat3%in%"minutes_only"] = "minutes_only"
		tmp[lat3%in%"degrees_only" & long3%in%"seconds"] = "seconds"		
		tmp[long3%in%"degrees_only" & lat3%in%"seconds"] = "seconds"
		tmp[lat3%in%"minutes_only" & long3%in%"seconds"] = "seconds"		
		tmp[long3%in%"minutes_only" & lat3%in%"seconds"] = "seconds"
		tmp[lat3%in%"seconds" & long3%in%"seconds?"] = "seconds?"		
		tmp[long3%in%"seconds" & lat3%in%"seconds?"] = "seconds?"
		tmp[lat3%in%"seconds?" & long3%in%"seconds?"] = "seconds?"		
		tmp[long3%in%"seconds?" & lat3%in%"seconds?"] = "seconds?"
		herb.data$resolution.coord[!is.na(herb.data$latitude.work)] = tmp
	##Removing problematic coordinates still in the mix
		herb.data$latitude.work[!is.na(herb.data$latitude.work)&abs(as.double(herb.data$latitude.work))>90] = NA
		herb.data$longitude.work[!is.na(herb.data$latitude.work)&abs(as.double(herb.data$latitude.work))>90] = NA
		herb.data$latitude.work[!is.na(herb.data$longitude.work)&abs(as.double(herb.data$longitude.work))>180] = NA
		herb.data$longitude.work[!is.na(herb.data$longitude.work)&abs(as.double(herb.data$longitude.work))>180] = NA
	}	
	##Replacing missing coordinates by the county coordinates from the gazetter 
		herb.data$latitude.work[is.na(herb.data$latitude.work)] = herb.data$latitude.gazetteer[is.na(herb.data$latitude.work)]
		herb.data$longitude.work[is.na(herb.data$longitude.work)] = herb.data$longitude.gazetteer[is.na(herb.data$longitude.work)]
		herb.data$origin.coord[!is.na(herb.data$latitude.work)&is.na(herb.data$origin.coord)] = "coord_gazet"
		herb.data$origin.coord[is.na(herb.data$origin.coord)] = "no_coord"
		herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "country"] = "ok_country_gazet"
		herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "state"] = "ok_state_gazet"		
		herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "county"] = "ok_county_gazet"
		herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "locality"] = "ok_locality_gazet"
	##Saving the filtered data
		path.csv = gsub("cleaned-loc.csv","cleaned-loc-coord.csv",myfiles0)
		write.csv(x=herb.data,file=path.csv, row.names = FALSE)
		cat(i,"\n")
}




##################################################################################################################################################################
##################################################################################################################################################################
#### 2 - Finding and editing locality data - GBIF ####
######################################################
## Uploading the final/edited version of the gazetter, with missing counties and ortographical variants of some names (not exaustive)
dic = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)
dic = dic[dic$status %in% "ok",]

## Finding the data
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE)
paths = paths[grep("-edited-cleaned.csv",paths)]		

#Looping for each collection
for(i in 1:length(paths)) {
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns and re-naming 
		cols = c("verbatimLatitude","verbatimLongitude","latitude","longitude","decimallatitude","decimallongitude",
		         "country","pais","countrycode",
		         "stateProvince","estado_prov","estado","stateprovince",
		         "municipality","cidade","county",
		         "locality", "descrlocal")
		cls = unique(cols[cols %in% names(herb.data)])
		herb.data1 = herb.data[,cls]
		n0 = herb.data1[,3] # country 
		n1 = herb.data1[,4] # province/state
		n2 = herb.data1[,5] # municipality/commune/county
		n3 = herb.data1[,6] # locality
		#n3 = rep(NA,dim(herb.data1)[1]) #locality
		#n4 = herb.data1[,6] #descrlocal
		n3[n3 %in% c("")] = NA
		n3 = gsub("\\.$|,$|:$","", n3)
		n3 = gsub("  "," ",n3)
		
		#Nivel0: Country
		n0 = countrycode(n0, 'iso2c', 'country.name')
		n0[is.na(n0)&!is.na(n3)&grepl("^brasil$|^brazil$",n3,ignore.case=T)&!grepl('\\?',n3)] = "Brazil"
		n0[is.na(n0)&!is.na(n3)&grepl("^brasil\\,|^brasil\\;|^brasil:",n3,ignore.case=T)&!grepl('\\?',n3)] = "Brazil"
		n0[is.na(n0)&!is.na(n3)&grepl("argentin|misiones",n3,ignore.case=T)&!grepl('\\?',n3)] = "Argentina"
		n0[is.na(n0)&!is.na(n3)&grepl("paraguay",n3,ignore.case=T)&!grepl('\\?',n3)] = "Paraguay"
		n0 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n0))
		n0[is.na(n0)] = "unspecified_country"
		#n0[n0 %in% c("No locality information available","Protected due to name conservation status","Completar datos","NO DISPONIBLE")] = "unspecified_country"
		n0 = sapply(strsplit(n0,' - | \\('), function(x) x[[1]][1])
		n0 = str_trim(n0)
		#sort(unique(n0))	

		#Nivel1 and 2: States, Provinces and Counties/Communes
		n1[n0%in%"brazil" & grepl("Brasilia DF|Brasília DF|Brasilia, DF|Brasília, DF",n3) & !grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "distrito federal"
		n2[n0%in%"brazil" & grepl("Brasilia DF|Brasília DF|Brasilia, DF|Brasília, DF",n3) & !grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "brasilia"
		n0[grepl("Reserva Natural del Bosque de Mbaracayú|Mbaracayú Natural Reserve", n3)] = "paraguay"
		n1[grepl("Reserva Natural del Bosque de Mbaracayú|Mbaracayú Natural Reserve", n3)] = "canindeyu"
		n2[grepl("Reserva Natural del Bosque de Mbaracayú|Mbaracayú Natural Reserve", n3)] = "villa ygatimi"		
		n1[is.na(n1)&grepl("^Rio de Janeiro$|^Rio de Janeiro\\.$|^Rio Janeiro\\.$|^Near Rio Janeiro\\.$|^Guanabara$|^Near Rio de Janeiro\\.$|^Nova Friburgo$|^Environs de Rio Janeiro",n3)] = "rio janeiro"
		n1[is.na(n1)&grepl("^Organ Mountains$|^Organ Mts\\.$|^Serra dos Órgãos|Organ Mount\\.",n3)] = "rio janeiro"
		n2[is.na(n2)&grepl("^Organ Mountains$|^Organ Mts\\.$|^Serra dos Órgãos|Organ Mount\\.",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "teresopolis"
		n3[is.na(n3)&grepl("^Organ Mountains$|^Organ Mts\\.$|^Serra dos Órgãos|Organ Mount\\.",n3)] = "serra orgaos"
		n1[is.na(n1)&grepl("^Restinga da Tijuca|^Restinga de Jacarepaguá|^Corcovado|^Restinga de Grumari$|^Gávea|^Vista Chinesa|^Barra da Tijuca",n3)] = "rio janeiro"
		n1[is.na(n1)&grepl("^Araguari$",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "minas gerais"
		n2[is.na(n2)&grepl("^Araguari$",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "araguari"
		n1[is.na(n1)&grepl("^Estação Esperimental de Silvicultura Tropical",n3)] = "amazonas"
		n1[n0%in%"brazil"&grepl("^Rio Cuieiras\\.$|^Manaos\\.$|^Manaus$",n3)] = "amazonas"
		n1[n0%in%"brazil"&grepl("^Belem$",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "para"
		n1[is.na(n1)&grepl(", Pratânia",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "sao paulo"
		n2[is.na(n2)&grepl(", Pratânia",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "pratania"
		n1[is.na(n1)&grepl("Parque Estadual do Ibitipoca|Serra de Ibitipoca",n3)] = "minas gerais"
		n2[is.na(n2)&grepl("Parque Estadual do Ibitipoca|Serra de Ibitipoca",n3)] = "lima duarte"
		n3[is.na(n3)&grepl("Parque Estadual do Ibitipoca",n3)] = "pe ibitipoca"
		n3[is.na(n3)&grepl("Parque Estadual do Ibitipoca",n3)] = "serra ibitipoca"
		n1[is.na(n1)&grepl("Alter do Chão",n3)] = "para"
		n2[is.na(n2)&grepl("Alter do Chão",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "alter chao"
		n1[is.na(n1)&grepl("Serra da Bocaina",n3)] = "rio janeiro"
		n1[is.na(n1)&grepl("Estação Biológica de Santa Lúcia",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "espirito santo"
		n2[is.na(n2)&grepl("Estação Biológica de Santa Lúcia",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "santa teresa"
		n3[is.na(n3)&grepl("Estação Biológica de Santa Lúcia",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "estacao biologica santa lucia"
		n0[is.na(n0)&grepl("^Sierra de Amambay$",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "paraguay"
		n1[is.na(n1)&grepl("^Sierra de Amambay$",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "amambay"
		n1[is.na(n1)&grepl("^Estação Ecológica de Mamirauá",n3)] = "amazonas"
		n1[is.na(n1)&grepl("^Parque Nacional de Jaú",n3)] = "amazonas"
		n1[is.na(n1)&grepl("^Reserva de Desenvolvimento Sustentável de Mamirauá",n3)] = "amazonas"
		n1[is.na(n1)&grepl("^Prov\\. Minas Geraes",n3)] = "minas gerais"
		n1[is.na(n1)&grepl("^Sao Francisco de Paula",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "rio grande sul"
		n2[is.na(n2)&grepl("^Sao Francisco de Paula",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "sao francisco paula"
		n1[is.na(n1)&grepl("^Parque Nacional do Viruá",n3)] = "roraima"
		n1[is.na(n1)&grepl("Reserva Ecológica Estadual de Jacarepiá",n3)] = "rio janeiro"
		n2[is.na(n2)&grepl("Reserva Ecológica Estadual de Jacarepiá",n3)] = "saquarema"
		n3[is.na(n3)&grepl("Reserva Ecológica Estadual de Jacarepiá",n3)] = "reserva ecologica estadual jacarepia"
		n1[is.na(n1)&grepl("Estação Ecológica Estadual de Paraíso",n3)] = "rio janeiro"
  		n1[is.na(n1)&grepl("Pico Alto Moirão",n3)] = "rio janeiro"
		n2[is.na(n2)&grepl("Pico Alto Moirão",n3)] = "marica"
		n1[is.na(n1)&grepl("Reserva Ecológica do IBGE",n3)] = "distrito federal"
		n2[is.na(n2)&grepl("Reserva Ecológica do IBGE",n3)] = "brasilia"
		n3[is.na(n3)&grepl("Reserva Ecológica do IBGE",n3)] = "resec ibge (roncador)"
		n1[is.na(n1)&grepl("Basin of Rio Madeira|Bacia do Rio Madeira|^Rio Madeira\\.",n3)] = "amazon"
		n1[is.na(n1)&grepl("^In vicinibus Santarem",n3)] = "para"
		n1[is.na(n1)&grepl("^Serra do Cachimbo\\.",n3)] = "amazon"
		n1[is.na(n1)&grepl("Cuiabá",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "mato grosso"
		n2[is.na(n2)&grepl("Cuiabá",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "cuiaba"
		n1[is.na(n1)&grepl("^Carangola\\.",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "minas gerais"
		n2[is.na(n2)&grepl("^Carangola\\.",n3)&!grepl("strada entre |odovia entre |divisa ",n3, ignore.case = TRUE)] = "carangola"
		n1[is.na(n1)&grepl("Reserva Florestal do Sacavem",n3)] = "maranhao"
		n2[is.na(n2)&grepl("Reserva Florestal do Sacavem",n3)] = "sao luis"
		n1[is.na(n1)&grepl("Ilha do Mel",n3)] = "parana"
		n2[is.na(n2)&grepl("Ilha do Mel",n3)] = "paranagua"
		n3[is.na(n3)&grepl("Ilha do Mel",n3)] = "ilha mel"
		n1[is.na(n1)&grepl("Reserva Florestal Ducke",n3)] = "amazonas"
		n1[is.na(n1)&grepl("Reserva Florestal Ducke",n3)] = "manaus"
		n1[is.na(n1)&grepl("Rio Iaco",n3)] = "acre"
		n1[is.na(n1)&grepl("basin of Rio Purus|bacia Rio Purus",n3,ignore.case=TRUE)] = "amazon"
		n1[is.na(n1)&grepl("Basin of Rio Jurua|bacia Rio Jurua",n3,ignore.case=TRUE)] = "amazon"
		n1[is.na(n1)&grepl("Basin of Rio Tapajoz|Rio Tapajoz",n3,ignore.case=TRUE)] = "amazon"
		n1[is.na(n1)&grepl("Basin of Rio Solimoes|Bacia do Rio Solimoes|Bacia do Rio Solimões",n3)] = "amazon"
		n1[is.na(n1)&grepl("Amazonia",n3, ignore.case=TRUE)] = "amazon"
		#Bacia do Rio SÃ£o Bartolomeu: DF?
		
		#find missing states for states that may create problems by using grep (e.g. Mato Grosso do Sul and Mato Grosso)
		n1[is.na(n1) & n0%in%"brazil" & grepl("Mato Grosso do Sul|Mato Grosso Sul|, MS\\.",n3) & !grepl("Divisa ",n3, ignore.case = TRUE)] = "Mato Grosso do Sul"
		n1[is.na(n1) & n0%in%"brazil" & grepl("Acre|, AC\\.",n3) & 
		     !grepl("Rio Acre|Novo Acre|Boca do Acre|Brasilia-Acre|Brasilica-Acre|Mato Grosso-Acre|Brasilia Acre Highway|Divisa ",n3, ignore.case = TRUE)] = "Acre"
		n1[is.na(n1) & n0%in%"brazil" & grepl("Amazonas|, AM\\.",n3) & 
		     !grepl("Rio Amazonas|Alto Amazonas|Rondonia|Rondônia|Roraima|Planta do Amazonas|Maria do Amazonas|Divisa ",n3, ignore.case = TRUE)] = "Amazonas"
		n1[is.na(n1) & n0%in%"brazil" & grepl("Bahia|, BA\\.",n3) & !grepl("Igarape Bahia|Victoria and Bahia|Bahia and Pernambuco|Coco-da Bahia|Bahia - Espirito Santo|Divisa ",n3, ignore.case = TRUE)] = "Bahia"
		n1[is.na(n1) & n0%in%"brazil" & grepl("Paraíba|Paraiba|, PB\\.",n3) & 
		     !grepl("fluv. Paraiba|Rio Paraiba|Río Paraiba|Rio Paraíba|Paraiba do Sul|Paraíba do Sul|Paraiba da Sul|Paraiba River|Além Paraiba|Além-Paraiba|Alem Paraiba|Alem-Paraiba|Divisa ",n3, ignore.case = TRUE)] = "Paraíba"
		n1[is.na(n1) & n0%in%"brazil" & grepl("Paraná|, PR\\.|, Parana\\.",n3) & 
		     !grepl("Rio Paraná|Mirití-Paraná|Alto Paraná|del Paraná|Ati Paraná|Autai Paraná|Mineruá|Mamupina|Juamí-Japurá|Anacho|Tefé|Xiborena|Paraná d|Divisa ",n3, ignore.case = TRUE)] = "Paraná"
		#some prep edits
		n3 = gsub("Brasilia DF,|Brasília DF,|Brasília DF\\.","Brasilia, DF.",n3) 
		n3 = gsub("Brasilia DF","Brasilia, DF",n3)
		#some list of expressions to avoid mismatches between states
		avoid = c("Alto Amazonas|Divisa |Brasilia-Acre|Mato Grosso-Acre|Estados Pará, Goiás, Mato Grosso|Rio Maranhão|to Maranhão km|Igarapé Espírito Santo|Fazenda Espírito Santo|Fazenda São Paulo|Distrito Federal or Goiás|Distrito Federal ou Goiás|Sao Paulo\\/Minas Gerais|Goias\\/Minas Gerais|Rio de Janeiro and São Paulo| of Rondônia state line| of Rondonia state line|Rondonia-Amazonas|Amazonas e Roraima|São Paulo de Oliven|Sao Paulo de Oliven|Sao Paulo d'Olivenc|Sao Paulode Olivenca|Linha Sao Paulo|Linha São Paulo|Universidade de São Paulo|Rio Tocantin")
		#Looping for missing state information in the 3 Atlantic Forest countries (All other states)
		bra.states1 = bra.states[!grepl("Mato Grosso do Sul|Acre|Amazonas|Bahia|Paraíba|Paraná",bra.states)]
		if (any(n0%in%"brazil")) for (j in 1:length(bra.states1)) { n1[is.na(n1) & n0%in%"brazil" & grepl(bra.states1[j],n3, ignore.case = TRUE) & !grepl(avoid,n3, ignore.case = TRUE)] = names(bra.states1[j]) }
		if (any(n0%in%"argentina")) for (j in 1:length(arg.states)) { n1[is.na(n1) & n0%in%"argentina" & grepl(arg.states[j],n3)] = names(arg.states[j]) }
		if (any(n0%in%"paraguay")) for (j in 1:length(par.states)) { n1[is.na(n1) & n0%in%"paraguay" & grepl(par.states[j],n3)] = names(par.states[j]) }
		
		#Further editing and standardization		
		n1 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n1))
		n2 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n2))
		n1 = str_trim(n1)
		n2 = str_trim(n2)
		
		##Editing information prior to the extraction of missing info
		n3 = gsub("Ã\u00AD","í",n3)		
		n3 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), n3))
		n3 = gsub("^no disponible$|^não informado$|^inderminado$|^inderminada$|^n\\.i\\.$|^desconhecido$|^sem informacao$|^s\\/localidade$|^no information$|^not available$",NA,n3)
		n3 = gsub("a¡","a",n3)
		n3 = gsub("a©|ã©|aª","e",n3)
		n3 = gsub("aº","u",n3)
		n3 = gsub("a§","c",n3)
		n3 = gsub("a´|a³","o",n3)
		n3 = gsub("a¢|a¡|a£|a'","a",n3)
		n3 = gsub("a\u0081","a",n3)
		n3 = gsub("^prov\\. ","provincia de ",n3)
		n3 = gsub(" prov\\. "," provincia de ",n3)
		n3 = gsub("^dep\\. |^dept\\. |^dpto\\. |dtto\\. |depto\\. ","departamento de ",n3)
		n3 = gsub(" dep\\. | dept\\. | dpto\\. "," departamento de ",n3)
		n3 = gsub("^mun\\. |^mun\\.: |^mpio\\. |^mpio: |^mpio\\.: ","municipio de ",n3)
		n3 = gsub(" mun\\. | mun\\.: | mpio\\. | mpio: | mpio\\.: "," municipio de ",n3)
		n3 = gsub("^mun\\.|^mun\\.:|^mpio\\.|^mpio:|^mpio\\.:","municipio",n3)
		n3 = gsub("\\(mun\\. ","(municipio de ",n3)
  		n3 = gsub("provincia de :","provincia de ",n3)	
		n3 = gsub("provincia de de ","provincia de ",n3)
		n3 = gsub("provincia de do ","provincia do ",n3)
		n3 = gsub("provincia de of ","provincia of ",n3)
		n3 = gsub("departamento de :","departamento de ",n3)	
		n3 = gsub("departamento de de ","departamento de ",n3)
		n3 = gsub("departamento de do ","departamento do ",n3)
		n3 = gsub("departamento de of ","departamento of ",n3)
		n3 = gsub("municipio de :","municipio de ",n3)	
		n3 = gsub("municipio de de ","municipio de ",n3)
		n3 = gsub("municipio de do ","municipio do ",n3)
		n3 = gsub("municipio de of ","municipio of ",n3)
		n3 = gsub("^est\\. ","estado ",n3)
		n3 = gsub("^gral\\. ","general ",n3)
		n3 = gsub("^rod\\. ","rodovia ",n3)
		n3 = gsub("^dr\\. ","doutor ",n3)
		n3 = gsub("municipio de sto\\.","municipio de santo",n3)
		n3 = gsub("municipio de sta\\.","municipio de santa",n3)
		n3 = gsub(" do sr\\. "," do senhor ",n3)
		n3 = gsub("faz\\. ","fazenda ",n3)
		n3 = gsub("estr\\. ","estrada ",n3)
		n3 = gsub("p\\.e\\.serra do mar,|p\\.e\\.serra do mar\\.","parque estadual serra mar",n3)
		n3 = gsub("p\\.e\\.serra do mar","parque estadual serra mar",n3)
		n3 = gsub("parque estadual da serra do mar,|parque estadual da serra do mar\\.","parque estadual serra mar",n3)
		n3 = gsub("serra do mar, nucleo |serra do mar \\(nucleo","serra do mar nucleo",n3) 
		n3 = gsub("e\\.f\\.b\\. |e\\. f\\. b\\. ","estacao de ferro de braganca",n3)
		n3 = gsub("r\\.f\\.ducke","reserva florestal ducke",n3)
		n3 = gsub("r\\.f\\.bananal","reserva florestal bananal",n3)
		n3 = gsub("r\\.f\\. ","reserva florestal",n3)
		n3 = gsub("parque est\\.","parque estadual",n3)
		n3 = gsub("parque nac\\.","parque nacional",n3)
		n3 = gsub("est\\. ecologica|est\\. ecol\\.|estacao ecol\\.","estacao ecologica",n3)
		n3 = gsub("est\\. experimental|est\\. exp\\.|estacao exp\\.","estacao experimental",n3)
		n3 = gsub("p\\. est\\.|p\\. \\e.|p\\.\\e.","parque estadual",n3)
		n3 = gsub("p\\. nac\\.|p\\. \\n.|p\\.\\n.","parque nacional",n3)
		n3 = gsub("e\\. ecologica|e\\.ecologica|e\\.ecol\\.|e\\. ecol\\.|e\\.ecologica|e\\.ec\\.","estacao ecologica",n3)
		n3 = gsub("e\\. experimental|e\\.experimental|e\\.exp\\.|e\\. exp\\.","estacao experimental",n3)
		n3 = gsub("e\\.b\\. |e\\. b\\. |e\\. biol\\. |est\\. biol\\. |est\\. biologica |e\\. biologica ","estacao biologica ",n3)
		n3 = gsub("e\\.e\\.|e\\. e\\.|e\\. biol\\. |est\\. biol\\. |est\\. biologica |e\\. biologica ","estacao biologica ",n3)
		n3 = gsub("^segipe","sergipe",n3)
		n3 = gsub("^mato gosso","mato grosso",n3)
		n3 = gsub("^santa cataria","santa catarina",n3)
		n3 = gsub("^rondonica","rondonia",n3)
		n3 = gsub("minas gerias|mnas gerais|minas geraes|minas-geraes|meinas geraes|meimas geraes","minas gerais",n3)
		n3 = gsub("^espiritu santo|^espirito sanrto|^espirito santos|^espirito sato|^espitiro santo","espirito santo",n3)
		n3 = gsub("^missiones","misiones",n3)
		n3 = gsub("^alagaoas","alagoas",n3)
		n3 = gsub(" sma\\."," santissima",n3)
		n3 = gsub(" st\\. antonio"," santo antonio",n3)
		n3 = gsub(" s\\.jose"," sao jose",n3)
		n3 = gsub(" sta\\. elena"," santa elena",n3)
		n3 = gsub(" sta\\. rita"," santa rita",n3)
		n3 = gsub(" s\\. jose"," sao jose",n3)
		n3 = gsub(" s\\. joao"," sao joao",n3)
		n3 = gsub(" s\\. bento| s\\.bento"," sao bento",n3)
		n3 = gsub(" s\\. luis"," sao luis",n3)
		n3 = gsub(" s\\. miguel"," sao miguel",n3)
		n3 = gsub(" s\\. f\\. paula"," sao francisco paula",n3)
		n3 = gsub(" nª sª| nossa sra\\."," nossa senhora",n3)
		n3 = gsub(" s\\. luiz"," sao luiz",n3)
		n3 = gsub(" s\\. mateus"," sao mateus",n3)
		n3 = gsub(" s\\. francisco"," sao francisco",n3)
		n3 = gsub(" sta\\. maria"," santa maria",n3)
		n3 = gsub("^ca\\. ","circa ",n3)
		n3 = gsub(", ca\\. ",", circa ",n3)
		n3 = gsub('c\\. 1','circa 1',n3); n3 = gsub('c\\. 2','circa 2',n3); n3 = gsub('c\\. 3','circa 3',n3) 
		n3 = gsub('c\\. 4','circa 1',n3); n3 = gsub('c\\. 5','circa 2',n3); n3 = gsub('c\\. 6','circa 3',n3) 
		n3 = gsub('c\\. 7','circa 1',n3); n3 = gsub('c\\. 8','circa 2',n3); n3 = gsub('c\\. 9','circa 3',n3) 
		n3 = gsub("^s\\. of","south of",n3)
		n3 = gsub(" s\\. of"," south of",n3)
		n3 = gsub("^n\\. of","north of",n3)
		n3 = gsub(" n\\. of"," north of",n3)
		n3 = gsub("^e\\. of","east of",n3)
		n3 = gsub(" e\\. of"," east of",n3)
		n3 = gsub("^w\\. of","west of",n3)
		n3 = gsub(" w\\. of"," west of",n3)
		n3 = gsub("^s\\.e\\. of","southeast of",n3)
		n3 = gsub(" s\\.e\\. of"," southeast of",n3)
		n3 = gsub("^n\\.e\\. of","northeast of",n3)
		n3 = gsub(" n\\.e\\. of"," northeast of",n3)
		n3 = gsub("^s\\.w\\. of","southwest of",n3)
		n3 = gsub(" s\\.w\\. of"," soutwest of",n3)
		n3 = gsub("^n\\.w\\. of","northwest of",n3)
		n3 = gsub(" n\\.w\\. of"," northwest of",n3)
		n3 = gsub("^s\\.t\\. ","saint ",n3)
		n3 = gsub("^st\\. ","saint ",n3)
		n3 = gsub(", s\\.t\\. ",", saint ",n3)
		n3 = gsub("u\\.s\\. virgin islands","us virgin islands",n3)
		n3 = gsub("prop\\. sr\\.","propriedade senhor",n3)
		n3 = gsub("p\\. n\\. d","parque nacional d",n3)
		n3 = gsub("p\\.n\\. d","parque nacional d",n3)
		n3 = gsub("^p\\.n\\. ","parque nacional ",n3)
		n3 = gsub("^p\\. n\\. ","parque nacional ",n3)
		n3 = gsub("^p\\.n\\.n\\. ","parque nacional natural ",n3)
		n3 = gsub("p\\. e\\. d","parque estadual d",n3)
		n3 = gsub("p\\.e\\. d","parque estadual d",n3)
		n3 = gsub("^p\\.e\\.","parque estadual",n3)
		n3 = gsub("p\\. i\\. ","posto indigena",n3)
		n3 = gsub("p\\.i\\. ","posto indigena",n3)
		n3 = gsub("i\\.n\\.p\\.","instituto nacional pinho",n3)
		n3 = gsub("i\\. n\\. p\\.","instituto nacional pinho",n3)
		n3 = gsub("distr\\. ","distrito ",n3)
		n3 = gsub("^dist\\. ","distrito ",n3)
		n3 = gsub("vic\\. of","vicinity of",n3)
		n3 = gsub("vic\\. ","vicinity ",n3)
		n3 = gsub("r\\. jari","rio jari",n3)
		n3 = gsub("r\\. xingu","rio xingu",n3)
		n3 = gsub("r\\. juruena","rio juruena",n3)
		n3 = gsub("r\\. uaupes","rio uaupes",n3)
		n3 = gsub("r\\. negro","rio negro",n3)
		n3 = gsub("r\\. trombetas","rio trombetas",n3)
		n3 = gsub("r\\. tapajoz|r\\. tapajos","rio tapajos",n3)
		n3 = gsub("\\(esec\\) ","",n3)
		n3 = gsub("\\(flona\\) ","",n3)
		n3 = gsub("\\(horto florestal\\) ","horto florestal",n3)
		n3 = gsub(" s\\. paulo"," sao paulo",n3)
		n3 = gsub("^s\\. ","s ",n3)
		n3 = gsub("^r\\. ","r ",n3)
		n3 = gsub("^prox\\. ","proximo ",n3)
		n3 = gsub("^prox\\. ","proximo ",n3)
		n3 = gsub("^p\\.n\\.i\\.","pni",n3)
		n3 = gsub("^p\\.n\\.s\\.o\\.","pnso",n3)
		n3 = gsub("^p\\. ","p ",n3)
		n3[n3 %in% c("")] = NA
		n3 = gsub("\\.$|,$|:$","",n3)
		n3 = gsub("  "," ",n3)
		n3 = str_trim(n3)

		#Getting missing locality information
		 n5 = strsplit(n3,",|\\.|:|\\(|)|;")
		 n5 = sapply(n5,str_trim)
		# Missing states/dpeartaments
		 n5.1 = as.character(sapply(n5, function(x) paste(unique(str_trim(x[grepl("stado d|state of|provincia d|province o|province d|territory o|territoire d|departamento d|departamiento d",x)])),collapse = "|")))
		 n5.1 = str_trim(n5.1)
		 n5.1 = gsub("^estado de |^estado do |^estado da |^state of |^provincia de |^province de |^province of |^territory of |^territoire du |^departamento de |^departamiento de |^departamiento del ","",n5.1)
		 n5.1 = gsub("^brasil estado de |^brasil estado do |^brasil estado da |^brazil state of |^brasil provincia de |^brasilia provincia de |^bresil province de |^brazil province of |^brazil territory of |^bresil territoire du ","",n5.1)
		 #n5.1 = gsub("estado | state|provincia | province| territory","",n5.1)
		 n5.1[n5.1%in%""] = NA
		 n5.1 = str_trim(n5.1)
		 # Missing counties/provinces
		 n5.2 = as.character(sapply(n5, function(x) paste(unique(str_trim(x[grepl("municipio|municipality|county|provincia|village",x)])),collapse = "|")))
		 n5.2 = str_trim(n5.2)
		 n5.2 = gsub("^municipio de |^municipio do |^municipality of |^county of |^provincia de|^provincia of|village of","",n5.2)
		 n5.2 = gsub("^municipio |^municipality |^municipio |^county |^provincia |village ","",n5.2)
		 n5.2 = str_trim(n5.2)
		 n5.2 = gsub("  "," ",n5.2)
		 n5.2[n5.2%in%""] = NA
		 # getting missing counties that may be the first part of the locality description (e.g. "Rio de Contas, Pico das Almas")
		 n5.2.1 = as.character(sapply(n5, function(x) x[1]))
		 n5.2.1 = str_trim(n5.2.1)
		 n5.2.1[n5.2.1%in%""] = NA
		 # isolating localities
		 locais = "parque|park|reserva|reserve|fazenda|nacional|national|estadual|parna|flona|rebio|rppn|e\\.e\\.|biologica|ecologica|extrativista|farm|estrada|rodovia|road|^sitio|^mata|^horto|^jardim|campus|^pico|^serra|^sierra|^morro|^chapada|mountain|peak|^laguna"
		 n5.3 = as.character(sapply(n5, function(x) paste(unique(str_trim(x[grepl(locais,x)])),collapse = ", ")))
		 n5.3 = str_trim(n5.3)
		 n5.3[n5.3%in%""] = NA
		#other localities, other than the ones in 'locais' and counties
		 n5.3[is.na(n5.3)&!is.na(n3)] = as.character(sapply(n5[is.na(n5.3)&!is.na(n3)],function(x) head(x[!grepl("municipio|municipality|county|provincia|village",x)],1)))

	##Replacing the locality extracted manually
		 # replacing states/provinces
		 n1[is.na(n1)&!is.na(n5.1)] = n5.1[is.na(n1)&!is.na(n5.1)]
		 # replacing missing counties/provinces
		 n2[is.na(n2)&!is.na(n5.2)] = n5.2[is.na(n2)&!is.na(n5.2)]
		 n3[!is.na(n5.3)] = n5.3[!is.na(n5.3)]
		 #first string of the locality fields for or other localities
		 n3[is.na(n5.3)] = as.character(sapply(n5[is.na(n5.3)],function(x) x[1]))
		 #sort(unique(n1))	
	  #Final corrections
		 n0 = str_trim(n0);  n0 = gsub("  "," ",n0); n0[n0%in%""] = NA
		 n1 = str_trim(n1);  n1 = gsub("  "," ",n1); n1[n1%in%""] = NA
		 n2 = str_trim(n2);  n2 = gsub("  "," ",n2); n2[n2%in%""] = NA
		 n3 = str_trim(n3);  n3 = gsub("  "," ",n3); n3[n3%in%""] = NA
		 n2[n2%in%"character(0)"] = NA
		 n3[n3%in%"character(0)"] = NA
		 		 
	## Definig a unique code for each county, state/province or
		loc = rep(NA,dim(herb.data1)[1])
		loc[!is.na(n0)&!is.na(n1)&!is.na(n2)] = paste(n0[!is.na(n0)&!is.na(n1)&!is.na(n2)],
			n1[!is.na(n0)&!is.na(n1)&!is.na(n2)],n2[!is.na(n0)&!is.na(n1)&!is.na(n2)],sep="_")   
		loc[is.na(loc)&!is.na(n0)&!is.na(n1)] = paste(n0[is.na(loc)&!is.na(n0)&!is.na(n1)],
			n1[is.na(loc)&!is.na(n0)&!is.na(n1)],sep="_")
		loc[is.na(loc)&!is.na(n0)] = n0[is.na(loc)&!is.na(n0)]
		loc[loc %in% "unspecified_country"] = NA
	## Definig a unique code for each locality
		loc1 = rep(NA,dim(herb.data1)[1])
		loc1[!is.na(n0)&!is.na(n1)&!is.na(n2)&!is.na(n3)] = 
		  paste(n0[!is.na(n0)&!is.na(n1)&!is.na(n2)&!is.na(n3)],n1[!is.na(n0)&!is.na(n1)&!is.na(n2)&!is.na(n3)],n2[!is.na(n0)&!is.na(n1)&!is.na(n2)&!is.na(n3)],n3[!is.na(n0)&!is.na(n1)&!is.na(n2)&!is.na(n3)],sep="_")   
		loc1[is.na(loc1)&!is.na(n0)&!is.na(n1)&!is.na(n3)] = 
		  paste(n0[is.na(loc1)&!is.na(n0)&!is.na(n1)&!is.na(n3)],n1[is.na(loc1)&!is.na(n0)&!is.na(n1)&!is.na(n3)],n2[is.na(loc1)&!is.na(n0)&!is.na(n1)&!is.na(n3)],n3[is.na(loc1)&!is.na(n0)&!is.na(n1)&!is.na(n3)],sep="_")   
		loc1[is.na(loc1)&!is.na(n0)&!is.na(n3)] = 
		  paste(n0[is.na(loc1)&!is.na(n0)&!is.na(n3)],n1[is.na(loc1)&!is.na(n0)&!is.na(n3)],n2[is.na(loc1)&!is.na(n0)&!is.na(n3)],n3[is.na(loc1)&!is.na(n0)&!is.na(n3)],sep="_")   
	## Definig a unique code for an alternative way of getting missing countis from the locality field
		loc2 = rep(NA,dim(herb.data1)[1])
		#county-level when county is not given
		loc2[!is.na(n0)&!is.na(n1)&is.na(n2)&!is.na(n5.2.1)] = 
		  paste(n0[!is.na(n0)&!is.na(n1)&is.na(n2)&!is.na(n5.2.1)], n1[!is.na(n0)&!is.na(n1)&is.na(n2)&!is.na(n5.2.1)], n5.2.1[!is.na(n0)&!is.na(n1)&is.na(n2)&!is.na(n5.2.1)],sep="_")	

	## Final edits to simplify the coordinate cross checking process
		loc = gsub("-"," ",loc); loc1 = gsub("-"," ",loc1); loc2 = gsub("-"," ",loc2)
		loc = gsub(" - "," / ",loc); loc1 = gsub(" - "," / ",loc1); loc2 = gsub(" - "," / ",loc2)
		loc = gsub(" do | da | de "," ",loc); loc1 = gsub(" do | da | de "," ",loc1); loc2 = gsub(" do | da | de "," ",loc2)
		loc = gsub(" dos | das | des "," ",loc); loc1 = gsub(" dos | das | des "," ",loc1); loc2 = gsub(" dos | das | des "," ",loc2)
		loc = gsub(' d\' | d\'| d´| d\"| d’'," ",loc); loc1 = gsub(' d\' | d\'| d´| d\"| d’'," ",loc1); loc2 = gsub(' d\' | d\'| d´| d\"| d’'," ",loc2)
		loc = gsub(' d\' | d\'| d´| d\"| d’'," ",loc); loc1 = gsub(' d\' | d\'| d´| d\"| d’'," ",loc1); loc2 = gsub(' d\' | d\'| d´| d\"| d’'," ",loc2)
		loc = gsub('\\.$',"",loc); loc1 = gsub('\\.$',"",loc1); loc2 = gsub('\\.$',"",loc2)
		loc = gsub(" dx "," ",loc); loc1 = gsub(" dx "," ",loc1); loc2 = gsub(" dx "," ",loc2)
		# Final corrections
		loc = gsub("esao pauloirito santo","espirito santo",loc)
		loc = gsub ('alagoas_s\\. j\\. lage|alagoas_s\\. j\\. laje|alagoas_s\\.j lage|alagoas_s\\.j\\. lage|alagoas_s\\.j\\. laje|alagoas_s\\.j\\.da laje|alagoas_s\\.j\\.da lage',"alagoas_sao jose laje",loc)
		loc = gsub ('vila \\.b\\.s\\. trindade|vila bela s\\. trindade|vila bela santis\\. trindade|vila bela santis\\.trindade|vila bela sant\\. trindade|vila bela sm\\. trindade|vila bela stma\\. trindade',"vila bela santissima trindade",loc)
		loc[loc%in%"brazil_sao paulo_sjrpreto"] = "brazil_sao paulo_sao jose rio preto"
		loc = gsub ('brazil_guanabara_rio janeiro',"brazil_rio janeiro_rio janeiro",loc)
		loc1 = gsub("esao pauloirito santo","espirito santo",loc1)
		loc1 = gsub ('alagoas_s\\. j\\. lage|alagoas_s\\. j\\. laje|alagoas_s\\.j lage|alagoas_s\\.j\\. lage|alagoas_s\\.j\\. laje|alagoas_s\\.j\\.da laje|alagoas_s\\.j\\.da lage',"alagoas_sao jose laje",loc1)
		loc1 = gsub ('vila \\.b\\.s\\. trindade|vila bela s\\. trindade|vila bela santis\\. trindade|vila bela santis\\.trindade|vila bela sant\\. trindade|vila bela sm\\. trindade|vila bela stma\\. trindade',"vila bela santissima trindade",loc1)
		loc1[loc1%in%"brazil_sao paulo_sjrpreto"] = "brazil_sao paulo_sao jose rio preto"
		loc2 = gsub ('brazil_guanabara_rio janeiro',"brazil_rio janeiro_rio janeiro",loc2)
		loc2 = gsub("esao pauloirito santo","espirito santo",loc2)
		loc2 = gsub ('alagoas_s\\. j\\. lage|alagoas_s\\. j\\. laje|alagoas_s\\.j lage|alagoas_s\\.j\\. lage|alagoas_s\\.j\\. laje|alagoas_s\\.j\\.da laje|alagoas_s\\.j\\.da lage',"alagoas_sao jose laje",loc2)
		loc2 = gsub ('vila \\.b\\.s\\. trindade|vila bela s\\. trindade|vila bela santis\\. trindade|vila bela santis\\.trindade|vila bela sant\\. trindade|vila bela sm\\. trindade|vila bela stma\\. trindade',"vila bela santissima trindade",loc2)
		loc2[loc2%in%"brazil_sao paulo_sjrpreto"] = "brazil_sao paulo_sao jose rio preto"
		loc2 = gsub ('brazil_guanabara_rio janeiro',"brazil_rio janeiro_rio janeiro",loc2)
		
	## Getting coordinates from the gazetteer
		tmp = left_join(data.frame(loc=loc, stringsAsFactors= FALSE),dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")],by="loc")
	   #Downgrading the locality resolution for the localities not found in the gazetteer
		tmp1 = tmp[!is.na(tmp$loc)&is.na(tmp$loc.correct),]$loc
		if(length(tmp1)>0) {
			if(any(str_count(tmp1,"_")==1)) tmp1[str_count(tmp1,"_")==1] = sapply(strsplit(tmp1[str_count(tmp1,"_")==1],"_"), function(x) x[1]) 
			if(any(str_count(tmp1,"_")==2)) tmp1[str_count(tmp1,"_")==2] = sapply(strsplit(tmp1[str_count(tmp1,"_")==2],"_"), function(x) paste(x[1],x[2],sep="_")) 
			tmp2 = left_join(data.frame(loc=tmp1, stringsAsFactors= FALSE),dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")],by="loc")
			tmp[!is.na(tmp$loc)&is.na(tmp$loc.correct),c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")] = tmp2[,c("loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")]
		} else { 	}
		
	## Getting coordinates from the gazetteer - locality level
		### REVER FORMA DE REMOVER LOCALIDADES COM COORDENADAS DIFERENTES...
		#dic1 = dic[!duplicated(dic$loc)&!duplicated(dic$loc.correct)&dic$resolution.gazetteer%in%c("localidade","localidade|sublocalidade","distrito|vila","distrito|bairro","cachoeira","mina","vila"),]
		dic1 = dic[!duplicated(dic$loc)&!is.na(dic$loc.correct)&dic$resolution.gazetteer%in%c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra"),]
		tmp3 = left_join(data.frame(loc=loc1, stringsAsFactors= FALSE),dic1[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")],by="loc")
		# replacing coordinates at county level by those found at locality level
		tmp[tmp$resolution.gazetteer%in%"county"&!is.na(tmp3$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")] = 
		  tmp3[tmp$resolution.gazetteer%in%"county"&!is.na(tmp3$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer")]
		# replacing coordinates at state level by those found at locality level (rare but can happen due to missing counties)
		tmp[tmp$resolution.gazetteer%in%"state"&!is.na(tmp3$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")] = 
		  tmp3[tmp$resolution.gazetteer%in%"state"&!is.na(tmp3$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")]
		tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra")] = "locality" 
		## Getting coordinates from the gazetteer - county level extracted from the locality (not 100% sure? needs validation...)
		tmp4 = left_join(data.frame(loc=loc2, stringsAsFactors= FALSE),dic[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")],by="loc")
		# replacing coordinates at state level by those found at county level
		tmp[tmp$resolution.gazetteer%in%"state"&!is.na(tmp4$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")] = 
		  tmp4[tmp$resolution.gazetteer%in%"state"&!is.na(tmp4$resolution.gazetteer),c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")]
		tmp$resolution.gazetteer[tmp$resolution.gazetteer %in% c("localidade","localidade|sublocalidade","sublocalidade","distrito|vila","distrito","distrito|bairro","bairro","cachoeira","mina","vila","serra")] = "locality"    

	##Saving the filtered data
		herb.data = cbind.data.frame(herb.data,tmp[,c("loc","loc.correct","latitude.gazetteer","longitude.gazetteer","resolution.gazetteer","AtlanticForest_Lei11428")],loc1)
		herb.data$loc.correct[is.na(herb.data$loc.correct)] = n0[is.na(herb.data$loc.correct)]
		path.csv = gsub("cleaned.csv","cleaned-loc.csv",myfiles0)
		write.csv(x=herb.data,file=path.csv, row.names = FALSE)
		cat(i,"\n")
}


####################################################################################
#### 2.1 Finding missing states in the gazetteer with lots of specimens in GBIF ####
####################################################################################
#paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE)
#paths = paths[grep("-edited-cleaned-loc.csv",paths)]		

#locs1 = vector("list",length(paths))
#Looping for each collection
#for(i in 1:length(paths)) {
#		myfiles0 = paths[i]
#		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
#	##Removing unwanted columns and re-naming 
#		cols = c("decimallatitude","decimallongitude","countrycode","locality","loc","loc.correct","resolution.gazetteer","loc1")
#		cls =  cols[cols %in% names(herb.data)]
#		herb.data = herb.data[,cls] 
#		#names(herb.data) = c("recordedBy","TDWG.name","last.name","fingerprint","identifiedBy","TDWG.name","last.name","fingerprint")
#	##Saving the filtered data
#		locs1[[i]] = herb.data
#}
#locs1 = do.call(rbind.data.frame,locs1)
#locs1 = droplevels(locs1)


###Table of missing counties (no coords at all)
#locs2 = locs1[is.na(locs1$decimallatitude),]
#tmp2 = data.frame(loc1= names(table(locs2$loc1)), count= as.vector(table(locs2$loc1)))
#tmp3 = merge(tmp2,locs2[,c("countrycode","locality","loc","loc1","loc.correct","resolution.gazetteer")],by="loc1",all.x=TRUE)
#tmp3 = tmp3[order(tmp3$count,decreasing = TRUE),]
#head(tmp3,10)
#write.csv(tmp3,"no_country.csv")


###Table of missing counties
#locs2 = locs1[locs1$resolution.gazetteer%in%c("country","state"),]
#tmp2 = data.frame(locality= names(table(locs2$locality)), count= as.vector(table(locs2$locality)))
#tmp3 = merge(tmp2,locs2[,c("countrycode","locality","loc","loc1","loc.correct","resolution.gazetteer")],by="locality",all.x=TRUE)
#tmp3 = tmp3[order(tmp3$count,decreasing = TRUE),]
#tmp3 = tmp3[!duplicated(tmp3$locality),]
#tmp3 = head(tmp3,10000)
#toto = strsplit(as.character(tmp3$loc1),"_")
#tmp3$country = sapply(toto,function(x) x[1])  
#tmp3$state = sapply(toto,function(x) x[2])
#tmp3$county = sapply(toto,function(x) x[3]) 
#tmp3$loc = sapply(toto,function(x) x[4]) 
#write.csv(tmp3,"no_county.csv")

###Table of missing localities
#locs2 = locs1[!is.na(locs1$loc1),]
#tmp2 = data.frame(locality= names(table(locs2$locality)), count= as.vector(table(locs2$locality)))
#tmp3 = merge(tmp2,locs1[,c("countrycode","locality","loc","loc1","loc.correct","resolution.gazetteer")],by="locality",all.x=TRUE)
#tmp3 = tmp3[order(tmp3$count,decreasing = TRUE),]
#tmp3 = tmp3[!duplicated(tmp3$locality),]
#tmp3 = head(tmp3,10000)
#toto = strsplit(as.character(tmp3$loc1),"_")
#tmp3$country = sapply(toto,function(x) x[1])  
#tmp3$state = sapply(toto,function(x) x[2])
#tmp3$county = sapply(toto,function(x) x[3]) 
#tmp3$loc = sapply(toto,function(x) x[4]) 
#head(tmp3)
#write.csv(tmp3,"no_locality.csv")

#locs2 = locs1[!is.na(locs1$loc1),]
#locs2 = locs2[grepl("country|state|county",locs2$resolution.gazetteer),]
#tmp2 = data.frame(locs= names(table(locs2$loc1)), count= as.vector(table(locs2$loc1)))
#tmp2 = tmp2[order(tmp2$count,decreasing = TRUE),]
#tmp3 = merge(tmp2,locs1[,c("countrycode","locality","loc","loc1","loc.correct","resolution.gazetteer")],by="locality",all.x=TRUE)
#toto = strsplit(as.character(tmp2$locs),"_")
#tmp2$country = sapply(toto,function(x) x[1])  
#tmp2$state = sapply(toto,function(x) x[2])
#tmp2$county = sapply(toto,function(x) x[3]) 
#tmp2$loc = sapply(toto,function(x) x[4]) 
#head(tmp2,50)
#write.csv(tmp2,"tmp6.csv")


##################################################################################################################################################################
######################################################################################
#### 2.2. Editing coordinates data and defining the working coordinates from GBIF ####
######################################################################################

## Finding the data
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE)
paths = paths[grep("-edited-cleaned-loc.csv",paths)]		

#Looping for each collection
tmp = vector("list",length(paths))
for(i in 1:length(paths)) {
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns and re-naming 
		cols = c(	"decimallatitude","decimallongitude")		#GBIF
		cls = unique(cols[cols %in% names(herb.data)])
		herb.data1 = herb.data[,cls]
	##Obtaining the columns with coordinates
		lat = herb.data1[,1]; long = herb.data1[,2]
	##Preliminary editing
		#coordinates without numbers
		lat[!grepl('\\d',lat)|!grepl('\\d',long)] = NA
		long[!grepl('\\d',lat)|!grepl('\\d',long)] = NA
		#zero coordinates as missing
		lat[lat==0] = NA; long[long==0] = NA  
		lat = gsub(',','\\.',lat); long = gsub(',','\\.',long) #possible problems with decimal division
	 #blocked coordinates
		lat[lat %in% c("bloqueada","Bloqueada","BLOQUEADA")] = NA
		long[long %in% c("bloqueada","Bloqueada","BLOQUEADA")] = NA
	##Any coordinate on non-decimal degrees format?	
		toto = sum(table(lat)) - sum(table(as.double(lat)))
		if(toto>0) {
			lat.ref = grepl("s|S",lat)&!grepl("n|N",lat); long.ref = grepl("w|W|o|O",long)&!grepl("e|E|l|L",long)
			lat = gsub('\'|\"|°|º|\\*|\\|'," ",lat); long = gsub('\'|\"|°|º|\\*|\\|'," ",long)
			lat = gsub('[a-z]'," ",lat, ignore.case=T); long = gsub('[a-z]'," ",long, ignore.case=T) 
			lat = gsub('   ',' ',lat); long = gsub('   ',' ',long)
			lat = gsub('  ',' ',lat); long = gsub('  ',' ',long)
			lat = str_trim(lat); long = str_trim(long)
			lat[!is.na(lat)&lat%in%c("0 0 0","0 0")] = NA; long[!is.na(long)&long%in%c("0 0 0","0 0")] = NA 
			lat[!is.na(lat)&lat%in%""] = NA; long[!is.na(long)&long%in%""] = NA 
			#unique(lat)
			lat1 = cbind.data.frame(grau = as.double(sapply(strsplit(lat," "),function(x)x[1])),
				  				min = as.double(sapply(strsplit(lat," "),function(x)x[2]))/60,
			       				sec = as.double(sapply(strsplit(lat," "),function(x)x[3]))/3600)
			lat2 = round(apply(lat1,1,sum,na.rm=TRUE),8)
			long1 = cbind.data.frame(grau = as.double(sapply(strsplit(long," "),function(x)x[1])),
				  				min = as.double(sapply(strsplit(long," "),function(x)x[2]))/60,
			       				sec = as.double(sapply(strsplit(long," "),function(x)x[3]))/3600)
			long2 = round(apply(long1,1,sum,na.rm=TRUE),8)
			lat2[lat.ref] = -lat2[lat.ref]; long2[long.ref] = -long2[long.ref] 
			lat2[!is.na(lat2)&lat2==0] = NA; long2[!is.na(long2)&long2==0] = NA 
		} else {
			lat2 = lat
			long2 = long
		}
	##Creating the working coodinates and defining the origin/resolution of the information
		if(all(is.na(lat))|all(is.na(long))) {
		  herb.data$latitude.work = NA
		  herb.data$longitude.work = NA
		  herb.data$origin.coord = NA
		  herb.data$resolution.coord = NA
		}	else {
   		  herb.data$latitude.work = lat2
   		  herb.data$longitude.work = long2
  		  herb.data$origin.coord = NA
  		  herb.data$origin.coord[!is.na(herb.data$latitude.work)] = "coord_original"
   ##Assessing the resolution of the original coordinates
  		  herb.data$resolution.coord = NA
  		  lat3 = herb.data$latitude.work[!is.na(herb.data$latitude.work)]
  		  long3 = herb.data$latitude.work[!is.na(herb.data$latitude.work)]
     #No minutes
  		  tmp1 = strsplit(lat3,"\\.")
  		  lat3[sapply(tmp1,length)==1] = "degrees_only"
  		  tmp2 = strsplit(long3,"\\.")
  		  long3[sapply(tmp2,length)==1] = "degrees_only"
     #No seconds
  		  mins = sapply(strsplit(as.character(round(1:59/60,6)),"\\."),function(x)x[2])
  		  lat3[!lat3%in%"degrees_only" & sapply(tmp1,function(x)x[2])%in%c(mins,0)] = "minutes_only"
  		  long3[!long3%in%"degrees_only" & sapply(tmp2,function(x)x[2])%in%c(mins,0)] = "minutes_only"
     #Minutes
  		  mins = as.double(mins)/10^(nchar(mins))
  		  secs = as.character(apply(cbind.data.frame(as.double(rep(c(mins,0),each=60)),as.double(rep(round(1:60/3600,6),60))),1,sum))
  		  secs = sapply(strsplit(secs,"\\."),function(x)x[2])
  		  secs = secs[!is.na(secs)]
  		  lat3[!lat3%in%c("degrees_only","minutes_only") & sapply(tmp1,function(x)x[2])%in%secs] = "seconds"
  		  long3[!lat3%in%c("degrees_only","minutes_only") & sapply(tmp2,function(x)x[2])%in%secs] = "seconds"
     #Rounding issues	
  		  lat3[!lat3%in%c("degrees_only","minutes_only","seconds")] = "seconds?"
  		  long3[!long3%in%c("degrees_only","minutes_only","seconds")] = "seconds?"
     #Saving the resolution
  		  tmp = herb.data$resolution.coord[!is.na(herb.data$latitude.work)]
  		  tmp[lat3%in%"degrees_only" & long3%in%"degrees_only"] = "degrees_only"
  		  tmp[lat3%in%"minutes_only" & long3%in%"minutes_only"] = "minutes_only"
  		  tmp[lat3%in%"seconds" & long3%in%"seconds"] = "seconds"
  		  tmp[lat3%in%"degrees_only" & long3%in%"minutes_only"] = "minutes_only"		
  		  tmp[long3%in%"degrees_only" & lat3%in%"minutes_only"] = "minutes_only"
  		  tmp[lat3%in%"degrees_only" & long3%in%"seconds"] = "seconds"		
  		  tmp[long3%in%"degrees_only" & lat3%in%"seconds"] = "seconds"
  		  tmp[lat3%in%"minutes_only" & long3%in%"seconds"] = "seconds"		
  		  tmp[long3%in%"minutes_only" & lat3%in%"seconds"] = "seconds"
  		  tmp[lat3%in%"seconds" & long3%in%"seconds?"] = "seconds?"		
  		  tmp[long3%in%"seconds" & lat3%in%"seconds?"] = "seconds?"
  		  tmp[lat3%in%"seconds?" & long3%in%"seconds?"] = "seconds?"		
  		  tmp[long3%in%"seconds?" & lat3%in%"seconds?"] = "seconds?"
  		  herb.data$resolution.coord[!is.na(herb.data$latitude.work)] = tmp
     ##Removing problematic coordinates still in the mix
  		  herb.data$latitude.work[!is.na(herb.data$latitude.work)&abs(as.double(herb.data$latitude.work))>90] = NA
  		  herb.data$longitude.work[!is.na(herb.data$latitude.work)&abs(as.double(herb.data$latitude.work))>90] = NA
  		  herb.data$latitude.work[!is.na(herb.data$longitude.work)&abs(as.double(herb.data$longitude.work))>180] = NA
  		  herb.data$longitude.work[!is.na(herb.data$longitude.work)&abs(as.double(herb.data$longitude.work))>180] = NA 
		}	
	##Replacing missing coordinates by the county coordinates from the gazetter 
		herb.data$latitude.work[is.na(herb.data$latitude.work)] = herb.data$latitude.gazetteer[is.na(herb.data$latitude.work)]
		herb.data$longitude.work[is.na(herb.data$longitude.work)] = herb.data$longitude.gazetteer[is.na(herb.data$longitude.work)]
		herb.data$origin.coord[!is.na(herb.data$latitude.work)&is.na(herb.data$origin.coord)] = "coord_gazet"
		herb.data$origin.coord[is.na(herb.data$origin.coord)] = "no_coord"
		herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "country"] = "ok_country_gazet"
		herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "state"] = "ok_state_gazet"		
		herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "county"] = "ok_county_gazet"
		herb.data$origin.coord[herb.data$origin.coord %in% "coord_gazet" & herb.data$resolution.gazetteer %in% "locality"] = "ok_locality_gazet"
	##Saving the filtered data
		path.csv = gsub("cleaned-loc.csv","cleaned-loc-coord.csv",myfiles0)
		write.csv(x=herb.data,file=path.csv, row.names = FALSE)
		cat(i,"\n")
}
