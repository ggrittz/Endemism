########################################################################
#### Downloading specimens records from speciesLink, GBIF and JABOT ####
########################################################################
rm(list=ls())

##TO DO LIST
#CHECK THE WAY THE SPECIES FILTER LIST IS DEFINED (1ST REFLORA, 2ND HANS', 3RD TPL)

#### Loading packages ####
require(RCurl)
require(XML)
require(utils)
require(finch)
require(stringr)
require(devtools)
source("functions.R")
require(flora)
require(taxize)
#library(methods)
#install.packages("stringi",type="binary")
require(readxl)
require(data.table)
require(countrycode)


############################################################################################################################################################################################################-
############################################################################################################################################################################################################-
##################### 
#### speciesLINK ####
#####################

## Organizing the list of herbarium available in speciesLink ##
#Getting the weg page content
base_url <- "http://ipt1.cria.org.br/ipt"
base_html <- getURLContent(base_url)[[1]]

#Extracting the herbaria code and name
txt = strsplit(base_html,"\\\n")[[1]]
txt1 = txt[grep("ipt1.cria.org.br\\/ipt\\/resource\\?r\\=",txt)]
txt1 = strsplit(txt1,"ipt1.cria.org.br\\/ipt\\/resource\\?r\\=")
txt1 = sapply(txt1,function(x) x[2]) 
txt1 = strsplit(txt1,"\\'><if>")
herb.list = data.frame(acronym = as.character(sapply(txt1,function(x) x[1])),
					name = sapply(txt1,function(x) x[2]))
herb.list$acronym = as.vector(herb.list$acronym)
herb.list$name = gsub("</a>\",","",herb.list$name)
#number of records
txt1 = txt[grep("ipt1.cria.org.br\\/ipt\\/resource\\?r\\=",txt)+4]
txt1 = gsub("           ","",txt1); txt1 = gsub(",","",txt1); txt1 = gsub("'","",txt1)
herb.list$records = sapply(strsplit(txt1,"'><if>"), function(x) x[1])
#last publication date
txt1 = txt[grep("ipt1.cria.org.br\\/ipt\\/resource\\?r\\=",txt)+6]
txt1 = gsub("           ","",txt1); txt1 = gsub(",","",txt1); txt1 = gsub("'","",txt1)
herb.list$pub.date = sapply(strsplit(txt1,"'><if>"), function(x) x[1])
#links for data download
txt1 = txt[grep("ipt1.cria.org.br\\/ipt\\/resource\\?r\\=",txt)]
txt1 = sapply(strsplit(txt1,"href='"), function(x) x[2])
herb.list$link = sapply(strsplit(txt1,"'><if>"), function(x) x[1])
#head(herb.list); dim(herb.list)
#removing unwanted datasets
herb.list = herb.list[grep("iloteca|ung|ntomol|pólen|icológica|arpoteca|Anfibio|Aves|Mamifero|Peixe|Reptei|Abelha|Bryophyt|Criptogamos|algas|Algas|Fonoteca|ZUEC",herb.list$name,invert=TRUE),]
head(herb.list); dim(herb.list)

##Saving the network overview and details
#write.csv(herb.list,"speciesLink.csv")

#######################################
#### Dowloading the herbarium data ####
#######################################
#Creating folders in the local directory to receive the files
#path="C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//speciesLink"
#for (i in 1:length(herb.list$acronym))  { 
#	dir.create(paste(path,herb.list$acronym[i], sep="//"),showWarnings = TRUE) 
#} 

#Obtaining the links for the DwC, EML and RTF files and other metadata
#herb.list$link.dwc = NA; herb.list$link.eml = NA; herb.list$link.rtf = NA
#herb.list$version = NA
#herb.list$citation = NA; herb.list$rights = NA; herb.list$gbif_UUID = NA

for(i in 1:length(herb.list$link)) {
	herb = herb.list$link[i]
	base_html <- getURLContent(herb)[[1]]
	#Links
	txt = strsplit(base_html,"\\\n")[[1]]
	txt1 = txt[grep(">DwC-A<|>EML<|>RTF<",txt)]
	herb.list$link.dwc[i] = sapply(strsplit(txt1,'"'),function(x) x[4])[1] #dwc
	herb.list$link.eml[i] = sapply(strsplit(txt1,'"'),function(x) x[4])[2] #eml
	herb.list$link.rtf[i] = sapply(strsplit(txt1,'"'),function(x) x[4])[3] #rtf
	#Version
	tmp = sapply(strsplit(txt1,'"'),function(x) x[4])[1]
	herb.list$version[i] = strsplit(tmp,'v=')[[1]][2]
	#Citation
	txt1 = txt[grep('howtocite">',txt)]
	txt1 = sapply(strsplit(txt1,'"'),function(x) x[5])
	txt1 = gsub(">","",txt1); txt1 = gsub(" </p","",txt1)
	herb.list$citation[i] = txt1
	#Rights
	txt1 = txt[grep('rel="license',txt)]
	txt1 = sapply(strsplit(txt1,'"'),function(x) x[10])
	herb.list$rights[i] = txt1
	#GBIF UUID
	txt1 = txt[grep('GBIF UUID',txt)]
	txt1 = sapply(strsplit(txt1,'>'),function(x) x[2])
	txt1 = gsub("</a","",txt1)
	herb.list$gbif_UUID[i] = txt1
}
rm(herb,i,tmp,txt,txt1)

#Downloading the content from each herbarium page
for(i in 1:length(herb.list$link.dwc)) {
	#Darwin Core
	link.dwc = herb.list$link.dwc[i]
	path.dwc = paste("dwca-",herb.list$acronym[i],"-v",herb.list$version[i],".zip",sep="")
	path.dwc = paste(paste(path,herb.list$acronym[i],sep="//"),path.dwc,sep="//")
	download.file(link.dwc, path.dwc, mode="wb")
	#EML
	link.eml = herb.list$link.eml[i]
	path.eml = paste("eml-",herb.list$acronym[i],"-v",herb.list$version[i],".xml",sep="")
	path.eml = paste(paste(path,herb.list$acronym[i],sep="//"),path.eml,sep="//")
	download.file(link.eml, path.eml)	
	#RTF
	link.rtf = herb.list$link.rtf[i]
	path.rtf = paste("rtf-",herb.list$acronym[i],"-v",herb.list$version[i],".rtf",sep="")
	path.rtf = paste(paste(path,herb.list$acronym[i],sep="//"),path.rtf,sep="//")
	download.file(link.rtf, path.rtf)
}
#write.csv(herb.list,"speciesLink.csv")

############################################################################################################################################################################################################-
############################################################################################################################################################################################################-
#### Loading, managing and filtering the herbarium data #### - speciesLink
############################################################

#Loading previously saved herbarium data
herb.list = read.csv("speciesLink.csv", as.is=TRUE)

#South America tree species names
spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=T)

#spp = spp[grepl("^ok",spp$TreeCo_status),] ##REMOVE CHECK STATUS AFTER PRELIMINAR CHECK
spp = spp[!grepl("^remove$|remove_herb|genus|hybrid|not_found",spp$TreeCo_status),]
spp = unique(spp$SpeciesKTSA)
spp1 = gsub(" var\\. | subsp\\. | ssp\\. | f\\. | fo\\. | subf\\. | subfo\\. | subvar\\. "," ",spp)
spp = spp[!is.na(spp)]
spp = sort(unique(c(spp,spp1)))
spp = spp[!is.na(spp)]

#Finding the paths for file upload and saving
path0= "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//speciesLink"
path1 = list.files(dir(path0,full.names=TRUE),full.names=TRUE)
path1 = path1[grep("dwca-",path1)]
path = as.vector(sapply(strsplit(path1,"/"),function(x) x[22]))

#looping each collection
for(i in 1:length(path)) {
	#Loading the zip file with the darwin core file
		path.dwc = path1[i]
		dwca_cache$delete_all()
		herb.data = dwca_read(path.dwc, read=TRUE, encoding="UTF-8")$data[[1]]
	#Removing columns with non-essential information
		cols = c("id","type","modified","rightsHolder","institutionCode","collectionCode","basisOfRecord","occurrenceID","catalogNumber","occurrenceRemarks",
			"recordNumber","recordedBy","individualCount","sex","otherCatalogNumbers","eventTime","year","month","day",
			"fieldNumber","country","stateProvince","municipality","locality","minimumElevationInMeters","maximumElevationInMeters",
			"verbatimLatitude","verbatimLongitude","decimalLatitude","decimalLongitude","coordinatePrecision","identifiedBy",
			"dateIdentified","typeStatus","scientificName","kingdom","phylum","family","genus","specificEpithet","infraspecificEpithet","scientificNameAuthorship")
		#other possible fields (from an old code of my) #CROSS CHECK FIELD NAMES WITH OTHER SOURCES
			#speciesKey	taxonRank	verbatimTaxonRank	identificationQualifier	fieldNotes	eventRemarks institutionCode	eventDate	type	typeStatus	preparations	georeferenceProtocol	lat	lon	adm1	
			#cloc	locationRemarks	habitat	occurrenceRemarks	elevation	elevationAccuracy ownerInstitutionCode	publishingOrgKey	coordinateAccuracy	
			#coordinateAccuracyInMeters	georeferenceRemarks	occurrenceStatus	reproductiveCondition taxonRemarks previousIdentifications	dataGeneralizations	establishmentMeans
		herb.data1 = herb.data[,cols]
		herb.data1$datasetkey = herb.list$gbif_UUID[herb.list$acronym %in% path[i]]
	#Removing records based on human observations and others types of records: column 'basisOfRecord'
		herb.data1 = herb.data1[herb.data1$basisOfRecord %in% c("PreservedSpecimen"),]
	#Filtering the data for South American tree name list
		herb.data1 = herb.data1[herb.data1$scientificName %in% spp,]
	#Removing records other than plants
		#herb.data1 = herb.data1[herb.data1$kingdom %in% c("Plantae","plantae","PLANTAE","Planta","Plantas"),]
	#Removing taxa not identified at species level or lower
		herb.data1 = herb.data1[grep(" ",herb.data1$scientificName),]
	#Removing records of "cultivated|cultivada" specimens
		remover = "cultivated|cultivada|cultivado|cultivato|cultivad|under cultivation|plantada|plantado|planted|plantio|arboreto|arboretum|exotic|exótica|canteiro|pomar|área de visitação|cult\\.|cant\\. [a-z]|cant [A-Z]|cant\\. [0-9]|cant \\. [0-9]|cant [0-9]|\\(cult\\)|\\(cult\\ )|in cultivo|in cultis| quadra [a-z]|quadra [a-z] do|naturalised|em experimento de"
		deixar =  "pastagem cultivada|área do arboreto|área do arboreto|presença de exóticas| área cultivada| área cultivada"
		herb.data1 = herb.data1[!grepl(remover, herb.data1$occurrenceRemarks, ignore.case=TRUE) & !grepl(deixar, herb.data1$occurrenceRemarks, ignore.case=TRUE),]
		herb.data1 = herb.data1[!grepl(remover, herb.data1$locality, ignore.case=TRUE) & !grepl(deixar, herb.data1$locality, ignore.case=TRUE),]	
		
   #Some botanical gardens with many of cultivated specimens
		# "Atherton" or  "Atherton And Tyldesley Botanical Gardens" or "SFR 191, Barron"
		# "TBG": Toronto BG?
		# "Fairchild Tropical Garden"
		# "Royal Botanic Gardens" or "Royal Botanic Garden"
		# "Pazo de Castrelos" or "Parque de Castrelos" or "Castrelos"
		# "Boettcher Memorial Tropical Conservatory"
		# "Royal Botanic Gardens Melbourne" or "Royal Botanic Gardens, Melbourne" or "Melbourne Botanic Gardens"		
		# "Foster Gardens" or "Foster Botanic Garden"
		# "Coconut Grove Introduction Garden"
		# "Botanic Garden Production Facility"
		# "US Botanic Garden"
		# "Singapore Botanic Garden"
		# "SFR 191, Barron" (Australia)
		
		
	#Correcting problems with accession number
		if(unique(herb.data1$collectionCode) %in% "SPSF") herb.data1$catalogNumber = as.numeric(sapply(strsplit(herb.data1$id,":"),function(x) x[4]))
	##Editing collection names
		herb.data1$collectionCode = gsub("MBML-HERBARIO","MBML",herb.data1$collectionCode)
		herb.data1$collectionCode = gsub("^PACA-AGP","PACA",herb.data1$collectionCode)
		herb.data1$collectionCode = gsub("^NX-CRIPTOGAMOS","NX",herb.data1$collectionCode)
		#herb.data1$collectionCode = gsub("^R_HERBARIO|^R-HERBARIO|^R_TIPOS|^R-TIPOS","R",herb.data1$collectionCode)

	##Saving the edited/filtered tree records
		path.csv = paste(path[i],"-v",herb.list$version[herb.list$acronym %in% path[i]],"-edited.csv",sep="")
		path.csv = paste(paste(path0,path[i],sep="//"),path.csv,sep="//")
		write.csv(x=herb.data1,file=path.csv, row.names = FALSE)
		cat(i,"\n")
}

############################################################################################################################################################################################################-
############################################################################################################################################################################################################-
################################################ 
#### SNDB - Portal de Datos de Biodiversidad Argentina (downloaded manually) ####
################################################

#South America tree species names
spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=T)

#spp = spp[grepl("^ok",spp$TreeCo_status),] ##REMOVE CHECK STATUS AFTER PRELIMINAR CHECK
spp = spp[!grepl("^remove$|remove_herb|genus|hybrid|not_found",spp$TreeCo_status),]
spp = unique(spp$SpeciesKTSA)
spp1 = gsub(" var\\. | subsp\\. | ssp\\. | f\\. | fo\\. | subf\\. | subfo\\. | subvar\\. "," ",spp)
spp = spp[!is.na(spp)]
spp = sort(unique(c(spp,spp1)))
spp = spp[!is.na(spp)]

##Gbif herbaria information list
gbif.list = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//collectionCodes.csv", as.is = TRUE, na.strings = c(""," ",NA))

#Finding the paths for file upload and saving
path0= "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//sndb_mincyt_arg"
path1 = list.files(dir(path0,full.names=TRUE),full.names=TRUE)
path1 = path1[grep("dwca-",path1)]
path = as.vector(sapply(strsplit(path1,"/"),function(x) x[22]))

##Creating the objetc to get the missing herbarium codes in the dictionary
#missing.codes = NULL

##Looping the collections
for(i in 1:length(path)) {
  #Loading the zip file with the darwin core file
  path.dwc = path1[i]
  dwca_cache$delete_all()
  herb.data = dwca_read(path.dwc, read=TRUE, encoding="UTF-8")$data[[1]]
  
  #Replacing missing columns for some specific datasets...
  if(path[i]=="bbb") names(herb.data) = c("id","county","recordedBy","recordNumber","scientificName","collectionCode","identifiedBy","country","family","scientificNameAuthorship","decimalLatitude","kingdom","basisOfRecord","modified","stateProvince","institutionCode","locality","decimalLongitude","V19","catalogNumber")
  if(path[i]=="cenpat") names(herb.data) = c("id","family","stateProvince","country","catalogNumber","modified","scientificNameAuthorship","collectionCode","scientificName","institutionCode")

  #Removing columns with non-essential information
  cols = c("id","type","modified","license","rightsHolder","institutionCode","collectionCode","basisOfRecord","occurrenceID","catalogNumber","occurrenceRemarks",
           "recordNumber","recordedBy","individualCount","sex","otherCatalogNumbers","eventTime","year","month","day",
           "fieldNumber","country","countryCode","stateProvince","county","municipality","locality","minimumElevationInMeters","maximumElevationInMeters",
           "verbatimLatitude","verbatimLongitude","decimalLatitude","decimalLongitude","coordinatePrecision","identifiedBy",
           "dateIdentified","typeStatus","scientificName","kingdom","phylum","family","genus","specificEpithet","infraspecificEpithet","scientificNameAuthorship","taxonRank")
  #Add missing columns to the dataset
  herb.data[,cols[!cols %in% names(herb.data)]] = NA
  #Filtering the dataset
  herb.data1 = data.table(herb.data[,cols])
  #herb.data1[,catalogNumber := as.factor(catalogNumber),]
  #setkeyv(herb.data1,"catalogNumber")
  #Removing records based on human observations and others types of records: column 'basisOfRecord'
  #herb.data1 = herb.data1[herb.data1$basisOfRecord %in% c("PreservedSpecimen","PRESERVEDSPECIMEN","S"),]
  
  #Edditing species name, for collections without the species epithet field
  herb.data1[,scientificName := str_trim(gsub(" - "," ",herb.data1$scientificName)),]
  herb.data1[,scientificName := as.character(sapply(scientificName, standardize.names)),]
  herb.data1[,scientificName := as.character(sapply(scientificName, remove.authors)),]

  # if(all(is.na(herb.data1$specificEpithet)) & !all(is.na(herb.data1$taxonRank))) {
  #   herb.data1$scientificName[herb.data1$taxonRank %in% c("species","SPECIES","sp.")] =
  #     as.character(sapply(strsplit(herb.data1$scientificName[herb.data1$taxonRank %in% c("species","SPECIES","sp.")]," "), function (x) paste(x[1],x[2],sep=" ")))
  #   infra = c("form","FORM","SUBSPECIES","subspecies","VARIETY","variety","subsp.","var.","f.")
  #   herb.data1$scientificName[herb.data1$taxonRank %in% infra] =
  #     as.character(sapply(strsplit(herb.data1$scientificName[herb.data1$taxonRank %in% infra]," "), function (x) paste(x[1],x[2],x[which(x %in% infra)],x[which(x %in% infra)+1],sep=" ")))
  #   herb.data1$scientificName[herb.data1$taxonRank %in% c("family","FAMILY")] = herb.data1$family[herb.data1$taxonRank %in% c("family","FAMILY")]
  #   herb.data1$scientificName[herb.data1$taxonRank %in% c("genus","GENUS")] = herb.data1$genus[herb.data1$taxonRank %in% c("genus","GENUS")]
  #   herb.data1$scientificName[herb.data1$taxonRank %in% c("kingdom","KINGDOM")] = herb.data1$kingdom[herb.data1$taxonRank %in% c("kingdom","KINGDOM")]
  # }
  # if(any(str_count(herb.data1$scientificName," ")>3)) { # more info than only species
  #   herb.data1$scientificName[herb.data1$taxonRank %in% c("species","SPECIES","sp.","")] =
  #     as.character(sapply(strsplit(herb.data1$scientificName[herb.data1$taxonRank %in% c("species","SPECIES","sp.","")]," "), function (x) paste(x[1],x[2],sep=" ")))
  #   infra = c("form","FORM","SUBSPECIES","subspecies","VARIETY","variety","subsp.","var.","f.")
  #   herb.data1$scientificName[herb.data1$taxonRank %in% infra] =
  #     as.character(sapply(strsplit(herb.data1$scientificName[herb.data1$taxonRank %in% infra]," "), function (x) paste(x[1],x[2],x[which(x %in% infra)],x[which(x %in% infra)+1],sep=" ")))
  #   herb.data1$scientificName[herb.data1$taxonRank %in% c("family","FAMILY")] = herb.data1$family[herb.data1$taxonRank %in% c("family","FAMILY")]
  #   herb.data1$scientificName[herb.data1$taxonRank %in% c("genus","GENUS")] = herb.data1$genus[herb.data1$taxonRank %in% c("genus","GENUS")]
  #   herb.data1$scientificName[herb.data1$taxonRank %in% c("kingdom","KINGDOM")] = herb.data1$kingdom[herb.data1$taxonRank %in% c("kingdom","KINGDOM")]
  #   herb.data1$scientificName[grepl(" NA$",herb.data1$scientificName)] = as.character(sapply(strsplit(herb.data1$scientificName[grepl(" NA$",herb.data1$scientificName)]," NA$"), function (x) x[1]))
  # }  
  # herb.data1$scientificName1 = herb.data1$scientificName
  # if(!all(is.na(herb.data1$specificEpithet))) {
  #   herb.data1$scientificName1 = as.character(paste(herb.data1$genus,herb.data1$specificEpithet,sep=" "))
  #   herb.data1$scientificName1[!is.na(herb.data1$infraspecificEpithet)&!as.character(herb.data1$infraspecificEpithet) %in% c(""," ")] = 
  #     as.character(paste(herb.data1[!is.na(herb.data1$infraspecificEpithet)&!as.character(herb.data1$infraspecificEpithet) %in% c(""," "),]$genus, herb.data1[!is.na(herb.data1$infraspecificEpithet)&!as.character(herb.data1$infraspecificEpithet) %in% c(""," "),]$specificEpithet, herb.data1[!is.na(herb.data1$infraspecificEpithet)&!as.character(herb.data1$infraspecificEpithet) %in% c(""," "),]$infraspecificEpithet,sep=" "))
  #   herb.data1$scientificName1[is.na(herb.data1$specificEpithet)|as.character(herb.data1$specificEpithet) %in% c(""," "," sp.","sp.")] = as.character(herb.data1[is.na(herb.data1$specificEpithet)|as.character(herb.data1$specificEpithet) %in% c(""," ", " sp.","sp."),]$genus)
  #   herb.data1$scientificName1[is.na(herb.data1$genus)|as.character(herb.data1$genus) %in% c(""," "," sp.","sp.")] = "Indet. sp."
  #   herb.data1$scientificName1 = str_trim(herb.data1$scientificName1)
  #   herb.data1$scientificName1[!herb.data1$scientificName %in% c(""," ") & herb.data1$scientificName1 %in% c(""," ","Indet. sp.")] = 
  #     herb.data1$scientificName[!herb.data1$scientificName %in% c(""," ") & herb.data1$scientificName1 %in% c(""," ","Indet. sp.")]
  #   herb.data1$scientificName1 = as.character(herb.data1$scientificName1)
  # }
  #Filtering the data for South American tree name list
  herb.data1[,scientificName := str_trim(gsub("  "," ",scientificName)),]
  herb.data1 = as.data.frame(herb.data1[herb.data1$scientificName %in% spp,])

  #Removing records other than plants
   #herb.data1 = herb.data1[herb.data1$kingdom %in% c("Plantae","plantae","PLANTAE","Planta","Plantas"),]
  #Removing taxa not identified at species level or lower
   herb.data1 = herb.data1[grepl(" ",herb.data1$scientificName),]
  #Removing records of "cultivated|cultivada" specimens
   remover = "cultivated|cultivada|cultivado|cultivato|cultivad|under cultivation|plantada|plantado|planted|plantio|arboreto|arboretum|exotic|exótica|canteiro|pomar|área de visitação|cult\\.|cant\\. [a-z]|cant [A-Z]|cant\\. [0-9]|cant \\. [0-9]|cant [0-9]|\\(cult\\)|\\(cult\\ )|in cultivo|in cultis| quadra [a-z]|quadra [a-z] do|naturalised|em experimento de"
   deixar =  "pastagem cultivada|área do arboreto|área do arboreto|presença de exóticas| área cultivada| área cultivada"
   herb.data1 = herb.data1[!grepl(remover, herb.data1$occurrenceRemarks, ignore.case=TRUE) & !grepl(deixar, herb.data1$occurrenceRemarks, ignore.case=TRUE),]
   herb.data1 = herb.data1[!grepl(remover, herb.data1$locality, ignore.case=TRUE) & !grepl(deixar, herb.data1$locality, ignore.case=TRUE),]	
  #Correcting problems with accession number
   #if(unique(herb.data1$collectionCode) %in% "SPSF") herb.data1$catalogNumber = as.numeric(sapply(strsplit(herb.data1$id,":"),function(x) x[4]))
  ##Editing collection names
   #herb.data1$collectionCode = gsub("MBML-HERBARIO","PACA",herb.data1$collectionCode)
   #herb.data1$collectionCode = gsub("^PACA-AGP","PACA",herb.data1$collectionCode)

  ##Getting the valid collection code from the Index Herbariorum or a working collection code for names if the code was not in IH
   tmp2 = herb.data1[,c("collectionCode","institutionCode","rightsHolder")]
   tmp2$order = 1:dim(tmp2)[1]
   tmp2$collection.string = str_trim(paste(tmp2$collectionCode,tmp2$institutionCode,sep="_"))
    # if(is.null(missing.codes)) {
    #  miss = tmp2[!tmp2$collection.string %in% gbif.list$collection.string, c("collection.string","collectionCode","institutionCode","rightsHolder")] 
    #  missing.codes = miss[!duplicated(miss$collection.string),]
    # } else {
    #  miss = tmp2[!tmp2$collection.string %in% gbif.list$collection.string, c("collection.string","collectionCode","institutionCode","rightsHolder")]
    #  missing.codes = rbind.data.frame(missing.codes, miss[!duplicated(miss$collection.string),])
    # }  
   tmp2 = merge(tmp2, gbif.list[,c("collection.string","index.herbariorum.or.working.code")], by="collection.string", all.x = TRUE)
   tmp2 = tmp2[order(tmp2$order),]
   herb.data1$collectionCode = as.character(tmp2$index.herbariorum.or.working.code)
   if(path[i]=="si") herb.data1$collectionCode = "SI"
  
  ##Replacing locality fields: 'country' by 'countryCode'
   herb.data1$country[is.na(herb.data1$country)] = countrycode(as.character(herb.data1$countryCode[is.na(herb.data1$country)]), 'iso2c', 'country.name')
  ##Replacing/removing locality fields: 'county' by 'municipality'
   herb.data1$county[is.na(herb.data1$county)|herb.data1$county %in% c("","-")] = 
     herb.data1$municipality[is.na(herb.data1$county)|herb.data1$county %in% c("","-")]
   herb.data1 = herb.data1[,-which(names(herb.data1)=="municipality")]
   
  ##Saving the edited/filtered tree records
   nome = gsub("dwca-|\\.zip$","",tail(strsplit(path.dwc,"/")[[1]],1))
   path.csv = paste(nome,"-edited.csv",sep="")
   path.csv = paste(paste(path0,path[i],sep="//"),path.csv,sep="//")
   write.csv(x=herb.data1,file=path.csv, row.names = FALSE)
   cat(i,"\n")
}
#missing.codes

############################################################################################################################################################################################################-
############################################################################################################################################################################################################-
################################################ 
#### speciesLINK - data downloaded manually ####
################################################

## Organizing the list of herbarium available in speciesLink ##
#Folders were created manually and data was downloaded into folders manually as well
path="C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//speciesLink_new"
path1 = dir(path,full.names=TRUE)
txt1 = dir(path,full.names=FALSE)

#herbarium names
txt2 = NULL
for (i in 1:length(path1)) {
  txt2[i] = gsub(".txt","",list.files(path1[i], pattern="\\.txt"))
}

## Extracting the herbaria code and name
herb.list = data.frame(acronym = as.character(txt1),
                       name = as.character(gsub(".txt","",txt2)))
herb.list$acronym = as.vector(herb.list$acronym)

#number of records
herb.list$records = NA
#download date
herb.list$pub.date = "2019-05-20"
#links for data download
herb.list$link = "http://www.splink.org.br"
#Obtaining the links for the DwC, EML and RTF files and other metadata
herb.list$link.dwc = NA; herb.list$link.eml = NA; herb.list$link.rtf = NA
herb.list$version = NA
herb.list$citation = NA; herb.list$rights = NA; herb.list$gbif_UUID = NA
#write.csv(herb.list,"speciesLink_new.csv")

############################################################################################################################################################################################################-
############################################################################################################################################################################################################-
#### Loading, managing and filtering the herbarium data #### speciesLink (data downloaded manually)
############################################################ - speciesLink (data downloaded manually)

#Loading previously saved herbarium data
herb.list = read.csv("speciesLink_new.csv", as.is=TRUE)

#South America tree species names
spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=T)

#spp = spp[grepl("^ok",spp$TreeCo_status),] ##REMOVE CHECK STATUS AFTER PRELIMINAR CHECK
spp = spp[!grepl("^remove$|remove_herb|genus|hybrid|not_found",spp$TreeCo_status),]
spp = unique(spp$SpeciesKTSA)
spp1 = gsub(" var\\. | subsp\\. | ssp\\. | f\\. | fo\\. | subf\\. | subfo\\. | subvar\\. "," ",spp)
spp = spp[!is.na(spp)]
spp = sort(unique(c(spp,spp1)))
spp = spp[!is.na(spp)]

#Finding the paths for file upload and saving
path0= "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//speciesLink_new"
path1 = list.files(dir(path0,full.names=TRUE),full.names=TRUE, pattern = ".zip")
path = as.vector(sapply(strsplit(path1,"/"),function(x) x[23]))

#unziping files
#for(i in 1:length(path1)) {
#  #Loading the zip file and unziping
#  path.zip = path1[i]
#  outDir = dir(path0,full.names=TRUE)[i]
#  unzip(path.zip, exdir= outDir)
#}  

#reading and editing xlsx files
path2 = list.files(dir(path0,full.names=TRUE),full.names=TRUE, pattern = ".xlsx$")
path3 = dir(path0,full.names=FALSE)

for(i in 1:length(path2)) {
  path.xls = path2[i]
  herb.data = read_excel(path.xls, na= c(""," ",NA))
  herb.list$records[herb.list$acronym %in% path3[i]] = dim(herb.data)[1]
  #Removing columns with non-essential information
  cols = c("datelastmodified","institutioncode","collectioncode","basisofrecord","catalognumber","notes","collectornumber",
           "collector","individualcount","sex","previouscatalognumber","timeofday","yearcollected","monthcollected","daycollected",
           "fieldnumber","country","stateprovince","county","locality","minimumelevation","maximumelevation","latitude_mun","longitude_mun",
           "latitude","longitude","coordinateprecision","identifiedby","yearidentified","typestatus",
           "scientificname","kingdom","phylum","family","genus","species","subspecies","scientificnameauthor")
  #other possible fields (from an old code of my) #CROSS CHECK FIELD NAMES WITH OTHER SOURCES
  #speciesKey	taxonRank	verbatimTaxonRank	identificationQualifier	fieldNotes	eventRemarks institutionCode	eventDate	type	typeStatus	preparations	georeferenceProtocol	lat	lon	adm1	
  #cloc	locationRemarks	habitat	occurrenceRemarks	elevation	elevationAccuracy ownerInstitutionCode	publishingOrgKey	coordinateAccuracy	
  #coordinateAccuracyInMeters	georeferenceRemarks	occurrenceStatus	reproductiveCondition taxonRemarks previousIdentifications	dataGeneralizations	establishmentMeans
  herb.data1 = herb.data[,cols]
  herb.data1$datasetkey = NA
  #Removing records based on human observations and others types of records: column 'basisOfRecord'
  herb.data1 = herb.data1[herb.data1$basisofrecord %in% c("S","preservedspecimen","PreservedSpecimen"),]
  #Filtering the data for South American tree name list
  herb.data1 = herb.data1[herb.data1$scientificname %in% spp,]
  #Removing records other than plants
  #herb.data1 = herb.data1[herb.data1$kingdom %in% c("Plantae","plantae","PLANTAE","Planta","Plantas"),]
  #Removing taxa not identified at species level or lower
  herb.data1 = herb.data1[grepl(" ",herb.data1$scientificname),]
  #Removing records of "cultivated|cultivada" specimens
  remover = "cultivated|cultivada|cultivado|cultivato|cultivad|under cultivation|plantada|plantado|planted|plantio|arboreto|arboretum|exotic|exótica|canteiro|pomar|área de visitação|cult\\.|cant\\. [a-z]|cant [A-Z]|cant\\. [0-9]|cant \\. [0-9]|cant [0-9]|\\(cult\\)|\\(cult\\ )|in cultivo|in cultis| quadra [a-z]|quadra [a-z] do|naturalised|em experimento de"
  deixar =  "pastagem cultivada|área do arboreto|área do arboreto|presença de exóticas| área cultivada| área cultivada"
  herb.data1 = herb.data1[!grepl(remover, herb.data1$notes, ignore.case=TRUE) & !grepl(deixar, herb.data1$notes, ignore.case=TRUE),]
  herb.data1 = herb.data1[!grepl(remover, herb.data1$locality, ignore.case=TRUE) & !grepl(deixar, herb.data1$locality, ignore.case=TRUE),]	
  ##Editing encoding problems
  herb.data1$stateprovince = as.character(iconv(herb.data1$stateprovince, from="UTF-8", to="windows-1252//TRANSLIT"))
  herb.data1$stateprovince = gsub("Ã\u00AD|Ã\u00A0","í",herb.data1$stateprovince)
  herb.data1$county = as.character(iconv(herb.data1$county, from="UTF-8", to="windows-1252//TRANSLIT"))
  herb.data1$county = gsub("Ã\u00AD|Ã\u00A0","í",herb.data1$county)
  herb.data1$locality = as.character(iconv(herb.data1$locality, from="UTF-8", to="windows-1252//TRANSLIT"))
  herb.data1$locality = gsub("Ã\u00AD|Ã\u00A0","í",herb.data1$locality)
  herb.data1$collector = as.character(iconv(herb.data1$collector, from="UTF-8", to="windows-1252//TRANSLIT"))
  herb.data1$collector = gsub("Ã\u00AD|Ã\u00A0","í",herb.data1$collector)
  herb.data1$identifiedby = as.character(iconv(herb.data1$identifiedby, from="UTF-8", to="windows-1252//TRANSLIT"))
  herb.data1$identifiedby = gsub("Ã\u00AD|Ã\u00A0","í",herb.data1$identifiedby)
  ##Editing collection names
  herb.data1$collectioncode = gsub("^ASU-Plants","ASU",herb.data1$collectioncode)
  herb.data1$collectioncode = gsub("^F_BOTANY_BR|^FALSE$","F",herb.data1$collectioncode)
  herb.data1$collectioncode = gsub("^G-DC","G",herb.data1$collectioncode)
  herb.data1$collectioncode = gsub("^MOBOT_BR","MO",herb.data1$collectioncode)
  herb.data1$collectioncode = gsub("^NHM-LONDON-BOT","BM",herb.data1$collectioncode)
  herb.data1$collectioncode = gsub("^MNHN_P_PC_BR","P",herb.data1$collectioncode)
  herb.data1$collectioncode = gsub("^NYBG_BR","NY",herb.data1$collectioncode)
  herb.data1$collectioncode = gsub("^HPUC-MG","HPUC",herb.data1$collectioncode)
  ##Saving the edited/filtered tree records
  path.csv = paste(path3[i],"-edited.csv",sep="")
  path.csv = paste(paste(path0,path3[i],sep="//"),path.csv,sep="//")
  write.csv(x=herb.data1,file=path.csv, row.names = FALSE)
  cat(i,"\n")
}
#write.csv(herb.list,"speciesLink_new.csv")

############################################################################################################################################################################################################-
############################################################################################################################################################################################################-
############################################################################################################################################################################################################-
############### 
#### JABOT ####
###############
#require(RJabot)

## Organizing the list of herbarium available in Jabot ##
#Getting the weg page content
base_url <- "http://jabot.jbrj.gov.br/v2/herbariosparceiros.php"
base_html <- getURLContent(base_url)[[1]]

#Extracting the herbaria code and name
txt = strsplit(base_html,"\\\n")[[1]]
txt1 = txt[grep('.jbrj.gov.br"><img',txt)]
txt1 = strsplit(txt1,'"')
jabot.list = cbind.data.frame(acronym = as.vector(sapply(txt1,function(x) x[2])),
                              name = as.character(sapply(txt1,function(x) x[10])),
                              link = as.character(sapply(txt1,function(x) x[2])), stringsAsFactors = FALSE)
jabot.list$acronym = gsub('http://|.jbrj.gov.br',"",jabot.list$acronym)
jabot.list$download.date = as.vector(strsplit(as.character(Sys.time())," ")[[1]][1])
jabot.list$name = as.character(iconv(jabot.list$name, from="UTF-8", to="windows-1252//TRANSLIT"))
#removing unwanted datasets
jabot.list = jabot.list[grep("edicinais|Burle Marx",jabot.list$name,invert=TRUE),]
jabot.list$acronym = toupper(jabot.list$acronym)
head(jabot.list); dim(jabot.list)

##Saving the network overview and details
write.csv(jabot.list,"jabot.list.csv")

############################################################################################################################################################################################################
## Dowloading the herbarium data ##
#Creating folders in the local directory to receive the files
#path="C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot"
#for (i in 1:length(jabot.list$acronym))  { 
#	dir.create(paste(path,jabot.list$acronym[i], sep="//"),showWarnings = TRUE) 
#} 

#Obtaining the links for the occurrences
#jabot.list$link.data = NA

############################################################################################################################################################################################################-
## Loading, menaging and filtering the herbarium data ##

##Reading JABOT herbaria information
jabot.list = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//jabot.list.csv", as.is=TRUE)

#South America tree species names
spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=T)
#spp = spp[grepl("^ok",spp$TreeCo_status),] ##REMOVE CHECK STATUS AFTER PRELIMINAR CHECK
spp = spp[!grepl("^remove$|remove_herb|genus|hybrid|not_found",spp$TreeCo_status),]
spp = spp[!spp$DataSource %in% "TreeCo common typos",]
spp = unique(spp$SpeciesKTSA)
spp1 = gsub(" var\\. | subsp\\. | ssp\\. | f\\. | fo\\. | subf\\. | subfo\\. | subvar\\. "," ",spp)
spp = spp[!is.na(spp)]
spp = sort(unique(c(spp,spp1)))
spp = spp[!is.na(spp)]

#Loading, merging and filtering the herbarium info from JABOT 
path = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot"
myfiles = list.files(path, full.names = TRUE)

#Looping for each collection
for(i in 1:length(myfiles)) {
  #Loading the csv file with the data
  myfiles0 = list.files(myfiles[i], full.names = TRUE)
  myfiles0 = myfiles0[grep("-edited",myfiles0,invert=TRUE)]
  myfiles0 = myfiles0[grep(".csv",myfiles0)]
  lst = vector("list",length(myfiles0))
  if(length(myfiles0)==0) { next } else {
    for (j in 1:length(lst)) lst[[j]] = read.csv(myfiles0[j], sep=";")
    herb = do.call(rbind,lst)
    herb = droplevels(herb)
  }
  #Removing columns with non-essential information
  cls = c("datelastmodified","basisofrecord","fieldnumber","longitude_mun","latitude_mun","coordinateprecision",
          "minimumelevation","maximumelevation","sex","individualcount","habitat","habito","descrindividuo","aux_coladic","altprof",
          "unidmedaltprof","codcolbot","aux_nomecompunidgeo_invertido","codespecime","numtombo","familia","genero","sp","autorsp",
          "infrasp","nat_typus","taxoncompleto","pais","estado_prov","cidade","descrlocal","latitude","longitude",
          "coletor","numcoleta","diacoleta","mescoleta","anocoleta","determinador","diadeterm","mesdeterm","anodeterm","notas",
          "codtestemunho","siglacolecao","precisao_coord","dataultalter","tipoespecime","altitude","siglaunidmed",                 
          "altura","unidmedaltura","duplicatas","detpor","datacoleta")
  herb1 = herb[,cls]
  #Removing records based on human observations and others types of records: column 'basisOfRecord'
  herb1 = herb1[as.character(herb1$tipoespecime) %in% c("Exsicata de planta","Esicata de planta","Exsicata"),]
  #Standarding taxonomy notation
  herb1$familia = as.vector(sapply(as.character(herb1$familia), simpleCap))
  herb1$genero = as.character(gsub(" Sp\\.","",herb1$genero))
  #Creating the working species name
  herb1$scientificName = as.character(paste(herb1$genero,herb1$sp,sep=" "))
  herb1$scientificName[!is.na(herb1$infrasp)&!as.character(herb1$infrasp) %in% ""] = 
    as.character(paste(herb1[!is.na(herb1$infrasp)&!as.character(herb1$infrasp) %in% "",]$genero,herb1[!is.na(herb1$infrasp)&!as.character(herb1$infrasp) %in% "",]$sp,herb1[!is.na(herb1$infrasp)&!as.character(herb1$infrasp) %in% "",]$infrasp,sep=" "))
  herb1$scientificName[is.na(herb1$sp)|as.character(herb1$sp) %in% ""] = as.character(herb1[is.na(herb1$sp)|as.character(herb1$sp) %in% "",]$genero)
  herb1$scientificName[is.na(herb1$genero)|as.character(herb1$genero) %in% ""] = "Indet. sp."
  herb1$scientificName = str_trim(herb1$scientificName)
  #sort(unique(herb1$scientificName))
  
  #Filtering the data for South American tree name list		
  herb1 = herb1[herb1$scientificName %in% spp,]
  #Removing possible duplicates due to the download  procedure from RB (family by family)
  herb1 = herb1[!duplicated(herb1$numtombo)&!duplicated(herb1$codtestemunho),]
  #Removing records other than plants
  #herb1 = herb1[herb1$kingdom %in% c("Plantae","plantae","PLANTAE","Planta","Plantas"),]
  #Removing taxa not identified at species level or lower
  herb1 = herb1[grep(" ",herb1$scientificName),]
  #Removing records of "cultivated|cultivada" specimens
  remover = "cultivated|cultivada|cultivado|cultivato|cultivad|under cultivation|plantada|plantado|planted|plantio|arboreto|arboretum|exotic|exótica|canteiro|pomar|área de visitação|cult\\.|cant\\. [a-z]|cant [A-Z]|cant\\. [0-9]|cant \\. [0-9]|cant [0-9]|\\(cult\\)|\\(cult\\ )|in cultivo|in cultis| quadra [a-z]|quadra [a-z] do|naturalised|em experimento de"
  deixar =  "pastagem cultivada|área do arboreto|área do arboreto|presença de exóticas| área cultivada| área cultivada"
  herb1 = herb1[!grepl(remover, herb1$habitat, ignore.case=TRUE) & !grepl(deixar, herb1$habitat, ignore.case=TRUE),]
  herb1 = herb1[!grepl(remover, herb1$descrindividuo, ignore.case=TRUE) & !grepl(deixar, herb1$descrindividuo, ignore.case=TRUE),]	
  herb1 = herb1[!grepl(remover, herb1$notas, ignore.case=TRUE) & !grepl(deixar, herb1$notas, ignore.case=TRUE),]
  herb1 = herb1[!grepl(remover, herb1$descrlocal, ignore.case=TRUE) & !grepl(deixar, herb1$descrlocal, ignore.case=TRUE),]
  ##Editing encoding problems
  herb1$estado_prov = as.character(iconv(herb1$estado_prov, from="UTF-8", to="windows-1252//TRANSLIT"))
  herb1$cidade = as.character(iconv(herb1$cidade, from="UTF-8", to="windows-1252//TRANSLIT"))
  herb1$descrlocal = as.character(iconv(herb1$descrlocal, from="UTF-8", to="windows-1252//TRANSLIT"))
  herb1$coletor = as.character(iconv(herb1$coletor, from="UTF-8", to="windows-1252//TRANSLIT"))
  herb1$determinador = as.character(iconv(herb1$determinador, from="UTF-8", to="windows-1252//TRANSLIT"))
  ##Creating the missing institution code for JABOT
  tmp = herb1[!duplicated(herb1$siglacolecao),]
  tmp1= merge(tmp,jabot.list,by.x="siglacolecao",by.y="acronym")
  herb1$intituto = paste(unique(tmp1$name),collapse="|")
  ##Saving the edited/filtered tree records
  acr = tail(strsplit(myfiles[i],'/')[[1]],1)
  path.csv = paste(myfiles[i],"/",acr,"-edited.csv",sep="")
  write.csv(x=herb1,file=path.csv, row.names = FALSE)
  cat(i,"\n")
}


############################################################################################################################################################################################################-
############################################################################################################################################################################################################-
############################################################################################################################################################################################################-
##############
#### GBIF ####
##############

##Checking for possible problems with filtering Gbif database
#frequency of occurrences found in gbif
tmp = read.table("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif//gbif_treeSA_stat.txt", h=F, sep=" ")
#list of names not found in gbif
tmp1 = read.table("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif//gbif_treeSA_absent.txt", h=F, sep="\t")

tmp2 = as.character(tmp1$V1)
for(i in 1:length(tmp2)) { # checking which names do not have occurrences in gbif indeed
  esp = strsplit(as.character(tmp1$V1[i])," ")[[1]]
  if(length(esp)==2) 	{ tmp2[i] = gbif(esp[1], esp[2], down=FALSE) } else 
  { tmp2[i] = gbif(esp[1], paste(esp[2],esp[3],sep=" "), down=FALSE)} 
}
tmp3 = cbind.data.frame(speciesName=as.character(tmp1$V1),gbif_records=tmp2, stringsAsFactors = FALSE)
tmp3$rank = as.factor(sapply(strsplit(tmp3$speciesName," "),length)) 
levels(tmp3$rank) = c("genus","species","var/subsp","subvar","error","error")
tmp3$rank = as.character(tmp3$rank) 
tmp4 = tmp3[tmp3$gbif_records==0,] # species in the entry list without records at all
tmp5 = tmp3[tmp3$gbif_records>0,] # species with records (filtering problems)
table(tmp4$rank);table(tmp5$rank)
head(tmp4[tmp4$rank=="species",])
head(tmp5[tmp5$rank=="species",])

#Finding possible synonyms, ortographical variants or errors in Gbif
tmp3$canonicalName = NA
tmp3$species = NA
for(i in 1:length(tmp3$speciesName)){
  tmp = name_backbone(name = tmp3$speciesName[i])
  if(is.null(tmp$canonicalName)) { next } else { 
    tmp3$canonicalName[i] = try(tmp$canonicalName,TRUE)
    if(is.null(tmp$species)) { next } else { tmp3$species[i] = try(tmp$species,silent=TRUE) }
  }
  cat(i,"\n")
}

##Input name (THREAT) and canonical name (GBIF) are the same?
tmp3$check = tmp3$canonicalName == tmp3$speciesName
##Canonical and species names (both from gibif) are the same?
tmp3$check1 = tmp3$canonicalName == tmp3$species

##Defining problems
tmp3$status = NA
tmp3[!is.na(tmp3$canonicalName)&tmp3$check==FALSE&tmp3$check1%in%NA,]$status = "name not in GBIF database"
tmp3[is.na(tmp3$status)&tmp3$check%in%NA&tmp3$check1%in%NA,]$status = "name not in GBIF database"
tmp3[is.na(tmp3$status)&!is.na(tmp3$check1)&tmp3$check==FALSE&tmp3$check1==TRUE,]$status = "ortogaphical variant"
tmp3[is.na(tmp3$status)&!is.na(tmp3$check1)&tmp3$check==TRUE&tmp3$check1==FALSE&tmp3$rank%in%"var/subsp",]$status = "variety/subspecies"
tmp3[is.na(tmp3$status)&!is.na(tmp3$check1)&tmp3$check==TRUE&tmp3$check1==FALSE&tmp3$rank%in%"species",]$status = "synonym_gbif"
tmp3[is.na(tmp3$status)&!is.na(tmp3$check1)&tmp3$check==TRUE&tmp3$check1==TRUE&tmp3$gbif_records%in%0,]$status = "zero records in GBIF"
tmp3[is.na(tmp3$status)&!is.na(tmp3$check1)&tmp3$check==TRUE&tmp3$check1==TRUE&!tmp3$gbif_records%in%0,]$status = "doubtful or filtering problem?"
tmp3[is.na(tmp3$status)&!is.na(tmp3$check1)&tmp3$check==FALSE&tmp3$check1==FALSE&tmp3$gbif_records%in%0&tmp3$rank%in%"var/subsp",]$status = "name not in GBIF database"
tmp3[is.na(tmp3$status)&!is.na(tmp3$check1)&tmp3$check==FALSE&tmp3$check1==FALSE&tmp3$gbif_records%in%0&tmp3$rank%in%"species",]$status = "ortogaphical variant"
tmp3[is.na(tmp3$status),]$status = "error"
table(tmp3$status)

## Check spelling errors with ReFlora
sp = unique(tmp3[tmp3$status %in% c("ortogaphical variant","synonym_gbif"),]$canonicalName)
nomes = get.taxa(sp, replace.synonyms = TRUE, life.form = FALSE, habitat = FALSE, states = FALSE, establishment = FALSE,
                 suggestion.distance = 0.92, drop = c("authorship"))
#assigning the information of each name on the list to the names on the original database
names(nomes)[grep("original.search",names(nomes))] = "canonicalName"
tmp3$ordem = 1:dim(tmp3)[1]
nomes.join = left_join(x= tmp3, y= nomes, by="canonicalName")

#Once again...
sp = unique(tmp3[!tmp3$rank %in% "var/subsp" & tmp3$status %in% c("ortogaphical variant","synonym_gbif"),]$species)
nomes = get.taxa(sp, replace.synonyms = TRUE, life.form = FALSE, habitat = FALSE, states = FALSE, establishment = FALSE,
                 suggestion.distance = 0.92, drop = c("authorship"))
names(nomes)[grep("original.search",names(nomes))] = "species"
nomes.join1 = left_join(x= nomes.join, y= nomes, by="species")

## ordering the output in the same order of the input dataframe
nomes.join1 = nomes.join1[order(nomes.join1$ordem),]
write.csv(nomes.join1,"tmp2.csv") 
# OK - CHECAGEM DE PROBLEMAS COM GBIF DATA JÁ REALIZADA POR RENATO EM 07/09/2018

############################################################################################################################################################################################################
#### South America tree species names for filtering in Gbif ####
spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=T)
#spp = spp[grepl("^ok",spp$TreeCo_status),] ##REMOVE CHECK STATUS AFTER PRELIMINAR CHECK
#spp = spp[grepl("^ok|^check",spp$TreeCo_status),] ##REMOVE CHECK STATUS AFTER PRELIMINAR CHECK
spp = spp[!grepl("^remove$|remove_herb|genus|hybrid|not_found",spp$TreeCo_status),]
spp = spp[!spp$DataSource %in% "TreeCo common typos",]
spp = unique(spp$SpeciesKTSA)
spp1 = gsub(" var\\. | subsp\\. | ssp\\. | f\\. | fo\\. | subf\\. | subfo\\. | subvar\\. "," ",spp)
spp = spp[!is.na(spp)]
spp = sort(unique(c(spp,spp1)))
spp = spp[!is.na(spp)]
#write.table(data.frame(speciesName=spp),"treesSA.txt", sep="\t")

##List of collections to have data excluded from gbif
#herb.list = read.csv("specieslink.csv",as.is=TRUE)
#herb.list1 = read.csv("specieslink_new.csv",as.is=TRUE)
#herb1 = read.csv("jabot.csv")
#herbs = toupper(c(herb.list$acronym,herb.list1$acronym,unique(as.character(herb1$siglacolecao))))
#herbs = sort(unique(c(herbs,gsub("_FANEROGAMAS|_HERBARIO|-HERBARIO|-AGP","",herbs))))
#write.table(data.frame(collectioncode=herbs),"herbaria.txt", sep="\t")

############################################################################################################################################################################################################
## Loading the filtered gbif occurrences (double checked filtering from all gbif data by Lucie Zinger using Unix)
#could not do the filtering in R - file is too big!

##Gbif herbaria information list
gbif.list = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//collectionCodes.csv", as.is = TRUE, na.strings = c(""," ",NA))

##Loading, merging and filtering the herbarium info from JABOT 
path = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif"
myfiles = list.files(path, full.names = TRUE)
myfiles = myfiles[grep("gbif_treeSA_filtered-",myfiles)]
myfiles = myfiles[grep("-edited",myfiles,invert=TRUE)]

##Creating the objetc to get the missing herbarium codes in the dictionary
#missing.codes = NULL

#### Editing GBIF data ####
for(i in 1:length(myfiles)) {
  #Loading gbif data for each pack
    myfiles0 = myfiles[i]
    gbif.data = read.csv(myfiles0,as.is=TRUE,header=TRUE,na.strings=c(""," ","NA"),encoding="UTF-8")
  #Removing columns with non-essential information
	cols = c("gbifID","datasetKey","phylum","family","genus","species","infraspecificEpithet","taxonRank","taxonomicStatus",
		"countryCode","stateProvince","county","municipality","locality",
		"decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters",
		"day","month","year","basisOfRecord","institutionCode","collectionCode","catalogNumber","recordNumber",
		"identifiedBy","dateIdentified","license","rightsHolder",
		"recordedBy","typeStatus","issue","eventDate",   
		"occurrenceRemarks","habitat","elevation","fieldNotes",
		"genericName","specificEpithet","acceptedScientificName") 
    gbif.data = gbif.data[,cols]
    names(gbif.data) = c("gbifid","datasetkey","phylum","family","genus","species","infraspecificepithet","taxonrank","taxonomicStatus",
                         "countrycode","stateProvince","county","municipality","locality",
					 "decimallatitude","decimallongitude","coordinateUncertaintyInMeters",
                         "day","month","year","basisofrecord","institutioncode","collectioncode","catalognumber","recordnumber",
                         "identifiedby","dateIdentified","license","rightsholder",
					 "recordedby","typestatus","issue","eventDate","occurrenceRemarks",
					 "habitat","elevation","fieldNotes","genericName","specificEpithet","acceptedScientificName")
  #Removing records based on human observations and others types of records: column 'basisOfRecord'
    #gbif.data = gbif.data[gbif.data$basisofrecord %in% c("PreservedSpecimen","PRESERVED_SPECIMEN"),]
  #Removing records based on human observations and others types of records: column 'basisOfRecord'
    #gbif.data = gbif.data[gbif.data$phylum %in% c("Tracheophyta"),]
  #Creating the working species name
    gbif.data$scientificName = NA
    gbif.data$scientificName[gbif.data$taxonrank%in%"SPECIES"] = gbif.data$species[gbif.data$taxonrank%in%"SPECIES"]
    #gbif.data[gbif.data$taxonrank%in%"GENUS",]$scientificName = gbif.data[gbif.data$taxonrank%in%"GENUS",]$genus
    #gbif.data[gbif.data$taxonrank%in%"FAMILY",]$scientificName = "Indet."
    gbif.data$scientificName[gbif.data$taxonrank%in%"VARIETY"] = as.character(paste(gbif.data$species[gbif.data$taxonrank%in%"VARIETY"],gbif.data$infraspecificepithet[gbif.data$taxonrank%in%"VARIETY"]), sep=" var. ")
    gbif.data$scientificName[gbif.data$taxonrank%in%"SUBSPECIES"] = as.character(paste(gbif.data$species[gbif.data$taxonrank%in%"SUBSPECIES"],gbif.data$infraspecificepithet[gbif.data$taxonrank%in%"SUBSPECIES"]), sep=" subsp. ")
    gbif.data$scientificName[gbif.data$taxonrank%in%"FORM"] = as.character(paste(gbif.data$species[gbif.data$taxonrank%in%"FORM"],gbif.data$infraspecificepithet[gbif.data$taxonrank%in%"FORM"]), sep=" f. ")
  #Making sure that no NA species are missed due to incomplete 'taxonrank' field
    gbif.data$scientificName[is.na(gbif.data$scientificName) & !is.na(gbif.data$species)] = 
      gbif.data$species[is.na(gbif.data$scientificName) & !is.na(gbif.data$species)]
    gbif.data = gbif.data[!is.na(gbif.data$scientificName),]
  #Any name with authors?
    if(any(str_count(gbif.data$scientificName," ")>3, na.rm = TRUE)) {
      gbif.data$scientificName[str_count(gbif.data$scientificName," ")>3] = 
        sapply(gbif.data$scientificName[str_count(gbif.data$scientificName," ")>3], remove.authors)
    }  
  #Removing taxa not identified at species level or lower
    #gbif.data = gbif.data[gbif.data$taxonrank%in%c("SPECIES","VARIETY","SUBSPECIES","FORM"),]
    gbif.data = gbif.data[str_count(gbif.data$scientificName," ")<=3,]
  #Filtering the data for South American tree name list
    gbif.data = gbif.data[gbif.data$scientificName %in% spp,]
  #Removing records of "cultivated|cultivada" specimens
    remover = "cultivated|cultivada|cultivado|cultivato|cultivad|under cultivation|plantada|plantado|planted|plantio|arboreto|arboretum|exotic|exótica|canteiro|pomar|área de visitação|botanischer garten|botanical garden|cult\\.|cant\\. [a-z]|cant [A-Z]|cant\\. [0-9]|cant \\. [0-9]|cant [0-9]|\\(cult\\)|\\(cult\\ )|in cultivo|in cultis| quadra [a-z]|quadra [a-z] do|naturalised|em experimento de|in experiment|plantation"
    deixar =  "pastagem cultivada|área do arboreto|área do arboreto|presença de exóticas| área cultivada| área cultivada"
    gbif.data = gbif.data[!grepl(remover, gbif.data$locality, ignore.case=TRUE) & !grepl(deixar, gbif.data$locality, ignore.case=TRUE),]

  ### Editing GBIF institution and collection codes ###
  #Removing numbers from the institution codes
    gbif.data$institutioncode = gsub('[0-9]','',gbif.data$institutioncode)
  #Editing some collection codes with numbers
    gbif.data$collectioncode = gsub("^56$","HCM",gbif.data$collectioncode)
    gbif.data$collectioncode = gsub("^51H-UDCA$","UDCA",gbif.data$collectioncode)
    gbif.data$collectioncode = gsub("^6$","NHT",gbif.data$collectioncode)
    gbif.data$collectioncode = gsub("^7$","BIHM",gbif.data$collectioncode)
  #Removing numbers from some institution codes
    tmp1 = gbif.data$collectioncode
    tmp1[grepl("^TAIF",tmp1)] = gsub("[0-9]|-","",tmp1[grepl("^TAIF",tmp1)])
    tmp1[grepl("^TAIFa|^TAIFb",tmp1)] = "TAIF"
    tmp1[grepl("^NMNSS",tmp1)] = gsub("[0-9]","",tmp1[grepl("^NMNSS",tmp1)])
    tmp1[grepl("^NH",tmp1)] = gsub("[0-9]|-","",tmp1[grepl("^NH",tmp1)])
    tmp1[grepl("^NU",tmp1)] = gsub("[0-9]|-","",tmp1[grepl("^NU",tmp1)])
    tmp1[grepl("^PL",tmp1)&!grepl("^PL-OB",tmp1)] = gsub("[0-9]|-","",tmp1[grepl("^PL",tmp1)&!grepl("^PL-OB",tmp1)])
    tmp1[grepl("^TAI",tmp1)&!grepl("^TAIF",tmp1)] = gsub("[0-9]|-","",tmp1[grepl("^TAI",tmp1)&!grepl("^TAIF",tmp1)])
    tmp1[grepl('[0-9]',tmp1)] = gsub('[0-9]',"",tmp1[grepl('[0-9]',tmp1)])
    gbif.data$collectioncode = tmp1
  #Removing all occurrences from seeds and other life form collections
    remover1 = c("Lichen Herbarium Berlin","BCN-Seeds","IGLUNAM-FOSILIA",
                 "Swiss Psyllid (Hemiptera) Collections - Geneva","Swiss Psyllid (Hemiptera) Collections - Basel",
                 "FloraCultivada","FrutosySemillas","Patología","Semillas",
                 "PAL","ZOO","Lichen","FUNED-POL","XFR","FloraCultivada_MMHNSR",
                 "SMNKfungicoll","RICEWEEDS","INSECTE",
                 "BSMeryscoll","BSMfungicoll","BSMlichenscoll","BSMmicrofexscoll")
    gbif.data = gbif.data[!gbif.data$collectioncode %in% remover1,]
  ##Getting the valid collection code from the Index Herbariorum or a working collection code for names if the code was not in IH
    tmp2 = gbif.data[,c("collectioncode","institutioncode","datasetkey","rightsholder")]
    tmp2$order = 1:dim(tmp2)[1]
    tmp2$collection.string = str_trim(paste(tmp2$collectioncode,tmp2$institutioncode,sep="_"))
    #if(is.null(missing.codes)) {
    #  miss = tmp2[!tmp2$collection.string %in% gbif.list$collection.string, c("collection.string","collectioncode","institutioncode","datasetkey","rightsholder")] 
    #  missing.codes = miss[!duplicated(miss$collection.string),]
    #} else {
    #  miss = tmp2[!tmp2$collection.string %in% gbif.list$collection.string, c("collection.string","collectioncode","institutioncode","datasetkey","rightsholder")]
    #  missing.codes = rbind.data.frame(missing.codes, miss[!duplicated(miss$collection.string),])
    #}  
    tmp2 = merge(tmp2, gbif.list[,c("collection.string","index.herbariorum.or.working.code")], by="collection.string", all.x = TRUE)
    tmp2 = tmp2[order(tmp2$order),]
    gbif.data$collectioncode = as.character(tmp2$index.herbariorum.or.working.code)
  #Removing all occurrences from wood collections
    gbif.data = gbif.data[!grepl('[A-Z]w$', gbif.data$collectioncode),]
  #Removing occurrences from some medicinal and germoplasm collections      
    remover2 = c("CNARP","EURISCO")
    gbif.data = gbif.data[!gbif.data$collectioncode %in% remover2,]
  #Removing all occurrences without collectioncode
    gbif.data = gbif.data[!is.na(gbif.data$collectioncode),]
    gbif.data = gbif.data[!gbif.data$collectioncode %in% "",]

  ##Replacing/removing locality fields: 'municipality' by 'county'
   gbif.data$county[is.na(gbif.data$county)|gbif.data$county %in% c("","-","*")] = 
     gbif.data$municipality[is.na(gbif.data$county)|gbif.data$county %in% c("","-","*")]
   gbif.data = gbif.data[,-which(names(gbif.data)=="municipality")]

  ##Replacing/removing locality fields: 'eventDate' by 'year'
   gbif.data$year[is.na(gbif.data$year)|gbif.data$year %in% c("","-","*")] = 
     gbif.data$eventDate[is.na(gbif.data$year)|gbif.data$year %in% c("","-","*")]
   gbif.data = gbif.data[,-which(names(gbif.data)=="eventDate")]

  ##Saving the edited/filtered tree records
    myfiles1 = gsub(".csv","-edited.csv",myfiles0)
    #myfiles1 = gsub("_treeSA_filtered","",myfiles0)
    write.csv(x=gbif.data,file=myfiles1, row.names = FALSE)
    cat(i,"\n")
}
#toto = missing.codes[!duplicated(missing.codes$collection.string),]
#write.csv(toto,'check.codes.csv')


#########################################################################################################################################################################################################################################-
#########################################################################################################################################################################################################################################-
#######################################################H
#### DOWNLOADING INFORMATION ON BOTANIC SPECIALISTS ####
#######################################################H
## Downloading data of authors of plant names from the Index of Botanists of the Harvard herbarium (http://kiki.huh.harvard.edu/databases/botanist_index.html)

##Getting the name of families to perform the search (from the preliminary spp list and herbarium data)
fam = read.csv("families.csv",as.is=T)

##finding the authors per families
ids = NULL
for (i in 1:length(fam$family)) {
  link = paste("http://kiki.huh.harvard.edu/databases/botanist_search.php?start=1&name=&id=&showid=true&remarks=",fam$family[i],"&specialty=&country=&is_author=on",sep="")
  base_html <- getURLContent(link)[[1]]
  
  if(grepl("Error: No matching results",base_html)) {
    df = data.frame(family = fam$family[i], author.ID = NA, author.name = NA)
  } else {
    txt = strsplit(base_html,"\\\n")[[1]]
    txt1 = txt[grep(' value=',txt)]
    txt1 = txt1[grep('\\[Team\\]',txt1, invert=TRUE)]
    txt1 = strsplit(txt1,"botanist_search.php\\?mode=details&id=")
    txt1 = sapply(txt1,function(x) x[2]) 
    txt2 = sapply(strsplit(txt1,'\'>'),function(x) x[2])
    df = data.frame(family = fam$family[i],
                    author.ID = as.vector(sapply(strsplit(txt1,'\'>'),function(x) x[1])),
                    author.name = as.vector(sapply(strsplit(txt2,'</a> '),function(x) x[1])))
  }		
  if(is.null(ids)) ids = df else ids = rbind.data.frame(ids,df)
  cat(i,"\n")
}
ids = ids[!is.na(ids$family)&!as.character(ids$family)%in%"",]
ids = ids[!is.na(as.character(ids$author.ID)),]

##finding the info per author
ids1 = as.character(ids[!duplicated(as.character(ids$author.ID)),]$author.ID)

autores = NULL
for(i in 1:length(ids1)) {
  id = ids1[i]
  link = paste("http://kiki.huh.harvard.edu/databases/botanist_search.php?mode=details&id=",id,sep="")
  base_html <- getURLContent(link)[[1]]
  txt = strsplit(base_html,"\\\n")[[1]]
  txt = txt[grep('<table><tr><td',txt)]
  txt1 = strsplit(txt,'\'>')[[1]]
  txt1 = gsub('</td></tr><tr><td class=\'cap','',txt1)
  txt1 = gsub('</td><td class=\'val','',txt1)
  #if(grepl('author note: ',txt1)) {
  #	txt2 = strsplit(txt1,'author note: ')[[1]][2]
  #	txt2 = as.character(strsplit(txt2,'\\]')[[1]][1]) 
  #} else {
  txt2 = txt1[grep('Remarks',txt1)+1]
  #	txt2 = as.character(strsplit(txt2,'\\[')[[1]][1]) 
  #}
  df = data.frame(author.ID = id,
                  name = txt1[which(txt1%in%"Name")+1],
                  variant.name = paste(txt1[which(txt1%in%"Variant name")+1],collapse="|"),
                  author.name = paste(txt1[which(txt1%in%"Author name")+1],collapse="|"),
                  #author.name = txt1[grep("Author name",txt1)+1],
                  author.abbrev = paste(txt1[grep("Abbrev.",txt1)+1],collapse="|"),
                  standard.name = paste(txt1[which(txt1%in%"Standard/Label&nbsp;Name")+1],collapse="|"),
                  full.name = paste(txt1[which(txt1%in%"Full Name")+1],collapse="|"),
                  geog.author = paste(txt1[which(txt1%in%"Geography Author")+1],collapse="|"),
                  geog.collector = paste(txt1[which(txt1%in%"Geography Collector")+1],collapse="|"),
                  author.note = txt2)
  if(is.null(autores)) autores = df else autores = rbind.data.frame(autores,df)
  cat(i,"\n")
}

#Correcting possible missing fields
for(i in 3:10) {
  autores[,i] = as.character(autores[,i])
  autores[,i][autores[,i]%in%""] = NA
}

#Denormalizing multi-variant name specialists
tmp = autores[grep("\\||;",autores$variant.name),]
tmp1= autores[grep("\\||;",autores$variant.name,invert=TRUE),]

tmp2= NULL
for(i in 1:dim(tmp)[1]) {
  linha = tmp[i,]
  res = NULL
  for (j in 1:str_count(linha$variant.name, "\\||;")) 
    if(is.null(res))  { res = rbind.data.frame(linha,linha) } else {
      res = rbind.data.frame(res,linha) }
  res$variant.name = str_trim(strsplit(as.character(tmp[i,"variant.name"]),"\\||;")[[1]])
  if(is.null(tmp2)) tmp2 = res else tmp2 = rbind.data.frame(tmp2,res)
  cat(i,"\n")
}
tmp2

#removing possible duplicated variant name for each authorID
toto = paste(tmp2$author.ID,tmp2$variant.name,sep="_")
tmp2 = tmp2[!duplicated(toto),]

#binding the results
autores1 = rbind.data.frame(tmp1,tmp2)
toto = paste(autores1$author.ID,autores1$variant.name,sep="_")
table(duplicated(toto)) #ok! no duplicated variant names for single author IDs

#editing the column with the specialist family
toto = strsplit(autores1$author.note," ")
toto = sapply(toto,function(x) gsub("[[:punct:]]","",x,ignore.case=TRUE))
toto1= sapply(toto,function(x) paste(x[grep("aceae|aceaem$|Leguminosae|Compositae|Guttiferae|Palmae|Gramineae",x)],collapse="|"))
dim(autores1)[1] == length(toto1)
autores1$family = toto1

#Denormalizing multi-variant name specialists
tmp = autores1[grep("\\|",autores1$family),]
tmp1= autores1[grep("\\|",autores1$family,invert=TRUE),]

tmp2= NULL
for(i in 1:dim(tmp)[1]) {
  linha = tmp[i,]
  res = NULL
  for (j in 1:str_count(linha$family, "\\|")) 
    if(is.null(res))  { res = rbind.data.frame(linha,linha) } else {
      res = rbind.data.frame(res,linha) }
  res$family = str_trim(strsplit(as.character(tmp[i,"family"]),"\\|")[[1]])
  if(is.null(tmp2)) tmp2 = res else tmp2 = rbind.data.frame(tmp2,res)
  cat(i,"\n")
}
tmp2

#binding the results
autores2 = rbind.data.frame(tmp1,tmp2)
toto = paste(autores2$author.ID,autores2$variant.name,autores2$family,sep="_")
table(duplicated(toto)) #ok! no duplicated variant names for single author IDs
autores2 = autores2[!duplicated(toto),]

#Creating the new columns
autores2$full.name1 = as.character(sapply(strsplit(as.character(autores2$name),", "),function(x) paste(x[2],x[1],sep=" ")))
autores2$tdwg.name = as.character(sapply(autores2$full.name1,format.name))
autores2$tdwg.name = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), autores2$tdwg.name)
autores2$last.name = tolower(sapply(strsplit(autores2$tdwg.name,","),function(x)x[1]))
autores2$last.name = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), autores2$last.name)
autores2$finger.print = as.vector(sapply(autores2$tdwg.name,fingerprint))
autores2$finger.print = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), autores2$finger.print)

autores2 = read.csv("autores.csv")
autores2 = autores2[autores2$tdwg.name %in% "",]
autores2$tdwg.name = as.character(sapply(autores2$full.name1,format.name))
autores2$tdwg.name = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), autores2$tdwg.name)
autores2$last.name = tolower(sapply(strsplit(autores2$tdwg.name,","),function(x)x[1]))
autores2$last.name = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), autores2$last.name)
autores2$finger.print = as.vector(sapply(autores2$tdwg.name,fingerprint))
autores2$finger.print = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), autores2$finger.print)
write.csv(autores2,"check.csv")

##Saving the result
#write.csv(autores2,"autores.csv")


##Final edits for the variant names and their respective strandardized variants
autores2 = read.csv("autores.csv",as.is=T)
nomes = gsub(' de$| do$| dos$| da$| van den$| van der$| van$| den$| von$',"",autores2$variant.name, ignore.case=TRUE)
nomes = gsub('\\.de$|\\.do$|\\.dos$|\\.da$|\\.van den$|\\.van der$|\\.von$',"",nomes, ignore.case=TRUE)
nomes = gsub(' de la |\\. de |\\. do |\\. dos |\\. da |\\. van den |\\. van der |\\. von ',"",nomes)
nomes = gsub(', de |, do |, dos |, da |, van den |, van der |, von ',"",nomes)
nomes = gsub(', de |, do |, dos |, da |, van den |, van der |, von ',"",nomes)
#Standardizing division between last and initials
nomes = gsub("  "," ",nomes)
nomes = gsub(",",", ",nomes)
nomes = gsub("  "," ",nomes)
nomes = gsub(" ,",",",nomes)
nomes = gsub("  "," ",nomes)
autores2$variant.name = nomes

autores2$tdwg.name1 = NA
autores2$tdwg.name1[grep(", ", autores2$variant.name)] =
  as.character(sapply(autores2$variant.name[grep(", ", autores2$variant.name)],format.name1)) 
autores2$tdwg.name1[grep(", ", autores2$variant.name, invert=TRUE)] =
  tmp=	as.character(sapply(autores2$variant.name[grep(", ", autores2$variant.name, invert=TRUE)],format.name)) 
autores2$tdwg.name1 = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), autores2$tdwg.name1)
autores2$tdwg.name1[autores2$tdwg.name1=="NANA"] = NA
autores2$tdwg.name1[autores2$tdwg.name1=="character(0)"] = NA
autores2$last.name1 = tolower(sapply(strsplit(autores2$tdwg.name1,","),function(x)x[1]))
autores2$last.name1 = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), autores2$last.name1)
autores2$finger.print1 = as.vector(sapply(autores2$tdwg.name1,fingerprint))
autores2$finger.print1 = chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), autores2$finger.print1)
autores2$check = autores2$tdwg.name == autores2$tdwg.name1
#write.csv(autores2,"autores1.csv")

#Denormalizing multi-variant name specialists
tmp = autores2[!is.na(autores2$check)&autores2$check==FALSE,]
tmp1= autores2[is.na(autores2$check)|autores2$check==TRUE,]

tmp2= NULL
for(i in 1:dim(tmp)[1]) {
  linha = tmp[i,]
  res = rbind.data.frame(linha,linha)
  res[2,c("tdwg.name","last.name","finger.print")] = res[1,c("tdwg.name1","last.name1","finger.print1")]
  res[2,c("order")] = res[2,c("order")] + 0.1
  if(is.null(tmp2)) tmp2 = res else tmp2 = rbind.data.frame(tmp2,res)
  cat(i,"\n")
}
tmp2

#binding the results
autores3 = rbind.data.frame(tmp1,tmp2)
autores3 = autores3[,1:18]
toto = paste(autores3$author.ID,autores3$variant.name,autores3$tdwg.name,sep="_")
table(duplicated(toto)) #ok! no duplicated variant names for single author IDs
#autores4 = autores3[!duplicated(toto),]
write.csv(autores3,"autores1.csv")