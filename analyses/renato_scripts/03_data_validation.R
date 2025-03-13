############################################################
############################################################
### DATA VALIDATION: DUPLICATE, GEOGRAPHIC AND TAXONOMIC ###
############################################################
############################################################
memory.limit(size=60000)
rm(list=ls())

### LOADING PACKAGES ###
require(flora)
require(taxize)
require(stringr)
require(dplyr)
require(raster)
require(rgeos)
require(sp)
require(parallel)
require(doParallel) 	
require(rgdal)
require(geosphere)
#require(ModelR)
source("functions.R")


##############################
#### TAXONOMIC VALIDATION ####
##############################

#CLASSIFY THE TAX. CONFIDENCE LEVEL??: 1:FAMILY TAXONOMIST, 0.5:OTHER FAMILIES TAXONOMISTS AND "SUPER-COLLECTORS", 0.1:"OTHERS DETERMINERS", 0:"NO DETERMINER"
### Reading and editing the validated species name and synonyms list ###
spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=TRUE, na.string=c(NA,""," "))
#removing unecessary columns
spp = spp[,c("TreeCo_status","SpeciesKTSA","family.reflora","genus.reflora","species.reflora","taxon.rank.reflora","name.status.reflora","notes.reflora",
			"Accepted_name_family","Accepted_name_genus","Accepted_name","Accepted_name_rank","Taxonomic_status","DataSource",
			"Family.TPL","New.Genus.TPL","NewTaxon.TPL","Taxonomic.status.TPL")]		

#replacing names not found in ReFlora by The Plant List (previous to TNRS)
spp[is.na(spp$species.reflora) & spp$TreeCo_status%in%"ok_use_TPL",c("family.reflora","genus.reflora","species.reflora")] =
		spp[is.na(spp$species.reflora) & spp$TreeCo_status%in%"ok_use_TPL",c("Family.TPL","New.Genus.TPL","NewTaxon.TPL")]
#replacing names not found in ReFlora by the TNRS/Hans taxonomy
spp[is.na(spp$species.reflora),c("family.reflora","genus.reflora","species.reflora","name.status.reflora")] =
		spp[is.na(spp$species.reflora),c("Accepted_name_family","Accepted_name_genus","Accepted_name","Taxonomic_status")]
#replacing final names not found in ReFlora and TNRS by the Plant List
spp[is.na(spp$species.reflora) & grepl("ok",spp$TreeCo_status),c("family.reflora","genus.reflora","species.reflora")] =
		spp[is.na(spp$species.reflora) & grepl("ok",spp$TreeCo_status),c("Family.TPL","New.Genus.TPL","NewTaxon.TPL")]

#flagging problematic names (and putting them down the list)
table(spp[is.na(spp$species.reflora),]$Taxonomic_status)
table(spp[is.na(spp$species.reflora),]$TreeCo_status)
tmp = spp[is.na(spp$species.reflora),]
tmp$species.reflora = tmp$SpeciesKTSA
tmp$name.status.reflora = "problematic"
spp = spp[!is.na(spp$species.reflora),]
table(spp$Taxonomic_status)
table(spp$TreeCo_status)
spp = rbind.data.frame(spp,tmp) #recombining valid and problematic names into a single data frame
rm(tmp)

#filtering columns
spp = spp[,c("SpeciesKTSA","family.reflora","genus.reflora","species.reflora","taxon.rank.reflora","name.status.reflora","notes.reflora","DataSource","TreeCo_status"),]
names(spp) = c("name","family.correct","genus.correct","species.correct","taxon.rank","name.status","notes","source","status")

#checking possible problems with family names
tmp = aggregate(spp$family.correct,list(spp$genus.correct),function(x)as.character(unique(x)))
tmp[is.na(tmp[,2]),]
tmp[is.na(tmp[,1]),]
tmp[grep(" ",tmp[,1]),]
tmp[grep("-",tmp[,1]),]
tmp[grep("-",tmp[,2]),]

##Considering names only at species level
#getting the names with any of the infra-specific notation
tmp = spp[grepl(" var\\.| subsp\\.| f\\.| fo\\.| ssp\\.| subf\\.| subfo\\.| subvar\\.",spp$name),]
#removing the infra-specific notation from the input names
spp$name = gsub(" var\\.| subsp\\.| f\\.| fo\\.| ssp\\.| subf\\.| subfo\\.| subvar\\.","",spp$name)
spp = spp[!grepl("\\.",spp$name),]
#Re-adding the infra-specific taxa in their original notation to make sure all names are found, and thus all family names from the occurrences are corrected
spp = rbind.data.frame(spp,tmp)

### Reading the specialists' name dictionary ###
autores = read.csv("autores.csv", as.is=TRUE,na.string=c(NA,""," "))
autores = autores[!is.na(autores$tdwg.name),]
autores = autores[!is.na(autores$family),]
autores = autores[!grepl('\\?|,',autores$family),]
autores = autores[!grepl('Floristics/Generalist \\(all families\\)|Wood anatomist', autores$family),]

#Standardizing family names
families.apg = read.csv("families_synonyms.csv", as.is=TRUE,na.string=c(NA,""," "))
autores = merge(autores,families.apg[,c("name","name.correct")],by.x="family",by.y="name",all.x=TRUE)
autores = autores[order(autores$order),]
#Getting misspelled family names
tmp = autores[is.na(autores$name.correct),]
head(tmp) #only fungi and cyano bacteria are not in the dictionary of name synonyms
#tmp1 = get.taxa (unique(tmp$family), life.form = FALSE, habitat = FALSE, states = FALSE,
#	suggestion.distance = 0.92, drop = c("id","accepted.name","search.str","scientific.name","specific.epiteth","infra.epiteth","authorship","threat.status"))
#tmp1.1 = tmp1[!is.na(tmp1$taxon.rank) & tmp1$taxon.rank %in% "family",]
#tmp1.1 = tmp1.1[order(tmp1.1$order),]
#autores[is.na(autores$name.correct),c("name.correct")] = tmp1.1[is.na(autores$name.correct),c("family.y")] 
#Getting names not found
#tmp1.2 = tmp1[tmp1$notes %in% c("not found"),]
#sort(tmp1.2$original.search)

#Getting the unique family-specialist combinations (1st options: family_tdwg.name)
combo = unique(paste(autores$family,autores$tdwg.name,sep="_"))
tmp = unique(paste(autores$name.correct[!is.na(autores$name.correct)],autores$tdwg.name[!is.na(autores$name.correct)],sep="_"))
tmp = tmp [!tmp %in% combo]
combo = c(combo,tmp)
combo = str_trim(combo)
table(duplicated(combo))
#combo1 = c(paste(autores$family,autores$last.name,sep="_")) #,paste(autores$family.y,autores$last.name,sep="_")))
#combo2 = c(paste(autores$family,autores$finger.print,sep="_")) #,paste(autores$family.y,autores$finger.print,sep="_")))

### Getting the file paths for the herbarium data
splink = read.csv("speciesLink.csv", as.is=T)
splink.new = read.csv("speciesLink_new.csv", as.is=T)
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE)
paths1 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new"
paths2 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot"
paths3 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//sndb_mincyt_arg"
#paths1 = dir(paths1,full.names=TRUE)[!dir(paths1) %in% splink$acronym]
#paths2 = dir(paths2,full.names=TRUE)[!dir(paths2) %in% splink$acronym]
paths1 = dir(paths1,full.names=TRUE)
paths2 = dir(paths2,full.names=TRUE)
paths3 = dir(paths3,full.names=TRUE)
#putting paths for all collections together
paths = c(paths,paths1,paths2,paths3)
paths = list.files(paths,full.names=TRUE)
paths = c(paths, list.files("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE))
paths = paths[grep("-edited-cleaned-loc-coord.csv",paths)] 

### Perfoming the taxonomical validation for all collections ###
for(i in 1:length(paths)) {
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,header=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns
		cols = c("family","familia",
		         "scientificName","scientificname",
		         "determinador.name","coletor.name",
		         "typeStatus","typestatus","nat_typus")
			    #"dateIdentified","dateidentified","yearidentified","anodeterm") #columns not present in GBIF
		cls =  c(unique(cols[cols %in% names(herb.data)]),"order")
		herb.data$order = as.vector(1:dim(herb.data)[1]) #column to put the data back on its original order		
		herb.data1 = herb.data[,cls]
		names(herb.data1)[1] = "family.original"
		names(herb.data1)[2] = "scientificName"
		names(herb.data1)[length(names(herb.data1))-1] = "typeStatus"
	##Standardizing species notation for JABOT - Not needed anymore, the species dictionary now has infra-specific taxons with and without the notations below
		#if(grepl("/jabot/",myfiles0)) {
		#	spp1 = spp
		#	spp1$name = gsub(' var\\.| subsp\\.| f\\.| fo\\.| ssp\\.| subf\\.| subfo\\.','',spp1$name) 
		#} else {
		#
		#	spp1 = spp
		#}
	##Obtainig the valid species name and family for each occurrence
		herb.data1 = merge(herb.data1, spp[,c("name","family.correct","species.correct","name.status","notes","source","status")],
						by.x="scientificName",by.y="name",all.x=TRUE)
		herb.data1$family.correct[is.na(herb.data1$family.correct)] = herb.data1$family.original[is.na(herb.data1$family.correct)]
		herb.data1$species.correct[is.na(herb.data1$species.correct)] = herb.data1$scientificName[is.na(herb.data1$species.correct)]		
		herb.data1 = herb.data1[order(herb.data1$order),]
	##Putting all infrageneric identifications at species level
		herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " var\\."),function(x) x[1])
		herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " subsp\\."),function(x) x[1])
		herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " f\\."),function(x) x[1])
		herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " fo\\."),function(x) x[1])
		herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " ssp\\."),function(x) x[1])
		herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " subf\\."),function(x) x[1]) 
		herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " subfo\\."),function(x) x[1]) 
		herb.data1$species.correct = sapply(strsplit(herb.data1$species.correct, " subvar\\."),function(x) x[1])
	##Obtainig the family-specialist combinations for each occurrence
		herb.data1$combo = paste(herb.data1$family.correct,herb.data1$determinador.name,sep="_")
		herb.data1$combo = str_trim(herb.data1$combo)
	##Crossing the occurrence and reference family-specialist combinations
		herb.data1$tax.check = herb.data1$combo %in% combo
	##Finding possible missing determinations due to small typos (max.2 letters of difference)
		#tmp = unique(herb.data1[!herb.data1$tax.check & !herb.data1$determinador.name %in% c("SemDeterminador","Semdeterminador"),]$combo)
		#names(tmp) = unique(herb.data1[!herb.data1$tax.check & !herb.data1$determinador.name %in% c("SemDeterminador","Semdeterminador"),]$combo)
		#if(length(tmp)==0) { herb.data1$tax.check.new = FALSE 
		#} else { 
		#	cl <- makeCluster(detectCores()-1)
		#	clusterExport(cl, list("tmp","combo"),envir=environment())
		#	tmp2 <- parLapply(cl,1:length(tmp), fun= function(j) {
  		#					tmp1 = agrep(tmp[j], combo, value=TRUE, max.distance=2)
		#					if(length(tmp1)==0) { 
		#						tmp[j] = tmp[j] 
		#					} else { 
		#						dst = adist(tmp[j],tmp1)
		#						id = which(dst==min(dst))
		#						tmp[j] = head(tmp1[id],1) 
		#					} } ) 
		#	stopImplicitCluster()
		#	stopCluster(cl)
		#	tmp2 = do.call(c, tmp2)
		#	tmp1 = data.frame(combo=names(tmp),combo.new=tmp2, stringsAsFactors=FALSE)
		#	tmp2 = herb.data1[!herb.data1$tax.check & !herb.data1$determinador.name %in% c("SemDeterminador","Semdeterminador"),c("order","combo")]
		#	tmp3 = merge(tmp2,tmp1,by="combo",all.x=TRUE)
		#	tmp3 = tmp3[order(tmp3$order),] #putting the data back on its original order
		#	herb.data1[!herb.data1$tax.check & !herb.data1$determinador.name %in% c("SemDeterminador","Semdeterminador"),]$combo = as.character(tmp3$combo.new)
		#	herb.data1$tax.check.new = herb.data1$combo %in% combo
		#}
	##Validating all type specimens (isotype, paratypes, etc) but not the "not a type"
		herb.data1$tax.check[!is.na(herb.data1$typeStatus)&!grepl("not a type|notatype|probable type|tipo provavel|tipo provável",herb.data1$typeStatus,ignore.case = TRUE)] = TRUE
	##Specifying why taxonomy was not validated
		herb.data1$tax.check[herb.data1$tax.check == FALSE & 
		                    herb.data1$determinador.name %in% c("Semdeterminador","SemDeterminador","Anonymus","Anonymous","Anonimo","Incognito","Unknown","s.d.")] = "cannot_check"
	##Validating all specimens collected by the family specialist but with the determiner field empty
		herb.data1$combo1 = paste(herb.data1$family.correct,herb.data1$coletor.name,sep="_")
		herb.data1$combo1 = str_trim(herb.data1$combo1)
		#Crossing the occurrence and reference family-specialist combinations
		herb.data1$tax.check1 = herb.data1$combo1 %in% combo
		#Replacing the specimens that could by validated
		herb.data1$tax.check[herb.data1$tax.check %in% c("cannot_check") & herb.data1$tax.check1 %in% TRUE] = TRUE
	##Saving the filtered data
		herb.data = merge(herb.data,herb.data1[,c("order","family.correct","species.correct","name.status","notes","source","status","tax.check")],by="order",all.x=T)
		herb.data = herb.data[!duplicated(herb.data$order),]
		path.csv = gsub("coord.csv","coord-tax.csv",myfiles0)
		write.csv(x=herb.data,file=path.csv, row.names = FALSE)
		cat(i,"\n")
}

########################################################################################################################################################################################-
########################################################################################################################################################################################-
#############################################-
#### GEOGRAPHICAL COORDINATES VALIDATION ####
#############################################-

### MAP FOR VALIDATION AT COUNTRY-LEVEL ###
##Loading, editing and converting the world country shapefile
path = "E://ownCloud//W_GIS"
#wo <- readOGR(dsn=paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp",sep=""),layer="gadm36_0")
#wo@data$pais = countrycode(wo@data$GID_0,'iso3c', 'country.name')
#wo@data$pais[is.na(wo@data$pais)] = as.character(wo@data$NAME_0[is.na(wo@data$pais)]) 
#wo@data$pais = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), wo@data$pais))
#wo@data$pais = sapply(strsplit(wo@data$pais,' - | \\('), function(x) x[[1]][1])
#wo@data$pais = str_trim(wo@data$pais)
#wo@data$pais = gsub("-"," ",wo@data$pais)
#wo@data$pais = gsub(" - "," / ",wo@data$pais)
#wo@data$pais = gsub(" do | da | de "," ",wo@data$pais)
#wo@data$pais = gsub(" dos | das | des "," ",wo@data$pais)
#wo@data$pais = gsub(' d\' | d\'| d´| d\"| d’'," ",wo@data$pais)
#wo@data$pais = gsub("\\.$","",wo@data$pais)
#wo@data$pais = gsub(" dx "," ",wo@data$pais)

#Saving the edited shapefile as .rds  
#saveRDS(wo,file=paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp//gadm36_0.rds",sep=""))

#Loading the edited .rds with the country limits of the world 
wo1 = readRDS(file=paste(path,"//WO_ADM_limits_GADM36//gadm36_levels_shp//gadm36_0.rds",sep=""))

#Defining the projection used by the world country shapefile
pj = crs(wo1) #defining and standardizing the geographical projections

### MAP FOR VALIDATION AT STATE AND COUNTY LEVEL - ARG, BRA AND PAY ###
#Loading and edititing country-specific shapefiles
path0 = "E://ownCloud//W_GIS//Am_Lat_ADM_GADM_v3.6//"
countries = list.files(path0,full.name=TRUE)
#countries = countries[grep("BRA|ARG|PRY",countries)]
countries = countries[grep("ARG|BOL|BRA|CHL|COL|ECU|GUY|GUF|SUR|PRY|PER|URY|VEN|MEX",countries)]
countries = countries[grep('_2_sp.rds',countries)]
country.list = vector('list',length(countries))
pais = NULL
for (i in 1:length(country.list)) {
	path = countries[i]
	tmp = readRDS(path)
	tmp1 = tmp@data
	tmp1 = tmp1[,c("NAME_0","NAME_1","NAME_2")]
	pais[i] = unique(tmp1$NAME_0)
   #Editing names to match the loc format 
	tmp1$NAME_0 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_0))
	tmp1$NAME_1 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_1))
	tmp1$NAME_2 = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), tmp1$NAME_2)) 
	tmp1 = apply(tmp1,2,function(x) as.character(gsub("-"," ",x)))
	tmp1 = apply(tmp1,2,function(x) as.character(gsub(" - "," / ",x)))
	tmp1 = apply(tmp1,2,function(x) as.character(gsub(" do | da | de "," ",x)))
	tmp1 = apply(tmp1,2,function(x) as.character(gsub(" dos | das | des "," ",x)))
	tmp1 = apply(tmp1,2,function(x) as.character(gsub(' d\' | d\'| d´| d\"| d’'," ",x)))
	tmp1 = apply(tmp1,2,function(x) as.character(gsub('\\.$',"",x)))
	tmp1 = apply(tmp1,2,function(x) as.character(gsub(" dx "," ",x)))
	tmp@data = as.data.frame(tmp1)
	country.list[[i]] = tmp 
}
names(country.list) = pais

## Standardizing the shapefile and the gazetteer names for Brazil
#Creating the locality strings for the shapefile dataframe
tmp = country.list$Brazil@data
tmp[,1] = as.character(tmp[,1]); tmp[,2] = as.character(tmp[,2]); tmp[,3] = as.character(tmp[,3])
tmp$order = 1:dim(tmp)[1]
tmp1= data.frame(loc=paste(tmp[,1],tmp[,2],tmp[,3],sep="_"))
tmp1$loc = as.character(tmp1$loc)
tmp1$order = 1:dim(tmp1)[1]
#loading the gazetteer and removing possible duplicated localities
dic = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)
dic = dic[dic$status %in% "ok",]
#dic1= dic[!duplicated(dic$loc),]
#dic1= dic1[!dic1$resolution.gazetteer%in%c("localidade","localidade|sublocalidade","distrito|vila","distrito|bairro","cachoeira","mina","vila","serra"),]
#merging the two sources of info
tmp2= merge(tmp1,dic[,c("NAME_0","NAME_1","NAME_2","loc","loc.correct")],by="loc",all.x=TRUE)
tmp3 = strsplit(tmp2$loc.correct,"_")
n0 = rep(NA,dim(tmp2)[1]); n1 = rep(NA,dim(tmp2)[1]); n2 = rep(NA,dim(tmp2)[1])
n0 = sapply(tmp3,function(x) x[1])   		
n1 = sapply(tmp3,function(x) x[2])
n2 = sapply(tmp3,function(x) x[3])
tmp2$country = n0
tmp2$state = n1
tmp2$county = n2
#replacing and savind the correct info
tmp2 = tmp2[order(tmp2$order),]
table(tmp$order == tmp2$order)
id = !tmp2$loc == tmp2$loc.correct
table(tmp[id,1] == tmp2[id,c("country")])
#tmp[id,1] = tmp2[id,c("country")]
table(tmp[id,2] == tmp2[id,c("state")]) #all differences were double-checked and the gazetteer is right!
cbind(tmp[id,c(2)], tmp2[id,c("state","county")])[tmp[id,2] == tmp2[id,c("state")],]
#tmp[id,2] = tmp2[id,c("state")]
table(!tmp[id,3] == tmp2[id,c("county")]) #all differences were double-checked and the gazetteer is right!
cbind(tmp[id,c(2,3)], tmp2[id,c("county")])[!tmp[id,3] == tmp2[id,c("county")],]
tmp[id,3] = tmp2[id,c("county")]
country.list$Brazil@data = tmp[,c("NAME_0","NAME_1","NAME_2")]

### MAP FOR VALIDATION AT STATE AND COUNTY LEVEL - ARG, BRA AND PAY ###
#require(gdata)
#require(maptools)
#require(RANN)

### Projecting the country shapefiles
#TA <- CRS("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +a=6378160 +b=6356537.557298475 +units=m +no_defs")
#country.list.proj = country.list
#for(i in 1:length(country.list)) { country.list.proj[[i]] = spTransform(country.list[[i]], TA)	}

### Loading the and projecting Atlantic Forest limits (ARG, BRA, PAY) ###
#path = "E://ownCloud//W_GIS"
##Lei MA + TNC para ARG+PAR
#af = readOGR(dsn=paste(path,"//AF_limites_milton",sep=""),layer="merge_limites_MA11428_TNC_ARG_BR_PAR")
#af.proj <- spTransform(af, TA)
#Biomas MMA + TNC para ARG+PAR
#af1 = readOGR(dsn=paste(path,"//AF_limites_milton",sep=""),layer="merge_limites_BiomasMMA_TNC_ARG_BR_PAR")
#af1.proj <- spTransform(af1, TA)

#biomas = readOGR(dsn=paste(path,"//BR_Biomas_MMA//Biomas_MMA",sep=""),layer="bioma")
#projection(biomas) <- CRS("+init=epsg:5580 +proj=longlat +ellps=GRS80 +a=6378137.0 +b=298.257222101 +units=m +no_defs")
#biomas.proj <- spTransform(biomas, TA)

### Defining the amount of area of each county in the Atlantic Forest
#lista = vector("list",length(country.list.proj))
#for(i in 1:length(lista)) {
#	pais = country.list.proj[[i]]
#	g2 = data.frame(country = pais@data$NAME_0, state = pais@data$NAME_1, county = pais@data$NAME_2)
#	g2$loc = paste(g2$country, state = g2$state, g2$county, sep="_")
#	g2$Area = NA; g2$AreaAF = NA
#	#asub <- gOverlaps(pais, af.proj, byid = TRUE)[1,]
#	asub <- gOverlaps(pais, af1.proj, byid = TRUE)[1,]
#	ids <- as.vector(which(asub==TRUE))
#	sts <- unique(as.character(g2[asub,]$state))
#	ids1 <- which(g2$state %in% sts)
#	for (j in ids1) {
#		tmp = pais[j,]
#		g2$Area[j] = gArea(tmp)
#		#x <- gIntersection(tmp,af.proj)
#		x <- gIntersection(tmp,af1.proj)
#		#if (i==2) { x1 = gIntersection(tmp,biomas.proj, byid = TRUE) }
#		if (!is.null(x)) { g2$AreaAF[j] <- gArea(x) }
#		cat(paste(i,j,sep="_"),"\n")
#	}
#	g2$AreaAF.prop = g2$AreaAF/g2$Area
#	lista[[i]] = g2
#}
#county.af = do.call("rbind.data.frame",lista)
#county.af = droplevels(county.af)
#county.af$domain = NA
##Assinging the classes in respect to the proportion of AF in each county
#county.af[!is.na(county.af$AreaAF)&county.af$AreaAF.prop>=0.9,]$domain = "AtlanticForest" 
#county.af[!is.na(county.af$AreaAF)&county.af$AreaAF.prop<0.9&county.af$AreaAF.prop>=0.1,]$domain = "Transition"
#county.af[!is.na(county.af$AreaAF)&county.af$AreaAF.prop<0.1,]$domain = "Others"
##Inspectng the classification
#plot(pais,lwd=2,xlim=c(-200000,-100000),ylim=c(-1400000,-1200000))
#plot(pais,add=T,border=as.double(as.factor(county.af$domain[county.af$country%in%"paraguay"])))
#text(coordinates(pais),label=pais$NAME_2,cex=0.5,pos=1)
#plot(af.proj,lwd=2,add=T, border="magenta")
#Saving
#dic = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)
#tmp = left_join(dic,county.af,by="loc")
#write.csv(tmp,"check5.csv")
#Inspecting the classification from the edited gazetteer for Brazil
#dic = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)
#dic = dic[dic$status %in% "ok",]

#tmp = dic[dic$NAME_0%in%c("Argentina","Brazil","Paraguay"),]
#tmp = tmp[!is.na(tmp$AtlanticForest)&!tmp$AtlanticForest%in%c("AtlanticForest"),]
#tmp = tmp[tmp$resolution.gazetteer%in%c("county"),]
#plot(tmp$longitude.gazetteer,tmp$latitude.gazetteer,
#		xlim=c(-60,-33),ylim=c(-32,-3),
#		xlim=c(-47,-45),ylim=c(-23,-21),
#		pch=19,cex=0.6,col=as.double(as.factor(tmp$AtlanticForest))+1)
#plot(country.list[[2]],add=T,border="darkgrey")
#plot(af,add=T,border=1,lwd=2)
#text(tmp$longitude.gazetteer,tmp$latitude.gazetteer,labels=tmp$NAME_2,cex=0.7,pos=4,col=2)

### Defining the amount of area of each county in each Brazilian biome
#Brazilian map
pais = country.list.proj[[2]] 
#Creating the data.frame
g2 = data.frame(country = pais@data$NAME_0, state = pais@data$NAME_1, county = pais@data$NAME_2)
g2$loc = paste(g2$country, state = g2$state, g2$county, sep="_")
g2$Area = gArea(pais, byid=TRUE)
#Creating the list of biome names to be checked and the list that will receive the results
#nomes = c("AMZ","CAAT","CER","MAT","PMP","PTN")
nomes = c("AMAZONIA","CAATINGA","CERRADO","MATA.ATLANTICA","PAMPA","PANTANAL")
levels(biomas.proj@data$CD_LEGEN1) = nomes

for(i in 1:length(nomes)) {
	bioma = biomas.proj[biomas.proj@data$CD_LEGEN1%in%nomes[i],]
	asub <- gOverlaps(pais, bioma, byid = TRUE)[1,]
	ids <- as.vector(which(asub==TRUE))
	sts <- unique(as.character(g2[asub,]$state))
	ids1 <- which(g2$state %in% sts)
	res = rep(NA,dim(pais)[1])
	for (j in ids1) {
		tmp = pais[j,]
		x <- gIntersection(tmp,bioma)
		if (!is.null(x)) { res[j] <- gArea(x) }
		cat(paste(i,j,sep="_"),"\n")
	}
	g2[,paste("Area",nomes[i],".prop",sep="")] = res/g2$Area
}
county.biomes = g2
county.biomes = droplevels(county.biomes)
county.biomes$biome = NA
#Assinging the classes in respect to the proportion of biomes in each county
county.biomes[!is.na(county.biomes$AreaAMAZONIA.prop)&as.double(county.biomes$AreaAMAZONIA.prop)>=0.9,]$biome = "Amazon" 
county.biomes[!is.na(county.biomes$AreaCAATINGA.prop)&as.double(county.biomes$AreaCAATINGA.prop)>=0.9,]$biome = "Caatinga"
county.biomes[!is.na(county.biomes$AreaCERRADO.prop)&as.double(county.biomes$AreaCERRADO.prop)>=0.9,]$biome = "Cerrado"
county.biomes[!is.na(county.biomes$AreaMATA.ATLANTICA.prop)&as.double(county.biomes$AreaMATA.ATLANTICA.prop)>=0.9,]$biome = "AtlanticForest"
county.biomes[!is.na(county.biomes$AreaPAMPA.prop)&as.double(county.biomes$AreaPAMPA.prop)>=0.9,]$biome = "Pampa"
county.biomes[!is.na(county.biomes$AreaPANTANAL.prop)&as.double(county.biomes$AreaPANTANAL.prop)>=0.9,]$biome = "Pantanal"

#Inspecting the classification
#plot(pais,lwd=2)
#plot(pais,add=T,col=as.double(as.factor(county.biomes$domain)))
#plot(biomas.proj,lwd=2,add=T, border="magenta")
#Saving
#tmp = left_join(dic,county.biomes,by="loc")
#write.csv(tmp,"check6.csv")


#### 1 - PERFORMING THE VALIDATION ITSELF ####

#Defining the projection used by the world country shapefile
pj1 = crs(country.list$Brazil) #defining and standardizing the geographical projections

## Uploading the final/edited version of the gazetter, with missing counties and ortographical variants of some names (not exaustive)
dic = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)
dic = dic[dic$status %in% "ok",]

##Getting the file paths for the herbarium data
splink = read.csv("speciesLink.csv", as.is=T)
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE)
paths1 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new"
paths2 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot"
paths3 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//sndb_mincyt_arg"
#paths1 = dir(paths1,full.names=TRUE)[!dir(paths1) %in% splink$acronym]
#paths2 = dir(paths2,full.names=TRUE)[!dir(paths2) %in% splink$acronym]
paths1 = dir(paths1,full.names=TRUE)
paths2 = dir(paths2,full.names=TRUE)
paths3 = dir(paths3,full.names=TRUE)

paths = c(paths,paths1,paths2,paths3)
paths = list.files(paths,full.names=TRUE)
paths = c(paths, list.files("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE))
paths = paths[grep("-edited-cleaned-loc-coord-tax.csv",paths)] 

### Performing the geographical validation for all collections ###
for(i in 25:length(paths)) {
	### PRE-VALIDATION STEPS ###
	##Loading the occurrence data
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,header=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns
		cols = c("loc.correct","latitude.gazetteer","longitude.gazetteer","origin.coord","resolution.coord","latitude.work","longitude.work")
		cls =  unique(cols[cols %in% names(herb.data)])
		herb.data1 = herb.data[,cls]
		herb.data1$order = 1:dim(herb.data1)[1] #putting the data back on its original order
	##Defining the country, state and county columns
		tmp = strsplit(herb.data1$loc.correct,"_")
		id = sapply(tmp,length)
		n0 = rep(NA,dim(herb.data1)[1]); n1 = rep(NA,dim(herb.data1)[1]); n2 = rep(NA,dim(herb.data1)[1])
		n0[id>=1] = sapply(tmp[id>=1],function(x) x[1])   		
		n1[id>=2] = sapply(tmp[id>=2],function(x) x[2])
		n2[id>=3] = sapply(tmp[id>=3],function(x) x[3])
		herb.data1$country = n0
		herb.data1$state = n1
		herb.data1$county = n2
	##Creating the geo.check columns
		herb.data1$geo.check = NA
		herb.data1$geo.check[herb.data1$origin.coord %in% c("ok_country_gazet")] = "ok_country_gazet"
		herb.data1$geo.check[herb.data1$origin.coord %in% c("ok_state_gazet")] = "ok_state_gazet"
		herb.data1$geo.check[herb.data1$origin.coord %in% c("ok_county_gazet")] = "ok_county_gazet"
		herb.data1$geo.check[herb.data1$origin.coord %in% c("ok_locality_gazet")] = "ok_locality_gazet"
		herb.data1$geo.check[herb.data1$origin.coord %in% "no_coord"] = "no_coord"
		herb.data1$geo.check[is.na(herb.data1$latitude.work)&is.na(herb.data1$latitude.gazetteer)] = "no_coord"
	##Subsetting the coordinates that need to be check
		tmp = herb.data1[herb.data1$origin.coord %in% "coord_original",]
		if(dim(tmp)[1]>0) {
			tmp = tmp[!is.na(tmp$longitude.work),]
			tmp = tmp[!is.na(tmp$latitude.work),]
	##Getting data frame with spatial coordinates (points) and standardizing the projection
			coordinates(tmp) <- c("longitude.work", "latitude.work")  # set spatial coordinates
			proj4string(tmp) <- pj  # define projection system of our data
			##Comparing the spatial data frame with the world country shapefile
			if(dim(tmp)[1]>20000) {
			  #for big verifications
  			no_cores = detectCores() -1
  			n <- 10 #number of dividision to the input data frame
  			parts <- split(x = 1:dim(tmp)[1], f = cut(1:dim(tmp)[1], n))
  			cl <- makeCluster(no_cores)
  			clusterExport(cl, list("tmp","parts","n","wo1"),envir=environment())
  			clusterEvalQ(cl, library(sp))
  			overParts <- parLapply(cl = cl, X = 1:n, fun = function(x) {
  			    over  <- over(tmp[parts[[x]],], wo1)
  			    over1 <- cbind.data.frame(tmp[parts[[x]],]@data,pais=as.character(over[,c("pais")]))
  			    gc() # release memory
  			    return(over1)
  			})
  			stopImplicitCluster()
  			stopCluster(cl)
  			tmp2 = do.call(rbind.data.frame, overParts)
  			
			} else {
			  #for small verifications
  			tmp0 = over(tmp, wo1, fn=NULL) ##Old version, took too long
  			tmp2 = cbind.data.frame(tmp,pais=as.character(tmp0[,c("pais")]))
		  }
			tmp3 = left_join(herb.data1,tmp2,by="order")
			herb.data1$pais = as.character(tmp3$pais) 
		} else { 	herb.data1$pais = NA }
		
	##Comparing the spatial data frame with the selected country shapefiles
		lista.paises = tolower(names(country.list))
		tmp = herb.data1[herb.data1$pais %in% lista.paises,]
		#tmp = herb.data1[herb.data1$pais %in% c("argentina","brazil","paraguay"),]
		if(dim(tmp)[1]>0) {
		  if(dim(tmp)[1]>20000) {
		    #For big verifications
  			no_cores = detectCores() - 1
  			n <- unique(tmp$pais)[unique(tmp$pais) %in% lista.paises] #list of countries in the database
  			cl <- makeCluster(no_cores)
  			#clusterExport(cl, list("tmp","n1","n2","lista.paises","pj1","n","country.list"),envir=environment())
  			clusterExport(cl, list("tmp","pj1","n","lista.paises","country.list"),envir=environment())
  			clusterEvalQ(cl, library(sp))
  			overParts <- parLapply(cl = cl, X = 1:length(n), fun = function(x) {
  			  tmp1.1 = tmp[tmp$pais %in% n[x],]
  			  coordinates(tmp1.1) <- c("longitude.work", "latitude.work")  # set spatial coordinates
  			  proj4string(tmp1.1) <- pj1  # define projection system of our data
  			  id = which(n[x] == lista.paises)
  			  pais.shp = country.list[[id]]
  			  over  <- over(tmp1.1, pais.shp, fn=NULL)
          tmp1.1@data$n1 = as.character(over$NAME_1)
          tmp1.1@data$n2 = as.character(over$NAME_2)
  			  gc() # release memory
  			  return(tmp1.1@data)
  			})
  			stopImplicitCluster()
  			stopCluster(cl)
  			tmp2 = do.call(rbind.data.frame, overParts)
  			tmp2 = tmp2[order(tmp2$order),]
  			#Creating the vectors with state and county/province/department names
  			n1 = rep(NA,dim(tmp)[1])
  			n2 = rep(NA,dim(tmp)[1]) 
  			n1[is.na(n1)&!is.na(as.character(tmp2$n1))] = as.character(tmp2$n1)[is.na(n1)&!is.na(as.character(tmp2$n1))]
  			n2[is.na(n2)&!is.na(as.character(tmp2$n2))] = as.character(tmp2$n2)[is.na(n2)&!is.na(as.character(tmp2$n2))]
  			
		  } else {
		    #For small verifications
		    list.countries = unique(tmp$country)
		    list.countries = list.countries[!is.na(list.countries)]
		    list.countries = list.countries[list.countries %in% lista.paises]
		    coordinates(tmp) <- c("longitude.work", "latitude.work")  # set spatial coordinates
		    proj4string(tmp) <- pj1  # define projection system of our data
		    n1 = rep(NA,dim(tmp)[1])
		    n2 = rep(NA,dim(tmp)[1]) 
		    for (j in 1:length(list.countries)) {
		      pais.shp = which(tolower(names(country.list))==list.countries[j])
		      tmp1 = country.list[[pais.shp]]
		      tmp2 = over(tmp, tmp1, fn=NULL)
		      n1[is.na(n1)&!is.na(as.character(tmp2$NAME_1))] = as.character(tmp2$NAME_1)[is.na(n1)&!is.na(as.character(tmp2$NAME_1))]
		      n2[is.na(n2)&!is.na(as.character(tmp2$NAME_2))] = as.character(tmp2$NAME_2)[is.na(n2)&!is.na(as.character(tmp2$NAME_2))]
		    }
		  }    
			#Saving the state and county/province/department names
			herb.data1$estado = NA; herb.data1$municipio = NA 
			herb.data1[herb.data1$pais %in% lista.paises,]$estado = n1
			herb.data1[herb.data1$pais %in% lista.paises,]$municipio = n2
			herb.data1$loc.coord = paste(herb.data1$pais,herb.data1$estado,herb.data1$municipio,sep="_")
			herb.data1$loc.coord[herb.data1$loc.coord %in% "NA_NA_NA"] = NA 
		} else {
			herb.data1$estado = NA
			herb.data1$municipio = NA
			herb.data1$loc.coord = NA
		}
		
	### GEO-VALIDATION STEPS ###
	##1- Validating the coordinates at different levels - exact matchs
	   #1.1 Cases with original coordinates but without country, state or county information (cannot check)
		herb.data1$geo.check[is.na(herb.data1$geo.check)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$pais)&is.na(herb.data1$loc.correct)] = "no_cannot_check"
     #1.2 Country-level: good country? All countries
		tmp = herb.data1[is.na(herb.data1$geo.check)&!is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$country)&!is.na(herb.data1$pais),]
		if(dim(tmp)[1]>0) {
			tmp1 = as.factor(tmp$country != tmp$pais)
			levels(tmp1) = c("ok_country","no_problem") 
			herb.data1$geo.check[is.na(herb.data1$geo.check)&!is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$country)&!is.na(herb.data1$pais)] = as.character(tmp1)
		} else { 	}
	   #1.3 State-level: good state? All countries
		tmp = herb.data1[herb.data1$country %in% lista.paises & !is.na(herb.data1$latitude.gazetteer) & !is.na(herb.data1$latitude.work) & !is.na(herb.data1$state) & !is.na(herb.data1$estado),]
		#tmp = herb.data1[herb.data1$country %in% c("argentina","brazil","paraguay")&!is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$state)&!is.na(herb.data1$estado),]
		if(dim(tmp)[1]>0) {
			tmp1 = as.factor(tmp$state != tmp$estado)
			levels(tmp1) = c("ok_state","no_problem") 
			herb.data1$geo.check[herb.data1$country %in% lista.paises & !is.na(herb.data1$latitude.gazetteer) & !is.na(herb.data1$latitude.work) & !is.na(herb.data1$state) & !is.na(herb.data1$estado)] = as.character(tmp1)	
		} else { 	}
	   #1.4 County-level. All countries
		tmp = herb.data1[herb.data1$country %in% lista.paises & !is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$county)&!is.na(herb.data1$municipio),]
		#tmp = herb.data1[herb.data1$country %in% c("argentina","brazil","paraguay")&!is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$county)&!is.na(herb.data1$municipio),]
		if(dim(tmp)[1]>0) {
			tmp1 = as.factor(tmp$county == tmp$municipio)
			levels(tmp1) = c(NA,"ok_county"); tmp1 = as.character(tmp1) 
			tmp1[is.na(tmp1)] = as.character(tmp$geo.check[is.na(tmp1)])
			herb.data1$geo.check[herb.data1$country %in% lista.paises & !is.na(herb.data1$latitude.gazetteer)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$county)&!is.na(herb.data1$municipio)] = tmp1
		} else { 	}
    #1.5 Calculating the distance between the original coordinates and the gazetter
		herb.data1$dist_m = NA
		tmp = herb.data1[herb.data1$geo.check%in%c("ok_country","ok_state","no_problem",NA)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$latitude.gazetteer),]
		if(dim(tmp)[1]>0) {
		  if(dim(tmp)[1]>10000) {
		    #for big checks
  		  no_cores = detectCores() - 1
  		  n <- 10 #number of dividision to the input data frame
  		  parts <- split(x = 1:dim(tmp)[1], f = cut(1:dim(tmp)[1], n))
  		  cl <- makeCluster(no_cores)
  		  clusterExport(cl, list("tmp","parts","n"),envir=environment())
  		  clusterEvalQ(cl, library(geosphere))
  		  overParts <- parLapply(cl = cl, X = 1:n, fun = function(x) {
  		    tmp1.1 = tmp[parts[[x]],]
  		    dist1 = rep(NA,dim(tmp1.1)[1])
  		    for (w in 1:length(dist1)) {
  		      dist1[w] = distm(c(tmp1.1$longitude.gazetteer[w],tmp1.1$latitude.gazetteer[w]),c(tmp1.1$longitude.work[w],tmp1.1$latitude.work[w]), fun = distHaversine)
  		    }
  		    tmp1.1$dist_m = dist1 
  		    gc() # release memory
  		    return(tmp1.1)
  		  })
  		  stopImplicitCluster()
  		  stopCluster(cl)
  		  tmp2 = do.call(rbind.data.frame, overParts)
  		  dist = tmp2$dist_m
  		} else {
  		  #for small checks
  		  dist = rep(NA,dim(tmp)[1])
  		  for (w in 1:length(dist)) { dist[w] = distm(c(tmp$longitude.gazetteer[w],tmp$latitude.gazetteer[w]),c(tmp$longitude.work[w],tmp$latitude.work[w]), fun = distHaversine)	}
  		} 
		  herb.data1$dist_m[herb.data1$geo.check%in%c("ok_country","ok_state","no_problem",NA)&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$latitude.gazetteer)] = dist	
		} else { 	}
	 #1.6 Cases with original coordinates but probably with a projection/precision issue (e.g. coordinates falling into the sea or bays)
		tmp = herb.data1[is.na(herb.data1$geo.check)&!is.na(herb.data1$country)&is.na(herb.data1$pais)&!is.na(herb.data1$dist_m)&herb.data1$origin.coord%in%"coord_original",]
		if(dim(tmp)[1]>0) {
			tmp2 = tmp$geo.check
			tmp2[tmp$dist_m<40000 & tmp$resolution.coord%in%c("degrees_only")] = "ok_county_coord_corrected"
			tmp2[tmp$dist_m<20000] = "ok_county_coord_corrected"
			tmp2[is.na(tmp2)] = "no_problem_in_water"
			herb.data1$geo.check[is.na(herb.data1$geo.check)&!is.na(herb.data1$country)&is.na(herb.data1$pais)&!is.na(herb.data1$dist_m)&herb.data1$origin.coord%in%"coord_original"] = as.character(tmp2)
		} else { 	}
	##2- Verification of coordinate problems: inversions and swaps
	  #2.1 Finding original coordinates too far away from their
		herb.data1$geo.check[herb.data1$geo.check%in%c("ok_country","ok_state")&herb.data1$dist_m>=100000&!is.na(herb.data1$county)&!is.na(herb.data1$dist_m)] = "no_problem_too_far"
		herb.data1$geo.check[is.na(herb.data1$geo.check)&is.na(herb.data1$pais)&!is.na(herb.data1$latitude.work)] = "no_problem_in_water" 
		#2.2 Creating the data frame with all possible variations of coordinates
		tmp = herb.data1[herb.data1$geo.check%in%c("no_problem","no_problem_in_water","no_problem_too_far")&!is.na(herb.data1$latitude.work)&!is.na(herb.data1$longitude.work),]
		if(dim(tmp)[1]>0) {
			tmp1 = tmp
			tmp1[,"latitude.work"] = -tmp1[,"latitude.work"] #inverting latitude
			tmp2 = tmp1
			tmp1 = tmp; tmp1[,"longitude.work"] = -tmp1[,"longitude.work"] #inverting longitude
			tmp2 = rbind.data.frame(tmp2,tmp1)
			tmp1 = tmp; tmp1[,c("longitude.work","latitude.work")] = -tmp1[,c("longitude.work","latitude.work")] #inverting both coordinates
			tmp2 = rbind.data.frame(tmp2,tmp1)
			tmp1 = tmp; tmp1[,c("longitude.work","latitude.work")] = tmp1[,c("latitude.work","longitude.work")] #swapping coordinates
			tmp2 = rbind.data.frame(tmp2,tmp1)
			tmp2$type.problem = c(rep("invert.lat",dim(tmp)[1]),rep("invert.long",dim(tmp)[1]),rep("invert.both",dim(tmp)[1]),rep("swap",dim(tmp)[1])) 
	   #2.3 Transforming points into spatial coordinates (points) and standardizing the projection
			coordinates(tmp2) <- c("longitude.work", "latitude.work")  # set spatial coordinates
			projection(tmp2) <- pj  # define projection system of our data
	   #2.4 Comparing the spatial data frame with the world country shapefile and creating the new locality columns
			tmp0.1 = over(tmp2, wo1, fn=NULL) ##PARALELLIZE according to the codes above (paralelliz only for big checkings!)
			tmp3 = cbind.data.frame(tmp2,pais1=tmp0.1$pais)
			tmp3$pais1 = as.character(tmp3$pais1)
			tmp3 = tmp3[!is.na(tmp3$pais1),] 
			if(dim(tmp3)[1]>0) {
				tmp3$estado1 = NA
				tmp3$municipio1 = NA
				tmp3$geo.check1 = NA
		   #2.4.1 Saving the new country information and cleaning the cases that remained as problems (country=NA)
				tmp3$geo.check1[tmp3$country==tmp3$pais1&!is.na(tmp3$country)&!is.na(tmp3$pais1)] = 
					paste("ok_country",as.character(tmp3$type.problem[tmp3$country==tmp3$pais1&!is.na(tmp3$country)&!is.na(tmp3$pais1)]),sep="_")
				tmp3 = tmp3[grepl("ok_country",tmp3$geo.check1),] 
	   #2.5 Comparing the spatial data frame with the selected country shapefiles
				tmp4 = tmp3[tmp3$pais1 %in% c("argentina","brazil","paraguay"),]
				if(dim(tmp4)[1]>0) {	
					coordinates(tmp4) <- c("longitude.work", "latitude.work")  # set spatial coordinates
					proj4string(tmp4) <- pj1  # define projection system of our data
					n1 = rep(NA,dim(tmp4@data)[1])
					n2 = rep(NA,dim(tmp4@data)[1]) 
					for (j in 1:length(countries)) {
						tmp1 = country.list[[j]]
						tmp2 = over(tmp4, tmp1, fn=NULL)
						n1[is.na(n1)&!is.na(as.character(tmp2$NAME_1))] = as.character(tmp2$NAME_1)[is.na(n1)&!is.na(as.character(tmp2$NAME_1))]
						n2[is.na(n2)&!is.na(as.character(tmp2$NAME_2))] = as.character(tmp2$NAME_2)[is.na(n2)&!is.na(as.character(tmp2$NAME_2))]
					}
		   #2.5.1 Saving the new state and county info retrieved and celanig the cases that remained as problems (county=NA) 
					tmp4@data$estado1 = n1
					tmp4@data$municipio1 = n2
					tmp4 = tmp4@data[!is.na(tmp4$estado1),]
		   #2.5.1 Updating the geographical coordinate validation classes  
					tmp4$geo.check1[tmp4$county==tmp4$municipio1&tmp4$state==tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)] = 
						paste("ok_county",as.character(tmp4$type.problem[tmp4$county==tmp4$municipio1&tmp4$state==tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)]),sep="_")
					tmp4$geo.check1[tmp4$county!=tmp4$municipio1&tmp4$state==tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)] = 
						paste("ok_county",as.character(tmp4$type.problem[tmp4$county!=tmp4$municipio1&tmp4$state==tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)]),sep="_")
					tmp4$geo.check1[tmp4$county!=tmp4$municipio1&tmp4$state!=tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)] = 
						paste("ok_county",as.character(tmp4$type.problem[tmp4$county!=tmp4$municipio1&tmp4$state!=tmp4$estado1&!is.na(tmp4$county)&!is.na(tmp4$municipio1)]),sep="_")
	   #2.6 Merging the results
					tmp3[tmp3$pais1 %in% c("argentina","brazil","paraguay"),c("estado1","municipio1","geo.check1")] = tmp4[,c("estado1","municipio1","geo.check1")]
	   #2.7 Recalculating dist_m for the the new coordinates
					dist = rep(NA,dim(tmp3)[1])
					for (w in 1:length(dist)) { dist[w] = distm(c(tmp3$longitude.gazetteer[w],tmp3$latitude.gazetteer[w]),c(tmp3$longitude.work[w],tmp3$latitude.work[w]), fun = distHaversine)	}
					tmp3$dist_m1 = dist	
				} else { 
					if(dim(tmp3)[1]>0) { 
					tmp3$dist_m1 = NA 
	   #2.8 Saving the results  
					tmp5 = left_join(herb.data1,tmp3[,c("order","latitude.work","longitude.work","geo.check1","pais1","estado1","municipio1","dist_m1")],by="order")		
					herb.data1[!is.na(tmp5$pais1),c("latitude.work","longitude.work","geo.check","pais","estado","municipio","dist_m")] = 
						tmp5[!is.na(tmp5$pais1),c("latitude.work.y","longitude.work.y","geo.check1","pais1","estado1","municipio1","dist_m1")]
					} else { }
				}
			} else { 	}
		} else { 	}
	##3- Final decisions and classification of particular cases based on coordinates resolution and distance to the reference/gazetteer coordinate
	   #3.1 Cases with original coordinates but probably low precision (seconds not explicitly defined)
		#max. distance due to minutes rounding = 2.624km (equator) to 2.199km (latitude 50)
		#max. distance due to degrees rounding = 78.714km (equator) to 66.065km (latitude 50)
		#quantiles of the distribution od dist_m for "ok_county"
		#tmp = herb.data1[herb.data1$geo.check %in% c("ok_county") & herb.data1$state%in%"bahia",]
		#dist = rep(NA,dim(tmp)[1])
		#for (w in 1:length(dist)) { dist[w] = distm(c(tmp$longitude.gazetteer[w],tmp$latitude.gazetteer[w]),c(tmp$longitude.work[w],tmp$latitude.work[w]), fun = distHaversine)	}
		#quantile(dist)	
		
		#for Bahia (large county-size AF state - max. limits), up to: 
			#2.5km (same), 5km (very close), 10km (close), 20km (not_too_far), 40km (far), >100km (problem)
 		tmp = herb.data1[herb.data1$geo.check%in%"ok_state"&!is.na(herb.data1$dist_m)&!is.na(herb.data1$county)&!is.na(herb.data1$municipio),]
		if(dim(tmp)[1]>0) {
			tmp$geo.check[tmp$dist_m<2500 & tmp$resolution.coord%in%c("seconds","seconds?")] = "ok_county_very_close"
			tmp$geo.check[tmp$dist_m<5000 & tmp$resolution.coord%in%c("minutes_only","degrees_only")] = "ok_county_very_close"
			tmp$geo.check[tmp$dist_m<5000 & tmp$resolution.coord%in%c("seconds","seconds?") & tmp$geo.check%in%"ok_state"] = "ok_county_close"
			tmp$geo.check[tmp$dist_m<10000 & tmp$resolution.coord%in%c("minutes_only","degrees_only") & tmp$geo.check%in%"ok_state"] = "ok_county_close"
			tmp$geo.check[tmp$dist_m<10000 & tmp$resolution.coord%in%c("seconds","seconds?") & tmp$geo.check%in%"ok_state"] = "ok_county_not_too_far"
			tmp$geo.check[tmp$dist_m<20000 & tmp$resolution.coord%in%c("minutes_only","degrees_only") & tmp$geo.check%in%"ok_state"] = "ok_county_not_too_far"
			tmp$geo.check[tmp$dist_m>=10000 & tmp$dist_m<20000 & tmp$resolution.coord%in%c("seconds","seconds?") & tmp$geo.check%in%"ok_state"] = "ok_county_name_mistaken?"
			tmp$geo.check[tmp$dist_m>=20000 & tmp$dist_m<100000 & tmp$resolution.coord%in%c("seconds","seconds?") & tmp$geo.check%in%"ok_state"] = "no_too_far"
			tmp$geo.check[tmp$dist_m>=40000 & tmp$dist_m<100000 & tmp$resolution.coord%in%c("minutes_only","degrees_only") & tmp$geo.check%in%"ok_state"] = "no_too_far"
			herb.data1$geo.check[herb.data1$geo.check%in%"ok_state"&!is.na(herb.data1$dist_m)&!is.na(herb.data1$county)&!is.na(herb.data1$municipio)] = tmp$geo.check
		} else { 	}
	##4 - Replacing the original coordinates by the gazetter coordinates for selected verification classes
	   #4.1 Coordinates falling into the sea ("ok_county_coord_corrected")
		herb.data1[herb.data1$geo.check%in%"ok_county_coord_corrected",c("latitude.work","longitude.work")] = 
			herb.data1[herb.data1$geo.check%in%"ok_county_coord_corrected",c("latitude.gazetteer","longitude.gazetteer")]
		herb.data1$origin.coord[herb.data1$geo.check%in%"ok_county_coord_corrected"] = "correction_coord_original"
		herb.data1$resolution.coord[herb.data1$geo.check%in%"ok_county_coord_corrected"] = "seconds_centroid"
	   #4.2 Problematic coordinates ("problem","problem_in_water","problem_too_far")
		herb.data1[grepl("no_problem",herb.data1$geo.check)&!is.na(herb.data1$country),c("latitude.work","longitude.work")] = 
			herb.data1[grepl("no_problem",herb.data1$geo.check)&!is.na(herb.data1$country),c("latitude.gazetteer","longitude.gazetteer")]
		herb.data1$origin.coord[grepl("no_problem",herb.data1$geo.check)&!is.na(herb.data1$country)] = "correction_coord_original"
		herb.data1$resolution.coord[grepl("no_problem",herb.data1$geo.check)&!is.na(herb.data1$country)] = "seconds_centroid"
		herb.data1$geo.check[grepl("no_problem",herb.data1$geo.check)&!is.na(herb.data1$country)] = "ok_county_problem_coord_corrected"
	   #4.3 Original coodinates between 40-100km of the centroid of the county ("ok_county_far")
		#Correct only in the case of coordinates at "degrees_only" resolution
		herb.data1[herb.data1$geo.check%in%"no_too_far"&!is.na(herb.data1$county)&herb.data1$resolution.coord%in%"degrees_only",c("latitude.work","longitude.work")] =
			herb.data1[herb.data1$geo.check%in%"no_too_far"&!is.na(herb.data1$county)&herb.data1$resolution.coord%in%"degrees_only",c("latitude.gazetteer","longitude.gazetteer")]
		ids =  herb.data1$order[herb.data1$geo.check%in%"no_too_far"&!is.na(herb.data1$county)&herb.data1$resolution.coord%in%"degrees_only"]
		herb.data1$geo.check[herb.data1$order%in%ids] = "ok_county_coord_corrected"
		herb.data1$origin.coord[herb.data1$order%in%ids] = "correction_coord_original"
		herb.data1$resolution.coord[herb.data1$order%in%ids] = "seconds_centroid"
	   #4.4 Coordinates with county info but with problematic original corodinates ("ok_state")
		#herb.data1[herb.data1$geo.check%in%"ok_state" & herb.data1$origin.coord%in%"coord_gazet_state",c("latitude.work","longitude.work")] =
		#	herb.data1[herb.data1$geo.check%in%"ok_state" & herb.data1$origin.coord%in%"coord_gazet_state",c("latitude.gazetteer","longitude.gazetteer")] 
		#Nothing to correct; but the gazetteer can include more variants and localities to avoid this problems

	### SAVING THE VALIDATION RESULTS ###
	   #Inpecting if the order of both objects match
		#table(herb.data$order == herb.data1$order)
	   #Updating the existing columns
		herb.data[,c("latitude.work","longitude.work","origin.coord","resolution.coord")] = herb.data1[,c("latitude.work","longitude.work","origin.coord","resolution.coord")]
	   #Saving the new columns of the geo-validation process and assigning the classes/precision to the coordinates from the gazetteer
		herb.data$geo.check = herb.data1$geo.check
		herb.data$resolution.coord[herb.data$origin.coord%in%"ok_country_gazet"] = "seconds_centroid"
		herb.data$resolution.coord[herb.data$origin.coord%in%"ok_state_gazet"] = "seconds_centroid"
		herb.data$resolution.coord[herb.data$origin.coord%in%"ok_county_gazet"] = "seconds_centroid"
		herb.data$resolution.coord[herb.data$origin.coord%in%"ok_locality_gazet"] = "seconds_point"
		#table(herb.data$geo.check,herb.data$origin.coord)
	 #Saving the new columns with the location and situation from the geographical coordinates
		dic1 = dic[!duplicated(dic$loc)&!is.na(dic$loc.correct) & !dic$resolution.gazetteer%in%c("localidade","localidade|sublocalidade","distrito|vila","distrito|bairro","cachoeira","mina","vila","serra"),]
		loc = herb.data1[,c("order","loc.coord")]
		names(loc)[2] = "loc"
		if(any(!is.na(loc$loc))) {
			tmp = left_join(loc,dic1[,c("loc","loc.correct","AtlanticForest_Lei11428")],by="loc")
			#table(tmp$order==herb.data$order)
			herb.data$loc.coord = tmp$loc.correct
			herb.data$AtlanticForest_Lei11428.coord = tmp$AtlanticForest_Lei11428
			} else {
			herb.data$loc.coord = NA
			herb.data$AtlanticForest_Lei11428.coord = NA
			}
	   #Creating the new file path and saving in the local folder
		path.csv = gsub("coord-tax.csv","coord-tax-geo.csv",myfiles0)
		write.csv(x=herb.data,file=path.csv, row.names = FALSE)
		cat(i,"\n")
}

##Studying possible distance due to coordinate precision 
#Degrees: maximum distance of 157km (equator) to 132km (at 50latS)
distm(c(-39,-50),c(-40,-51), fun = distHaversine)
distm(c(-39,0),c(-40,-1), fun = distHaversine)
#Minutes: maximum distance of 2.62km (equator) to 2.21km (at 50latS)
distm(c(-40.01666667,0.00000),c(-40.00,-0.01666667), fun = distHaversine)
distm(c(-40.01666667,-50.00000),c(-40.00,-50.01666667), fun = distHaversine)
#Seconds: maximum distance of 37 m (equator) to 44 m (at 50latS)  
distm(c(-40.0002777778,0.00000),c(-40.00,-0.0002777778), fun = distHaversine)
distm(c(-40.0002777778,-50.00000),c(-40.00,-50.0002777778), fun = distHaversine)


##Compare results with data cleaning using 'speciesgeocodeR'


########################################################################################################################################################################################-
########################################################################################################################################################################################-
#####################################################################
#### 2 - PLOTTING OCCURRENCES AGAINST THE ATLANTIC FOREST LIMITS ####
#####################################################################

### Loading the and projecting Atlantic Forest limits (ARG, BRA, PAY) ###
path = "E://ownCloud//W_GIS"
#Lei MA + TNC para ARG+PAR
af = readOGR(dsn=paste(path,"//AF_limites_milton",sep=""),layer="merge_limites_MA11428_TNC_ARG_BR_PAR")
#Biomas MMA + TNC para ARG+PAR
af1 = readOGR(dsn=paste(path,"//AF_limites_milton",sep=""),layer="merge_limites_BiomasMMA_TNC_ARG_BR_PAR")
pj = crs(af1)

#Brazilian map
path0 = "E://ownCloud//W_GIS//Am_Lat_ADM_GADM_v3.6//"
path0 = list.files(path0,full.name=TRUE)
path0 = path0[grep("BRA_0",path0)]
brasil =	readRDS(path0)
pj1 = crs(brasil)

##CHANGE PROJECTION FOR SIRGAS_2000?? IT DID NOT CHANGED THE CONTOURS
#SIRGAS 2000
#pj2 = CRS("+init=epsg:4674 +proj=longlat +ellps=GRS80 +units=m +no_defs") 
#brasil1 <- spTransform(brasil, pj2)
#plot(brasil)
#plot(brasil1,add=T,border="red")

### Uploading the final/edited version of the gazetter, with missing counties and ortographical variants of some names (not exaustive)
dic = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//gazetteer.csv",as.is=TRUE)
dic = dic[dic$status %in% "ok",]

#Finding the AF states
estados.af = unique(dic$loc.correct[dic$resolution.gazetteer%in%"state" & dic$AtlanticForest_Lei11428 %in% "AtlanticForest"])
estados.other = unique(dic$loc.correct[dic$resolution.gazetteer%in%"state" & dic$AreaAF.prop_Lei11428 %in% 0])

##Getting the file paths for the herbarium data
splink = read.csv("speciesLink.csv", as.is=T)
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE)
paths1 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new"
paths2 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot"
paths3 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//sndb_mincyt_arg"
#paths1 = dir(paths1,full.names=TRUE)[!dir(paths1) %in% splink$acronym]
#paths2 = dir(paths2,full.names=TRUE)[!dir(paths2) %in% splink$acronym]
paths1 = dir(paths1,full.names=TRUE)
paths2 = dir(paths2,full.names=TRUE)
paths3 = dir(paths3,full.names=TRUE)

paths = c(paths,paths1,paths2,paths3)
paths = list.files(paths,full.names=TRUE)
paths = c(paths, list.files("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE))
paths = paths[grep("-edited-cleaned-loc-coord-tax-geo.csv",paths)] 

### Validating if the occurrenceas are inside or outside the Atlantic Forest map ###
for(i in 1:length(paths)) {
  	### PRE-VALIDATION STEPS ###
	##Loading the occurrence data
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,header=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns
		cols = c("loc.correct","resolution.gazetteer","AtlanticForest_Lei11428",
				"origin.coord","resolution.coord","latitude.work","longitude.work",
				"geo.check","loc.coord","AtlanticForest_Lei11428.coord")
		cls =  unique(cols[cols %in% names(herb.data)])
		herb.data1 = herb.data[,cls]
		herb.data1$order = 1:dim(herb.data1)[1] #putting the data back on its original order
	##Creating the AF inside/outside column
		herb.data1$af = NA
	##1 - ORIGINAL COORDINATES, VALIDATED AT COUNTY LEVEL
	   #Getting data frame with spatial coordinates (points) and standardizing the projection
		tmp = herb.data1[herb.data1$origin.coord%in%"coord_original"& grepl("^ok_county",herb.data1$geo.check) & !is.na(herb.data1$latitude.work) & !is.na(herb.data1$longitude.work) ,]
		if(dim(tmp)[1]>0) {
			coordinates(tmp) <- c("longitude.work", "latitude.work")  # set spatial coordinates
			proj4string(tmp) <- pj  # define projection system of our data
	   #Comparing the spatial data frame with the Atlantic Forest shapefile
			tmp0 = over(tmp, af, fn=NULL)
			#tmp1 = over(tmp, brasil, fn=NULL)
			tmp$af = !is.na(tmp0$OBJECTID) ## TRUE and FALSE classes
		 #Inside the AF limits but in a county in the transition to other biomes? => TRUE_in_transition
			tmp$af[tmp$af & tmp$AtlanticForest_Lei11428.coord %in% c("others","transition")] = "TRUE_in_transition"
		 #Outside the AF limits but in a county in the transition to the Atlantic Forest? => FALSE_in_transition
			tmp$af[tmp$af %in% "FALSE" & tmp$AtlanticForest_Lei11428.coord %in% c("AtlanticForest_predominant","transition")] = "FALSE_in_transition"
	   #Saving the new AF information
			herb.data1$af[herb.data1$origin.coord%in%"coord_original"& grepl("^ok_county",herb.data1$geo.check) & !is.na(herb.data1$latitude.work) & !is.na(herb.data1$longitude.work)] =  tmp$af
		} else { 	}
	##2 - ORIGINAL COORDINATES, VALIDATED AT STATE OR COUNTRY LEVELS
	   #Getting data frame with the occurrences
		tmp = herb.data1[herb.data1$origin.coord%in%"coord_original" & grepl("^ok_state|^ok_country",herb.data1$geo.check) & !is.na(herb.data1$latitude.work) ,]
	   #Validating occurrences inside the AF states and non AF states
		if(dim(tmp)[1]>0) {
		  #note: mato grosso do sul and brazil_parana and brazil_paraiba have the same start as mato grosso and para
			tmp$af[grepl(paste(estados.other,collapse="|"),tmp$loc.correct)] = FALSE
			tmp$af[grepl(paste(estados.af,collapse="|"),tmp$loc.correct)] = TRUE
			#correcting problems of mato grosso do sul, brazil_parana and brazil_paraiba
			tmp$af[grepl("^brazil_mato grosso sul|brazil_parana|brazil_paraiba",tmp$loc.correct) & tmp$af %in% FALSE] = NA
	   #Validating occurrences outside the AF countries
			tmp$af[!grepl("^brazil|^argentina|^paraguay",tmp$loc.correct)] = FALSE
			#tmp$af[!grepl("^brazil|^argentina|^paraguay",tmp$loc.correct) & grepl("^ok_country",tmp$geo.check)] = FALSE
	   #Saving the new AF information
			herb.data1$af[herb.data1$origin.coord%in%"coord_original" & grepl("^ok_state|^ok_country",herb.data1$geo.check) & !is.na(herb.data1$latitude.work)] = tmp$af
		} else { 	}
	##3 - COORDINATES FROM THE GAZETTER, AT COUNTY AND LOCALITY LEVEL
	   #Getting data frame with the occurrences
		tmp = herb.data1[herb.data1$origin.coord%in%c("ok_county_gazet","ok_locality_gazet","correction_coord_original") & !grepl("^ok_state|^ok_country",herb.data1$geo.check),]
	   #Validating occurrences inside the AF counties and non AF counties
		if(dim(tmp)[1]>0) {
			tmp$af[tmp$AtlanticForest_Lei11428 %in% "AtlanticForest" & tmp$resolution.gazetteer %in% c("county","locality")] = TRUE
			tmp$af[tmp$AtlanticForest_Lei11428 %in% "AtlanticForest_predominant" & tmp$resolution.gazetteer %in% c("county","locality")] = "TRUE_in_transition"
			tmp$af[tmp$AtlanticForest_Lei11428 %in% "others" & tmp$resolution.gazetteer %in% c("county","locality")] = FALSE
			tmp$af[tmp$AtlanticForest_Lei11428 %in% "transition" & tmp$resolution.gazetteer %in% c("county","locality")] = "FALSE_in_transition"
	   #Saving the new AF information
			herb.data1$af[herb.data1$origin.coord%in%c("ok_county_gazet","ok_locality_gazet","correction_coord_original") & !grepl("^ok_state|^ok_country",herb.data1$geo.check)] = tmp$af
		} else { 	}
	##4 - COORDINATES FROM THE GAZETTER, AT STATE AND COUNTRY LEVELS
	   #Getting data frame with the occurrences
		tmp = herb.data1[is.na(herb.data1$af)&herb.data1$origin.coord%in%c("ok_state_gazet","ok_country_gazet"),]
	   #Validating occurrences inside the AF states and non AF states
		if(dim(tmp)[1]>0) {
			tmp$af[grepl(paste(estados.af,collapse="|"),tmp$loc.correct)] = TRUE
			tmp$af[grepl(paste(estados.other,collapse="|"),tmp$loc.correct)] = FALSE
			#correcting problems of mato grosso do sul, brazil_parana and brazil_paraiba
			tmp$af[grepl("^brazil_mato grosso sul|brazil_parana|brazil_paraiba",tmp$loc.correct) & tmp$af %in% FALSE] = NA
	   #Saving the new AF information
			herb.data1$af[is.na(herb.data1$af)&herb.data1$origin.coord%in%c("ok_state_gazet","ok_country_gazet")] = tmp$af
		} else { 	}
	##5 - FINAL ASSIGNMENTS FOR COORDINATES NOT VALIDATED
	   #Getting data frame with the occurrences
		tmp = herb.data1[is.na(herb.data1$af) & herb.data1$resolution.gazetteer%in%c("county","locality") & herb.data1$geo.check%in%"ok_state",]
	   #Both location and coordinate provided falling inside/outside the AF?
		if(dim(tmp)[1]>0) {
			tmp$af[tmp$AtlanticForest_Lei11428 %in% "AtlanticForest" & tmp$AtlanticForest_Lei11428.coord %in% "AtlanticForest"] = TRUE
			tmp$af[tmp$AtlanticForest_Lei11428 %in% "AtlanticForest_predominant" & tmp$AtlanticForest_Lei11428.coord %in% "AtlanticForest"] = "TRUE_in_transition"
			tmp$af[tmp$AtlanticForest_Lei11428 %in% "others" & tmp$AtlanticForest_Lei11428.coord %in% "others"] = FALSE
			#tmp$af[tmp$AtlanticForest_Lei11428 %in% "transition" & tmp$AtlanticForest_Lei11428.coord %in% "AtlanticForest"] = "FALSE_in_transition"
	   #Saving the new AF information
			herb.data1$af[is.na(herb.data1$af) & herb.data1$resolution.gazetteer%in%c("county","locality") & herb.data1$geo.check%in%"ok_state"] = tmp$af
		} else { 	}
	##6- CORRECTING FINAL PROBLEMS
		#6.1 occurrences falling into the sea but close to the Atlantic Forest
	   #Getting data frame with the occurrences
		tmp = herb.data1[!herb.data1$af%in%TRUE & herb.data1$origin.coord%in%"coord_original" & grepl("^ok_county",herb.data1$geo.check) & herb.data1$AtlanticForest_Lei11428 %in%"AtlanticForest" & herb.data1$AtlanticForest_Lei11428.coord %in%"AtlanticForest" ,]
  	   #Validating occurrences inside the AF counties
		if(dim(tmp)[1]>0) {
			tmp$af = TRUE
	   #Saving the new AF information
			herb.data1$af[!herb.data1$af%in%TRUE & herb.data1$origin.coord%in%"coord_original" & grepl("^ok_county",herb.data1$geo.check) & herb.data1$AtlanticForest_Lei11428 %in%"AtlanticForest" & herb.data1$AtlanticForest_Lei11428.coord %in%"AtlanticForest"] = tmp$af
		} else { 	}
		#6.2 original coordinates corrected due to a problem in the location description or in the coordinates (check later if needed)
		 #Getting data frame with the occurrences
		tmp = herb.data1[is.na(herb.data1$af) & herb.data1$origin.coord%in%"correction_coord_original" & grepl("ok_county_problem_coord_corrected",herb.data1$geo.check) & !is.na(herb.data1$AtlanticForest_Lei11428) ,]
		 #Validating occurrences inside the AF counties
		if(dim(tmp)[1]>0) {
		  tmp$af = "CHECK_state_name_or_coordinate_mistake?"
		  #Saving the new AF information
		  herb.data1$af[is.na(herb.data1$af) & herb.data1$origin.coord%in%"correction_coord_original" & grepl("ok_county_problem_coord_corrected",herb.data1$geo.check) & !is.na(herb.data1$AtlanticForest_Lei11428)] = tmp$af
		} else { 	}
	##Inspecting and saving the results
		#table(herb.data1[is.na(herb.data1$af),]$geo.check,herb.data1[is.na(herb.data1$af),]$origin.coord)
		#table(herb.data1[is.na(herb.data1$af),]$geo.check,herb.data1[is.na(herb.data1$af),]$AtlanticForest_Lei11428)
		#table(herb.data$order == herb.data1$order)
		herb.data$af.check = herb.data1$af
		herb.data$af.check[is.na(herb.data$af.check)] = "cannot_check"
		herb.data = herb.data[,!names(herb.data)%in%c("loc.coord","AtlanticForest_Lei11428.coord")]		
	   #Creating the new file path and saving in the local folder
		path.csv = gsub("coord-tax-geo.csv","coord-tax-geo-af.csv",myfiles0)
		write.csv(x=herb.data,file=path.csv, row.names = FALSE)
		cat(i,"\n")
}




########################################################################################################################################################################################
########################################################################################################################################################################################
##########################################################################################
#### CREATING COLLECTION AND SPECIMEN UNIQUE IDENTIFIERS AND PREPARING DATA FOR MERGE ####
##########################################################################################

##Getting the file paths for the herbarium data
splink = read.csv("speciesLink.csv", as.is=T)
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE)
paths1 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new"
paths2 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot"
paths3 = "C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//sndb_mincyt_arg"
#paths1 = dir(paths1,full.names=TRUE)[!dir(paths1) %in% splink$acronym]
#paths2 = dir(paths2,full.names=TRUE)[!dir(paths2) %in% splink$acronym]
paths1 = dir(paths1,full.names=TRUE)
paths2 = dir(paths2,full.names=TRUE)
paths3 = dir(paths3,full.names=TRUE)

paths = c(paths,paths1,paths2,paths3)
paths = list.files(paths,full.names=TRUE)
paths = c(paths, list.files("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE))
paths = paths[grep("-edited-cleaned-loc-coord-tax-geo-af.csv",paths)] 

### Creating specimen strings and standardizing columns for each collection ###
for(i in 1:length(paths)) {
	### PRE-VALIDATION STEPS ###
	##Loading the occurrence data
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,header=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns
		cols = c("siglacolecao","numtombo","collectionCode","catalogNumber","collectioncode","catalognumber",
				"family.correct","species.correct","coletor.name","coletor.last.name","coletor.number",
				"loc.correct","cidade","county","municipality","locality","descrlocal",
				"anocoleta","year","yearcollected",
				"anodeterm","dateIdentified","yearidentified")
		cls =  unique(cols[cols %in% names(herb.data)])
		herb.data1 = herb.data[,cls]
		##Replacing/removing locality fields: 'county' by 'municipality'
		if(sum(names(herb.data1) %in% c("county","municipality"))==2) { 
		  herb.data1$county[is.na(herb.data1$county)|as.character(herb.data1$county) %in% c("","-")] = as.character(herb.data1$municipality[is.na(herb.data1$county) | as.character(herb.data1$county) %in% c("","-")])
		  herb.data1$municipality[is.na(herb.data1$municipality)|as.character(herb.data1$municipality) %in% c("","-")] = as.character(herb.data1$county[is.na(herb.data1$municipality)|as.character(herb.data1$municipality) %in% c("","-")])
		  herb.data1 = herb.data1[,-which(names(herb.data1)=="county")]
		}
		#Renaming columns
		names(herb.data1)[(grep("loc.correct",names(herb.data1))+1)] = "localidade"
		names(herb.data1)[1] = "colecao"
		names(herb.data1)[2] = "tombo"
		if(any(grepl("anodeterm|dateIdentified|yearidentified",names(herb.data1)))) { 
			if(names(herb.data1)[dim(herb.data1)[2]]=="dateIdentified") {
				names(herb.data1)[(dim(herb.data1)[2]-1)] = "ano"
				names(herb.data1)[(dim(herb.data1)[2])] = "ano.det"
			} else {
				names(herb.data1)[(dim(herb.data1)[2]-1)] = "ano"
				names(herb.data1)[(dim(herb.data1)[2])] = "ano.det"
			}	
		} else { 
			names(herb.data1)[(dim(herb.data1)[2])] = "ano"
			herb.data1$ano.det = NA 
		}
		herb.data1$order = 1:dim(herb.data1)[1] #string to put the data back on its original order
	##Editing the county column
		city = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), herb.data1$localidade))
		city = gsub("-"," ",city)
		city = gsub(" - "," / ",city)
		city = gsub(" do | da | de "," ",city)
		city = gsub(" dos | das | des "," ",city)
		city = gsub(' d\' | d\'| d´| d\"| d’'," ",city)
		city = gsub(' d\' | d\'| d´| d\"| d’'," ",city)
		city = gsub('\\.$',"",city)
		city = gsub(" dx "," ",city)
		city = gsub("esao pauloirito santo","espirito santo",city)
		city = gsub ('alagoas_s\\. j\\. lage|alagoas_s\\. j\\. laje|alagoas_s\\.j lage|alagoas_s\\.j\\. lage|alagoas_s\\.j\\. laje|alagoas_s\\.j\\.da laje|alagoas_s\\.j\\.da lage',"alagoas_sao jose laje",city)
		city = gsub ('vila \\.b\\.s\\. trindade|vila bela s\\. trindade|vila bela santis\\. trindade|vila bela santis\\.trindade|vila bela sant\\. trindade|vila bela sm\\. trindade|vila bela stma\\. trindade',"vila bela santissima trindade",city)
		city[city%in%"brazil_sao paulo_sjrpreto"] = "brazil_sao paulo_sao jose rio preto"
		city = gsub ('brazil_guanabara_rio janeiro',"brazil_rio janeiro_rio janeiro",city)
	##Defining the new county column		
		tmp = strsplit(herb.data1$loc.correct,"_")
		id = sapply(tmp,length)
		n2 = rep(NA,dim(herb.data1)[1])
		if(any(id>=3)) n2[id>=3] = sapply(tmp[id>=3],function(x) x[3])
		herb.data1$municipio = n2
		herb.data1$municipio[is.na(herb.data1$municipio)&!is.na(city)] = as.character(city[is.na(herb.data1$municipio)&!is.na(city)])
	
	##Creating the collection unique identifier
		col = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), herb.data1$colecao))
		tmb = tolower(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), herb.data1$tombo))
		#Edting acessions with characters other than numbers
		tmp1 = tmb[grepl('\\D',tmb)]
		if(length(tmp1)>0) {
  		#no number? than NA
  		tmp1[!grepl('\\d',tmp1)] = NA
  		#removing separators and exclamation points
  		#tmp1 = gsub('-|!','',tmp1) #is this step really necessary? not sure, so I am not doing it
  		#removing the name of the collection and any other letters from the beggining of the accession number
  		#sometimes accessions have a letter in front of the number
  		pos = sapply(tmp1,function (x) gregexpr(pattern ='\\d',x)[[1]][1])
  		if(any(!is.na(pos))) {
    		pos1 = pos[!is.na(pos)&pos>1]
    		tmp2 = tmp1[!is.na(pos)&pos>1]
    		if(grepl('/gbif/',myfiles0)) {
    		  tmp2 = substring(tmp2,pos1,nchar(tmp2))
    		} else {
    		  clc = paste(paste("^",unique(col),sep=""),collapse ="|")
    		  tmp2[grepl(clc,tmp2)] = gsub(unique(col),'',tmp2[grepl(clc,tmp2)])
    		} 
    		tmp2[grepl(pattern ='\\d',tmp2) & !grepl(pattern ='\\D',tmp2)] = as.numeric(tmp2[grepl(pattern ='\\d',tmp2) & !grepl(pattern ='\\D',tmp2)])
    		#tmp2[grepl('[a-z]',tmp2,ignore.case=TRUE)]
    		tmp1[!is.na(pos)&pos>1] = tmp2
  		}	
  		#removing zeros from the begining of the numerical string
  		pos = sapply(tmp1,function (x) gregexpr(pattern ='\\D',x)[[1]][1])
  		pos[is.na(pos)] = 0
  		if(any(!is.na(pos)) & any(pos>=1)) {
    		pos1 = pos[!is.na(pos)&pos>=1]
    		tmp2 = tmp1[!is.na(pos)&pos>=1]
    		tmp3 = sapply(1:length(tmp2), function(x) {
    		        inicio1 = inicio2 = pos1[x]
    		        if(pos1[x]==1) inicio1 = 1 else inicio1 = pos1[x]-1
    		        if(pos1[x]==1) inicio2 = pos1[x]+1  else inicio2 = pos1[x]
    		        parte1 = substring(tmp2[x], 1, inicio1)
    		        if(!grepl(pattern ='\\D',parte1)) parte1 = as.numeric(parte1) 
    		        parte2 = substring(tmp2[x], inicio2)
    		        if(!grepl(pattern ='\\D',parte2)) parte2 = as.numeric(parte2)
    		        result = paste(str_trim(parte1),str_trim(parte2),sep="")
    		        return(result)
                }
    		  )
    		tmp1[!is.na(pos)&pos>=1] = tmp3
    	}
  	tmb[grepl('\\D',tmb)] = tmp1
		}
		#Edting acessions only with numbers
		tmb[!grepl('\\D',tmb)] = as.numeric(tmb[!grepl('\\D',tmb)])
		#tmb[grepl('[a-z]',tmb,ignore.case=TRUE)]
		#Remove GBIF data without accession only with a gbifID?? Yes!
		tmb[!is.na(tmb) & str_count(tmb,"-")>=3] = NA
		#Creating the final/edited string of the collection+accession number		
		herb.data1$numTombo = paste(col,tmb,sep="_")

	##Creating the collection year field
		if(any(grepl("ano.det",names(herb.data1)))) {
			tmp = herb.data1$ano.det
			tmp[is.na(tmp)] = "s.d."
			tmp = gsub("T00:00:00Z", "", tmp)
			if(any(!tmp == "s.d.")) {
  			#Romain months
  			tmp = gsub('XII','12',tmp); tmp = gsub('XI','11',tmp);tmp = gsub('IX','09',tmp);tmp = gsub('X','10',tmp) 
  			tmp = gsub('VIII','08',tmp); tmp = gsub('VII','07',tmp); tmp = gsub('VI','06',tmp); tmp = gsub('IV','04',tmp); tmp = gsub('V','05',tmp)
  			tmp = gsub('III','03',tmp); tmp = gsub('II','02',tmp); tmp = gsub('I','01',tmp)
  			#years separated by points or commas
  			tmp1 = tmp[grepl('^[0-9]\\.|^[0-9],',tmp)]
  			tmp1 = as.character(sapply(strsplit(tmp1,"\\.|,"),function(x) paste(x,collapse = "")))
  			if(length(tmp1)>0) tmp[grepl('^[0-9]\\.|^[0-9],',tmp)] = tmp1
  			#complete dates, separated by slashs
  			tmp1 = tmp[grepl('\\/',tmp)]
  			tmp1 = as.character(sapply(strsplit(tmp1,"\\/"),function(x) x[nchar(x)>=4]))
  			if(length(tmp1)>0) tmp[grepl('\\/',tmp)] = tmp1
  			#complete dates, not separated by '-'
  			tmp1 = tmp[nchar(tmp)%in%8 & !grepl("-",tmp)]
  			tmp1 = substr(tmp1,5,8)
  			if(length(tmp1)>0) tmp[nchar(tmp)%in%8 & !grepl("-",tmp)] = tmp1
  			tmp1 = tmp[nchar(tmp)%in%7]
  			tmp1 = substr(tmp1,4,7)
  			if(length(tmp1)>0) tmp[nchar(tmp)%in%7 & !grepl("-",tmp)] = tmp1
  			tmp1 = tmp[nchar(tmp)%in%6]
  			tmp1 = substr(tmp1,3,6)
  			if(length(tmp1)>0) tmp[nchar(tmp)%in%6 & !grepl("-",tmp)] = tmp1
  			tmp1 = tmp[nchar(tmp)%in%5]
  			tmp1 = substr(tmp1,2,5)
  			if(length(tmp1)>0) tmp[nchar(tmp)%in%5 & !grepl("-",tmp)] = tmp1
  			#dates separeted by '-' (only numbers)
			tmp1 = tmp[grepl("-",tmp) & !grepl('[a-z]', tmp, ignore.case = TRUE)]
  			tmp1 = as.character(sapply(strsplit(tmp1,"-"), function(x) unique(x[nchar(str_trim(x))>=4])))
			if(length(tmp1)>0) tmp[grepl("-",tmp) & !grepl('[a-z]', tmp, ignore.case = TRUE)] = tmp1
		     #dates separeted by '-' (numbers and letters)
			tmp1 = tmp[grepl("-",tmp) & grepl('[a-z]', tmp, ignore.case = TRUE)]
			tmp1 = as.character(sapply(strsplit(tmp1,"-"), function(x) x[grepl('\\d', x, ignore.case = TRUE)]))
			if(length(tmp1)>0) tmp[grepl("-",tmp) & grepl('[a-z]', tmp, ignore.case = TRUE)] = tmp1
 			#spaces
  			tmp1 = tmp[grepl(" ",tmp)]
    			tmp1 = as.character(sapply(strsplit(tmp1," "),function(x) unique(x[nchar(x)>=4])))
    			if(length(tmp1)>0) tmp[grepl(" ",tmp)] = tmp1
    		#years in the bad format
    		#tmp1 = tmp[nchar(tmp)%in%c(1,2)]
    		#tmp1 = "s.d."
    		#if(length(tmp1)>0) tmp[nchar(tmp)%in%c(1,2)] = tmp1
			}	
    		#Saving the edits
			herb.data1$ano.det = tmp
		} else { }
	##Final edits to avoid misleading duplicates
	   #Collector number	
		numb = herb.data1$coletor.number
		numb[numb %in% 0] = "s.n."
		numb[!grepl('[0-9]',numb)] = "s.n."
		numb = gsub("--","-",numb)
		numb = gsub("&nf;","",numb)
		numb[grepl("c\\(\\\"",numb)] = gsub("c\\(\\\"","",numb[grepl("c\\(\\\"",numb)]) 
		numb[grepl("\\\",-\\\"",numb)] = gsub("\\\",-\\\"","-",numb[grepl("\\\",-\\\"",numb)]) 
		numb[grepl("\\\"\\)",numb)] = gsub("\\\"\\)","",numb[grepl("\\\"\\)",numb)]) 
		#numb = gsub('[a-z]-[0-9]','',numb, ignore.case=TRUE) ##CHECK
	   #Collector last name
		nome = herb.data1$coletor.last.name
		nome[!is.na(nome)&nchar(nome)<2] = herb.data1$coletor.name[!is.na(nome)&nchar(nome)<2]
		nome[nome%in%c("SemColetor","semcoletor","anonymous","unknown","entry")] = "s.c."
	   #Collection year
		tmp = herb.data1$ano
		tmp = gsub('[^0-9\\.]',' ',tmp) ##CHECK: NOT SURE WHAT TO CHECK HERE....
		tmp = as.character(sapply(strsplit(tmp," "),function(x) unique(x[nchar(x)>=4])))
		herb.data1$ano = tmp
	##Creating the specimens identifier
  	#1:family+last name+number+municipio 
		herb.data1$specimen1 = paste(herb.data1$family.correct,nome,numb,herb.data1$municipio,sep="_")
		herb.data1$specimen1[grep("_SemColetor_|_SemNumero|_nana_|_NA_|_NA$|_s/n_|_s\\.n\\.",herb.data1$specimen1,ignore.case=TRUE)] = NA
		#2:family+number+year+municipio
		herb.data1$specimen2 = paste(herb.data1$family.correct,numb,herb.data1$ano,herb.data1$municipio,sep="_")
		herb.data1$specimen2[grep("_SemNumero|_nana_|_NA_|_NA$|_s/n_|_s\\.n\\.",herb.data1$specimen2,ignore.case=TRUE)] = NA
		#3:species+last name+number+year
		herb.data1$specimen3 = paste(herb.data1$species.correct,nome,numb,herb.data1$ano,sep="_")
		herb.data1$specimen3[grep("_SemColetor_|_SemNumero|_nana_|_NA_|_NA$|_s/n_|_s\\.n\\.",herb.data1$specimen3,ignore.case=TRUE)] = NA
		#4:last name+number+year+municipio
		herb.data1$specimen4 = paste(nome,numb,herb.data1$ano,herb.data1$municipio,sep="_")
		herb.data1$specimen4[grep("_SemColetor_|_SemNumero|_nana_|_NA_|_NA$|_s/n_|_s\\.n\\.",herb.data1$specimen4,ignore.case=TRUE)] = NA
		##Saving the new info
		herb.data = herb.data[order(herb.data$order),]
		herb.data1 = herb.data1[order(herb.data1$order),]
		herb.data = cbind.data.frame(herb.data,herb.data1[,c("numTombo","ano","ano.det","municipio","specimen1","specimen2","specimen3","specimen4")])
	##Filtering the columns to be used during analysis
		cols1 = c("collectionCode","collectioncode","siglacolecao",
		          "catalogNumber","catalognumber","numtombo",
		          "recordedBy","collector","coletor","recordedby",
		          "recordNumber","collectornumber","numcoleta","recordnumber",
		          "year","yearcollected","anocoleta",
		          "country","pais","countrycode",
		          "estado_prov","estado","stateProvince","stateprovince",
		          "municipality","cidade","county",
		          "locality","descrlocal",                
		          "decimalLatitude","decimalLongitude","latitude","longitude","decimallatitude","decimallongitude",
		          "identifiedBy","identifiedby","determinador",
		          "typeStatus","typestatus","nat_typus",
		          "dateIdentified","yearidentified","anodeterm",
		          "family","familia",
		          "scientificName","scientificname",
		          "occurrenceRemarks","notes.x","notas",#"notes",              
		          #"datasetkey",
		          "numTombo","coletor.name","coletor.last.name","coletor.number","ano",
		          "loc","loc1","loc.correct","municipio",
		          "latitude.work","longitude.work","origin.coord","resolution.coord",
		          "determinador.name","ano.det",		          
		          "status","source","family.correct","species.correct",
			      "tax.check","geo.check","af.check","AtlanticForest_Lei11428",
			      "specimen1","specimen2","specimen3","specimen4")
		cls1 =  unique(cols1[cols1 %in% names(herb.data)])
		herb.data2 = herb.data[,cls1]
		
	##Creating the new file path and saving in the local folder
		path.csv = gsub("coord-tax-geo-af.csv","coord-tax-geo-af-dup.csv",myfiles0)
		write.csv(x=herb.data2,file=path.csv, row.names = FALSE)
		cat(i,"\n")
}


#### END ####






##############################################
## Preparing data for model-R data cleaning ##
##############################################
### CODIGO NÃO UTILIZADO ###
##formato csv para model-r
#[espécie],[estado],[município],[coletor],[número de coleta],[longitude],[latitude]

### Perfoming the geographical validation for all collections ###
for(i in 1:length(paths)) {
		myfiles0 = paths[i]
		herb.data = read.csv(myfiles0,as.is=TRUE,header=TRUE,na.strings=c(""," ","NA"))
	##Removing unwanted columns
		cols = c("species.correct","loc.correct","coletor.name","coletor.number",
				"latitude.gazetteer","longitude.gazetteer",
				"collectionCode","catalogNumber","siglacolecao","numtombo","collectioncode","catalognumber",    
				"resolution.coord","longitude.work","latitude.work")
		cls =  unique(cols[cols %in% names(herb.data)])
		herb.data1 = herb.data[,cls]
		
		#herb.data1$order = 1:dim(herb.data1)[1] #putting the data back on its original order
	##Defining the state and county columns
		tmp = strsplit(herb.data1$loc.correct,"_")
		id = sapply(tmp,length)
		n1 = rep(NA,dim(herb.data1)[1]); n2 = rep(NA,dim(herb.data1)[1])   		
		n1[id>=2] = sapply(tmp[id>=2],function(x) x[2])
		n2[id>=3] = sapply(tmp[id>=3],function(x) x[3])
		herb.data1$estado = n1
		herb.data1$município = n2
	##Editing/standardizing column names
		names(herb.data1) = gsub("species.correct","espécie",names(herb.data1))
		names(herb.data1) = gsub("coletor.name","coletor",names(herb.data1))
		names(herb.data1) = gsub("coletor.number","número de coleta",names(herb.data1))
		names(herb.data1) = gsub("\\.work","",names(herb.data1))
		names(herb.data1) = gsub("siglacolecao|collectioncode","collectionCode",names(herb.data1))
		names(herb.data1) = gsub("numtombo|catalognumber","catalogNumber",names(herb.data1))
	##Creating a unique identifier for the specimen within the collection
		herb.data1$tombo = paste(herb.data1$collectionCode,herb.data1$catalogNumber,sep="_")
	##Organizing and saving the output table
		herb.data1 = herb.data1[,c("espécie","estado","município","coletor","número de coleta",
							"longitude","latitude","tombo")] #,"resolution.coord")] #INCLUIR APÓS RODAR TUDO DE NOVO
		#REMOVE OCCURRENCES WITH COORDINATES OBTAINED FROM THE GAZETTER
		path.csv = gsub("coord-tax.csv","coord-tax-modelr.csv",myfiles0)
		write.csv(x=herb.data1,file=path.csv, row.names = FALSE)

}


















