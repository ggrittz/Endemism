#########################################
## DATA MERGING FROM DIFFERENT SOURCES ##
#########################################

### LOADING AND MERGING THE DATABASES ###

#################
## speciesLINK ##
#################
#Loading the herbarium info and collection details 
splink = read.csv("speciesLink.csv", as.is=T)

#Loading and merging the different collections
path = "D://Documentos//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//speciesLink"

lista = vector("list",length(splink$acronym))
for (i in 1:length(splink$acronym)) {
	path1 = paste(path,splink$acronym[i],sep="//")
	path.csv = list.files(path1,full.names=TRUE)
	path.csv = tail(sort(path.csv[grep("edited",path.csv)]),1)
	tmp = read.csv(path.csv)
	lista[[i]] = tmp
}
splink.data = do.call(rbind,lista)
splink.data = droplevels(splink.data)
rm(lista)	

##Removing possible duplicates from the same collection
splink.data$tombo = paste(splink.data$collectionCode, splink.data$catalogNumber,sep="")
splink.data = splink.data[!is.na(splink.data$catalogNumber)&!duplicated(splink.data$tombo),]

##
splink.data$recordedBy = as.character(splink.data$recordedBy)
sort(unique(splink.data$recordedBy))

###########
## JABOT ##
###########
#jabot = read.csv("jabot.csv", as.is=T)

#Columns in jabot to be used
cls = c("siglacolecao","numtombo","codespecime","numcoleta","coletor","coletor1",
		"diacoleta","mescoleta","anocoleta","pais","estado_prov","cidade","descrlocal",
		"latitude","longitude","determinador","diadeterm","mesdeterm","anodeterm",	
		"familia","genero","scientificName","datelastmodified","habitat","habito",
		"coletor.name","coletor.last.name","coletor.fingerprint","coletor.number")
		
#Loading and merging the different collections
path = "D://Documentos//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot"
path = dir(path,full.names=TRUE)[!dir(path) %in% splink$acronym]

lista = vector("list",length(path))
for (i in 1:length(path)) {
	path.csv = list.files(path[i],full.names=TRUE)
	path.csv = tail(sort(path.csv[grep("edited",path.csv)]),1)
	tmp = read.csv(path.csv)
	tmp = tmp[,cls]
	lista[[i]] = tmp
}
jabot.data = do.call(rbind,lista)
jabot.data = droplevels(jabot.data)
rm(lista)	

##########
## GBIF ##
##########

## Organizing the list of herbarium available in Gbif ##


#gbf = read.csv("gbif.csv", as.is=T)












##ADICIONAR A herb.list O NÚMERO DE OCORRÊNCIAS POTENCIAIS/FILTRADAS



##Replacing missing county and localities:  is.na(adm1) by higherGeography and is.na(municipality) by adm2
#herb.data1[!is.na(herb.data1$adm2)&is.na(herb.data1$municipality),]$municipality = herb.data1[!is.na(herb.data1$adm2)&is.na(herb.data1$municipality),]$adm2
#herb.data1[is.na(herb.data1$adm1)&!is.na(herb.data1$higherGeography),]$adm1 = herb.data1[is.na(herb.data1$adm1)&!is.na(herb.data1$higherGeography),]$higherGeography
#herb.data1[is.na(herb.data1$cloc)&!is.na(herb.data1$locality),]$cloc = herb.data1[is.na(herb.data1$cloc)&!is.na(herb.data1$locality),]$locality

##ADICIONAR COORDENADAS PARA OS MUNICÍPIOS SEM?
#m.lat = aggregate(tmp2$lat,list(x$local),function(x) as.numeric(names(sort(table(x),decreasing=T)))[1])
#m.lon = aggregate(tmp2$lon,list(x$local),function(x) as.numeric(names(sort(table(x),decreasing=T)))[1])
#y = cbind.data.frame(local=m.lat$Group.1,lat1=m.lat$x,lon1=m.lon$x)
#y$local = as.character(y$local)
#y1= inner_join(x,y,by="local")
#tmp2$lat1 = y1$lat1
#tmp2$lon1 = y1$lon1

