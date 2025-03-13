############################################H
#### DATA MERGING FROM DIFFERENT SOURCES ####
############################################H
round(sort(sapply(mget(ls(),envir=as.environment(-1),inherits=T),object.size),decreasing=T)/(1024), digits=2)
rm(list=ls())
gc()
memory.limit(size = 50000)

## LOADING PACKAGES
require(data.table)
require(igraph)
require(stringr)
require(dplyr)

#### LOADING AND MERGING THE DATABASES ####

#### 1- speciesLINK ####
#Loading the herbarium info and collection details 
splink = read.csv("speciesLink.csv", as.is=T)

#Getting the file paths for the herbarium data
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink",full.names=TRUE)
paths = list.files(paths,full.names=TRUE)
paths = paths[grepl("-af-dup.csv",paths)] 

#Loading and merging the different collections
lista = vector("list",length(paths))
for (i in 1:length(paths)) {
  path.csv = paths[i]
  tmp = read.csv(path.csv,as.is=TRUE,na.strings=c(""," ","NA"))
  lista[[i]] = tmp
}
splink.data = do.call(rbind.data.frame,lista)
splink.data[] <- lapply(splink.data, as.character)
rm(lista)	

##Removing possible duplicates from the same collection
splink.data = splink.data[!duplicated(splink.data$numTombo)&!grepl("_NA$",splink.data$numTombo),]


#### 2- speciesLINK new ####
#Loading the herbarium info and collection details 
splink.new = read.csv("speciesLink_new.csv", as.is=T)

#Getting the file paths for the herbarium data
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//specieslink_new",full.names=TRUE)
paths = list.files(paths,full.names=TRUE)
paths = paths[grepl("-af-dup.csv",paths)] 

#Loading and merging the different collections
lista = vector("list",length(paths))
for (i in 1:length(paths)) {
  path.csv = paths[i]
  tmp = read.csv(path.csv,as.is=TRUE,na.strings=c(""," ","NA"))
  lista[[i]] = tmp
}
splink.new.data = do.call(rbind.data.frame,lista)
splink.new.data[] <- lapply(splink.new.data, as.character)
rm(lista)	

##Removing possible duplicates from the same collection
splink.new.data = splink.new.data[!duplicated(splink.new.data$numTombo)&!grepl("_NA$",splink.new.data$numTombo),]

##Final homogeneization of collection codes (check data download codes)
splink.new.data$collectioncode = gsub("^ASU-Plants|^ASU-PLANTS","ASU",splink.new.data$collectioncode)
splink.new.data$numTombo = gsub("^asu-plants","asu",splink.new.data$numTombo)
splink.new.data$collectioncode = gsub("^HERBMG","MG",splink.new.data$collectioncode)
splink.new.data$numTombo = gsub("^herbmg_mg","mg_",splink.new.data$numTombo)
splink.new.data$numTombo = gsub("^herbmg_","mg_",splink.new.data$numTombo)
splink.new.data$collectioncode = gsub("^FALSE","F",splink.new.data$collectioncode)
splink.new.data$numTombo = gsub("^false_v00","f_",splink.new.data$numTombo)
splink.new.data$numTombo = gsub("^false_v0","f_",splink.new.data$numTombo)
splink.new.data$numTombo = gsub("^false_v","f_",splink.new.data$numTombo)
splink.new.data$numTombo = gsub("^false_","f_",splink.new.data$numTombo)
splink.new.data$collectioncode = gsub("^R-TIPOS","R",splink.new.data$collectioncode)
splink.new.data$numTombo = gsub("^r-tipos_","r_",splink.new.data$numTombo)
splink.new.data$numTombo = gsub("^b_10 0","b_100",splink.new.data$numTombo)

#### 3- JABOT ####
#jabot = read.csv("jabot.csv", as.is=T)

#Getting the file paths for the herbarium data
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//jabot",full.names=TRUE)
paths = list.files(paths,full.names=TRUE)
paths = paths[grepl("-af-dup.csv",paths)] 

#Loading and merging the different collections
lista = vector("list",length(paths))
for (i in 1:length(paths)) {
  path.csv = paths[i]
  tmp = read.csv(path.csv,as.is=TRUE,na.strings=c(""," ","NA"))
  lista[[i]] = tmp
}
jabot.data = do.call(rbind.data.frame,lista)
jabot.data[] <- lapply(jabot.data, as.character)
rm(lista)	

##Removing possible duplicates from the same collection
jabot.data = jabot.data[!duplicated(jabot.data$numTombo)&!grepl("_NA$",jabot.data$numTombo),]

##Final homogeneization of collection codes (check data download codes)
jabot.data$numTombo = gsub("^rb_00","rb_", jabot.data$numTombo)
jabot.data$numTombo = gsub("^rb_0","rb_", jabot.data$numTombo)


#### 4- GBIF ####
#Getting the file paths for the herbarium data
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//gbif",full.names=TRUE)
paths = paths[grepl("-af-dup.csv",paths)] 

#Loading and merging the different collections
lista = vector("list",length(paths))
for (i in 1:length(paths)) {
  path.csv = paths[i]
  tmp = read.csv(path.csv,as.is=TRUE,na.strings=c(""," ","NA"))
  lista[[i]] = tmp
}
gbif.data = do.call(rbind,lista)
gbif.data[] <- lapply(gbif.data, as.character)
rm(lista)	

##Removing possible duplicates from the same collection
gbif.data = gbif.data[!duplicated(gbif.data$numTombo)&!grepl("_NA$",gbif.data$numTombo),]


#### 5- SNDB ####

#Getting the file paths for the herbarium data
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//herbaria//sndb_mincyt_arg",full.names=TRUE)
paths = list.files(paths,full.names=TRUE)
paths = paths[grepl("-af-dup.csv",paths)] 

#Loading and merging the different collections
lista = vector("list",length(paths))
for (i in 1:length(paths)) {
  path.csv = paths[i]
  tmp = read.csv(path.csv,as.is=TRUE,na.strings=c(""," ","NA"))
  lista[[i]] = tmp
}
sndb.data = do.call(rbind.data.frame,lista)
sndb.data[] <- lapply(sndb.data, as.character)
rm(lista)	

##Removing possible duplicates from the same collection
sndb.data = sndb.data[!duplicated(sndb.data$numTombo)&!grepl("_NA$",sndb.data$numTombo),]

##Removing the extra county column
#sndb.data$county[is.na(sndb.data$county)|as.character(sndb.data$county) %in% c("","-")] = as.character(sndb.data$municipality[is.na(sndb.data$county) | as.character(sndb.data$county) %in% c("","-")])
#sndb.data$municipality[is.na(sndb.data$municipality)|as.character(sndb.data$municipality) %in% c("","-")] = as.character(sndb.data$county[is.na(sndb.data$municipality)|as.character(sndb.data$municipality) %in% c("","-")])
#sndb.data = sndb.data[,-which(names(sndb.data)=="county")]

###############################################################-
#### FINDING AND REMOVING DATA FROM DUPLICATED COLLECTIONS ####
###############################################################-
### CHECK POSSIBLE REMOVAL OF TYPES FROM COLLECTIONS OTHER THEN SPECIES LINK (e.g. in gbif P00161345 is a type byt the info is missing in p_br)

### spLink vs. JABOT ###
tmp = c(unique(splink.data$collectionCode),sort(unique(jabot.data$siglacolecao)))
tmp1	= tmp[duplicated(tmp)]
hb1 = splink.data[splink.data$collectionCode %in% tmp1,]; hb1 = hb1[order(hb1$numTombo),]
hb2 = jabot.data[jabot.data$siglacolecao %in% tmp1,]; hb2 = hb2[order(hb2$numTombo),]
table(hb1$collectionCode)
table(hb2$siglacolecao)
tmp2 = hb2[!hb2$numTombo %in% hb1$numTombo,]
table(tmp2$siglacolecao)
#Any non-assigned types among the specimens to de removed? 
hb1.typ = hb1$numTombo[!is.na(hb1$typeStatus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel",hb1$typeStatus,ignore.case = TRUE)] 
hb2.typ = hb2$numTombo[!is.na(hb2$nat_typus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel",hb2$nat_typus,ignore.case = TRUE)] 
hb2.typ[!hb2.typ %in% hb1.typ]# No!
#Removing the duplicated records and add the non_duplicated ones (why there are differences betweens sources? download mode?)
dim(jabot.data)
jabot.data = jabot.data[!jabot.data$siglacolecao %in% tmp1,]
dim(jabot.data)
jabot.data = rbind.data.frame(jabot.data, tmp2)
dim(jabot.data)

### spLink vs. spLink_new ###
tmp = c(unique(splink.data$collectionCode),sort(unique(splink.new.data$collectioncode)))
tmp1	= tmp[duplicated(tmp)]
hb1 = splink.data[splink.data$collectionCode %in% tmp1,]; hb1 = hb1[order(hb1$numTombo),]
hb2 = splink.new.data[splink.new.data$collectioncode %in% tmp1,]; hb2 = hb2[order(hb2$numTombo),]
table(hb1$collectionCode)
table(hb2$collectioncode)
#Any non-assigned types among the specimens to de removed? 
hb1.typ = hb1$numTombo[!is.na(hb1$typeStatus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel",hb1$typeStatus,ignore.case = TRUE)] 
hb2.typ = hb2$numTombo[!is.na(hb2$typestatus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel",hb2$typestatus,ignore.case = TRUE)] 
hb2.typ[!hb2.typ %in% hb1.typ]# No!

#Removing the duplicated records (duplicated download of the collection)
splink.new.data = splink.new.data[!splink.new.data$collectioncode %in% tmp1,]

### spLink_new vs. JABOT ###
tmp = c(unique(jabot.data$siglacolecao),unique(splink.new.data$collectioncode))
tmp1	= tmp[duplicated(tmp)]
hb1 = splink.new.data[splink.new.data$collectioncode %in% tmp1,]; hb1 = hb1[order(hb1$numTombo),]
hb2 = jabot.data[jabot.data$siglacolecao %in% tmp1,]; hb2 = hb2[order(hb2$numTombo),]
table(hb1$collectioncode)
table(hb2$siglacolecao)
tmp2 = hb2[!hb2$numTombo %in% hb1$numTombo,]
table(tmp2$siglacolecao)
#Any non-assigned types among the specimens to de removed? 
hb1.typ = hb1$numTombo[!is.na(hb1$typestatus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel",hb1$typestatus,ignore.case = TRUE)] 
hb2.typ = hb2$numTombo[!is.na(hb2$nat_typus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel",hb2$nat_typus,ignore.case = TRUE)] 
tmp = hb2.typ[!hb2.typ %in% hb1.typ]# Yes!
tmp = tmp[tmp %in% splink.new.data$numTombo] #Only 1!
splink.new.data[splink.new.data$numTombo %in% tmp, c("identifiedby","typestatus","yearidentified","tax.check","family.correct","species.correct")] = 
  jabot.data[jabot.data$numTombo %in% tmp, c("determinador","nat_typus","anodeterm","tax.check","family.correct","species.correct")]
#Removing the duplicated records and add the non_duplicated ones (why there are differences betweens sources? download mode?)
dim(jabot.data)
jabot.data = jabot.data[!jabot.data$siglacolecao %in% tmp1,]
dim(jabot.data)
jabot.data = rbind.data.frame(jabot.data, tmp2)
dim(jabot.data)

### spLink + splink_new + JABOT vs. GBIF ###
#Fixing differences in source names
names(splink.new.data) = names(splink.data)
names(jabot.data) = names(splink.data)
names(gbif.data) = names(splink.data)
names(sndb.data) = names(splink.data)

#List of collections to have data excluded from gbif
herb.list = sort(unique(splink.data$collectionCode))
herb.list1 = sort(unique(splink.new.data$collectionCode))
herb.list2 = sort(unique(jabot.data$collectionCode))
herb.list3 = sort(unique(sndb.data$collectionCode))

herbs = toupper(c(herb.list,herb.list1,herb.list2,herb.list3))
herbs = sort(unique(c(herbs,gsub("_FANEROGAMAS|_HERBARIO|-HERBARIO|-AGP|-PLANTS|-MG|_BR","",herbs))))
#Finding and editing GBIF collections
tmp1 = unique(gbif.data$collectionCode)
#Finding the duplicated collections in GBIF
tmp2 = tmp1[tmp1 %in% herbs]
hb1 = rbind.data.frame(splink.data[splink.data$collectionCode %in% c(tmp2),c(1:5,12:14,18:22,34:35)],
                       splink.new.data[splink.new.data$collectionCode %in% c(tmp2),c(1:5,12:14,18:22,34:35)],
                       jabot.data[jabot.data$collectionCode %in% c(tmp2),c(1:5,12:14,18:22,34:35)],
                       sndb.data[sndb.data$collectionCode %in% c(tmp2),c(1:5,12:14,18:22,34:35)],
                       stringsAsFactors = FALSE)
hb2 = gbif.data[gbif.data$collectionCode %in% c(tmp2),]; hb2 = hb2[order(hb2$numTombo),]
table(hb1$collectionCode)
table(hb2$collectionCode)
#Any non-assigned types among the specimens to de removed? 
hb1.typ = hb1$numTombo[!is.na(hb1$typeStatus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel|ORIGINALMATERIAL",hb1$typeStatus,ignore.case = TRUE)] 
hb2.typ = hb2$numTombo[!is.na(hb2$typeStatus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel|ORIGINALMATERIAL",hb2$typeStatus,ignore.case = TRUE)] 
tmp = hb2.typ[!hb2.typ %in% hb1.typ]# Yes!
#speciesLink
tmp0 = tmp[tmp %in% splink.data$numTombo & !grepl('^paca',tmp)] #12 in speciesLink! PACA has problems...
toto = merge(splink.data[splink.data$numTombo %in% tmp0, c("numTombo","recordedBy","recordNumber","identifiedBy","typeStatus","dateIdentified","tax.check","family.correct","species.correct")],
             gbif.data[gbif.data$numTombo %in% tmp0, c("numTombo","recordedBy","recordNumber","identifiedBy","typeStatus","dateIdentified","tax.check","family.correct","species.correct")],
             by="numTombo",all.x=TRUE,sort=FALSE)
table(splink.data$numTombo[splink.data$numTombo %in% tmp0] == toto$numTombo)
splink.data[splink.data$numTombo %in% tmp0, c("identifiedBy","typeStatus","dateIdentified","tax.check")] = toto[, c("identifiedBy.y","typeStatus.y","dateIdentified.y","tax.check.y")]
splink.data$dateIdentified[splink.data$numTombo %in% tmp0][12] = 2003
#speciesLink
tmp0 = tmp[tmp %in% splink.data$numTombo & !grepl('^paca',tmp)] #21 in speciesLink! PACA has problems...
tmp0 = tmp0[!tmp0 %in% "peufr_37471"]
toto = merge(splink.data[splink.data$numTombo %in% tmp0, c("numTombo","recordedBy","recordNumber","identifiedBy","typeStatus","dateIdentified","tax.check","family.correct","species.correct")],
             gbif.data[gbif.data$numTombo %in% tmp0, c("numTombo","recordedBy","recordNumber","identifiedBy","typeStatus","dateIdentified","tax.check","family.correct","species.correct")],
             by="numTombo",all.x=TRUE,sort=FALSE)
table(splink.data$numTombo[splink.data$numTombo %in% tmp0] == toto$numTombo)
splink.data[splink.data$numTombo %in% tmp0, c("identifiedBy","typeStatus","dateIdentified","tax.check")] = toto[, c("identifiedBy.y","typeStatus.y","dateIdentified.y","tax.check.y")]
#speciesLink_new
tmp0 = tmp[tmp %in% splink.new.data$numTombo & !grepl('^us_',tmp)] #12 in speciesLink_new! US has problems...
toto = merge(splink.new.data[splink.new.data$numTombo %in% tmp0, c("numTombo","recordedBy","recordNumber","identifiedBy","typeStatus","dateIdentified","tax.check","family.correct","species.correct")],
             gbif.data[gbif.data$numTombo %in% tmp0, c("numTombo","recordedBy","recordNumber","identifiedBy","typeStatus","dateIdentified","tax.check","family.correct","species.correct")],
             by="numTombo",all.x=TRUE,sort=FALSE)
table(splink.new.data$numTombo[splink.new.data$numTombo %in% tmp0] == toto$numTombo)
splink.new.data[splink.new.data$numTombo %in% tmp0, c("typeStatus","tax.check")] = toto[, c("typeStatus.y","tax.check.y")]
#jabot
tmp0 = tmp[tmp %in% jabot.data$numTombo & !grepl('^us_|^paca|^rb_',tmp)] #0 in Jabot! PACA, US and RB have problems...
#toto = merge(jabot.data[jabot.data$numTombo %in% tmp0, c("numTombo","recordedBy","recordNumber","identifiedBy","typeStatus","dateIdentified","tax.check","family.correct","species.correct")],
#             gbif.data[gbif.data$numTombo %in% tmp0, c("numTombo","recordedBy","recordNumber","identifiedBy","typeStatus","dateIdentified","tax.check","family.correct","species.correct")],
#             by="numTombo",all.x=TRUE,sort=FALSE)
#table(jabot.data$numTombo[splink.new.data$numTombo %in% tmp0] == toto$numTombo)
#jabot.data[jabot.data$numTombo %in% tmp0, c("typeStatus","tax.check")] = toto[, c("typeStatus.y","tax.check.y")]

##Checking herbaria with some large differences in the collections in GBIF
#RB, LUSC, HUCO: different values of catalogNumber between JABOT and GBIF => remover na unha!
#UFACPZ, R, CGMS, EAN, BHZB: ok, mais ou varias coletas diferentes no gbif mesmo... manter... PROBLEMA DE FILTRAGEM INICIAL DOS DADOS DO GBIF? TALVEZ USAMOS UMA LISTA COM MAIS NOMES NA ?POCA...
#CHECAR different values of catalogNumber speciesLink and GBIF: PACA
tmp4 = hb2[!hb2$numTombo %in% hb1$numTombo,]
#Removing same specimens with different accessions from RB
dups = paste(tmp4$coletor.name, tmp4$coletor.number, tmp4$ano, tmp4$family.correct,sep="_")
dups1= paste(hb1$coletor.name[grepl("^rb_",hb1$numTombo)], hb1$coletor.number[grepl("^rb_",hb1$numTombo)], hb1$ano[grepl("^rb_",hb1$numTombo)], hb1$family.correct[grepl("^rb_",hb1$numTombo)],sep="_")
tmp5 = tmp4[grepl("^rb_",tmp4$numTombo) & !(dups %in% dups1),]
dim(tmp4);dim(tmp5)
tmp4 = rbind.data.frame(tmp4[!grepl("^rb_",tmp4$numTombo),],tmp5)
#Removing same specimens with different accessions from LUSC
dups = paste(tmp4$coletor.name, tmp4$coletor.number, tmp4$ano, tmp4$family.correct,sep="_")
dups1= paste(hb1$coletor.name[grepl("^lusc_",hb1$numTombo)], hb1$coletor.number[grepl("^lusc_",hb1$numTombo)], hb1$ano[grepl("^lusc_",hb1$numTombo)], hb1$family.correct[grepl("^lusc_",hb1$numTombo)],sep="_")
tmp5 = tmp4[grepl("^lusc_",tmp4$numTombo) & !(dups %in% dups1),]
dim(tmp4);dim(tmp5)
tmp4 = rbind.data.frame(tmp4[!grepl("^lusc_",tmp4$numTombo),],tmp5)
dim(tmp4)
#Removing same specimens with different accessions from HUCO
dups = paste(tmp4$coletor.name, tmp4$coletor.number, tmp4$ano, tmp4$family.correct,sep="_")
dups1= paste(hb1$coletor.name[grepl("^huco_",hb1$numTombo)], hb1$coletor.number[grepl("^huco_",hb1$numTombo)], hb1$ano[grepl("^huco_",hb1$numTombo)], hb1$family.correct[grepl("^huco_",hb1$numTombo)],sep="_")
tmp5 = tmp4[grepl("^huco_",tmp4$numTombo) & !(dups %in% dups1),]
dim(tmp4);dim(tmp5)
tmp4 = rbind.data.frame(tmp4[!grepl("^huco_",tmp4$numTombo),],tmp5)
dim(tmp4)
#Any other to remove? I did not checked all collections... hope the duplicate search will take care of those cases
table1 = table(hb1$collectionCode)
table2 = table(tmp4$collectionCode)
resultado = merge(table1,table2,by="Var1",all=TRUE)
resultado$prop = round(100*resultado[,3]/resultado[,2],2)
resultado = resultado[order(resultado$prop),]
resultado
#hb3 = hb1[grepl("^rb_",hb1$numTombo),]; hb3 = hb3[order(hb3$numTombo),]
#hb4 = tmp4[grepl("^rb_",tmp4$numTombo),]; hb4 = hb4[order(hb4$numTombo),]
#tail(table(hb4$coletor.name))
#hb4[grepl('roppo',hb4$recordedBy)&hb4$coletor.number%in%"2233",]
#hb3[grepl('roppo',hb3$recordedBy)&hb3$coletor.number%in%"2233",]

#Removing the duplicated records and add the non_duplicated ones
dim(gbif.data)
gbif.data = gbif.data[!gbif.data$collectionCode %in% c(tmp2),]
dim(gbif.data)
gbif.data = rbind.data.frame(gbif.data, tmp4)
dim(gbif.data)
rm(hb1,hb2,dups,dups1,tmp0,tmp,tmp2,tmp4,tmp5)

########################################################################################################################################################################################-
########################################################################################################################################################################################-
##################################
#### SOLVING HOMONYM PROBLEMS ####
##################################

##Merging files for bad homomym/synonym search
dados = rbind.data.frame(splink.data,splink.new.data,jabot.data,sndb.data,gbif.data,stringsAsFactors = FALSE)
tmp = c(rep("splink",dim(splink.data)[1]),rep("splink_new",dim(splink.new.data)[1]),rep("jabot",dim(jabot.data)[1]),rep("sndb",dim(sndb.data)[1]),rep("gbif",dim(gbif.data)[1]))
dados = cbind.data.frame(source=as.character(tmp),dados,stringsAsFactors = FALSE)
rm(list=ls()[! ls() %in% c("dados")])

##Finding species with bad homomym/synonym problems
tmp = unique(dados$species.correct[grepl("ok, mas|ok but|ok, but",dados$status) & !grepl("but not validly published",dados$status)])

sp = "Erythroxylum umbu"; paises = c("BR","Brazil","Brasil","BRASIL","BRAZIL","UY","Indeterminado")
ids= dados$species.correct %in% sp & !is.na(dados$country) & !dados$country %in% paises
dados[ids, c("species.correct")] = dados[ids, c("scientificName")]; dados$status[ids] = "ok" 

#sp = "Solanum pseudoquina"; paises = c("Argentina","ARGENTINA","BR","Brazil","Brasil","BRASIL","BRAZIL","Paraguai","Paraguay","PARAGUAI","PARAGUAY","UY","Indeterminado")
#ids= dados$species.correct %in% sp & !is.na(dados$country) & !dados$country %in% paises
#dados[ids, c("family.correct","species.correct")] = dados[ids, c("family","scientificName")]; dados$status[ids] = "ok" 

sp = "Terminalia acuminata"; paises = c("BO","BR","Brasil","Brazil","BRAZIL","Argentina","ARGENTINA","BR","Brazil","Brasil","BRASIL","BRAZIL","Paraguai","Paraguay","PARAGUAI","PARAGUAY","UY","Indeterminado")
ids= dados$species.correct %in% sp & !is.na(dados$country) & !dados$country %in% paises
dados[ids, c("species.correct")] = "Anogeissus acuminata"; dados$status[ids] = "ok" 

sp = c("Marlierea subulata")
ids= dados$species.correct %in% sp & !is.na(dados$country) & !grepl("brazil_sao paulo", dados$loc) 
ids1= dados$species.correct %in% sp & !is.na(dados$country) & grepl("brazil_sao paulo", dados$loc) 
dados[ids, c("species.correct")] = "Myrcia subulata"
dados[ids1, c("species.correct")] = "Myrcia skortzoviana"
dados$status[ids] = "ok"; dados$status[ids1] = "ok" 

sp = c("Eugenia brunoi"); paises = c("NC")
ids= dados$species.correct %in% sp & !is.na(dados$country) & dados$country %in% paises 
dados[ids, c("species.correct")] = dados[ids, c("scientificName")]
dados$status[ids] = "ok"

sp = "Chrysophyllum flexuosum"; paises = c("BO","BR","Brasil","Brazil","BRAZIL","Argentina","ARGENTINA","BR","Brazil","Brasil","BRASIL","BRAZIL","Paraguai","Paraguay","PARAGUAI","PARAGUAY","UY","Indeterminado")
ids= dados$species.correct %in% sp & !is.na(dados$country) & !dados$country %in% paises
dados[ids, c("species.correct")] = "Chrysophyllum lanceolatum"; dados$status[ids] = "ok" 

sp = "Solanum amygdalifolium"; paises = c("BO","Bolivia","BR","Brasil","Brazil","BRAZIL","AR","Argentina","ARGENTINA","BR","Brazil","Brasil","BRASIL","BRAZIL","PY","Paraguai","Paraguay","PARAGUAI","PARAGUAY","UY","Uruguai","Uruguay","URUGUAI","URUGUAY","Indeterminado","CL")
ids= dados$species.correct %in% sp & !is.na(dados$country) & !dados$country %in% paises
dados[ids, c("species.correct")] = dados[ids, c("scientificName")]; dados$status[ids] = "ok" 

### ADD Ocotea lucida (Meisn.) Vattimo-Gil (syn. of Ocotea brachybotra (Meisn.) Mez.?) vs. Ocotea lucida Mart. ex Nees (syn. of Endlicheria paniculata (Spreng.) J.F.Macbr.?)

#tmp1 = dados[dados$species.correct %in% sp | dados$scientificName %in% sp,]
#unique(tmp1$status)
#table(tmp1$scientificName, tmp1$species.correct)
#table(countrycode(tmp1$country, "iso2c", "country.name"))
#table(tmp1$country)
#table(tmp1$scientificName, tmp1$species.correct, countrycode(tmp1$country, "iso2c", "country.name"))
########################################################################################################################################################################################-
########################################################################################################################################################################################-
####################################
#### DUPLICATED-SPECIMEN SEARCH ####
####################################

##Creating the the columns for duplicated search
dados$dup.check= NA 
dados$dup.numb = NA
dados$dup.prop = NA
dados$dup.ID = NA  
dados$dup.merge= NA 

##Checking for the duplicates (top down)
dados$dup.check1 = duplicated(dados$specimen1, incomparables = NA)
dados$dup.check2 = duplicated(dados$specimen2, incomparables = NA)
dados$dup.check3 = duplicated(dados$specimen3, incomparables = NA)
dados$dup.check4 = duplicated(dados$specimen4, incomparables = NA)

##Checking for the duplicates (bottom up)
dup.check1 = duplicated(dados$specimen1, incomparables = NA, fromLast = TRUE)
dup.check2 = duplicated(dados$specimen2, incomparables = NA, fromLast = TRUE)
dup.check3 = duplicated(dados$specimen3, incomparables = NA, fromLast = TRUE)
dup.check4 = duplicated(dados$specimen4, incomparables = NA, fromLast = TRUE)

### List of specimens with indication of duplicates 
dados$dup.check1[dados$dup.check1==FALSE & dup.check1==TRUE] = TRUE
dados$dup.check2[dados$dup.check2==FALSE & dup.check2==TRUE] = TRUE
dados$dup.check3[dados$dup.check3==FALSE & dup.check3==TRUE] = TRUE
dados$dup.check4[dados$dup.check4==FALSE & dup.check4==TRUE] = TRUE

#### Number of duplicated strings found (max. 4) ####
dados$dup.numb[is.na(dados$specimen1)&is.na(dados$specimen2)&is.na(dados$specimen3)&is.na(dados$specimen4)] = "cc"
dados$dup.numb[dados$dup.check1&dados$dup.check2&dados$dup.check3&dados$dup.check4] = 4
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check1&dados$dup.check2&dados$dup.check3] = 3
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check1&dados$dup.check2&dados$dup.check4] = 3
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check1&dados$dup.check3&dados$dup.check4] = 3
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check2&dados$dup.check3&dados$dup.check4] = 3
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check1&dados$dup.check2] = 2
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check1&dados$dup.check3] = 2
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check1&dados$dup.check4] = 2
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check2&dados$dup.check3] = 2
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check2&dados$dup.check4] = 2
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check3&dados$dup.check4] = 2
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check1] = 1
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check2] = 1
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check3] = 1
dados$dup.numb[is.na(dados$dup.numb)&dados$dup.check4] = 1
dados$dup.numb[is.na(dados$dup.numb)] = 0

#### Proportion of duplicated strings found among te possible non-NA strings (max. 1) ####
#table(is.na(dados$specimen1),dados$source)
#table(is.na(dados$specimen2),dados$source)
#table(is.na(dados$specimen3),dados$source) #gbif mainly has only specimen3 which is not NA
#table(is.na(dados$specimen4),dados$source)
tmp = dados[,c("specimen1","specimen2","specimen3","specimen4")]
tmp1= apply(tmp,1,function(x) sum(!is.na(x)))
tmp1[!dados$dup.numb %in% "cc"] = as.double(dados$dup.numb[!dados$dup.numb %in% "cc"])/tmp1[!dados$dup.numb %in% "cc"]
tmp1[dados$dup.numb %in% "cc"] = "cc"
dados$dup.prop = tmp1

#### Assigning a unique column with occurrences with any indication of duplicates ####
dados$dup.check[!dados$dup.numb %in% "cc"] = dados$dup.numb[!dados$dup.numb  %in% "cc"]>0
dados$dup.check[dados$dup.numb %in% "cc"] = "cannot_check"
rm(list=ls()[! ls() %in% c("dados")])

#### Defining the unique duplicated code ####
## Filtering data with any evidence of duplicates
dados1 = dados[dados$dup.check %in% "TRUE",]  # only data with any indication of duplicates
dados1$ordem = 1:dim(dados1)[1]

## Filtering the dataset and converting to the data.table format
dt = dados1[,c("ordem","numTombo","specimen1","specimen2","specimen3","specimen4")]  # only data with any indication of duplicates
dt = data.table(dt)

## Finding indirect duplicated search strings
## HENRIK SOLUTION
#see: https://stackoverflow.com/questions/56740990/optimizing-grepl-and-other-operations-inside-a-loop-using-data-table

#start_time <- Sys.time()
# melt data to long format
d <- melt(dt, id.vars = "numTombo", na.rm = TRUE)

# convert to graph
g <- graph_from_data_frame(d[ , .(numTombo, value)])

# get components 
memb <- components(g)$membership

# add component id to original data
dt[.(names(memb)), on = .(numTombo), memb := memb] 

# concatenate 'id' by 'memb' column
setkeyv(dt, c("memb"))
dt[!is.na(memb), new_id := paste(sort(numTombo), collapse = "|"), by = memb]
#end_time <- Sys.time()
#end_time - start_time # Time difference of 28.811 secs

## Removing groups of duplicates based only on indirect duplicates (e.g. dupID "botu_4673|botu_4722|botu_5209|icn_90228")
dt[new_id == "botu_4673|botu_4722|botu_5209|icn_90228"]
dt[!is.na(new_id), new_id := ifelse(anyDuplicated(specimen1, incomparables=NA)>0 | anyDuplicated(specimen3, incomparables=NA)>0, new_id, NA_character_), by = new_id]

# checking - ok!
#dt[dt$numTombo %like% "^alcb_101037|^rb_515149|^ucs_37083"]
#dt[dt$memb == "12093"]

#Saving the duplicated IDs in the main data frame
setkeyv(dt, c("ordem")) #re-ordering the data.table
table(dados1$ordem == dt$ordem)
dados$dup.ID[dados$dup.check %in% "TRUE"] = as.character(dt$new_id)

### OLD CODES ### Replaced after a post in stack overflow with a much better and faster solution
#see: https://stackoverflow.com/questions/56740990/optimizing-grepl-and-other-operations-inside-a-loop-using-data-table
## Specimen 1 
#Filtering the data specific for duplicate search using the search string
#tmp = dados1[dados1$specimen1 %in% spm1 & dados1$dup.check1,c("numTombo1","specimen1")]
#Converting to the data.table format and finding duplicated specimens using the search string
#tmp = data.table(tmp)
#tmp1 = tmp[,new.id:= paste(numTombo1,collapse="|"), by = list(specimen1)]
#Saving the concatenated accession numbers to the original data frame
#dados1$numTombo1[dados1$specimen1 %in% spm1 & dados1$dup.check1] = tmp1$new.id 

## SUPRESSED SAME STEPS FOR Specimen 2, 3 and 4 

#### Edtiting/creating the unique duplicate ID for all search strings
#tmp = dados1[,c("numTombo","numTombo1")]
#tmp1 = sapply(tmp$numTombo1, 
#	function(x) sort(unique(as.character(strsplit(x,"\\|")[[1]]), incomparables = NA)))
#tmp2 = as.character(sapply(tmp1, function(x) paste(x, collapse="|")))
#Saving the IDs in the duplicate data frame
#dados1$numTombo1 = tmp2
#rm(list=ls()[! ls() %in% c("dados","dados1")])

##Codes by: Lucie Zinger <3
#library(stringr)

##Filtering the unique duplicated ID
#toto = dados1[!duplicated(dados1$numTombo1),c("numTombo","numTombo1")]
#toto$numTombo2 = 
#  sapply(1:nrow(toto), function(x) {
#    tmp = grep(paste(toto$numTombo[x],c("","$"),sep="",collapse="|"), toto$numTombo1)
#    if(length(tmp)==0){
#      toto$numTombo1[x]
#    } else {
#      test = strsplit(toto$numTombo1[tmp], "")
#      toto$numTombo1[tmp[which.max(sapply(test, function(x) sum(str_count(x, "\\|"))))]]
#    }
#  })
#saveRDS(toto,"dupID.rds")

### Finding duplicates specimen by specimen - slower but safer - TOO SLOW (TOOK 20H TO DO 160,000 LINES... ABORTING)
#tmp = dados1[,c("numTombo","specimen1","specimen2","specimen3","specimen4","dup.ID")]
#rm(list=ls()[! ls() %in% c("dados","dados1","tmp")])
#gc()
#memory.limit(size=40000)
#tmp$order = 1:dim(tmp)[1]
#system.time(
#for(i in 1:dim(tmp)[1]) {
#  tmp1 = tmp[i,]
#  strs = tmp1[,c("specimen1","specimen2","specimen3","specimen4")]
#  strs[is.na(strs)] = "toto"
#  spcms= tmp[tmp$specimen1 %in% strs[1] | tmp$specimen2 %in% strs[2] | tmp$specimen3 %in% strs[3] | tmp$specimen4 %in% strs[4],]
#  id = sort(spcms$numTombo)
#  if(all(is.na(tmp$dup.ID[tmp$numTombo %in% id]))) { 
#    tmp$dup.ID[tmp$numTombo %in% id] = paste(id,collapse="|") 
#  } else {
#    id1 = sort(unique(c(id,strsplit(tmp$dup.ID[tmp$numTombo %in% id],"\\|")[[1]]),incomparables = NA))
#    tmp$dup.ID[tmp$numTombo %in% id] = paste(id1,collapse="|")
#  }
#}
#)
#saveRDS(tmp[,c("numTombo","dup.ID")],"dupID.rds")


########################################################################################################################################################################################-
########################################################################################################################################################################################-
###################################################
#### FINAL ASSESSMENT OF MISSING SPECIALIST AND TYPE LOCALITIES ####
###################################################
##Done in 06 2019!
## Names of determiner with lots of tax.check == FALSE
#tmp = table(dados$determinador.name[!grepl(",",dados$determinador.name)&dados$tax.check==FALSE])
#tmp1= dados[dados$determinador.name %in% names(tmp),]
#tmp1= tmp1[!tmp1$determinador.name %in% c("Semdeterminador","SemDeterminador","Anonymus","Anonymous","Anonimo","Incognito","Unknown"),]
#tmp2= aggregate(tmp1$determinador.name,list(tmp1$identifiedBy,tmp1$family.correct),function(x) paste(unique(x),collapse="|"))
#tmp2$n= aggregate(tmp1$determinador.name,list(tmp1$identifiedBy,tmp1$family.correct),function(x) length(x))$x
#tmp2 = tmp2[order(tmp2$n,decreasing = TRUE),]
#write.csv(tmp2,"check_taxonomists.csv")

##Still needs checking... but files are in the folder already
## Missing localities for the type specimens (i.e. tax.check == TRUE)
#tmp = dados[!is.na(dados$typeStatus)&!grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel",dados$typeStatus,ignore.case = TRUE),]
#tmp1= tmp[tmp$af.check %in% "cannot_check",]
#tmp1= tmp1[!is.na(tmp1$locality),]
#tmp2= aggregate(tmp1$loc,list(tmp1$locality),function(x) paste(unique(x),collapse="|"))
#tmp2$loc1= aggregate(tmp1$loc1,list(tmp1$locality),function(x) paste(unique(x),collapse="|"))$x
#tmp2$loc.correct= aggregate(tmp1$loc.correct,list(tmp1$locality),function(x) paste(unique(x),collapse="|"))$x
#tmp2$country= aggregate(tmp1$country,list(tmp1$locality),function(x) paste(unique(x),collapse="|"))$x
#tmp2$stateProvince= aggregate(tmp1$stateProvince,list(tmp1$locality),function(x) paste(unique(x),collapse="|"))$x
#tmp2$n= aggregate(tmp1$loc,list(tmp1$locality),function(x) length(x))$x
#tmp2 = tmp2[order(tmp2$n,decreasing = TRUE),]
#write.csv(tmp2,"check_type_localities.csv")

########################################################################################################################################################################################-
########################################################################################################################################################################################-
#######################################################
#### REMOVING CULTIVATED SPECIMENS STILL REMAINING ####
#######################################################
dados <- data.table(dados)

remover = dados[occurrenceRemarks %like% "ultivad|ultivated|cultivato|under cultivation|plantada|plantado|planted|plantio|arboreto|arboretum|in cultivo|in cultis", numTombo]
dim(dados)
dados <- data.table(dados[!numTombo %in% remover])
dim(dados)

#Planta/arvore introduzida
remover = dados[occurrenceRemarks %like% "Planta introduzida|rvore introduzida", numTombo]
dados <- data.table(dados[!numTombo %in% remover])

# cultivated in the JBRJ or elsewhere
remover = c("mo_100241110","g_364411","us_1690039","r_43636","rb_233901","ipa_43656")
dados <- data.table(dados[!numTombo %in% remover])

#Coletado no Sítio Roberto Burle Marx
remover = dados[locality %like% "Coletado no Sítio Roberto Burle Marx|Coletada no Sítio Roberto Burle Marx|Coletada no Sítio R\\. Burle Marx", numTombo]
dados <- data.table(dados[!numTombo %in% remover])


########################################################################################################################################################################################-
########################################################################################################################################################################################-
#####################################################
#### SAVING THE OCCURRENCE DATA FAMILY BY FAMILY ####
#####################################################

#### Checking family names ####
require(stringr)
#Generating the list of species per family
tmp = dados[!duplicated(paste(dados$family.correct,dados$species.correct,sep="_")),c("family.correct","species.correct")]
tmp$genus.correct = as.character(sapply(strsplit(tmp$species.correct," "),function(x) str_trim(x[1])))
tmp = tmp[,c("family.correct","genus.correct","species.correct")]
tmp = tmp[order(tmp$species.correct),]
tmp = tmp[order(tmp$family.correct),]
tmp$order = 1:dim(tmp)[1]

#Getting the correct names of the families
families = read.csv("families_synonyms.csv", as.is=TRUE,na.string=c(NA,""," "))
tmp = merge(tmp,families[,c("name","name.correct")],by.x="family.correct",by.y="name",all.x=TRUE)
tmp[!tmp$family.correct == tmp$name.correct,]
##Ok! Only small differences: 
#Adoxaceae (Reflora) e Viburnaceae (APG)
#Laxmanniaceae (Reflora) e Asparagaceae (APG)
#Quiinaceae (Reflora) e Ochnaceae (APG)
#Turneraceae (Reflora) e Passifloraceae (APG)

#Any missing problems at family-level?
tmp1 = get.taxa(unique(tmp$family.correct), life.form = FALSE, habitat = FALSE, states = FALSE,
                suggestion.distance = 0.92, drop = c("id","accepted.name","search.str","scientific.name","specific.epiteth","infra.epiteth","authorship","threat.status"))
#Confirming some family names verified manually (families not in ReFlora)
f.n.i.fbo = c("Aextoxicaceae","Juglandaceae","Betulaceae","Alzateaceae","Cupressaceae",
              "Taxaceae","Buxaceae","Casuarinaceae","Fagaceae","Columelliaceae","Iteaceae",
              "Cornaceae","Fouquieriaceae","Garryaceae","Gomortegaceae","Tapisciaceae","Hydrangeaceae","Cyrillaceae",
              "Koeberliniaceae", "Atherospermataceae", "Petiveriaceae", "Altingiaceae", "Hamamelidaceae",
              "Metteniusaceae", "Myricaceae","Nothofagaceae", "Dipterocarpaceae", "Paulowniaceae",
              "Dipentodontaceae", "Phyllonomaceae", "Platanaceae", "Lepidobotryaceae","Actinidiaceae")
tmp1[is.na(tmp1$family)&!tmp1$original.search %in% f.n.i.fbo,]
#Ok!

#Any problems with species/genus names?
tmp1 = get.taxa(unique(tmp$genus.correct), life.form = FALSE, habitat = FALSE, states = FALSE,
                suggestion.distance = 1, drop = c("id","accepted.name","search.str","scientific.name","specific.epiteth","infra.epiteth","authorship","threat.status"))
tmp1[tmp1$notes %in% "check +1 accepted",] # Thieleodoxa, Thieleodoxa stipularis => Alibertia stipularis (Ducke) W.Schultze-Motel: CORRECTED IN sppSA
tmp1[tmp1$notes %in% "check no accepted name",]
#Corrigido na planilha treesSA: Cameraria, Hylaea, Myginda, Cassine, Gymnosporia
#Corrigido: Rauwolfia => Rauvolfia, Topobea => Blakea
#Corrigido: Aulomyrcia => Myrcia, Calycorectes => Eugenia, Gomidesia => Myrcia,
#Corrigido: Hexachlamys, Luma, Paramyrciaria, Phyllocalyx, Stenocalyx 
#Corrigido: Cercouratea => Ouratea, Gomphia => Ouratea
#Corrigido: Matthiola => Guettarda, Tournefortiopsis => Guettarda
#Corrigido: Gerascanthus calophyllus => Cordia 
#Corrigido: Argythamnia dioca => Ditaxis dioica
#Corrigido: nomes de especies de Patagonula => Cordia, Sclerolobium => Tachigali

#List of genera that still needs checking/homogeneizing in the file treesSA
tmp1[tmp1$notes %in% "replaced synonym",]

#List of genera not in Flora do Brasil
g.n.i.fbo = c("Aextoxicon","Alfaroa","Alnus","Alzatea","Austrocedrus","Austrotaxus","Buxus",
              "Carpinus","Casuarina","Colombobalanus","Columellia","Cornus","Cupressus","Desfontainia",
              "Fitzroya","Fouquieria","Garrya","Gomortega","Huertea","Hydrangea","Itea","Juglans",
              "Juniperus","Koeberlinia","Laurelia","Ledenbergia","Liquidambar","Matudaea","Metteniusa",
              "Morella","Myrica","Nothofagus","Oecopetalum","Oreomunnea","Ostrya","Ottoschulzia",
              "Pakaraimaea","Paulownia","Perrottetia","Phyllonoma","Pilgerodendron","Platanus",
              "Poissonia","Poraqueiba","Pseudomonotes","Quercus","Ruptiliocarpon","Saurauia",
              "Styloceras","Taxodium")
tmp1$notes[tmp1$original.search %in% g.n.i.fbo] = ""
tmp1[tmp1$notes %in% "not found",]
#Ok? These genera may need some checking, because they can actually be synonyms of names of genera Reflora.

#### Saving data per family ####
###List of families
dados$family.correct = gsub("Leguminosae-Mim\\.|Leguminosae Subfam\\. Caesalpinioideae \\(Mimosoide\\)","Fabaceae",dados$family.correct)
familias = sort(unique(dados$family.correct))

#Path to the folder in the local directory to receive the files
path="C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//occurrence_data"
for (i in 1:length(familias)){
  familia = familias[i]
  dado = dados[dados$family.correct %in% familia,]
  nome = paste(path,paste(familia,".csv",sep=""),sep="//")
  #write.csv(x=dado, file= nome)
  fwrite(x=dado, file= nome)
} 


########################################################################################################################################################################################-
########################################################################################################################################################################################-
##########################################################
#### CLASSIFYING DIFFERENT TYPES OF DUPLICATED GROUPS ####
##########################################################
rm(list=ls()); gc()

#### GETTING VALIDATED OCCURRENCE DATA ####
#Getting the file paths for the herbarium data
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//occurrence_data",full.names=TRUE)
paths = paths[grepl("ae.csv",paths)] 

#Loading and merging the different collections
lista = vector("list",length(paths))
for (i in 1:length(paths)) {
  path.csv = paths[i]
  tmp = read.csv(path.csv,as.is=TRUE,na.strings=c(""," ","NA"))
  lista[[i]] = tmp
}
dados = do.call(rbind.data.frame,lista)
dados[] <- lapply(dados, as.character)
rm(lista)

#### DETECTING TRUE, PROBABLE AND DUBIOUS DUPLICATES ####
#dados1 = dados[!is.na(dados$dup.ID),]  # data with any indication of duplicates
#dados1$ordem = 1:dim(dados1)[1]
#dados1$dup.ID = as.factor(dados1$dup.ID)
#dt = data.table(dados1) #transforming the data frame into a data.table format
#setkeyv(dt, "dup.ID") # setting 'dup.ID' as key to the data.table (makes computations faster)

#CATEGORIAS DE SPECIMENS DUPLICADOS: 
#true: 4/4, 3/4
#probable: 2/2
#gbif_probable: 1/1 for specimen3
#maybe: 1/2 or 1/1 for strings other than specimen3
#dubious or the comparison specimen has lots of NAs: 1/4 and 2/4

#CATEGORIAS DE GRUPOS DE DUPLICATAS
#creating the data.frame to find typical categories 
#toto = data.table(dt[family.correct %in% "Rutaceae" & is.na(dup.merge),])
#toto = data.table(toto[,c("numTombo","dup.ID","dup.numb","dup.prop","dup.check1","dup.check2","dup.check3","dup.check4","specimen1","specimen2","specimen3","specimen4"),])
#setkeyv(toto, "dup.ID") # setting 'dup.ID' as key to the data.table (makes computations faster)
#toto[,dup.check1 := ifelse(is.na(specimen1), 'na', dup.check1),]
#toto[,dup.check2 := ifelse(is.na(specimen2), 'na', dup.check2),]
#toto[,dup.check3 := ifelse(is.na(specimen3), 'na', dup.check3),]
#toto[,dup.check4 := ifelse(is.na(specimen4), 'na', dup.check4),]
#toto[,dup.check1 := ifelse(dup.check1=="TRUE", 'sim', dup.check1),]
#toto[,dup.check1 := ifelse(dup.check1=="FALSE", 'no', dup.check1),]
#toto[,dup.check2 := ifelse(dup.check2=="TRUE", 'sim', dup.check2),]
#toto[,dup.check2 := ifelse(dup.check2=="FALSE", 'no', dup.check2),]
#toto[,dup.check3 := ifelse(dup.check3=="TRUE", 'sim', dup.check3),]
#toto[,dup.check3 := ifelse(dup.check3=="FALSE", 'no', dup.check3),]
#toto[,dup.check4 := ifelse(dup.check4=="TRUE", 'sim', dup.check4),]
#toto[,dup.check4 := ifelse(dup.check4=="FALSE", 'no', dup.check4),]
#toto = data.table(toto[,c("dup.ID","dup.numb","dup.prop","dup.check1","dup.check2","dup.check3","dup.check4"),])
#toto[,dup.not.na := as.double(dup.numb)/as.double(dup.prop),]
#toto1 = toto[,c("dup.numb","dup.prop","dup.not.na","dup.check1","dup.check2","dup.check3","dup.check4"),]
#toto1$dup.numb = as.double(toto1$dup.numb)
#toto1$dup.prop = as.double(toto1$dup.prop)
#toto1$dup.check1 = as.factor(toto1$dup.check1)
#toto1$dup.check2 = as.factor(toto1$dup.check2)
#toto1$dup.check3 = as.factor(toto1$dup.check3)
#toto1$dup.check4 = as.factor(toto1$dup.check4)

# Dissimilarity matrix
#library(cluster)
#disMat <- daisy(toto1, metric = "gower")

#K-means
#mixedClusters <- kmeans(disMat, centers=17)
#table(mixedClusters$cluster)
#toto$cluster.Km <- mixedClusters$cluster

#Hierarchical clustering
#library(dplyr)
#dist_mat <- dist(toto1, method = 'euclidean')
#hclust_avg <- hclust(disMat, method = 'complete')
#cut_avg <- cutree(hclust_avg, k = 17)
#toto1_cl <- mutate(toto1, cluster = cut_avg)
#count(toto1_cl, cluster)
#toto$cluster.HAC <- toto1_cl$cluster

#Now grouping dup.IDs by the frequency of their clusters
#toto2 = as.data.frame.matrix(table(as.character(toto$dup.ID),toto$cluster.HAC))
#disMat1 <- dist(toto2)

#K-means
#set.seed(6)
#mixedClusters1 <- kmeans(disMat1, centers=12)
#table(mixedClusters1$cluster)
#toto2$cluster.Km <- mixedClusters1$cluster

#Hierarchical clustering
#set.seed(6)
#hclust_avg1 <- hclust(disMat1, method = 'complete')
#cut_avg1 <- cutree(hclust_avg1, k = 15)
#hclust_avg1$labels = cut_avg1
#toto1_cl1 <- mutate(toto2, cluster = cut_avg1)
#count(toto1_cl1, cluster)
#toto2$cluster.HAC <- toto1_cl1$cluster

#Ploting the dendrogram
#require(dendextend)
#avg_dend_obj <- as.dendrogram(hclust_avg1)
# Set the colors of 4 branches
#cols_branches <- rainbow(n=15)
#avg_dend_obj <- color_branches(avg_dend_obj, k = 15, col = cols_branches)
#cores = as.factor(cut_avg1)
#levels(cores) <- cols_branches
#cores <- as.character(cores)
#colors_to_use <- cores[order.dendrogram(avg_dend_obj)]
#labels_colors(avg_dend_obj) <- colors_to_use

#Plotting the dendogram
#par(mar=c(2,1,1,4))
#plot(avg_dend_obj, cex=0.2, horiz = TRUE, xlim=c(20,0),
#  ylim=c(0,50))

#Finding the most abundant classes of duplicates
#toto3 = toto2
#head(toto3)
#sort(table(as.character(toto3$cluster.HAC)))
#table(as.character(toto3$cluster.HAC),as.character(toto3$cluster.Km))


#cluster 4: true dup with one missing municipio and maybe loc_typo
#toto3[as.character(toto3$cluster.HAC) == 4,] 
#dt["cepec_14205|ny_331060|rb_543853",20:55] # one missing locality + loc_typo 
#dt["cepec_29625|hrb_4773|rb_209963",20:55] # one missing locality + loc_typo 
#dt["cpap_15134|spf_101769",20:55] # one missing municipio
#dt["cepec_146524|rb_648511",20:55] # one missing municipio

#cluster 5: true dup with any(is.na(ano)) & unique(municipio)>1
#toto3[toto3$cluster.HAC == 5 & toto3$cluster.Km == 2,] # one/two specimen is.na(ano) (1,1; 1,1; 1,.25 or 1,1; 1,.25; 2,0.5)
#dt["esa_63988|fuel_28557|mbm_244486",20:55] # 2 x is.na(ano)
#dt["esa_63992|fuel_28323",20:55]
#toto3[toto3$cluster.HAC == 5 & toto3$cluster.Km == 2,] # one/two specimen is.na(ano) with one missing locality (1,1; 1,1; 2,0.5)
#dt["inpa_153544|mbm_237999|ny_1141781|ufacpz_18034",20:55]
#dt["mbm_11779|mo_2814604|upcb_7822",20:55]

#cluster 3: almost all or all missing ano
#toto3[toto3$cluster.HAC == 3,] # one/two specimen is.na(ano) (1,1; 1,1; 1,.25 or 1,1; 1,.25; 2,0.5)
#dt["esa_34279|sp_310310|spf_122204|spsf_21665|uec_92625",20:55] # all but one is not is.na(ano)
#dt["esa_110107|rb_663714|spsf_47035",20:55] # no ano
#dt["huefs_107487|spf_176959",20:55] # no ano

#cluster 1: only one specimens has municipio; if two have municpio they may be different
#toto3[as.character(toto3$cluster.HAC) == 1 & toto3$cluster.Km == 4,]
#dt["inpa_88893|mo_2809496|ny_1141767|rb_371023|uec_23939|us_2937866",20:55] # all but one has municipio
#dt["mbm_65413|ny_51868|ny_51869|rb_202528|ub_17576",20:55] # all but one has municipio
#dt["botu_22177|cgms_7533|spf_134080|uec_108900|ufv_23706|vic_23706",20:55] # two have municipio bu they are different
#dt["mo_2809165|ny_382556|rb_195543|ub_165335|us_2845235",20:55] # all but one has municipio
#toto3[as.character(toto3$cluster.HAC) == 1 & toto3$cluster.Km == 1,] 
#dt["inpa_84021|mo_3384979|ny_1141925",20:55] # only one has municipio
#dt["joi_825|spf_183963|spf_3577",20:55] # only one has municipio
#dt["hucs_13836|ny_1003008|ucs_13836",20:55] # only one has municipio

#group 2: one/two missing municipio or one missing municipio and one is.na(ano)
#toto3[toto3$cluster.HAC == 2 & toto3$cluster.Km == 11,] # one/two missing municipio (3,.75; 4,1; 4,1; 1,1) or (4,1, 4,1, 1,1)
#dt["hbr_23456|hrb_13127|ny_382225|spf_1141",20:55]
#dt["hbr_29019|hrb_13099|spf_1144",20:55]
#toto3[toto3$cluster.HAC == 2 & toto3$cluster.Km == 11,] # one missing municipio and one is.na(ano)
#dt["cepec_80760|huefs_47839|mbm_248941|spf_134602",20:55]
#dt["cepec_80798|ny_331128|spf_177296",20:55]
#toto3[toto3$cluster.HAC == 2 & toto3$cluster.Km == 7,] # one missing municipio, careful with mix of specimens with different families (same last name but not same colector name)
#dt["flor_46330|fuel_13231|fuel_18165|huefs_41568|sjrp_18446|sp_327659|upcb_38099",20:55]
#dt["flor_9438|hucs_1141|mbm_103625|ucs_1141",20:55]
#toto3[toto3$cluster.HAC == 2 & toto3$cluster.Km == 7,] # typo in colDate unique(ano)>1
#dt["cepec_86789|mo_100237629|ny_1112647|spf_154216|uesc_11669",20:55]
#dt["cepec_87909|mo_3400522|ny_566389|spf_154214",20:55]

#cluster 8: one without municipio among many >=4, maybe with is.na(ano) or uniqueN(coletor.last.name)>1
#toto3[toto3$cluster.HAC == 8 & toto3$cluster.Km == 8,] # 
#dt["inpa_105975|ma_381203-1|mbm_73078|mo_3407804|ub_165241|us_2931776",20:55] # one without municipio among many >=4
#dt["cepec_47081|e_205530|ny_612621|sp_247379|spf_68564",20:55] # one without municipio among many >4
#dt["cas_2102|cepec_61518|huefs_17897|mbm_167894|mo_1837349|ny_245871|spf_90892",20:55] # one without municipio among many >4

#toto3[toto3$cluster.HAC == 8 & toto3$cluster.Km == 7,] 
#dt["f_389623f|false_v0389623f|icn_121707|inpa_130243|mbm_104581|ny_382686",20:55] # two without municipio among many >=4 and one without ano
#dt["cepec_66106|mbml_635|paca_635|rb_325454|rb_371086|spf_76545",20:55] # two without municipio among many >=4 and one with colName typo

##cluster 6: one is.na(ano) or is.na(municipio) among many >6 or variable municipio names
#toto3[toto3$cluster.HAC == 6 & toto3$cluster.Km == 10,] 
#dt["cesj_50841|fuel_32782|hcf_15926|hucs_32946|hufu_51064|rb_747080|s_15-23058|ucs_32946|ufu_51064",20:55] # two without municipio among many >6
#dt["cgms_13110|cgms_17971|cor_8511|hufu_46531|uec_146250|ufu_46531",20:55] # one without municipio among many >6
#dt["botu_26006|furb_12606|huco_797|hucs_41938|icn_179890|ucs_41938",20:55] # one without municipio among many >6
#toto3[toto3$cluster.HAC == 6 & toto3$cluster.Km == 8,] 
#dt["inpa_157518|mo_3405478|ny_235418|ny_235419|ufrr_1428|us_3338477",20:55] # variable municipio names
#dt["hbr_21953|mbm_48775|ny_382171|paca_65240|us_2278662",20:55] # one is.na(ano) among many
#dt["asu_46622|bhcb_20603|huefs_33111|mbm_169628|spf_94736|spsf_19632",20:55] # one is.na(ano) and one subsp. among many
#dt["heph_5697|huem_1450|mbm_99171|spf_41809|uec_45061",20:55] # one is.na(ano) among many

#cluster 7: # one is.na(ano) among many >7 or one is.na(ano) and two is.na(municipio) among many
#toto3[toto3$cluster.HAC == 7 & toto3$cluster.Km == 12,] 
#dt["flor_28356|flor_5195|hbr_36868|mbm_51110|ny_331007|paca_66979|paca_66980|rb_259164|rb_367333|us_2575055",20:55] # one is.na(ano) and two is.na(municipio) among many
#dt["asu_46545|cepec_78538|flor_29148|huefs_33106|mbm_204296|mo_3401892|ny_382280|spf_119503",20:55] # one is.na(ano) among many  >7
#dt["cen_46332|cesj_31889|flor_36777|fuel_27300|furb_3421|hucs_13586|mbm_233460|ub_165204|ucs_13586|upcb_37844",20:55] # one is.na(ano) among many >7

#cluster 9: one or more colName typo or one colName typo + one is.na(municipio)
#toto3[toto3$cluster.HAC == 9 & toto3$cluster.Km == 6,] 
#dt["cepec_23160|hrb_5911|inpa_98548",20:55] # one colector is not the same, but same site, colNumb, year and spp: Label error?
#dt["bah_7449|flor_30519|hrb_19685|mac_5325",] # colName typo
#dt["cen_78949|ub_184509",] # colName typo
#dt["alcb_86952|cesj_50900",] # one colector is not the same, but same site, colNumb, year and spp: Label error?  
#toto3[toto3$cluster.HAC == 9 & toto3$cluster.Km == 2,] 
#dt["sp_251395|spf_144041|ufp_38072",20:55] # one colName typo + one is.na(municipio)
#dt["cepec_66473|rb_370620|spf_194220",20:55] # one colName typo + one is.na(municipio) 
#dt["bhcb_139420|hufu_56106|ufu_56106",20:55] # one colName typo + one is.na(municipio)

#cluster 15 e 14: only 1-2 municipio !is.na() among many >6-8
#toto3[toto3$cluster.HAC == 15,] 
#dt["fuel_23748|hufu_11047|hufu_15284|spf_120810|ufu_11047|ufu_15284|ufv_14187|vic_14187",20:55] # only 2 municipio among many >6
#dt["cepec_20316|e_177882|ny_382641|ny_382642|spf_38682|uec_41620",20:55] # only 2 municipio among many >6
#dt["cepec_19187|ny_331080|spf_38681|uec_41623|us_3027165",20:55] # only 2 municipio among many >5
#toto3[toto3$cluster.HAC == 14,] 
#dt["cas_124091|inpa_90184|mbm_116887|mo_1839834|ny_335317|r-tipos_145320|r_145320|rb_228886|spf_125000",20:55] # only 1 municipio among many >8
#dt["mo_2802480|mo_2802593|ny_929266|ny_929267|ub_165230|us_2833234|us_2833235",20:55] # only 1 municipio among many >8

#cluster 11 e 12: 1 is.na(ano)/is.na(municipio) among many >8-10 or 2-3 is.na(municipio) + 1 colName typo among many >8-10
#toto3[toto3$cluster.HAC == 11,] 
#dt["cor_9747|e_678941|fuel_39186|hufu_45049|hufu_60771|huto_4444|inpa_215062|mo_100173477|spf_164359|spf_170681|spf_173068|ub_11134|ufmt_36028|ufu_45049|ufu_60771",20:55] # 3 is.na(municipio) + 1 colName typo among many >10
#dt["cesj_32312|esa_39783|flor_28106|inpa_195546|mbm_210434|rb_341924|rb_664100|sjrp_16045|spf_126138|spsf_23202",20:55] # 1 is.na(ano) among many >8
#dt["alcb_71149|cgms_63936|fuel_39381|hcf_5141|hucp_17022|hucp_17060|hucs_26527|huefs_115952|spf_226329|ucs_26527",20:55] # 1 is.na(municipio) among many >8
#toto3[toto3$cluster.HAC == 12,] 
#dt["alcb_60323|ase_12536|asu_46626|cesj_60080|esa_85326|fuel_34385|heph_19163|huefs_71412|ma_780751-1|paca_87091|rb_392996|rb_402228|sp_364328|spf_144122|spsf_31877|upcb_46415",] # 1 is.na(municipio) + 1 municipio typo among many >10
#dt["botu_26520|esa_16352|flor_19717|hrb_24833|huefs_9746|inpa_147899|mbm_97670|mo_2813245|ny_382738|spf_41816|uec_45068|upcb_15807",] # 1 is.na(ano) among many >10

#cluster 10 e 13: 3-4 is.na(municipio) + 1 is.na(ano)/colDate typo or + municipio typo among many 8-10
#toto3[toto3$cluster.HAC == 10,] 
#dt["alcb_50052|hucs_13755|huefs_44187|hufu_57227|inpa_200830|mbm_222933|mo_3345299|spf_130495|spsf_27116|ucs_13755|ufu_57227|upcb_38787",20:55] # 1 is.na(ano) + 3 is.na(municipio) + 1 != municipio among many >8
#dt["cas_2099|cepec_61000|huefs_17576|k_531002|k_531003|mbm_230231|mo_1837377|ny_245874|rb_543843|sp_332833|spf_81267",20:55] # 4 is.na(municipio) + 1 colDate typo among many >8
#dt["cesj_58500|hucs_40059|hufu_65759|icn_165575|ipa_87646|mbm_355922|rb_780386|ucs_40059|ufu_65759",20:55] # 3 is.na(municipio) among many >8
#toto3[toto3$cluster.HAC == 13,] 
#dt["bm_547269|cen_29456|f_71321f|false_v0071321f|g_380074|gh_93019|inpa_192025|mo_1841687|ny_214339|r-tipos_198855|r_198855|rb_327456|rfa_42600|sp_318325|spf_126052|uec_94398|us_3351376",20:55] # 1 is.na(ano) among many >10


################################################################################################################################################################################################################################################-
#### CLASSIFICATION OF THE MOST COMMON GROUPS OF dup.ID ####
dados1 = dados[!is.na(dados$dup.ID),]  # data with any indication of duplicates
dados1$ordem = 1:dim(dados1)[1]
dados1$dup.ID = as.factor(dados1$dup.ID)
dt = data.table(dados1) #transforming the data frame into a data.table format
setkeyv(dt, "dup.ID") # setting 'dup.ID' as key to the data.table (makes computations faster)

#### Case 1: duplicate IDs with only TRUE DUPLICATES (3/4 or 4/4 strings duplicated) ###
dt[, dup.merge := ifelse(all(dup.numb==4) & all(dup.prop==1), 
                         "ok", NA_character_), by = dup.ID]
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb>=3) & all(dup.prop>=0.75) & all(dup.check1==TRUE) & all(dup.check2==TRUE) & all(dup.check4==TRUE), 
                                         "ok", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

#### Case 2: duplicate IDs only with PROBABLE DUPLICATES (1/1 when only string search 3 available) ###
#This is very common for gbif occurrences that has no county informartion available (specimen1, specimen2 and specimen4 are NAs)
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb==1) & all(dup.prop==1) & all(dup.check3==TRUE), 
                                         "ok_gbif", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

#### Case 3: all or almost all duplicates don't have an available colDate (only string search 1 available) - cluster 3 ###
#This is very common for occurrences were the recordDate is not provided at ll or if almost all are missing (specimen2, specimen3 and specimen4 are NAs)
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.check1==TRUE) & all(is.na(ano)) & sum(is.na(municipio))==0 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1,
                                         "ok_no_colDate", NA_character_), by = dup.ID]
#dt["esa_110107|rb_663714|spsf_47035",20:55] # no ano
table(dt$dup.merge);table(is.na(dt$dup.merge))

### Case 4: all possible combinations of  missing info and typos ###
# 1-2 missing ano and 1-2 missing municipio, colName and colDate typo, all species are the same ###
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))%in%c(1,2) & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==2 & uniqueN(coletor.last.name)==2 & uniqueN(coletor.number)==1 & uniqueN(species.correct)==1,
                                         "ok_colDate_loc_NA_colDate_colName_typo", NA_character_), by = dup.ID]
#dt["cepec_42981|huefs_13196|mbm_150598|mbm_163184|rb_1199501|rb_667959",c(5:56),]
# 1-2 missing ano and 1-2 missing municipio, municipio and colDate typo, all species are the same ###
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))%in%c(1,2) & uniqueN(municipio, na.rm=TRUE)==2 & uniqueN(ano, na.rm=TRUE)==2 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(species.correct)==1,
                                         "ok_colDate_loc_NA_loc_colName_typo", NA_character_), by = dup.ID]
# 1-2 missing ano and 1-2 missing municipio, municipio and colName typo, all species are the same ###
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))%in%c(1,2) & uniqueN(municipio, na.rm=TRUE)==2 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==2 & uniqueN(coletor.number)==1 & uniqueN(species.correct)==1,
                                         "ok_colDate_loc_NA_loc_colDate_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

# 1-2 missing ano and 1-4 missing municipio, colName typo, all families are the same ###
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))%in%c(1,2,3,4) & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==2 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_colDate_loc_NA_colName_typo", NA_character_), by = dup.ID]
# 1-2 missing ano and 1-4 missing municipio, colDate typo, all families are the same ###
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))%in%c(1,2,3,4) & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==2 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_colDate_loc_NA_colDate_typo", NA_character_), by = dup.ID]
# 1-2 missing ano and 1-4 missing municipio, municipio typo, all families are the same ###
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))%in%c(1,2,3,4) & uniqueN(municipio, na.rm=TRUE)==2 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_colDate_loc_NA_loc_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

# 1-2 missing ano and 1-4 missing municipio, no typos, all families are the same
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))%in%c(1,2,3,4) & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_colDate_loc_NA", NA_character_), by = dup.ID]
# 1 missing ano and 1-2 missing municipio, no typos
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))==1 & sum(is.na(municipio))%in%c(1,2) & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1,
                                         "ok_colDate_loc_NA", NA_character_), by = dup.ID]
#dt["inpa_153544|mbm_237999|ny_1141781|ufacpz_18034",20:55]
#dt["cepec_80760|huefs_47839|mbm_248941|spf_134602",20:55]
#dt["mbm_11779|mo_2814604|upcb_7822",20:55]
#dt["cepec_80798|ny_331128|spf_177296",20:55]
table(dt$dup.merge);table(is.na(dt$dup.merge))

# 1-2 missing ano and 1 municipio typo, all families are the same
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))==0 & uniqueN(municipio, na.rm=TRUE)==2 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_colDate_NA_loc_typo", NA_character_), by = dup.ID]
# 1-2 missing ano and 1 colector.name typo, all families are the same
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))==0 & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==2 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_colDate_NA_colName_typo", NA_character_), by = dup.ID]
# 1-2 missing ano and 1 colDate typo, all families are the same
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))==0 & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==2 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_colDate_NA_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

# 1-4 missing municipio and 1 municipio typo, all families are the same
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))==0 & sum(is.na(municipio))%in%c(1,2,3,4) & uniqueN(municipio, na.rm=TRUE)==2 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1 & uniqueN(stateProvince, na.rm=TRUE)==1,
                                         "ok_loc_NA_typo", NA_character_), by = dup.ID]
# 1-4 missing municipio and 1 colector.name typo, all families are the same
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))==0 & sum(is.na(municipio))%in%c(1,2,3,4) & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==2 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_loc_NA_colName_typo", NA_character_), by = dup.ID]
# 1-4 missing municipio and 1 colector.name typo, all families are the same
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))==0 & sum(is.na(municipio))%in%c(1,2,3,4) & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==2 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_loc_NA_colDate_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

### Case 5: missing info for individual fields or typos ###
# 1-2 missing ano; same family
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))%in%c(1,2) & sum(is.na(municipio))==0 & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_colDate_NA", NA_character_), by = dup.ID]
#dt["cgms_23685|fuel_17041|icn_121399"]
#dt["esa_34279|sp_310310|spf_122204|spsf_21665|uec_92625",20:55] # all but one is not is.na(ano)
#dt["esa_63988|fuel_28557|mbm_244486",20:55] # 2 x is.na(ano)
table(dt$dup.merge);table(is.na(dt$dup.merge))
# 1-4 missing municipio
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))==0 & sum(is.na(municipio))%in%c(1,2,3,4) & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1,
                                         "ok_loc_NA", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

# colName typo, all families equal
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))==0 & sum(is.na(municipio))==0 & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==2 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_colName_typo", NA_character_), by = dup.ID]
# colDate typo within same collection, all families equal
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))==0 & sum(is.na(municipio))==0 & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==2 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(collectionCode)==1,
                                         "ok_colDate_typo_same_collection", NA_character_), by = dup.ID]
# colDate typo, all families equal
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))==0 & sum(is.na(municipio))==0 & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==2 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1,
                                         "ok_colDate_typo", NA_character_), by = dup.ID]
# colDate typo, all species are equal
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))==0 & sum(is.na(municipio))==0 & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==3 & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(species.correct)==1,
                                         "ok_colDate_typo", NA_character_), by = dup.ID]

# colName typo, all species equal
dt[is.na(dup.merge), dup.merge := ifelse(sum(is.na(ano))==0 & sum(is.na(municipio))==0 & uniqueN(municipio, na.rm=TRUE)==1 & uniqueN(ano, na.rm=TRUE)==1 & uniqueN(coletor.last.name)==2 & uniqueN(coletor.number)==1 & uniqueN(species.correct)==1,
                                         "ok_colName_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

### Extra cases ###
# No typos in author name and locality, but with some missing colDate  
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb%in%c(1,4)) & all(dup.prop==1) & all(dup.check3==TRUE) & any(is.na(specimen1)), 
                                         "ok_splink", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))
# At least one with all strings with some information and all other are NA
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb==1) & all(dup.prop %in% c(1,0.25)) & uniqueN(specimen1, na.rm = TRUE)==1 & all(dup.check3==TRUE) & all(dup.check1==FALSE) & all(dup.check2==FALSE) & all(dup.check4==FALSE), 
                                         "ok_splink", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))
# duplicate IDs with TRUE (3/4 and 4/4) AND PROBABLE (1/1 and 2/2) DUPLICATES ###
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.prop>=0.75), "ok_splink", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

##checking
#dt["esa_63992|fuel_28323",20:55]
#dt["herbmg_mg154347|hfsl_1981|inpa_179786|inpa_183143|upcb_25278",20:55]
#dt["inpa_154634|inpa_172472"]

# Same family, colNumber and year but different colName
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb%in%c(1,2,4)) & all(dup.prop%in%c(1,0.5,0.25)) & uniqueN(specimen2,na.rm=TRUE)==1 & all(!is.na(specimen1)), 
                                         "probable_colName_typo", NA_character_), by = dup.ID]
# Same family, colNumber, colDate, municipio but many different authors
dt[is.na(dup.merge), dup.merge := ifelse(uniqueN(coletor.last.name)<=3 & uniqueN(coletor.number)==1 & uniqueN(ano)==1 & uniqueN(municipio)==1 & uniqueN(family.correct)==1, 
                                         "probable_colName_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))
# All specimen are complete but only specimen 3 is duplicated (only loc is different)
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb==1) & all(dup.prop=0.25) & uniqueN(specimen3,na.rm=TRUE)==1 & all(!is.na(specimen1)), 
                                         "probable_loc_typo", NA_character_), by = dup.ID]
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb%in%c(1,4)) & all(dup.prop%in%c(1,0.25)) & uniqueN(specimen3,na.rm=TRUE)==1 & all(!is.na(specimen1)), 
                                         "probable_loc_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

# Missing or typos in the locality (same author, same colNum, colYear, same family but different places) ###
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb%in%c(1,2,4)) & all(dup.prop%in%c(0.5,1)) & uniqueN(coletor.number)==1 & uniqueN(ano)==1 & uniqueN(family.correct)==1, 
                                         "probable_loc_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))
# All specimens with at least 2 duplicated strings
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.prop>=0.5), "probable_mixed", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))
# Everything good for most of them but some may have typos in date, locality or author name (same author, same colNum, colYear, same family but different places) ###
dt[is.na(dup.merge), dup.merge := ifelse(any(dup.numb%in%c(2,3,4)) & any(dup.prop==1) & uniqueN(coletor.last.name)==2 & uniqueN(ano)==2 & uniqueN(coletor.number)==1 & uniqueN(family.correct)==1, 
                                         "probable_colDate_colName_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))
# Same species, last name, colNumber, same locality, but many different years 
dt[is.na(dup.merge), dup.merge := ifelse(uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(ano, na.rm = TRUE)%in%c(2,3) & uniqueN(municipio, na.rm = TRUE)==1 & uniqueN(species.correct)==1, 
                                         "probable_colDate_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

# Same last name, colNumber, same locality, but different years and species
dt[is.na(dup.merge), dup.merge := ifelse(uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(ano, na.rm = TRUE)%in%c(2,3) & uniqueN(municipio, na.rm = TRUE)==1 & uniqueN(species.correct)==2, 
                                         "probable_colDate_typo_dif_spp", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

# Same species, colNumber, same locality, same year, but different colNames and species
dt[is.na(dup.merge), dup.merge := ifelse(uniqueN(coletor.last.name)%in%c(2,3) & uniqueN(coletor.number)==1 & uniqueN(ano, na.rm = TRUE)==1 & uniqueN(municipio, na.rm = TRUE)==1 & uniqueN(species.correct)==1, 
                                         "probable_colName_loc_typo", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

# Same colNumber, colDate, locality, but different colNames and spp names
dt[is.na(dup.merge), dup.merge := ifelse(uniqueN(coletor.last.name, na.rm = TRUE)%in%c(2,3) & uniqueN(coletor.number)==1 & uniqueN(ano)==1 & uniqueN(municipio)==1 & uniqueN(species.correct)==2, 
                                         "probable_colName_typo_dif_spp", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

### PROBLEMATIC CASES ###
# Problable confusion between specimens in the same collection or bad indirect duplicate searching (same author, same place, same year, but IDs completely opposite) ###
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb==1) & all(dup.prop==0.25) & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(ano)==1 & uniqueN(family.correct)>1 & uniqueN(collectionCode)==1, 
                                         "problematic_duplicate", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))
#Other problems possible problematic duplicates ###
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb %in% c(1)) & all(dup.prop%in%c(0.25)) & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(ano)==1 & uniqueN(family.correct)>1, 
                                         "problematic_duplicate", NA_character_), by = dup.ID]
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb %in% c(1,4)) & all(dup.prop%in%c(0.25,1)) & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(ano)==1 & uniqueN(family.correct)>1, 
                                         "problematic_duplicate", NA_character_), by = dup.ID]
dt[is.na(dup.merge), dup.merge := ifelse(all(dup.numb %in% c(1,3)) & all(dup.prop%in%c(0.25,0.75)) & uniqueN(coletor.last.name)==1 & uniqueN(coletor.number)==1 & uniqueN(ano)==1 & uniqueN(family.correct)>1, 
                                         "problematic_duplicate", NA_character_), by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))

# Dubious duplicates due to same locality but all collectors are different (all indirect duplicates)
dt[is.na(dup.merge), dup.merge := ifelse(uniqueN(specimen1,na.rm=TRUE)>=2 & uniqueN(specimen2,na.rm=TRUE)>=2 & uniqueN(specimen3,na.rm=TRUE)>=2 & uniqueN(specimen4,na.rm=TRUE)>=2, 
                                         "dubious_duplicate", NA_character_), by = dup.ID]
dt[is.na(dup.merge), dup.merge := ifelse(uniqueN(specimen1,na.rm=TRUE)>=2 | uniqueN(specimen2,na.rm=TRUE)>=2 & uniqueN(specimen3,na.rm=TRUE)>=2 & uniqueN(specimen4,na.rm=TRUE)>=2, 
                                         "dubious_duplicate", NA_character_), by = dup.ID]
table(dt$dup.merge); table(is.na(dt$dup.merge))

# Other problems that still need checking ###
dt[is.na(dup.merge), dup.merge := "not_checked", by = dup.ID]
table(dt$dup.merge);table(is.na(dt$dup.merge))
#dt[is.na(dup.merge),c(5:11,32:39,42:50),][18:25,]

#### ELIMINATING BAD dup.IDs BEFORE MERGING THE INFORMATION ####
## REMOVING THE PROBLEMATIC SPECIMENS FROM DUBIOUS DUPLICATES BEFORE MERGING THE INFO
dt[dup.merge == "dubious_duplicate", dup.ID := ifelse(dup.numb == 1 & dup.prop == 0.25, 
	NA_integer_, dup.ID), by = dup.ID, ]
dt[dup.merge == "dubious_duplicate", dup.merge := ifelse(dup.numb == 1 & dup.prop == 0.25, 
	NA_character_, "ok_dubious_dups_removed"), by = dup.ID, ]
dt[dup.merge == "ok_dubious_dups_removed", dup.ID := ifelse(uniqueN(numTombo) == 1, 
	NA_integer_, dup.ID), by = dup.ID,]
dt[dup.merge == "ok_dubious_dups_removed", dup.merge := ifelse(uniqueN(numTombo) == 1, 
	NA_character_, dup.merge), by = dup.ID,]

## REMOVING THE PROBLEMATIC SPECIMENS FROM PROBLEMATIC DUPLICATES BEFORE MERGING THE INFO
dt[dup.merge == "problematic_duplicate", dup.ID := ifelse(dup.numb < 3 & dup.prop < 0.75, 
	NA_integer_, dup.ID), by = dup.ID, ]
dt[dup.merge == "problematic_duplicate", dup.merge := ifelse(dup.numb < 3 & dup.prop < 0.75, 
	NA_character_, "ok_problematic_dups_removed"), by = dup.ID, ]
dt[dup.merge == "ok_problematic_dups_removed", dup.ID := ifelse(uniqueN(numTombo) == 1, 
	NA_integer_, dup.ID), by = dup.ID,]
dt[dup.merge == "ok_problematic_dups_removed", dup.merge := ifelse(uniqueN(numTombo) == 1, 
	NA_character_, dup.merge), by = dup.ID,]


#### INSPECTING CLASSES ####
#check the specimesn that changed families to see if it is not a duplicate-grouping error

### NEEDS DOUBLE CHECKING ###
#REMAINING PROBLEMS:
dt[dup.ID == "cvrd_14438|cvrd_14466|rb_853546"] # ok_colName_typo: ok some authors are different but ALL with in the same place with same collector number
dt[dup.ID == "moss_14359|moss_14367|moss_14368"] # ok_colName_typo: problem! all authors are different but with a collector number '1' in the collectinng in the place and year (filed course)
dt['huto_3916|iac_53454|ibge_67532'] #probable_loc_typo

#OKs:
#dt["eac_35164|ipa_61941|moss_14231|us_3334796"] # ok: ok!
#dt["br_5739370|k_485754|ny_320667|rb_541637|s_6071|us_1660947"] # ok_splink: ok!
#dt["us_3243651|us_3269498|us_3269499"] # sim_gbif: ok!
#dt["vic_51104|vic_51112|vies_6444"] # ok_splink: ok!
#dt["br_8415134|f_71854f|false_v0071854f|k_35373|mo_1884737|ny_1271368|us_1660933"] # ok_mixed: ok!
#dt["cen_81051|ny_1048258|ny_1048282|us_2579094|us_2579098|us_3274262"] # probable1: all problems with exclusion of NA search strings which was excluding all author names with an 's'+any letter+'n'
#dt["alcb_10844|cepec_5255|hrb_18418"] # probable1: all problems with exclusion of NA search strings which was excluding all author names with an 's'+any letter+'n'
#dt["eac_38349|ipa_69527|k_1046289|spf_173119"] # typos: ok despite of possible date error in one of the specimens
#dt["cepec_74267|hrb_38589|rb_506208"] # colName typo: ok!
#dt["rb_471292|rffp_442"] # colName typo: ok despite of having different collectors! Error in the label?
#dt["bhcb_4989|cesj_27060"] # colName typo: ok despite of having different collectors! Error in the label?
#dt["a_11059|k_644378|k_644379|mo_2131861|ny_126991"] # probable no date: ok!
#dt["us_1804913|us_1804914|us_1804915"] # probable no date: ok!
#dt["bm_598000|f_68812f|g_436515|g_436516|g_436517|k_1137444"] # probable_gbif: ok!
#dt["bri_738402|canb_305902.1|cns_62150.1|cns_62150.5|cns_62151.3|cns_62152.4"] # probable_gbif: ok!
#dt["a_66430|bm_624332|br_6411077|br_8719553|br_8719560|cas_1439|f_60127f|gh_66431|k_471095|mo_2223469|mo_2226241|ny_97881|ny_97882|us_42713|yu_69982"] # probable_gbif: ok!
#dt["a_75708|f_72106f|false_v0072106f|mo_1967451|ny_273557|r-tipos_75085|r_75085|s_5-2214|us_1693224"] # probable_splink: ok!
#dt["alcb_40868|huefs_26323|spf_123884"] # probable_splink: ok!
#dt["cepec_25540|ny_663803|rb_363054|us_2950400"] # probable_gbif1: ok!
#dt["a_46211|inpa_230772|k_800937|ny_428204|ny_428205|r-tipos_75387|r_75387|rb_537037|us_1907122|us_2592663"] # locality typo: ok!
#dt[dup.ID == "bhcb_9530|mbm_172846|mbm_173259|ub_157404"] # one of the specimens of the same author with same number at same place in the same collection with different family/species: label error


#### CONVERTING DETAILED DUPLICATED CATEGORIES INTO MORE SIMPLIFIED ONES ####
table(dt$dup.merge);table(is.na(dt$dup.merge))
dt[, dup.merge1 := dup.merge] # creating the new columns for taxonomic check 
dt[, dup.merge1 := ifelse(dup.merge %in% c("dubious_duplicate","problematic_duplicate","not_checked"), "no", dup.merge1), ] 
dt[, dup.merge1 := ifelse(dup.merge %like% "^ok|^ok_", "ok", dup.merge1), ]
dt[, dup.merge1 := ifelse(dup.merge %like% "^probable_", "probable", dup.merge1), ]
table(dt$dup.merge1);table(is.na(dt$dup.merge1))

#### SAVING THE CALSSIFICATION IN THE MAIN DATA FRAME ####
setkeyv(dt, c("ordem")) #re-ordering the data.table
table(dados1$ordem == dt$ordem)
dados1$dup.ID  = as.character(dt$dup.ID)
dados1$dup.merge  = as.character(dt$dup.merge)
dados1$dup.merge1 = as.character(dt$dup.merge1)

table(dados$numTombo[!is.na(dados$dup.ID)] == dados1$numTombo)
dados$dup.merge[!is.na(dados$dup.ID)]  = as.character(dados1$dup.merge)
dados$dup.merge1 = NA
dados$dup.merge1[!is.na(dados$dup.ID)] = as.character(dados1$dup.merge1)
dados$dup.ID[!is.na(dados$dup.ID)]  = as.character(dados1$dup.ID)


########################################################################################################################################################################################-
########################################################################################################################################################################################-
#######################################################
#### MERGING INFORMATION FROM DUPLICATED SPECIMENS ####
#######################################################
dados1 = dados[!is.na(dados$dup.ID),]  # data with any indication of duplicates
dados1$ordem = 1:dim(dados1)[1]
dados1$dup.ID = as.factor(dados1$dup.ID)
dt = data.table(dados1) #transforming the data frame into a data.table format
setkeyv(dt, "dup.ID") # setting 'dup.ID' as key to the data.table (makes computations faster)

#### FINDING THE MOST RECENT TAXONOMICALLY VALIDATE NAME FOR EACH DUPLICATED ID ####
### AND HOMOGENEIZING THE TAXONOMY WITHIN DUPLICATED GROUPS ###
## Are all taxonomically-validated species names equal within a group?
dt[, same_spp := "vazio"] # creating the new columns for taxonomic check 
#All names for taxonomically-validated specimens are the same? Then, mark as 'yes'
dt[dup.merge1 %in% c("ok","probable") & tax.check == "TRUE", same_spp := if(uniqueN(species.correct)==1) "sim" else same_spp, by = dup.ID]
dt[, same_spp := if(any(same_spp == "sim") & uniqueN(species.correct)==1) "sim" else same_spp, by = dup.ID]
dt[, same_spp := if(any(same_spp == "sim") & uniqueN(species.correct)>1) "no" else same_spp, by = dup.ID]
#Not all names for taxonomically-validated specimens are the same? Then, mark as 'no'
dt[dup.merge1 %in% c("ok","probable") & tax.check == "TRUE", same_spp := if(uniqueN(species.correct)>1) "no" else same_spp, by = dup.ID]
dt[, same_spp := if(any(same_spp == "no")) "no" else same_spp, by = dup.ID]

#checking
#dt["acam_1389|cvrd_4379|huefs_84071",c(32:39,49,56:57),] # tax.check should become all "sim": ok!
#dt["acam_1377|cvrd_4342|huefs_36221|huefs_84053|mo_2849959",c(32:39,49,56:57),] # tax.check should become all "no": ok!
#dt["vies_6602|vies_8205",c(32:39,49,56:57),] # tax.check should become all "vazio" (no taxonomically validated name available for any of the duplicates): ok!
#table(dt$same_spp)
#table(dt$same_spp,dt$tax.check)
#table(dt$same_spp,dt$dup.numb)
#table(dt$same_spp,dt$dup.prop)

## Defining the most up-to-date species name for the case of >=1 taxonomically validated names (new column species.correct1)
# creating the new columns for taxonomic check
dt[, c("family.correct1","species.correct1","determinador.name1","ano.det1","tax.check1") := list(family.correct,species.correct,determinador.name, ano.det,tax.check)] # creating the new columns for taxonomic check
#Saving for each duplicate group ID, the most recent identification, in the case of disagreement between the identification of taxonomically-validated specimens
dt[same_spp == "no", "family.correct1" := family.correct[tax.check %in% TRUE][which.max(ano.det[tax.check %in% TRUE])], by = dup.ID]
dt[same_spp == "no", "species.correct1" := species.correct[tax.check %in% TRUE][which.max(ano.det[tax.check %in% TRUE])], by = dup.ID]
dt[same_spp == "no", "determinador.name1" := determinador.name[tax.check %in% TRUE][which.max(ano.det[tax.check %in% TRUE])], by = dup.ID]
dt[same_spp == "no", "ano.det1" := ano.det[tax.check %in% TRUE][which.max(ano.det[tax.check %in% TRUE])], by = dup.ID]
dt[same_spp == "no", "tax.check1" := tax.check[tax.check %in% TRUE][which.max(ano.det[tax.check %in% TRUE])], by = dup.ID]
dt[same_spp == "sim", determinador.name1 := determinador.name[tax.check == TRUE & which.max(ano.det)][1], by = dup.ID]
dt[same_spp == "sim", ano.det1 := ano.det[tax.check == TRUE & which.max(ano.det)][1], by = dup.ID]
dt[same_spp == "sim", tax.check1 := if(any(tax.check1== TRUE)) "TRUE" else tax.check1, by = dup.ID]
table(dt$tax.check); table(dt$tax.check1)
#checking
#dt["acam_1389|cvrd_4379|huefs_84071",c(32:39,49,56,57:61),] # species.correct1 should be the same as species.correct: ok!
#dt["acam_1377|cvrd_4342|huefs_36221|huefs_84053|mo_2849959",c(32:39,49,56,57:61),] # species.correct1 may not be the same as species.correct: ok!
#dt["vies_6602|vies_8205",c(32:39,49,56,57:61),] # species.correct1 should be the same as species.correct: ok!
#dt["herbmg_mg154347|hfsl_1981|inpa_179786|inpa_183143|upcb_25278"] #dup.merge = "dubiuos" should all remain the same
#dt["inpa_154634|inpa_172472"] 
#dt["icn_90731|icn_90733|mbm_144950|mbm_144951|sp_256375"] # different determinations, the one chosen should be of the specialist

##############################################################################################################################################################-
#### FINDING THE BEST COORDINATES AND GEOGRAPHICAL VALIDATION FOR EACH DUPLICATED ID ####
## Defining the most up-to-date species locality for different degrees of resolution (cases 1 to 10)

# removing NAs and creating the new columns for locality and geographical check
dt[,geo.check := ifelse(is.na(geo.check),"no_cannot_check",geo.check),]
dt[,resolution.coord := ifelse(is.na(resolution.coord),"no_coord",resolution.coord),]
dt[,latitude.work := ifelse(is.na(latitude.work),"no_coord",latitude.work),]
dt[,longitude.work := ifelse(is.na(longitude.work),"no_coord",longitude.work),]
dt[,loc.correct := ifelse(is.na(loc.correct),"no_loc",loc.correct),]
dt[, c("latitude.work1","longitude.work1","origin.coord1","resolution.coord1","geo.check1","af.check1","loc.correct1") := list(latitude.work,longitude.work,origin.coord,resolution.coord,geo.check,af.check,loc.correct)] # creating the new columns for taxonomic check
dt[,geo.dup := NA_character_,]

#correcting a problem of the gazetteer (Vitória, ES)
dt[dt$dup.ID %in% "r_207232|vies_7525",] # was a problem in the gazetteer
dt[loc.correct %in% "brazil_espirito santo_vitoria" & geo.check %in% "ok_county_gazet", latitude.work := "-20.302988", ]
dt[loc.correct %in% "brazil_espirito santo_vitoria" & geo.check %in% "ok_county_gazet", latitude.work1 := "-20.302988", ]
dt[loc.correct %in% "brazil_espirito santo_vitoria" & geo.check %in% "ok_county_gazet", longitude.work := "-40.308919", ]
dt[loc.correct %in% "brazil_espirito santo_vitoria" & geo.check %in% "ok_county_gazet", longitude.work1 := "-40.308919", ]

#correcting one specific problem found
dt[dt$dup.ID %in% "rb_404692|uec_100308",] #problem in the stateProvince
dt[dt$numTombo %in% "rb_404692", c("loc.correct","latitude.work","longitude.work","latitude.work1","longitude.work1","loc.correct1")] = 
  dt[dt$numTombo %in% "uec_100308", c("loc.correct","latitude.work","longitude.work","latitude.work1","longitude.work1","loc.correct1")]


### Case 1 - ok_county: coordinates are validated at county level and resolution is good (i.e. seconds, seconds?):
dt[dup.merge1 %in% c("ok","probable"), geo.check1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?"))) "ok_county" else geo.check1, by = dup.ID]
dt[dup.merge1 %in% c("ok","probable"), origin.coord1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?"))) "coord_original" else origin.coord1, by = dup.ID]
dt[dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?"))) "seconds" else resolution.coord1, by = dup.ID]
dt[dup.merge1 %in% c("ok","probable"), af.check1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?")))  
  paste(unique(af.check[geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?")]),collapse="|") else af.check1, by = dup.ID]
dt[dup.merge1 %in% c("ok","probable"), loc.correct1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?"))) 
  paste(unique(loc.correct[geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?")]),collapse="|") else loc.correct1, by = dup.ID]
dt[dup.merge1 %in% c("ok","probable"), latitude.work1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?"))) 
  as.character(mean(as.double(unique(latitude.work[geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?")])))) else latitude.work1, by = dup.ID]
dt[dup.merge1 %in% c("ok","probable"), longitude.work1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?"))) 
  as.character(mean(as.double(unique(longitude.work[geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?")])))) else longitude.work1, by = dup.ID]
dt[dup.merge1 %in% c("ok","probable"), geo.dup := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("seconds","seconds?"))) "yes" else geo.dup, by = dup.ID]
#checking
#dt["alcb_16444|huefs_1938|mbm_98225",c("dup.ID","geo.check","origin.coord","resolution.coord","af.check","loc.correct","latitude.work","longitude.work","geo.check1","origin.coord1","resolution.coord1","af.check1","loc.correct1","latitude.work1","longitude.work1"),] #should not change geo.check or any other variables
#dt["bm_581963|g_389978|mo_2227492|ny_63067|ny_63068|rb_26658|us_1662181",c("dup.ID","geo.check","origin.coord","resolution.coord","af.check","loc.correct","latitude.work","longitude.work","geo.check1","origin.coord1","resolution.coord1","af.check1","loc.correct1","latitude.work1","longitude.work1")]

### Case 2 - ok_county: coordinates are validated at county level and resolution is less good (i.e. minutes)
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.check1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only"))) "ok_county" else geo.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), origin.coord1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only"))) "coord_original" else origin.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only"))) "minutes_only" else resolution.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), af.check1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only")))  
  paste(unique(af.check[geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only")]),collapse="|") else af.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), loc.correct1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only"))) 
  paste(unique(loc.correct[geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only")]),collapse="|") else loc.correct1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), latitude.work1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only"))) 
  as.character(mean(as.double(unique(latitude.work[geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only")])))) else latitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), longitude.work1 := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only"))) 
  as.character(mean(as.double(unique(longitude.work[geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only")])))) else longitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.dup := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("minutes_only"))) "yes" else geo.dup, by = dup.ID]

### Case 3 - ok_county_close and ok_county_very_close: coordinates are closely validated and resolution is good (i.e. seconds, seconds?)
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), origin.coord1 := if(any(geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?"))) "coord_original" else origin.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(any(geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?"))) "seconds" else resolution.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), af.check1 := if(any(geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?")))  
  paste(unique(af.check[geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?")]),collapse="|") else af.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), loc.correct1 := if(any(geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?"))) 
  paste(unique(loc.correct[geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?")]),collapse="|") else loc.correct1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), latitude.work1 := if(any(geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?"))) 
  as.character(mean(as.double(unique(latitude.work[geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?")])))) else latitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), longitude.work1 := if(any(geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?"))) 
  as.character(mean(as.double(unique(longitude.work[geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?")])))) else longitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.check1 := if(any(geo.check1 %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?"))) "ok_county_close" else geo.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.dup := if(any(geo.check %in% c("ok_county_close","ok_county_very_close") & resolution.coord %in% c("seconds","seconds?"))) "yes" else geo.dup, by = dup.ID]
#checking
#dt["alcb_106445|huefs_192544|rb_587513",c("dup.ID","geo.check","origin.coord","resolution.coord","af.check","loc.correct","latitude.work","longitude.work","geo.check1","origin.coord1","resolution.coord1","af.check1","loc.correct1","latitude.work1","longitude.work1")]
#dt["alcb_28431|asu_55774|cepec_46317|icn_110125|k_343559|rb_420260|spf_79072|ub_132302",c("dup.ID","geo.check","origin.coord","resolution.coord","af.check","loc.correct","latitude.work","longitude.work","geo.check1","origin.coord1","resolution.coord1","af.check1","loc.correct1","latitude.work1","longitude.work1"),]

### Case 4 - ok_locality_gazet: locality description matches the gazetter at locality level
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), origin.coord1 := if(any(geo.check1 %in% c("ok_locality_gazet"))) "ok_locality_gazet" else origin.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(any(geo.check1 %in% c("ok_locality_gazet"))) "seconds_point" else resolution.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), af.check1 := if(any(geo.check1 %in% c("ok_locality_gazet")))  
  paste(unique(af.check[geo.check1 %in% c("ok_locality_gazet")]),collapse="|") else af.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), loc.correct1 := if(any(geo.check1 %in% c("ok_locality_gazet"))) 
  paste(unique(loc.correct[geo.check1 %in% c("ok_locality_gazet")]),collapse="|") else loc.correct1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), latitude.work1 := if(any(geo.check1 %in% c("ok_locality_gazet"))) 
  as.character(mean(as.double(unique(latitude.work[geo.check1 %in% c("ok_locality_gazet")])))) else latitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), longitude.work1 := if(any(geo.check1 %in% c("ok_locality_gazet"))) 
  as.character(mean(as.double(unique(longitude.work[geo.check1 %in% c("ok_locality_gazet")])))) else longitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.check1 := if(any(geo.check1 %in% c("ok_locality_gazet"))) "ok_locality_gazet" else geo.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.dup := if(any(geo.check %in% c("ok_locality_gazet"))) "yes" else geo.dup, by = dup.ID]

### Case 5 - ok_county_gazet: locality description matches the gazetter at county level
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), origin.coord1 := if(any(geo.check1 %in% c("ok_county_gazet"))) "ok_county_gazet" else origin.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(any(geo.check1 %in% c("ok_county_gazet"))) "seconds_centroid" else resolution.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), af.check1 := if(any(geo.check1 %in% c("ok_county_gazet")))  
  paste(unique(af.check[geo.check1 %in% c("ok_county_gazet")]),collapse="|") else af.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), loc.correct1 := if(any(geo.check1 %in% c("ok_county_gazet"))) 
  paste(unique(loc.correct[geo.check1 %in% c("ok_county_gazet")]),collapse="|") else loc.correct1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), latitude.work1 := if(any(geo.check1 %in% c("ok_county_gazet"))) 
  as.character(mean(as.double(unique(latitude.work[geo.check1 %in% c("ok_county_gazet")])))) else latitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), longitude.work1 := if(any(geo.check %in% c("ok_county_gazet"))) 
  as.character(mean(as.double(unique(longitude.work[geo.check1 %in% c("ok_county_gazet")])))) else longitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.check1 := if(any(geo.check1 %in% c("ok_county_gazet"))) "ok_county_gazet" else geo.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.dup := if(any(geo.check %in% c("ok_county_gazet"))) "yes" else geo.dup, by = dup.ID]

### Case 6 - ok_county_coord_corrected or ok_county_problem_coord_corrected: problematic coordinate replaced by the match from the gazetteer at county level
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), origin.coord1 := if(any(geo.check1 %like% "_coord_corrected")) "correction_coord_original" else origin.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(any(geo.check1 %like% "_coord_corrected")) "seconds_centroid" else resolution.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), af.check1 := if(any(geo.check1 %like% "_coord_corrected"))  
  paste(unique(af.check[geo.check1 %like% "_coord_corrected"]),collapse="|") else af.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), loc.correct1 := if(any(geo.check1 %like% "_coord_corrected")) 
  paste(unique(loc.correct[geo.check1 %like% "_coord_corrected"]),collapse="|") else loc.correct1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), latitude.work1 := if(any(geo.check1 %like% "_coord_corrected")) 
  as.character(mean(as.double(unique(latitude.work[geo.check1 %like% "_coord_corrected"])))) else latitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), longitude.work1 := if(any(geo.check1 %like% "_coord_corrected")) 
  as.character(mean(as.double(unique(longitude.work[geo.check1 %like% "_coord_corrected"])))) else longitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.check1 := if(any(geo.check1 %like% "_coord_corrected")) "ok_county_coord_corrected" else geo.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.dup := if(any(geo.check %like% "_coord_corrected")) "yes" else geo.dup, by = dup.ID]

### Case 7 - ok_county: coordinates are validated at county level, but the resolution is poor (i.e. degrees only)
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), origin.coord1 := if(any(geo.check1 %in% "ok_county" & resolution.coord1 %in% c("degrees_only"))) "coord_original" else origin.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(any(geo.check1 %in% "ok_county" & resolution.coord1 %in% c("degrees_only"))) "degrees_only" else resolution.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), af.check1 := if(any(geo.check1 %in% "ok_county" & resolution.coord1 %in% c("degrees_only")))  
  paste(unique(af.check[geo.check %in% "ok_county" & resolution.coord %in% c("degrees_only")]),collapse="|") else af.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), loc.correct1 := if(any(geo.check1 %in% "ok_county" & resolution.coord1 %in% c("degrees_only"))) 
  paste(unique(loc.correct[geo.check %in% "ok_county" & resolution.coord %in% c("degrees_only")]),collapse="|") else loc.correct1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), latitude.work1 := if(any(geo.check1 %in% "ok_county" & resolution.coord1 %in% c("degrees_only"))) 
  as.character(mean(as.double(unique(latitude.work[geo.check %in% "ok_county" & resolution.coord %in% c("degrees_only")])))) else latitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), longitude.work1 := if(any(geo.check1 %in% "ok_county" & resolution.coord1 %in% c("degrees_only"))) 
  as.character(mean(as.double(unique(longitude.work[geo.check %in% "ok_county" & resolution.coord %in% c("degrees_only")])))) else longitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.check1 := if(any(geo.check1 %in% "ok_county" & resolution.coord1 %in% c("degrees_only"))) "ok_county" else geo.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.dup := if(any(geo.check %in% "ok_county" & resolution.coord %in% c("degrees_only"))) "yes" else geo.dup, by = dup.ID]

### Case 8 - ok_state: coordinates are validated at state level (any resolution)
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), origin.coord1 := if(all(!geo.check %like% "ok_county") & any(geo.check %in% "ok_state")) "coord_original" else origin.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(all(!geo.check %like% "ok_county") & any(geo.check %in% "ok_state"))  
  paste(unique(resolution.coord[geo.check %in% "ok_state"]),collapse="|") else resolution.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), af.check1 := if(all(!geo.check %like% "ok_county") & any(geo.check %in% "ok_state"))  
  paste(unique(af.check[geo.check %in% "ok_state"]),collapse="|") else af.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), loc.correct1 := if(all(!geo.check %like% "ok_county") & any(geo.check %in% "ok_state")) 
  paste(unique(loc.correct[geo.check %in% "ok_state"]),collapse="|") else loc.correct1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), latitude.work1 := if(all(!geo.check %like% "ok_county") & any(geo.check %in% "ok_state")) 
  as.character(mean(as.double(unique(latitude.work[geo.check %in% "ok_state"])))) else latitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), longitude.work1 := if(all(!geo.check %like% "ok_county") & any(geo.check %in% "ok_state")) 
  as.character(mean(as.double(unique(longitude.work[geo.check %in% "ok_state"])))) else longitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.check1 := if(all(!geo.check %like% "ok_county") & any(geo.check %in% "ok_state")) "ok_state" else geo.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.dup := if(all(!geo.check %like% "ok_county") & any(geo.check %in% "ok_state")) "yes" else geo.dup, by = dup.ID]

### Case 9 - ok_state_gazet: locality description matches the gazetter at state level
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), origin.coord1 := if(all(!geo.check1 %like% "ok_county") & all(geo.check1 != "ok_state") & any(geo.check1 %in% c("ok_state_gazet"))) "ok_state_gazet" else origin.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(all(!geo.check1 %like% "ok_county") & all(geo.check1 != "ok_state") & any(geo.check1 %in% c("ok_state_gazet"))) "seconds_centroid" else resolution.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), af.check1 := if(all(!geo.check1 %like% "ok_county") & all(geo.check1 != "ok_state") & any(geo.check1 %in% c("ok_state_gazet")))  
  paste(unique(af.check[geo.check1 %in% c("ok_state_gazet")]),collapse="|") else af.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), loc.correct1 := if(all(!geo.check1 %like% "ok_county") & all(geo.check1 != "ok_state") & any(geo.check1 %in% c("ok_state_gazet"))) 
  paste(unique(loc.correct[geo.check1 %in% c("ok_state_gazet")]),collapse="|") else loc.correct1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), latitude.work1 := if(all(!geo.check1 %like% "ok_county") & all(geo.check1 != "ok_state") & any(geo.check1 %in% c("ok_state_gazet"))) 
  as.character(mean(as.double(unique(latitude.work[geo.check1 %in% c("ok_state_gazet")])))) else latitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), longitude.work1 := if(all(!geo.check1 %like% "ok_county") & all(geo.check1 != "ok_state") & any(geo.check %in% c("ok_state_gazet"))) 
  as.character(mean(as.double(unique(longitude.work[geo.check1 %in% c("ok_state_gazet")])))) else longitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.check1 := if(all(!geo.check1 %like% "ok_county") & all(geo.check1 != "ok_state") & any(geo.check1 %in% c("ok_state_gazet"))) "ok_state_gazet" else geo.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.dup := if(all(!geo.check %like% "ok_county") & all(geo.check != "ok_state") & any(geo.check %in% c("ok_state_gazet"))) "yes" else geo.dup, by = dup.ID]

#### CHECK HERE: NEED TO PERFORM THE EXPANSION OF THE INFO AT COUNTRY LEVEL?
### Case 10 - ok_country: coordinates are validated at county level  (any resolution or only seconds??)
## think about gbif cases where the majority of records are only validated at country level. Aren`t we losing good coordinates? Should we stop at state-level?
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), origin.coord1 := if(all(!geo.check1 %like% "ok_county|ok_state") & any(geo.check %in% "ok_country")) "coord_original" else origin.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(all(!geo.check1 %like% "ok_county|ok_state") & any(geo.check %in% "ok_country"))  
  paste(unique(resolution.coord[geo.check %in% "ok_country"]),collapse="|") else resolution.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), af.check1 := if(all(!geo.check1 %like% "ok_county|ok_state") & any(geo.check %in% "ok_country"))  
  paste(unique(af.check[geo.check %in% "ok_country"]),collapse="|") else af.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), loc.correct1 := if(all(!geo.check1 %like% "ok_county|ok_state") & any(geo.check %in% "ok_country")) 
  paste(unique(loc.correct[geo.check %in% "ok_country"]),collapse="|") else loc.correct1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), latitude.work1 := if(all(!geo.check1 %like% "ok_county|ok_state") & any(geo.check %in% "ok_country")) 
  as.character(mean(as.double(unique(latitude.work[geo.check %in% "ok_country"])))) else latitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), longitude.work1 := if(all(!geo.check1 %like% "ok_county|ok_state") & any(geo.check %in% "ok_country")) 
  as.character(mean(as.double(unique(longitude.work[geo.check %in% "ok_country"])))) else longitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.check1 := if(all(!geo.check1 %like% "ok_county|ok_state") & any(geo.check %in% "ok_country")) "ok_country" else geo.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.dup := if(all(!geo.check %like% "ok_county|ok_state") & any(geo.check %in% "ok_country")) "yes" else geo.dup, by = dup.ID]

## REMOVE THIS STEP? NO SPECIMENS ok_country_gazet LEFT? OR PROBLEM IN THE CODE?
### Case 11 - ok_country_gazet: locality description matches the gazetter at country level
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), origin.coord1 := if(all(!geo.check1 %like% "ok_county|ok_state") & all(geo.check1 != "ok_country") & any(geo.check1 %in% "ok_country_gazet")) "ok_country_gazet" else origin.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), resolution.coord1 := if(all(!geo.check1 %like% "ok_county|ok_state") & all(geo.check1 != "ok_country") & any(geo.check1 %in% "ok_country_gazet")) "seconds_centroid" else resolution.coord1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), af.check1 := if(all(!geo.check1 %like% "ok_county|ok_state") & all(geo.check1 != "ok_country") & any(geo.check %in% "ok_country_gazet"))  
  paste(unique(af.check[geo.check %in% "ok_country_gazet"]),collapse="|") else af.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), loc.correct1 := if(all(!geo.check1 %like% "ok_county|ok_state") & all(geo.check1 != "ok_country") & any(geo.check1 %in% "ok_country_gazet")) 
  paste(unique(loc.correct[geo.check %in% "ok_country_gazet"]),collapse="|") else loc.correct1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), latitude.work1 := if(all(!geo.check1 %like% "ok_county|ok_state") & all(geo.check1 != "ok_country") & any(geo.check1 %in% "ok_country_gazet")) 
  as.character(mean(as.double(unique(latitude.work[geo.check %in% "ok_country_gazet"])))) else latitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), longitude.work1 := if(all(!geo.check1 %like% "ok_county|ok_state") & all(geo.check1 != "ok_country") & any(geo.check1 %in% "ok_country_gazet")) 
  as.character(mean(as.double(unique(longitude.work[geo.check %in% "ok_country_gazet"])))) else longitude.work1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.check1 := if(all(!geo.check1 %like% "ok_county|ok_state") & all(geo.check1 != "ok_country") & any(geo.check1 %in% "ok_country_gazet")) "ok_country_gazet" else geo.check1, by = dup.ID]
dt[is.na(geo.dup) & dup.merge1 %in% c("ok","probable"), geo.dup := if(all(!geo.check %like% "ok_county|ok_state") & all(geo.check != "ok_country") & any(geo.check %in% "ok_country_gazet")) "yes" else geo.dup, by = dup.ID]

##############################################################################################################################################################-
#### EDITING THE EXPANDED INFORMATION ON THE GEOGRAPHICLA VALIDATION ####
##VER O QUE FAZER NESSES CASOS (AF.CHECK ? TRUE AND FALSE AO MESMO TEMPO) - N?o usar!!

#### CHECAR AQUI: ####
dt['huto_3916|iac_53454|ibge_67532']

# Pq um af.check1 = 'FALSE|TRUE_in_transition' virou 'TRUE': 'huto_3916|iac_53454|ibge_67532'
#Ver plot do Geographically validated occurrences in the AF "af.geo.tax.occs.jpeg"

sort(table(dt$af.check2))
dt[, af.check2 := af.check1, ]
dt[, af.check2 := gsub('\\|cannot_check$|^cannot_check\\|', '', af.check2), by = af.check2]
dt[, af.check2 := gsub('\\|cannot_check\\|', '|', af.check2), by = af.check2]
dt[, af.check2 := gsub('\\|CHECK_state_name_or_coordinate_mistake\\?$|^CHECK_state_name_or_coordinate_mistake\\?\\|', '', af.check2), by = af.check2]
dt[, af.check2 := gsub('\\|CHECK_state_name_or_coordinate_mistake\\?\\|', '', af.check2), by = af.check2]
dt[, af.check2 := gsub('CHECK_state_name_or_coordinate_mistake\\?', 'cannot_check', af.check2), by = af.check2]
dt[, af.check2 := gsub('^FALSE_in_transition\\|TRUE\\|FALSE', 'TRUE|FALSE', af.check2), by = af.check2]
dt[, af.check2 := gsub('^TRUE\\|TRUE_in_transition$|^TRUE_in_transition\\|TRUE$', 'TRUE_in_transition', af.check2), by = af.check2]
dt[, af.check2 := gsub('^FALSE\\|FALSE_in_transition$|^FALSE_in_transition\\|FALSE$', 'FALSE_in_transition', af.check2), by = af.check2]
dt[, af.check2 := gsub('^TRUE_in_transition\\|FALSE$|^FALSE\\|TRUE_in_transition$', 'TRUE|FALSE', af.check2), by = af.check2]
dt[, af.check2 := gsub('^FALSE_in_transition\\|TRUE_in_transition$|^TRUE_in_transition\\|FALSE_in_transition$', 'TRUE|FALSE', af.check2), by = af.check2]
dt[, af.check2 := gsub('^TRUE\\|FALSE_in_transition$|^FALSE_in_transition\\|TRUE$', 'TRUE|FALSE', af.check2), by = af.check2]
dt[, af.check2 := gsub('^FALSE\\|TRUE$', 'TRUE|FALSE', af.check2), by = af.check2]
##VER O QUE FAZER NESSES CASOS (AF.CHECK ? TRUE AND FALSE AO MESMO TEMPO) - Nao usar!!

##############################################################################################################################################################-
#### SAVING THE EXPANDED INFORMATION FOR THE DUPLICATES WITH THE UNICATE DATA  ####

## Creating the missing new columns in the unicate subset of the data
dados2 = dados[is.na(dados$dup.ID),]  # data with no indication of duplicates
dados2$family.correct1 = dados2$family.correct 
dados2$species.correct1 = dados2$species.correct 
dados2$determinador.name1 = dados2$determinador.name 
dados2$ano.det1 = dados2$ano.det 
dados2$tax.check1 = dados2$tax.check
dados2$latitude.work1 = dados2$latitude.work
dados2$longitude.work1 = dados2$longitude.work
dados2$origin.coord1 = dados2$origin.coord
dados2$resolution.coord1 = dados2$resolution.coord
dados2$geo.check1 = dados2$geo.check
dados2$af.check1 = dados2$af.check
dados2$loc.correct1 = dados2$loc.correct1
dados2$af.check2 = dados2$af.check
colunas = names(dt)[names(dt) %in% names(dados2)] 
dt1 = data.table(dt[,..colunas]) 
table(names(dados2) == names(dt1))

## Merging unicate and merged-duplicate occurrences
rm(dados1, dt);gc()
dt2 = data.table(dados2) #transforming the unicate data into a data.table format
rm(dados2); gc()
dt3 = rbindlist(list(dt2, dt1)) #merging the unicate and duplicated tables

## Removing some unwanted columns
#dt3[,X:=NULL] #removing first column
#loc and loc1?
#specimen1, specimen2
dt3[,c("specimen1","specimen2","specimen3","specimen4"):=NULL]
dt3[,c("dup.check","dup.check1","dup.check2","dup.check3","dup.check4"):=NULL]


########################################################################################################################################################################################-
########################################################################################################################################################################################-
###########################################################
#### FINAL CORRECTIONS AFTER MERGING AND BEFORE SAVING ####
###########################################################

#### SOLVING PROBLEMS OF DETERMINATION ####
### missing dates of determination, older but better determination, probable det. mistakes, labelling errors, etc ####
##Last update: 05/01/2020

# Myrcianthes myrsinoides -> Eugenia prasina
dt3[numTombo %in% c("cepec_40849","icn_64105","mbm_89481","mo_3199409","us_3088976"), c("species.correct1") := "Eugenia prasina"]
#Aspidosperma melanocalyx
dt3[scientificName %like% "Aspidosperma leucomelanum|Aspidosperma limae|Aspidosperma melanocalyx", species.correct := "Aspidosperma melanocalyx"]
dt3[scientificName %like% "Aspidosperma leucomelanum|Aspidosperma limae|Aspidosperma melanocalyx", species.correct1 := "Aspidosperma melanocalyx"]
dt3[numTombo %in% c("mo_3274485"), species.correct1 := "Aspidosperma melanocalyx",]
#Machaerium isadelphum
dt3[numTombo %in% c("k_909457"), species.correct1 := "Machaerium aculeatum",]
#Faramea glandulosa
dt3[numTombo %in% c("ny_397381"), species.correct1 := "Faramea tenuiflora",]
#Coussapoa latifolia/obovata
dt3[numTombo %in% c("us_2596785"), species.correct1 := "Coussapoa curranii",]
dt3[numTombo %in% c("us_2596785"), geo.check1 := "ok_county_gazet",]
dt3[numTombo %in% c("us_2596785"), origin.coord1 := "ok_county_gazet",]
dt3[numTombo %in% c("us_2596785"), latitude.work1 := "-22.543520",]
dt3[numTombo %in% c("us_2596785"), longitude.work1 := "-43.172056",]
#Couratari stellata
dt3[coletor.name == "Duarte, A.P." & coletor.number == 9760 & ano == 1966, species.correct1 := "Couratari macrosperma",]
dt3[coletor.name == "Duarte, A.P." & coletor.number == 9760 & ano == 1966, geo.check1 := "ok_county_gazet",]
#Ormosia stipularis
dt3[numTombo %in% c("k_892611"), species.correct1 := "Ormosia fastigiata",]
#Systemonodaphne geminiflora
dt3[numTombo %in% c("g_131518"), species.correct1 := "Phyllostemonodaphne geminiflora",]
dt3[numTombo %in% c("ny_355141"), species.correct1 := "Phyllostemonodaphne geminiflora",]
#Geonoma undata
dt3[dup.ID %like% "mo_3391361|rb_631409", species.correct1 := "Geonoma pohliana"]
#Xylopia longicuspis
dt3[numTombo %in% c("rb_323465"), species.correct1 := "Xylopia langsdorffiana",]
#Mezilaurus itauba
dt3[dup.ID %like% "ny_355612" | numTombo == "b_100185315", species.correct1 := "Ocotea hookeriana",]
#Ocotea sprucei
dt3[numTombo %in% c("ny_162372","ny_162371"), species.correct1 := "Ocotea argentea",]
#Rinoria paniculata
dt3[numTombo %in% c("rb_450394","rb_450384","cepec_14736"), species.correct1 := "Rinorea guianensis",]
#Mimosa verrucosa
dt3[numTombo %in% c("ipa_13572","ny_933286","spf_184977"), species.correct1 := "Mimosa schomburgkii",]

#### SOLVING SPECIFIC PROBLEMS AFTER DUPLICATE MERGE (leading to new species records for the Atlantic Forest list) ####
# Problems after duplicated merge
dt3[dup.ID %like% "inpa_114144|ny_2158610", af.check1 := "FALSE",]
dt3[dup.ID %like% "inpa_114144|ny_2158610", af.check2 := "FALSE",]
dt3[dup.ID %like% "inpa_114144|ny_2158610", latitude.work1 := latitude.work,]
dt3[dup.ID %like% "inpa_114144|ny_2158610", longitude.work1 := longitude.work,]
dt3[dup.ID %like% "herbmg_mg039547|herbmg_mg113542|inpa_25458|mo_2879197|ny_1408502|us_3247739", af.check1 := "FALSE",]
dt3[dup.ID %like% "herbmg_mg039547|herbmg_mg113542|inpa_25458|mo_2879197|ny_1408502|us_3247739", af.check2 := "FALSE",]
dt3[dup.ID %like% "cepec_9036|rb_246949", af.check1 := "TRUE_in_transition",]
dt3[dup.ID %like% "cepec_9036|rb_246949", af.check2 := "TRUE_in_transition",]
dt3[dup.ID %like% "esa_106266|ub_208207|upcb_64164", af.check1 := "FALSE",]
dt3[dup.ID %like% "esa_106266|ub_208207|upcb_64164", af.check2 := "FALSE",]
dt3[dup.ID %like% "esa_106266|ub_208207|upcb_64164", latitude.work1 := latitude.work,]
dt3[dup.ID %like% "esa_106266|ub_208207|upcb_64164", longitude.work1 := longitude.work,]

#Other problems
# e.g. records in other non-AF states marked as Rio de Janeiro, Parana, Esp. Santo or São Paulo
dt3[numTombo %in% c("mo_2817588","mo_2818518"), loc.correct := "brazil",]
dt3[numTombo %in% c("mo_2817588","mo_2818518"), origin.coord := "ok_country_gazet",]
dt3[numTombo %in% c("mo_2817588","mo_2818518"), origin.coord1 := "ok_country_gazet",]
dt3[numTombo %in% c("mo_2817588","mo_2818518"), geo.check := "ok_country_gazet",]
dt3[numTombo %in% c("mo_2817588","mo_2818518"), geo.check1 := "ok_country_gazet",]
dt3[numTombo %in% c("mo_2817588","mo_2818518"), af.check := "FALSE",]
dt3[numTombo %in% c("mo_2817588","mo_2818518"), af.check1 := "FALSE",]
dt3[numTombo %in% c("mo_2817588","mo_2818518"), af.check2 := "FALSE",]
dt3[dup.ID %like% "ny_2332207|s_5-4195", af.check := "FALSE",]
dt3[dup.ID %like% "ny_2332207|s_5-4195", af.check1 := "FALSE",]
dt3[dup.ID %like% "ny_2332207|s_5-4195", af.check2 := "FALSE",]
dt3[dup.ID %like% "ny_2332207|s_5-4195", latitude.work1 := "-3.974215002",]
dt3[dup.ID %like% "ny_2332207|s_5-4195", longitude.work1 := "-53.06377009",]
dt3[dup.ID %like% "k_892440|ny_1461941|sp_57742", loc.correct := "brazil_para",]
dt3[dup.ID %like% "k_892440|ny_1461941|sp_57742", af.check := "FALSE",]
dt3[dup.ID %like% "k_892440|ny_1461941|sp_57742", af.check1 := "FALSE",]
dt3[dup.ID %like% "k_892440|ny_1461941|sp_57742", af.check2 := "FALSE",]
dt3[dup.ID %like% "k_892440|ny_1461941|sp_57742", latitude.work1 := "-3.974215002",]
dt3[dup.ID %like% "k_892440|ny_1461941|sp_57742", longitude.work1 := "-53.06377009",]
dt3[dup.ID %like% "sp_77775|us_2438683", loc.correct := "brazil_para",]
dt3[dup.ID %like% "sp_77775|us_2438683", af.check := "FALSE",]
dt3[dup.ID %like% "sp_77775|us_2438683", af.check1 := "FALSE",]
dt3[dup.ID %like% "sp_77775|us_2438683", af.check2 := "FALSE",]
dt3[dup.ID %like% "sp_77775|us_2438683", latitude.work1 := "-3.974215002",]
dt3[dup.ID %like% "sp_77775|us_2438683", longitude.work1 := "-53.06377009",]
dt3[dup.ID %like% "mo_1001187", loc.correct := "brazil_minas gerais",]
dt3[dup.ID %like% "mo_1001187", af.check := "FALSE",]
dt3[dup.ID %like% "mo_1001187", af.check1 := "FALSE",]
dt3[dup.ID %like% "mo_1001187", af.check2 := "FALSE",]
dt3[dup.ID %like% "mo_1001187", latitude.work1 := "-19.166988",]
dt3[dup.ID %like% "mo_1001187", longitude.work1 := "-46.617255",]
dt3[numTombo %in% "mo_3096697", loc.correct := "brazil_sao paulo_sao carlos"]
dt3[numTombo %in% "mo_3096697", latitude.work1 := "-21.91857385",]
dt3[numTombo %in% "mo_3096697", longitude.work1 := "-47.86709407",]
dt3[numTombo %in% "k_426841", loc.correct := "brazil_minas gerais_diamantina"]
dt3[numTombo %in% "k_426841", af.check := "TRUE_in_transition",]
dt3[numTombo %in% "k_426841", af.check1 := "TRUE_in_transition",]
dt3[numTombo %in% "k_426841", af.check2 := "TRUE_in_transition",]
dt3[numTombo %in% "k_426841", latitude.work1 := "-17.99283125",]
dt3[numTombo %in% "k_426841", longitude.work1 := "-43.61023331",]
dt3[numTombo %in% "rb_280201", loc.correct := "brazil_rondonia"]
dt3[numTombo %in% "rb_280201", af.check := "FALSE",]
dt3[numTombo %in% "rb_280201", af.check1 := "FALSE",]
dt3[numTombo %in% "rb_280201", af.check2 := "FALSE",]
dt3[numTombo %in% "rb_280201", latitude.work1 := "-10.89856477",]
dt3[numTombo %in% "rb_280201", longitude.work1 := "-62.85654951",]
dt3[dup.ID %like% "k_834607|us_1518562", loc.correct := "brazil_para"]
dt3[dup.ID %like% "k_834607|us_1518562", af.check := "FALSE",]
dt3[dup.ID %like% "k_834607|us_1518562", af.check1 := "FALSE",]
dt3[dup.ID %like% "k_834607|us_1518562", af.check2 := "FALSE",]
dt3[dup.ID %like% "k_834607|us_1518562", latitude.work1 := "-1.425669",]
dt3[dup.ID %like% "k_834607|us_1518562", longitude.work1 := "-51.610601",]   
dt3[numTombo %in% "us_1615724", loc.correct := "brazil_amazonas_manaus"]
dt3[numTombo %in% "us_1615724", af.check := "FALSE",]
dt3[numTombo %in% "us_1615724", af.check1 := "FALSE",]
dt3[numTombo %in% "us_1615724", af.check2 := "FALSE",]
dt3[numTombo %in% "us_1615724", latitude.work1 := "-2.625919383",]
dt3[numTombo %in% "us_1615724", longitude.work1 := "-60.25962801",]
dt3[dup.ID %like% "bm_952047|f_59869f|mo_3217569", loc.correct := "brazil_amazonas"]
dt3[dup.ID %like% "bm_952047|f_59869f|mo_3217569", af.check := "FALSE",]
dt3[dup.ID %like% "bm_952047|f_59869f|mo_3217569", af.check1 := "FALSE",]
dt3[dup.ID %like% "bm_952047|f_59869f|mo_3217569", af.check2 := "FALSE",]
dt3[dup.ID %like% "bm_952047|f_59869f|mo_3217569", latitude.work1 := "-4.182240778",]
dt3[dup.ID %like% "bm_952047|f_59869f|mo_3217569", longitude.work1 := "-64.69850145",]   
dt3[numTombo %in% "mo_2837229", loc.correct := "brazil_amapa"]
dt3[numTombo %in% "mo_2837229", af.check := "FALSE",]
dt3[numTombo %in% "mo_2837229", af.check1 := "FALSE",]
dt3[numTombo %in% "mo_2837229", af.check2 := "FALSE",]
dt3[numTombo %in% "mo_2837229", latitude.work1 := "1.437893618",]
dt3[numTombo %in% "mo_2837229", longitude.work1 := "-51.96677136",]
dt3[numTombo %in% "us_1442374", loc.correct := "brazil_para"]
dt3[numTombo %in% "us_1442374", af.check := "FALSE",]
dt3[numTombo %in% "us_1442374", af.check1 := "FALSE",]
dt3[numTombo %in% "us_1442374", af.check2 := "FALSE",]
dt3[numTombo %in% "us_1442374", latitude.work1 := "-3.974215002",]
dt3[numTombo %in% "us_1442374", longitude.work1 := "-53.06377009",]
dt3[numTombo %in% "mo_699532", loc.correct := "brazil_amazonas_sao gabriel cachoeira"]
dt3[numTombo %in% "mo_699532", af.check := "FALSE",]
dt3[numTombo %in% "mo_699532", af.check1 := "FALSE",]
dt3[numTombo %in% "mo_699532", af.check2 := "FALSE",]
dt3[numTombo %in% "mo_699532", latitude.work1 := "0.721461",]
dt3[numTombo %in% "mo_699532", longitude.work1 := "-66.090034",]
dt3[numTombo %in% "e_643531", loc.correct := "brazil_amazonas"]
dt3[numTombo %in% "e_643531", af.check := "FALSE",]
dt3[numTombo %in% "e_643531", af.check1 := "FALSE",]
dt3[numTombo %in% "e_643531", af.check2 := "FALSE",]
dt3[numTombo %in% "e_643531", latitude.work1 := "-4.182240778",]
dt3[numTombo %in% "e_643531", longitude.work1 := "-64.69850145",]   
dt3[numTombo %in% "us_1615753", loc.correct := "brazil_acre"]
dt3[numTombo %in% "us_1615753", af.check := "FALSE",]
dt3[numTombo %in% "us_1615753", af.check1 := "FALSE",]
dt3[numTombo %in% "us_1615753", af.check2 := "FALSE",]
dt3[numTombo %in% "us_1615753", latitude.work1 := "-9.309916736",]
dt3[numTombo %in% "us_1615753", longitude.work1 := "-70.44666618",]
dt3[numTombo %in% "mo_1559962", loc.correct := "brazil_amazonas"]
dt3[numTombo %in% "mo_1559962", af.check := "FALSE",]
dt3[numTombo %in% "mo_1559962", af.check1 := "FALSE",]
dt3[numTombo %in% "mo_1559962", af.check2 := "FALSE",]
dt3[numTombo %in% "mo_1559962", latitude.work1 := "-4.182240778",]
dt3[numTombo %in% "mo_1559962", longitude.work1 := "-64.69850145",]
dt3[numTombo %in% "mbm_99980", loc.correct := "brazil_amazonas_manaus_cidade nova"]
dt3[numTombo %in% "mbm_99980", af.check := "FALSE",]
dt3[numTombo %in% "mbm_99980", af.check1 := "FALSE",]
dt3[numTombo %in% "mbm_99980", af.check2 := "FALSE",]
dt3[numTombo %in% "mbm_99980", latitude.work1 := "-3.038772",]
dt3[numTombo %in% "mbm_99980", longitude.work1 := "-59.984537",]
dt3[dup.ID %like% "f_59635f|mo_3211034", loc.correct := "brazil"]
dt3[dup.ID %like% "f_59635f|mo_3211034", af.check := "cannot_check",]
dt3[dup.ID %like% "f_59635f|mo_3211034", af.check1 := "cannot_check",]
dt3[dup.ID %like% "f_59635f|mo_3211034", af.check2 := "cannot_check",]
dt3[numTombo %in% c("e_643534","e_643535","e_643518","e_643532"), loc.correct := "brazil_amazonas"]
dt3[numTombo %in% c("e_643534","e_643535","e_643518","e_643532"), af.check := "FALSE",]
dt3[numTombo %in% c("e_643534","e_643535","e_643518","e_643532"), af.check1 := "FALSE",]
dt3[numTombo %in% c("e_643534","e_643535","e_643518","e_643532"), af.check2 := "FALSE",]
dt3[numTombo %in% c("e_643534","e_643535","e_643518","e_643532"), latitude.work1 := "-4.182240778",]
dt3[numTombo %in% c("e_643534","e_643535","e_643518","e_643532"), longitude.work1 := "-64.69850145",]
dt3[dup.ID %like% "g_357086|g_357087", loc.correct := "venezuela"]
dt3[dup.ID %like% "g_357086|g_357087", af.check := "FALSE",]
dt3[dup.ID %like% "g_357086|g_357087", af.check1 := "FALSE",]
dt3[dup.ID %like% "g_357086|g_357087", af.check2 := "FALSE",]
dt3[dup.ID %like% "g_357086|g_357087", latitude.work1 := "1.936026",]
dt3[dup.ID %like% "g_357086|g_357087", longitude.work1 := "-67.004104",]
dt3[numTombo %in% c("p_307481","p_307482"), loc.correct := "brazil"]
dt3[numTombo %in% c("p_307481","p_307482"), af.check := "cannot_check",]
dt3[numTombo %in% c("p_307481","p_307482"), af.check1 := "cannot_check",]
dt3[numTombo %in% c("p_307481","p_307482"), af.check2 := "cannot_check",]
dt3[numTombo %in% c("rb_284401","rb_388711","rb_284414","rb_388713"), loc.correct := "brazil_distrito federal_brasilia"]
dt3[numTombo %in% c("rb_284401","rb_388711","rb_284414","rb_388713"), municipio := "brasilia"]
dt3[numTombo %in% c("rb_284401","rb_388711","rb_284414","rb_388713"), af.check := "FALSE",]
dt3[numTombo %in% c("rb_284401","rb_388711","rb_284414","rb_388713"), af.check1 := "FALSE",]
dt3[numTombo %in% c("rb_284401","rb_388711","rb_284414","rb_388713"), af.check2 := "FALSE",]
dt3[numTombo %in% c("rb_284401","rb_388711","rb_284414","rb_388713"), latitude.work1 := "-15.78111952",]
dt3[numTombo %in% c("rb_284401","rb_388711","rb_284414","rb_388713"), longitude.work1 := "-47.79722597",]

########################################################################################################################################################################################-
########################################################################################################################################################################################-
############################################################
#### SAVING THE MERGED OCCURRENCE DATA FAMILY BY FAMILY ####
############################################################

#### Saving data per family ####
###List of families
familias = sort(unique(dt3$family.correct))

#Path to the folder in the local directory to receive the files
path="C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//occurrence_data"

#Trying to make processing faster
dt3[, family.correct1 := as.factor(family.correct1)] # converting family to factors
setkeyv(dt3, "family.correct1") # setting 'family.correct1' as key to the data.table (makes computations faster)

#Removing unnecessary columns
#specimen1	specimen2	specimen3	specimen4	dup.check	dup.numb	dup.prop	dup.ID	dup.merge	dup.check1	dup.check2	dup.check3	dup.check4

#Saving the data family by family
for (i in 1:length(familias)){
  familia = familias[i]
  dado = dt3[familia]
  nome = paste(path,paste(familia,"_merged.csv",sep=""),sep="//")
  fwrite(x=dado, file= nome)
  #write.csv(x=dado, file= nome)
} 

##Saving files to Hans and Silvya
# dado = data.table(dt3["Annonaceae"])
# dado = data.table(dado[!country %in% c("Brasil","Brazil","BR","CO","PE","CR","BO","US","EC","MX","PY","PA","VE","NI","BRAZIL","BRASIL")])
# fwrite(x=dado, file= "Annonaceae_merged_to_HTS_new.csv")
# 
# dado = data.table(dt3["Myristicaceae"])
# dado = data.table(dado[species.correct %like% "^Virola" | species.correct1 %like% "^Virola"])
# fwrite(x=dado, file= "Virola_merged_to_SM_new.csv")

#### END ####

########################################################################################################################################################################################
########################################################################################################################################################################################
##############################################################
#### A PRELIMINARY LIST OF SPECIES FOR THE ATLANTIC FOREST ###
##############################################################

###Very conservative removal of duplicates
dados2 = dados1[!duplicated(dados1$specimen3,incomparables = NA),]
dados2 = dados2[!duplicated(dados2$specimen1,incomparables = NA),]
dados2 = dados2[!duplicated(dados2$specimen2,incomparables = NA),]
dados6 = rbind.data.frame(dados,dados2)

###Main type localities without validated coordinates
tmp = dados6[dados6$af.check%in%"cannot_check",]
tmp = tmp[tmp$species.correct %in% unique(spp),]
tmp = tmp[!is.na(tmp$typeStatus),]
tmp2 = data.frame(locality= names(table(tmp$loc.correct)), count= as.vector(table(tmp$loc.correct)))
tmp2 = tmp2[order(tmp2$count,decreasing = TRUE),]
toto = strsplit(as.character(tmp2$locality),"_")
tmp2$country = sapply(toto,function(x) x[1])  
tmp2$state = sapply(toto,function(x) x[2])
tmp2$county = sapply(toto,function(x) x[3]) 
tmp2$loc = sapply(toto,function(x) x[4]) 
tmp3 = tmp2[is.na(tmp2$county)&is.na(tmp2$loc),]
tmp4 = tmp3[!is.na(tmp3$loc),]
#write.csv(tmp3,"tipos.csv")

###Any validated taxonomy problems? 
##Almost all validated occurrences without determinador.name are types, except to: "Not a Type", "?", "not a type", "Not a type", "NOTATYPE"
tmp = dados6[dados6$tax.check %in% TRUE,]
tail(sort(table(tmp$determinador.name)),50)
tmp1 = tmp[tmp$determinador.name %in% "Semdeterminador",]
table(is.na(tmp1$typeStatus))
table(tmp1$typeStatus)
##
tmp1 = tmp[is.na(tmp$specimen1)&is.na(tmp$specimen2)&is.na(tmp$specimen3),]
head(tmp1)
table(tmp1$dup.check)

###List of taxonomically- and geographically-validated names inside the AF
spp = dados6$species.correct[!grepl("^no_",dados6$geo.check) & dados6$af.check%in%TRUE & dados6$tax.check%in%TRUE & grepl("^ok",dados6$status)]
length(unique(spp))# number of species
length(spp)# numero de registros

###All taxonomically-validated occurrences for the species occurring inside the AF
###CHECK IF THERE ARE NO DUPLICATES LEFT IN THE TABLE (check with Dinizia juerana)
dados7 = dados6[!grepl("^no_",dados6$geo.check) & dados6$tax.check%in%TRUE & dados6$species.correct %in% unique(spp),] #occurrences inside the AF
dim(dados7) # total number of records 

###Endemism ratio for the species
##REMOVE NON-TREES AND EXOTICS/NATURALIZED!!
tmp = table(dados7$species.correct,dados7$af.check)
endemismo = cbind.data.frame(spp=row.names(tmp),inside=as.double(tmp[,3]),outside=as.double(tmp[,2]))
endemismo$total = apply(endemismo[,2:3],1,sum,na.rm=TRUE)
endemismo$nivel = round(100*endemismo$inside/endemismo$total,2)
hist(endemismo$nivel,nclass=40); abline(v=c(90,95,99))

###Possible identification problems?
tmp = endemismo[endemismo$total>500&endemismo$inside==3,]
dados7[dados7$species.correct %in% as.character(tmp$spp) & dados7$af.check%in%TRUE,]

###Number of endemics per endemism ratio
100*sum(endemismo$nivel>=50)/dim(endemismo)[1]
100*sum(endemismo$nivel>=55)/dim(endemismo)[1]
100*sum(endemismo$nivel>=60)/dim(endemismo)[1]
100*sum(endemismo$nivel>=65)/dim(endemismo)[1]
100*sum(endemismo$nivel>=70)/dim(endemismo)[1]
100*sum(endemismo$nivel>=75)/dim(endemismo)[1]
100*sum(endemismo$nivel>=80)/dim(endemismo)[1]
100*sum(endemismo$nivel>=85)/dim(endemismo)[1]
100*sum(endemismo$nivel>=90)/dim(endemismo)[1]
100*sum(endemismo$nivel>=95)/dim(endemismo)[1]
100*sum(endemismo$nivel>=99)/dim(endemismo)[1]
100*sum(endemismo$nivel%in%100)/dim(endemismo)[1]

###Crossing the list of names with the preliminary checklist
tmp = dados6[!grepl("^no_",dados6$geo.check) & dados6$af.check%in%TRUE & dados6$tax.check%in%TRUE,]
tmp = tmp[!duplicated(tmp$species.correct),]
tmp = tmp[,c("status","scientificName","family.correct","species.correct")]
names(tmp)[2] = "SpeciesKTSA"
spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=T, na.string=c(NA,""," "))
spp1 = left_join(tmp,spp,by="SpeciesKTSA")
spp1 = spp1[,c("order","TreeCo_status","status","SpeciesKTSA","family.correct","species.correct",
               "establishment.AF","establishment.BR","habito","habito.reflora",
               "id.reflora","scientific.name.reflora","taxon.rank.reflora","taxon.status.reflora","name.status.reflora","notes.reflora", 
               "life.form.reflora","domain.reflora","establishment.reflora",
               "vegtype.reflora","domain.reflora","endemism.Reflora",
               "AtlanticForest","Domain","Endemism",
               "PotentialAtlanticForest","AtlanticForestChecklist_preliminary","treeco"
)]
spp1 = spp1[!spp1$habito%in%0,]
spp1 = spp1[str_count(spp1$species.correct," ")%in%1,]
spp1 = spp1[!spp1$species.correct%in%c("exotic","naturalized","cultivated","cultivated?","not in Brazil"),]
table(spp1$notes.reflora)
table(spp1$habito)
table(spp1$habito.reflora)
table(spp1$life.form.reflora)
sort(table(spp1$domain.reflora))
table(spp1$establishment.BR)

###Lista aceita para o reflora
spp2 = spp1[grepl("AtlanticForest|Mata Atl",spp1$domain.reflora)|grepl("AtlanticForest|Mata Atl",spp1$domain.reflora.1),]
table(spp2$endemism.Reflora,spp2$habito)

#Crossing the list of names with the preliminary checklist
spp = read.csv("D://Documentos//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=T, na.string=c(NA,""," "))
tmp = spp[grepl("^sim",spp$checagem_vinicius),]



spp1= spp[!is.na(spp$species.reflora)&spp$habito%in%c("0","0.5","1"),]
spp1= spp1[!duplicated(spp1$species.reflora),]
spp2= spp[!is.na(spp$species.reflora)&spp$habito%in%c("check","0.5?","1?",NA),]
tmp = spp2[,c("order","establishment.BR","SpeciesKTSA","species.reflora")]
tmp1 = left_join(tmp,spp1[,c("habito","species.reflora")],by="species.reflora")

tmp2 = merge(spp,tmp1,by="order",all.x=TRUE)
write.csv(tmp2,"check3.csv")

trait = read.csv("D://Documentos//Pos Doc//Databases//Species traits//Atributos das esp?cies//traits.species.csv", as.is=T, na.string=c(NA,""," "))
tmp1 = merge(tmp[,c("order","SpeciesKTSA")],trait[,c("TAX","Habit","MaxHeight_m","MaxDbh_cm","GeogDist")],by.x="SpeciesKTSA",by.y="TAX",all.x=T)
tmp2 = merge(tmp[,c("order","species.reflora")],trait[,c("TAX","Habit","MaxHeight_m","MaxDbh_cm","GeogDist")],by.x="species.reflora",by.y="TAX",all.x=T)

tmp1 = merge(spp,tmp1,by="order",all.x=TRUE)
tmp2 = merge(spp,tmp2,by="order",all.x=TRUE)
write.csv(tmp1,"check3.csv")
write.csv(tmp2,"check4.csv")

##ADICIONAR A herb.list O N?MERO DE OCORR?NCIAS POTENCIAIS/FILTRADAS


