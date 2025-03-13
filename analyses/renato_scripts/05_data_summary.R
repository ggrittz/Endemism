#######################################################################
#### THE LIST OF TREES AND ENDEMISM LEVELS FOR THE ATLANTIC FOREST ####
#######################################################################
rm(list=ls())
gc()
memory.limit(size = 50000)

#### LOADING PACKAGES  ####
require(data.table)
require(stringr)
require(splines)
require(bbmle)
require(segmented)
require(raster)
require(rgeos)
require(rgdal)


#### LOADING TREE OCCURRENCE DATA  ####
## Loading the paths to occurrence data 
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//occurrence_data",full.names=TRUE)
paths = paths[grepl('merged_outliers.csv',paths)]

## Loading the different collections
lista = vector("list",length(paths))
for (i in 1:length(paths)) {
  path.csv = paths[i]
  #tmp = read.csv(path.csv,as.is=TRUE,na.strings=c(""," ","NA"))
  tmp = fread(path.csv,na.strings=c(""," ","NA"))
  lista[[i]] = tmp
}
## Merging the different collections
oc.data = rbindlist(lista) #merging the unicate and duplicated tables
rm(lista, tmp)


######################################################################################################################################################################################-
#### REMOVING SPATIAL OUTLIERS  ####

#### GETTING SPECIES INFORMATION: TBC, geographical range and cultivation #### Getting species information and generating the vectors for the groups of species
spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=T, na.string=c(NA,""," "))
tmp = unique(oc.data[,species.correct2,])
spp = spp[spp$SpeciesKTSA %in% tmp,]
uso = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Databases//Species Uses//plant_uses.csv", na= c(""," ",NA))
uso = uso[as.character(uso$Name_submitted) %in% tmp,]

## Preliminary editing of the list
spp$nomes = spp$species.reflora
#replacing names not found in ReFlora by the TNRS/Hans taxonomy
spp$nomes[is.na(spp$nomes)] = spp$Accepted_name[is.na(spp$nomes)]
#replacing names not found in ReFlora and TNRS by the Plant List
spp$nomes[is.na(spp$nomes)] = spp$NewTaxon.TPL[is.na(spp$nomes)]

## Getting the list of species per group
#tbc = spp$nomes[spp$TBC %in% "TBC"]
cult= as.character(uso$Name_submitted[as.character(uso$group_renato) %in% "cultivated"])

#### REMOVING TRUE OUTLIERS ####

#1- remover true outliers para (todas?) as especies: classic Mahalanobis distance >3 & robust (Mahalanobis) distance>16
#table(oc.data$true.out, useNA = "always")
toto = dim(oc.data)[1]
oc.data <- data.table(oc.data[is.na(true.out) | true.out %in% FALSE])
toto1 = dim(oc.data)[1]
toto-toto1; 100*(toto-toto1)/toto # 48980 occs excluded or 0.788%

#### REMOVING PROBABLE OUTLIERS ####
#2- remover probable outliers para cultivated species:  classic Mahalanobis distance >2.5 & robust (Mahalanobis) distance>11.5
tmp <- data.table(oc.data[species.correct2 %in% cult])
#table(tmp$probable.out, useNA = "always")
tmp1 <- data.table(tmp[probable.out %in% TRUE])
oc.data <- oc.data[!numTombo %in% tmp1$numTombo]
toto2 = dim(oc.data)[1]
toto1-toto2; 100*(toto1-toto2)/dim(tmp)[1] # 2908 occs excluded or 0.788%
rm(tmp, tmp1, toto, toto1, toto2)

#3- remover non-core specimens for heavily cultivated native species (e.g. Araucaria angustifolia): rob.out.99 == FALSE

## REMOVE OCCURRENCES OUTSIDE THE NEOTROPICS, EXCEPT FOR PANTROPICAL SPECIES?
## REMOVE OCCURRENCE DATA FROM WELL-KNOW BOTANICAL GARDENS GROWING SOUTH AMERICA SPECIES?

######################################################################################################################################################################################-
#### REMOVING DUPLICATES  ####

## Ordering the dataset to more-easily remove duplicates
#first by dup.ID, then decreasing dup.numb, then decreasing dup.prop, then decreasing source name (preferenc to splink, than jabot, then splink_new, then gbif)
oc.data[, source1 := as.double(as.character(factor(source, levels = c("gbif","sndb","jabot","splink","splink_new"), labels = c(5,4,2,1,3)))), ]
setorder(oc.data,  dup.ID, -dup.numb, -dup.prop, source1)               ## re-ordering the data.table, using setorder()
oc.data[,source1:=NULL] #removing the extra column created for ranking

#creating the unique accession number and multiple-acession number for each specimen
oc.data[, dup.ID1 := dup.ID]
oc.data[is.na(dup.ID1), dup.ID1 := numTombo, by = numTombo]
#removing the duplicates
oc.data1 = unique(oc.data, by = "dup.ID1")
#removing the extra column created for ranking
oc.data[,dup.ID1:=NULL] 
oc.data1[,dup.ID1:=NULL]

## How many non-duplicates occurrences?
dim(oc.data1) # 4,628,942 previously, then: 4,680,232; then: 4,945,029, now: 4,859,649
  
## How many non-duplicates occurrences inside the AF?
oc.data.af = data.table(oc.data1[oc.data1$af.check2 %in% c("TRUE","TRUE_in_transition"),]) # occurrences inside the AF
dim(oc.data.af) # 667,174 previously, then: 685,914, then: 813,916, then: 810,803, now: 808,176 
dim(oc.data.af[af.check2 %in% c("TRUE"),])[1] #then 763,796, then 760,761, now 758,209 truly inside the AF

## How many non-duplicates, taxonomic-validated occurrences inside the AF?
oc.data.af.tax = oc.data.af[oc.data.af$tax.check1 == "TRUE",] # occurrences inside the AF
dim(oc.data.af.tax) # before: 245,225 (225,764 TRUE and 19,461 TRUE_in_transition)
                    # then: 257,395 (238,065 TRUE and 19,330 TRUE_in_transition)
                    # then: 300,895 (279,424 TRUE and 21,471 TRUE_in_transition)
                    # then: 300,275 (278,540 TRUE and 21416  TRUE_in_transition)
                    # now: 299,070 (277,649 TRUE and 21421  TRUE_in_transition)
#### PLOTTING THE RESULTS ####

## Defining the general path to read/save files (tifs and shape files) ###
# path = "E://ownCloud//W_GIS"
# am.lat <- readRDS(paste(path,"//Am_Lat_ADM_GADM_v3.6//gadm36_SouthAm_0_sp.rds",sep=""))
# neotrop <- readRDS(paste(path,"//Am_Lat_ADM_GADM_v3.6//gadm36_Neotrop_0_sp_simplified.rds",sep=""))
# brasil = readRDS(paste(path,"//Am_Lat_ADM_GADM_v3.6//gadm36_BRA_1_sp.rds",sep=""))
# af = readOGR(dsn=paste(path,"//AF_limites_milton",sep=""),layer="merge_limites_MA11428_TNC_ARG_BR_PAR")
# 
# ## All Occurrences ##
# jpeg(filename = "all.occs_new.jpeg", width = 3000*1.5, height = 3000*2, units = "px", pointsize = 12,
#      res = 600, family = "sans", type="cairo", bg="white")
# par(mfrow=c(1,1), las=1, mar=c(0,0,0,0), mgp=c(1.7,0.4,0), tcl=-.3)
# tmp = oc.data[!is.na(latitude.work1) & !is.na(longitude.work1)]
# #tmp = head(tmp, 100000)
# plot(neotrop, border="white")
# points(tmp$longitude.work1, tmp$latitude.work1, pch=21, cex=0.1, col=2)
# plot(af, add=TRUE, border="darkgreen", lwd=1)
# plot(neotrop, add=TRUE)
# dev.off()
# 
# gc()
# ## Non-duplicated occurrences ##
# jpeg(filename = "non.dup.occs_new.jpeg", width = 3000*1.5, height = 3000*2, units = "px", pointsize = 12,
#      res = 600, family = "sans", type="cairo", bg="white")
# par(mfrow=c(1,1), las=1, mar=c(0,0,0,0), mgp=c(1.7,0.4,0), tcl=-.3)
# tmp = oc.data1[!is.na(latitude.work1) & !is.na(longitude.work1)]
# #tmp = head(tmp, 100000)
# plot(neotrop, border="white")
# points(tmp$longitude.work1, tmp$latitude.work1, pch=21, cex=0.1, col=2)
# plot(af, add=TRUE, border="darkgreen", lwd=1)
# plot(neotrop, add=TRUE)
# dev.off()

rm(oc.data1)
######################################################################################################################################################################################-
#### LIST OF ALL NAMES CITED FOR THE ATLANTIC FOREST ####
## How many names for the records inside the AF (valid, invalid, synonyms, etc.)?
# re-ordering the data.table, using setorder(): af.check == TRUE on top
setorder(oc.data.af.tax,  af.check2)
# valid names
tmp = oc.data.af.tax[!duplicated(oc.data.af.tax$species.correct1),]
tmp$spp.group = "valid"
# extra input names (that will be part of the excluded names from the checklist)
tmp1 = oc.data.af.tax[!oc.data.af.tax$scientificName %in% tmp$species.correct1,]  
tmp1 = tmp1[!duplicated(tmp1$scientificName),]
tmp1 = tmp1[!str_count(tmp1$scientificName," ") %in% 2 & !str_count(tmp1$species.correct1," ") %in% 3,]
tmp1$spp.group = "replaced"
# any other name removed by duplicated homogeneization?
tmp2 = oc.data.af.tax[!oc.data.af.tax$species.correct %in% tmp$species.correct1,]
tmp2 = tmp2[!tmp2$species.correct %in% tmp1$scientificName,]
tmp2 = tmp2[!str_count(tmp2$scientificName," ") %in% 2 & !str_count(tmp2$species.correct," ") %in% 3,]
tmp2$spp.group = "replaced"
# binding all names
tmp = rbindlist(list(tmp,tmp1,tmp2)) # merging the names from all tables
tmp = tmp[,c("status","scientificName","species.correct","family.correct1","species.correct1","af.check","af.check2","spp.group")]
names(tmp)[2] = "SpeciesKTSA"
rm(tmp1,tmp2)

#number of valid names inside the AF
length(unique(tmp$species.correct1)) # 6425 spp previously, then: 6019 spp, then: 6027 spp, now: 5991 spp

## Any valid names not in the entry name list? No!
spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=T, na.string=c(NA,""," "))
tmp$species.correct1[!tmp$species.correct1 %in% spp$SpeciesKTSA] #ok!

## Crossing the list of names with the list of valid names found for the AF
spp1= merge(tmp, spp, by.x= "species.correct1", by.y="SpeciesKTSA",all.x=TRUE)

#preliminary edits before replacement
spp1[is.na(spp1$species.reflora)]$spp.group = paste(spp1[is.na(spp1$species.reflora)]$spp.group,"_not_in_reflora",sep="")
spp1$scientific.name.tnrs = paste(spp1$Accepted_name,spp1$Accepted_name_author,sep=" ")
spp1$scientific.name.tpl = paste(spp1$NewTaxon.TPL,spp1$New.Authority.TPL,sep=" ")
#replacing names not found in ReFlora by the TNRS/Hans taxonomy
spp1[is.na(spp1$species.reflora),c("scientific.name.reflora","family.reflora","genus.reflora","species.reflora","name.status.reflora")] =
  spp1[is.na(spp1$species.reflora),c("scientific.name.tnrs","Accepted_name_family","Accepted_name_genus","Accepted_name","Taxonomic_status")]
#replacing names not found in ReFlora and TNRS by the Plant List
spp1[is.na(spp1$species.reflora),c("scientific.name.reflora","family.reflora","genus.reflora","species.reflora")] =
  spp1[is.na(spp1$species.reflora),c("scientific.name.tpl","Family.TPL","New.Genus.TPL","NewTaxon.TPL")]
#removing the extra column created for replacing full species names
spp1[,c("scientific.name.tnrs","scientific.name.tpl"):=NULL]

## Any TBC missing from the list? Yes!
tmp1 = spp[spp$TBC %in% "TBC",]$species.reflora[!spp[spp$TBC %in% "TBC",]$species.reflora %in% spp1$species.reflora]
setorder(oc.data.af,  af.check2) # re-ordering the data.table, using setorder(): af.check == TRUE on top
tmp2 = oc.data.af[oc.data.af$species.correct1 %in% tmp1,]
tmp2 = tmp2[!duplicated(tmp2$species.correct1),]
tmp2$spp.group = "valid_TBC"
tmp2 = tmp2[,c("status","scientificName","species.correct","family.correct1","species.correct1","af.check","af.check2","spp.group")]
spp2= merge(tmp2, spp, by.x= "species.correct1", by.y="SpeciesKTSA",all.x=TRUE)

## Any tree names cited as AF in Reflora missing from our specimens? Yes!
tmp1 = spp[!spp$species.reflora %in% spp1$species.reflora,]
tmp1 = tmp1[grepl("AtlanticForest",tmp1$domain.reflora),]
tmp1 = tmp1[!tmp1$habito %in% c("0","0.5","0.5?") & tmp1$establishment.BR %in% "native",]
tmp2 = oc.data.af[oc.data.af$species.correct1 %in% tmp1$species.reflora,]
tmp2 = tmp2[!duplicated(tmp2$species.correct1),]
tmp2$spp.group = "valid_in_reflora"
tmp2 = tmp2[,c("status","scientificName","species.correct","family.correct1","species.correct1","af.check","af.check2","spp.group")]
spp3= merge(tmp2, spp, by.x= "species.correct1", by.y="SpeciesKTSA",all.x=TRUE)

## Merging names from all "sources": specimens, TBC and missing
spp.all = rbindlist(list(spp1,spp2,spp3)) #merging the unicate and duplicated tables
rm(tmp,tmp1,tmp2,spp,spp1,spp2,spp3)
# filtering the final table
spp.all= spp.all[,c("order","checagem_VCS","TreeCo_status","SpeciesKTSA","family.correct1","species.correct1",
              "af.check", "af.check2", "spp.group",
              "establishment.AF","establishment.BR","habito","habito.reflora",
              "scientific.name.reflora","family.reflora","genus.reflora","species.reflora",
              "taxon.rank.reflora","taxon.status.reflora","name.status.reflora","notes.reflora", 
              "life.form.reflora","domain.reflora","establishment.reflora",
              "vegtype.reflora","domain.reflora","endemism.Reflora",
              "AtlanticForest","Domain","Endemism",
              "PotentialAtlanticForest","AtlanticForestChecklist_preliminary","treeco",
              "VCS.jan2019..nome.","VCS.jan2019..habito.","VCS.jan2019..establishment.","Distribution.VCS","TBC"
)]

## Any names left that are known to occur only under cultivation in the AF? Verified by RAFL in 03/01/2019
toto = c("Antrocaryon amazonicum","Attalea cohune","Annona salicifolia","Acoelorraphe wrightii",
         "Brownea rosa-de-monte","Diospyros nigra","Mimozyganthus carinatus","Calliandra calothyrsus",
         "Himatanthus attenuatus","Tabernaemontana heterophylla","Euterpe oleracea",
         "Socratea exorrhiza","Aiphanes aculeata","Dipteryx rosea","Elizabetha paraensis",
         "Inga cinnamomea","Inga pilosula","Vatairea guianensis","Pseudobombax munguba",
         "Plinia pinnata","Erisma calcaratum","Bactris brongniartii","Heterocoma lanuginosa",
         "Clusia insignis","Amburana acreana","Dipteryx punctata","Elizabetha duckei","Elizabetha durissima",
         "Parkia nitida","Allantoma lineata","Guadua superba","Triplaris weigeltiana","Centrolobium paraense",
         "Cecropia obtusa","Cecropia peltata","Mimosa glutinosa","Inga sapindoides","Bertholletia excelsa",
         "Libidibia paraguariensis","Bowdichia nitida","Diplotropis brasiliensis","Swartzia polyphylla",
         "Macrosamanea pubiramea","Macrolobium bifolium","Parkia multijuga",
	 "Cereus repandus","Rauvolfia tetraphylla")
spp.all[species.reflora %in% toto, establishment.AF := "cultivated"]
toto = c("Bowdichia nitida","Diplotropis brasiliensis","Swartzia polyphylla","Macrosamanea pubiramea",
         "Elizabetha princeps","Isertia coccinea","Psychotria poeppigiana",
	 "Garcinia longifolia","Humiriastrum cuspidatum")
spp.all[!establishment.AF %in% "cultivated" & species.reflora %in% toto, establishment.AF := "probably cultivated in JBRJ"]

## Any names left due to spurius locality/synonym assigning or other problems?
toto1 = c("Terminalia amazonia","Dalbergia amazonica","Quiina amazonica","Vachellia albicorticata",
          "Ocotea porphyria","Trichilia trifolia","Platymiscium trinitatis",
	  "Grabowskia boerhaaviifolia","Maytenus macrocarpa","Mimosa rufescens",
	  "Myrceugenia obtusa","Ocotea megaphylla","Sloanea multiflora",
	  "Solanum confusum","Solanum lanceifolium","Zanthoxylum schreberi")
spp.all[species.reflora %in% toto1, establishment.AF := "probable error in locality description"]

##Removing unecessary objects
rm(oc.data.af, oc.data.af.tax)
######################################################################################################################################################################################-
#### THE AF CHECKLIST - LIST OF ALL VALID NAMES CONFIRMED FOR THE ATLANTIC FOREST ####

## Separating synonyms and valid duplicated valid names
spp.synonym <- data.table(spp.all[duplicated(spp.all$species.correct1),])
spp.all <- data.table(spp.all[!duplicated(spp.all$species.correct1),])
spp.synonym <- spp.synonym[!SpeciesKTSA %in% spp.all$species.correct1]
spp.synonym <- spp.synonym[grepl("replaced", spp.synonym$spp.group)]

## Removing names at infraspecific level already cited in the 
tmp <- spp.all$species.correct1[str_count(spp.all$species.correct1," ") %in% 3]
tmp1 <- sapply(strsplit(tmp," "), function(x) paste(x[1],x[2],sep=" "))
tmp2 <- tmp[tmp1 %in% spp.all$species.correct1]
tmp[!tmp1 %in% spp.all$species.correct1] # species only found at infra-specific level (check synonyms?)
#spp2 = spp2[str_count(spp2$species.correct1," ") %in% 1,]
spp.all <- spp.all[!spp.all$species.correct1 %in% tmp2,]

## Creating a vector with the names of specimens determined at species and infra-specific levels
spp.all[,species.correct2 := sapply(strsplit(species.correct1," "), function(x) paste(x[1],x[2],sep=" ")),]
spp.synonym[,species.correct2 := sapply(strsplit(species.correct1," "), function(x) paste(x[1],x[2],sep=" ")),]
spp.synonym[,SpeciesKTSA1 := sapply(strsplit(SpeciesKTSA," "), function(x) paste(x[1],x[2],sep=" ")),]
spp.synonym <- spp.synonym[!SpeciesKTSA1 %in% spp.all$species.correct2]

## Removing herbs and lianas (only shrubs, treelets and trees)
spp.all <- spp.all[!habito %in% c("0","check"),]
spp.synonym <- spp.synonym[!habito %in% c("0","check"),]

## Saving the entire Atlantic Forest checklist
setkeyv(spp.all, "species.correct2")
saveRDS(spp.all, "spp.all.rds")
saveRDS(spp.synonym, "spp.synonym.rds")

## Removing exotic, cultivated and naturalized species
spp.af <- spp.all[!spp.all$establishment.BR %in% c("exotic","naturalized","cultivated","cultivated?","not in Brazil"),]

## Species not in Brazil but maybe in the AF?
tmp <- data.table(spp.all[spp.all$establishment.BR %in% "not in Brazil",]) 
tmp <- tmp[is.na(tmp$Distribution.VCS) | tmp$Distribution.VCS %in% c("PAY","BOL|PAY","PAY|BOL","ARG|BOL")]
tmp <- tmp[is.na(tmp$establishment.AF) & is.na(tmp$checagem_VCS)]
tmp <- tmp[!is.na(tmp$PotentialAtlanticForest) | !is.na(tmp$VCS.jan2019..establishment.)]
spp.af <- data.table(rbind(spp.af, tmp))
spp.af <- spp.af[!establishment.AF %in% c("exotic","naturalized","cultivated","cultivated?","not in the AF","probable error in locality description","probably cultivated in JBRJ"),]

## Removing Unresolved, Uncertain application and etc
spp.af <- spp.af[!TreeCo_status %in% c("remove","remove?","remove? Unresolved","Incorrect","Missapplied","Illegitimate","Illegitimate name","Uncertain application","Unresolved","not in the AF"),]

## Checking the results
table(spp.af$establishment.BR)
table(spp.af$establishment.AF)
table(spp.af$habito)
sort(table(spp.af$domain.reflora[!grepl("Atlantic",spp.af$domain.reflora)]))
table(spp.af$notes.reflora)
sort(table(spp.af$checagem_VCS))

#names with possible problems 
sort(table(spp.af$TreeCo_status))
spp.af <- spp.af[TreeCo_status %like% "^ok",]

#names that are synonyms that are still in the mix
#tmp = get.taxa(spp.af$species.correct1)
#tmp1 = tmp[!tmp$notes %in% "",]

## Which are the most rich families in the AF tree flora?
head(sort(table(spp.af$family.correct1), decreasing = TRUE),20)
head(sort(table(spp.af$family.correct1[spp.af$af.check2 %in% "TRUE" & spp.af$spp.group %in% c("valid","valid_not_in_reflora","valid_TBC")]), decreasing = TRUE),20)

## How many valid names in the Atlantic Forest checklist? Disregarding the 'valid_in_Reflora': they are included only as potential occurrences.
dim(spp.af[af.check2 %in% "TRUE" & spp.group %in% c("valid","valid_not_in_reflora","valid_TBC"),])[1] # before:???; then: 5166 spp; now: 5079 spp

## Saving the Atlantic Forest checklist
setkeyv(spp.af, "species.correct2")
saveRDS(spp.af,'spp.af.rds')

######################################################################################################################################################################################-
#### TRUE VS. PROBABLE DETERMINANTIONS: SPECIALISTS FROM OTHER FAMILIES AND TAXA WITH LOW TAXONOMIC COMPLEXITY (TBC) ####

#Creating a vector with the names of specimens determined at species and infra-specific levels
oc.data[,tax.check2 := as.character(tax.check1),]

#Validating all occurrences of TBC
spp = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//DomainsKnownTreesNeotropics.csv", as.is=T, na.string=c(NA,""," "))
tbc = spp$species.reflora[spp$TBC %in% "TBC"][spp$species.reflora[spp$TBC %in% "TBC"] %in% spp.af$species.correct1]
oc.data[species.correct1 %in% tbc, tax.check2 := "TRUE_TBC",]

#Validating all occurrences identified by other-families taxonomists
tmp = data.table(oc.data[tax.check %in% "TRUE" & is.na(typeStatus),])
tmp = table(tmp$determinador.name)
taxonomists = names(tmp[tmp>=20]) #### CHECK HERE: what should be the best cut-off to set the most frequent taxonomists? Set to the 1 quartile ####
oc.data[determinador.name %in% taxonomists & tax.check1 %in% FALSE, tax.check2 := "TRUE_PROBABLE",]

rm(spp,tmp,tmp1,tmp2,tbc,taxonomists)
######################################################################################################################################################################################-
#### ALL SPECIMENS FOR THE TREES OCCURRING IN THE ATLANTIC FOREST ####
## All taxonomically- and geographically validated occurrences for the species occurring inside the AF

#Creating a vector with the names of specimens determined at species and infra-specific levels
oc.data[,species.correct2 := sapply(strsplit(species.correct1," "), function(x) paste(x[1],x[2],sep=" ")),]

## Filtering the data by the original name or the name at species level (only non-duplicated specimens of the species confirmed to the AF) 
oc.data.af.spp <- data.table(oc.data[species.correct1 %in% spp.af$species.correct1 | species.correct2 %in% spp.af$species.correct1,])

## GENERAL DESCRIPTION OF THE DATA CLEANING PROCESS ##
## How many collections?
length(table(oc.data.af.spp$collectionCode)) # before: 606! now: 543 (removal of outliers, probably)
tail(sort(table(oc.data.af.spp$collectionCode)),100)
head(sort(table(oc.data.af.spp$collectionCode)),50)
table(oc.data.af.spp$collectionCode)

## How many specimens per source
sort(table(oc.data.af.spp$source))

## How many occurrences in total?
dim(oc.data.af.spp)[1] # then: 3,217,224, now:  3,115,648

## How many geographycally-validate occurrences in total?
dim(oc.data.af.spp[geo.check1 %like% "ok_"])[1]/dim(oc.data.af.spp)[1] # 92.2%
dim(oc.data.af.spp[geo.check1 %like% "ok_county|ok_locality"])[1]/dim(oc.data.af.spp)[1] # 63.8% 
dim(oc.data.af.spp[geo.check1 %like% "ok_state"])[1]/dim(oc.data.af.spp)[1] # 13.7%
dim(oc.data.af.spp[geo.check1 %like% "ok_country"])[1]/dim(oc.data.af.spp)[1] # 14.7%
dim(oc.data.af.spp[geo.check1 %in% "no_coord"])[1]/dim(oc.data.af.spp)[1] # 5.3%
dim(oc.data.af.spp[geo.check1 %in% c(NA,"no_cannot_check","no_problem_in_water","no_too_far")])[1]/dim(oc.data.af.spp)[1] # 2.5%
#table(oc.data.af.spp$geo.check1, useNA = "ifany")

## How many data without info on county/municipality?
dim(oc.data.af.spp[municipio %in% NA])[1]/dim(oc.data.af.spp)[1] # 31.3%

## How many taxonomically validate occurrences in total?
dim(oc.data.af.spp[tax.check1 %in% TRUE])[1]/dim(oc.data.af.spp)[1] # 40.34%

## How many taxonomically validate and probably validated occurrences in total?
dim(oc.data.af.spp[tax.check2  %like% "TRUE"])[1]/dim(oc.data.af.spp)[1] # 50.11%

## How many taxonomically unvalid occurrences due to NAs or to other names?
dim(oc.data.af.spp[tax.check1 %in% c("FALSE","cannot_check") & determinador.name1 %in% c("Semdeterminador","cannot_check")])[1]/dim(oc.data.af.spp)[1] # 28.06%
dim(oc.data.af.spp[tax.check1 %in% "FALSE" & !determinador.name1 %in% c("Semdeterminador")])[1]/dim(oc.data.af.spp)[1] # 31.49%
dim(oc.data.af.spp[tax.check2 %in% "FALSE" & !determinador.name1 %in% c("Semdeterminador")])[1]/dim(oc.data.af.spp)[1] # 23.36%

## How many non-duplicated occurrences in total?
#first by dup.ID, then decreasing dup.numb, then decreasing dup.prop, then decreasing source name (preferenc to splink, than jabot, then splink_new, then gbif)
oc.data.af.spp[, source1 := as.double(as.character(factor(source, levels = c("gbif","sndb","jabot","splink","splink_new"), labels = c(5,4,2,1,3)))), ]
setorder(oc.data.af.spp,  dup.ID, -dup.numb, -dup.prop, source1)               ## re-ordering the data.table, using setorder()
oc.data.af.spp[,source1:=NULL] #removing the extra column created for ranking
#creating the unique accession number and multiple-acession number for each specimen
oc.data.af.spp[, dup.ID1 := dup.ID]
oc.data.af.spp[is.na(dup.ID1), dup.ID1 := numTombo, by = numTombo]
#removing the duplicates
oc.data.af.spp1 = unique(oc.data.af.spp, by = "dup.ID1")
#removing the extra column created for ranking
oc.data.af.spp[,dup.ID1:=NULL] 
oc.data.af.spp1[,dup.ID1:=NULL]
#How many duplicates
1 - dim(oc.data.af.spp1)[1]/dim(oc.data.af.spp)[1] # 23.76%

## How many non-duplicated, geographically validated specimens?
oc.data.af.spp.geo <- oc.data.af.spp1[geo.check1 %like% "ok_"]
dim(oc.data.af.spp.geo)[1] # before: 2,205,706 total occurrences; then: 2,221,714 occs; now 2,142,165 occs
#dim(oc.data.af.spp.geo[species.correct1 == "Myrcia splendens"]) # 14503 records

## How many non-duplicated, geographically and taxonomically validated specimens?
oc.data.af.spp.geo.tax <- oc.data.af.spp.geo[tax.check1 %like% TRUE]
dim(oc.data.af.spp.geo.tax)[1] # 756,118 valid occurrences; now: 774,337 valid occurrences; now: 755,177 valid occurrences
#dim(oc.data.af.spp.geo.tax[species.correct1 == "Myrcia splendens"]) # 6415 records

## How many valid specimens inside or outside the Atlantic Forest?
dim(oc.data.af.spp.geo.tax[af.check2 %in% c("TRUE","FALSE")])[1] # now: 555,254

## How many non-duplicated, geographically and taxonomically validated (true or probable) specimens?
oc.data.af.spp.geo.tax1 <- oc.data.af.spp.geo[tax.check2 %like% "TRUE"]
dim(oc.data.af.spp.geo.tax1)[1] # 983,680 valid occurrences; then: 999,837 occurrences; now: 976,142 occurrences
#dim(oc.data.af.spp.geo.tax1[species.correct1 == "Myrcia splendens"]) # 6995 records
saveRDS(oc.data.af.spp.geo.tax1, "oc.data.af.spp.geo.tax1.rds")

## How many non-duplicated, geographically and taxonomically validated specimens inside the AF?
dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE])[1] # before: 224,162; then: 254,470; now: 252,972
#dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE & species.correct == "Myrcia splendens"])[1] # 2184 records

## How many species for the core AF? 5074-5079 spp
oc.data.af.spp.geo.tax[af.check2 %in% TRUE, .(uniqueN(species.correct2))] # how many species af.check == TRUE
dim(spp.af[af.check2 %in% TRUE & spp.group %in% c("valid","valid_not_in_reflora","valid_TBC"),])[1]

## How many species for the core AF and transitions? 5278-5283 spp
oc.data.af.spp.geo.tax[af.check2 %in% c("TRUE","TRUE_in_transition"), .(uniqueN(species.correct2))] # how many species af.check == TRUE & TRUE_in_transition
dim(spp.af[af.check2 %in% c("TRUE","TRUE_in_transition") & spp.group %in% c("valid","valid_not_in_reflora","valid_TBC"),])[1]

## How many species for each country: Argentina, Paraguay and Brazil?
#Brazil: 5115 species out of 5161 (97.4%) from 254,470 records (validation ratio: 37.8%)
oc.data.af.spp.geo.tax[af.check2 %in% TRUE & loc.correct %like% "brazil", .(uniqueN(species.correct2))]
dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE & loc.correct %like% "brazil"])[1]/dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE])[1]
dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE & loc.correct %like% "brazil"])[1]/dim(oc.data.af.spp1[af.check2 %in% TRUE & loc.correct %like% "brazil"])[1]

#Argentina: 303 species out of 5161 (5.8%) from 2367 records (validation ratio: 21.3%)
oc.data.af.spp.geo.tax[af.check2 %in% TRUE & loc.correct %like% "argentina", .(uniqueN(species.correct2))]
dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE & loc.correct %like% "argentina"])[1]/dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE])[1]
dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE & loc.correct %like% "argentina"])[1]/dim(oc.data.af.spp1[af.check2 %in% TRUE & loc.correct %like% "argentina"])[1]

#Paraguay: 463 species out of 5161 (8.9%) from 4051 records (validation ratio: 38.7%)
oc.data.af.spp.geo.tax[af.check2 %in% TRUE & loc.correct %like% "paraguay", .(uniqueN(species.correct2))]
dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE & loc.correct %like% "paraguay"])[1]/dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE])[1]
dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE & loc.correct %like% "paraguay"])[1]/dim(oc.data.af.spp1[af.check2 %in% TRUE & loc.correct %like% "paraguay"])[1]

## How many species per biogreographical regions?? Is it easy to do?


#### PLOTTING THE RESULTS ####

## Geographically validated occurrences ##
# jpeg(filename = "geo.occs_new.jpeg", width = 3000*1.5, height = 3000*2, units = "px", pointsize = 12,
#      res = 600, family = "sans", type="cairo", bg="white")
# par(mfrow=c(1,1), las=1, mar=c(0,0,0,0), mgp=c(1.7,0.4,0), tcl=-.3)
# tmp = oc.data.af.spp.geo[!is.na(latitude.work1) & !is.na(longitude.work1)]
# #tmp = head(tmp, 100000)
# plot(neotrop, border="white")
# points(tmp$longitude.work1, tmp$latitude.work1, pch=21, cex=0.1, col=2)
# plot(af, add=TRUE, border="darkgreen", lwd=1)
# plot(neotrop, add=TRUE)
# dev.off()
# 
# ## Geographically validated occurrences ##
# jpeg(filename = "geo.tax.occs_new.jpeg", width = 3000*1.5, height = 3000*2, units = "px", pointsize = 12,
#      res = 600, family = "sans", type="cairo", bg="white")
# par(mfrow=c(1,1), las=1, mar=c(0,0,0,0), mgp=c(1.7,0.4,0), tcl=-.3)
# tmp = oc.data.af.spp.geo.tax[!is.na(latitude.work1) & !is.na(longitude.work1)]
# #tmp = head(tmp, 100000)
# plot(neotrop, border="white")
# points(tmp$longitude.work1, tmp$latitude.work1, pch=21, cex=0.1, col=2)
# plot(af, add=TRUE, border="darkgreen", lwd=1)
# plot(neotrop, add=TRUE)
# dev.off()
# 
# ## Geographically validated occurrences in the AF ##
# jpeg(filename = "af.geo.tax.occs_new.jpeg", width = 3000*1.5, height = 3000*2, units = "px", pointsize = 12,
#      res = 600, family = "sans", type="cairo", bg="white")
# par(mfrow=c(1,1), las=1, mar=c(0,0,0,0), mgp=c(1.7,0.4,0), tcl=-.3)
# tmp = oc.data.af.spp.geo.tax[af.check2 %in% TRUE]
# tmp = tmp[!af.check1 %like% "FALSE|CHECK"]
# tmp = tmp[geo.check1 %like% "ok_"]
# tmp = tmp[!is.na(latitude.work1) & !is.na(longitude.work1)]
# tmp = tmp[!loc.correct %in% c("brazil_minas gerais_brasilandia minas")]
# #tmp = head(tmp, 100000)
# plot(af, border="white")
# #plot(af, border="black", xlim=c(-40,-31), ylim=c(-20,-15))
# points(tmp$longitude.work1, tmp$latitude.work1, pch=21, cex=0.2, col=2)
# #text(as.double(tmp$longitude.work1), as.double(tmp$latitude.work1), labels=tmp$numTombo, cex=0.7, pos=1:4)
# plot(af, add=TRUE, border="darkgreen", lwd=1)
# plot(neotrop, add=TRUE)
# dev.off()


######################################################################################################################################################################################-
#### ENDEMISM AND NEAR-ENDEMISM LEVELS FOR ALL SPECIES OCCURRING IN THE ATLANTIC FOREST ####

#### Endemism ratio for the species (only true taxonomically validates species) ####
tmp = table(oc.data.af.spp.geo.tax$species.correct2, oc.data.af.spp.geo.tax$af.check2)
endemismo = cbind.data.frame(spp=row.names(tmp),outside=as.double(tmp[,"FALSE"]),out.in=as.double(tmp[,"FALSE_in_transition"]),in.out=as.double(tmp[,"TRUE_in_transition"]),inside=as.double(tmp[,"TRUE"]),cc=as.double(tmp[,"cannot_check"]))
endemismo$total = apply(endemismo[,c("outside","inside")],1,sum,na.rm=TRUE)
endemismo$total1 = apply(endemismo[,c("outside","out.in","in.out","inside")],1,sum,na.rm=TRUE)
endemismo$nivel = round(100*endemismo$inside/endemismo$total,6)
endemismo$nivel1 = round(100*apply(endemismo[,c("in.out","inside")],1,sum,na.rm=TRUE)/endemismo$total1,6)
tmp = endemismo[,2:5]
tmp$out.in1 = tmp$out.in*0.5
tmp$in.out1 = tmp$in.out*0.5
tmp = tmp[,c(1,5,6,4)]
tmp$sum.total = apply(tmp,1,sum)
tmp$sum.in  = apply(tmp[,c("inside","in.out1")],1,sum)
#tmp$sum.out = apply(tmp[,c("outside","out.in1")],1,sum) 
endemismo$nivel2 = round(100*tmp$sum.in/tmp$sum.total,6)
#endemismo$nivel3 = (-tmp$sum.out + tmp$sum.in)/tmp$sum.total
par(mfrow=c(1,2))
plot(endemismo$nivel2 ~ endemismo$nivel)
plot(endemismo$nivel2 ~ endemismo$nivel1)
par(mfrow=c(1,1))

#Converting indices for species without valid occurrences
endemismo$nivel[is.nan(endemismo$nivel)] = NA
endemismo$nivel1[is.nan(endemismo$nivel1)] = NA
endemismo$nivel2[is.nan(endemismo$nivel2)] = NA

#Removing species not in the checklist
endemismo = endemismo[endemismo$spp %in% spp.af$species.correct2[!spp.af$spp.group %in% "valid_in_reflora" & spp.af$af.check2 %in% TRUE],]

#plotting the results
# par(mfrow=c(1,3),mar=c(2,2,2,1))
# hist(endemismo$nivel,nclass=40, prob=TRUE, ylim=c(0,0.2)); abline(v=c(90,95,99), col="red",lty=2)
# hist(endemismo$nivel1,nclass=40, prob=TRUE, ylim=c(0,0.2)); abline(v=c(90,95,99), col="red",lty=2)
# hist(endemismo$nivel2,nclass=40, prob=TRUE, ylim=c(0,0.2)); abline(v=c(90,95,99), col="red",lty=2)

###Possible identification problems?
tmp = endemismo[endemismo$total>200 & endemismo$inside<=3,]
tmp = data.table(oc.data.af.spp.geo.tax[oc.data.af.spp.geo.tax$species.correct %in% as.character(tmp$spp) & oc.data.af.spp.geo.tax$af.check1 %in% TRUE,list(coletor.name,recordNumber,year,country,stateProvince,municipality,loc.correct,determinador.name,determinador.name1,ano.det,species.correct,species.correct1, dup.ID),])
setkeyv(tmp, "species.correct1")

### Plotting the number of endemics per endemism ratio
prop = seq(0,100,by=1)
end.prop = NULL
for (i in 1:length(prop)) end.prop[i] = 100*sum(endemismo$nivel>=prop[i], na.rm=TRUE)/dim(endemismo[!is.na(endemismo$nivel),])[1]
end.prop.2 = NULL
for (i in 1:length(prop)) end.prop.2[i] = 100*sum(endemismo$nivel2>=prop[i], na.rm=TRUE)/dim(endemismo[!is.na(endemismo$nivel2),])[1]
par(mfrow=c(1,2),mar=c(4,4,2,1),mgp=c(2.5,0.7,0), las=1, tcl=-0.2)
plot(end.prop ~ prop, xlab= "% of occurrences inside", ylab= "% of endemism", type="b")
abline(h=50,lty=2,col=3); abline(v=85.5,lty=2,col=3)
abline(h=80,lty=2,col=2); abline(v=12.5,lty=2,col=2)
plot(end.prop.2 ~ prop, xlab= "% of occurrences inside", ylab= "% of endemism", type="b")
abline(h=50,lty=2,col=3); abline(v=81,lty=2,col=3)
abline(h=80,lty=2,col=2); abline(v=14,lty=2,col=2)

### Finding the point where the slope get more steep
sspl <- smooth.spline(prop, end.prop, df = 13)
ispl <- interpSpline(prop, end.prop)
fspl <- splinefun(prop, end.prop)

### Using piece-wise regression to identify the breaking points in the endemism level
# Set X breakpoints: Set psi=NA and K=n:
#Only Inside or Outside the AF
tmp0.1 = lm(end.prop ~ prop)
tmp0.2 = segmented(tmp0.1, seg.Z = ~prop, psi=c(20,80), control=seg.control(display=FALSE, K=3)) ## Piecewise Regression METHOD 1
tmp0.3 = segmented(tmp0.1, seg.Z = ~prop, psi=c(20,80,90), control=seg.control(display=FALSE, K=4)) ## Piecewise Regression METHOD 1
tmp0.4 = segmented(tmp0.1, seg.Z = ~prop, psi=c(10,20,80,90), control=seg.control(display=FALSE, K=5)) ## Piecewise Regression METHOD 1
tmp0.5 = segmented(tmp0.1, seg.Z = ~prop, psi=c(10,20,50,80,90), control=seg.control(display=FALSE, K=6)) ## Piecewise Regression METHOD 1
#slope(tmp0.4)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp0.1,tmp0.2,tmp0.3,tmp0.4,tmp0.5)
#Inside, outside and in the transitions of the AF
tmp0.1.2 = lm(end.prop.2 ~ prop)
tmp0.2.2 = segmented(tmp0.1.2, seg.Z = ~prop, psi=c(20,80), control=seg.control(display=FALSE, K=3)) ## Piecewise Regression METHOD 1
tmp0.3.2 = segmented(tmp0.1.2, seg.Z = ~prop, psi=c(20,80,90), control=seg.control(display=FALSE, K=4)) ## Piecewise Regression METHOD 1
tmp0.4.2 = segmented(tmp0.1.2, seg.Z = ~prop, psi=c(10,20,80,90), control=seg.control(display=FALSE, K=5)) ## Piecewise Regression METHOD 1
tmp0.5.2 = segmented(tmp0.1.2, seg.Z = ~prop, psi=c(10,20,50,80,90), control=seg.control(display=FALSE, K=6)) ## Piecewise Regression METHOD 1
AICtab(tmp0.1.2,tmp0.2.2,tmp0.3.2,tmp0.4.2,tmp0.5.2)

#plotting the results
par(mfrow=c(1,1),mar=c(2,3,2,1))
plot(end.prop ~ prop, pch=16)
#abline(tmp1,lwd=2)
plot(tmp0.2,lwd=3,col=2,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp0.2$psi[,2], lty=3, col=2)
plot(tmp0.3,lwd=3,col=3,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp0.3$psi[,2], lty=3, col=3)
plot(tmp0.4,lwd=3,col=4,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp0.4$psi[-2,2], lty=3, col=4)
plot(tmp0.5,lwd=3,col=5,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp0.5$psi[-c(1,3),2], lty=3, col=5)

#All break-point estimtes
breaks0 = cbind(rbind(confint(tmp0.2)$prop[1,],c(NA,NA,NA),confint(tmp0.2)$prop[2,]),
               confint(tmp0.3)$prop,confint(tmp0.4)$prop[-2,],confint(tmp0.5)$prop[-c(1,3),])

plot(end.prop.2 ~ prop, pch=16)
#abline(tmp1,lwd=2)
plot(tmp0.2.2,lwd=3,col=2,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp0.2.2$psi[,2], lty=3, col=2)
plot(tmp0.3.2,lwd=3,col=3,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp0.3.2$psi[,2], lty=3, col=3)
plot(tmp0.4.2,lwd=3,col=4,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp0.4.2$psi[-2,2], lty=3, col=4)
plot(tmp0.5.2,lwd=3,col=5,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp0.5.2$psi[-c(1,3),2], lty=3, col=5)

breaks0.2 = cbind(rbind(confint(tmp0.2.2)$prop[1,],c(NA,NA,NA),confint(tmp0.2.2)$prop[2,]),
                confint(tmp0.3.2)$prop,confint(tmp0.4.2)$prop[-2,],confint(tmp0.5.2)$prop[-c(1,3),])

#### Endemism ratio for the species (true or probable taxonomically validates species) ####
tmp = table(oc.data.af.spp.geo.tax1$species.correct2, oc.data.af.spp.geo.tax1$af.check2)
endemismo1 = cbind.data.frame(spp=row.names(tmp),outside=as.double(tmp[,"FALSE"]),out.in=as.double(tmp[,"FALSE_in_transition"]),in.out=as.double(tmp[,"TRUE_in_transition"]),inside=as.double(tmp[,"TRUE"]),cc=as.double(tmp[,"cannot_check"]))
endemismo1$total = apply(endemismo1[,c("outside","inside")],1,sum,na.rm=TRUE)
endemismo1$total1 = apply(endemismo1[,c("outside","out.in","in.out","inside")],1,sum,na.rm=TRUE)
endemismo1$nivel = round(100*endemismo1$inside/endemismo1$total,3)
endemismo1$nivel1 = round(100*apply(endemismo1[,c("in.out","inside")],1,sum,na.rm=TRUE)/endemismo1$total1,3)
tmp = endemismo1[,2:5]
tmp$out.in1 = tmp$out.in*0.5
tmp$in.out1 = tmp$in.out*0.5
tmp = tmp[,c(1,5,6,4)]
tmp$sum.total = apply(tmp,1,sum)
tmp$sum.in  = apply(tmp[,c("inside","in.out1")],1,sum)
#tmp$sum.out = apply(tmp[,c("outside","out.in1")],1,sum) 
endemismo1$nivel2 = round(100*tmp$sum.in/tmp$sum.total,6)
#endemismo$nivel3 = (-tmp$sum.out + tmp$sum.in)/tmp$sum.total
par(mfrow=c(1,2))
plot(endemismo1$nivel2 ~ endemismo1$nivel)
plot(endemismo1$nivel2 ~ endemismo1$nivel1)
par(mfrow=c(1,1))

#Converting indices for species without valid occurrences
endemismo1$nivel[is.nan(endemismo1$nivel)] = NA
endemismo1$nivel1[is.nan(endemismo1$nivel1)] = NA
endemismo1$nivel2[is.nan(endemismo1$nivel2)] = NA

#Removing species not in the checklist
endemismo1 = endemismo1[endemismo1$spp %in% spp.af$species.correct2[!spp.af$spp.group %in% "valid_in_reflora" & spp.af$af.check2 %in% TRUE],]

#plotting the results
# par(mfrow=c(2,2),mar=c(2,3,2,1))
# hist(endemismo$nivel,nclass=40, prob=TRUE, ylim=c(0,0.2)); abline(v=c(90,95,99), col="red",lty=2)
# hist(endemismo$nivel1,nclass=40, prob=TRUE, ylim=c(0,0.2)); abline(v=c(90,95,99), col="red",lty=2)
# hist(endemismo1$nivel2,nclass=40, prob=TRUE, ylim=c(0,0.2)); abline(v=c(90,95,99), col="red",lty=2)

###Number of endemics per endemism ratio ### 
end.prop1 = NULL
for (i in 1:length(prop)) end.prop1[i] = 100*sum(endemismo1$nivel>=prop[i], na.rm=TRUE)/dim(endemismo1[!is.na(endemismo1$nivel),])[1]
end.prop1.2 = NULL
for (i in 1:length(prop)) end.prop1.2[i] = 100*sum(endemismo1$nivel2>=prop[i], na.rm=TRUE)/dim(endemismo1[!is.na(endemismo1$nivel2),])[1]
# par(mfrow=c(1,2),mar=c(4,4,2,1),mgp=c(2.5,0.7,0), las=1, tcl=-0.2)
# plot(end.prop1 ~ prop, xlab= "% of occurrences inside", ylab= "% of endemism", ylim=c(34,100))
# abline(h=50,lty=2,col=3); abline(v=85.5,lty=2,col=3)
# plot(end.prop1.2 ~ prop, xlab= "% of occurrences inside", ylab= "% of endemism", ylim=c(34,100))
# abline(h=50,lty=2,col=3); abline(v=82,lty=2,col=3)

### Finding the point where the slope get more steep
# sspl <- smooth.spline(prop, end.prop1, df = 13)
# ispl <- interpSpline(prop, end.prop1)
# fspl <- splinefun(prop, end.prop1)

#plotting the fit and the first derivates
# par(mfrow=c(1,3))
# plot(end.prop1 ~ prop)
# lines(ispl$coefficients[,1] ~ ispl$knots, lwd=2, col=2)
# lines(sspl, lwd=2, col=4)
# plot(fspl(prop, deriv = 1) ~ prop)
# #plot(fspl(prop, deriv = 1) ~ end.prop1)
# plot(fspl(end.prop1, deriv = 1) ~ prop)
# #plot(fspl(end.prop1, deriv = 1)~ end.prop1)

### Using piece-wise regression to identify the breaking points in the endemism level
# Set X breakpoints: Set psi=NA and K=n:
#Only Inside or Outside the AF
tmp1 = lm(end.prop1 ~ prop)
tmp2 = segmented(tmp1, seg.Z = ~prop, psi=c(20,80), control=seg.control(display=FALSE, K=3)) ## Piecewise Regression METHOD 1
tmp3 = segmented(tmp1, seg.Z = ~prop, psi=c(20,80,90), control=seg.control(display=FALSE, K=4)) ## Piecewise Regression METHOD 1
tmp4 = segmented(tmp1, seg.Z = ~prop, psi=c(10,20,80,90), control=seg.control(display=FALSE, K=5)) ## Piecewise Regression METHOD 1
tmp5 = segmented(tmp1, seg.Z = ~prop, psi=c(10,20,50,80,90), control=seg.control(display=FALSE, K=6)) ## Piecewise Regression METHOD 1
slope(tmp4)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp1,tmp2,tmp3,tmp4,tmp5)
#Inside, outside and in the transitions of the AF
tmp1.2 = lm(end.prop1.2 ~ prop)
tmp2.2 = segmented(tmp1.2, seg.Z = ~prop, psi=c(20,80), control=seg.control(display=FALSE, K=3)) ## Piecewise Regression METHOD 1
tmp3.2 = segmented(tmp1.2, seg.Z = ~prop, psi=c(20,80,90), control=seg.control(display=FALSE, K=4)) ## Piecewise Regression METHOD 1
tmp4.2 = segmented(tmp1.2, seg.Z = ~prop, psi=c(10,20,80,90), control=seg.control(display=FALSE, K=5)) ## Piecewise Regression METHOD 1
tmp5.2 = segmented(tmp1.2, seg.Z = ~prop, psi=c(10,20,50,80,90), control=seg.control(display=FALSE, K=6)) ## Piecewise Regression METHOD 1
slope(tmp4.2)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp1.2,tmp2.2,tmp3.2,tmp4.2,tmp5.2)

#plotting the results
par(mfrow=c(1,1),mar=c(2,3,2,1))
plot(end.prop1 ~ prop, pch=16)
#abline(tmp1,lwd=2)
plot(tmp2,lwd=3,col=2,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp2$psi[,2], lty=3, col=2)
plot(tmp3,lwd=3,col=3,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp3$psi[,2], lty=3, col=3)
plot(tmp4,lwd=3,col=4,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp4$psi[-1,2], lty=3, col=4)
plot(tmp5,lwd=3,col=5,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp5$psi[-c(1,3),2], lty=3, col=5)

#All break-point estimtes
breaks = cbind(rbind(confint(tmp2)$prop[1,],c(NA,NA,NA),confint(tmp2)$prop[2,]),
               confint(tmp3)$prop,confint(tmp4)$prop[-1,],confint(tmp5)$prop[-c(1,3),])

# #based on the low end break
# dim(endemismo1[!is.na(endemismo1$nivel) & endemismo1$nivel>breaks[3,1],])[1]/dim(endemismo1[!is.na(endemismo1$nivel),])[1]
# dim(endemismo1[!is.na(endemismo1$nivel) & endemismo1$nivel>breaks[2,4],])[1]/dim(endemismo1[!is.na(endemismo1$nivel),])[1]
# dim(endemismo1[!is.na(endemismo1$nivel) & endemismo1$nivel>breaks[2,7],])[1]/dim(endemismo1[!is.na(endemismo1$nivel),])[1]
# dim(endemismo1[!is.na(endemismo1$nivel) & endemismo1$nivel>breaks[2,10],])[1]/dim(endemismo1[!is.na(endemismo1$nivel),])[1]
# 
# #based on the high end break
# dim(endemismo1[!is.na(endemismo1$nivel) & endemismo1$nivel>breaks[3,1],])[1]/dim(endemismo1[!is.na(endemismo1$nivel),])[1]
# dim(endemismo1[!is.na(endemismo1$nivel) & endemismo1$nivel>breaks[3,4],])[1]/dim(endemismo1[!is.na(endemismo1$nivel),])[1]
# dim(endemismo1[!is.na(endemismo1$nivel) & endemismo1$nivel>breaks[3,7],])[1]/dim(endemismo1[!is.na(endemismo1$nivel),])[1]
# dim(endemismo1[!is.na(endemismo1$nivel) & endemismo1$nivel>breaks[3,10],])[1]/dim(endemismo1[!is.na(endemismo1$nivel),])[1]

#plotting the results
par(mfrow=c(1,1),mar=c(2,3,2,1))
plot(end.prop1.2 ~ prop, pch=16)
#abline(tmp1,lwd=2)
plot(tmp2.2,lwd=3,col=2,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp2.2$psi[,2], lty=3, col=2)
plot(tmp3.2,lwd=3,col=3,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp3.2$psi[,2], lty=3, col=3)
plot(tmp4.2,lwd=3,col=4,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp4.2$psi[-2,2], lty=3, col=4)
plot(tmp5.2,lwd=3,col=5,add=TRUE,dens.rug=FALSE,rug=FALSE)
abline(v=tmp5.2$psi[-c(1,3),2], lty=3, col=5)

#All break-point estimtes
breaks.2 = cbind(rbind(confint(tmp2.2)$prop[1,],c(NA,NA,NA),confint(tmp2.2)$prop[2,]),
               confint(tmp3.2)$prop,confint(tmp4.2)$prop[-2,],confint(tmp5.2)$prop[-c(1,3),])


######################################################################################################################################################################################-
#### COMPARING THE ENDEMISM LEVELS AND THE ENDEMISM ACCEPTED IN REFLORA #### 

#### PERFORM ANALYSIS ONLY WITH SPECIES WITH MORE THAN 10 OCCURRENCES? ####
endemismo = merge(endemismo, spp.af [,-c("checagem_VCS","TreeCo_status","VCS.jan2019..nome.","VCS.jan2019..habito.","VCS.jan2019..establishment.","Distribution.VCS")],by.x = "spp", by.y = "species.correct2", all.x = TRUE)
endemismo1 = merge(endemismo1, spp.af [,-c("checagem_VCS","TreeCo_status","VCS.jan2019..nome.","VCS.jan2019..habito.","VCS.jan2019..establishment.","Distribution.VCS")],by.x = "spp", by.y = "species.correct2", all.x = TRUE)
table(endemismo1$habito)
table(endemismo1$domain.reflora, endemismo1$endemism.Reflora)

#summary description of the endemism level following Reflora classification
aggregate(endemismo1$nivel[!is.na(endemismo1$domain.reflora) & endemismo1$total>=10], list(endemismo1$domain.reflora[!is.na(endemismo1$domain.reflora) & endemismo1$total>=10] == "AtlanticForest"), quantile, prob= c(0,0.05,0.1,0.2,0.25,0.5,0.75,0.8,0.9,0.95,1))
aggregate(endemismo1$nivel2[!is.na(endemismo1$domain.reflora) & endemismo1$total1>=10], list(endemismo1$domain.reflora[!is.na(endemismo1$domain.reflora) & endemismo1$total1>=10] == "AtlanticForest"), quantile, prob= c(0,0.05,0.1,0.2,0.25,0.5,0.75,0.8,0.9,0.95,1))

## CHECKING POSSIBLE CLASSIFICATION PROBLEMS ##
endemismo1[!is.na(endemismo1$domain.reflora) & !is.na(endemismo1$Domain) & endemismo1$total>=10 & endemismo1$nivel < 30 & endemismo1$domain.reflora == "AtlanticForest" & endemismo1$Domain == "AtlanticForest",][,1:10]
endemismo1[!is.na(endemismo1$domain.reflora) & !is.na(endemismo1$Domain) & endemismo1$total>=10 & endemismo1$nivel > 90 & !grepl("AtlanticForest",endemismo1$domain.reflora) & !grepl("AtlanticForest",endemismo1$Domain),][,1:10]
endemismo1[!is.na(endemismo1$domain.reflora) & !is.na(endemismo1$Domain) & endemismo1$total>=10 & endemismo1$nivel2 < 30 & endemismo1$domain.reflora == "AtlanticForest" & endemismo1$Domain == "AtlanticForest",][,1:10]
endemismo1[!is.na(endemismo1$domain.reflora) & !is.na(endemismo1$Domain) & endemismo1$total>=10 & endemismo1$nivel2 > 90 & !grepl("AtlanticForest",endemismo1$domain.reflora) & !grepl("AtlanticForest",endemismo1$Domain),][,1:10]

# Removing possible NAs
endemismo = endemismo[!is.na(endemismo$nivel),]
endemismo1 = endemismo1[!is.na(endemismo1$nivel),]

#### Performing the comparison to the Reflora classification - only TRUE taxonomy ####
reflora.af = endemismo$domain.reflora[!is.na(endemismo$domain.reflora)] == "AtlanticForest"
reflora.other = !grepl("AtlanticForest",endemismo$domain.reflora[!is.na(endemismo$domain.reflora)])
end.match = cbind.data.frame(false.false = rep(NA,length(prop)), false.true= rep(NA,length(prop)), true.true= rep(NA,length(prop)),
                             false.false.2 = rep(NA,length(prop)), false.true.2= rep(NA,length(prop)), true.true.2= rep(NA,length(prop)))
end.match.other = cbind.data.frame(false.false = rep(NA,length(prop)), false.true= rep(NA,length(prop)), true.true= rep(NA,length(prop)),
                                   false.false.2 = rep(NA,length(prop)), false.true.2= rep(NA,length(prop)), true.true.2= rep(NA,length(prop)))
for(i in 1:length(prop)) {
  #near endemics, only TRUE and FALSE geocheck
  occ.end = endemismo$nivel[!is.na(endemismo$domain.reflora)] >= prop[i]
  end.match$false.false[i] = sum(!reflora.af & !occ.end)
  end.match$false.true[i] = sum(!reflora.af & occ.end | reflora.af & !occ.end)
  end.match$true.true[i] = sum(reflora.af & occ.end)
  #near endemics, TRUE, FALSE and _TRANSITION
  occ.end.2 = endemismo$nivel2[!is.na(endemismo$domain.reflora)]>=prop[i]
  end.match$false.false.2[i] = sum(!reflora.af & !occ.end.2)
  end.match$false.true.2[i] = sum(!reflora.af & occ.end.2 | reflora.af & !occ.end.2)
  end.match$true.true.2[i] = sum(reflora.af & occ.end.2)
  #ocasional species, only TRUE and FALSE
  occ.end1 = endemismo$nivel[!is.na(endemismo$domain.reflora)]<prop[i]
  end.match.other$false.false[i] = sum(!reflora.other & !occ.end1)
  end.match.other$false.true[i] = sum(!reflora.other & occ.end1 | reflora.other & !occ.end1)
  end.match.other$true.true[i] = sum(reflora.other & occ.end1)
  #ocasional species, TRUE, FALSE and _TRANSITION
  occ.end1.2 = endemismo$nivel2[!is.na(endemismo$domain.reflora)]<prop[i]
  end.match.other$false.false.2[i] = sum(!reflora.other & !occ.end1.2)
  end.match.other$false.true.2[i] = sum(!reflora.other & occ.end1.2 | reflora.other & !occ.end1.2)
  end.match.other$true.true.2[i] = sum(reflora.other & occ.end1.2)
}
row.names(end.match) = prop; row.names(end.match.other) = prop 

### Using piece-wise regression to identify the breaking points in the endemism level
false.true0 = end.match$false.true[prop>=50]
false.true02 = end.match$false.true.2[prop>=50]
false.true01 = end.match.other$false.true[prop<50]
false.true01.2 = end.match.other$false.true.2[prop<50]
prop1 = prop[prop>=50]
prop2 = prop[prop<50]

## Near endemics break, only TRUE and FALSE occurrrences in the AF
tmp1 = lm(false.true0 ~ prop1)
# Set X breakpoints: Set psi=NA and K=n:
tmp2 = segmented(tmp1, seg.Z = ~prop1, psi=c(75,90), control=seg.control(display=FALSE)) ## Piecewise Regression METHOD 1
tmp3 = segmented(tmp1, seg.Z = ~prop1, psi=c(75,90,95), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
tmp4 = segmented(tmp1, seg.Z = ~prop1, psi=c(70,80,85,96), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
#slope(tmp4)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp1,tmp2,tmp3,tmp4)
brk0.near.nivel = tmp4

## Near endemics break, TRUE, FALSE and _TRANSITION occurrrences in the AF
tmp1 = lm(false.true02 ~ prop1)
# Set X breakpoints: Set psi=NA and K=n:
tmp2 = segmented(tmp1, seg.Z = ~prop1, psi=c(75,80), control=seg.control(display=FALSE, K=3)) ## Piecewise Regression METHOD 1
tmp3 = segmented(tmp1, seg.Z = ~prop1, psi=c(75,80,95), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
tmp4 = segmented(tmp1, seg.Z = ~prop1, psi=c(75,80,90,95), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
#slope(tmp4)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp1,tmp2,tmp3,tmp4)
brk0.near.nivel2 = tmp4

## Occasional species, only TRUE and FALSE occurrrences in the AF
tmp1.1 = lm(false.true01 ~ prop2)
# Set X breakpoints: Set psi=NA and K=n:
tmp2.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(5,10), control=seg.control(display=FALSE)) ## Piecewise Regression METHOD 1
tmp3.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(5,10,15), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
tmp4.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(5,10,15,25), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
#slope(tmp4.1)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp1.1,tmp2.1,tmp3.1,tmp4.1)
brk0.ocas.nivel = tmp4.1

## Occasional species, TRUE, FALSE and _TRANSITION occurrrences in the AF
tmp1.1 = lm(false.true01.2 ~ prop2)
# Set X breakpoints: Set psi=NA and K=n:
tmp2.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(5,10), control=seg.control(display=FALSE, K=3)) ## Piecewise Regression METHOD 1
tmp3.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(5,10,20), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
tmp4.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(5,10,15,25), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
#slope(tmp4.1)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp1.1,tmp2.1,tmp3.1,tmp4.1)
brk0.ocas.nivel2 = tmp4.1

#### Performing the comparison to the Reflora classification - TRUE and PROBABLE taxonomy ####
reflora.af = endemismo1$domain.reflora[!is.na(endemismo1$domain.reflora)] == "AtlanticForest"
reflora.other = !grepl("AtlanticForest",endemismo1$domain.reflora[!is.na(endemismo1$domain.reflora)])
end.match1 = cbind.data.frame(false.false = rep(NA,length(prop)), false.true= rep(NA,length(prop)), true.true= rep(NA,length(prop)),
                             false.false.2 = rep(NA,length(prop)), false.true.2= rep(NA,length(prop)), true.true.2= rep(NA,length(prop)))
end.match.other1 = cbind.data.frame(false.false = rep(NA,length(prop)), false.true= rep(NA,length(prop)), true.true= rep(NA,length(prop)),
                                   false.false.2 = rep(NA,length(prop)), false.true.2= rep(NA,length(prop)), true.true.2= rep(NA,length(prop)))
for(i in 1:length(prop)) {
  #near endemics, only TRUE and FALSE
  occ.end = endemismo1$nivel[!is.na(endemismo1$domain.reflora)]>=prop[i]
  end.match1$false.false[i] = sum(!reflora.af & !occ.end)
  end.match1$false.true[i] = sum(!reflora.af & occ.end | reflora.af & !occ.end)
  end.match1$true.true[i] = sum(reflora.af & occ.end)
  #near endemics, TRUE, FALSE and _TRANSITION
  occ.end.2 = endemismo1$nivel2[!is.na(endemismo1$domain.reflora)]>=prop[i]
  end.match1$false.false.2[i] = sum(!reflora.af & !occ.end.2)
  end.match1$false.true.2[i] = sum(!reflora.af & occ.end.2 | reflora.af & !occ.end.2)
  end.match1$true.true.2[i] = sum(reflora.af & occ.end.2)
  #ocasional species, only TRUE and FALSE
  occ.end1 = endemismo1$nivel[!is.na(endemismo1$domain.reflora)]<prop[i]
  end.match.other1$false.false[i] = sum(!reflora.other & !occ.end1)
  end.match.other1$false.true[i] = sum(!reflora.other & occ.end1 | reflora.other & !occ.end1)
  end.match.other1$true.true[i] = sum(reflora.other & occ.end1)
  #ocasional species, TRUE, FALSE and _TRANSITION
  occ.end1.2 = endemismo1$nivel2[!is.na(endemismo1$domain.reflora)]<prop[i]
  end.match.other1$false.false.2[i] = sum(!reflora.other & !occ.end1.2)
  end.match.other1$false.true.2[i] = sum(!reflora.other & occ.end1.2 | reflora.other & !occ.end1.2)
  end.match.other1$true.true.2[i] = sum(reflora.other & occ.end1.2)
}
row.names(end.match1) = prop; row.names(end.match.other1) = prop 

# par(mfrow=c(1,2),mar=c(4,4,2,1))
# plot(end.match1$false.true ~ prop, col="red", pch=19, ylim=c(0,4500), xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# points(end.match1$true.true ~ prop, col="green", pch=19, xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# points(end.match1$false.false ~ prop, col="blue", pch=19, xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# plot(end.match.other1$false.true ~ prop, col="red", ylim=c(0,4500),pch=19, xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# points(end.match.other1$true.true ~ prop, col="green", pch=19, xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# points(end.match.other1$false.false ~ prop, col="blue", pch=19, xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# 
# plot(end.match1$false.true.2 ~ prop, col="red", pch=19, ylim=c(0,4500), xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# points(end.match1$true.true.2 ~ prop, col="green", pch=19, xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# points(end.match1$false.false.2 ~ prop, col="blue", pch=19, xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# plot(end.match.other1$false.true.2 ~ prop, col="red", ylim=c(0,4500),pch=19, xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# points(end.match.other1$true.true.2 ~ prop, col="green", pch=19, xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")
# points(end.match.other1$false.false.2 ~ prop, col="blue", pch=19, xlab= "% of occurrences inside", ylab= "No. of Matches/Mismacthes")

# Finding the lowest point of the mismatches
# sspl <- smooth.spline(prop, end.match1$false.true, df = 13)
# ispl <- interpSpline(prop, end.match1$false.true)
# fspl <- splinefun(prop, end.match1$false.true)
# sspl1 <- smooth.spline(prop, end.match.other1$false.true, df = 13)
# ispl1 <- interpSpline(prop, end.match.other1$false.true)
# fspl1 <- splinefun(prop, end.match.other1$false.true)

### Using piece-wise regression to identify the breaking points in the endemism level
false.true = end.match1$false.true[prop>=50]
false.true2 = end.match1$false.true.2[prop>=50]
false.true1 = end.match.other1$false.true[prop<50]
false.true1.2 = end.match.other1$false.true.2[prop<50]
prop1 = prop[prop>=50]
prop2 = prop[prop<50]

## Near endemics break, only TRUE and FALSE occurrrences in the AF
tmp1 = lm(false.true ~ prop1)
# Set X breakpoints: Set psi=NA and K=n:
tmp2 = segmented(tmp1, seg.Z = ~prop1, psi=c(70,90), control=seg.control(display=FALSE, K=3)) ## Piecewise Regression METHOD 1
tmp3 = segmented(tmp1, seg.Z = ~prop1, psi=c(70,90,95), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
tmp4 = segmented(tmp1, seg.Z = ~prop1, psi=c(60,70,90,95), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
#slope(tmp4)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp1,tmp2,tmp3,tmp4)
brk.near.nivel = tmp4
## Near endemics break, TRUE, FALSE and _TRANSITION occurrrences in the AF
tmp1 = lm(false.true2 ~ prop1)
# Set X breakpoints: Set psi=NA and K=n:
tmp2 = segmented(tmp1, seg.Z = ~prop1, psi=c(70,90), control=seg.control(display=FALSE, K=3)) ## Piecewise Regression METHOD 1
tmp3 = segmented(tmp1, seg.Z = ~prop1, psi=c(70,90,95), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
tmp4 = segmented(tmp1, seg.Z = ~prop1, psi=c(65,80,89,96), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
#slope(tmp4)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp1,tmp2,tmp3,tmp4)
brk.near.nivel2 = tmp4

## Occasional species, only TRUE and FALSE occurrrences in the AF
tmp1.1 = lm(false.true1 ~ prop2)
# Set X breakpoints: Set psi=NA and K=n:
tmp2.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(5,15), control=seg.control(display=FALSE, K=3)) ## Piecewise Regression METHOD 1
tmp3.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(5,15,20), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
tmp4.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(1,10,16,32), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
#slope(tmp4.1)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp1.1,tmp2.1,tmp3.1,tmp4.1)
brk.ocas.nivel = tmp4.1

## Occasional species, TRUE, FALSE and _TRANSIITON occurrrences in the AF
tmp1.1 = lm(false.true1.2 ~ prop2)
# Set X breakpoints: Set psi=NA and K=n:
tmp2.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(5,15), control=seg.control(display=FALSE, K=3)) ## Piecewise Regression METHOD 1
tmp3.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(5,15,25), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
tmp4.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(3.3,9.2,18.9,29.3), control=seg.control(display=FALSE, it.max=1500, n.boot=50)) ## Piecewise Regression METHOD 1
#tmp5.1 = segmented(tmp1.1, seg.Z = ~prop2, psi=c(3,9,18,25,32), control=seg.control(display=FALSE, it.max=1500, n.boot=50))
#slope(tmp4.1)  # defaults to confidence level of 0.95 (conf.level=0.95)
AICtab(tmp1.1,tmp2.1,tmp3.1,tmp4.1)
brk.ocas.nivel2 = tmp4.1


###############################################################################
#### PERFOMING THE CLASSIFICATION OF SPECIES USING THE ESTIMATED THRESHOLD ####
###############################################################################

#### CHECAR AQUI: VER SE TODOS OS BREAKS ESTAO BEM INDEXADOS!!! ####

## Getting the list of pure endemics, near-endemics, widespread and occasional species
## TRUE taxonomy
endemismo$end.group = NA # TRUE and FALSE geocheck
endemismo$end.group[endemismo$nivel >= 100] = "pure"
endemismo$end.group[endemismo$nivel <= confint(brk0.ocas.nivel)$prop[2,1]] = "occ"
endemismo$end.group[is.na(endemismo$end.group) & endemismo$nivel >= confint(brk0.near.nivel)$prop[3,1]] = "near"
endemismo$end.group[is.na(endemismo$end.group)] = "wide"
endemismo$end.group2 = NA # TRUE, FALSE and _TRANSITION geocheck
endemismo$end.group2[endemismo$nivel2 >= 100] = "pure"
endemismo$end.group2[endemismo$nivel2 <= confint(brk0.ocas.nivel2)$prop[2,1]] = "occ"
endemismo$end.group2[is.na(endemismo$end.group2) & endemismo$nivel2 >= confint(brk0.near.nivel2)$prop[3,1]] = "near"
endemismo$end.group2[is.na(endemismo$end.group2)] = "wide"
endemismo = endemismo[endemismo$spp %in% spp.af$species.reflora,]
table(endemismo$end.group)
table(endemismo$end.group2)
100*table(endemismo$end.group2)/sum(table(endemismo$end.group2))

## TRUE AND PROBABLE taxonomy
endemismo1$end.group = NA # TRUE and FALSE geocheck
endemismo1$end.group[endemismo1$nivel >= 100] = "pure"
endemismo1$end.group[endemismo1$nivel <= confint(brk.ocas.nivel)$prop[2,1]] = "occ"
endemismo1$end.group[is.na(endemismo1$end.group) & endemismo1$nivel >= confint(brk.near.nivel)$prop[3,1]] = "near"
endemismo1$end.group[is.na(endemismo1$end.group)] = "wide"
endemismo1$end.group2 = NA # TRUE, FALSE and _TRANSITION geocheck
endemismo1$end.group2[endemismo1$nivel2 >= 100] = "pure"
endemismo1$end.group2[endemismo1$nivel2 <= confint(brk.ocas.nivel2)$prop[2,1]] = "occ"
endemismo1$end.group2[is.na(endemismo1$end.group2) & endemismo1$nivel2 >= confint(brk.near.nivel2)$prop[3,1]] = "near"
endemismo1$end.group2[is.na(endemismo1$end.group2)] = "wide"
endemismo1 = endemismo1[endemismo1$spp %in% spp.af$species.reflora,]
table(endemismo1$end.group)
table(endemismo1$end.group2)
100*table(endemismo1$end.group2)/sum(table(endemismo1$end.group2))

##Saving the table
saveRDS(endemismo,"endemismo.rds")
saveRDS(endemismo1,"endemismo1.rds")

##############################################################################################################################################################################################H
##############################################################################################################################################################################################H
#### SUMMARIZING AND PLOTTING THE RESULTS ####
##############################################

## How many occurrences used in the endemism level analysis?
sum(endemismo1[,c("outside","out.in","in.out","inside")]) # true and probable taxonomy
sum(endemismo[,c("outside","out.in","in.out","inside")]) # true taxonomy

## Working limit to define near-endemics and occasional species
confint(brk.near.nivel2)
confint(brk.ocas.nivel2)

confint(brk0.near.nivel2)
confint(brk0.ocas.nivel2)

## How many pure endemics?
table(endemismo1$end.group2);table(endemismo1$end.group2)/sum(table(endemismo1$end.group2))*100 # true and probable taxonomy 
table(endemismo$end.group2);table(endemismo$end.group2)/sum(table(endemismo$end.group2))*100 # only true taxonomy

## which are the families with higher mean endemism level?
tmp = cbind.data.frame(aggregate(endemismo1$nivel2, list(endemismo1$family.reflora), length),
      aggregate(endemismo1$nivel2, list(endemismo1$family.reflora), summary)$x)
tmp$Mean.w = NA
for (i in 1:dim(tmp)[1]) {
  familia = tmp$Group.1[i]
  dados = endemismo1[endemismo1$family.reflora == familia,]
  tmp$Mean.w[i] = weighted.mean(dados$nivel2, dados$total1)
}
tmp = tmp[order(tmp$Mean, decreasing = TRUE),]
head(tmp[tmp$x>=15,],11)
paste(head(tmp[tmp$x>=15,],11)$Group.1," (",round(head(tmp[tmp$x>=15,],11)$Mean,1),"%)",sep="")

## How many endemic species per 100 km2
path = "E://ownCloud//W_GIS"
af = readOGR(dsn=paste(path,"//AF_limites_milton",sep=""),layer="merge_limites_MA11428_TNC_ARG_BR_PAR")
af.proj = spTransform(af, CRS("+init=epsg:5641"))
gArea(af.proj)

#AF area total = 1,632,266 km2
#AF area remaining = 226124 km2
sum(table(endemismo1$end.group2)[c("near","pure")])/16322.66 #100 km2, original
sum(table(endemismo1$end.group2)[c("near","pure")])/2261.24 #100 km2, remaining

## How many non-duplicated specimens (i.e. dim(oc.data.af)[1]) per km2
dim(oc.data.af.spp1[af.check2 %in% TRUE])[1]
677065/1632266 # km2, original
677065/226124 # km2, remaining

## How many new trees for the Atlantic Forest not in the FB 2020?
tmp = spp.af[af.check2 %in% "TRUE" & spp.group %in% c("valid","valid_not_in_reflora","valid_TBC"),]
names(tmp)[grep("domain.reflora",names(tmp))] = paste(names(tmp)[grep("domain.reflora",names(tmp))],c("",1),sep="")
tmp = merge(tmp, endemismo1[,c("spp","end.group2")], by.x = "species.correct2", by.y = "spp", all.x=TRUE)
dim(tmp)[1] ## checklist: before 5204 spp; then 5159 spp; now: 5079 spp
table(tmp$habito)[c("1")] ## trees in the checklist: before 4189 spp; then 4143 spp; 4073 spp
tmp1 = tmp[!grepl("AtlanticForest",tmp$domain.reflora) | is.na(tmp$domain.reflora),]

## Where these plants come from?
tmp2 = table(tmp1$domain.reflora)
sum(tmp2[grepl("Cerrado",names(tmp2))])/sum(tmp2)
sum(tmp2[grepl("Amazonia",names(tmp2))])/sum(tmp2)
sum(tmp2[grepl("Caatinga",names(tmp2))])/sum(tmp2)
sum(tmp2[grepl("Pantanal",names(tmp2))])/sum(tmp2)
sum(tmp2[grepl("Pampa",names(tmp2))])/sum(tmp2)

## What groups they belong to?
100*table(tmp1$end.group2, tmp1$habito)[,"1"]/sum(table(tmp1$end.group2, tmp1$habito)[,"1"])

## How many endemic species are threatened?


##################
#### FIGURE 1 ####
##################
jpeg(filename = "Figure1.jpg", width = 3100*2, height = 3100, units = "px", pointsize = 12,
     res = 600, family = "sans", type="cairo", bg="white")
## TRUE AND PROBABLE taxonomy, only TRUE, FALSE and _TRANSITION geocheck
# Number of mismatches ~ prop
par(mfrow=c(1,2),mar=c(3,4,1,0.5),mgp=c(2.1,0.3,0), tcl=-.25, las=1)
plot(end.match1$false.true.2 ~ prop, col="black", pch=19, ylim=c(640,2800), 
     xlab= "Endemism level (%)", ylab= "Number of mismatches", cex=1.1)
points(end.match.other1$false.true.2 ~ prop, col="black", pch=25, ylim=c(0,3000))
abline(v=confint(brk.near.nivel2)$prop[3,1],lty=3) ## upper CI of the estimate for the best model: 88%
#abline(v=confint(brk.near.nivel2)$prop[2,1],lty=3, col="darkgreen") ## upper CI of the estimate for the best model: 88%
#plot(brk.near.nivel2, add=TRUE)
text(x=82,y=1000,label=paste(round(confint(brk.near.nivel2)$prop[3,1],0),"%",sep=""))
abline(v=confint(brk.ocas.nivel2)$prop[2,1],lty=3) ## upper CI of the estimate for the best model: 88%
#abline(v=confint(brk.ocas.nivel2)$prop[2,1],lty=3) ## upper CI of the estimate for the best model: 88%
text(x=15,y=1000,label=paste(round(confint(brk.ocas.nivel2)$prop[2,1],0),"%",sep=""))
#text(x=12.5,y=1000,label=paste(round(confint(brk.ocas.nivel2)$prop[2,1],0),"%",sep=""))
legend("topleft",expression(bold((a))), cex=1.5, bty="n",inset=c(-0.05,-0.02))
legend("top",c("Occasional species","Endemic species"), pch=c(25,19),cex=1.1, bty="n",inset=c(-0.09,0.01))
#plot(brk.ocas.nivel2, add=T)

#derivate ~ prop : curve peaks or valleys
# plot(fspl(prop, deriv = 1) ~ prop, col="darkgreen", pch=21, cex=1.1, 
#      ylab="Derivate", xlab="% of occurrences inside"); abline(h=0,lty=2)
# points(fspl1(prop, deriv = 1) ~ prop, col="red", pch=25); abline(h=0,lty=2)
# abline(v=confint(brk.near.nivel)$prop[3,1],lty=3, col="darkgreen") ## upper CI of the estimate for the best model: 88%
# text(x=79,y=75,label=paste(round(confint(brk.near.nivel)$prop[3,1],0),"%",sep=""),col="darkgreen")
# abline(v=confint(brk.ocas.nivel)$prop[3,1],lty=3, col="red") ## upper CI of the estimate for the best model: 88%
# text(x=25,y=75,label=paste(round(confint(brk.ocas.nivel)$prop[3,1],0),"%",sep=""),col="red")

#endemism ~ prop : what is the best working definition of endemism for the AF (true and near)?
plot(end.prop1.2 ~ prop, xlab= "Endemism level (%)", ylab= "Biome endemism ratio (%)", pch=19)
abline(v=confint(brk.near.nivel2)$prop[3,1],lty=2)
abline(v=confint(brk.ocas.nivel2)$prop[2,1],lty=2)
(endemismo.af = 100*sum(endemismo1$nivel2>=confint(brk.near.nivel2)$prop[3,1], na.rm=TRUE)/dim(endemismo1[!is.na(endemismo1$nivel2),])[1])
(transient.af = 100 - 100*sum(endemismo1$nivel2<confint(brk.ocas.nivel2)$prop[2,1], na.rm=TRUE)/dim(endemismo1[!is.na(endemismo1$nivel2),])[1])
abline(h=endemismo.af,lty=2) 
abline(h=transient.af,lty=2)
arrows(67,37,85,37, length=0.08); text(50,38.5,"Near-endemic"); text(50,36,"species")
arrows(67,30.5,95,30.5, length=0.08); text(50,32.5,"Pure endemic"); text(50,30,"species")
arrows(37,93,15,93, length=0.08); text(50,94.5,"Occasional"); text(50,92,"species")
text(50,56,"Widespread");text(50,53.5,"species")
lines(x=0,y=100*sum(endemismo1$nivel2>=confint(brk.near.nivel2)$prop[3,1], na.rm=TRUE)/dim(endemismo1[!is.na(endemismo1$nivel),])[1],lwd=2,col="darkgreen",type="l")
arrows(x0=confint(brk.near.nivel2)$prop[3,1],y0=0,
       y1=transient.af,
       lwd=2, length=0)#, col="darkgreen")
arrows(x0=confint(brk.ocas.nivel2)$prop[2,1],x1=110,
       y0=endemismo.af,
       lwd=2, length=0)#, col="darkgreen")
arrows(x0=confint(brk.ocas.nivel2)$prop[2,1],y0=110,
       y1=endemismo.af,
       lwd=2, length=0)#, col="red")
arrows(x1=confint(brk.near.nivel2)$prop[3,1],x0=-10,
       y0=transient.af,
       lwd=2, length=0)#, col="red")
legend("topleft",expression(bold((b))), cex=1.5, bty="n",inset=c(-0.05,-0.02))
dev.off()

#### FIGURE S3 ####
## Only TRUE taxonomy, only TRUE, FALSE and _TRANSITION geocheck
# Number of mismatches ~ prop
jpeg(filename = "FigureS3.jpg", width = 3100*2, height = 3100, units = "px", pointsize = 12,
     res = 600, family = "sans", type="cairo", bg="white")
par(mfrow=c(1,2),mar=c(3,4,1,0.5),mgp=c(2.1,0.3,0), tcl=-.25, las=1)
plot(end.match$false.true.2 ~ prop, col="darkgreen", pch=19, ylim=c(640,2800), 
     xlab= "Endemism level (%)", ylab= "Number of mismatches", cex=1.1)
points(end.match.other$false.true.2 ~ prop, col="red", pch=25, ylim=c(0,3000))

abline(v=confint(brk0.near.nivel2)$prop[3,1],lty=3, col="darkgreen") ## upper CI of the estimate for the best model: 88%
text(x=81,y=1000,label=paste(round(confint(brk0.near.nivel2)$prop[3,1],0),"%",sep=""),col="darkgreen")
abline(v=confint(brk0.ocas.nivel2)$prop[2,1],lty=3, col="red") ## upper CI of the estimate for the best model: 88%
#abline(v=confint(brk0.ocas.nivel2)$prop[2,1],lty=3, col="red") ## upper CI of the estimate for the best model: 88%
text(x=17,y=1000,label=paste(round(confint(brk0.ocas.nivel2)$prop[2,1],0),"%",sep=""),col="red")
legend("topleft",expression(bold((a))), cex=1.5, bty="n",inset=c(-0.05,-0.02))
legend("top",c("Occasional species","Endemic species"), pch=c(25,19),cex=1.1, col=c("red","darkgreen"),bty="n",inset=c(-0.09,0.01))

#endemism ~ prop : what is the best working definition of endemism for the AF (true and near)?
plot(end.prop.2 ~ prop, xlab= "Endemism level (%)", ylab= "Biome endemism ratio (%)", pch=19)
abline(v=confint(brk0.near.nivel2)$prop[3,1],lty=2,col="darkgreen")
abline(v=confint(brk0.ocas.nivel2)$prop[2,1],lty=2,col="red")
(endemismo.af = 100*sum(endemismo$nivel2>=confint(brk0.near.nivel2)$prop[3,1], na.rm=TRUE)/dim(endemismo[!is.na(endemismo$nivel2),])[1])
(transient.af = 100 - 100*sum(endemismo$nivel2<confint(brk0.ocas.nivel2)$prop[2,1], na.rm=TRUE)/dim(endemismo[!is.na(endemismo$nivel2),])[1])
abline(h=endemismo.af,lty=2,col="darkgreen") 
abline(h=transient.af,lty=2,col="red")
arrows(67,39,85,39, length=0.08); text(50,40,"Near-endemic"); text(50,37.5,"species")
arrows(67,32,95,32, length=0.08); text(50,34,"Pure endemic"); text(50,31.5,"species")
arrows(37,93,17,93, length=0.08); text(50,94,"Occasional"); text(50,91.5,"species")
text(50,55.5,"Widespread");text(50,53,"species")
lines(x=0,y=100*sum(endemismo$nivel2>=confint(brk0.near.nivel2)$prop[3,1], na.rm=TRUE)/dim(endemismo[!is.na(endemismo$nivel),])[1],lwd=2,col="darkgreen",type="l")
arrows(x0=confint(brk0.near.nivel2)$prop[3,1],y0=0,
       y1=transient.af,
       lwd=2, length=0, col="darkgreen")
arrows(x0=confint(brk0.ocas.nivel2)$prop[2,1],x1=110,
       y0=endemismo.af,
       lwd=2, length=0, col="darkgreen")
arrows(x0=confint(brk0.ocas.nivel2)$prop[2,1],y0=110,
       y1=endemismo.af,
       lwd=2, length=0, col="red")
arrows(x1=confint(brk0.near.nivel2)$prop[3,1],x0=-10,
       y0=transient.af,
       lwd=2, length=0, col="red")
legend("topleft",expression(bold((b))), cex=1.5, bty="n",inset=c(-0.05,-0.02))
dev.off()


####################
#### APPENDIX 1 ####
####################
## Sources of information
# use the file with specimens inside and outside (i.e oc.data.af.spp)

#information from the herbaria
splink = read.csv('speciesLink.csv', as.is=TRUE, na.strings = c(""," ",NA))
splink$collectionCode = toupper(gsub("_herbario|-herbario|-agp|_fanerogamas","",splink$acronym))
jabot = read.csv('jabot.list.csv', as.is=TRUE, na.strings = c(""," ",NA))
jabot$collectionCode = gsub("JABOT","RB",jabot$acronym)
jabot$collectionCode = gsub("HERBARIUMR","R",jabot$acronym)
jabot = jabot[!jabot$collectionCode %in% splink$collectionCode,]
herbs = read.csv('collectionCodes.csv', as.is=TRUE, na.strings = c(""," ",NA))
herbs = herbs[!duplicated(herbs$index.herbariorum.or.working.code),]
herbs = herbs[!herbs$index.herbariorum.or.working.code %in% unique(c(splink$collectionCode, jabot$collectionCode)),]

## Final number of occurrences consulted per collection (overall)
tmp = table(oc.data.af.spp$collectionCode)
tmp1= data.frame(collectionCode = names(tmp), records.all = as.double(tmp))
## Final number of occurrences inside the AF per collection
tmp = table(oc.data.af.spp[af.check2 %in% "TRUE",.(collectionCode)])
tmp2= data.frame(collectionCode = names(tmp), records.af = as.double(tmp))
tmp2= merge(tmp1, tmp2, by="collectionCode", all.x=TRUE)

## Merging collection info with the numbers of collections
splink1= merge(splink, tmp2, by="collectionCode", all.y=TRUE)[,c("collectionCode","name","pub.date","version","citation","rights","gbif_UUID","records.all","records.af")]
jabot1= merge(jabot, tmp2, by="collectionCode", all.y=TRUE)
table(splink1$collectionCode == jabot1$collectionCode)
splink1[is.na(splink1$name) & !is.na(jabot1$name), c("name","pub.date","citation")] =
  jabot1[is.na(splink1$name) & !is.na(jabot1$name), c("name","download.date","citation")]
herbs1 = merge(herbs, tmp2, by.x="index.herbariorum.or.working.code", by.y="collectionCode", all.y=TRUE)
herbs2 = herbs1[,colnames(herbs1) %in% c("index.herbariorum.or.working.code","name","version","pub.date","citation","license","rights","records.all","records.af","datasetkey","rightsholder","organization","section")]
table(splink1$collectionCode == herbs2$index.herbariorum.or.working.code)
herbs2[!is.na(splink1$name) & !is.na(splink1$gbif_UUID), c("name","pub.date","citation","license","datasetkey")] =
  splink1[!is.na(splink1$name) & !is.na(splink1$gbif_UUID), c("name","pub.date","citation","rights","gbif_UUID")]
herbs2[!is.na(splink1$name) & is.na(herbs2$name), c("name","pub.date","citation")] =
  splink1[!is.na(splink1$name) & is.na(herbs2$name), c("name","pub.date","citation")]

## Final edits of herbarium information
herbs2[is.na(herbs2$name) & !is.na(herbs2$section) & !herbs2$section %in% c("Herbario","Herbário","Herbari","Herbarium","Herbariun","Herbier","Herbaria"),c("name")] = 
  herbs2[is.na(herbs2$name) & !is.na(herbs2$section) & !herbs2$section %in% c("Herbario","Herbário","Herbari","Herbarium","Herbariun","Herbier","Herbaria"),c("section")]
herbs2[is.na(herbs2$name) & !is.na(herbs2$section) & !is.na(herbs2$organization) & herbs2$section %in% c("Herbario","Herbário","Herbari","Herbarium","Herbariun","Herbier","Herbaria"),c("name")] =
  paste("Herbarium of the ", herbs2[is.na(herbs2$name) & !is.na(herbs2$section) & !is.na(herbs2$organization) & herbs2$section %in% c("Herbario","Herbário","Herbari","Herbarium","Herbariun","Herbier","Herbaria"),c("organization")],sep="")
herbs2$rightsholder[is.na(herbs2$rightsholder) & !is.na(herbs2$organization)] = 
  herbs2$organization[is.na(herbs2$rightsholder) & !is.na(herbs2$organization)]
herbs2$citation[is.na(herbs2$citation) & !herbs2$index.herbariorum.or.working.code %in% unique(c(splink$collectionCode, jabot$collectionCode))] = 
  "GBIF.org (11 December 2019) GBIF Occurrence Download https://doi.org/10.15468/dl.mzmat2"
herbs2$citation[is.na(herbs2$citation) & herbs2$index.herbariorum.or.working.code %in% unique(jabot$collectionCode)] = 
  "JBRJ - Instituto de Pesquisas Jardim Botânico do Rio de Janeiro. Jabot - Banco de Dados da Flora Brasileira. Available at: [http://jabot.jbrj.gov.br/]. Accessed in 06/06/2019"
herbs2$pub.date[is.na(herbs2$pub.date) & !herbs2$index.herbariorum.or.working.code %in% unique(c(splink$collectionCode, jabot$collectionCode))] = "11/12/2019"  
herbs2$license = gsub("Creative Commons Attribution Non Commercial \\(CC-BY-NC\\) 4\\.0 License","CC_BY_NC_4_0",herbs2$license)
herbs3 = herbs2[,!colnames(herbs2) %in% c("organization","section")]
names(herbs3)[1] = "collectionCode"
names(herbs3)[grepl("name",names(herbs3))] = "collectionName"
names(herbs3)[grepl("pub.date",names(herbs3))] = "publication or download date"
names(herbs3)[grepl("rightsholder",names(herbs3))] = "rightsHolder or institutionName"
names(herbs3)[grepl("datasetkey",names(herbs3))] = "datasetKey"
names(herbs3)[grepl("datasetkey",names(herbs3))] = "datasetKey"
names(herbs3)[grepl("records.all",names(herbs3))] = "All records"
names(herbs3)[grepl("records.af",names(herbs3))] = "Atlantic Forest records"
head(herbs3)

sum(herbs3$`All records`)
sum(herbs3$`Atlantic Forest records`, na.rm = TRUE)

#Saving
write.csv(herbs3, "records_Appendix1.csv", row.names = FALSE)


####################
#### APPENDIX 2 ####
####################
## List of taxonomists per family
autores = read.csv('autores.csv', as.is=TRUE, na.strings = c(""," ",NA))
autores <- autores[,c("source","family","full.name1","tdwg.name")]
autores <- autores[!is.na(autores$tdwg.name),]
autores <- autores[!is.na(autores$family),]
autores <- autores[!grepl('\\?|,',autores$family),]
autores <- autores[!grepl('Floristics/Generalist \\(all families\\)|Wood anatomist', autores$family),]
autores$source[grepl("wikipedia|botanicaargentina",autores$source)] = "wikipedia"
autores$source[grepl("renato|vinicius",autores$source)] = "herbaria"

#Saving
write.csv(autores, "autores_Appendix2.csv", row.names = FALSE)

####################
#### APPENDIX 3 ####
####################
## The Atlantic Forest checklist: only valid names that are confimed (tax and geo validated)

## Reading the Atlantic Forest overall list of species
spp.all = readRDS('spp.all.rds')

##Vouchers: colector.name, colector.number (collections of deposit) 
checklist = spp.all
checklist = checklist[,c("family.reflora","scientific.name.reflora","habito","VCS.jan2019..nome.","VCS.jan2019..habito.","VCS.jan2019..establishment.",
                         "spp.group","TBC","domain.reflora","species.correct2","checagem_VCS","TreeCo_status","af.check","af.check2",
                         "establishment.AF","establishment.BR")]
checklist = checklist[!duplicated(checklist$scientific.name),]
checklist = checklist[order(checklist$scientific.name.reflora),]
checklist = checklist[order(checklist$family.reflora),]
checklist$records = NA
checklist$vouchers = NA
checklist$loc.correct = NA
checklist$locality = NA
checklist$notes = NA

##Reading the data
dados <- data.table(readRDS('oc.data.af.spp.geo.tax1.rds'))
#dados <- data.table(oc.data.af.spp.geo.tax[tax.check1 %like% TRUE])
#dados <- data.table(dados[af.check2 %in% "TRUE"])
dados[collectionCode == "R-TIPOS", collectionCode := "R"]
dados[numTombo %like% "r-tipos_", numTombo := gsub('r-tipos_',"r_", numTombo)]
dados[dup.ID %like% "r-tipos_", dup.ID := gsub('r-tipos_',"r_", dup.ID)]
unique(dados[catalogNumber %like% "^[A-Z]", .(collectionCode)])
dados[catalogNumber %like% "^[A-Z]", catalogNumber := gsub('[a-z]|_',"", numTombo)]
setkeyv(dados, "species.correct2")

##Getting the vouchers for each species
for(i in 1:dim(checklist)[1]) {
  ##Filtering the data
  sp = checklist$species.correct2[i]
  occs <- dados[sp]
  ##Number of valid occurrences inside the Atlantic Forest
  checklist$records[i] = dim(occs)[1]
  coletores = paste(occs$coletor.name, occs$coletor.number, sep=", ")
  accessions = paste(occs$collectionCode, occs$catalogNumber, sep=" ")
  ##Correcting accessions for duplicates across herbaria
  if(any(!is.na(occs$dup.ID))) {
    tmp = occs$dup.ID[!is.na(occs$dup.ID)]
    tmp = toupper(gsub("_"," ",tmp))
    tmp = sapply(sapply(strsplit(tmp,"\\|"), unique), paste, collapse="|")
    tmp = gsub("\\|",", ",tmp)
    accessions[!is.na(occs$dup.ID)] = tmp
  }
  vchrs = paste(coletores,paste("(",accessions,")",sep=""), sep=" ")
  ##Too many vouchers?
  types= !is.na(occs$typeStatus) & !grepl("not a type|notatype|probable type|tipo provavel|tipo prov?vel",occs$typeStatus,ignore.case = TRUE)
  if(sum(types)<11) {
    if(length(vchrs)>30) {
      # chosing vouchers in respect to the completeness of the label information
      keep = rep(TRUE,length(vchrs))
      if (any(occs$coletor.number == "s.n.") & any(occs$coletor.number != "s.n.")) keep[occs$coletor.number == "s.n." & is.na(occs$typeStatus)] = FALSE
      if (any(grepl("\\d",occs$coletor.number))) keep[grepl("\\D",occs$coletor.number) & is.na(occs$typeStatus)] = FALSE
      if (any(nchar(occs$coletor.name)<3)) keep[nchar(occs$coletor.name)<3] = FALSE
      # chosing vouchers in respect to their locality
      estado = occs$loc.correct
      estado[estado == "no_loc"] = NA
      estado[!grepl("_",estado)] = NA
      estado = sapply(strsplit(estado,"_"), function(x) x[2])
      if(length(unique(estado,incomparables = NA))>1) {
        keep1 = !duplicated(estado, incomparables = NA)
        keep2 = !duplicated(estado, incomparables = NA, fromLast = TRUE)
      } else {
        keep1 = rep(TRUE,length(estado))
        keep2 = rep(TRUE,length(estado))
      }
      #filtering the chosen vouchers
      if(any(keep & keep1) | any(keep & keep2)) {
        vchrs1 = vchrs[(keep & keep1) | (keep & keep2)]
        nomes = occs$coletor.name[(keep & keep1) | (keep & keep2)]
        tax = occs$tax.check[(keep & keep1) | (keep & keep2)]
        af = occs$af.check[(keep & keep1) | (keep & keep2)]
      } else {
        vchrs1 = vchrs[keep]
        nomes = occs$coletor.name[keep]
        tax = occs$tax.check[keep]
        af = occs$af.check[keep]
      }  
      ##Still too many vouchers?
      if(length(vchrs1)>30) vchrs1 = vchrs1[!duplicated(nomes) & tax == TRUE & af == "TRUE"]
      
      ##Making sure types are included
      vchrs2 = head(unique(c(vchrs[types], vchrs1)), 30)
    } else { vchrs2 = head(unique(c(vchrs[types], vchrs)), 30)  }
  } else {  
    vchrs2 = vchrs[types]
  
    if(dim(occs)[1]>length(vchrs2)) { 
      vchrs2 = head(unique(c(vchrs2, vchrs[!types])), 30)
    } else {
      vchrs2 = vchrs2
    }
  }
  checklist$vouchers[i] = paste(sort(vchrs2), collapse= "; ",sep="")
  cat(i,'\n')
  
  if(dim(occs)[1] == 1) checklist$loc.correct[i] = occs$loc.correct
  if(dim(occs)[1] == 2) checklist$loc.correct[i] = paste(occs$loc.correct, collapse= "; ",sep="")
  
  if(dim(occs)[1] == 1) checklist$locality[i] = occs$locality
  if(dim(occs)[1] == 2) checklist$locality[i] = paste(occs$locality, collapse= "; ",sep="")
  
  if(dim(occs)[1] == 1) checklist$notes[i] = occs$occurrenceRemarks
  if(dim(occs)[1] == 2) checklist$notes[i] = paste(occs$occurrenceRemarks, collapse= "; ",sep="")
}

##Getting the cultivated species
uso = read.csv("C://Users//renato//Documents//raflima//Pos Doc//Databases//Species Uses//plant_uses.csv", na.strings= c(""," ",NA))
uso = uso[as.character(uso$Name_submitted) %in% checklist$species.correct2,]
uso = uso[as.character(uso$group_renato) %in% "cultivated",]
checklist$cultivated = NA
checklist$cultivated[checklist$species.correct2 %in% as.character(uso$Name_submitted)] = "cultivated"

##Filtering the columns of interest
checklist = checklist[,c("family.reflora","scientific.name.reflora","habito","VCS.jan2019..nome.","VCS.jan2019..habito.","VCS.jan2019..establishment.",
                         "spp.group","domain.reflora","TBC","cultivated","records","vouchers","loc.correct","locality","notes",
                         "species.correct2","checagem_VCS","TreeCo_status","af.check","af.check2","establishment.AF","establishment.BR")]
names(checklist) = c("family","scientific.name","life.form","nome_VCS","habito_VCS","establ_VCS",
                     "BF.2020","domain.reflora","TBC","cultivated","records","vouchers","loc.correct","locality","notes",
                     "species.correct2","checagem_VCS","TreeCo_status","af.check","af.check2","establishment.AF","establishment.BR")

#Replace encoding problems in author`s names
checklist$scientific.name = gsub("Allem\\?\\?o","Allemao",checklist$scientific.name)
checklist$scientific.name = gsub("Lobão|Lob\\?\\?o","Lobao",checklist$scientific.name)
checklist$scientific.name = gsub("M\\?¦ll\\.Arg\\.","Mull.Arg.",checklist$scientific.name)
checklist$scientific.name = gsub("M\\?¦ll \\.Arg\\.","Mull.Arg.",checklist$scientific.name)
checklist$scientific.name = gsub("M\\?\\?ll \\.Arg\\.","Mull.Arg.",checklist$scientific.name)
checklist$scientific.name = gsub("MÃ¼ll\\. Arg\\.","Mull.Arg.",checklist$scientific.name)
checklist$scientific.name = gsub("M\\?ªll\\.Arg\\.","Mull.Arg.",checklist$scientific.name)
checklist$scientific.name = gsub("M\\?¦ll\\. Arg.","Mull.Arg.",checklist$scientific.name)
checklist$scientific.name = gsub("Dus\\?\\?n","Dusen",checklist$scientific.name)
checklist$scientific.name = gsub("Esp\\?\\?rito","Espirito",checklist$scientific.name)
checklist$scientific.name = gsub("Z\\?\\?chia","Zachia",checklist$scientific.name)
checklist$scientific.name = gsub("Piraj\\?\\?","Piraja",checklist$scientific.name)
checklist$scientific.name = gsub("G\\?¦rke","Gurke",checklist$scientific.name)
checklist$scientific.name = gsub("Joch\\.M\\?\\?ll\\.","Joch.Mull.",checklist$scientific.name)
checklist$scientific.name = gsub("Arr\\?\\?b\\.","Arrab.",checklist$scientific.name)
checklist$scientific.name = gsub("F\\?\\?e","Fee",checklist$scientific.name)
checklist$scientific.name = gsub("L\\?\\?onard","Leonard",checklist$scientific.name)
checklist$scientific.name = gsub("Garc\\?\\?a","Garcia",checklist$scientific.name)
checklist$scientific.name = gsub("Mendon??a","Mendonca",checklist$scientific.name)
checklist$scientific.name = gsub("Madri\\?\\?\\?\\?n","Madrinan",checklist$scientific.name)
checklist$scientific.name = gsub("AllemÃ£o","Allemao",checklist$scientific.name)
checklist$scientific.name = gsub("Gir.-Ca\\?\\?as","Gir.-Canas",checklist$scientific.name)
checklist$scientific.name = gsub("Sess\\?\\?","Sesse",checklist$scientific.name)
checklist$scientific.name = gsub("L\\?\\?rio","Lirio",checklist$scientific.name)
checklist$scientific.name = gsub("Tr\\?\\?cul","Trecul",checklist$scientific.name)
checklist$scientific.name = gsub("C\\.D\\.Bouch\\?\\?","C.D.Bouche",checklist$scientific.name)
checklist$scientific.name = gsub("Louren\\?\\?o","Lourenco",checklist$scientific.name)
checklist$scientific.name = gsub("B\\?¦nger","Bunger",checklist$scientific.name)
checklist$scientific.name = gsub("Lem\\?\\?e","Lemee",checklist$scientific.name)
checklist$scientific.name = gsub("Londo\\?\\?o","Londono",checklist$scientific.name)
checklist$scientific.name = gsub("D\\?\\?ll|DÃ¶ll","Doll",checklist$scientific.name)
checklist$scientific.name = gsub("B.St\\?\\?hl","B.Stahl",checklist$scientific.name)
checklist$scientific.name = gsub("?vila","Avila",checklist$scientific.name)
checklist$scientific.name = gsub("Ara\\?\\?jo","Araujo",checklist$scientific.name)
checklist$scientific.name = gsub("Aubr\\?\\?v","Aubrev",checklist$scientific.name)
checklist$scientific.name = gsub("M\\?\\?nico","Monico",checklist$scientific.name)
checklist$scientific.name = gsub("LafetÃ¡","Lafeta",checklist$scientific.name)
checklist$scientific.name = gsub("L\\'H\\?\\?r\\.","L'Her.",checklist$scientific.name)
checklist$scientific.name = gsub("CarriÃ¨re","Carriere",checklist$scientific.name)
checklist$scientific.name = gsub("Bid\\?\\?","Bida",checklist$scientific.name)
checklist$scientific.name = gsub("O\\?\\?\\?Leary","O'Leary",checklist$scientific.name)
checklist$scientific.name = gsub("Fran\\?\\?a","Franca",checklist$scientific.name)
checklist$scientific.name = gsub("Gon\\?\\?\\.","Gonc.",checklist$scientific.name)
checklist$scientific.name = gsub("Fraz\\?\\?o","Frazao",checklist$scientific.name)
checklist$scientific.name = gsub("Bouch\\?\\?","Bouche",checklist$scientific.name)

## Life forms
checklist$life.form = gsub("1","tree", checklist$life.form)
checklist$life.form = gsub("0.5","shrub", checklist$life.form)

## Missing collectors names
checklist$vouchers = gsub("Anonimo\\,|Anon\\.\\,|Unknown\\,|SemColetor\\,|Coletor\\, S\\.\\,|S\\/c\\,|s\\.coll\\.\\,|s\\.col\\.\\,|S\\.C\\.\\,|Unspecified\\,|Null\\,","Anonymous,", checklist$vouchers)
checklist$vouchers = gsub("Anonymous\\, 0","Anonymous, s.n.", checklist$vouchers)
checklist$vouchers = gsub("Anonymous\\, C\\.\\,","Anonymous,", checklist$vouchers)

## Final corrections
checklist$vouchers = gsub('\\"\\"\\"\\"','',checklist$vouchers)
checklist$vouchers = gsub('NA\\, NA \\(NA NA\\)', NA, checklist$vouchers)
checklist = checklist[!is.na(checklist$scientific.name),]

## Final filtering and edits
spp.af = readRDS('spp.af.rds')
tmp = spp.af[!spp.af$spp.group %in% c("valid_in_reflora"),]
tmp = tmp[tmp$af.check2 == "TRUE",]
checklist.af = checklist[checklist$species.correct2 %in% tmp$species.correct2]
checklist.af = checklist.af[!checklist.af$vouchers %in% "",]

#Final filtering
checklist.af$nome_VCS = gsub("vinicius_ok","ok", as.character(checklist.af$nome_VCS))
checklist.af$habito_VCS = gsub("vinicius_ok","ok", as.character(checklist.af$habito_VCS))
checklist.af$establ_VCS = gsub("vinicius_ok","ok", as.character(checklist.af$establ_VCS))
checklist.af$BF.2020 = gsub("^valid$|valid_TBC","yes", as.character(checklist.af$BF.2020))
checklist.af$BF.2020 = gsub("valid_not_in_reflora","new to BF.2020", as.character(checklist.af$BF.2020))
checklist.af$domain.reflora = grepl("AtlanticForest", checklist.af$domain.reflora)
checklist.af$BF.2020[checklist.af$domain.reflora & checklist.af$BF.2020 == "yes"] = "confirmed"
checklist.af$BF.2020[!checklist.af$domain.reflora & checklist.af$BF.2020 == "yes"] = "new to AF"
checklist.af1 = checklist.af[,c("family","scientific.name","life.form","BF.2020","TBC","cultivated","records","vouchers")]

## A ver:
#vouchers com "Fliho Filho," ou "Neto Neto,"
#'()' for the TBC

## Saving
write.csv(checklist.af, "af_checklist_complete.csv", row.names = FALSE)
names(checklist.af1)[which(names(checklist.af1) == "BF.2020")] = "status"
write.csv(checklist.af1, "af_checklist_Appendix3.csv", row.names = FALSE)
write.csv(checklist.af[,c("family","species.correct2","life.form","BF.2020","records","checagem_VCS","TreeCo_status")], "af_checklist.csv", row.names = FALSE)

####################
#### APPENDIX 4 ####
####################
## Probable occurrences for the Atlantic Forest: TRUE_in_transition and missing names from Reflora
spp.af = readRDS('spp.af.rds')
tmp  = spp.af[spp.af$af.check2 == "TRUE_in_transition" & spp.af$spp.group %in% c("valid","valid_not_in_reflora"),] # in transition
tmp1 = spp.af[spp.af$spp.group %in% c("valid_in_reflora"),]
tmp2 = rbind(tmp, tmp1)
tmp2 = tmp2[!duplicated(tmp2$species.correct2),]
checklist.prob = checklist[checklist$species.correct2 %in% tmp2$species.correct2]
checklist.prob = checklist.prob[!checklist.prob$species.correct2 %in% checklist.af$species.correct2,]

## Final edits and saving
checklist.prob$BF.2020 = gsub("valid_in_reflora","no records found but cited in BF.2020", as.character(checklist.prob$BF.2020))
checklist.prob$BF.2020 = gsub("^valid$","probable occurrence in the AF", as.character(checklist.prob$BF.2020))
checklist.prob$BF.2020 = gsub("valid_not_in_reflora","probable occurrence in the AF", as.character(checklist.prob$BF.2020))
checklist.prob1 = checklist.prob[,c("family","scientific.name","life.form","BF.2020","records","vouchers")]

## Saving
names(checklist.prob1)[which(names(checklist.prob1) == "BF.2020")] = "status"
write.csv(checklist.prob1, "probable_occurrences_Appendix4.csv", row.names = FALSE)

####################
#### APPENDIX 5 ####
####################
## Names not considered in the checklist (not confirmed neither probable): unresolved, illegitimate, etc.
checklist.exc = checklist[!checklist$species.correct2 %in% c(checklist.af$species.correct2,checklist.prob$species.correct2),]

## Final edits
checklist.exc$status = NA
checklist.exc$status[checklist.exc$establishment.AF %in% c("cultivated","not in the AF")] = "Cultivated in the Atlantic Forest"
checklist.exc$status[checklist.exc$establishment.AF %in% "naturalized"] = "Naturalized in the Atlantic Forest"
checklist.exc$status[checklist.exc$establishment.AF %in% "probably cultivated in JBRJ"] = "Cultivated in the Atlantic Forest"
checklist.exc$status[checklist.exc$establishment.AF %in% "probable error in locality description"] = "Probable error in locality description"
checklist.exc$status[is.na(checklist.exc$status) & checklist.exc$establishment.BR %in% c("cultivated","cultivated?")] = "Cultivated"
checklist.exc$status[is.na(checklist.exc$status) & checklist.exc$establishment.BR %in% "naturalized"] = "Naturalized"
checklist.exc$status[is.na(checklist.exc$status) & checklist.exc$establishment.BR %in% c("exotic","not in Brazil")] = "Exotic"
checklist.exc$status[is.na(checklist.exc$status) & grepl("Unresolved", checklist.exc$TreeCo_status)] = "Unresolved"
checklist.exc$status[is.na(checklist.exc$status) & grepl("remove|check", checklist.exc$TreeCo_status)] = "Unresolved"
checklist.exc$status[is.na(checklist.exc$status) & grepl("Uncertain application", checklist.exc$TreeCo_status)] = "Uncertain application"
checklist.exc$status[is.na(checklist.exc$status) & checklist.exc$TreeCo_status == "ok"] = "No taxonomically validated voucher inside or in the transition to the AF"

## Filtering
checklist.exc1 = checklist.exc[,c("family","scientific.name","life.form","status")]

## Adding the synonyms
spp.synonym = readRDS('spp.synonym.rds')
checklist.syn = spp.synonym[!spp.synonym$SpeciesKTSA1 %in% c(checklist.af$species.correct2,checklist.prob$species.correct2,checklist.exc$species.correct2),]
checklist.syn = checklist.syn[!is.na(checklist.syn$scientific.name.reflora),]

## Getting authorities for the names
require(Taxonstand)
require(flora)
tmp = get.taxa(checklist.syn$SpeciesKTSA1, replace.synonyms = FALSE,  suggest.names = FALSE, drop = c("names.status"), suggestion.distance = 1)
tmp$check = NA
tmp$check[!is.na(tmp$id) & !is.na(tmp$scientific.name) & tmp$notes %in% ""] = "synonym of "
tmp1 = get.taxa(checklist.syn$SpeciesKTSA1, replace.synonyms = FALSE,  suggest.names = TRUE, drop = c("names.status"), suggestion.distance = 0.92)
tmp$check[is.na(tmp$id) & !is.na(tmp1$id) & !is.na(tmp1$scientific.name)] = "typo of "
tmp$check[is.na(tmp$check)] = "replaced by "

#from tropicos
tmp2 = checklist.syn$SpeciesKTSA1[tmp$check %in% "replaced by "]
res = vector("list", length(tmp2))
for (i in 1:length(tmp2)) {
  rslt = gnr_resolve(names=tmp2[i], best_match_only = FALSE)
  if(length(rslt) == 0) {
    res[i] = "error"
  } else {
    if(any(rslt$data_source_title %in% "Tropicos - Missouri Botanical Garden")) { 
      res1 = as.character(rslt[rslt$data_source_title %in% "Tropicos - Missouri Botanical Garden",3])
      if(grepl("^c\\(", res1)) {
        res[i] = "ambiguous"
      } else {
        res[i] = res1 
      }
    } else {
      res1 = rslt$matched_name
      conta = str_count(res1, " ")
      res[i] = res1[min(which(conta>=3))]
    }
  }
}
res1 = do.call("rbind.data.frame", res)
res1 = droplevels(res1)
res1 = as.character(unlist(res1))

## REplacing the  names with authorities
nomes = tmp[,c("scientific.name","authorship","notes","original.search","check")]
nomes[is.na(tmp$id) & !is.na(tmp1$id) , c("scientific.name","authorship","notes")] = 
  tmp1[is.na(tmp$id) & !is.na(tmp1$id) , c("scientific.name","authorship","notes")]
nomes[tmp$check %in% "replaced by ", c("scientific.name")][!is.na(res1) & !res %in% c("ambiguous","error")] = 
   res1[!is.na(res1) & !res %in% c("ambiguous","error")]
nomes$check[tmp$check %in% "replaced by "][!is.na(res1) & !res %in% c("ambiguous","error")] = "synonym of "
table(nomes$original.search == checklist.syn$SpeciesKTSA1)

#Synonyms
checklist.syn$SpeciesKTSA1[nomes$check %in% "synonym of "] = nomes$scientific.name[nomes$check %in% "synonym of "]
checklist.syn$scientific.name.reflora[nomes$check %in% "synonym of "] = 
  paste("Synonym of ",checklist.syn$scientific.name.reflora[nomes$check %in% "synonym of "], sep="")
#Typos
checklist.syn$SpeciesKTSA1[nomes$check %in% "typo of "] = 
  paste(checklist.syn$SpeciesKTSA1[nomes$check %in% "typo of "], nomes$authorship[nomes$check %in% "typo of "], sep=" ")
checklist.syn$scientific.name.reflora[nomes$check %in% "typo of "] = 
  paste("Typo/Ort. variant of ",checklist.syn$scientific.name.reflora[nomes$check %in% "typo of "], sep="")
#Other
checklist.syn$scientific.name.reflora[nomes$check %in% "replaced by "] = 
  paste("Replaced by ",checklist.syn$scientific.name.reflora[nomes$check %in% "replaced by "], sep="")

#Replace encoding problems in author`s names
checklist.syn$SpeciesKTSA1 = gsub("Allem\\?\\?o","Allemao",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Lobão|Lob\\?\\?o","Lobao",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("M\\?¦ll\\.Arg\\.","Mull.Arg.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("M\\?¦ll \\.Arg\\.","Mull.Arg.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("M\\?\\?ll \\.Arg\\.","Mull.Arg.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("MÃ¼ll\\. Arg\\.","Mull.Arg.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("MÃ¼ll\\.Arg\\.","Mull.Arg.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("M\\?ªll\\.Arg\\.","Mull.Arg.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("M\\?¦ll\\. Arg.","Mull.Arg.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Dus\\?\\?n","Dusen",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Esp\\?\\?rito","Espirito",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Z\\?\\?chia","Zachia",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Piraj\\?\\?","Piraja",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Piraj\\?\\?","Piraja",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("G\\?¦rke","Gurke",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Joch\\.M\\?\\?ll\\.","Joch.Mull.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Arr\\?\\?b\\.","Arrab.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("F\\?\\?e","Fee",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("L\\?\\?onard","Leonard",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Garc\\?\\?a","Garcia",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Mendon??a","Mendonca",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Madri\\?\\?\\?\\?n","Madrinan",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("AllemÃ£o","Allemao",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Gir.-Ca\\?\\?as","Gir.-Canas",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Sess\\?\\?","Sesse",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("L\\?\\?rio","Lirio",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Tr\\?\\?cul","Trecul",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("C\\.D\\.Bouch\\?\\?","C.D.Bouche",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Louren\\?\\?o","Lourenco",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("B\\?¦nger","Bunger",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Lem\\?\\?e","Lemee",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Londo\\?\\?o","Londono",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("D\\?\\?ll|DÃ¶ll","Doll",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("B.St\\?\\?hl","B.Stahl",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("?vila","Avila",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Ara\\?\\?jo","Araujo",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Aubr\\?\\?v","Aubrev",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("M\\?\\?nico","Monico",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("LafetÃ¡","Lafeta",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("L\\'H\\?\\?r\\.","L'Her.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("CarriÃ¨re","Carriere",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Bid\\?\\?","Bida",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("O\\?\\?\\?Leary","O'Leary",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Fran\\?\\?a","Franca",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Gon\\?\\?\\.","Gonc.",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Fraz\\?\\?o","Frazao",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Bouch\\?\\?","Bouche",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Carri\\?\\?re","Carriere",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Feuill\\?e","Feuillee",checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Ã£","a", checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Ã©","e", checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Ã¡","a", checklist.syn$SpeciesKTSA1)
checklist.syn$SpeciesKTSA1 = gsub("Ã±","n", checklist.syn$SpeciesKTSA1)

checklist.syn$scientific.name.reflora = gsub("Allem\\?\\?o","Allemao",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Lobão|Lob\\?\\?o","Lobao",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("M\\?¦ll\\.Arg\\.","Mull.Arg.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("M\\?¦ll \\.Arg\\.","Mull.Arg.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("M\\?\\?ll \\.Arg\\.","Mull.Arg.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("MÃ¼ll\\. Arg\\.","Mull.Arg.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("MÃ¼ll\\.Arg\\.","Mull.Arg.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("M\\?ªll\\.Arg\\.","Mull.Arg.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("M\\?¦ll\\. Arg.","Mull.Arg.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Dus\\?\\?n","Dusen",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Esp\\?\\?rito","Espirito",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Z\\?\\?chia","Zachia",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Piraj\\?\\?","Piraja",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Piraj\\?\\?","Piraja",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("G\\?¦rke","Gurke",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Joch\\.M\\?\\?ll\\.","Joch.Mull.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Arr\\?\\?b\\.","Arrab.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("F\\?\\?e","Fee",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("L\\?\\?onard","Leonard",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Garc\\?\\?a","Garcia",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Mendon??a","Mendonca",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Madri\\?\\?\\?\\?n","Madrinan",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("AllemÃ£o","Allemao",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Gir.-Ca\\?\\?as","Gir.-Canas",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Sess\\?\\?","Sesse",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("L\\?\\?rio","Lirio",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Tr\\?\\?cul","Trecul",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("C\\.D\\.Bouch\\?\\?","C.D.Bouche",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Louren\\?\\?o","Lourenco",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("B\\?¦nger","Bunger",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Lem\\?\\?e","Lemee",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Londo\\?\\?o","Londono",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("D\\?\\?ll|DÃ¶ll","Doll",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("B.St\\?\\?hl","B.Stahl",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("?vila","Avila",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Ara\\?\\?jo","Araujo",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Aubr\\?\\?v","Aubrev",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("M\\?\\?nico","Monico",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("LafetÃ¡","Lafeta",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("L\\'H\\?\\?r\\.","L'Her.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("CarriÃ¨re","Carriere",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Bid\\?\\?","Bida",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("O\\?\\?\\?Leary","O'Leary",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Fran\\?\\?a","Franca",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Gon\\?\\?\\.","Gonc.",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Fraz\\?\\?o","Frazao",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Bouch\\?\\?","Bouche",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Carri\\?\\?re","Carriere",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Feuill\\?e","Feuillee",checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Ã£","a", checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Ã©","e", checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Ã¡","a", checklist.syn$scientific.name.reflora)
checklist.syn$scientific.name.reflora = gsub("Ã±","n", checklist.syn$scientific.name.reflora)

## Filtering and binding dataframes
checklist.syn1 = checklist.syn[,c("family.reflora","SpeciesKTSA1","habito","scientific.name.reflora")]
names(checklist.syn1) = c("family","scientific.name","life.form","status")
checklist.syn1 = checklist.syn1[!duplicated(checklist.syn1$scientific.name),]
checklist.exc2 = rbind.data.frame(checklist.exc1, checklist.syn1)
checklist.exc2 = checklist.exc2[order(checklist.exc2$scientific.name),]
checklist.exc2 = checklist.exc2[order(checklist.exc2$family),]

## Life forms
checklist.exc2$life.form = gsub("1","tree", checklist.exc2$life.form)
checklist.exc2$life.form = gsub("0.5","shrub", checklist.exc2$life.form)

## Final fixes 
checklist.exc2$scientific.name = gsub('[0-9]','',checklist.exc2$scientific.name)
checklist.exc2$scientific.name = gsub(', $','',checklist.exc2$scientific.name)

## Saving
write.csv(checklist.exc2, "names_excluded_Appendix5.csv", row.names = FALSE)

####################
#### APPENDIX 6 ####
####################
## The endemism level of the Atlantic Tree flora
#c("family","name","no.collections","endemism_ration","RefloraEndemic?"

## Reading the endemism levels
endemismo = readRDS("endemismo.rds")
endemismo1= readRDS("endemismo1.rds")

## Reading the Atlantic Forest checklist
checklist.af = read.csv("af_checklist_complete.csv", as.is=TRUE, na.strings = c(""," ",NA))
endemismo = endemismo[endemismo$species.correct1 %in% checklist.af$species.correct2,]
endemismo = endemismo[order(endemismo$species.correct1),]
endemismo = endemismo[order(endemismo$family.reflora),]
endemismo1 = endemismo1[endemismo1$species.correct1 %in% endemismo$species.correct1,]
endemismo1 = endemismo1[order(endemismo1$species.correct1),]
endemismo1 = endemismo1[order(endemismo1$family.reflora),]

sum(endemismo[,2:6]) # overall valid occurrences, only TRUE taxonomy
sum(endemismo[,c(2,5)]) # in or out
sum(endemismo[,c(3:4)]) # transitions
sum(endemismo1[,2:6]) # overall valid occurrences, only TRUE and PROBABLE taxonomy
sum(endemismo1[,c(2,5)]) # in or out
sum(endemismo1[,c(3:4)]) # transitions
sum(checklist.af$records)
table(endemismo$species.correct1 == endemismo1$species.correct1)

tmp = cbind.data.frame(endemismo1[,c("family.reflora","species.correct1","outside","out.in","in.out","inside")], #,"cc")],
                       endemismo[,c("nivel2","end.group2")],
                       endemismo1[,c("nivel2","end.group2","domain.reflora")])
tmp$endemism.BF.2020 = NA
tmp$endemism.BF.2020[tmp$domain.reflora %in% "AtlanticForest"] = "Endemic"
tmp$endemism.BF.2020[is.na(tmp$endemism.BF.2020) & !is.na(tmp$domain.reflora) & !grepl("AtlanticForest", tmp$domain.reflora)] = "Not in the AF"
tmp$endemism.BF.2020[is.na(tmp$endemism.BF.2020) & !is.na(tmp$domain.reflora)] = "Not endemic"          
table(tmp$endemism.BF.2020, useNA = "ifany")
#boxplot(tmp$nivel2 ~ tmp$endemism.BF.2020, notch = TRUE, varwidth = TRUE)


## Renaming and filtering the columns
names(tmp) = c("family","species","outside.AF","out.in","in.out","inside.AF", #"cannot.check",
               "endemism.level","group","endemism.level1","group1","domain.reflora","endemism.BF.2020")
tmp1 = tmp[, c("family","species","outside.AF","out.in","in.out","inside.AF", #"cannot.check",
               "endemism.level","group","endemism.level1","group1","endemism.BF.2020")]

## Final edits
tmp1$group = gsub("occ","occasional",tmp1$group)
tmp1$group = gsub("wide","widespread",tmp1$group)
tmp1$group = gsub("near","near_endemic",tmp1$group)
tmp1$group = gsub("pure","pure_endemic",tmp1$group)
tmp1$group1 = gsub("occ","occasional",tmp1$group1)
tmp1$group1 = gsub("wide","widespread",tmp1$group1)
tmp1$group1 = gsub("near","near_endemic",tmp1$group1)
tmp1$group1 = gsub("pure","pure_endemic",tmp1$group1)

## Saving 
write.csv(tmp1, "endemism_levels_Appendix6.csv", row.names = FALSE)

