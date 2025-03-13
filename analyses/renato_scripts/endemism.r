###############################
## GETTING INFO FROM REFLORA ##
###############################

##AFAZERES:
#1-Check if all tree species in ReFlora are in the list - HOW??
#2-Check if all tree species in ATDN/TreeCo are in the final Atlantic Forest list
#3-Obtain vegtype for species in Reflora - OK

## loading packages
require(taxize)
require(flora)
require(RCurl)
require(dplyr)
require(stringr)
require(finch)


## loading Grandtner & Chevrette (2014) species list
#neo_trees = read.csv('NeoTropTree.csv', as.is=T, stringsAsFactors=F)
trees = read.csv('DomainsKnownTreesNeotropics.csv', as.is=T, stringsAsFactors=F)

## loading the ReFlora species list
zip <- system.file("examples", "dwca-lista_especies_flora_brasil-v393.158.zip",package = "finch")
reflora <- dwca_read(zip)

trees[trees$Accepted_name %in% c("",NA),]$Accepted_name = trees[trees$Accepted_name %in% c("",NA),]$search.str.reflora
trees[trees$Accepted_name %in% c("",NA),]$Accepted_name = trees[trees$Accepted_name %in% c("",NA),]$Accepted_name_species
trees[trees$Accepted_name %in% c("",NA),]$Accepted_name = as.character(unlist(sapply(trees[trees$Accepted_name %in% c("",NA),]$Species, function(x) strsplit(x,"\\=|\\?")[[1]][1])))

## getting unique accepted names
spp = unique(trees$Accepted_name)

## obtaining taxonomic information for each name on the list
nomes = get.taxa(spp$spp,life.form = TRUE, habitat = TRUE, states = TRUE, establishment = TRUE,
	suggestion.distance = 0.95, drop = c("authorship"))

## filtering only the names found in ReFlora
nomes1 = nomes[!is.na(nomes$scientific.name),]

## obtaining species domain of occurrence from ReFlora
sq = c(seq(1,dim(nomes1)[1],by=200),dim(nomes1)[1])
domains = NULL
for(i in 1:(length(sq)-1)) {
 tmp = get_domains(nomes1[sq[i]:sq[i+1],])
 if(is.null(domains)) domains = tmp else domains = rbind.data.frame(domains,tmp)
 cat(i,"\n")
}

## obtaining species vegetation types from ReFlora
nomes1 = trees[!is.na(trees$id.reflora),]
nomes1 = nomes1[is.na(nomes1$vegtype),]

nomes1 = nomes1[,c(31:43,5,44:47)]
names(nomes1) = names(nomes)

sq = c(seq(1,dim(nomes1)[1],by=200),dim(nomes1)[1])
vegtype = NULL
for(i in 18:(length(sq)-1)) {
 tmp = get_vegtype(nomes1[sq[i]:sq[i+1],])
 if(is.null(vegtype)) vegtype = tmp else vegtype = rbind.data.frame(vegtype,tmp)
cat(i,"\n")
}

#REFEZER: 17
tmp = nomes1[!nomes1$original.search %in% tmp3$original.search,]

sq = c(seq(1,dim(tmp)[1],by=2),dim(tmp)[1])
vegtype1 = NULL
for(i in 9:(length(sq)-2)) {
 tmp1 = get_vegtype(tmp[sq[i]:sq[i+1],])
 if(is.null(vegtype1)) vegtype1 = tmp1 else vegtype1 = rbind.data.frame(vegtype1,tmp1)
cat(i,"\n")
}

tmp3 = rbind.data.frame(tmp2,vegtype1)
tmp3[is.na(tmp3$original.search),]$original.search = tmp3[is.na(tmp3$original.search),]$search.str
tmp3 = tmp3[!duplicated(tmp3$original.search),]


#Merging info
nomes2 = merge(nomes1,tmp3[,c("original.search","vegtype")],by="original.search",all.x=T)
names(nomes2)[grep("original.search",names(nomes2))] = "SpeciesKTSA"
nomes3 = left_join(trees,nomes2[,c("SpeciesKTSA","vegtype")],by="SpeciesKTSA")

## assigning the information of each name on the list to the names on the original database
names(nomes3)[grep("original.search",names(nomes3))] = "SpeciesKTSA"
nomes.join = left_join(x= sa_trees, y= nomes3, by="SpeciesKTSA")

## ordering the output in the same order of the input dataframe
nomes.join1 = nomes.join[order(nomes.join$order),]

#### MERGING WITH THE EDITED VERSION OF ARY'S SPECIES TABLE ##
tmp = read.csv("NeoTropTree1.csv", as.is=T)
names(tmp)[grep("original.search",names(tmp))] = "SpeciesKTSA"

nomes.join2 = merge(nomes.join1,tmp, by="SpeciesKTSA",all=T)
nomes.join2 = nomes.join2[order(nomes.join2$order),]


#### SAVING THE RESULTS #####
write.csv(nomes.join2,"DomainsTreesSA2.csv")

write.csv(nomes3,"vegtype1.csv")


#### LOOKING IN REFLORA FOR MISSING TREE SPECIES ####
#any: occurs in one of the given states
#only: occurs only in the list of states
#all: occurs at least in the states listed 
sts = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG",
		"PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
flora = occurrence(sts, type = c("any"), taxa = NULL)
tmp = get.taxa(flora$search.str,life.form = TRUE, habitat = FALSE, states = FALSE, establishment = TRUE,
	suggestion.distance = 1, drop = c("occurrence"))
tmp1 = tmp[grep("Ãrvore|Ãrvore|Árvore|Palmeira",tmp$life.form),] 
tmp2 = tmp1[!tmp1$search.str %in% unique(c(trees$search.str.reflora,trees$Accepted_name )),]

#CONSIDERAR ARBUSTOS??

#Cheking missing Atlantic forest species
tmp = rbind.data.frame(occurrence(c("RJ"), type = c("any"), taxa = NULL),
		occurrence(c("ES"), type = c("any"), taxa = NULL),
		occurrence(c("SC"), type = c("any"), taxa = NULL))
tmp1 = rbind.data.frame(occurrence(c("RJ","ES"), type = c("only"), taxa = NULL),
		occurrence(c("RJ"), type = c("only"), taxa = NULL),
		occurrence(c("ES"), type = c("only"), taxa = NULL),
		occurrence(c("SC"), type = c("only"), taxa = NULL))

#Cheking missing Atlantic forest species



### OBTAINIG THE ATLANTIF FOREST LIST AND ENDEMIC RATE ###
trees = read.csv('DomainsKnownTreesNeotropics.csv', as.is=T, stringsAsFactors=F)

#Getting accepted names
trees[trees$Accepted_name %in% c("",NA),]$Accepted_name = trees[trees$Accepted_name %in% c("",NA),]$search.str.reflora
trees[trees$Accepted_name %in% c("",NA),]$Accepted_name = trees[trees$Accepted_name %in% c("",NA),]$Accepted_name_species
trees[trees$Accepted_name %in% c("",NA),]$Accepted_name = as.character(unlist(sapply(trees[trees$Accepted_name %in% c("",NA),]$Species, function(x) strsplit(x,"\\=|\\?")[[1]][1])))
trees[trees$AtlanticForest %in% "yes"&trees$search.str.reflora %in% c("",NA),]$search.str.reflora = trees[trees$AtlanticForest %in% "yes"&trees$search.str.reflora %in% c("",NA),]$Accepted_name

#Getting records for the Atlantic Forest
tmp = trees[!is.na(trees$AtlanticForest) & trees$AtlanticForest %in% "yes",]
tmp1= tmp[!duplicated(tmp$search.str.reflora),]
tmp2= tmp[!duplicated(tmp$Accepted_name),]
dim(tmp1);dim(tmp2)

table(tmp1$Endemism)
table(tmp1[grep("endemic_genus",tmp1$Endemism),]$Genus)