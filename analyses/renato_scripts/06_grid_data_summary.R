############################################
#### PLOTING AND ANALYZYIN THE RESULTS  ####
############################################
rm(list=ls())

#### LOADING PACKAGES  ####
require(data.table)
require(raster)
require(rgeos)
require(rgdal)
require(cleangeo)
require(iNEXT)
require(parallel)
require(doParallel)
require(nlme)
require(gstat)
require(sp)
require(maptools)
require(rcompanion)
require(bbmle)
#require(stringr)
#require(splines)
#require(bbmle)
#require(segmented)

#### LOADING THE DATA  ####

## Tree occurrence data ##
# Non-duplicated, geographically and taxonomically validated (true or probable) specimens
oc.data.af.spp.geo.tax1 = readRDS("oc.data.af.spp.geo.tax1.rds")

# Non-duplicated, geographically and taxonomically validated (true) specimens
oc.data.af.spp.geo.tax = oc.data.af.spp.geo.tax1[tax.check1 %in% TRUE]

# non-duplicated, geographically and taxonomically validated specimens inside the AF
dim(oc.data.af.spp.geo.tax[af.check2 %in% TRUE])[1] # 252,972 (only taxonomy == TRUE)
dim(oc.data.af.spp.geo.tax1[af.check2 %in% TRUE])[1] # 329,052 (taxonomy == TRUE OR PROBABLE)

## Getting the list of species and their classification of endemism levels ##
endemismo = readRDS("endemismo.rds")
endemismo1 = readRDS("endemismo1.rds")

######################################################################################################################################################################################-
#### CENTERS OF ENDEMISM, NEAR-ENDEMISM AND OCCASIONAL SPECIES IN THE ATLANTIC FOREST ####

## TRANSFORMING OCCURRENCES INTO SPATIAL DATA FRAME ##
#### CHECK HERE: Include filter: af.check2 %like% "TRUE" for the pure endemics? ####
occs = as.data.frame(oc.data.af.spp.geo.tax1)
occs = occs[,c("numTombo","species.correct2","latitude.work1","longitude.work1","geo.check1","af.check2","tax.check2","loc.correct","municipio")]
#dim(occs)

##removing possible missing coordinates
occs = occs[!is.na(occs$latitude.work1)&!is.na(occs$longitude.work1),]
#dim(occs)

##removing coordinates that could not be verified
occs = occs[occs$af.check2 %in% c("FALSE","FALSE_in_transition","TRUE_in_transition","TRUE"),]
#dim(occs)

##Transforming the occurrences into spatial points
occs$latitude.work1 = as.numeric(occs$latitude.work1)
occs$longitude.work1 = as.numeric(occs$longitude.work1)
occs.sp = SpatialPointsDataFrame(coords = cbind(occs$longitude.work1, occs$latitude.work1), 
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +no_defs"),
                                 data = occs[,c("numTombo","species.correct2","geo.check1","af.check2","tax.check2","loc.correct","municipio")])
occs.sp = spTransform(occs.sp, CRS("+init=epsg:5641"))

## CROSSING THE OCCURRENCE DATA WITH THE AF GRID ##
## Getting the grid and editing it
grid = readRDS("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo Hyperdominance//af_proj_50km.rds")

## Making sure grid IDs are the same internally
grid1 = grid
tmp = SpatialPoints(coords = coordinates(grid),proj4string = crs(grid))
tmp1= over(tmp,grid)
for(i in 1:length(grid1)) { slot(slot(grid1, "polygons")[[i]],"ID") = as.character(paste("ID",i,sep=""))} 
#plot(grid1)

## Overlaying the occurences against the grid
occs.sp@data$cell.ID = paste("ID", over(occs.sp, grid1),sep="")

## Re-assing grid ID for points falling near the coast
tmp = occs.sp@data$loc.correct[occs.sp@data$cell.ID %in% "IDNA" & !is.na(occs.sp@data$municipio)]
tmp1= occs.sp@data[occs.sp@data$loc.correct %in% unique(tmp),] 
tmp1 = tmp1[!tmp1$cell.ID %in% "IDNA",]
tmp2 = aggregate(tmp1$loc.correct, list(tmp1$cell.ID,tmp1$loc.correct),function(x) length(as.character(x)))
tmp2 = tmp2[order(tmp2$x,decreasing = TRUE),]
tmp2 = tmp2[order(tmp2$Group.2,decreasing = TRUE),]
tmp2 = cbind.data.frame(loc.correct=tmp2[!duplicated(tmp2$Group.2),]$Group.2,
                        ID=tmp2[!duplicated(tmp2$Group.2),]$Group.1)
tmp = occs.sp@data[occs.sp@data$cell.ID %in% "IDNA" & !is.na(occs.sp@data$municipio),]
tmp$ordem = 1:dim(tmp)[1]
tmp = merge(tmp,tmp2,by="loc.correct",all.x=TRUE)
tmp = tmp[order(tmp$ordem),]
tmp$ID = as.character(tmp$ID)
tmp$ID[is.na(tmp$ID)] = tmp$cell.ID[is.na(tmp$ID)]
table(tmp$numTombo == occs.sp@data$numTombo[occs.sp@data$cell.ID %in% "IDNA" & !is.na(occs.sp@data$municipio)])
occs.sp@data$cell.ID[occs.sp@data$cell.ID %in% "IDNA" & !is.na(occs.sp@data$municipio)] = tmp$ID

#Finding the grid cell for points with projection issues
tmp = occs.sp[occs.sp@data$cell.ID %in% "IDNA",]
ext = extent(grid) + c(-50000,50000,-50000,50000)
tmp= crop(tmp, ext)

result = cbind.data.frame(numTombo = as.character(occs.sp@data$numTombo[occs.sp@data$cell.ID %in% "IDNA"]), 
                          cell.ID = "IDNA", stringsAsFactors = FALSE)
tmp1 = gDistance(tmp, grid, byid=TRUE) # calculating the distance between points and all grid cells
for(i in 1:dim(tmp1)[2]){
  IDi = which.min(tmp1[,i])
  dist1 = tmp1[which.min(tmp1[,i]),i]
  if(dist1>0 & dist1<5000) result$cell.ID[i] = as.character(paste("ID",IDi,sep=""))
}
occs.sp@data$cell.ID[occs.sp@data$cell.ID %in% "IDNA"] = result$cell.ID

## Removing points falling outside the grid limits
occs.sp = occs.sp[!occs.sp@data$cell.ID %in% "IDNA",]

## Getting the classification of species endemism groups
#occs.sp1 => both TRUE and PROBABLE validated taxonomy
#occs.sp2 => only TRUE validated taxonomy
occs.sp1 = merge(occs.sp@data, endemismo1[,c("spp","end.group","end.group2")], by.x = "species.correct2", by.y= "spp", all.x=TRUE)
occs.sp1 = occs.sp1[!is.na(occs.sp1$end.group),]
#dim(occs.sp);dim(occs.sp1)
occs.sp2 = merge(occs.sp@data, endemismo[,c("spp","end.group","end.group2")], by.x = "species.correct2", by.y= "spp", all.x=TRUE)
occs.sp2 = occs.sp2[!is.na(occs.sp2$end.group),]

## Obtaining the range of each species
tmp = aggregate(occs.sp1$cell.ID, list(occs.sp1$species.correct2), function(x) length(unique(x)))
names(tmp) = c("species.correct2","spp.range")
occs.sp1 = merge(occs.sp1, tmp, by = "species.correct2", all.x=TRUE)
tmp = aggregate(occs.sp2$cell.ID, list(occs.sp2$species.correct2), function(x) length(unique(x)))
names(tmp) = c("species.correct2","spp.range")
occs.sp2 = merge(occs.sp2, tmp, by = "species.correct2", all.x=TRUE)

#### SUMMARIES REPRESENTATIVENESS OF THE SAMPLING EFFORT PER GRID CELL FOR EACH SPECIES GROUP ####
grid.result = cbind.data.frame(order = 1:length(grid1), 
                               ID = as.character(sapply(slot(grid1, "polygons"), function(x) slot(x, "ID"))),
                               plotOrder = slot(grid1, "plotOrder"),
                               area_ha = as.double(gArea(grid1, byid = TRUE)/10000),
                               lat = coordinates(grid1)[,2],
                               long = coordinates(grid1)[,1])
grid.result$ID = as.character(grid.result$ID)

#grid.result => both TRUE and PROBABLE validated taxonomy
#grid.result2 => only TRUE validated taxonomy
grid.result2 = grid.result

boot = 499; cf = 0.95; m = c(25,50,100)
### All species ###
# Number of occurrences, number of species and sampling coverage per site
# TRUE + PROBABLE taxonomy
tmp = DataInfo(as.data.frame.matrix(table(occs.sp1$species.correct2,occs.sp1$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
samp.cov.cutoff = quantile(tmp$SC[tmp$n>=10], prob=c(0.05,0.1,0.25,0.5,0.75,0.8,0.85,0.9,0.95,0.99))
names(tmp)[1:4] = c("ID","N.total","S.total","SampCover")
grid.result = merge(grid.result, tmp[,c("ID","N.total","S.total","SampCover")], by.x= "ID", by.y= "ID", all.x=TRUE)
# only TRUE taxonomy
tmp = DataInfo(as.data.frame.matrix(table(occs.sp2$species.correct2,occs.sp2$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
samp.cov.cutoff2 = quantile(tmp$SC[tmp$n>=10], prob=c(0.05,0.1,0.25,0.5,0.75,0.8,0.85,0.9,0.95,0.99))
names(tmp)[1:4] = c("ID","N.total","S.total","SampCover")
grid.result2 = merge(grid.result2, tmp[,c("ID","N.total","S.total","SampCover")], by.x= "ID", by.y= "ID", all.x=TRUE)

# Weighted endemsim and corrected weighted endemsim 
# TRUE + PROBABLE taxonomy
tmp = occs.sp1[!duplicated(paste(occs.sp1$species.correct2,occs.sp1$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.total")
grid.result = merge(grid.result, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result$CWE.total = grid.result$WE.total/grid.result$S.total
# Calculating diversity indices based on N and S (NOT INCLUDED ANYMORE)
#tmp1 = grid.result[,c("N.total","S.total")]
#tmp1$mix.coef = suppressWarnings(tmp1[,2]/tmp1[,1]) # Mixture coefficient
#tmp1$margalef = suppressWarnings((tmp1[,2]-1)/log(tmp1[,1])) # Margalef index 
#tmp1$menhinick = suppressWarnings((tmp1[,2])/sqrt(tmp1[,1])) # Menhinick index (S/sqrt(N))
#tmp1 = tmp1[,-c(1,2)]
#tmp1[is.nan(tmp1$margalef),] = NA
#names(tmp1) = paste(names(tmp1),".total",sep="")
#grid.result = cbind.data.frame(grid.result, tmp1)
# only TRUE taxonomy
tmp = occs.sp2[!duplicated(paste(occs.sp2$species.correct2,occs.sp2$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.total")
grid.result2 = merge(grid.result2, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result2$CWE.total = grid.result2$WE.total/grid.result2$S.total
# Calculating diversity indices based on N and S (NOT INCLUDED ANYMORE)
#tmp1 = grid.result2[,c("N.total","S.total")]
#tmp1$mix.coef = suppressWarnings(tmp1[,2]/tmp1[,1]) # Mixture coefficient
#tmp1$margalef = suppressWarnings((tmp1[,2]-1)/log(tmp1[,1])) # Margalef index 
#tmp1$menhinick = suppressWarnings((tmp1[,2])/sqrt(tmp1[,1])) # Menhinick index (S/sqrt(N))
#tmp1 = tmp1[,-c(1,2)]
#tmp1[is.nan(tmp1$margalef),] = NA
#names(tmp1) = paste(names(tmp1),".total",sep="")
#grid.result2 = cbind.data.frame(grid.result2, tmp1)

###############################H
### TRUE + PROBABLE taxonomy ###
###############################H
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(occs.sp1$species.correct2,occs.sp1$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                             ".total",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 e 2 (Species richness and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"total",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"total",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result = merge(grid.result, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

### Only pure endemics ###
## RAW ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
pure.occs = occs.sp1[occs.sp1$end.group %in% c("pure"),]
tmp = DataInfo(as.data.frame.matrix(table(pure.occs$species.correct2,pure.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.pure","S.pure","SampCover.pure")
grid.result = merge(grid.result, tmp[,c("ID","N.pure","S.pure","SampCover.pure")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = pure.occs[!duplicated(paste(pure.occs$species.correct2,pure.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.pure")
grid.result = merge(grid.result, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result$CWE.pure = grid.result$WE.pure/grid.result$S.pure
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(pure.occs$species.correct2,pure.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".pure",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"pure",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"pure",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result = merge(grid.result, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

## WEIGHTED ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
pure.occs = occs.sp1[occs.sp1$end.group2 %in% c("pure"),]
tmp = DataInfo(as.data.frame.matrix(table(pure.occs$species.correct2,pure.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.pure2","S.pure2","SampCover.pure2")
grid.result = merge(grid.result, tmp[,c("ID","N.pure2","S.pure2","SampCover.pure2")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = pure.occs[!duplicated(paste(pure.occs$species.correct2,pure.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.pure2")
grid.result = merge(grid.result, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result$CWE.pure2 = grid.result$WE.pure2/grid.result$S.pure2 
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(pure.occs$species.correct2,pure.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".pure2",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"pure2",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"pure2",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result = merge(grid.result, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

### Only near endemics ###
## RAW ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
near.occs = occs.sp1[occs.sp1$end.group %in% c("near"),]
tmp = DataInfo(as.data.frame.matrix(table(near.occs$species.correct2,near.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.near","S.near","SampCover.near")
grid.result = merge(grid.result, tmp[,c("ID","N.near","S.near","SampCover.near")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = near.occs[!duplicated(paste(near.occs$species.correct2,near.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.near")
grid.result = merge(grid.result, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result$CWE.near = grid.result$WE.near/grid.result$S.near 
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(near.occs$species.correct2,near.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".near",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"near",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"near",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result = merge(grid.result, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)
## WEIGHTED ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
near.occs = occs.sp1[occs.sp1$end.group2 %in% c("near"),]
tmp = DataInfo(as.data.frame.matrix(table(near.occs$species.correct2,near.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.near2","S.near2","SampCover.near2")
grid.result = merge(grid.result, tmp[,c("ID","N.near2","S.near2","SampCover.near2")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = near.occs[!duplicated(paste(near.occs$species.correct2,near.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.near2")
grid.result = merge(grid.result, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result$CWE.near2 = grid.result$WE.near2/grid.result$S.near2 
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(near.occs$species.correct2,near.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".near2",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"near2",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"near2",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result = merge(grid.result, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

### Pure and near endemics ###
## RAW ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
pure.near.occs = occs.sp1[occs.sp1$end.group %in% c("pure","near"),]
tmp = DataInfo(as.data.frame.matrix(table(pure.near.occs$species.correct2,pure.near.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.pure.near","S.pure.near","SampCover.pure.near")
grid.result = merge(grid.result, tmp[,c("ID","N.pure.near","S.pure.near","SampCover.pure.near")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = pure.near.occs[!duplicated(paste(pure.near.occs$species.correct2,pure.near.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.pure.near")
grid.result = merge(grid.result, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result$CWE.pure.near = grid.result$WE.pure.near/grid.result$S.pure.near
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(pure.near.occs$species.correct2,pure.near.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".pure.near",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"pure.near",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"pure.near",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result = merge(grid.result, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)
## WEIGHTED ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
pure.near.occs = occs.sp1[occs.sp1$end.group2 %in% c("pure","near"),]
tmp = DataInfo(as.data.frame.matrix(table(pure.near.occs$species.correct2,pure.near.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.pure.near2","S.pure.near2","SampCover.pure.near2")
grid.result = merge(grid.result, tmp[,c("ID","N.pure.near2","S.pure.near2","SampCover.pure.near2")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = pure.near.occs[!duplicated(paste(pure.near.occs$species.correct2,pure.near.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.pure.near2")
grid.result = merge(grid.result, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result$CWE.pure.near2 = grid.result$WE.pure.near2/grid.result$S.pure.near2
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(pure.near.occs$species.correct2,pure.near.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".pure.near2",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"pure.near2",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"pure.near2",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result = merge(grid.result, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

### Occasional species ###
## RAW ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
ocas.occs = occs.sp1[occs.sp1$end.group %in% c("occ"),]
tmp = DataInfo(as.data.frame.matrix(table(ocas.occs$species.correct2,ocas.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.ocas","S.ocas","SampCover.ocas")
grid.result = merge(grid.result, tmp[,c("ID","N.ocas","S.ocas","SampCover.ocas")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = ocas.occs[!duplicated(paste(ocas.occs$species.correct2,ocas.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.ocas")
grid.result = merge(grid.result, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result$CWE.ocas = grid.result$WE.ocas/grid.result$S.ocas
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(ocas.occs$species.correct2,ocas.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".ocas",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"ocas",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"ocas",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result = merge(grid.result, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)
## WEIGHTED ENDEMISM LEVEL ##
ocas.occs = occs.sp1[occs.sp1$end.group2 %in% c("occ"),]
tmp = DataInfo(as.data.frame.matrix(table(ocas.occs$species.correct2,ocas.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.ocas2","S.ocas2","SampCover.ocas2")
grid.result = merge(grid.result, tmp[,c("ID","N.ocas2","S.ocas2","SampCover.ocas2")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = ocas.occs[!duplicated(paste(ocas.occs$species.correct2,ocas.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.ocas2")
grid.result = merge(grid.result, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result$CWE.ocas2 = grid.result$WE.ocas2/grid.result$S.ocas2
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(ocas.occs$species.correct2,ocas.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".ocas2",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"ocas2",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"ocas2",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result = merge(grid.result, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

#########################H
### only TRUE taxonomy ###
#########################H
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(occs.sp2$species.correct2,occs.sp2$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".total",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 e 2 (Species richness and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"total",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"total",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result2 = merge(grid.result2, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

### Only pure endemics ###
## RAW ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
pure.occs = occs.sp2[occs.sp2$end.group %in% c("pure"),]
tmp = DataInfo(as.data.frame.matrix(table(pure.occs$species.correct2,pure.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.pure","S.pure","SampCover.pure")
grid.result2 = merge(grid.result2, tmp[,c("ID","N.pure","S.pure","SampCover.pure")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = pure.occs[!duplicated(paste(pure.occs$species.correct2,pure.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.pure")
grid.result2 = merge(grid.result2, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result2$CWE.pure = grid.result2$WE.pure/grid.result2$S.pure
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(pure.occs$species.correct2,pure.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".pure",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"pure",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"pure",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result2 = merge(grid.result2, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)
## WEIGHTED ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
pure.occs = occs.sp2[occs.sp2$end.group2 %in% c("pure"),]
tmp = DataInfo(as.data.frame.matrix(table(pure.occs$species.correct2,pure.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.pure2","S.pure2","SampCover.pure2")
grid.result2 = merge(grid.result2, tmp[,c("ID","N.pure2","S.pure2","SampCover.pure2")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = pure.occs[!duplicated(paste(pure.occs$species.correct2,pure.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.pure2")
grid.result2 = merge(grid.result2, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result2$CWE.pure2 = grid.result2$WE.pure2/grid.result2$S.pure2
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(pure.occs$species.correct2,pure.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".pure2",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"pure2",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"pure2",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result2 = merge(grid.result2, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

### Only near endemics ###
## RAW ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
near.occs = occs.sp2[occs.sp2$end.group %in% c("near"),]
tmp = DataInfo(as.data.frame.matrix(table(near.occs$species.correct2,near.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.near","S.near","SampCover.near")
grid.result2 = merge(grid.result2, tmp[,c("ID","N.near","S.near","SampCover.near")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = near.occs[!duplicated(paste(near.occs$species.correct2,near.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.near")
grid.result2 = merge(grid.result2, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result2$CWE.near = grid.result2$WE.near/grid.result2$S.near
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(near.occs$species.correct2,near.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".near",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"near",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"near",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result2 = merge(grid.result2, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)
## WEIGHTED ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
near.occs = occs.sp2[occs.sp2$end.group2 %in% c("near"),]
tmp = DataInfo(as.data.frame.matrix(table(near.occs$species.correct2,near.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.near2","S.near2","SampCover.near2")
grid.result2 = merge(grid.result2, tmp[,c("ID","N.near2","S.near2","SampCover.near2")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = near.occs[!duplicated(paste(near.occs$species.correct2,near.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.near2")
grid.result2 = merge(grid.result2, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result2$CWE.near2 = grid.result2$WE.near2/grid.result2$S.near2
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(near.occs$species.correct2,near.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".near2",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"near2",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"near2",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result2 = merge(grid.result2, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

### Pure and near endemics ###
## RAW ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
pure.near.occs = occs.sp2[occs.sp2$end.group %in% c("pure","near"),]
tmp = DataInfo(as.data.frame.matrix(table(pure.near.occs$species.correct2,pure.near.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.pure.near","S.pure.near","SampCover.pure.near")
grid.result2 = merge(grid.result2, tmp[,c("ID","N.pure.near","S.pure.near","SampCover.pure.near")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = pure.near.occs[!duplicated(paste(pure.near.occs$species.correct2,pure.near.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.pure.near")
grid.result2 = merge(grid.result2, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result2$CWE.pure.near = grid.result2$WE.pure.near/grid.result2$S.pure.near
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(pure.near.occs$species.correct2,pure.near.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".pure.near",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"pure.near",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"pure.near",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result2 = merge(grid.result2, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)
## WEIGHTED ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
pure.near.occs = occs.sp2[occs.sp2$end.group2 %in% c("pure","near"),]
tmp = DataInfo(as.data.frame.matrix(table(pure.near.occs$species.correct2,pure.near.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.pure.near2","S.pure.near2","SampCover.pure.near2")
grid.result2 = merge(grid.result2, tmp[,c("ID","N.pure.near2","S.pure.near2","SampCover.pure.near2")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = pure.near.occs[!duplicated(paste(pure.near.occs$species.correct2,pure.near.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.pure.near2")
grid.result2 = merge(grid.result2, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result2$CWE.pure.near2 = grid.result2$WE.pure.near2/grid.result2$S.pure.near2
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(pure.near.occs$species.correct2,pure.near.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".pure.near2",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"pure.near2",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"pure.near2",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result2 = merge(grid.result2, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

### Occasional species ###
## RAW ENDEMISM LEVEL ##
# Number of occurrences, number of species and sampling coverage per site
ocas.occs = occs.sp2[occs.sp2$end.group %in% c("occ"),]
tmp = DataInfo(as.data.frame.matrix(table(ocas.occs$species.correct2,ocas.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.ocas","S.ocas","SampCover.ocas")
grid.result2 = merge(grid.result2, tmp[,c("ID","N.ocas","S.ocas","SampCover.ocas")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = ocas.occs[!duplicated(paste(ocas.occs$species.correct2,ocas.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.ocas")
grid.result2 = merge(grid.result2, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result2$CWE.ocas = grid.result2$WE.ocas/grid.result2$S.ocas
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(ocas.occs$species.correct2,ocas.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".ocas",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"ocas",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"ocas",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result2 = merge(grid.result2, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)
## WEIGHTED ENDEMISM LEVEL ##
ocas.occs = occs.sp2[occs.sp2$end.group2 %in% c("occ"),]
tmp = DataInfo(as.data.frame.matrix(table(ocas.occs$species.correct2,ocas.occs$cell.ID))) # getting metrics
tmp$SC[tmp$n==1] = 0.0001 # correcting values of SampCover for sites with N<10 and S ~~ N
tmp$SC[tmp$n==2&tmp$S.obs==2] = 0.0005; tmp$SC[tmp$n==2&tmp$S.obs==1] = 0.001
tmp$SC[tmp$n==3&tmp$S.obs==3] = 0.001; tmp$SC[tmp$n==3&tmp$S.obs==2] = 0.005; tmp$SC[tmp$n==3&tmp$S.obs==1] = 0.01
tmp$SC[tmp$n==4&tmp$S.obs==4] = 0.01; tmp$SC[tmp$n==4&tmp$S.obs==3] = 0.025; tmp$SC[tmp$n==4&tmp$S.obs==2] = 0.05; tmp$SC[tmp$n==4&tmp$S.obs==1] = 0.1
tmp$SC[tmp$n==5&tmp$S.obs==5] = 0.1; tmp$SC[tmp$n==5&tmp$S.obs==4] = 0.125; tmp$SC[tmp$n==5&tmp$S.obs==3] = 0.15; tmp$SC[tmp$n==5&tmp$S.obs==2] = 0.175; tmp$SC[tmp$n==5&tmp$S.obs==1] = 0.2
tmp$SC[tmp$n==6&tmp$S.obs==6] = 0.125; tmp$SC[tmp$n==6&tmp$S.obs==5] = 0.15; tmp$SC[tmp$n==6&tmp$S.obs==4] = 0.175; tmp$SC[tmp$n==6&tmp$S.obs==3] = 0.2; tmp$SC[tmp$n==6&tmp$S.obs==2] = 0.225; tmp$SC[tmp$n==6&tmp$S.obs==1] = 0.25
tmp$SC[tmp$n==7&tmp$S.obs==7] = 0.15; tmp$SC[tmp$n==7&tmp$S.obs==6] = 0.175; tmp$SC[tmp$n==7&tmp$S.obs==5] = 0.2; tmp$SC[tmp$n==7&tmp$S.obs==4] = 0.225; tmp$SC[tmp$n==7&tmp$S.obs==3] = 0.25; tmp$SC[tmp$n==7&tmp$S.obs==2] = 0.275; tmp$SC[tmp$n==7&tmp$S.obs==1] = 0.3
tmp$SC[tmp$n==8&tmp$S.obs==8] = 0.175; tmp$SC[tmp$n==8&tmp$S.obs==7] = 0.2; tmp$SC[tmp$n==8&tmp$S.obs==6] = 0.225; tmp$SC[tmp$n==8&tmp$S.obs==5] = 0.25; tmp$SC[tmp$n==8&tmp$S.obs==4] = 0.275; tmp$SC[tmp$n==8&tmp$S.obs==3] = 0.3; tmp$SC[tmp$n==8&tmp$S.obs==2] = 0.325; tmp$SC[tmp$n==8&tmp$S.obs==1] = 0.35
tmp$SC[tmp$n==9&tmp$S.obs==9] = 0.2; tmp$SC[tmp$n==9&tmp$S.obs==8] = 0.225; tmp$SC[tmp$n==9&tmp$S.obs==7] = 0.25; tmp$SC[tmp$n==9&tmp$S.obs==6] = 0.275; tmp$SC[tmp$n==9&tmp$S.obs==5] = 0.3; tmp$SC[tmp$n==9&tmp$S.obs==4] = 0.325; tmp$SC[tmp$n==9&tmp$S.obs==3] = 0.35; tmp$SC[tmp$n==9&tmp$S.obs==2] = 0.375; tmp$SC[tmp$n==9&tmp$S.obs==1] = 0.4
tmp$SC[tmp$n==12&tmp$S.obs==5]= 0.35
#plot(tmp$SC ~ log(tmp$n))
names(tmp)[1:4] = c("ID","N.ocas2","S.ocas2","SampCover.ocas2")
grid.result2 = merge(grid.result2, tmp[,c("ID","N.ocas2","S.ocas2","SampCover.ocas2")], by.x= "ID", by.y= "ID", all.x=TRUE)
# Weighted species richness
tmp = ocas.occs[!duplicated(paste(ocas.occs$species.correct2,ocas.occs$cell.ID,sep="_")),]
tmp$spp.range = 1/tmp$spp.range
tmp1 = aggregate(tmp$spp.range, list(tmp$cell.ID), sum, na.rm=TRUE)
names(tmp1) = c("ID","WE.ocas2")
grid.result2 = merge(grid.result2, tmp1, by.x= "ID", by.y= "ID", all.x=TRUE)
grid.result2$CWE.ocas2 = grid.result2$WE.ocas2/grid.result2$S.ocas2
# Calculating diversity indices based on raw occurrence data
# observed diversity, asymptotic estimates, estimated bootstrap s.e. and 95% confidence intervals 
#for Hill number (q) = 0, 1 and 2; see Chao et al. (2014) for asymptotic estimators.
tmp2 = as.data.frame.matrix(table(ocas.occs$species.correct2,ocas.occs$cell.ID))
tmp2 = tmp2[,apply(tmp2,2,sum)>=10] #removing grid cells with very low coverage
tmp2 = tmp2[apply(tmp2,1,sum)>=1,] #removing species present only in grid cells with very low coverage
rownames(tmp2) = gsub(" ","_",rownames(tmp2))
#asymptotic estimates for q= 0, 1 and 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp5 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3 = iNEXT(tmp2[,i], datatype="abundance", q = 2, size = 100, endpoint= 100, knots=2, se=FALSE, conf= cf)$AsyEst
  tmp4 = c(colnames(tmp2)[i], tmp3[,2][1], tmp3[,4][1],  tmp3[,5][1], # q= 0
           log(tmp3[,1][2]), tmp3[,2][2], tmp3[,4][2], tmp3[,5][2], # q= 1
           1 - 1/tmp3[,1][3], tmp3[,2][3], tmp3[,4][3], tmp3[,5][3]) # q= 2
  return(tmp4)
})
stopImplicitCluster()
stopCluster(cl)
tmp6 = do.call("rbind.data.frame",tmp5)
names(tmp6) = c("ID",paste(c("S.est","S.low","S.high","H","H.est","H.low","H.high","D","D.est","D.low","D.high"),
                           ".ocas2",sep=""))
for (i in 2:dim(tmp6)[2]) { tmp6[,i] = as.double(as.character(tmp6[,i]))}
#interpolation/extrapolation estimates for q= 0 and 2 (SR and Simpson)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("tmp2","boot","cf","m"),envir=environment())
tmp7 = parLapply(cl,1:dim(tmp2)[2], fun= function(i) {
  tmp3.1 = iNEXT(tmp2[,i], datatype="abundance", q=c(0,2), size=m, se=TRUE, nboot = boot, conf= cf)$iNextEst
  tmp3.1 = tmp3.1[tmp3.1$m %in% m, 4:6]
  tmp4.1 = c(as.double(tmp3.1[1,]), as.double(tmp3.1[2,]), as.double(tmp3.1[3,])) # q= 0
  tmp4.2 = c(as.double(tmp3.1[4,]), as.double(tmp3.1[5,]), as.double(tmp3.1[6,])) # q= 2
  return(c(tmp4.1,tmp4.2))
})
stopImplicitCluster()
stopCluster(cl)
tmp8 = do.call("rbind.data.frame",tmp7)
names(tmp8) = c(paste(c("qS","qS.LCL","qS.UCL"),rep(m,each=3),"ocas2",sep="."),
                paste(c("qD","qD.LCL","qD.UCL"),rep(m,each=3),"ocas2",sep="."))
tmp9 = cbind.data.frame(tmp6,tmp8)
grid.result2 = merge(grid.result2, tmp9, by.x= "ID", by.y= "ID", all.x=TRUE)

###############################################################################################################################################################################H
#########################################
#### Final edits of the grid summary ####
#########################################
# reordering the data frame
grid.result = grid.result[order(grid.result$order),]
grid.result2 = grid.result2[order(grid.result2$order),]

## Is there an effect of cell area on N.total, S.Total, S.est, H.est e D.est? Yes, for all variables!
toto = grid.result[grid.result$area_ha<249500,]
par(mfrow=c(1,1))
plot(toto$N.total ~ toto$area_ha)
abline(lm(toto$N.total ~ toto$area_ha))
plot(log(toto$S.total) ~ toto$area_ha)
abline(lm(log(toto$S.total) ~ toto$area_ha))
plot(log(toto$S.est.total) ~ toto$area_ha)
abline(lm(log(toto$S.est.total) ~ toto$area_ha))
anova(lm(log(toto$S.est.total) ~ toto$area_ha))
plot(toto$D.est.total ~ toto$area_ha)
abline(lm(toto$D.est.total ~ toto$area_ha), lwd=2)
anova(lm(toto$D.est.total ~ toto$area_ha))
plot(toto$H.est.total ~ toto$area_ha)
abline(lm(toto$H.est.total ~ toto$area_ha), lwd=2)
anova(lm(toto$H.est.total ~ toto$area_ha))
plot(toto$S.est.total ~ toto$area_ha)
abline(lm(toto$S.est.total ~ toto$area_ha), lwd=2)
anova(lm(toto$S.est.total ~ toto$area_ha))
plot(toto$qD.100.total ~ toto$area_ha)
abline(lm(toto$qD.100.total ~ toto$area_ha), lwd=2)
anova(lm(toto$qD.100.total ~ toto$area_ha))
plot(toto$CWE.pure ~ toto$area_ha)
abline(lm(toto$CWE.pure ~ toto$area_ha), lwd=2)
anova(lm(toto$CWE.pure ~ toto$area_ha))
plot(toto$CWE.pure.near ~ toto$area_ha)
abline(lm(toto$CWE.pure.near ~ toto$area_ha), lwd=2)
anova(lm(toto$CWE.pure.near ~ toto$area_ha))

### Defining the relationship between cell area and sampling intensity and correcting the diversity estimates ###
#function that perform the corrections
 # dados = grid.result
 # lim.area = 249500
 # nome.N = "N.pure"
 # nome.est = "S.est.pure"

correct.estimates = function(dados,lim.area,nome.N,nome.est,...) {
  # data for cells < 250000
  toto = dados[dados$area_ha<lim.area,]
  N = log(toto[,nome.N])
  ids = !is.na(N) & !is.infinite(N)
  area = toto$area_ha[ids]
  N = N[ids]
  # fitting and comparing the models: N ~ area
  mod.N = lm(N ~ area)
  mod.N1 = lm(N ~ area + I(area^2))
  aic = AICtab(mod.N,mod.N1,sort=FALSE)
  if(aic$dAIC[1]>log(8)) mod.final = mod.N1 else mod.final = mod.N
  par(mfrow=c(1,3), las=1,mar=c(4,4,1,0.5),mgp=c(2,0.5,0),tcl=-.3,...)
  plot(N ~ area)
  curve(predict(mod.final, newdata= data.frame(area= x)),add=TRUE, lwd=2, col=2)
  # calculating the corrected sampling effort per grid cell
  if(aic$dAIC[1]>log(8)) { 
    N.est = round(exp(coef(mod.final)[1] + coef(mod.final)[2]*area + coef(mod.final)[3]*area^2),2)
    N.max = round(exp(coef(mod.final)[1] + coef(mod.final)[2]*250000 + coef(mod.final)[3]*250000^2),2)
  } else {
    N.est = round(exp(coef(mod.final)[1] + coef(mod.final)[2]*area),2)
    N.max = round(exp(coef(mod.final)[1] + coef(mod.final)[2]*250000),2)
  }
  prop = (N.max - N.est)/N.est
  decay = try(nls(prop~a*exp(b*area),start=list(a=25,b=-0.0001)),TRUE)
  if(class(decay) == "try-error") decay = try(nls(prop~a*exp(b*area),start=list(a=10,b=-0.0001)),TRUE)
  #plot(prop ~ area);   curve(coef(decay)[1]*exp(coef(decay)[2]*x), lwd=2, col=2, add=T)
  prop[prop<0.0] = coef(decay)[1]*exp(coef(decay)[2]*area[prop<0.0])*0.5
  N.correct = toto[,nome.N][ids] + round(prop*toto[,nome.N][ids],0)

  # data for cells < 250000
  toto = dados[dados$area_ha>200000,]
  ids = !is.na(toto[,nome.N]) & !is.na(toto[,nome.est]) & !is.infinite(log(toto[,nome.N])) & !is.infinite(log(toto[,nome.est]))
  N1 = log(toto[,nome.N])[ids]
  est = log(toto[,nome.est])[ids]
  w = toto$SampCover[ids]
  # fitting and comparing the models: N ~ area
  mod.est  = lm(est ~ N1, weights = w^3)
  mod.est1 = lm(est ~ N1 + I(N1^2), weights = w^3)
  aic = AICtab(mod.est,mod.est1,sort=FALSE)
  if(aic$dAIC[1]>log(8)) mod.final1 = mod.est1 else mod.final1 = mod.est
  plot(est ~ N1)
  curve(predict(mod.final1, newdata= data.frame(N1= x)),add=TRUE, lwd=2, col=2)
  # calculating the corrected sampling effort per grid cell
  if(aic$dAIC[1]>log(8)) { 
    est.pred = round(exp(coef(mod.final1)[1] + coef(mod.final1)[2]*N + coef(mod.final1)[3]*N^2),4)
    est.est = round(exp(coef(mod.final1)[1] + coef(mod.final1)[2]*log(N.correct) + coef(mod.final1)[3]*log(N.correct)^2),4)
  } else {
    est.pred = round(exp(coef(mod.final1)[1] + coef(mod.final1)[2]*N),4)
    est.est = round(exp(coef(mod.final1)[1] + coef(mod.final1)[2]*log(N.correct)),4)
  }
  prop1 = (est.est - est.pred)/est.est
  decay = try(nls(prop1~a*exp(b*area),start=list(a=25,b=-0.0001)),TRUE)
  if(class(decay) == "try-error") decay = try(nls(prop1~a*exp(b*area),start=list(a=5,b=-0.0001)),TRUE)
  if(class(decay) == "try-error") decay = try(nls(prop1~a*exp(b*area),start=list(a=1,b=-0.0001)),TRUE)
  if(class(decay) == "try-error") decay = try(nls(prop1~a*exp(b*area),start=list(a=0.1,b=-0.0001)),TRUE)
  if(class(decay) == "try-error") decay = try(nls(prop1~a*exp(b*area),start=list(a=-0.2,b=-0.00001)),TRUE)
  #plot(prop1 ~ area);   curve(coef(decay)[1]*exp(coef(decay)[2]*x), lwd=2, col=2, add=T)
  prop1[prop1<0.0] = coef(decay)[1]*exp(coef(decay)[2]*area[prop1<0.0])*0.1
  
  #plot(prop1 ~ area)
  est.correct = dados[,nome.est]
  est.correct[dados$area_ha<lim.area & !is.na(dados[,nome.N])] = est.correct[dados$area_ha<lim.area & !is.na(dados[,nome.N])] +
    prop1 * est.correct[dados$area_ha<lim.area & !is.na(dados[,nome.N])]
  plot(est.correct ~ dados[,nome.est], xlab="estimate",ylab="corrected est.")
  legend("topleft", nome.est, cex=1.2, bty="n");abline(0,1,lty=2)
  par(mfrow=c(1,1))
  return(est.correct)
}
  
#generating the corrected estimates for each group
## TRUE and PROBABLE taxonomy
grid.result$S.total.correct = correct.estimates(grid.result,249500,"N.total","S.est.total")
grid.result$WE.total.correct = correct.estimates(grid.result,249500,"N.total","WE.total")
grid.result$CWE.total.correct = correct.estimates(grid.result,249500,"N.total","CWE.total")
grid.result$H.total.correct = correct.estimates(grid.result,249500,"N.total","H.est.total")
grid.result$D.total.correct = correct.estimates(grid.result,249500,"N.total","D.est.total")
grid.result$qS.50.total.correct = correct.estimates(grid.result,249500,"N.total","qS.50.total")
grid.result$qS.100.total.correct = correct.estimates(grid.result,249500,"N.total","qS.100.total")
grid.result$qD.50.total.correct = correct.estimates(grid.result,249500,"N.total","qD.50.total")
grid.result$qD.100.total.correct = correct.estimates(grid.result,249500,"N.total","qD.100.total")

grid.result$S.pure.correct = correct.estimates(grid.result,249500,"N.pure","S.est.pure")
grid.result$WE.pure.correct = correct.estimates(grid.result,249500,"N.pure","WE.pure")
grid.result$CWE.pure.correct = correct.estimates(grid.result,249500,"N.pure","CWE.pure")
grid.result$H.pure.correct = correct.estimates(grid.result,249500,"N.pure","H.est.pure")
grid.result$D.pure.correct = correct.estimates(grid.result,249500,"N.pure","D.est.pure")
grid.result$qS.50.pure.correct = correct.estimates(grid.result,249500,"N.pure","qS.50.pure")
grid.result$qS.100.pure.correct = correct.estimates(grid.result,249500,"N.pure","qS.100.pure")
grid.result$qD.50.pure.correct = correct.estimates(grid.result,249500,"N.pure","qD.50.pure")
grid.result$qD.100.pure.correct = correct.estimates(grid.result,249500,"N.pure","qD.100.pure")
grid.result$S.pure.correct2 = correct.estimates(grid.result,249500,"N.pure2","S.est.pure2")
grid.result$WE.pure.correct2 = correct.estimates(grid.result,249500,"N.pure2","WE.pure2")
grid.result$CWE.pure.correct2 = correct.estimates(grid.result,249500,"N.pure2","CWE.pure2")
grid.result$H.pure.correct2 = correct.estimates(grid.result,249500,"N.pure2","H.est.pure2")
grid.result$D.pure.correct2 = correct.estimates(grid.result,249500,"N.pure2","D.est.pure2")
grid.result$qS.50.pure.correct2 = correct.estimates(grid.result,249500,"N.pure2","qS.50.pure2")
grid.result$qS.100.pure.correct2 = correct.estimates(grid.result,249500,"N.pure2","qS.100.pure2")
grid.result$qD.50.pure.correct2 = correct.estimates(grid.result,249500,"N.pure2","qD.50.pure2")
grid.result$qD.100.pure.correct2 = correct.estimates(grid.result,249500,"N.pure2","qD.100.pure2")

grid.result$S.near.correct = correct.estimates(grid.result,249500,"N.near","S.est.near")
grid.result$WE.near.correct = correct.estimates(grid.result,249500,"N.near","WE.near")
grid.result$CWE.near.correct = correct.estimates(grid.result,249500,"N.near","CWE.near")
grid.result$H.near.correct = correct.estimates(grid.result,249500,"N.near","H.est.near")
grid.result$D.near.correct = correct.estimates(grid.result,249500,"N.near","D.est.near")
grid.result$qS.50.near.correct = correct.estimates(grid.result,249500,"N.near","qS.50.near")
grid.result$qS.100.near.correct = correct.estimates(grid.result,249500,"N.near","qS.100.near")
grid.result$qD.50.near.correct = correct.estimates(grid.result,249500,"N.near","qD.50.near")
grid.result$qD.100.near.correct = correct.estimates(grid.result,249500,"N.near","qD.100.near")
grid.result$S.near.correct2 = correct.estimates(grid.result,249500,"N.near2","S.est.near2")
grid.result$WE.near.correct2 = correct.estimates(grid.result,249500,"N.near2","WE.near2")
grid.result$CWE.near.correct2 = correct.estimates(grid.result,249500,"N.near2","CWE.near2")
grid.result$H.near.correct2 = correct.estimates(grid.result,249500,"N.near2","H.est.near2")
grid.result$D.near.correct2 = correct.estimates(grid.result,249500,"N.near2","D.est.near2")
grid.result$qS.50.near.correct2 = correct.estimates(grid.result,249500,"N.near2","qS.50.near2")
grid.result$qS.100.near.correct2 = correct.estimates(grid.result,249500,"N.near2","qS.100.near2")
grid.result$qD.50.near.correct2 = correct.estimates(grid.result,249500,"N.near2","qD.50.near2")
grid.result$qD.100.near.correct2 = correct.estimates(grid.result,249500,"N.near2","qD.100.near2")

grid.result$S.pure.near.correct = correct.estimates(grid.result,249500,"N.pure.near","S.est.pure.near")
grid.result$WE.pure.near.correct = correct.estimates(grid.result,249500,"N.pure.near","WE.pure.near")
grid.result$CWE.pure.near.correct = correct.estimates(grid.result,249500,"N.pure.near","CWE.pure.near")
grid.result$H.pure.near.correct = correct.estimates(grid.result,249500,"N.pure.near","H.est.pure.near")
grid.result$D.pure.near.correct = correct.estimates(grid.result,249500,"N.pure.near","D.est.pure.near")
grid.result$qS.50.pure.near.correct = correct.estimates(grid.result,249500,"N.pure.near","qS.50.pure.near")
grid.result$qS.100.pure.near.correct = correct.estimates(grid.result,249500,"N.pure.near","qS.100.pure.near")
grid.result$qD.50.pure.near.correct = correct.estimates(grid.result,249500,"N.pure.near","qD.50.pure.near")
grid.result$qD.100.pure.near.correct = correct.estimates(grid.result,249500,"N.pure.near","qD.100.pure.near")
grid.result$S.pure.near.correct2 = correct.estimates(grid.result,249500,"N.pure.near2","S.est.pure.near2")
grid.result$WE.pure.near.correct2 = correct.estimates(grid.result,249500,"N.pure.near2","WE.pure.near2")
grid.result$CWE.pure.near.correct2 = correct.estimates(grid.result,249500,"N.pure.near2","CWE.pure.near2")
grid.result$H.pure.near.correct2 = correct.estimates(grid.result,249500,"N.pure.near2","H.est.pure.near2")
grid.result$D.pure.near.correct2 = correct.estimates(grid.result,249500,"N.pure.near2","D.est.pure.near2")
grid.result$qS.50.pure.near.correct2 = correct.estimates(grid.result,249500,"N.pure.near2","qS.50.pure.near2")
grid.result$qS.100.pure.near.correct2 = correct.estimates(grid.result,249500,"N.pure.near2","qS.100.pure.near2")
grid.result$qD.50.pure.near.correct2 = correct.estimates(grid.result,249500,"N.pure.near2","qD.50.pure.near2")
grid.result$qD.100.pure.near.correct2 = correct.estimates(grid.result,249500,"N.pure.near2","qD.100.pure.near2")

grid.result$S.ocas.correct = correct.estimates(grid.result,249500,"N.ocas","S.est.ocas")
grid.result$WE.ocas.correct = correct.estimates(grid.result,249500,"N.ocas","WE.ocas")
grid.result$CWE.ocas.correct = correct.estimates(grid.result,249500,"N.ocas","CWE.ocas")
grid.result$H.ocas.correct = correct.estimates(grid.result,249500,"N.ocas","H.est.ocas")
grid.result$D.ocas.correct = correct.estimates(grid.result,249500,"N.ocas","D.est.ocas")
grid.result$qS.50.ocas.correct = correct.estimates(grid.result,249500,"N.ocas","qS.50.ocas")
grid.result$qS.100.ocas.correct = correct.estimates(grid.result,249500,"N.ocas","qS.100.ocas")
grid.result$qD.50.ocas.correct = correct.estimates(grid.result,249500,"N.ocas","qD.50.ocas")
grid.result$qD.100.ocas.correct = correct.estimates(grid.result,249500,"N.ocas","qD.100.ocas")
grid.result$S.ocas.correct2 = correct.estimates(grid.result,249500,"N.ocas2","S.est.ocas2")
grid.result$WE.ocas.correct2 = correct.estimates(grid.result,249500,"N.ocas2","WE.ocas2")
grid.result$CWE.ocas.correct2 = correct.estimates(grid.result,249500,"N.ocas2","CWE.ocas2")
grid.result$H.ocas.correct2 = correct.estimates(grid.result,249500,"N.ocas2","H.est.ocas2")
grid.result$D.ocas.correct2 = correct.estimates(grid.result,249500,"N.ocas2","D.est.ocas2")
grid.result$qS.50.ocas.correct2 = correct.estimates(grid.result,249500,"N.ocas2","qS.50.ocas2")
grid.result$qS.100.ocas.correct2 = correct.estimates(grid.result,249500,"N.ocas2","qS.100.ocas2")
grid.result$qD.50.ocas.correct2 = correct.estimates(grid.result,249500,"N.ocas2","qD.50.ocas2")
grid.result$qD.100.ocas.correct2 = correct.estimates(grid.result,249500,"N.ocas2","qD.100.ocas2")

## only TRUE taxonomy
grid.result2$S.total.correct = correct.estimates(grid.result2,249500,"N.total","S.est.total")
grid.result2$WE.total.correct = correct.estimates(grid.result2,249500,"N.total","WE.total")
grid.result2$CWE.total.correct = correct.estimates(grid.result2,249500,"N.total","CWE.total")
grid.result2$H.total.correct = correct.estimates(grid.result2,249500,"N.total","H.est.total")
grid.result2$D.total.correct = correct.estimates(grid.result2,249500,"N.total","D.est.total")
grid.result2$qS.50.total.correct = correct.estimates(grid.result2,249500,"N.total","qS.50.total")
grid.result2$qS.100.total.correct = correct.estimates(grid.result2,249500,"N.total","qS.100.total")
grid.result2$qD.50.total.correct = correct.estimates(grid.result2,249500,"N.total","qD.50.total")
grid.result2$qD.100.total.correct = correct.estimates(grid.result2,249500,"N.total","qD.100.total")

grid.result2$S.pure.correct = correct.estimates(grid.result2,249500,"N.pure","S.est.pure")
grid.result2$WE.pure.correct = correct.estimates(grid.result2,249500,"N.pure","WE.pure")
grid.result2$CWE.pure.correct = correct.estimates(grid.result2,249500,"N.pure","CWE.pure")
grid.result2$H.pure.correct = correct.estimates(grid.result2,249500,"N.pure","H.est.pure")
grid.result2$D.pure.correct = correct.estimates(grid.result2,249500,"N.pure","D.est.pure")
grid.result2$qS.50.pure.correct = correct.estimates(grid.result2,249500,"N.pure","qS.50.pure")
grid.result2$qS.100.pure.correct = correct.estimates(grid.result2,249500,"N.pure","qS.100.pure")
grid.result2$qD.50.pure.correct = correct.estimates(grid.result2,249500,"N.pure","qD.50.pure")
grid.result2$qD.100.pure.correct = correct.estimates(grid.result2,249500,"N.pure","qD.100.pure")
grid.result2$S.pure.correct2 = correct.estimates(grid.result2,249500,"N.pure2","S.est.pure2")
grid.result2$WE.pure.correct2 = correct.estimates(grid.result2,249500,"N.pure2","WE.pure2")
grid.result2$CWE.pure.correct2 = correct.estimates(grid.result2,249500,"N.pure2","CWE.pure2")
grid.result2$H.pure.correct2 = correct.estimates(grid.result2,249500,"N.pure2","H.est.pure2")
grid.result2$D.pure.correct2 = correct.estimates(grid.result2,249500,"N.pure2","D.est.pure2")
grid.result2$qS.50.pure.correct2 = correct.estimates(grid.result2,249500,"N.pure2","qS.50.pure2")
grid.result2$qS.100.pure.correct2 = correct.estimates(grid.result2,249500,"N.pure2","qS.100.pure2")
grid.result2$qD.50.pure.correct2 = correct.estimates(grid.result2,249500,"N.pure2","qD.50.pure2")
grid.result2$qD.100.pure.correct2 = correct.estimates(grid.result2,249500,"N.pure2","qD.100.pure2")

grid.result2$S.near.correct = correct.estimates(grid.result2,249500,"N.near","S.est.near")
grid.result2$WE.near.correct = correct.estimates(grid.result2,249500,"N.near","WE.near")
grid.result2$CWE.near.correct = correct.estimates(grid.result2,249500,"N.near","CWE.near")
grid.result2$H.near.correct = correct.estimates(grid.result2,249500,"N.near","H.est.near")
grid.result2$D.near.correct = correct.estimates(grid.result2,249500,"N.near","D.est.near")
grid.result2$qS.50.near.correct = correct.estimates(grid.result2,249500,"N.near","qS.50.near")
grid.result2$qS.100.near.correct = correct.estimates(grid.result2,249500,"N.near","qS.100.near")
grid.result2$qD.50.near.correct = correct.estimates(grid.result2,249500,"N.near","qD.50.near")
grid.result2$qD.100.near.correct = correct.estimates(grid.result2,249500,"N.near","qD.100.near")
grid.result2$S.near.correct2 = correct.estimates(grid.result2,249500,"N.near2","S.est.near2")
grid.result2$WE.near.correct2 = correct.estimates(grid.result2,249500,"N.near2","WE.near2")
grid.result2$CWE.near.correct2 = correct.estimates(grid.result2,249500,"N.near2","CWE.near2")
grid.result2$H.near.correct2 = correct.estimates(grid.result2,249500,"N.near2","H.est.near2")
grid.result2$D.near.correct2 = correct.estimates(grid.result2,249500,"N.near2","D.est.near2")
grid.result2$qS.50.near.correct2 = correct.estimates(grid.result2,249500,"N.near2","qS.50.near2")
grid.result2$qS.100.near.correct2 = correct.estimates(grid.result2,249500,"N.near2","qS.100.near2")
grid.result2$qD.50.near.correct2 = correct.estimates(grid.result2,249500,"N.near2","qD.50.near2")
grid.result2$qD.100.near.correct2 = correct.estimates(grid.result2,249500,"N.near2","qD.100.near2")

grid.result2$S.pure.near.correct = correct.estimates(grid.result2,249500,"N.pure.near","S.est.pure.near")
grid.result2$WE.pure.near.correct = correct.estimates(grid.result2,249500,"N.pure.near","WE.pure.near")
grid.result2$CWE.pure.near.correct = correct.estimates(grid.result2,249500,"N.pure.near","CWE.pure.near")
grid.result2$H.pure.near.correct = correct.estimates(grid.result2,249500,"N.pure.near","H.est.pure.near")
grid.result2$D.pure.near.correct = correct.estimates(grid.result2,249500,"N.pure.near","D.est.pure.near")
grid.result2$qS.50.pure.near.correct = correct.estimates(grid.result2,249500,"N.pure.near","qS.50.pure.near")
grid.result2$qS.100.pure.near.correct = correct.estimates(grid.result2,249500,"N.pure.near","qS.100.pure.near")
grid.result2$qD.50.pure.near.correct = correct.estimates(grid.result2,249500,"N.pure.near","qD.50.pure.near")
grid.result2$qD.100.pure.near.correct = correct.estimates(grid.result2,249500,"N.pure.near","qD.100.pure.near")
grid.result2$S.pure.near.correct2 = correct.estimates(grid.result2,249500,"N.pure.near2","S.est.pure.near2")
grid.result2$WE.pure.near.correct2 = correct.estimates(grid.result2,249500,"N.pure.near2","WE.pure.near2")
grid.result2$CWE.pure.near.correct2 = correct.estimates(grid.result2,249500,"N.pure.near2","CWE.pure.near2")
grid.result2$H.pure.near.correct2 = correct.estimates(grid.result2,249500,"N.pure.near2","H.est.pure.near2")
grid.result2$D.pure.near.correct2 = correct.estimates(grid.result2,249500,"N.pure.near2","D.est.pure.near2")
grid.result2$qS.50.pure.near.correct2 = correct.estimates(grid.result2,249500,"N.pure.near2","qS.50.pure.near2")
grid.result2$qS.100.pure.near.correct2 = correct.estimates(grid.result2,249500,"N.pure.near2","qS.100.pure.near2")
grid.result2$qD.50.pure.near.correct2 = correct.estimates(grid.result2,249500,"N.pure.near2","qD.50.pure.near2")
grid.result2$qD.100.pure.near.correct2 = correct.estimates(grid.result2,249500,"N.pure.near2","qD.100.pure.near2")

grid.result2$S.ocas.correct = correct.estimates(grid.result2,249500,"N.ocas","S.est.ocas")
grid.result2$WE.ocas.correct = correct.estimates(grid.result2,249500,"N.ocas","WE.ocas")
grid.result2$CWE.ocas.correct = correct.estimates(grid.result2,249500,"N.ocas","CWE.ocas")
grid.result2$H.ocas.correct = correct.estimates(grid.result2,249500,"N.ocas","H.est.ocas")
grid.result2$D.ocas.correct = correct.estimates(grid.result2,249500,"N.ocas","D.est.ocas")
grid.result2$qS.50.ocas.correct = correct.estimates(grid.result2,249500,"N.ocas","qS.50.ocas")
grid.result2$qS.100.ocas.correct = correct.estimates(grid.result2,249500,"N.ocas","qS.100.ocas")
grid.result2$qD.50.ocas.correct = correct.estimates(grid.result2,249500,"N.ocas","qD.50.ocas")
grid.result2$qD.100.ocas.correct = correct.estimates(grid.result2,249500,"N.ocas","qD.100.ocas")
grid.result2$S.ocas.correct2 = correct.estimates(grid.result2,249500,"N.ocas2","S.est.ocas2")
grid.result2$WE.ocas.correct2 = correct.estimates(grid.result2,249500,"N.ocas2","WE.ocas2")
grid.result2$CWE.ocas.correct2 = correct.estimates(grid.result2,249500,"N.ocas2","CWE.ocas2")
grid.result2$H.ocas.correct2 = correct.estimates(grid.result2,249500,"N.ocas2","H.est.ocas2")
grid.result2$D.ocas.correct2 = correct.estimates(grid.result2,249500,"N.ocas2","D.est.ocas2")
grid.result2$qS.50.ocas.correct2 = correct.estimates(grid.result2,249500,"N.ocas2","qS.50.ocas2")
grid.result2$qS.100.ocas.correct2 = correct.estimates(grid.result2,249500,"N.ocas2","qS.100.ocas2")
grid.result2$qD.50.ocas.correct2 = correct.estimates(grid.result2,249500,"N.ocas2","qD.50.ocas2")
grid.result2$qD.100.ocas.correct2 = correct.estimates(grid.result2,249500,"N.ocas2","qD.100.ocas2")

### Correcting some problematic estimates (e.g. S.pure > S.total)
## TRUE and PROBABLE taxonomy
grid.result$D.est.pure[grid.result$D.est.pure>grid.result$D.est.total & !is.na(grid.result$D.est.pure)] = 
  grid.result$D.est.total[grid.result$D.est.pure>grid.result$D.est.total & !is.na(grid.result$D.est.pure)]
grid.result$D.est.near[grid.result$D.est.near>grid.result$D.est.total & !is.na(grid.result$D.est.near)] = 
  grid.result$D.est.total[grid.result$D.est.near>grid.result$D.est.total & !is.na(grid.result$D.est.near)]
grid.result$D.est.pure.near[grid.result$D.est.pure.near>grid.result$D.est.total & !is.na(grid.result$D.est.pure.near)] = 
  grid.result$D.est.total[grid.result$D.est.pure.near>grid.result$D.est.total & !is.na(grid.result$D.est.pure.near)]
grid.result$D.pure.correct[grid.result$D.pure.correct>grid.result$D.total.correct & !is.na(grid.result$D.pure.correct)] = 
  grid.result$D.total.correct[grid.result$D.pure.correct>grid.result$D.total.correct & !is.na(grid.result$D.pure.correct)]
grid.result$D.near.correct[grid.result$D.near.correct>grid.result$D.total.correct & !is.na(grid.result$D.near.correct)] = 
  grid.result$D.total.correct[grid.result$D.near.correct>grid.result$D.total.correct & !is.na(grid.result$D.near.correct)]
grid.result$D.pure.near.correct[grid.result$D.pure.near.correct>grid.result$D.total.correct & !is.na(grid.result$D.pure.near.correct)] = 
  grid.result$D.total.correct[grid.result$D.pure.near.correct>grid.result$D.total.correct & !is.na(grid.result$D.pure.near.correct)]
grid.result$qS.50.pure.correct[grid.result$qS.50.pure.correct>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.pure.correct)] = 
  grid.result$qS.50.total.correct[grid.result$qS.50.pure.correct>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.pure.correct)]
grid.result$qS.100.pure.correct[grid.result$qS.100.pure.correct>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.pure.correct)] =
  grid.result$qS.100.total.correct[grid.result$qS.100.pure.correct>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.pure.correct)]
grid.result$qD.50.pure.correct[grid.result$qD.50.pure.correct>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.pure.correct)] = 
  grid.result$qD.50.total.correct[grid.result$qD.50.pure.correct>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.pure.correct)]
grid.result$qD.100.pure.correct[grid.result$qD.100.pure.correct>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.pure.correct)] =
  grid.result$qD.100.total.correct[grid.result$qD.100.pure.correct>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.pure.correct)]
grid.result$qS.50.near.correct[grid.result$qS.50.near.correct>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.near.correct)] = 
  grid.result$qS.50.total.correct[grid.result$qS.50.near.correct>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.near.correct)]
grid.result$qS.100.near.correct[grid.result$qS.100.near.correct>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.near.correct)] =
  grid.result$qS.100.total.correct[grid.result$qS.100.near.correct>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.near.correct)]
grid.result$qD.50.near.correct[grid.result$qD.50.near.correct>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.near.correct)] = 
  grid.result$qD.50.total.correct[grid.result$qD.50.near.correct>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.near.correct)]
grid.result$qD.100.near.correct[grid.result$qD.100.near.correct>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.near.correct)] =
  grid.result$qD.100.total.correct[grid.result$qD.100.near.correct>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.near.correct)]
grid.result$qS.50.pure.near.correct[grid.result$qS.50.pure.near.correct>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.pure.near.correct)] = 
  grid.result$qS.50.total.correct[grid.result$qS.50.pure.near.correct>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.pure.near.correct)]
grid.result$qS.100.pure.near.correct[grid.result$qS.100.pure.near.correct>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.pure.near.correct)] =
  grid.result$qS.100.total.correct[grid.result$qS.100.pure.near.correct>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.pure.near.correct)]
grid.result$qD.50.pure.near.correct[grid.result$qD.50.pure.near.correct>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.pure.near.correct)] = 
  grid.result$qD.50.total.correct[grid.result$qD.50.pure.near.correct>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.pure.near.correct)]
grid.result$qD.100.pure.near.correct[grid.result$qD.100.pure.near.correct>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.pure.near.correct)] =
  grid.result$qD.100.total.correct[grid.result$qD.100.pure.near.correct>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.pure.near.correct)]

grid.result$D.est.pure2[grid.result$D.est.pure2>grid.result$D.est.total & !is.na(grid.result$D.est.pure2)] = 
  grid.result$D.est.total[grid.result$D.est.pure2>grid.result$D.est.total & !is.na(grid.result$D.est.pure2)]
grid.result$D.est.near2[grid.result$D.est.near2>grid.result$D.est.total & !is.na(grid.result$D.est.near2)] = 
  grid.result$D.est.total[grid.result$D.est.near2>grid.result$D.est.total & !is.na(grid.result$D.est.near2)]
grid.result$D.est.pure.near2[grid.result$D.est.pure.near2>grid.result$D.est.total & !is.na(grid.result$D.est.pure.near2)] = 
  grid.result$D.est.total[grid.result$D.est.pure.near2>grid.result$D.est.total & !is.na(grid.result$D.est.pure.near2)]
grid.result$D.pure.correct2[grid.result$D.pure.correct2>grid.result$D.total.correct & !is.na(grid.result$D.pure.correct2)] = 
  grid.result$D.total.correct[grid.result$D.pure.correct2>grid.result$D.total.correct & !is.na(grid.result$D.pure.correct2)]
grid.result$D.near.correct2[grid.result$D.near.correct2>grid.result$D.total.correct & !is.na(grid.result$D.near.correct2)] = 
  grid.result$D.total.correct[grid.result$D.near.correct2>grid.result$D.total.correct & !is.na(grid.result$D.near.correct2)]
grid.result$D.pure.near.correct2[grid.result$D.pure.near.correct2>grid.result$D.total.correct & !is.na(grid.result$D.pure.near.correct2)] = 
  grid.result$D.total.correct[grid.result$D.pure.near.correct2>grid.result$D.total.correct & !is.na(grid.result$D.pure.near.correct2)]
grid.result$qS.50.pure.correct2[grid.result$qS.50.pure.correct2>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.pure.correct2)] = 
  grid.result$qS.50.total.correct[grid.result$qS.50.pure.correct2>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.pure.correct2)]
grid.result$qS.100.pure.correct2[grid.result$qS.100.pure.correct2>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.pure.correct2)] =
  grid.result$qS.100.total.correct[grid.result$qS.100.pure.correct2>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.pure.correct2)]
grid.result$qD.50.pure.correct2[grid.result$qD.50.pure.correct2>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.pure.correct2)] = 
  grid.result$qD.50.total.correct[grid.result$qD.50.pure.correct2>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.pure.correct2)]
grid.result$qD.100.pure.correct2[grid.result$qD.100.pure.correct2>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.pure.correct2)] =
  grid.result$qD.100.total.correct[grid.result$qD.100.pure.correct2>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.pure.correct2)]
grid.result$qS.50.near.correct2[grid.result$qS.50.near.correct2>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.near.correct2)] = 
  grid.result$qS.50.total.correct[grid.result$qS.50.near.correct2>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.near.correct2)]
grid.result$qS.100.near.correct2[grid.result$qS.100.near.correct2>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.near.correct2)] =
  grid.result$qS.100.total.correct[grid.result$qS.100.near.correct2>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.near.correct2)]
grid.result$qD.50.near.correct2[grid.result$qD.50.near.correct2>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.near.correct2)] = 
  grid.result$qD.50.total.correct[grid.result$qD.50.near.correct2>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.near.correct2)]
grid.result$qD.100.near.correct2[grid.result$qD.100.near.correct2>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.near.correct2)] =
  grid.result$qD.100.total.correct[grid.result$qD.100.near.correct2>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.near.correct2)]
grid.result$qS.50.pure.near.correct2[grid.result$qS.50.pure.near.correct2>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.pure.near.correct2)] = 
  grid.result$qS.50.total.correct[grid.result$qS.50.pure.near.correct2>grid.result$qS.50.total.correct & !is.na(grid.result$qS.50.pure.near.correct2)]
grid.result$qS.100.pure.near.correct2[grid.result$qS.100.pure.near.correct2>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.pure.near.correct2)] =
  grid.result$qS.100.total.correct[grid.result$qS.100.pure.near.correct2>grid.result$qS.100.total.correct & !is.na(grid.result$qS.100.pure.near.correct2)]
grid.result$qD.50.pure.near.correct2[grid.result$qD.50.pure.near.correct2>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.pure.near.correct2)] = 
  grid.result$qD.50.total.correct[grid.result$qD.50.pure.near.correct2>grid.result$qD.50.total.correct & !is.na(grid.result$qD.50.pure.near.correct2)]
grid.result$qD.100.pure.near.correct2[grid.result$qD.100.pure.near.correct2>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.pure.near.correct2)] =
  grid.result$qD.100.total.correct[grid.result$qD.100.pure.near.correct2>grid.result$qD.100.total.correct & !is.na(grid.result$qD.100.pure.near.correct2)]

grid.result$WE.pure[grid.result$WE.pure>grid.result$WE.total & !is.na(grid.result$WE.pure)] = 
  grid.result$WE.total[grid.result$WE.pure>grid.result$WE.total & !is.na(grid.result$WE.pure)]
grid.result$WE.pure2[grid.result$WE.pure2>grid.result$WE.total & !is.na(grid.result$WE.pure2)] = 
  grid.result$WE.total[grid.result$WE.pure2>grid.result$WE.total & !is.na(grid.result$WE.pure2)]
grid.result$CWE.pure[grid.result$CWE.pure>grid.result$CWE.total & !is.na(grid.result$CWE.pure)] = 
  grid.result$CWE.total[grid.result$CWE.pure>grid.result$CWE.total & !is.na(grid.result$CWE.pure)]
grid.result$CWE.pure2[grid.result$CWE.pure2>grid.result$CWE.total & !is.na(grid.result$CWE.pure2)] = 
  grid.result$CWE.total[grid.result$CWE.pure2>grid.result$CWE.total & !is.na(grid.result$CWE.pure2)]
grid.result$WE.near[grid.result$WE.near>grid.result$WE.total & !is.na(grid.result$WE.near)] = 
  grid.result$WE.total[grid.result$WE.near>grid.result$WE.total & !is.na(grid.result$WE.near)]
grid.result$WE.near2[grid.result$WE.near2>grid.result$WE.total & !is.na(grid.result$WE.near2)] = 
  grid.result$WE.total[grid.result$WE.near2>grid.result$WE.total & !is.na(grid.result$WE.near2)]
grid.result$CWE.near[grid.result$CWE.near>grid.result$CWE.total & !is.na(grid.result$CWE.near)] = 
  grid.result$CWE.total[grid.result$CWE.near>grid.result$CWE.total & !is.na(grid.result$CWE.near)]
grid.result$CWE.near2[grid.result$CWE.near2>grid.result$CWE.total & !is.na(grid.result$CWE.near2)] = 
  grid.result$CWE.total[grid.result$CWE.near2>grid.result$CWE.total & !is.na(grid.result$CWE.near2)]
grid.result$WE.pure.near[grid.result$WE.pure.near>grid.result$WE.total & !is.na(grid.result$WE.pure.near)] = 
  grid.result$WE.total[grid.result$WE.pure.near>grid.result$WE.total & !is.na(grid.result$WE.pure.near)]
grid.result$WE.pure.near2[grid.result$WE.pure.near2>grid.result$WE.total & !is.na(grid.result$WE.pure.near2)] = 
  grid.result$WE.total[grid.result$WE.pure.near2>grid.result$WE.total & !is.na(grid.result$WE.pure.near2)]
grid.result$CWE.pure.near[grid.result$CWE.pure.near>grid.result$CWE.total & !is.na(grid.result$CWE.pure.near)] = 
  grid.result$CWE.total[grid.result$CWE.pure.near>grid.result$CWE.total & !is.na(grid.result$CWE.pure.near)]
grid.result$CWE.pure.near2[grid.result$CWE.pure.near2>grid.result$CWE.total & !is.na(grid.result$CWE.pure.near2)] = 
  grid.result$CWE.total[grid.result$CWE.pure.near2>grid.result$CWE.total & !is.na(grid.result$CWE.pure.near2)]

## only TRUE taxonomy
grid.result2$D.est.pure[grid.result2$D.est.pure>grid.result2$D.est.total & !is.na(grid.result2$D.est.pure)] = 
  grid.result2$D.est.total[grid.result2$D.est.pure>grid.result2$D.est.total & !is.na(grid.result2$D.est.pure)]
grid.result2$D.est.near[grid.result2$D.est.near>grid.result2$D.est.total & !is.na(grid.result2$D.est.near)] = 
  grid.result2$D.est.total[grid.result2$D.est.near>grid.result2$D.est.total & !is.na(grid.result2$D.est.near)]
grid.result2$D.est.pure.near[grid.result2$D.est.pure.near>grid.result2$D.est.total & !is.na(grid.result2$D.est.pure.near)] = 
  grid.result2$D.est.total[grid.result2$D.est.pure.near>grid.result2$D.est.total & !is.na(grid.result2$D.est.pure.near)]
grid.result2$D.pure.correct[grid.result2$D.pure.correct>grid.result2$D.total.correct & !is.na(grid.result2$D.pure.correct)] = 
  grid.result2$D.total.correct[grid.result2$D.pure.correct>grid.result2$D.total.correct & !is.na(grid.result2$D.pure.correct)]
grid.result2$D.near.correct[grid.result2$D.near.correct>grid.result2$D.total.correct & !is.na(grid.result2$D.near.correct)] = 
  grid.result2$D.total.correct[grid.result2$D.near.correct>grid.result2$D.total.correct & !is.na(grid.result2$D.near.correct)]
grid.result2$D.pure.near.correct[grid.result2$D.pure.near.correct>grid.result2$D.total.correct & !is.na(grid.result2$D.pure.near.correct)] = 
  grid.result2$D.total.correct[grid.result2$D.pure.near.correct>grid.result2$D.total.correct & !is.na(grid.result2$D.pure.near.correct)]
grid.result2$qS.50.pure.correct[grid.result2$qS.50.pure.correct>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.pure.correct)] = 
  grid.result2$qS.50.total.correct[grid.result2$qS.50.pure.correct>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.pure.correct)]
grid.result2$qS.100.pure.correct[grid.result2$qS.100.pure.correct>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.pure.correct)] =
  grid.result2$qS.100.total.correct[grid.result2$qS.100.pure.correct>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.pure.correct)]
grid.result2$qD.50.pure.correct[grid.result2$qD.50.pure.correct>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.pure.correct)] = 
  grid.result2$qD.50.total.correct[grid.result2$qD.50.pure.correct>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.pure.correct)]
grid.result2$qD.100.pure.correct[grid.result2$qD.100.pure.correct>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.pure.correct)] =
  grid.result2$qD.100.total.correct[grid.result2$qD.100.pure.correct>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.pure.correct)]
grid.result2$qS.50.near.correct[grid.result2$qS.50.near.correct>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.near.correct)] = 
  grid.result2$qS.50.total.correct[grid.result2$qS.50.near.correct>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.near.correct)]
grid.result2$qS.100.near.correct[grid.result2$qS.100.near.correct>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.near.correct)] =
  grid.result2$qS.100.total.correct[grid.result2$qS.100.near.correct>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.near.correct)]
grid.result2$qD.50.near.correct[grid.result2$qD.50.near.correct>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.near.correct)] = 
  grid.result2$qD.50.total.correct[grid.result2$qD.50.near.correct>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.near.correct)]
grid.result2$qD.100.near.correct[grid.result2$qD.100.near.correct>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.near.correct)] =
  grid.result2$qD.100.total.correct[grid.result2$qD.100.near.correct>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.near.correct)]
grid.result2$qS.50.pure.near.correct[grid.result2$qS.50.pure.near.correct>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.pure.near.correct)] = 
  grid.result2$qS.50.total.correct[grid.result2$qS.50.pure.near.correct>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.pure.near.correct)]
grid.result2$qS.100.pure.near.correct[grid.result2$qS.100.pure.near.correct>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.pure.near.correct)] =
  grid.result2$qS.100.total.correct[grid.result2$qS.100.pure.near.correct>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.pure.near.correct)]
grid.result2$qD.50.pure.near.correct[grid.result2$qD.50.pure.near.correct>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.pure.near.correct)] = 
  grid.result2$qD.50.total.correct[grid.result2$qD.50.pure.near.correct>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.pure.near.correct)]
grid.result2$qD.100.pure.near.correct[grid.result2$qD.100.pure.near.correct>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.pure.near.correct)] =
  grid.result2$qD.100.total.correct[grid.result2$qD.100.pure.near.correct>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.pure.near.correct)]

grid.result2$D.est.pure2[grid.result2$D.est.pure2>grid.result2$D.est.total & !is.na(grid.result2$D.est.pure2)] = 
  grid.result2$D.est.total[grid.result2$D.est.pure2>grid.result2$D.est.total & !is.na(grid.result2$D.est.pure2)]
grid.result2$D.est.near2[grid.result2$D.est.near2>grid.result2$D.est.total & !is.na(grid.result2$D.est.near2)] = 
  grid.result2$D.est.total[grid.result2$D.est.near2>grid.result2$D.est.total & !is.na(grid.result2$D.est.near2)]
grid.result2$D.est.pure.near2[grid.result2$D.est.pure.near2>grid.result2$D.est.total & !is.na(grid.result2$D.est.pure.near2)] = 
  grid.result2$D.est.total[grid.result2$D.est.pure.near2>grid.result2$D.est.total & !is.na(grid.result2$D.est.pure.near2)]
grid.result2$D.pure.correct2[grid.result2$D.pure.correct2>grid.result2$D.total.correct & !is.na(grid.result2$D.pure.correct2)] = 
  grid.result2$D.total.correct[grid.result2$D.pure.correct2>grid.result2$D.total.correct & !is.na(grid.result2$D.pure.correct2)]
grid.result2$D.near.correct2[grid.result2$D.near.correct2>grid.result2$D.total.correct & !is.na(grid.result2$D.near.correct2)] = 
  grid.result2$D.total.correct[grid.result2$D.near.correct2>grid.result2$D.total.correct & !is.na(grid.result2$D.near.correct2)]
grid.result2$D.pure.near.correct2[grid.result2$D.pure.near.correct2>grid.result2$D.total.correct & !is.na(grid.result2$D.pure.near.correct2)] = 
  grid.result2$D.total.correct[grid.result2$D.pure.near.correct2>grid.result2$D.total.correct & !is.na(grid.result2$D.pure.near.correct2)]
grid.result2$qS.50.pure.correct2[grid.result2$qS.50.pure.correct2>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.pure.correct2)] = 
  grid.result2$qS.50.total.correct[grid.result2$qS.50.pure.correct2>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.pure.correct2)]
grid.result2$qS.100.pure.correct2[grid.result2$qS.100.pure.correct2>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.pure.correct2)] =
  grid.result2$qS.100.total.correct[grid.result2$qS.100.pure.correct2>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.pure.correct2)]
grid.result2$qD.50.pure.correct2[grid.result2$qD.50.pure.correct2>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.pure.correct2)] = 
  grid.result2$qD.50.total.correct[grid.result2$qD.50.pure.correct2>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.pure.correct2)]
grid.result2$qD.100.pure.correct2[grid.result2$qD.100.pure.correct2>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.pure.correct2)] =
  grid.result2$qD.100.total.correct[grid.result2$qD.100.pure.correct2>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.pure.correct2)]
grid.result2$qS.50.near.correct2[grid.result2$qS.50.near.correct2>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.near.correct2)] = 
  grid.result2$qS.50.total.correct[grid.result2$qS.50.near.correct2>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.near.correct2)]
grid.result2$qS.100.near.correct2[grid.result2$qS.100.near.correct2>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.near.correct2)] =
  grid.result2$qS.100.total.correct[grid.result2$qS.100.near.correct2>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.near.correct2)]
grid.result2$qD.50.near.correct2[grid.result2$qD.50.near.correct2>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.near.correct2)] = 
  grid.result2$qD.50.total.correct[grid.result2$qD.50.near.correct2>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.near.correct2)]
grid.result2$qD.100.near.correct2[grid.result2$qD.100.near.correct2>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.near.correct2)] =
  grid.result2$qD.100.total.correct[grid.result2$qD.100.near.correct2>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.near.correct2)]
grid.result2$qS.50.pure.near.correct2[grid.result2$qS.50.pure.near.correct2>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.pure.near.correct2)] = 
  grid.result2$qS.50.total.correct[grid.result2$qS.50.pure.near.correct2>grid.result2$qS.50.total.correct & !is.na(grid.result2$qS.50.pure.near.correct2)]
grid.result2$qS.100.pure.near.correct2[grid.result2$qS.100.pure.near.correct2>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.pure.near.correct2)] =
  grid.result2$qS.100.total.correct[grid.result2$qS.100.pure.near.correct2>grid.result2$qS.100.total.correct & !is.na(grid.result2$qS.100.pure.near.correct2)]
grid.result2$qD.50.pure.near.correct2[grid.result2$qD.50.pure.near.correct2>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.pure.near.correct2)] = 
  grid.result2$qD.50.total.correct[grid.result2$qD.50.pure.near.correct2>grid.result2$qD.50.total.correct & !is.na(grid.result2$qD.50.pure.near.correct2)]
grid.result2$qD.100.pure.near.correct2[grid.result2$qD.100.pure.near.correct2>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.pure.near.correct2)] =
  grid.result2$qD.100.total.correct[grid.result2$qD.100.pure.near.correct2>grid.result2$qD.100.total.correct & !is.na(grid.result2$qD.100.pure.near.correct2)]

grid.result2$WE.pure[grid.result2$WE.pure>grid.result2$WE.total & !is.na(grid.result2$WE.pure)] = 
  grid.result2$WE.total[grid.result2$WE.pure>grid.result2$WE.total & !is.na(grid.result2$WE.pure)]
grid.result2$WE.pure2[grid.result2$WE.pure2>grid.result2$WE.total & !is.na(grid.result2$WE.pure2)] = 
  grid.result2$WE.total[grid.result2$WE.pure2>grid.result2$WE.total & !is.na(grid.result2$WE.pure2)]
grid.result2$CWE.pure[grid.result2$CWE.pure>grid.result2$CWE.total & !is.na(grid.result2$CWE.pure)] = 
  grid.result2$CWE.total[grid.result2$CWE.pure>grid.result2$CWE.total & !is.na(grid.result2$CWE.pure)]
grid.result2$CWE.pure2[grid.result2$CWE.pure2>grid.result2$CWE.total & !is.na(grid.result2$CWE.pure2)] = 
  grid.result2$CWE.total[grid.result2$CWE.pure2>grid.result2$CWE.total & !is.na(grid.result2$CWE.pure2)]
grid.result2$WE.near[grid.result2$WE.near>grid.result2$WE.total & !is.na(grid.result2$WE.near)] = 
  grid.result2$WE.total[grid.result2$WE.near>grid.result2$WE.total & !is.na(grid.result2$WE.near)]
grid.result2$WE.near2[grid.result2$WE.near2>grid.result2$WE.total & !is.na(grid.result2$WE.near2)] = 
  grid.result2$WE.total[grid.result2$WE.near2>grid.result2$WE.total & !is.na(grid.result2$WE.near2)]
grid.result2$CWE.near[grid.result2$CWE.near>grid.result2$CWE.total & !is.na(grid.result2$CWE.near)] = 
  grid.result2$CWE.total[grid.result2$CWE.near>grid.result2$CWE.total & !is.na(grid.result2$CWE.near)]
grid.result2$CWE.near2[grid.result2$CWE.near2>grid.result2$CWE.total & !is.na(grid.result2$CWE.near2)] = 
  grid.result2$CWE.total[grid.result2$CWE.near2>grid.result2$CWE.total & !is.na(grid.result2$CWE.near2)]
grid.result2$WE.pure.near[grid.result2$WE.pure.near>grid.result2$WE.total & !is.na(grid.result2$WE.pure.near)] = 
  grid.result2$WE.total[grid.result2$WE.pure.near>grid.result2$WE.total & !is.na(grid.result2$WE.pure.near)]
grid.result2$WE.pure.near2[grid.result2$WE.pure.near2>grid.result2$WE.total & !is.na(grid.result2$WE.pure.near2)] = 
  grid.result2$WE.total[grid.result2$WE.pure.near2>grid.result2$WE.total & !is.na(grid.result2$WE.pure.near2)]
grid.result2$CWE.pure.near[grid.result2$CWE.pure.near>grid.result2$CWE.total & !is.na(grid.result2$CWE.pure.near)] = 
  grid.result2$CWE.total[grid.result2$CWE.pure.near>grid.result2$CWE.total & !is.na(grid.result2$CWE.pure.near)]
grid.result2$CWE.pure.near2[grid.result2$CWE.pure.near2>grid.result2$CWE.total & !is.na(grid.result2$CWE.pure.near2)] = 
  grid.result2$CWE.total[grid.result2$CWE.pure.near2>grid.result2$CWE.total & !is.na(grid.result2$CWE.pure.near2)]

### replacing estimates that remained NAs by the observed diversity measure
### NOT REALLY NEEDED: WAS ONLY USED FOR PLOTTING PURPOSES BUT ALL ANALYSIS EXCLUDE GRID CELLS WITH LOW SAMPLING COVERAGE
#Species richness (q=0)
## TRUE AND PROBABLE taxonomy
#nls(S.total.correct ~ a*exp(b*S.total), start=list(a=2,b=0.3),data = subset(grid.result,grid.result$N.total<15))
grid.result$S.total.correct[is.na(grid.result$S.total.correct) & !is.na(grid.result$S.total)] = 
  round(2.5*exp(0.29*grid.result$S.total[is.na(grid.result$S.total.correct) & !is.na(grid.result$S.total)]),2)
#nls(S.pure.correct ~ a*exp(b*S.pure), start=list(a=2,b=0.3),data = subset(grid.result,grid.result$N.pure<15))
grid.result$S.pure.correct[is.na(grid.result$S.pure.correct) & !is.na(grid.result$S.pure)] = 
  round(1.1*exp(0.29*grid.result$S.pure[is.na(grid.result$S.pure.correct) & !is.na(grid.result$S.pure)]),2)
#nls(S.near.correct ~ a*exp(b*S.near), start=list(a=1,b=0.5),data = subset(grid.result,grid.result$N.near<15))
grid.result$S.near.correct[is.na(grid.result$S.near.correct) & !is.na(grid.result$S.near)] = 
  round(1.25*exp(0.29*grid.result$S.near[is.na(grid.result$S.near.correct) & !is.na(grid.result$S.near)]),2)
#nls(S.pure.near.correct ~ a*exp(b*S.pure.near), start=list(a=1,b=0.5),data = subset(grid.result,grid.result$N.pure.near<15))
grid.result$S.pure.near.correct[is.na(grid.result$S.pure.near.correct) & !is.na(grid.result$S.pure.near)] = 
  round(2*exp(0.29*grid.result$S.pure.near[is.na(grid.result$S.pure.near.correct) & !is.na(grid.result$S.pure.near)]),2)
#nls(S.ocas.correct ~ a*exp(b*S.ocas), start=list(a=1,b=0.5),data = subset(grid.result,grid.result$N.ocas<15))
grid.result$S.ocas.correct[is.na(grid.result$S.ocas.correct) & !is.na(grid.result$S.ocas)] = 
  round(1.5*exp(0.29*grid.result$S.ocas[is.na(grid.result$S.ocas.correct) & !is.na(grid.result$S.ocas)]),2)
grid.result$S.pure.correct2[is.na(grid.result$S.pure.correct2) & !is.na(grid.result$S.pure2)] = 
  round(1.1*exp(0.29*grid.result$S.pure2[is.na(grid.result$S.pure.correct2) & !is.na(grid.result$S.pure2)]),2)
grid.result$S.near.correct2[is.na(grid.result$S.near.correct2) & !is.na(grid.result$S.near2)] = 
  round(1.25*exp(0.29*grid.result$S.near2[is.na(grid.result$S.near.correct2) & !is.na(grid.result$S.near2)]),2)
grid.result$S.pure.near.correct2[is.na(grid.result$S.pure.near.correct2) & !is.na(grid.result$S.pure.near2)] = 
  round(2*exp(0.29*grid.result$S.pure.near2[is.na(grid.result$S.pure.near.correct2) & !is.na(grid.result$S.pure.near2)]),2)
grid.result$S.ocas.correct2[is.na(grid.result$S.ocas.correct2) & !is.na(grid.result$S.ocas2)] = 
  round(1.5*exp(0.29*grid.result$S.ocas2[is.na(grid.result$S.ocas.correct2) & !is.na(grid.result$S.ocas2)]),2)
dim(grid.result)

# only TRUE taxonomy
grid.result2$S.total.correct[is.na(grid.result2$S.total.correct) & !is.na(grid.result2$S.total)] = 
  round(2.5*exp(0.29*grid.result2$S.total[is.na(grid.result2$S.total.correct) & !is.na(grid.result2$S.total)]),2)
#nls(S.pure.correct ~ a*exp(b*S.pure), start=list(a=2,b=0.3),data = subset(grid.result2,grid.result2$N.pure<15))
grid.result2$S.pure.correct[is.na(grid.result2$S.pure.correct) & !is.na(grid.result2$S.pure)] = 
  round(1.1*exp(0.29*grid.result2$S.pure[is.na(grid.result2$S.pure.correct) & !is.na(grid.result2$S.pure)]),2)
#nls(S.near.correct ~ a*exp(b*S.near), start=list(a=1,b=0.5),data = subset(grid.result2,grid.result2$N.near<15))
grid.result2$S.near.correct[is.na(grid.result2$S.near.correct) & !is.na(grid.result2$S.near)] = 
  round(1.25*exp(0.29*grid.result2$S.near[is.na(grid.result2$S.near.correct) & !is.na(grid.result2$S.near)]),2)
#nls(S.pure.near.correct ~ a*exp(b*S.pure.near), start=list(a=1,b=0.5),data = subset(grid.result2,grid.result2$N.pure.near<15))
grid.result2$S.pure.near.correct[is.na(grid.result2$S.pure.near.correct) & !is.na(grid.result2$S.pure.near)] = 
  round(2*exp(0.29*grid.result2$S.pure.near[is.na(grid.result2$S.pure.near.correct) & !is.na(grid.result2$S.pure.near)]),2)
#nls(S.ocas.correct ~ a*exp(b*S.ocas), start=list(a=1,b=0.5),data = subset(grid.result2,grid.result2$N.ocas<15))
grid.result2$S.ocas.correct[is.na(grid.result2$S.ocas.correct) & !is.na(grid.result2$S.ocas)] = 
  round(1.5*exp(0.29*grid.result2$S.ocas[is.na(grid.result2$S.ocas.correct) & !is.na(grid.result2$S.ocas)]),2)
grid.result2$S.pure.correct2[is.na(grid.result2$S.pure.correct2) & !is.na(grid.result2$S.pure2)] = 
  round(1.1*exp(0.29*grid.result2$S.pure2[is.na(grid.result2$S.pure.correct2) & !is.na(grid.result2$S.pure2)]),2)
grid.result2$S.near.correct2[is.na(grid.result2$S.near.correct2) & !is.na(grid.result2$S.near2)] = 
  round(1.25*exp(0.29*grid.result2$S.near2[is.na(grid.result2$S.near.correct2) & !is.na(grid.result2$S.near2)]),2)
grid.result2$S.pure.near.correct2[is.na(grid.result2$S.pure.near.correct2) & !is.na(grid.result2$S.pure.near2)] = 
  round(2*exp(0.29*grid.result2$S.pure.near2[is.na(grid.result2$S.pure.near.correct2) & !is.na(grid.result2$S.pure.near2)]),2)
grid.result2$S.ocas.correct2[is.na(grid.result2$S.ocas.correct2) & !is.na(grid.result2$S.ocas2)] = 
  round(1.5*exp(0.29*grid.result2$S.ocas2[is.na(grid.result2$S.ocas.correct2) & !is.na(grid.result2$S.ocas2)]),2)

#Shannon diversity (q=1)
### USE 2 OR 2.5? INSPECT THE RESULTING: 2 looks ok... 
grid.result$H.total.correct[is.na(grid.result$H.total.correct)] =  2*grid.result$S.total[is.na(grid.result$H.total.correct)]
grid.result$H.pure.correct[is.na(grid.result$H.pure.correct)] =  2*grid.result$S.pure[is.na(grid.result$H.pure.correct)]
grid.result$H.near.correct[is.na(grid.result$H.near.correct)] =  2*grid.result$S.near[is.na(grid.result$H.near.correct)]
grid.result$H.pure.near.correct[is.na(grid.result$H.pure.near.correct)] =  2*grid.result$S.pure.near[is.na(grid.result$H.pure.near.correct)]
grid.result$H.ocas.correct[is.na(grid.result$H.ocas.correct)] =  2*grid.result$S.ocas[is.na(grid.result$H.ocas.correct)]
grid.result$H.pure.correct2[is.na(grid.result$H.pure.correct2)] =  2*grid.result$S.pure2[is.na(grid.result$H.pure.correct2)]
grid.result$H.near.correct2[is.na(grid.result$H.near.correct2)] =  2*grid.result$S.near2[is.na(grid.result$H.near.correct2)]
grid.result$H.pure.near.correct2[is.na(grid.result$H.pure.near.correct2)] =  2*grid.result$S.pure.near2[is.na(grid.result$H.pure.near.correct2)]
grid.result$H.ocas.correct2[is.na(grid.result$H.ocas.correct2)] =  2*grid.result$S.ocas2[is.na(grid.result$H.ocas.correct2)]

grid.result2$H.total.correct[is.na(grid.result2$H.total.correct)] =  2*grid.result2$S.total[is.na(grid.result2$H.total.correct)]
grid.result2$H.pure.correct[is.na(grid.result2$H.pure.correct)] =  2*grid.result2$S.pure[is.na(grid.result2$H.pure.correct)]
grid.result2$H.near.correct[is.na(grid.result2$H.near.correct)] =  2*grid.result2$S.near[is.na(grid.result2$H.near.correct)]
grid.result2$H.pure.near.correct[is.na(grid.result2$H.pure.near.correct)] =  2*grid.result2$S.pure.near[is.na(grid.result2$H.pure.near.correct)]
grid.result2$H.ocas.correct[is.na(grid.result2$H.ocas.correct)] =  2*grid.result2$S.ocas[is.na(grid.result2$H.ocas.correct)]
grid.result2$H.pure.correct2[is.na(grid.result2$H.pure.correct2)] =  2*grid.result2$S.pure2[is.na(grid.result2$H.pure.correct2)]
grid.result2$H.near.correct2[is.na(grid.result2$H.near.correct2)] =  2*grid.result2$S.near2[is.na(grid.result2$H.near.correct2)]
grid.result2$H.pure.near.correct2[is.na(grid.result2$H.pure.near.correct2)] =  2*grid.result2$S.pure.near2[is.na(grid.result2$H.pure.near.correct2)]
grid.result2$H.ocas.correct2[is.na(grid.result2$H.ocas.correct2)] =  2*grid.result2$S.ocas2[is.na(grid.result2$H.ocas.correct2)]

#Simpson diversity (q=2)
### USE 2 OR 2.5? INSPECT THE RESULTING 
grid.result$D.total.correct[is.na(grid.result$D.total.correct)] =  2*grid.result$S.total[is.na(grid.result$D.total.correct)]
grid.result$D.pure.correct[is.na(grid.result$D.pure.correct)] =  2*grid.result$S.pure[is.na(grid.result$D.pure.correct)]
grid.result$D.near.correct[is.na(grid.result$D.near.correct)] =  2*grid.result$S.near[is.na(grid.result$D.near.correct)]
grid.result$D.pure.near.correct[is.na(grid.result$D.pure.near.correct)] =  2*grid.result$S.pure.near[is.na(grid.result$D.pure.near.correct)]
grid.result$D.ocas.correct[is.na(grid.result$D.ocas.correct)] =  2*grid.result$S.ocas[is.na(grid.result$D.ocas.correct)]
grid.result$D.pure.correct2[is.na(grid.result$D.pure.correct2)] =  2*grid.result$S.pure2[is.na(grid.result$D.pure.correct2)]
grid.result$D.near.correct2[is.na(grid.result$D.near.correct2)] =  2*grid.result$S.near2[is.na(grid.result$D.near.correct2)]
grid.result$D.pure.near.correct2[is.na(grid.result$D.pure.near.correct2)] =  2*grid.result$S.pure.near2[is.na(grid.result$D.pure.near.correct2)]
grid.result$D.ocas.correct2[is.na(grid.result$D.ocas.correct2)] =  2*grid.result$S.ocas2[is.na(grid.result$D.ocas.correct2)]

grid.result2$D.total.correct[is.na(grid.result2$D.total.correct)] =  2*grid.result2$S.total[is.na(grid.result2$D.total.correct)]
grid.result2$D.pure.correct[is.na(grid.result2$D.pure.correct)] =  2*grid.result2$S.pure[is.na(grid.result2$D.pure.correct)]
grid.result2$D.near.correct[is.na(grid.result2$D.near.correct)] =  2*grid.result2$S.near[is.na(grid.result2$D.near.correct)]
grid.result2$D.pure.near.correct[is.na(grid.result2$D.pure.near.correct)] =  2*grid.result2$S.pure.near[is.na(grid.result2$D.pure.near.correct)]
grid.result2$D.ocas.correct[is.na(grid.result2$D.ocas.correct)] =  2*grid.result2$S.ocas[is.na(grid.result2$D.ocas.correct)]
grid.result2$D.pure.correct2[is.na(grid.result2$D.pure.correct2)] =  2*grid.result2$S.pure2[is.na(grid.result2$D.pure.correct2)]
grid.result2$D.near.correct2[is.na(grid.result2$D.near.correct2)] =  2*grid.result2$S.near2[is.na(grid.result2$D.near.correct2)]
grid.result2$D.pure.near.correct2[is.na(grid.result2$D.pure.near.correct2)] =  2*grid.result2$S.pure.near2[is.na(grid.result2$D.pure.near.correct2)]
grid.result2$D.ocas.correct2[is.na(grid.result2$D.ocas.correct2)] =  2*grid.result2$S.ocas2[is.na(grid.result2$D.ocas.correct2)]

## NEED TO CORRECT THE ESTIMATES OF WE OR CWE?

# TRUE AND PROBABLE taxonomy
#Rarefaction (SR, q = 0)
dados = grid.result[!is.na(grid.result$N.total)&grid.result$N.total<=15,]
tmp = as.double(coef(lm(qS.50.total.correct ~ S.total - 1, data = dados)))
grid.result$qS.50.total.correct[is.na(grid.result$qS.50.total.correct)] =  tmp[1]*grid.result$S.total[is.na(grid.result$qS.50.total.correct)]
tmp = as.double(coef(lm(qS.100.total.correct ~ S.total - 1, data = dados)))
grid.result$qS.100.total.correct[is.na(grid.result$qS.100.total.correct)] =  tmp[1]*grid.result$S.total[is.na(grid.result$qS.100.total.correct)]
dados = grid.result[!is.na(grid.result$N.pure)&grid.result$N.pure<=15,]
tmp = as.double(coef(lm(qS.50.pure.correct ~ S.pure - 1, data = dados)))
grid.result$qS.50.pure.correct[is.na(grid.result$qS.50.pure.correct)] =  tmp[1]*grid.result$S.pure[is.na(grid.result$qS.50.pure.correct)]
tmp = as.double(coef(lm(qS.100.pure.correct ~ S.pure - 1, data = dados)))
grid.result$qS.100.pure.correct[is.na(grid.result$qS.100.pure.correct)] =  tmp[1]*grid.result$S.pure[is.na(grid.result$qS.100.pure.correct)]
dados = grid.result[!is.na(grid.result$N.near)&grid.result$N.near<=15,]
tmp = as.double(coef(lm(qS.50.near.correct ~ S.near - 1, data = dados)))
grid.result$qS.50.near.correct[is.na(grid.result$qS.50.near.correct)] =  tmp[1]*grid.result$S.near[is.na(grid.result$qS.50.near.correct)]
tmp = as.double(coef(lm(qS.100.near.correct ~ S.near - 1, data = dados)))
grid.result$qS.100.near.correct[is.na(grid.result$qS.100.near.correct)] =  tmp[1]*grid.result$S.near[is.na(grid.result$qS.100.near.correct)]
dados = grid.result[!is.na(grid.result$N.pure.near)&grid.result$N.pure.near<=15,]
tmp = as.double(coef(lm(qS.50.pure.near.correct ~ S.pure.near - 1, data = dados)))
grid.result$qS.50.pure.near.correct[is.na(grid.result$qS.50.pure.near.correct)] =  tmp[1]*grid.result$S.pure.near[is.na(grid.result$qS.50.pure.near.correct)]
tmp = as.double(coef(lm(qS.100.pure.near.correct ~ S.pure.near - 1, data = dados)))
grid.result$qS.100.pure.near.correct[is.na(grid.result$qS.100.pure.near.correct)] =  tmp[1]*grid.result$S.pure.near[is.na(grid.result$qS.100.pure.near.correct)]
dados = grid.result[!is.na(grid.result$N.ocas)&grid.result$N.ocas<=15,]
tmp = as.double(coef(lm(qS.50.ocas.correct ~ S.ocas - 1, data = dados)))
grid.result$qS.50.ocas.correct[is.na(grid.result$qS.50.ocas.correct)] =  tmp[1]*grid.result$S.ocas[is.na(grid.result$qS.50.ocas.correct)]
tmp = as.double(coef(lm(qS.100.ocas.correct ~ S.ocas - 1, data = dados)))
grid.result$qS.100.ocas.correct[is.na(grid.result$qS.100.ocas.correct)] =  tmp[1]*grid.result$S.ocas[is.na(grid.result$qS.100.ocas.correct)]

dados = grid.result[!is.na(grid.result$N.pure2)&grid.result$N.pure2<=15,]
tmp = as.double(coef(lm(qS.50.pure.correct2 ~ S.pure2 - 1, data = dados)))
grid.result$qS.50.pure.correct2[is.na(grid.result$qS.50.pure.correct2)] =  tmp[1]*grid.result$S.pure2[is.na(grid.result$qS.50.pure.correct2)]
tmp = as.double(coef(lm(qS.100.pure.correct2 ~ S.pure2 - 1, data = dados)))
grid.result$qS.100.pure.correct2[is.na(grid.result$qS.100.pure.correct2)] =  tmp[1]*grid.result$S.pure2[is.na(grid.result$qS.100.pure.correct2)]
dados = grid.result[!is.na(grid.result$N.near2)&grid.result$N.near2<=15,]
tmp = as.double(coef(lm(qS.50.near.correct2 ~ S.near2 - 1, data = dados)))
grid.result$qS.50.near.correct2[is.na(grid.result$qS.50.near.correct2)] =  tmp[1]*grid.result$S.near2[is.na(grid.result$qS.50.near.correct2)]
tmp = as.double(coef(lm(qS.100.near.correct2 ~ S.near2 - 1, data = dados)))
grid.result$qS.100.near.correct2[is.na(grid.result$qS.100.near.correct2)] =  tmp[1]*grid.result$S.near2[is.na(grid.result$qS.100.near.correct2)]
dados = grid.result[!is.na(grid.result$N.pure.near2)&grid.result$N.pure.near2<=15,]
tmp = as.double(coef(lm(qS.50.pure.near.correct2 ~ S.pure.near2 - 1, data = dados)))
grid.result$qS.50.pure.near.correct2[is.na(grid.result$qS.50.pure.near.correct2)] =  tmp[1]*grid.result$S.pure.near2[is.na(grid.result$qS.50.pure.near.correct2)]
tmp = as.double(coef(lm(qS.100.pure.near.correct2 ~ S.pure.near2 - 1, data = dados)))
grid.result$qS.100.pure.near.correct2[is.na(grid.result$qS.100.pure.near.correct2)] =  tmp[1]*grid.result$S.pure.near2[is.na(grid.result$qS.100.pure.near.correct2)]
dados = grid.result[!is.na(grid.result$N.ocas2)&grid.result$N.ocas2<=15,]
tmp = as.double(coef(lm(qS.50.ocas.correct2 ~ S.ocas2 - 1, data = dados)))
grid.result$qS.50.ocas.correct2[is.na(grid.result$qS.50.ocas.correct2)] =  tmp[1]*grid.result$S.ocas2[is.na(grid.result$qS.50.ocas.correct2)]
tmp = as.double(coef(lm(qS.100.ocas.correct2 ~ S.ocas2 - 1, data = dados)))
grid.result$qS.100.ocas.correct2[is.na(grid.result$qS.100.ocas.correct2)] =  tmp[1]*grid.result$S.ocas2[is.na(grid.result$qS.100.ocas.correct2)]

#Rarefaction (Simpson, q = 2)
dados = grid.result[!is.na(grid.result$N.total)&grid.result$N.total<=15,]
tmp = as.double(coef(lm(qD.50.total.correct ~ S.total - 1, data = dados)))
grid.result$qD.50.total.correct[is.na(grid.result$qD.50.total.correct)] =  tmp[1]*grid.result$S.total[is.na(grid.result$qD.50.total.correct)]
tmp = as.double(coef(lm(qD.100.total.correct ~ S.total - 1, data = dados)))
grid.result$qD.100.total.correct[is.na(grid.result$qD.100.total.correct)] =  tmp[1]*grid.result$S.total[is.na(grid.result$qD.100.total.correct)]
dados = grid.result[!is.na(grid.result$N.pure)&grid.result$N.pure<=15,]
tmp = as.double(coef(lm(qD.50.pure.correct ~ S.pure - 1, data = dados)))
grid.result$qD.50.pure.correct[is.na(grid.result$qD.50.pure.correct)] =  tmp[1]*grid.result$S.pure[is.na(grid.result$qD.50.pure.correct)]
tmp = as.double(coef(lm(qD.100.pure.correct ~ S.pure - 1, data = dados)))
grid.result$qD.100.pure.correct[is.na(grid.result$qD.100.pure.correct)] =  tmp[1]*grid.result$S.pure[is.na(grid.result$qD.100.pure.correct)]
dados = grid.result[!is.na(grid.result$N.near)&grid.result$N.near<=15,]
tmp = as.double(coef(lm(qD.50.near.correct ~ S.near - 1, data = dados)))
grid.result$qD.50.near.correct[is.na(grid.result$qD.50.near.correct)] =  tmp[1]*grid.result$S.near[is.na(grid.result$qD.50.near.correct)]
tmp = as.double(coef(lm(qD.100.near.correct ~ S.near - 1, data = dados)))
grid.result$qD.100.near.correct[is.na(grid.result$qD.100.near.correct)] =  tmp[1]*grid.result$S.near[is.na(grid.result$qD.100.near.correct)]
dados = grid.result[!is.na(grid.result$N.pure.near)&grid.result$N.pure.near<=15,]
tmp = as.double(coef(lm(qD.50.pure.near.correct ~ S.pure.near - 1, data = dados)))
grid.result$qD.50.pure.near.correct[is.na(grid.result$qD.50.pure.near.correct)] =  tmp[1]*grid.result$S.pure.near[is.na(grid.result$qD.50.pure.near.correct)]
tmp = as.double(coef(lm(qD.100.pure.near.correct ~ S.pure.near - 1, data = dados)))
grid.result$qD.100.pure.near.correct[is.na(grid.result$qD.100.pure.near.correct)] =  tmp[1]*grid.result$S.pure.near[is.na(grid.result$qD.100.pure.near.correct)]
dados = grid.result[!is.na(grid.result$N.ocas)&grid.result$N.ocas<=15,]
tmp = as.double(coef(lm(qD.50.ocas.correct ~ S.ocas - 1, data = dados)))
grid.result$qD.50.ocas.correct[is.na(grid.result$qD.50.ocas.correct)] =  tmp[1]*grid.result$S.ocas[is.na(grid.result$qD.50.ocas.correct)]
tmp = as.double(coef(lm(qD.100.ocas.correct ~ S.ocas - 1, data = dados)))
grid.result$qD.100.ocas.correct[is.na(grid.result$qD.100.ocas.correct)] =  tmp[1]*grid.result$S.ocas[is.na(grid.result$qD.100.ocas.correct)]

dados = grid.result[!is.na(grid.result$N.pure2)&grid.result$N.pure2<=15,]
tmp = as.double(coef(lm(qS.50.pure.correct2 ~ S.pure2 - 1, data = dados)))
grid.result$qS.50.pure.correct2[is.na(grid.result$qS.50.pure.correct2)] =  tmp[1]*grid.result$S.pure2[is.na(grid.result$qS.50.pure.correct2)]
tmp = as.double(coef(lm(qS.100.pure.correct2 ~ S.pure2 - 1, data = dados)))
grid.result$qS.100.pure.correct2[is.na(grid.result$qS.100.pure.correct2)] =  tmp[1]*grid.result$S.pure2[is.na(grid.result$qS.100.pure.correct2)]
dados = grid.result[!is.na(grid.result$N.near2)&grid.result$N.near2<=15,]
tmp = as.double(coef(lm(qS.50.near.correct2 ~ S.near2 - 1, data = dados)))
grid.result$qS.50.near.correct2[is.na(grid.result$qS.50.near.correct2)] =  tmp[1]*grid.result$S.near2[is.na(grid.result$qS.50.near.correct2)]
tmp = as.double(coef(lm(qS.100.near.correct2 ~ S.near2 - 1, data = dados)))
grid.result$qS.100.near.correct2[is.na(grid.result$qS.100.near.correct2)] =  tmp[1]*grid.result$S.near2[is.na(grid.result$qS.100.near.correct2)]
dados = grid.result[!is.na(grid.result$N.pure.near2)&grid.result$N.pure.near2<=15,]
tmp = as.double(coef(lm(qS.50.pure.near.correct2 ~ S.pure.near2 - 1, data = dados)))
grid.result$qS.50.pure.near.correct2[is.na(grid.result$qS.50.pure.near.correct2)] =  tmp[1]*grid.result$S.pure.near2[is.na(grid.result$qS.50.pure.near.correct2)]
tmp = as.double(coef(lm(qS.100.pure.near.correct2 ~ S.pure.near2 - 1, data = dados)))
grid.result$qS.100.pure.near.correct2[is.na(grid.result$qS.100.pure.near.correct2)] =  tmp[1]*grid.result$S.pure.near2[is.na(grid.result$qS.100.pure.near.correct2)]
dados = grid.result[!is.na(grid.result$N.ocas2)&grid.result$N.ocas2<=15,]
tmp = as.double(coef(lm(qS.50.ocas.correct2 ~ S.ocas2 - 1, data = dados)))
grid.result$qS.50.ocas.correct2[is.na(grid.result$qS.50.ocas.correct2)] =  tmp[1]*grid.result$S.ocas2[is.na(grid.result$qS.50.ocas.correct2)]
tmp = as.double(coef(lm(qS.100.ocas.correct2 ~ S.ocas2 - 1, data = dados)))
grid.result$qS.100.ocas.correct2[is.na(grid.result$qS.100.ocas.correct2)] =  tmp[1]*grid.result$S.ocas2[is.na(grid.result$qS.100.ocas.correct2)]

# only TRUE taxonomy
#Rarefaction (SR, q = 0)
dados = grid.result2[!is.na(grid.result2$N.total)&grid.result2$N.total<=15,]
tmp = as.double(coef(lm(qS.50.total.correct ~ S.total - 1, data = dados)))
grid.result2$qS.50.total.correct[is.na(grid.result2$qS.50.total.correct)] =  tmp[1]*grid.result2$S.total[is.na(grid.result2$qS.50.total.correct)]
tmp = as.double(coef(lm(qS.100.total.correct ~ S.total - 1, data = dados)))
grid.result2$qS.100.total.correct[is.na(grid.result2$qS.100.total.correct)] =  tmp[1]*grid.result2$S.total[is.na(grid.result2$qS.100.total.correct)]
dados = grid.result2[!is.na(grid.result2$N.pure)&grid.result2$N.pure<=15,]
tmp = as.double(coef(lm(qS.50.pure.correct ~ S.pure - 1, data = dados)))
grid.result2$qS.50.pure.correct[is.na(grid.result2$qS.50.pure.correct)] =  tmp[1]*grid.result2$S.pure[is.na(grid.result2$qS.50.pure.correct)]
tmp = as.double(coef(lm(qS.100.pure.correct ~ S.pure - 1, data = dados)))
grid.result2$qS.100.pure.correct[is.na(grid.result2$qS.100.pure.correct)] =  tmp[1]*grid.result2$S.pure[is.na(grid.result2$qS.100.pure.correct)]
dados = grid.result2[!is.na(grid.result2$N.near)&grid.result2$N.near<=15,]
tmp = as.double(coef(lm(qS.50.near.correct ~ S.near - 1, data = dados)))
grid.result2$qS.50.near.correct[is.na(grid.result2$qS.50.near.correct)] =  tmp[1]*grid.result2$S.near[is.na(grid.result2$qS.50.near.correct)]
tmp = as.double(coef(lm(qS.100.near.correct ~ S.near - 1, data = dados)))
grid.result2$qS.100.near.correct[is.na(grid.result2$qS.100.near.correct)] =  tmp[1]*grid.result2$S.near[is.na(grid.result2$qS.100.near.correct)]
dados = grid.result2[!is.na(grid.result2$N.pure.near)&grid.result2$N.pure.near<=15,]
tmp = as.double(coef(lm(qS.50.pure.near.correct ~ S.pure.near - 1, data = dados)))
grid.result2$qS.50.pure.near.correct[is.na(grid.result2$qS.50.pure.near.correct)] =  tmp[1]*grid.result2$S.pure.near[is.na(grid.result2$qS.50.pure.near.correct)]
tmp = as.double(coef(lm(qS.100.pure.near.correct ~ S.pure.near - 1, data = dados)))
grid.result2$qS.100.pure.near.correct[is.na(grid.result2$qS.100.pure.near.correct)] =  tmp[1]*grid.result2$S.pure.near[is.na(grid.result2$qS.100.pure.near.correct)]
dados = grid.result2[!is.na(grid.result2$N.ocas)&grid.result2$N.ocas<=15,]
tmp = as.double(coef(lm(qS.50.ocas.correct ~ S.ocas - 1, data = dados)))
grid.result2$qS.50.ocas.correct[is.na(grid.result2$qS.50.ocas.correct)] =  tmp[1]*grid.result2$S.ocas[is.na(grid.result2$qS.50.ocas.correct)]
tmp = as.double(coef(lm(qS.100.ocas.correct ~ S.ocas - 1, data = dados)))
grid.result2$qS.100.ocas.correct[is.na(grid.result2$qS.100.ocas.correct)] =  tmp[1]*grid.result2$S.ocas[is.na(grid.result2$qS.100.ocas.correct)]

dados = grid.result2[!is.na(grid.result2$N.pure2)&grid.result2$N.pure2<=15,]
tmp = as.double(coef(lm(qS.50.pure.correct2 ~ S.pure2 - 1, data = dados)))
grid.result2$qS.50.pure.correct2[is.na(grid.result2$qS.50.pure.correct2)] =  tmp[1]*grid.result2$S.pure2[is.na(grid.result2$qS.50.pure.correct2)]
tmp = as.double(coef(lm(qS.100.pure.correct2 ~ S.pure2 - 1, data = dados)))
grid.result2$qS.100.pure.correct2[is.na(grid.result2$qS.100.pure.correct2)] =  tmp[1]*grid.result2$S.pure2[is.na(grid.result2$qS.100.pure.correct2)]
dados = grid.result2[!is.na(grid.result2$N.near2)&grid.result2$N.near2<=15,]
tmp = as.double(coef(lm(qS.50.near.correct2 ~ S.near2 - 1, data = dados)))
grid.result2$qS.50.near.correct2[is.na(grid.result2$qS.50.near.correct2)] =  tmp[1]*grid.result2$S.near2[is.na(grid.result2$qS.50.near.correct2)]
tmp = as.double(coef(lm(qS.100.near.correct2 ~ S.near2 - 1, data = dados)))
grid.result2$qS.100.near.correct2[is.na(grid.result2$qS.100.near.correct2)] =  tmp[1]*grid.result2$S.near2[is.na(grid.result2$qS.100.near.correct2)]
dados = grid.result2[!is.na(grid.result2$N.pure.near2)&grid.result2$N.pure.near2<=15,]
tmp = as.double(coef(lm(qS.50.pure.near.correct2 ~ S.pure.near2 - 1, data = dados)))
grid.result2$qS.50.pure.near.correct2[is.na(grid.result2$qS.50.pure.near.correct2)] =  tmp[1]*grid.result2$S.pure.near2[is.na(grid.result2$qS.50.pure.near.correct2)]
tmp = as.double(coef(lm(qS.100.pure.near.correct2 ~ S.pure.near2 - 1, data = dados)))
grid.result2$qS.100.pure.near.correct2[is.na(grid.result2$qS.100.pure.near.correct2)] =  tmp[1]*grid.result2$S.pure.near2[is.na(grid.result2$qS.100.pure.near.correct2)]
dados = grid.result2[!is.na(grid.result2$N.ocas2)&grid.result2$N.ocas2<=15,]
tmp = as.double(coef(lm(qS.50.ocas.correct2 ~ S.ocas2 - 1, data = dados)))
grid.result2$qS.50.ocas.correct2[is.na(grid.result2$qS.50.ocas.correct2)] =  tmp[1]*grid.result2$S.ocas2[is.na(grid.result2$qS.50.ocas.correct2)]
tmp = as.double(coef(lm(qS.100.ocas.correct2 ~ S.ocas2 - 1, data = dados)))
grid.result2$qS.100.ocas.correct2[is.na(grid.result2$qS.100.ocas.correct2)] =  tmp[1]*grid.result2$S.ocas2[is.na(grid.result2$qS.100.ocas.correct2)]

#Rarefaction (Simpson, q = 2)
dados = grid.result2[!is.na(grid.result2$N.total)&grid.result2$N.total<=15,]
tmp = as.double(coef(lm(qD.50.total.correct ~ S.total - 1, data = dados)))
grid.result2$qD.50.total.correct[is.na(grid.result2$qD.50.total.correct)] =  tmp[1]*grid.result2$S.total[is.na(grid.result2$qD.50.total.correct)]
tmp = as.double(coef(lm(qD.100.total.correct ~ S.total - 1, data = dados)))
grid.result2$qD.100.total.correct[is.na(grid.result2$qD.100.total.correct)] =  tmp[1]*grid.result2$S.total[is.na(grid.result2$qD.100.total.correct)]
dados = grid.result2[!is.na(grid.result2$N.pure)&grid.result2$N.pure<=15,]
tmp = as.double(coef(lm(qD.50.pure.correct ~ S.pure - 1, data = dados)))
grid.result2$qD.50.pure.correct[is.na(grid.result2$qD.50.pure.correct)] =  tmp[1]*grid.result2$S.pure[is.na(grid.result2$qD.50.pure.correct)]
tmp = as.double(coef(lm(qD.100.pure.correct ~ S.pure - 1, data = dados)))
grid.result2$qD.100.pure.correct[is.na(grid.result2$qD.100.pure.correct)] =  tmp[1]*grid.result2$S.pure[is.na(grid.result2$qD.100.pure.correct)]
dados = grid.result2[!is.na(grid.result2$N.near)&grid.result2$N.near<=15,]
tmp = as.double(coef(lm(qD.50.near.correct ~ S.near - 1, data = dados)))
grid.result2$qD.50.near.correct[is.na(grid.result2$qD.50.near.correct)] =  tmp[1]*grid.result2$S.near[is.na(grid.result2$qD.50.near.correct)]
tmp = as.double(coef(lm(qD.100.near.correct ~ S.near - 1, data = dados)))
grid.result2$qD.100.near.correct[is.na(grid.result2$qD.100.near.correct)] =  tmp[1]*grid.result2$S.near[is.na(grid.result2$qD.100.near.correct)]
dados = grid.result2[!is.na(grid.result2$N.pure.near)&grid.result2$N.pure.near<=15,]
tmp = as.double(coef(lm(qD.50.pure.near.correct ~ S.pure.near - 1, data = dados)))
grid.result2$qD.50.pure.near.correct[is.na(grid.result2$qD.50.pure.near.correct)] =  tmp[1]*grid.result2$S.pure.near[is.na(grid.result2$qD.50.pure.near.correct)]
tmp = as.double(coef(lm(qD.100.pure.near.correct ~ S.pure.near - 1, data = dados)))
grid.result2$qD.100.pure.near.correct[is.na(grid.result2$qD.100.pure.near.correct)] =  tmp[1]*grid.result2$S.pure.near[is.na(grid.result2$qD.100.pure.near.correct)]
dados = grid.result2[!is.na(grid.result2$N.ocas)&grid.result2$N.ocas<=15,]
tmp = as.double(coef(lm(qD.50.ocas.correct ~ S.ocas - 1, data = dados)))
grid.result2$qD.50.ocas.correct[is.na(grid.result2$qD.50.ocas.correct)] =  tmp[1]*grid.result2$S.ocas[is.na(grid.result2$qD.50.ocas.correct)]
tmp = as.double(coef(lm(qD.100.ocas.correct ~ S.ocas - 1, data = dados)))
grid.result2$qD.100.ocas.correct[is.na(grid.result2$qD.100.ocas.correct)] =  tmp[1]*grid.result2$S.ocas[is.na(grid.result2$qD.100.ocas.correct)]

dados = grid.result2[!is.na(grid.result2$N.pure2)&grid.result2$N.pure2<=15,]
tmp = as.double(coef(lm(qS.50.pure.correct2 ~ S.pure2 - 1, data = dados)))
grid.result2$qS.50.pure.correct2[is.na(grid.result2$qS.50.pure.correct2)] =  tmp[1]*grid.result2$S.pure2[is.na(grid.result2$qS.50.pure.correct2)]
tmp = as.double(coef(lm(qS.100.pure.correct2 ~ S.pure2 - 1, data = dados)))
grid.result2$qS.100.pure.correct2[is.na(grid.result2$qS.100.pure.correct2)] =  tmp[1]*grid.result2$S.pure2[is.na(grid.result2$qS.100.pure.correct2)]
dados = grid.result2[!is.na(grid.result2$N.near2)&grid.result2$N.near2<=15,]
tmp = as.double(coef(lm(qS.50.near.correct2 ~ S.near2 - 1, data = dados)))
grid.result2$qS.50.near.correct2[is.na(grid.result2$qS.50.near.correct2)] =  tmp[1]*grid.result2$S.near2[is.na(grid.result2$qS.50.near.correct2)]
tmp = as.double(coef(lm(qS.100.near.correct2 ~ S.near2 - 1, data = dados)))
grid.result2$qS.100.near.correct2[is.na(grid.result2$qS.100.near.correct2)] =  tmp[1]*grid.result2$S.near2[is.na(grid.result2$qS.100.near.correct2)]
dados = grid.result2[!is.na(grid.result2$N.pure.near2)&grid.result2$N.pure.near2<=15,]
tmp = as.double(coef(lm(qS.50.pure.near.correct2 ~ S.pure.near2 - 1, data = dados)))
grid.result2$qS.50.pure.near.correct2[is.na(grid.result2$qS.50.pure.near.correct2)] =  tmp[1]*grid.result2$S.pure.near2[is.na(grid.result2$qS.50.pure.near.correct2)]
tmp = as.double(coef(lm(qS.100.pure.near.correct2 ~ S.pure.near2 - 1, data = dados)))
grid.result2$qS.100.pure.near.correct2[is.na(grid.result2$qS.100.pure.near.correct2)] =  tmp[1]*grid.result2$S.pure.near2[is.na(grid.result2$qS.100.pure.near.correct2)]
dados = grid.result2[!is.na(grid.result2$N.ocas2)&grid.result2$N.ocas2<=15,]
tmp = as.double(coef(lm(qS.50.ocas.correct2 ~ S.ocas2 - 1, data = dados)))
grid.result2$qS.50.ocas.correct2[is.na(grid.result2$qS.50.ocas.correct2)] =  tmp[1]*grid.result2$S.ocas2[is.na(grid.result2$qS.50.ocas.correct2)]
tmp = as.double(coef(lm(qS.100.ocas.correct2 ~ S.ocas2 - 1, data = dados)))
grid.result2$qS.100.ocas.correct2[is.na(grid.result2$qS.100.ocas.correct2)] =  tmp[1]*grid.result2$S.ocas2[is.na(grid.result2$qS.100.ocas.correct2)]

### Replacing estimates that are true NAs (N.total>0) by 0
cols = c("S.pure","S.near","S.pure.near","S.ocas","S.pure2","S.near2","S.pure.near2","S.ocas2",
         "WE.pure","WE.near","WE.pure.near","WE.ocas","WE.pure2","WE.near2","WE.pure.near2","WE.ocas2",
         "CWE.pure","CWE.near","CWE.pure.near","CWE.ocas","CWE.pure2","CWE.near2","CWE.pure.near2","CWE.ocas2")
for (i in 1:length(cols)) grid.result[,cols[i]][!is.na(grid.result$N.total) & is.na(grid.result[,cols[i]])] = 0
for (i in 1:length(cols)) grid.result2[,cols[i]][!is.na(grid.result2$N.total) & is.na(grid.result2[,cols[i]])] = 0

cols = c("S.est.pure","S.est.near","S.est.pure.near","S.est.ocas", "S.est.pure2","S.est.near2","S.est.pure.near2","S.est.ocas2",
         "S.pure.correct","S.near.correct","S.pure.near.correct", "S.ocas.correct2","S.pure.correct2","S.near.correct2","S.pure.near.correct2","S.ocas.correct2",
         "D.est.pure","D.est.near","D.est.pure.near","D.est.ocas", "D.est.pure2","D.est.near2","D.est.pure.near2","D.est.ocas2",
         "H.est.pure","H.est.near","H.est.pure.near","H.est.ocas", "H.est.pure2","H.est.near2","H.est.pure.near2","H.est.ocas2",
         "qS.50.pure.correct","qS.50.near.correct","qS.50.pure.near.correct","qS.50.ocas.correct", "qS.50.pure.correct2","qS.50.near.correct2","qS.50.pure.near.correct2","qS.50.ocas.correct2",
         "qS.100.pure.correct","qS.100.near.correct","qS.100.pure.near.correct","qS.100.ocas.correct", "qS.100.pure.correct2","qS.100.near.correct2","qS.100.pure.near.correct2","qS.100.ocas.correct2",
         "qD.50.pure.correct","qD.50.near.correct","qD.50.pure.near.correct","qD.50.ocas.correct", "qD.50.pure.correct2","qD.50.near.correct2","qD.50.pure.near.correct2","qD.50.ocas.correct2",
         "qD.100.pure.correct","qD.100.near.correct","qD.100.pure.near.correct","qD.100.ocas.correct", "qD.100.pure.correct2","qD.100.near.correct2","qD.100.pure.near.correct2","qD.100.ocas.correct2")
for (i in 1:length(cols)) grid.result[,cols[i]][!is.na(grid.result$S.est.total) & is.na(grid.result[,cols[i]])] = 0
for (i in 1:length(cols)) grid.result2[,cols[i]][!is.na(grid.result2$S.est.total) & is.na(grid.result2[,cols[i]])] = 0

## Saving the grid results
saveRDS(list(grid.result,samp.cov.cutoff),"grid.results_50km_true_prob_taxonomy.rds")
saveRDS(list(grid.result2,samp.cov.cutoff2),"grid.results_50km_true_taxonomy.rds")

#### END ####