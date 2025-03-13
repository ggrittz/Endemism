#######################################################
#### Detecting outliers in species occurrence data ####
#######################################################
## Following Liu et al 2018 paper
rm(list=ls())

#### LOADING PACKAGES  ####
require(data.table)
require(stringr)
require(mvoutlier)
require(rrcovHD)
#require(Rlof)
require(kernlab)
#require(randomForest)
#install.packages("IsolationForest")
#require(IsolationForest)
require(spatstat)
require(raster)
require(sp)


#### LOADING TREE OCCURRENCE DATA  ####
## Loading the paths to occurrence data 
paths = dir("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//occurrence_data",full.names=TRUE)
paths = paths[grepl('merged.csv',paths)]

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
rm(lista, tmp, paths, i)


#### FINDING OUTLIERS USING DIFFERENT METHODS ####
# Creating a vector with the names of specimens determined at species and infra-specific levels
oc.data[,species.correct2 := sapply(strsplit(species.correct1," "), function(x) paste(x[1],x[2],sep=" ")),]
# setting 'species.correct2' as key to the data.table (makes computations faster)
setkeyv(oc.data, "species.correct2")

#### 1 - EXTREME  OUTLIERS ####

## Calculating the Mahalanobis distance per species
#creating the function
mahalanobis.cla <- function(y) {
  dt = cbind.data.frame(longitude.work1 = as.double(y$longitude.work1), 
                        latitude.work1 = as.double(y$latitude.work1),
                        numTombo = as.character(y$numTombo),
                        dup.ID = as.character(y$dup.ID))
  dt$dup.ID = as.character(dt$dup.ID)
  dt$dup.ID[is.na(dt$dup.ID)] = as.character(dt$numTombo[is.na(dt$dup.ID)])
  #dt1 = dt[!duplicated(paste(dt$longitude.work1, dt$longitude.work1,sep="_")),]
  dt1 = dt[!duplicated(dt$dup.ID, incomparables = NA),]
  if (length(unique(dt1$longitude.work1))<5 & length(unique(dt1$latitude.work1))<5) {
    dt1$longitude.work1 = jitter(dt1$longitude.work1, factor = 0.0001)
    dt1$latitude.work1 = jitter(dt1$latitude.work1, factor = 0.0001)
  }
   if(dim(dt1)[1]<10) { 
    res = as.double(rep(NA_character_, dim(dt)[1]))
  } else {
    #res = sqrt(mahalanobis(dt, center = apply(dt, 2, mean), cov = cov(dt)))
    res0 = cbind.data.frame(dup.ID = dt1$dup.ID,  
                            res = sqrt(mahalanobis(dt1[,1:2], center = apply(dt1[,1:2], 2, mean), cov = cov(dt1[,1:2]))))
    #Putting data back in the original order
    res1 = merge(dt[,c("numTombo","dup.ID")], res0, by = "dup.ID", all.x = TRUE, sort=FALSE)
    res1 = res1[order(match(res1$numTombo, dt$numTombo)),]
    res  = as.double(res1$res)
  }
  return(res)
}
#running the function
oc.data[geo.check1 %like% "ok_" & !is.na(longitude.work1), maha.cla := mahalanobis.cla(.SD), by = .(species.correct2)]
#oc.data[,maha.cla := NULL,]

## Calculating the robust (Mahalanobis) distance per species
#creating the function
mahalanobis.rob <- function(y) {
  dt = cbind.data.frame(longitude.work1 = as.double(y$longitude.work1), 
                        latitude.work1 = as.double(y$latitude.work1),
                        numTombo = as.character(y$numTombo),
                        dup.ID = as.character(y$dup.ID))
  dt$dup.ID = as.character(dt$dup.ID)
  dt$dup.ID[is.na(dt$dup.ID)] = as.character(dt$numTombo[is.na(dt$dup.ID)])
  dt1 = dt[!duplicated(dt$dup.ID, incomparables = NA),]
  #dt1 = dt1[!is.na(dt1$longitude.work1),]
  if (length(unique(dt1$longitude.work1))<5 & length(unique(dt1$latitude.work1))<5) {
    dt1$longitude.work1 = jitter(dt1$longitude.work1, factor = 0.0001)
    dt1$latitude.work1 = jitter(dt1$latitude.work1, factor = 0.0001)
  }
  if(dim(dt1)[1]<10) { 
    res = as.double(rep(NA_character_, dim(dt)[1]))
  } else {
    rob <- try(covMcd(dt1[,1:2], alpha = 1/2), TRUE)
    if(class(rob)=="try-error") {
      dt1$longitude.work2 = jitter(dt1$longitude.work1, factor = 0.005)
      dt1$latitude.work2 = jitter(dt1$latitude.work1, factor = 0.005)
      rob <- covMcd(dt1[,c("longitude.work2","latitude.work2")], alpha = 1/2, tol=1e-20)
      res0 = cbind.data.frame(dup.ID = dt1$dup.ID,  
                              res = sqrt(mahalanobis(dt1[,c("longitude.work2","latitude.work2")], center = rob$center, cov = rob$cov, tol=1e-20)))
    } else {
      if(length(rob$singularity)>0) {
        dt1$longitude.work2 = jitter(dt1$longitude.work1, factor = 0.05)
        dt1$latitude.work2 = jitter(dt1$latitude.work1, factor = 0.05)
        rob <- covMcd(dt1[,c("longitude.work2","latitude.work2")], alpha = 1/2, tol=1e-20)
        res0 = cbind.data.frame(dup.ID = dt1$dup.ID,  
                                res = sqrt(mahalanobis(dt1[,c("longitude.work2","latitude.work2")], center = rob$center, cov = rob$cov, tol=1e-20)))
      } else {
      res0 = cbind.data.frame(dup.ID = dt1$dup.ID,  
                              res = sqrt(mahalanobis(dt1[,c("longitude.work1","latitude.work1")], center = rob$center, cov = rob$cov, tol=1e-20)))
     }
    }
    #Putting data back in the original order
    res1 = merge(dt[,c("numTombo","dup.ID")], res0, by = "dup.ID", all.x = TRUE, sort=FALSE)
    res1 = res1[order(match(res1$numTombo, dt$numTombo)),]
    res  = as.double(res1$res)
  }
  return(res)
}
#running the function
oc.data[geo.check1 %like% "ok_" & !is.na(longitude.work1), maha.rob := mahalanobis.rob(.SD), by = .(species.correct2)]
#oc.data[,maha.rob := NULL,]

## Finding the best cut-offs for each distance
#creating the function
mahalanobis.cut <- function(y) {
  dt = cbind.data.frame(longitude.work1 = as.double(y$longitude.work1), 
                        latitude.work1 = as.double(y$latitude.work1),
                        numTombo = as.character(y$numTombo),
                        dup.ID = as.character(y$dup.ID))
  dt$dup.ID = as.character(dt$dup.ID)
  dt$dup.ID[is.na(dt$dup.ID)] = as.character(dt$numTombo[is.na(dt$dup.ID)])
  dt1 = dt[!duplicated(dt$dup.ID, incomparables = NA),]
  dt1 = dt1[!is.na(dt1$longitude.work1),]
  dt1$longitude.work1 = jitter(dt1$longitude.work1, factor = 0.0005)
  dt1$latitude.work1 = jitter(dt1$latitude.work1, factor = 0.0005)
  if(dim(dt1)[1]<20) { 
    res = paste(dim(dt1)[1],paste(rep(NA_character_, 14),collapse="|",sep=""),collapse="|",sep="|")
  } else {
    rob <- try(covMcd(dt1[,1:2], alpha = 1/2), TRUE)
    if(class(rob)=="try-error") {
      res = paste(dim(dt1)[1],paste(rep(NA_character_, 14),collapse="|",sep=""),collapse="|",sep="|")
    } else {
      if(length(rob$singularity)>0) {
        res = paste(dim(dt1)[1],paste(rep(NA_character_, 14),collapse="|",sep=""),collapse="|",sep="|")
      } else {
        rob <- covMcd(dt1[,1:2], alpha = 1/2)
        distcla <- sqrt(mahalanobis(dt1[,1:2], center = apply(dt1[,1:2], 2, mean), cov = cov(dt1[,1:2])))
        distrob <- sqrt(mahalanobis(dt1[,1:2], center = rob$center, cov = rob$cov))
        xarw <- arw(dt1[,1:2], rob$center, rob$cov, alpha = 0.025)
        if (xarw$cn != Inf) {   
          alpha <- sqrt(c(xarw$cn, qchisq(c(0.75, 0.5, 0.25), ncol(dt1))))   
        } else {
          alpha <- sqrt(qchisq(c(0.975, 0.75, 0.5, 0.25), ncol(dt1)))
        }
        o <- (distrob > min(sqrt(xarw$cn), sqrt(qchisq(0.975, dim(dt1[,1:2])[2]))))
        qs <- quantile(distrob[o], prob=c(0.5,0.75,seq(0.9,1,0.01)))
        #res = c(n = dim(dt1)[1],qs)
        res = paste(dim(dt1)[1],round(alpha[1],3),paste(round(qs,3),collapse="|",sep=""),collapse="|",sep="|")
      }
    }
    #res = merge(dt[,c("numTombo","dup.ID")], res0, by = "dup.ID", all.x = TRUE, sort=FALSE)$res
  }
  return(res)
}
#running the function
tmp <- oc.data[geo.check1 %like% "ok_" & !is.na(longitude.work1), mahalanobis.cut(.SD), by = .(species.correct2)]
tmp1 = sapply(tmp$V1,strsplit,"\\|")  
tmp1 = do.call(rbind.data.frame,tmp1)
names(tmp1) = c("n","q50","q75","q90","q91","q92","q93","q94","q95","q96","q97","q98","q99","q100")
cut.rob = cbind.data.frame(species.correct2 = tmp$species.correct2, tmp1)
for (i in 2:dim(cut.rob)[2]) cut.rob[,i] = as.double(as.character(cut.rob[,i]))

#creating the function
mahalanobis.cut1 <- function(y) {
  dt = cbind.data.frame(longitude.work1 = as.double(y$longitude.work1), 
                        latitude.work1 = as.double(y$latitude.work1),
                        numTombo = as.character(y$numTombo),
                        dup.ID = as.character(y$dup.ID))
  dt$dup.ID = as.character(dt$dup.ID)
  dt$dup.ID[is.na(dt$dup.ID)] = as.character(dt$numTombo[is.na(dt$dup.ID)])
  dt1 = dt[!duplicated(dt$dup.ID, incomparables = NA),]
  dt1 = dt1[!is.na(dt1$longitude.work1),]
  dt1$longitude.work1 = jitter(dt1$longitude.work1, factor = 0.0005)
  dt1$latitude.work1 = jitter(dt1$latitude.work1, factor = 0.0005)
  if(dim(dt1)[1]<25) { 
    res = paste(dim(dt1)[1],paste(rep(NA_character_, 14),collapse="|",sep=""),collapse="|",sep="|")
  } else {
    rob <- try(covMcd(dt1[,1:2], alpha = 1/2), TRUE)
    if(class(rob)=="try-error") {
      res = paste(dim(dt1)[1],paste(rep(NA_character_, 14),collapse="|",sep=""),collapse="|",sep="|")
    } else {
      if(length(rob$singularity)>0) {
        res = paste(dim(dt1)[1],paste(rep(NA_character_, 14),collapse="|",sep=""),collapse="|",sep="|")
      } else {
        rob <- covMcd(dt1[,1:2], alpha = 1/2)
        distcla <- sqrt(mahalanobis(dt1[,1:2], center = apply(dt1[,1:2], 2, mean), cov = cov(dt1[,1:2])))
        distrob <- sqrt(mahalanobis(dt1[,1:2], center = rob$center, cov = rob$cov))
        xarw <- arw(dt1[,1:2], rob$center, rob$cov, alpha = 0.025)
        if (xarw$cn != Inf) {   
          alpha <- sqrt(c(xarw$cn, qchisq(c(0.75, 0.5, 0.25), ncol(dt1))))   
        } else {
          alpha <- sqrt(qchisq(c(0.975, 0.75, 0.5, 0.25), ncol(dt1)))
        }
        o <- (distrob > min(sqrt(xarw$cn), sqrt(qchisq(0.975, dim(dt1[,1:2])[2]))))
        qs <- quantile(distcla[o], prob=c(0.5,0.75,seq(0.9,1,0.01)))
        #res = c(n = dim(dt1)[1],qs)
        res = paste(dim(dt1)[1],round(alpha[1],3),paste(round(qs,3),collapse="|",sep=""),collapse="|",sep="|")
      }
    }
  }
  return(res)
} 
#running the function
toto <- oc.data[!is.na(geo.check1) & geo.check1 %like% "ok_" & !is.na(longitude.work1), mahalanobis.cut1(.SD), by = .(species.correct2)]
toto1 = sapply(toto$V1, strsplit,"\\|")  
toto1 = do.call(rbind.data.frame, toto1)
names(toto1) = c("n","q50","q75","q90","q91","q92","q93","q94","q95","q96","q97","q98","q99","q100")
cut.cla = cbind.data.frame(species.correct2 = tmp$species.correct2, toto1)
for (i in 2:dim(cut.cla)[2]) cut.cla[,i] = as.double(as.character(cut.cla[,i]))

# filtering the data for species occurrring in the Atlantic Forest ##
spp.af = readRDS('spp.af.rds')
spp.af.true = spp.af[af.check2 %in% "TRUE" & spp.group %in% c("valid","valid_not_in_reflora","valid_TBC"),]
tmp2 = cut.rob[cut.rob$species.correct2 %in% spp.af$species.correct2,] # occurrences inside the AF
tmp3 = cut.cla[cut.cla$species.correct2 %in% spp.af$species.correct2,] # occurrences inside the AF

# defining the cut-offs
exp(quantile(log(tmp2$q95[!is.na(tmp2$q95)]), prob = c(0.4,0.5,0.75)))
# 16 is a good cut-off for extreme outliers for maha.rob in the Atlantic Forest
# 11.5 is a good cut-off for possible outliers for maha.rob the in Atlantic Forest - not always (use only the one above for automatic exclusion of specimens)
hist(log(tmp2$q95), nclass= 40)
abline(v=c(11.5,16,17), lty=2)

exp(quantile(log(tmp3$q95[!is.na(tmp3$q95)]), prob = c(0.4,0.5,0.75)))
# 3 is a good cut-off for extreme outliers for maha.rob in the Atlantic Forest
# 2.7 is a good cut-off for possible outliers for maha.rob the in Atlantic Forest - not always (use only the one above for automatic exclusion of specimens)
hist(log(tmp3$q95), nclass= 40)
abline(v=c(2.5,2.75,3), lty=2)

## Flaging the true and probable outliers based on both the cut-offs defines above
oc.data[, true.out := ifelse(maha.cla>3 & maha.rob>16, TRUE, FALSE), ]
oc.data[, probable.out := ifelse(maha.cla>2.5 & maha.rob>11.5, TRUE, FALSE), ]

## Removing the columns that are no longer necessary
oc.data[, maha.cla := NULL,]
oc.data[, maha.rob := NULL,]

#### 2 - NON-CORE OCCURRENCES (may be outliers for some species) ####
## Calculating the default robust-Mahalanobis distance outliers per species (for alpha 2.5% and 1%)
#creating the function
mahalanobis.out <- function(y, alfa) {
  dt = cbind.data.frame(longitude.work1 = as.double(y$longitude.work1), 
                        latitude.work1 = as.double(y$latitude.work1),
                        numTombo = as.character(y$numTombo),
                        dup.ID = as.character(y$dup.ID))
  dt$dup.ID = as.character(dt$dup.ID)
  dt$dup.ID[is.na(dt$dup.ID)] = as.character(dt$numTombo[is.na(dt$dup.ID)])
  dt1 = dt[!duplicated(dt$dup.ID, incomparables = NA),]
  if (length(unique(dt1$longitude.work1))<5 & length(unique(dt1$latitude.work1))<5) {
    dt1$longitude.work1 = jitter(dt1$longitude.work1, factor = 0.001)
    dt1$latitude.work1 = jitter(dt1$latitude.work1, factor = 0.001)
  }
  if(dim(dt1)[1]<10) { 
    res = rep(NA_character_, dim(dt)[1])
  } else {
    rob <- try(covMcd(dt1[,1:2], alpha = 1/2), TRUE)
    if(class(rob)=="try-error") {
      dt1$longitude.work2 = jitter(dt1$longitude.work1, factor = 0.005)
      dt1$latitude.work2 = jitter(dt1$latitude.work1, factor = 0.005)
      rob <- covMcd(dt1[,c("longitude.work2","latitude.work2")], alpha = 1/2, tol=1e-20)
      distrob <- sqrt(mahalanobis(dt1[,1:2], center = rob$center, cov = rob$cov))
      xarw <- arw(dt1[,1:2], rob$center, rob$cov, alpha = alfa)
      if (xarw$cn != Inf) {   
        alpha <- sqrt(xarw$cn)
      } else {
        alpha <- sqrt(qchisq(1-alfa, ncol(dt1)))
      }
      out <- (distrob > max(sqrt(xarw$cn), sqrt(qchisq(1-alfa, dim(dt1[,1:2])[2]))))
      res0 = cbind.data.frame(dup.ID = dt1$dup.ID,  outs = out)
    } else {
      if(length(rob$singularity)>0) {
        dt1$longitude.work2 = jitter(dt1$longitude.work1, factor = 0.05)
        dt1$latitude.work2 = jitter(dt1$latitude.work1, factor = 0.05)
        rob <- covMcd(dt1[,c("longitude.work2","latitude.work2")], alpha = 1/2, tol=1e-20)
        distrob <- sqrt(mahalanobis(dt1[,1:2], center = rob$center, cov = rob$cov))
        xarw <- arw(dt1[,1:2], rob$center, rob$cov, alpha = alfa)
        if (xarw$cn != Inf) {   
          alpha <- sqrt(xarw$cn)
        } else {
          alpha <- sqrt(qchisq(1-alfa, ncol(dt1)))
        }
        out <- (distrob > max(sqrt(xarw$cn), sqrt(qchisq(1-alfa, dim(dt1[,1:2])[2]))))
        res0 = cbind.data.frame(dup.ID = dt1$dup.ID,  outs = out)
      } else {
        #rob <- covMcd(dt1[,1:2], alpha = 1/2)
        #distcla <- sqrt(mahalanobis(dt1[,1:2], center = apply(dt1[,1:2], 2, mean), cov = cov(dt1[,1:2])))
        distrob <- sqrt(mahalanobis(dt1[,1:2], center = rob$center, cov = rob$cov))
        xarw <- arw(dt1[,1:2], rob$center, rob$cov, alpha = alfa)
        if (xarw$cn != Inf) {   
          alpha <- sqrt(xarw$cn)
        } else {
          alpha <- sqrt(qchisq(1-alfa, ncol(dt1)))
        }
        out <- (distrob > max(sqrt(xarw$cn), sqrt(qchisq(1-alfa, dim(dt1[,1:2])[2]))))
        res0 = cbind.data.frame(dup.ID = dt1$dup.ID,  outs = out)
      }
    }
    #Putting data back in the original order
    res1 = merge(dt[,c("numTombo","dup.ID")], res0, by = "dup.ID", all.x = TRUE, sort=FALSE)
    res1 = res1[order(match(res1$numTombo, dt$numTombo)),]
    res =as.character(res1$outs)
  }
  return(res)
}
#running the function
oc.data[geo.check1 %like% "ok_" & !is.na(longitude.work1), rob.out.97.5 := mahalanobis.out(.SD, alfa = 0.025), by = .(species.correct2)]
oc.data[geo.check1 %like% "ok_" & !is.na(longitude.work1), rob.out.99 := mahalanobis.out(.SD, alfa = 0.01), by = .(species.correct2)]
#oc.data[,rob.out.97.5 := NULL,]
#oc.data[,rob.out.99 := NULL,]


#### NON-NEOTROPICAL COORDINATES ####
#creating the function
neotrop.coord = function(y) {
  dt = cbind.data.frame(longitude.work1 = suppressWarnings(as.double(y$longitude.work1)), 
                        latitude.work1 = suppressWarnings(as.double(y$latitude.work1)),
                        numTombo = as.character(y$numTombo),
                        dup.ID = as.character(y$dup.ID))
  dt$dup.ID = as.character(dt$dup.ID)
  dt$dup.ID[is.na(dt$dup.ID)] = as.character(dt$numTombo[is.na(dt$dup.ID)])
  dt1 = dt[!is.na(dt$longitude.work1),]
  dt1 = dt1[!is.na(dt1$latitude.work1),]
  dt1 = dt1[!duplicated(dt1$dup.ID),]
  if(dim(dt1)[1] == 0) {
    res = rep(NA_character_, dim(dt)[1])
  } else {
    # creating the spatial points
    p = SpatialPoints(dt1[,1:2], CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    # creating the Neotropical boundaries
    coords = matrix(c(-120, 34,
                      -120, -58,
                      -30, -58,
                      -30, 34), 
                      ncol = 2, byrow = TRUE)
    N = Polygon(coords)
    N = SpatialPolygons(list(Polygons(list(N), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    # overlaying the points in the polygon (note: over performed better than functions 'gIntersects' and 'st_intersects')
    ov = over(p, N, fun=NULL)
    res0 = ov==1
    res0[is.na(res0)] = FALSE
    res0 = cbind.data.frame(dup.ID = dt1$dup.ID,  check = res0)
    # putting data back in the original order
    res1 = merge(dt[,c("numTombo","dup.ID")], res0, by = "dup.ID", all.x = TRUE, sort=FALSE)
    res1 = res1[order(match(res1$numTombo, dt$numTombo)),]
    res = as.character(res1$check)
  }
  return(res)
} 
# performing the validation
oc.data[, neotrop.check := neotrop.coord(.SD), by = .(species.correct2)]
#oc.data[,neotrop.check := NULL,]

## Detecting places, collections and species with lots of outliers of Atlantic Forest species
spp.af = readRDS('spp.af.rds')
spp.af.true <- spp.af[af.check2 %in% "TRUE",]
tmp <- oc.data[species.correct2 %in% spp.af$species.correct2,] # occurrences inside the AF

## Ouliers outside the neotropics
tmp1 <- tmp[true.out %in% TRUE & neotrop.check %in% FALSE,]# & species.correct2 %in% "Araucaria angustifolia",]

#top localities per country
tmp2 = aggregate(tmp1$locality, list(tmp1$country,tmp1$locality), length)
tmp2 = tmp2[order(tmp2$x, decreasing = TRUE),]
head(tmp2, 20)

#top Atlantic Forest species outliers outside the neotropics
tmp2 = aggregate(tmp1$species.correct2, list(tmp1$species.correct2), length)
#tmp2 = tmp2[order(tmp2$x, decreasing = TRUE),]

#top South America species outliers outside the neotropics
tmp0 <- oc.data[true.out %in% TRUE & neotrop.check %in% FALSE,]
tmp3 = aggregate(tmp0$species.correct2, list(tmp0$species.correct2), length)
tmp3 = merge(tmp3, tmp2, by = "Group.1", all.x = TRUE)
tmp3 = tmp3[order(tmp3$x.x, decreasing = TRUE),]
tmp3 = tmp3[order(tmp3$x.y, decreasing = TRUE),]
write.csv(tmp3, "check.csv")

#top Non-neotropical/total occurrences
tmp3 = as.data.frame.matrix(table(tmp$species.correct2, tmp$neotrop.check))
tmp3$prop = tmp3[,1]/apply(tmp3,1,sum)
tmp3 = tmp3[order(tmp3$prop, decreasing = TRUE),]
write.csv(tmp3, "check1.csv")

## Outliers within the Neotropics
tmp1 <- tmp[true.out %in% TRUE & neotrop.check %in% TRUE,]
tmp2 = aggregate(tmp1$species.correct2, list(tmp1$species.correct2), length)
tmp2 = tmp2[order(tmp2$x, decreasing = TRUE),]

tmp3 = aggregate(tmp1$locality,list(tmp1$municipio), length)
tmp3 = tmp3[order(tmp3$x, decreasing = TRUE),]
tail(sort(table(tmp1[municipio %in% "linhares",]$species.correct2)),10)


#######################################################
#### SAVING THE VERIFIED OUTLIERS FAMILY BY FAMILY ####
#######################################################

#### Saving data per family ####
###List of families
familias = sort(unique(oc.data$family.correct))

#Path to the folder in the local directory to receive the files
path="C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo AF checklist//data analysis//occurrence_data"

#Trying to make processing faster
oc.data[, family.correct1 := as.factor(family.correct1)] # converting family to factors
setkeyv(oc.data, "family.correct1") # setting 'family.correct1' as key to the data.table (makes computations faster)

#Saving the data family by family
for (i in 1:length(familias)){
  familia = familias[i]
  dado = oc.data[familia]
  nome = paste(path, paste(familia,"_merged_outliers.csv",sep=""),sep="//")
  fwrite(x=dado, file= nome)
  #write.csv(x=dado, file= nome)
} 




###########################################################################################################################################################################
###########################################################################################################################################################################
#### OLD CODES (DO NOT RUN) ####

sppp = c("Psidium cattleianum","Euterpe edulis","Araucaria angustifolia","Antrocaryon amazonicum","Terminalia amazonia","Casearia sylvestris","Cabralea canjerana",
      "Myrcia guianensis","Myrcia splendens","Plinia complanata","Myrcia insularis","Curitiba prismatica","Xylopia aromatica",
      "Paubrasilia echinata","Libidibia ferrea","Schinus terebinthifolia","Dodonaea viscosa","Senna pendula","Magnolia ovata","Zygia unifoliolata",
      "Abarema acreana")
spp = sppp[1] 

tmp = oc.data[species.correct2 %in% spp & geo.check1 %like% "ok_",]
tmp = tmp[!is.na(tmp$longitude.work1),]
dt = cbind.data.frame(longitude.work1 = as.double(as.character(tmp$longitude.work1)),
                      latitude.work1 = as.double(as.character(tmp$latitude.work1)))

## Mahalanobis-Robust distance plot (Package mvoutlier)
par(mfrow=c(2,3), mar=c(4,4,1,1), mgp = c(2.5,0.8,0))
#xlim = c(-110, -30)
#ylim = c(-60, 35)
dd <- dd.plot(dt[,1:2])
rob <- covMcd(dt[,1:2], alpha = 1/2)
distcla <- sqrt(mahalanobis(dt[,1:2], center = apply(dt[,1:2], 2, mean), cov = cov(dt[,1:2])))
distrob <- sqrt(mahalanobis(dt[,1:2], center = rob$center, cov = rob$cov))
#cut1 = quantile(distrob[dd$outliers], prob=c(0.95))*1.2
#cut2 = quantile(distcla[dd$outliers], prob=c(0.95))*1.2
#outs = dd$md.rob>cut1 & dd$md.cla>cut2
outs= dd$md.rob>=10 & dd$md.cla>=2.5
#outs1= dd$md.rob>cut1 | dd$md.cla>cut2
outs2= dd$md.rob>=17 & dd$md.cla>=3

plot(dt, col="white", main="Mahalanobis-Robust distance plot")#, xlim=xlim, ylim=ylim)
points(dt[dd$outliers,], pch=19, cex=0.8, col = "red")
points(dt[!dd$outliers,], pch=19, cex=0.8, col = "blue")
#plot(neotrop, add=TRUE)
plot(dt, col="white", main = "cut1 & cut2")#, xlim=xlim, ylim=ylim)
points(dt[outs,], pch=19, cex=0.8, col = "red")
points(dt[!outs,], pch=19, cex=0.8, col = "blue")
# legend("topright",spp,bty="n", cex=1.2)
# plot(dt, col="white", main = "cut1 | cut2")
# points(dt[outs1,], pch=19, cex=0.8, col = "red")
# points(dt[!outs1,], pch=19, cex=0.8, col = "blue")
plot(dt, col="white", main = "cut1>17&cut2>3")#, xlim=xlim, ylim=ylim)
legend("topright",spp,bty="n", cex=1.2)
points(dt[!outs2,], pch=19, cex=0.8, col = "blue")
points(dt[outs2,], pch=19, cex=0.8, col = "red")
#npo <- sum(outs2)	# how many potential outliers are identified ?

## Mahalanobis distance (Package rrcovHD)
obj <- OutlierMahdist(dt)
getCutoff(obj)              # returns an array of cutoff values (for each class, usually equal)
ot <- getOutliers(obj)
plot(dt, col="white", main = "Mahalanobis distance")#, xlim=xlim, ylim=ylim)
points(dt[ot,], pch=19, cex=0.8, col = "red")
points(dt[-ot,], pch=19, cex=0.8, col = "blue")

## Principal components (Package rrcovHD)
# obj <- OutlierPCOut(dt)
# ot1 <- getOutliers(obj)
# plot(dt, col="white", main="Principal components")#, xlim=xlim, ylim=ylim)
# points(dt[ot1,], pch=19, cex=0.8, col = "red")
# points(dt[-ot1,], pch=19, cex=0.8, col = "blue")

## Spatial signs (Package rrcovHD)
# obj <- OutlierSign2(dt)
# ot2 <- getOutliers(obj)
# plot(dt, col="white", main="Spatial signs")#, xlim=xlim, ylim=ylim)
# points(dt[ot1,], pch=19, cex=0.8, col = "red")
# points(dt[-ot1,], pch=19, cex=0.8, col = "blue")

## package "kernlab" - not working
# sv <- ksvm(dt, type = "one-svc")
# psv1 <- predict(sv,dt)

## package "Rlof" - too poor
# ot3 <- lof(dt, k=10) # MinPtsLB is that it should be at least 10 to remove un-wanted statistical fluctuations
# ot3[is.infinite(ot3)] = NA
# quantile(ot, prob=c(0.95),na.rm= TRUE)
# ot3 = ot3>=5
# ot3[is.na(ot3)] = FALSE
# plot(dt, col="white", main="local outlier factor")
# points(dt[ot3,], pch=19, cex=0.8, col = "red")
# points(dt[!ot3,], pch=19, cex=0.8, col = "blue")

#	Isolation Forest - not working
# tr <- IsolationTrees(as.data.frame(dt),rFactor=1,rowSamp=TRUE,nRowSamp=100,colSamp=FALSE)
# as <- AnomalyScore(as.data.frame(dt),tr)
# ot4 <- as$outF	# show anomaly score

# Random Forest - too poor
# rf <- randomForest(dt, ntree=50, proximity=TRUE)
# ot5 <- outlier(rf)
# plot(dt, col="white", main="random forest")
# points(dt[ot5>1,], pch=19, cex=0.8, col = "red")
# points(dt[ot5<=1,], pch=19, cex=0.8, col = "blue")

# Kernel density
quant = 0.025
X = SpatialPoints(dt, CRS("+proj=longlat +datum=WGS84"))
Y = ppp(x=dt[,1], y=dt[,2], c(-180, 180),c(-70, 70) ) # whole world
#Y = ppp(x=dt[,1], y=dt[,2], c(-120, -25),c(-65, 35) ) # only neotropics
D = density(Y, kernel = "gaussian")
d = extract(raster(D), X)
d[is.na(d)] = 0
qp = quantile(d, quant)
ot6 = d > qp
plot(dt, col="white", main="kernel density")#, xlim=xlim, ylim=ylim)
points(dt[ot6,], pch=19, cex=0.8, col = "blue")
points(dt[!ot6,], pch=19, cex=0.8, col = "red")
# plot(D, main= "density kernel estimation")
# points(X, pch=16, cex=0.5, col="green")
# contour(D, axes = FALSE, add = T)
# #add.geography(draw.forestborder = T)
# points(X[d<qp], pch=16, cex=0.5, col="red")
#plot(neotrop, add=TRUE)



