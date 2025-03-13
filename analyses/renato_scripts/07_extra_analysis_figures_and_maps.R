##########################################
#### EXTRA DATA AANALYSIS AND FIGURES ####
##########################################
rm(list=ls())

#### LOADING PACKAGES  ####
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
require(MASS)
require(fields)



## reading the resultes and diversity metrics per grid cell
grid.result = readRDS("grid.results_50km_true_prob_taxonomy.rds")[[1]]
samp.cov.cutoff = readRDS("grid.results_50km_true_prob_taxonomy.rds")[[2]]
grid.result2 = readRDS("grid.results_50km_true_taxonomy.rds")[[1]]
samp.cov.cutoff2 = readRDS("grid.results_50km_true_taxonomy.rds")[[2]]

####################################################################################################################################################################################H
####################################################################################################################################################################################H
#### FIGURES ####
#################

## Defining the sampling coverage cut-off for analysis
#For rarefaction, a temptative minimum sampling is:
plot(qS.50.total.correct ~ S.total, grid.result, log="xy"); abline(0,1); abline(v=30)
plot(qD.50.total.correct ~ S.total, grid.result, log="xy"); abline(0,1); abline(v=c(30,50))
plot(qS.100.total.correct ~ S.total, grid.result, log="xy"); abline(0,1); abline(v=50)
plot(qD.100.total.correct ~ S.total, grid.result, log="xy"); abline(0,1); abline(v=50)
plot(qS.50.total.correct ~ S.total, grid.result2, log="xy"); abline(0,1); abline(v=30)
plot(qD.50.total.correct ~ S.total, grid.result2, log="xy"); abline(0,1); abline(v=c(30,50))
plot(qS.100.total.correct ~ S.total, grid.result2, log="xy"); abline(0,1); abline(v=50)
plot(qD.100.total.correct ~ S.total, grid.result2, log="xy"); abline(0,1); abline(v=50)
# 0.35-0.40%, for qS.50 and qD.50
# 0.44-0.46% (equivalente a 50 species per cell) for qS.100 and qD.100

#What does it mean to remove cells above a given quantile of the sampling coverage distribution?
result = NULL; result1 = NULL
for (i in 1:length(samp.cov.cutoff)) {
  toto = samp.cov.cutoff[i]
  toto1= summary(grid.result$N.total[grid.result$SampCover<toto])
  toto2= summary(grid.result$N.total[grid.result$SampCover>=toto])
  toto3= c(toto1[c(3,4,1,6)],toto2[c(3,4,1,6)]) 
  toto4= summary(grid.result$S.total[grid.result$SampCover<toto])
  toto5= summary(grid.result$S.total[grid.result$SampCover>=toto])
  toto6= c(toto4[c(3,4,1,6)],toto5[c(3,4,1,6)]) 
  if(is.null(result)) result = toto3 else result = rbind.data.frame(result, toto3)
  if(is.null(result1)) result1 = toto6 else result1 = rbind.data.frame(result1, toto6)
}
row.names(result) = row.names(result1) = names(samp.cov.cutoff)
colnames(result) = colnames(result1) = c("med.excl","mean.excl","min.excl","max.excl","med.incl","mean.incl","min.incl","max.incl")
result
result1
cut = samp.cov.cutoff[4] # using the 50% quantile (mean) of the sampling coverage distribution
head(sort(grid.result$N.total[grid.result$SampCover>cut]),50) ## REMOVE ALSO CELLS WITH LESS THAN < 30 OCCURRENCES

#Inspecting other possibilities of data filtering
grid.result$select = ((grid.result$SampCover>=samp.cov.cutoff[5]  & grid.result$N.total>=30) | 
                        (grid.result$SampCover>=samp.cov.cutoff[4] & grid.result$SampCover<samp.cov.cutoff[5] & grid.result$N.total>=50) | 
                        (grid.result$SampCover>=samp.cov.cutoff[3] & grid.result$SampCover<samp.cov.cutoff[4] & grid.result$N.total>=100))
plot(grid.result$SampCover ~ grid.result$N.total, log="x")
abline(v=c(25,50,100), col=c(1,2,3)); abline(h=samp.cov.cutoff[3:5], col=c(3,2,1))
points(SampCover ~ N.total, subset(grid.result,select), col=2, pch=21)
summary(grid.result$N.total[grid.result$select])
head(sort(grid.result$N.total[grid.result$select]),50)
summary(grid.result$SampCover[grid.result$select])


## Atlantic Forest Maps 
## Defining the general path to read/save files (tifs and shape files) ###
path = "E://ownCloud//W_GIS"
#neotrop <- readRDS(paste(path,"//Am_Lat_ADM_GADM_v3.6//gadm36_Neotrop_0_sp.rds",sep=""))
am.lat <- readOGR(dsn=paste(path,"//Am_Lat_ADM_ArcGis",sep=""),layer="LatinAmerica")
am.lat.proj= spTransform(am.lat,  CRS("+init=epsg:5641"))
#s.am <- readRDS(paste(path,"//Am_Lat_ADM_GADM_v3.6//gadm36_SouthAm_0_sp.rds",sep=""))
brasil = readRDS(paste(path,"//Am_Lat_ADM_GADM_v3.6//gadm36_BRA_1_sp.rds",sep=""))
brasil.proj= spTransform(brasil,  CRS("+init=epsg:5641"))
#af <- readOGR(dsn=paste(path,"//AF_limites_milton//ma_novo",sep=""),layer="Atlantic_MMA_miltinho_LEI_WWF_INTERNATIONAL_gcs_wgs84")
af = readOGR(dsn=paste(path,"//AF_limites_milton",sep=""),layer="merge_limites_MA11428_TNC_ARG_BR_PAR")
af = gBuffer(af, byid=TRUE, width=0) #correcting possible overlapping polygons
af = clgeo_Clean(af) # fixing possible problems with the new aggregated polygon (e.g. orphaned holes)
af = aggregate(af, dissolve=TRUE)
af.buf <- readOGR(dsn=paste(path,"//AF_limites_milton",sep=""),layer="merge_limites_MA_buffer20km_ajustado")
af.buf = spTransform(af.buf, crs(am.lat))
af.buf = crop(af.buf, am.lat) #clipping the AF buffer
rm(am.lat)
af.buf = spTransform(af.buf,  CRS("+init=epsg:5641"))
af.proj= spTransform(af,  CRS("+init=epsg:5641"))

## Getting the grid and editing it
grid = readRDS("C://Users//renato//Documents//raflima//Pos Doc//Manuscritos//Artigo Hyperdominance//af_proj_50km.rds")

## Making sure grid IDs are the same internally
grid1 = grid
tmp = SpatialPoints(coords = coordinates(grid),proj4string = crs(grid))
tmp1= over(tmp,grid)
for(i in 1:length(grid1)) { slot(slot(grid1, "polygons")[[i]],"ID") = as.character(paste("ID",i,sep=""))} 
#plot(grid1)


##################H
#### FIGURE S0 ####
##################H
## NEW FIGURE REQUIRED BY REVIEWER 2

alt.af <- raster(paste(path,"//AF_Altitude_WClim//Altitude1.tif",sep=""))
alt.af <- crop(alt.af, af)
alt.af1 <- mask(alt.af, af)
alt.af1[alt.af1<0] <- 0

municipios <- readOGR(dsn=paste(path,"//BR_municipios_IBGE",sep=""),layer="BRMUE250GC_SIR")
municipios <- municipios[grepl("^35",municipios$CD_GEOCMU),]

jpeg(filename = "FigureS0.jpg", width = 3100*2, height = 3000*1.5, units = "px", pointsize = 12,
     res = 600, family = "sans", type="cairo", bg="white")
par(mfrow=c(1,2), las=1,mar=c(1,1,0.2,0.1),mgp=c(1.7,0.4,0),tcl=-.3)
#Atlantic Forest topography
plot(af, border="white")
image(alt.af1, add=TRUE, col = rev(terrain.colors(255)))
plot(brasil, add=TRUE, lty="1414", border= "lightgrey")
plot(af, add=TRUE, border= 1)
plot(am.lat, add=TRUE, border = 1)
legend("topleft", "A - Topography", cex=1.5, bty="n",inset=c(-0.05,0.02))
image.plot( legend.only=TRUE, col = rev(terrain.colors(255)), add=TRUE,
            legend.shrink = 0.5, legend.width = 1.5, legend.mar = 4, 
            legend.line = 1, legend.cex = 1,
            zlim=c(1,2500),
            smallplot= c(.75,.8,0.045,0.36),
            #breaks = seq(0,100,20),	 
            axis.args=list(at=seq(0,2500,500),  mgp=c(1.5,0.6,0), tcl=-.3, cex.axis=1, labels=seq(0,2500,500)))
legend(-48.3,-34.2,expression(bold("Altitude (m)")),cex=1.1,bty="n",y.intersp=-0.01)
ext <- extent(c(-58.75,-34,-35.6,-0.95))
plot(ext, add=TRUE, col= 1, lwd=2)
#GISTools::north.arrow(xb = -56, yb=-39, len = 0.22, lab="N")

# rect(xleft =-58.5, ybottom = -10,xright = -52,ytop = -1.15, col="white")
# par(usr=c(-85, -55,-39, 15))
# par(usr=c(-58, -52, -10, -1))
# #rect(xleft = -125,ybottom = 25,xright = -66,ytop = 50.5,col = "white")
# plot(am.lat, add=TRUE,
#       xlim=c(-85,-39),ylim=c(-55,15),
#       lwd=1, bty="l", axes = FALSE, cex.axis=0.6, border = "black")

#Position of the occurrences
par(las=1,mar=c(2,2,0.1,0.1),mgp=c(1.7,0.4,0),tcl=-.3)
test.municip <- municipios[municipios$NM_MUNICIP %in% c("RIO CLARO","IPEÃšNA","ITIRAPINA","SANTA GERTRUDES"
                                                        #,"ARARAS","BROTAS","CHARQUEADA","CORUMBATAÃ\u008d","LEME","AJAPI","SÃƒO PEDRO","SÃƒO CARLOS"
                                                        #,"PIRACICABA","LIMEIRA","CORDEIRÃ“POLIS"
                                                        ),]
ext1 <- extent(c(-47.965,-47.44,-22.559,-22.125))
af1 <- crop(af, ext1, add=TRUE)
test.municip1 <- crop(test.municip, ext1, add=TRUE)
plot(test.municip1, 
     xlim = c(-47.95,-47.45), ylim = c(-22.45,-22.25), axes=FALSE, xlab="")
plot(af1, border= 3, add= TRUE, lwd=2, col=adjustcolor(3,alpha.f=0.3))
plot(ext1, col=1, lwd=2, add=TRUE)
points(c(-47.82243, -47.70701, -47.60231, -47.51492),
       c(-22.38088, -22.41519, -22.44493, -22.47238),
       pch=c(15,0,21,19), cex=1.2)
legend("topleft", "B - Position of records", cex=1.5, bty="n",inset=c(-0.05,0.02))
legend("bottomleft",expression(Outside~(italic(O)[out]),
                               Outside~"in"~the~transition(italic(O)[to]),
                               Inside~"in"~the~transition~(italic(O)[ti]),
                               Inside~(italic(O)["in"])),
       cex=1.1,bty="n",pch=c(15,0,21,19))
legend("bottomright", c("County limits", "Atlantic Forest"),
       cex=1.1,bty="n",lwd=c(1,2), col=c(1,3))
dev.off()

##################H
#### FIGURE S1 ####
##################H
jpeg(filename = "FigureS1.jpg", width = 3100*1.5, height = 3000*1.5, units = "px", pointsize = 12,
     res = 600, family = "sans", type="cairo", bg="white")
par(mfrow=c(2,2), las=1,mar=c(3,3,0.5,0.25),mgp=c(1.7,0.4,0),tcl=-.3)
#Pure
dados = na.omit(grid.result[,c("S.pure2","N.pure2","SampCover","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut & dados[,2]>=10,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover
tmp0 = lm(log(y) ~ 1, data=dados)
tmp  = rlm(log(y)~ log(x), data=dados)
tmp1 = lm(log(y) ~ log(x), data=dados, weights = w)
tmp2 = lm(log(y) ~ log(x), data=dados, weights = w^3)
#tmp3 = gls(log(y)~ log(x), data=dados, corr = corExp(c(3.335227e+08,2.699452e-02), form= ~long + lat, nugget = TRUE), control = list(maxIter = 200, tolerance = 0.01))
tmp3 = gls(log(y)~ log(x), data=dados, corr = corExp(c(3.335227e+08), form= ~long + lat, nugget = FALSE), control = list(maxIter = 200, tolerance = 0.01))
# tmp4 = gls(log(y)~ log(x), data=dados, corr = corExp(c(3.335226e+06,5.664609e-02), form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(log(y)~ log(x), data=dados, corr = corExp(c(3.335227e+08,2.699452e-03), form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3)#,tmp4,tmp5)
#plot(S.pure ~ N.pure, data=grid.result, log="xy",
#     xlab="Number of occurrences",ylab="Number of species")
plot(y ~ x, data=grid.result, log="xy",
     xlab="Number of occurrences",ylab="Number of species")
#curve(exp(predict(tmp, newdata=list(x=x))), col="blue", lwd=2, add=TRUE, from = 1, to = 1750)
curve(exp(predict(tmp3, newdata=list(x=x))), col="red", lwd=2, add=TRUE, from = 1, to = 1750)
#curve(exp(predict(tmp4, newdata=list(x=x))), col="red", lwd=2, add=TRUE, from = 1, to = 1750)
#lines(x, exp(predict(tmp, newdata=list(x=x))) ,col="red", lwd=2)
legend("topleft",expression(bold((a))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(log(y),predict(tmp3))^2
R2.1 <- 1 - with(dados, (sum((log(y)-predict(tmp3))^2)/sum((log(y)-mean(log(y)))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("bottomright",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.01,-0.03)
)
#Near
dados = na.omit(grid.result[,c("S.near2","N.near2","SampCover","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut & dados[,2]>=10,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover
tmp0 = lm(log(y) ~ 1, data=dados)
tmp  = rlm(log(y)~ log(x), data=dados)
tmp1 = lm(log(y) ~ log(x), data=dados, weights = w)
tmp2 = lm(log(y) ~ log(x), data=dados, weights = w^3)
#tmp3 = gls(log(y)~ log(x), data=dados, corr = corExp(c(3.335227e+08,2.699452e-02), form= ~long + lat, nugget = TRUE), control = list(maxIter = 200, tolerance = 0.01))
tmp3 = gls(log(y)~ log(x), data=dados, corr = corExp(c(3.335227e+08), form= ~long + lat, nugget = FALSE), control = list(maxIter = 200, tolerance = 0.01))
# tmp4 = gls(log(y)~ log(x), data=dados, corr = corExp(c(1.5e+08,0.002), form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(log(y)~ log(x), data=dados, corr = corExp(c(1.5e+08,0.002), form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3,tmp4,tmp5)
#plot(S.near ~ N.near, data=grid.result, log="xy",
#     xlab="Number of occurrences",ylab="Number of species")
plot(y ~ x, data=grid.result, log="xy",
     xlab="Number of occurrences",ylab="Number of species")
#curve(exp(predict(tmp, newdata=list(x=x))), col="blue", lwd=2, add=TRUE, from = 1, to = 4250)
curve(exp(predict(tmp3, newdata=list(x=x))), col="red", lwd=2, add=TRUE, from = 1, to = 4250)
#lines(x, exp(predict(tmp, newdata=list(x=x))) ,col="red", lwd=2)
legend("topleft",expression(bold((b))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(log(y),predict(tmp3))^2
#R2.1 <- 1 - with(dados, (sum((log(y)-predict(tmp3))^2)/sum((log(y)-mean(log(y)))^2)))
#R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("bottomright",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.01,-0.03)
)
#Endemics
dados = na.omit(grid.result[,c("S.pure.near2","N.pure.near2","SampCover","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut & dados[,2]>=10,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover
tmp0 = lm(log(y) ~ 1, data=dados)
tmp  = rlm(log(y)~ log(x), data=dados)
tmp1 = lm(log(y) ~ log(x), data=dados, weights = w)
tmp2 = lm(log(y) ~ log(x), data=dados, weights = w^3)
#tmp3 = gls(log(y)~ log(x), data=dados, corr = corExp(c(3.335227e+08,2.699452e-02), form= ~long + lat, nugget = TRUE), control = list(maxIter = 200, tolerance = 0.01))
tmp3 = gls(log(y)~ log(x), data=dados, corr = corExp(c(3.335227e+08), form= ~long + lat, nugget = FALSE), control = list(maxIter = 200, tolerance = 0.01))
# tmp4 = gls(log(y)~ log(x), data=dados, corr = corExp(c(1.5e+08,0.002), form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(log(y)~ log(x), data=dados, corr = corExp(c(1.5e+08,0.002), form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3,tmp4,tmp5)
# plot(S.pure.near ~ N.pure.near, data=grid.result, log="xy",
#      xlab="Number of occurrences",ylab="Number of species")
plot(y ~ x, data=grid.result, log="xy",
     xlab="Number of occurrences",ylab="Number of species")
#curve(exp(predict(tmp, newdata=list(x=x))), col="blue", lwd=2, add=TRUE, from = 1, to = 4250)
curve(exp(predict(tmp3, newdata=list(x=x))), col="red", lwd=2, add=TRUE, from = 1, to = 4250)
#lines(x, exp(predict(tmp, newdata=list(x=x))) ,col="red", lwd=2)
legend("topleft",expression(bold((c))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(log(y),predict(tmp3))^2
#R2.1 <- 1 - with(dados, (sum((log(y)-predict(tmp3))^2)/sum((log(y)-mean(log(y)))^2)))
#R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("bottomright",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.01,-0.03)
)
#Ocasional
dados = na.omit(grid.result[,c("S.ocas2","N.ocas2","SampCover","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut & dados[,2]>=10,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover
tmp0 = lm(log(y) ~ 1, data=dados)
tmp  = rlm(log(y)~ log(x), data=dados)
tmp1 = lm(log(y) ~ log(x), data=dados, weights = w)
tmp2 = lm(log(y) ~ log(x), data=dados, weights = w^3)
#tmp3 = gls(log(y)~ log(x), data=dados, corr = corExp(c(3.335227e+08,2.699452e-02), form= ~long + lat, nugget = TRUE), control = list(maxIter = 200, tolerance = 0.01))
tmp3 = gls(log(y)~ log(x), data=dados, corr = corExp(c(3.335227e+08), form= ~long + lat, nugget = FALSE), control = list(maxIter = 200, tolerance = 0.01))
# tmp4 = gls(log(y)~ log(x), data=dados, corr = corExp(c(1.5e+08,0.002), form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(log(y)~ log(x), data=dados, corr = corExp(c(1.5e+08,0.002), form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3,tmp4,tmp5)
# plot(S.ocas ~ N.ocas, data=grid.result, log="xy",
#      xlab="Number of occurrences",ylab="Number of species")
plot(y ~ x, data=grid.result, log="xy",
     xlab="Number of occurrences",ylab="Number of species")
#curve(exp(predict(tmp, newdata=list(x=x))), col="blue", lwd=2, add=TRUE, from = 1, to = 4250)
curve(exp(predict(tmp3, newdata=list(x=x))), col="red", lwd=2, add=TRUE, from = 1, to = 4250)
#lines(x, exp(predict(tmp, newdata=list(x=x))) ,col="red", lwd=2)
legend("topleft",expression(bold((d))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(log(y),predict(tmp3))^2
#R2.1 <- 1 - with(dados, (sum((log(y)-predict(tmp3))^2)/sum((log(y)-mean(log(y)))^2)))
#R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("bottomright",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.01,-0.03)
)
dev.off()

##################H
#### FIGURE S2 ####
##################H
jpeg(filename = "FigureS2.jpg", width = 3100*1.5, height = 3000*1.5, units = "px", pointsize = 12,
     res = 600, family = "sans", type="cairo", bg="white")
par(mfrow=c(2,2), las=1,mar=c(3,3,0.5,0.25),mgp=c(1.8,0.35,0),tcl=-.25)
#Species Richness (q=0)
dados = na.omit(grid.result[,c("S.total","N.total","SampCover","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover 
tmp0 = lm(log(y) ~ 1, data=dados)
tmp  = rlm(log(y) ~ log(x), data=dados)
tmp1 = lm(log(y) ~ log(x), data=dados, weights = w)
tmp2 = lm(log(y) ~ log(x), data=dados, weights = w^3)
tmp3 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = FALSE))
# tmp4 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3,tmp4,tmp5)
#plot(S.total  ~ N.total, data=grid.result, log="xy",
#     xlab="Number of occurrences",ylab="Observed richness")
plot(y  ~ x, data=grid.result, log="xy",
     xlab="Number of occurrences",ylab="Observed richness")
#curve(exp(predict(tmp, newdata=list(x=x))),
#      col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp, newdata=list(x=x))),col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp1, newdata=list(x=x))),col="green", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp2, newdata=list(x=x))),col="gold", lwd=2, add=TRUE, from = 1, to = 10000)
curve(exp(predict(tmp3, newdata=list(x=x))),col="red", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp4, newdata=list(x=x))),col="cyan", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp5, newdata=list(x=x))),col="brown", lwd=2, add=TRUE, from = 1, to = 10000)
legend("topleft",expression(bold((a))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(log(y),predict(tmp3))^2
R2.1 <- 1 - with(dados, (sum((log(y)-predict(tmp3))^2)/sum((log(y)-mean(log(y)))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("bottomright",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.01,-0.03)
)
#CWE
dados = na.omit(grid.result[,c("CWE.total.correct","N.total","SampCover","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover
tmp0 = gls(log(y) ~ 1, data=dados)
tmp  = rlm(log(y) ~ log(x), data=dados)
tmp1 = gls(log(y) ~ log(x), data=dados, weights = ~w)
tmp2 = gls(log(y) ~ log(x), data=dados, weights = ~w^3)
tmp3 = gls(log(y) ~ log(x), data=dados, corr = corExp(c(3e+06), form= ~long + lat, nugget = FALSE), control = list(maxIter = 20, tolerance = 0.01))
# tmp4 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3,tmp4,tmp5)
#plot(WE.total.correct  ~ N.total, data=grid.result, log="xy",
#     xlab="Number of occurrences",ylab="Weighted endemism")
plot(y  ~ x, data=grid.result, log="xy",
     xlab="Number of occurrences",ylab="Corrected weighted endemism")
#curve(exp(predict(tmp, newdata=list(x=x))),
#      col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
curve(exp(predict(tmp3, newdata=list(x=x))), 
      col="red", lwd=2, add=TRUE, from = 1, to = 10000)
legend("topleft",expression(bold((b))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(log(y),predict(tmp3))^2
R2.1 <- 1 - with(dados, (sum((log(y)-predict(tmp3))^2)/sum((log(y)-mean(log(y)))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("bottomright",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.01,-0.03)
)
#Rarefied/extrapolated richness (q=0)
dados = na.omit(grid.result[,c("qS.100.total.correct","N.total","SampCover","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover 
tmp0 = lm(log(y) ~ 1, data=dados)
tmp  = rlm(log(y) ~ log(x), data=dados)
tmp1 = lm(log(y) ~ log(x), data=dados, weights = w)
tmp2 = lm(log(y) ~ log(x), data=dados, weights = w^3)
tmp3 = gls(log(y) ~ log(x), data=dados, corr = corExp(c(3e+06), form= ~long + lat, nugget = FALSE), control = list(maxIter = 20, tolerance = 0.01))
# tmp4 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3,tmp4,tmp5)
#plot(S.total.correct  ~ N.total, data=grid.result, log="xy",
#     xlab="Number of occurrences",ylab="Species richness (q = 0)")
plot(y  ~ x, data=grid.result, log="xy",
     xlab="Number of occurrences",ylab="Rarefied/extrapolated richness")
#curve(exp(predict(tmp, newdata=list(x=x))),
#      col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp, newdata=list(x=x))),col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp1, newdata=list(x=x))),col="green", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp2, newdata=list(x=x))),col="gold", lwd=2, add=TRUE, from = 1, to = 10000)
curve(exp(predict(tmp3, newdata=list(x=x))),col="red", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp4, newdata=list(x=x))),col="cyan", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp5, newdata=list(x=x))),col="brown", lwd=2, add=TRUE, from = 1, to = 10000)
legend("topleft",expression(bold((c))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(log(y),predict(tmp3))^2
R2.1 <- 1 - with(dados, (sum((log(y)-predict(tmp3))^2)/sum((log(y)-mean(log(y)))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("bottomright",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.01,-0.03)
)
#Effective Richness  (q=2)
dados = na.omit(grid.result[,c("D.total.correct","N.total","SampCover","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover 
tmp0 = lm(log(y) ~ 1, data=dados)
tmp  = rlm(log(y) ~ log(x), data=dados)
tmp1 = lm(log(y) ~ log(x), data=dados, weights = w)
tmp2 = lm(log(y) ~ log(x), data=dados, weights = w^3)
tmp3 = gls(log(y) ~ log(x), data=dados, corr = corExp(c(3e+06), form= ~long + lat, nugget = FALSE), control = list(maxIter = 20, tolerance = 0.01))
# tmp4 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3,tmp4,tmp5)
#plot(D.total.correct  ~ N.total, data=grid.result, log="xy",
#     xlab="Number of occurrences",ylab="Effective richness (S2)")
plot(y  ~ x, data=grid.result, log="xy",
     xlab="Number of occurrences",ylab="Effective richness")
#curve(exp(predict(tmp, newdata=list(x=x))),
#      col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
#curve(exp(predict(tmp, newdata=list(x=x))),col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp1, newdata=list(x=x))),col="green", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp2, newdata=list(x=x))),col="gold", lwd=2, add=TRUE, from = 1, to = 10000)
curve(exp(predict(tmp3, newdata=list(x=x))),col="red", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp4, newdata=list(x=x))),col="cyan", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp5, newdata=list(x=x))),col="brown", lwd=2, add=TRUE, from = 1, to = 10000)
legend("topleft",expression(bold((d))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(log(y),predict(tmp3))^2
R2.1 <- 1 - with(dados, (sum((log(y)-predict(tmp3))^2)/sum((log(y)-mean(log(y)))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("bottomright",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.01,-0.03)
)
dev.off()


##################H
#### FIGURE S4 ####
##################H
jpeg(filename = "FigureS4.jpg", width = 3100*2, height = 3100, units = "px", pointsize = 12,
     res = 600, family = "sans", type="cairo", bg="white")
par(mfrow=c(1,2), las=1,mar=c(3,3.5,0.5,0.25),mgp=c(1.8,0.35,0),tcl=-.25)
#Species Richness (q=0)
dados = na.omit(grid.result[,c("WE.pure.correct2","WE.near.correct2","SampCover","lat","long","select")])
dados = dados[dados$select,]
dados = dados[dados[,1]>0&dados[,2]>0,]
#dados = dados[dados$SampCover>=cut,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover 
tmp0 = lm(log(y) ~ 1, data=dados)
tmp  = rlm(log(y) ~ log(x), data=dados)
tmp1 = lm(log(y) ~ log(x), data=dados, weights = w)
tmp2 = lm(log(y) ~ log(x), data=dados, weights = w^3)
tmp3 = gls(log(y) ~ log(x), data=dados, corr = corExp(c(3e+06), form= ~long + lat, nugget = FALSE), control = list(maxIter = 20, tolerance = 0.01))
# tmp4 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3)
#plot(S.total  ~ N.total, data=grid.result, log="xy",
#     xlab="Number of occurrences",ylab="Observed richness")
plot(y  ~ x, data=grid.result, log="xy", yaxt="n", xaxt="n",
     xlab="Near endemics",ylab="Pure endemics")
axis(2,at = c(0.02, 0.1, 0.5, 2, 5, 20, 100), labels = c("0.02","0.1","0.5","2","5","20","100"),
     las=1, las=1,mar=c(3,3.5,0.5,0.25),mgp=c(1.8,0.35,0),tcl=-.25)
axis(1,at = c(0.02, 0.1, 0.5, 2, 5, 20, 100), labels = c("0.02","0.1","0.5","2","5","20","100"),
     las=1, las=1,mar=c(3,3.5,0.5,0.25),mgp=c(1.8,0.35,0),tcl=-.25)
#curve(exp(predict(tmp, newdata=list(x=x))),
#      col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp, newdata=list(x=x))),col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp1, newdata=list(x=x))),col="green", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp2, newdata=list(x=x))),col="gold", lwd=2, add=TRUE, from = 1, to = 10000)
curve(exp(predict(tmp3, newdata=list(x=x))),col="red", lwd=2, add=TRUE, from = 0.015, to = 25)
# curve(exp(predict(tmp4, newdata=list(x=x))),col="cyan", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp5, newdata=list(x=x))),col="brown", lwd=2, add=TRUE, from = 1, to = 10000)
legend("topleft",expression(bold((a))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(log(y),predict(tmp3))^2
R2.1 <- 1 - with(dados, (sum((log(y)-predict(tmp3))^2)/sum((log(y)-mean(log(y)))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("topleft",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.05,0.1)
)
dados = na.omit(grid.result[,c("qS.100.pure.correct2","qS.100.near.correct2","SampCover","lat","long","select")])
dados = dados[dados$select,]
dados = dados[dados[,1]>0&dados[,2]>0,]
#dados = dados[dados$SampCover>=cut,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover 
tmp0 = lm(log(y+1) ~ 1, data=dados)
tmp  = rlm(log(y+1) ~ log(x+1), data=dados)
tmp1 = lm(log(y+1) ~ log(x+1), data=dados, weights = w)
tmp2 = lm(log(y+1) ~ log(x+1), data=dados, weights = w^3)
#tmp3 = gls(log(y+1) ~ log(x+1), data=dados, corr = corExp(form= ~long + lat, nugget = FALSE))
tmp3 = gls(log(y+1) ~ log(x+1), data=dados, corr = corExp(c(3e+06), form= ~long + lat, nugget = FALSE), control = list(maxIter = 20, tolerance = 0.01))
# tmp4 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(log(y) ~ log(x), data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3)
#plot(S.total  ~ N.total, data=grid.result, log="xy",
#     xlab="Number of occurrences",ylab="Observed richness")
plot(I(y+1) ~ I(x+1), data=grid.result, log="xy",
     xlab="Near endemics",ylab="Pure endemics")
#curve(exp(predict(tmp, newdata=list(x=x))),
#      col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp, newdata=list(x=x))),col="blue", lwd=2, add=TRUE, from = 1, to = 10000)
curve(exp(predict(tmp1, newdata=list(x=x))),col="red", lwd=2, add=TRUE, from = 5, to = 10000)
# curve(exp(predict(tmp2, newdata=list(x=x))),col="gold", lwd=2, add=TRUE, from = 1, to = 10000)
#curve(exp(predict(tmp3, newdata=list(x=x))),col="red", lwd=2, add=TRUE, from = 3, to = 10000)
# curve(exp(predict(tmp4, newdata=list(x=x))),col="cyan", lwd=2, add=TRUE, from = 1, to = 10000)
# curve(exp(predict(tmp5, newdata=list(x=x))),col="brown", lwd=2, add=TRUE, from = 1, to = 10000)
legend("topleft",expression(bold((b))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(log(y),predict(tmp3))^2
R2.1 <- 1 - with(dados, (sum((log(y)-predict(tmp3))^2)/sum((log(y)-mean(log(y)))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("topleft",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.05,0.1)
)
dev.off()

#################H
#### FIGURE 2 ####
#################H

jpeg(filename = "Figure2.jpg", width = 3100*1.5, height = 3000*1.5, units = "px", pointsize = 12,
     res = 600, family = "sans", type="cairo", bg="white")
par(mfrow=c(2,2), las=1,mar=c(3,3,0.5,0.25),mgp=c(1.7,0.4,0),tcl=-.3)
#Pure
dados = na.omit(grid.result[,c("qS.100.pure.correct","qS.100.total.correct","SampCover","N.pure","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover
#x1 = x - min(x)
tmp0 = lm(y ~ 1, data=dados)
tmp  = rlm(y ~ x, data=dados)
tmp1 = lm(y ~ x, data=dados, weights = w)
tmp2 = lm(y ~ x, data=dados, weights = w^3)
#tmp3 = gls(y ~ x, data=dados, corr = corSpatial(value=1000000, form= ~long + lat, nugget = TRUE))
tmp3 = gls(y ~ x, data=dados, corr = corSpatial(form= ~long + lat, nugget = TRUE), control = list(maxIter = 50, tolerance = 0.01))
#tmp4 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = TRUE))
#tmp5 = gls(y ~ x, data=dados, corr = corSpatial(value=1000000, form= ~long + lat, nugget = 1))
# tmp4 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3)
# plot(grid.result$D.pure.correct ~ grid.result$D.total.correct, log="xy", 
#      xlab= "All species", ylab= "Pure endemics", xlim=c(1.3,600), ylim=c(1.3,600))
plot(y ~ x, pch=20, col="grey",yaxt="n",xaxt="n",
     xlab= "All species", ylab= "Pure endemics", xlim=c(20,100), ylim=c(3,92))
abline(0,1, lty=2)
axis(2,at=seq(20,80,20),labels=TRUE)
axis(1,at=seq(20,100,20),labels=FALSE)
#curve(exp(predict(tmp1, newdata=list(x=x))), col="blue", lwd=2, add=TRUE, from = 27, to = 750)
#curve(nlme:::predict.gls(tmp3, newdata=list(x=x)), col="black", lwd=2, add=TRUE, from = 19, to = 105)
#curve(coef(tmp3)[1] + coef(tmp3)[2]*x, col="black", lwd=2, add=TRUE, from = 19, to = 105)
#curve(exp(nlme:::predict.gls(tmp1, newdata=list(x=x))), col="red", lwd=2, add=TRUE, from = 19, to = 105)
#lines(x, exp(predict(tmp1, newdata=list(x=x))) ,col="red", lwd=2)
#curve(exp(predict(tmp2,newdata=data.frame(x=x))),add=T,col="green", lwd=2)
legend("topleft",expression(bold((a))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(y,predict(tmp3))^2
#R2.1 <- 1 - with(dados, (sum((y-predict(tmp3))^2)/sum((y-mean(y))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
slp = confint(tmp3)[2,]
legend("topleft",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%"),
  bquote({slope}~"="~.(round(coef(tmp3)[2],2)))
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.05,0.1)
)
#Near
dados = na.omit(grid.result[,c("qS.100.near.correct","qS.100.total.correct","SampCover","N.near","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover
tmp0 = lm(y ~ 1, data=dados)
tmp  = rlm(y ~ x, data=dados)
tmp1 = lm(y ~ x, data=dados, weights = w)
tmp2 = lm(y ~ x, data=dados, weights = w^3)
#tmp3 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = FALSE))
#tmp3 = gls(y ~ x, data=dados, corr = corSpatial(value=1000000, form= ~long + lat, nugget = TRUE))
tmp3 = gls(y ~ x, data=dados, corr = corSpatial(form= ~long + lat, nugget = TRUE), control = list(maxIter = 50, tolerance = 0.01))
# tmp4 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3)
#plot(grid.result$D.near.correct ~ grid.result$D.total.correct, log="xy", 
#     xlab= "All species", ylab= "Near endemics", xlim=c(1.3,600), ylim=c(1.3,600))
plot(y ~ x, pch=20, col="grey",yaxt="n",xaxt="n",
     xlab= "All species", ylab= "Near endemics", xlim=c(20,100), ylim=c(3,92))
abline(0,1, lty=2)
axis(2,at=seq(20,80,20),labels=FALSE)
axis(1,at=seq(20,100,20),labels=FALSE)
#curve(exp(predict(tmp, newdata=list(x=x))), col="blue", lwd=2, add=TRUE, from = 5, to = 750)
#curve(exp(predict(tmp3, newdata=list(x=x))),col="black", lwd=2, add=TRUE, from = 19, to = 105)
#curve(predict(tmp3, newdata=list(x=x)),col="black", lwd=2, add=TRUE, from = 19, to = 105)
#curve(predict(tmp1, newdata=list(x=x)),col="red", lwd=2, add=TRUE, from = 19, to = 105)
#lines(x, exp(predict(tmp, newdata=list(x=x))) ,col="red", lwd=2)
#lines(x, exp(predict(tmp1, newdata=list(x=x))) ,col="green", lwd=2)
#curve(exp(predict(tmp2,newdata=data.frame(x=x))),add=T,col="blue", lwd=2)
legend("topleft",expression(bold((b))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(y,predict(tmp3))^2
#R2.1 <- 1 - with(dados, (sum((y-predict(tmp3))^2)/sum((y-mean(y))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("topleft",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%"),
  bquote({slope}~"="~.(round(coef(tmp3)[2],2)))
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.05,0.1)
)
#All endemics
dados = na.omit(grid.result[,c("qS.100.pure.near.correct","qS.100.total.correct","SampCover","N.pure.near","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover
tmp0 = lm(y ~ 1, data=dados)
tmp  = rlm(y ~ x, data=dados)
tmp1 = lm(y ~ x, data=dados, weights = w)
tmp2 = lm(y ~ x, data=dados, weights = w^3)
#tmp3 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = FALSE))
#tmp3 = gls(y ~ x, data=dados, corr = corSpatial(value=1000000, form= ~long + lat, nugget = TRUE))
tmp3 = gls(y ~ x, data=dados, corr = corSpatial(form= ~long + lat, nugget = FALSE), control = list(maxIter = 50, tolerance = 0.01))
# tmp4 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3,tmp4,tmp5)
#plot(grid.result$D.pure.near.correct ~ grid.result$D.total.correct, log="xy", 
#     xlab= "All species", ylab= "All endemics", xlim=c(1.3,600), ylim=c(1.3,600))
plot(y ~ x, pch=20, col="grey",
     xlab= "All species", ylab= "All endemics", xlim=c(20,100), ylim=c(3,92))
abline(0,1, lty=2)
#curve(exp(predict(tmp, newdata=list(x=x))),col="blue", lwd=2, add=TRUE, from = 7, to = 750)
#curve(exp(predict(tmp1, newdata=list(x=x))), col="red", lwd=2, add=TRUE, from = 19, to = 105)
#curve(exp(predict(tmp3, newdata=list(x=x))), col="black", lwd=2, add=TRUE, from = 19, to = 105)
#lines(x, exp(predict(tmp, newdata=list(x=x))) ,col="red", lwd=2)
#lines(x, exp(predict(tmp1, newdata=list(x=x))) ,col="green", lwd=2)
#curve(exp(predict(tmp2,newdata=data.frame(x=x))),add=T,col="blue", lwd=2)
legend("topleft",expression(bold((c))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(y,predict(tmp3))^2
#R2.1 <- 1 - with(dados, (sum((y-predict(tmp3))^2)/sum((y-mean(y))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("topleft",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%"),
  bquote({slope}~"="~.(round(coef(tmp3)[2],2)))
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.05,0.1)
)
#Ocasional species
dados = na.omit(grid.result[,c("qS.100.ocas.correct","qS.100.total.correct","SampCover","N.ocas","lat","long","select")])
dados = dados[dados$select,]
#dados = dados[dados$SampCover>=cut,]
x = dados[,2]; y = dados[,1]; w = dados$SampCover
y[y>x] = x[y>x]
tmp0 = lm(y ~ 1, data=dados)
tmp  = rlm(y ~ x, data=dados)
tmp1 = lm(y ~ x, data=dados, weights = w)
tmp2 = lm(y ~ x, data=dados, weights = w^3)
#tmp3 = gls(y ~ x, data=dados, corr = corExp(c(139357),form= ~long + lat, nugget = FALSE))
#tmp3 = gls(y ~ x, data=dados, corr = corSpatial(value=1000000, form= ~long + lat, nugget = TRUE))
#tmp3 = gls(y ~ x, data=dados, corr = corExp(c(3e+06), form= ~long + lat, nugget = FALSE), control = list(maxIter = 20, tolerance = 0.01))
tmp3 = gls(y ~ x, data=dados, corr = corSpatial(form= ~long + lat, nugget = TRUE), control = list(maxIter = 50, tolerance = 0.01))
# tmp4 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w)
# tmp5 = gls(y ~ x, data=dados, corr = corExp(form= ~long + lat, nugget = TRUE), weights = ~w^3)
AICtab(tmp0,tmp,tmp1,tmp2,tmp3,tmp4,tmp5)
# plot(grid.result$D.ocas.correct ~ grid.result$D.total.correct, log="xy", 
#      xlab= "All species", ylab= "Ocasional species", xlim=c(1.3,600), ylim=c(1.3,600))
plot(y ~ x, pch=20, col="grey", yaxt="n",
     xlab= "All species", ylab= "Ocasional species", xlim=c(20,100), ylim=c(3,92))
abline(0,1, lty=2)
axis(2,at=seq(20,80,20),labels=FALSE)
#curve(exp(predict(tmp, newdata=list(x=x))), col="blue", lwd=2, add=TRUE, from = 1.5, to = 750)
#curve(exp(predict(tmp1, newdata=list(x=x))), col="red", lwd=2, add=TRUE, from = 19, to = 105)
#curve(exp(predict(tmp3, newdata=list(x=x))), col="black", lwd=2, add=TRUE, from = 19, to = 105)
#lines(x, exp(predict(tmp, newdata=list(x=x))) ,col="red", lwd=2)
#lines(x, exp(predict(tmp1, newdata=list(x=x))) ,col="green", lwd=2)
#curve(exp(predict(tmp2,newdata=data.frame(x=x))),add=T,col="blue", lwd=2)
legend("topleft",expression(bold((d))), cex=1.5, bty="n",inset=c(-0.09,-0.03))
R2 <- cor(y,predict(tmp3))^2
#R2.1 <- 1 - with(dados, (sum((y-predict(tmp3))^2)/sum((y-mean(y))^2)))
R2.n <- 1-(as.numeric(logLik(tmp3)/logLik(tmp0)))
df = tmp3$dims$N - tmp3$dims$p
legend("topleft",legend=do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp3)$F[2],1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%"),
  bquote({slope}~"="~.(round(coef(tmp3)[2],2)))
)),
bty="n",horiz=F,cex=1,adj=c(0,0),inset = c(-0.05,0.1)
)
dev.off()



##################################################H
#### CENTRES OF ENDEMIC AND OCCASIONAL SPECIES ####
##################################################H

#require(colorRamps)
cores = c("lightyellow","yellow","gold","orange","red","darkred") 
cores1 = c("floralwhite","lightyellow","yellow","gold","orange","red","darkred") 
cores2 = c("lightgreen","greenyellow","yellow","gold","orange","red","darkred") 
#cores3 = matlab.like(7)[-c(1)]
colfunc<-colorRampPalette(cores1)
grid.result$S.end.col = as.character(cut(grid.result$S.pure.near.correct, 
                                         breaks = quantile(grid.result$S.pure.near.correct, na.rm=TRUE, prob = seq(0,1,0.05)) + seq(0,0.02,0.001), 
                                         labels = colfunc(20), include.lowest = TRUE))
grid.result$S.ocas.col = as.character(cut(grid.result$S.ocas.correct,
                                          breaks = quantile(grid.result$S.ocas.correct, na.rm=TRUE, prob = seq(0,1,0.05)) + seq(0,0.02,0.001),
                                          labels = colfunc(20), include.lowest = TRUE))
grid.result$D.end.col = as.character(cut(grid.result$D.pure.near.correct,
                                         breaks = quantile(grid.result$D.pure.near.correct, na.rm=TRUE, prob = seq(0,1,0.05)) + seq(0,0.02,0.001),
                                         labels = colfunc(20), include.lowest = TRUE))
grid.result$D.ocas.col = as.character(cut(grid.result$D.ocas.correct,
                                          breaks = quantile(grid.result$D.ocas.correct, na.rm=TRUE, prob = seq(0,1,0.05)) + seq(0,0.02,0.001),
                                          labels = colfunc(20), include.lowest = TRUE))


par(mfrow=c(1,2))
plot(grid1,border=grey(0.9)) ## ENDEMIC SPECIES RICHNESS (SR)
legend("bottomright", "Pure + near endemics",bty='n',cex=2)
plot(grid1[!is.na(grid.result$S.pure.near)],
     col=grid.result$S.end.col[!is.na(grid.result$S.pure.near)],
     border=grid.result$S.end.col[!is.na(grid.result$S.pure.near)],
     #border=grey(0.9),
     add=TRUE)
plot(af.proj, add=TRUE)
plot(grid1,border=grey(0.9)) ## OCASIONAL SPECIES RICHNESS (SR)
legend("bottomright", "Ocasional",bty='n',cex=2)
plot(grid1[!is.na(grid.result$S.ocas)],
     col=grid.result$S.ocas.col[!is.na(grid.result$S.ocas)],
     border=grid.result$S.ocas.col[!is.na(grid.result$S.ocas)],
     #border=grey(0.9),
     add=TRUE)
plot(af.proj, add=TRUE)

plot(grid1,border=grey(0.9)) ## ENDEMIC EFFECTIVE RICHNESS (S2)
legend("bottomright", "Pure + near endemics",bty='n',cex=2)
plot(grid1[!is.na(grid.result$D.pure.near)],
     col=grid.result$D.end.col[!is.na(grid.result$D.pure.near)],
     border=grid.result$D.end.col[!is.na(grid.result$D.pure.near)],
     #border=grey(0.9),
     add=TRUE)
plot(af.proj, add=TRUE)
plot(grid1,border=grey(0.9)) ## OCASIONAL EFFECTIVE RICHNESS (S2)
legend("bottomright", "Ocasional",bty='n',cex=2)
plot(grid1[!is.na(grid.result$D.ocas)],
     col=grid.result$D.ocas.col[!is.na(grid.result$D.ocas)],
     border=grid.result$D.ocas.col[!is.na(grid.result$D.ocas)],
     #border=grey(0.9),
     add=TRUE)
plot(af.proj, add=TRUE)


#### DELIMITING CENTER OF ENDEMIC AND OCASIONAL SPECIES DIVERSITY ####

## Creating the grid used for prediction
# Define the grid extent
bb = bbox(grid1)
x.range <- as.numeric(c(bb[1,1],bb[1,2]))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(bb[2,1],bb[2,2]))  # min/max latitude of the interpolation area
rec_grid <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 5000), 
                        y = seq(from = y.range[1], to = y.range[2], by = 5000))  # expand points to grid
coordinates(rec_grid) <- ~x + y
#gridded(rec_grid) <- TRUE
proj4string(rec_grid) <- crs(grid1)
rec_grid = as(rec_grid, "SpatialPoints")

#Selecting the grid cells intersecting the map
tmp = over(rec_grid, grid1)
rec_grid1 = rec_grid[!is.na(tmp),]
#rec_grid1 = rec_grid[gIntersects(rec_grid, grid1, byid = TRUE),]

#Creating the grid for interpolation
x.out = as.double(coordinates(rec_grid1)[,1])
y.out = as.double(coordinates(rec_grid1)[,2])
grid.out = cbind.data.frame(x.out, y.out)
names(grid.out)[1:2] = c("x","y")
coordinates(grid.out) = ~x+y
proj4string(grid.out) <- crs(grid1)
#gridded(grid.out) <- TRUE

##Preparing data for interpolation
grid2 = grid1[!is.na(grid.result$select) & grid.result$select,]
x.data = as.double(coordinates(grid2)[,1])
y.data = as.double(coordinates(grid2)[,2])
dados = grid.result[!is.na(grid.result$select) & grid.result$select,]

grid2 = cbind.data.frame(x.data,y.data,dados)
names(grid2)[1:2] = c("x","y")
coordinates(grid2) = ~x+y
proj4string(grid2) <- crs(grid1)

cores = c("lightyellow","yellow","gold","orange","red","darkred") 
cores1 = c("floralwhite","lightyellow","yellow","gold","orange","red","darkred") 
cores2 = c("lightgreen","greenyellow","yellow","gold","orange","red","darkred") 
cores3 = c("white","lightgrey","grey","darkgrey","black") 
colfunc<-colorRampPalette(cores1)
cor.af = "darkgreen" # "black" #
cor.br = "grey"
cor.am = "black" 
cutoff = "80%"
#vars = c("N.total","qS.100.pure.correct2","qS.100.pure.near.correct2","qS.100.ocas.correct2")
#vars = c("qS.100.near.correct2")
vars = c("qS.100.pure.correct2","qS.100.near.correct2","qS.100.pure.near.correct2","qS.100.ocas.correct2",
          "S.pure.correct2","S.near.correct2","S.pure.near.correct2","S.ocas.correct2",
          "CWE.pure2","CWE.near2","CWE.pure.near2","CWE.ocas2",
          "D.pure.correct2","D.near.correct2","D.pure.near.correct2","D.ocas.correct2"
          )
#nomes = c("A - Number of occurrences","B - Pure endemics","C - All endemics","D - Ocasional species")
#nomes = "B - Near endemics"
nomes = rep(c("A - Pure endemics","B - Near endemics","C - All endemics","D - Ocasional species"),4)
am.lat.proj1 = crop(am.lat.proj,bb)
brasil.proj1 = crop(brasil.proj,bb)

for(i in 1:length(vars)) {
  var = vars[i]
  nome.fig = paste("Figure2_",var,".jpg",sep="")
  #nome.fig = paste("Figure2_",var,"_BW.jpg",sep="")
  nome.shp = gsub("\\.","_",paste("shp_centre_divers_",var,sep=""))

  if (var == "N.total") {
    dados1 = grid.result[!is.na(grid.result[,var]),] 
    dados2 = log(dados1[,var]+1)
    
    grid2.1 = grid1
    x.data.1 = as.double(coordinates(grid2.1)[,1])
    y.data.1 = as.double(coordinates(grid2.1)[,2])
    grid2.1 = cbind.data.frame(x.data.1, y.data.1, grid.result)
    names(grid2.1)[1:2] = c("x","y")
    coordinates(grid2.1) = ~x+y
    proj4string(grid2.1) <- crs(grid1)
    grid3  = grid2.1[!is.na(grid2.1@data[,var]),]
  } else { 
    dados1 = dados[!is.na(dados[,var]),]
    if(grepl("CWE",var)) {
      dados2 = log(dados1[,var])
      infs = min(dados2[!is.infinite(dados2)]) - 0.05
      dados2[is.infinite(dados2)] = infs
    } else {
      dados2 = log(dados1[,var]+1)
    }
    grid3  = grid2[!is.na(grid2@data[,var]),]
  }
  #Inverse distance weighting
  #idw <- idw(dados2 ~ 1, locations= grid3, newdata= grid.out, idp =1, nmax=30)
  #idw.output = cbind.data.frame(coordinates(grid.out), idw$var1.pred)
  #names(idw.output)<-c("x","y","z")
  #par(mfrow=c(1,1), mar=c(2,2,0,0))
  # plot(af.proj, border="white")
  # image(idw.output, col = colfunc(21), add=TRUE,  breaks = c(0,quantile(dados2, na.rm=TRUE, prob = seq(0,1,0.05)) + seq(0,0.02,0.001)))
  # plot(af.proj, add=TRUE, border="grey")
  # idw.output1 = xtabs(z~x+y, idw.output)
  # cl <- contourLines(as.double(attributes(idw.output1)$dimnames$x),
  #             as.double(attributes(idw.output1)$dimnames$y),
  #             idw.output1, levels = quantile(dados2, prob= cutoff), nlevels=1)
  # shp <- ContourLines2SLDF(cl, proj4string=crs(grid1))
  # shp.idw = disaggregate(shp)
  # shp.idw1= shp.idw[gLength(shp.idw, byid = TRUE)>=mean(gLength(shp.idw, byid = TRUE)),]
  #plot(shp.idw, add=TRUE, lwd=2)
  
  ## Kriging
  # Fitting the semivariogram
  semivariog<-variogram(dados2 ~ 1, locations= grid3, data= grid3)

  #using fit.variogram
  rm(fvl, fve, fvs, fvg)
  #the data looks like it might be an exponential shape, so we will try that first with the values estimated from the empirical 
  fvl <- fit.variogram(semivariog, vgm(1, "Lin", 600000, 10))
  plot(semivariog, fvl)
  fve <- fit.variogram(semivariog, vgm(1, "Exp", 600000, 10000))
  plot(semivariog, fve)
  fvs <- fit.variogram(semivariog, vgm(1, "Sph", 600000, 10000))
  plot(semivariog, fvs)
  fvg <- fit.variogram(semivariog, vgm(1, "Gau", 600000, 10))
  plot(semivariog, fvg)
  #fvm <- fit.variogram(semivariog, vgm(1, "Mat", 600000, 10))
  #plot(semivariog, fvm)

  # Chosing the best model with the best fit for kriging
  SSErrs = c(fve = attr(fve, "SSErr"), 
		   fvs  = attr(fvs, "SSErr"), 
		   fvl  = attr(fvg, "SSErr"),
		   fvg  = attr(fvg, "SSErr"))
		   #fvm  = attr(fvm, "SSErr"))
  tmp = list(fve = fve, fvs = fvs, fvl = fvl, fvg = fvg)
  best.m = tmp[[head(names(SSErrs)[which(SSErrs == min(SSErrs))],1)]]

  # Kriging
  krig<- gstat::krige(formula= dados2 ~ 1, locations= grid3, newdata= grid.out, model= best.m, nmax = 25)
  #krig1<-krige(formula= dados2 ~ x+y, locations= grid3, newdata= grid.out, model= fit.variog.exp, nmax = 20)
  krig.output = cbind.data.frame(coordinates(grid.out), krig$var1.pred)
  names(krig.output)<-c("x","y","z")
  
  jpeg(filename = nome.fig, width = 3000*1.5, height = 3000*2, units = "px", pointsize = 12,
       res = 600, family = "sans", type="cairo", bg="white")
  
  #Definig the breaks for colors 
  qts = quantile(dados2, na.rm=TRUE, prob = seq(0,1,0.05))
  zeros = sum(qts==0)
  if(grepl("CWE", var)) zeros = sum(qts==infs)
  cres = c(rep("#FFFFFF",zeros), colfunc(20-zeros))
  brks = qts
  if(zeros>0) {
    brks[brks==0] = seq(0,(zeros-1)*0.00001,0.00001)
    if(grepl("CWE", var)) brks[brks==infs] = seq(infs,infs+(zeros-1)*0.00001,0.00001)
  }
  #ploting
  par(mfrow=c(1,1), las=1,mar=c(0,0,0,0),mgp=c(1.7,0.4,0),tcl=-.3)
  plot(af.proj, border="white")
  image(krig.output, col = cres, add=TRUE,  breaks = brks)
  krig.output1 = xtabs(z~x+y, krig.output)
  cl <- contourLines(as.double(attributes(krig.output1)$dimnames$x),
                     as.double(attributes(krig.output1)$dimnames$y),
                     krig.output1, levels = qts[which(names(qts) %in% c("75%","80%","85%","90%","95%"))])
  shp <- ContourLines2SLDF(cl, proj4string=crs(grid1))
  shp@data$quantile = head(c("75%","80%","85%","90%","95%"), length(shp)) 
  if(grepl("CWE",var)) {
    shp@data[,var] = NA
    shp@data[,var] = exp(as.double(as.character(shp@data$level)))
  } else {
    shp@data[,var] = NA
    shp@data[,var] = exp(as.double(as.character(shp@data$level))) - 1
  }  
  shp.krig.cut = disaggregate(shp[which(shp@data$level == qts[which(names(qts)==cutoff)]),])
  #shp.krig.cut1 = shp.krig.cut[gLength(shp.krig.cut, byid = TRUE) >= mean(gLength(shp.krig.cut, byid = TRUE)),]
  shp.krig.cut1 = shp.krig.cut[gLength(shp.krig.cut, byid = TRUE) >=2*pi*25000,]
  plot(brasil.proj1, add=TRUE, lty="1414", border= cor.br)
  plot(af.proj, add=TRUE, border= cor.af)
  plot(am.lat.proj1, add=TRUE, border = cor.am)
  nome = nomes[i]
  legend("topleft",nome, cex=1.5, bty="n",inset=c(0.02,0.02))
  plot(shp.krig.cut1, add=TRUE, lwd=2, col=1)
  par(fig = c(0.5, 0.85, 0.015, 0.4), mar=c(0,0,0,0), new=TRUE)
  image.plot( legend.only=TRUE, col = cres, add=TRUE,
    legend.shrink = 1, legend.width = 4, legend.mar = 1.2, 
    legend.line = 1, legend.cex = 0.5,
    zlim=c(2.5,97.5),
    #breaks = seq(0,100,20),	 
    axis.args=list(at=seq(0,100,20),  mgp=c(1.5,0.6,0), tcl=-.3, cex.axis=1, labels=paste0(seq(0,100,20),"%")))
  legend(4977239,6024198,expression(bold("Quantiles")),cex=1.1,bty="n",y.intersp=-0.01)
  #colorbar.plot(5549396, 7757332, strip=cbind(0:100), horizontal=F, 
  #	    strip.width = 0.04, strip.length = 0.6,
  #      col=cres)
  #axis(2, at=seq(0,100,20), labels = TRUE, las=1)
  #plot(shp.idw1, add=TRUE, lwd=2, col=6)
  #plot(shp.krig1, add=TRUE, lwd=2, col=1)

  #Saving the shapefile
  shp@data = shp@data[,2:3]
  #writeOGR(obj=shp, dsn='.', layer=nome.shp, driver="ESRI Shapefile", overwrite_layer=TRUE)

  ## Piecewise linear interpolation
  # IL = interp(grid3, z= var, xo=x.data, yo=y.data,  
  #             nx=100, ny= 100)
  #plot(af.proj, border="white")
  # image(IL, add=TRUE)
  # x = coordinates(IL)[,1]; y = coordinates(IL)[,2]; z = IL@data[,var]
  # obj = xtabs(z ~ x + y)
  # cl <- contourLines(as.double(attributes(obj)$dimnames$x),
  #                    as.double(attributes(obj)$dimnames$y),
  #                    obj, levels = quantile(log(dados+1), prob= cutoff), nlevels=1)
  # shp <- ContourLines2SLDF(cl, proj4string=crs(grid1))
  # shp.lin = disaggregate(shp)
  # shp.lin1 = shp.lin[gLength(shp.lin, byid = TRUE)>mean(gLength(shp.lin, byid = TRUE)),]
  # plot(shp.lin1, add=TRUE, lwd=2, col=5)
  
  ## Bilinear interpolation
  #obj = xtabs(dados1~x.data + y.data)
  #obj = list(as.double(attributes(obj)$dimnames$x.data),
  #           as.double(attributes(obj)$dimnames$y.data),
  #           obj)
  #interp.surface.grid(obj, coordinates(grid.out))
  dev.off()
}

#### END ####


#### EXPORTING THE SPECIES OCCURRENCE DATA TO DO THE ENDEMICITY ANALYSIS ####
## Create a csv with 3 columns, separated by semi-colons
tmp = cbind.data.frame(Species = oc.data.af.spp.geo.tax1$species.correct2,
                       Latitude = oc.data.af.spp.geo.tax1$latitude.work1,
                       Longitude = oc.data.af.spp.geo.tax1$longitude.work1)

## Removing occurrences for the same species with the exact same coordinates
tmp$check = paste(tmp$Species,tmp$Latitude,tmp$Longitude,sep="_")
tmp = tmp[!duplicated(tmp$check),1:3]

## Removing occurrences without coordinates
tmp = tmp[!is.na(tmp$Latitude),]
tmp = tmp[!is.na(tmp$Longitude),]

## Putting in the NDM format

#Headers
#'autogrid' can be replaced by arguments: fill 5 5, assume 25 25, rows 12, cols 12 
tmp = tmp[order(tmp$Species),] #ordering by species names
spp = unique(tmp$Species)
tmp1 = paste("automatrix","autogrid","nocommas","longlat",paste("spp",length(spp),sep=" "),"xydata", sep="\n")

#Species coordinates
nomes =  
  
  
  
  ## Saving
  write.table(tmp,file='oc_data_to_NDM.csv',row.names = FALSE, quote = FALSE, sep=";")
