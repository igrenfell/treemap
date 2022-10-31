setwd("G:\\Workspace\\treemap\\treemap2014_rasters2016\\z05_test")

allplot.2016 <- read.table("x_table_TreeMap2016_final.csv", header = TRUE, sep = ",")



library(yaImpute)
library(raster)
library(rgdal)
library(foreign)
library(parallel)
library(foreach)
library(doParallel)




###Change this!
cur.zone <- "z7"
cur.zone.zero <- "z07"
outfolder <- cur.zone
ncores <- 24
setwd("G:\\Workspace\\treemap\\treemap2014_rasters2016\\target_data_reclassified_final")
setwd(cur.zone.zero)

flist.tif <- Sys.glob("*.tif")


raster.stack <- stack(flist.tif)
p4s.albers <- proj4string(raster.stack)
raster.list <- vector("list", length(flist.tif))
nrasters <- length(flist.tif)
for(i in 1:length(flist.tif))  
{
  raster.list[[i]] <- raster()  
}

setwd("G:\\Workspace\\treemap\\treelist_2016\\EVG_remap")


meters.db <- read.table("metersdb.txt")


allplot.2014 <- read.table("x_table_TreeMap2014_final_EVG_Karin_reclass_plus_loblolly_manual.txt", header = TRUE, sep = ",")
allplot.2016 <- read.table("x_table_TreeMap2016_final.csv", header = TRUE, sep = ",")


allplot <- allplot.2014
#allplot <- allplot.2016

setwd("G:\\Workspace\\treemap\\treemap2014_rasters2016\\EVG_remap-2014\\EVG_remap")

fin <- paste(cur.zone.zero, "_EVG_remap.txt", sep = "")

remap <- read.table(fin , sep=":")

#Limit allplot to just the veg types in the remap table
plot.df <- allplot[allplot$EVT_GP %in% remap$V1,]
dim(plot.df)

###Change this!
#dir.create(paste("F:\\Tree_List_c2014\\outputs\\", cur.zone, "_disturb", sep=""))
plot.df$CN <- factor(plot.df$CN)
#write.csv(plot.df, paste("F:\\Tree_List_c2014\\outputs\\", cur.zone, "_disturb\\", cur.zone, "_x_table_allplots_reclass.txt", sep=""), row.names = F)
##plot.df$CN <- as.numeric(plot.df$CN)
# Karin commented out above because it was changing CN)

merge.df <- merge(plot.df, meters.db, by = "CN")

##Build X predictor matrix
evg.fac <- as.factor(plot.df$EVT_GP)
dc.code.fac <- as.factor(plot.df$disturb_code)
dc.year.fac <- as.factor(plot.df$disturb_year)
dc.year.num <- as.numeric(plot.df$disturb_year)


lev.dc <- levels(dc.code.fac)
lev.year <- levels(dc.year.fac)

plot.df[,18] <- evg.fac


##Build Y response matrix
plot.df$POINT_X <- merge.df$POINT_X
plot.df$POINT_Y <- merge.df$POINT_Y



####Reclass evgs
#evg.reclass <- read.table(paste(cur.zone.zero, "_EVG_remap.txt", sep=""), sep=":")
evg.reclass <- remap
n.evgs <- dim(remap )[1]

evg.out <- rep(0, dim(plot.df)[1])
evg.vec <- plot.df$"EVT_GP"
for(i in 1:n.evgs)  
{  
  cur.evg <- evg.reclass[i, 1]  
  sub.ind <- evg.vec == cur.evg  
  evg.out[sub.ind] <- i  
}	
evg.in <- as.factor(evg.out)

plot.df$"EVT_GP" <- as.factor(evg.out)
plot.df$disturb_code <- as.factor(plot.df$disturb_code)


#Create X Table
X.df <- plot.df[,5:20]

aspect.temp <- X.df$ASPECT
rad.temp <- (pi/180)*aspect.temp
northing.temp <- cos(rad.temp)
easting.temp <- sin(rad.temp)
X.df <- X.df[,-2]
X.df$NORTHING <- northing.temp
X.df$EASTING <- 	easting.temp

rownames(X.df) <- plot.df$ID
id.table <-  plot.df$ID
Y.df <- data.frame(plot.df[,16:18])
rownames(Y.df) <- plot.df$ID
#X.df <- X.df[,-c(9, 10)]

# build the random forests model (X=all predictors, Y=EVG, EVC, EVH)
set.seed(56789)

#yai.treelist <- yai(X.df, Y.df, method = "randomForest", ntree = 249)

##Recode Distrubrnce as 0/1, add to Y matrix

dc.bin <- as.character(X.df$disturb_code)
dc.bin[dc.bin !="0"] <- "1"

dc.bin <- as.factor(dc.bin)
X.df.orig <- X.df 
Y.df.orig <- Y.df

X.df$disturb_code <- dc.bin
Y.df$disturb_code <- dc.bin
yai.treelist.bin <- yai(X.df, Y.df, method = "randomForest", ntree = 400)

# build dataframes from the raster data
raster.coords <- coordinates(raster.stack)
asp.raster <- raster.stack[[1]]
dem.raster <- raster.stack[[2]]

currow.vals <- cellFromRow(dem.raster, 1500)
coords.currow <- raster.coords[currow.vals,]

extract.currow <- extract(raster.stack, coords.currow)
p4s.latlong <- CRS("+proj=longlat +datum=NAD83") 

maxrow <- max(as.numeric(rownames(X.df)))

nrows.out <- dim(raster.stack)[1]
ncols.out <- dim(raster.stack)[2]

rs2 <- raster.stack


impute.row <- function(currow)  
{  
  library(yaImpute) 
  library(raster) 
  library(rgdal)
  currow.vals <- cellFromRow(dem.raster, currow)
  coords.currow <- raster.coords[currow.vals,]  
  
  # get data from each row of rasters (coordinates)
  sp.currow <- SpatialPoints(coords.currow, CRS(p4s.albers)) 
  extract.currow <- extract(rs2,   sp.currow)
  
  colseq <- 1:length(extract.currow[,1])
  valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))]
  # tempname <- paste(cur.zone,"slp_1_2", sep =   "")
  #colseq <- 1:length(currow.vals)
  #valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))]
  ncols.df <- dim(extract.currow)[2]
  # invalid.cols <- colseq[as.logical(is.na(extract.currow[,1]))]
  #extract.currow[invalid.cols,] <- rep(1, ncols.df)
  extract.currow <- data.frame(extract.currow)
  extract.currow$"POINT_X" <- sp.currow$x
  extract.currow$"POINT_Y" <-sp.currow$y
  extract.currow <- na.exclude(extract.currow)
  X.df.temp <- data.frame(extract.currow)
  nrow.temp <- dim(X.df.temp)[1]
  
  aspect.temp <- X.df.temp$ASPECT  
  rad.temp <- (pi/180)*aspect.temp  
  northing.temp <- cos(rad.temp)  
  easting.temp <- sin(rad.temp)
  
  X.df.temp <- X.df.temp[,-1]  
  X.df.temp$NORTHING <- northing.temp  
  X.df.temp$EASTING <- 	easting.temp  
  temp.evg <- X.df.temp$'EVT_GP'
  
  #get nonappearing evgs   
  evg.orig <- 1:n.evgs 
  #evg.orig <- as.numeric(levels(evg.in))
  evg.val <- evg.orig  
  evg.val.temp <- X.df.temp$'EVT_GP'  
  n.evgs.orig <- length(sort(unique(evg.orig)))  
  evg.orig.seq <- 1:n.evgs.orig  
  
  nonappearing.evgs <- evg.val[-sort(unique(as.numeric(as.character(evg.val.temp))))]  
  n.dummy.rows <- length(nonappearing.evgs)  
  X.df.temp.old <- X.df.temp
  
  if(n.dummy.rows > 0)    
  {    
    dummy.rows <- X.df.temp[1:n.dummy.rows,]    
    tempchar <- as.character(X.df.temp$'EVT_GP')    
    X.df.temp$'EVT_GP' <- tempchar    
    dummy.rows$'EVT_GP' <- as.character(nonappearing.evgs)    
    X.df.temp <- rbind(X.df.temp, dummy.rows)    
  }
  
  n.rows.orig <- dim(extract.currow)[1]	  
  temp.fac <- factor(X.df.temp$'EVT_GP', levels = levels(evg.in))  
  dc.code.fac.temp <- factor( X.df.temp$disturb_code, levels=lev.dc)  
  #dc.year.fac.temp <- factor( X.df.temp$disturb_year, levels=lev.year)  
  
  X.df.temp$'EVT_GP' <- as.factor(temp.fac)  
  X.df.temp$disturb_code <- dc.code.fac.temp   
  #X.df.temp$disturb_year <- dc.year.fac   
  nrow.temp <- dim(X.df.temp)[1]  
  impute.out <- rep(-1, nrow.temp)  
  #X.df.temp <- X.df.temp[,-c(12, 13)]  
  
  nc.orig <- dim(coords.currow)[1]  
  impute.out <- rep(NA,nc.orig)  
  nrows.orig <- dim(extract.currow)[1]  
  if(nrow.temp > 0)    
  {    
    colseq.out <- 1:dim(X.df.temp)[1]    
    rownames.all <- colseq.out+maxrow    
    rownames(X.df.temp) <- paste("T-", rownames.all)    
    #rownames(X.df.temp) <- rownames.all[valid.cols]    
    #names(X.df.temp) <- names(X.df)    
    #temp.yai <- impute(yai.treelist, ancilliaryData = X.df.valid)
    
    # take object from formed random forests model and use X.df.temp dataframe to make predictions    
    X.df.temp$Total_Cover <- X.df.temp$canopy_cover
    X.df.temp$Dominant_Nom_Ht <- X.df.temp$canopy_height
    temp.dc <- as.character(X.df.temp$disturb_code)
    temp.dc[temp.dc!= "0"] <- 1
    temp.dc <- as.factor(temp.dc)
    X.df.temp$disturb_code <- temp.dc
    temp.newtargs <- newtargets(yai.treelist.bin, newdata = X.df.temp)    
    temp.xall <- temp.newtargs$xall    
    out.neiIds <- temp.newtargs$neiIdsTrgs    
    out.trgrows <- temp.newtargs$trgRows    
    yrows <- as.numeric(out.neiIds[,1])    
    id.out <- id.table[yrows]    
    impute.out[valid.cols] <- yrows[1:nrows.orig]    
  }
  outval <- currow / nrows.out
  fout <- "zoneprog.txt"
  write.table(outval, fout)

  return(impute.out)  
}



Sys.time()
cl <- makeCluster(ncores, port="10187")
registerDoParallel(cl)
Sys.time()
mout.2014.rasters.2016 <- foreach(m = 1:nrows.out, .packages = c("raster", "rgdal", "yaImpute"), .combine="rbind") %dopar%   impute.row(m)
	
stopCluster(cl)
closeAllConnections()
Sys.time()

mout <- mout.2014.rasters.2016 

m.raster <-dem.raster

m.raster.out <- raster(mout)
m.raster.out@extent <-dem.raster@extent
m.raster.out@crs <-dem.raster@crs


setwd("G:\\Workspace\\treemap\\treelist_2016\\Output")

fout <- paste(cur.zone, "_2014-xtable-2016-raster-bindc.tif", sep="")
writeRaster(m.raster.out, fout, overwrite=TRUE)


