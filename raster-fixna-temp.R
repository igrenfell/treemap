library(raster)
library(rgdal)
library(sp)

library(parallel)

setwd("G:\\Workspace\\treemap")

setwd("G:\\Workspace\\treemap\\Spatial_data\\biophysical_gradients\\d.gradients")


rdir <- getwd()
subdirs <- list.files()
ndirs <- length(subdirs)
clusterExport(cl, "base")
writebrick <- function(curdir.sub)
{
  library(raster)
  library(rgdal)
  library(sp)
  
  ###Test if done
  isdone <- FALSE
  
  
 
   
  setwd(zdir)
  setwd(subdirs.sub[curdir.sub])
   
  cur.wd <- getwd()
  # 
  # setwd("G:\\Workspace\\treemap\\Spatial_data\\output")
  # 
  # testroot <- subdirs.sub[curdir.sub]
  # testfname <-paste(testroot, "tmax.tif", sep = "")
  # if(file.exists(testfname))
  # {
  #   isdone = TRUE
  #   
  # }
  # 
  # 
  
  if(!isdone)
  {
      
    setwd(cur.wd)
    
    
    rtemp <- "G:\\Workspace\\treemap\\Spatial_data\\biophysical_gradients\\rastertemp"
    
    rasterOptions(tmpdir=rtemp)
    img.files <- Sys.glob("*.img")
    nras <- length(img.files)
    
    img.list <- vector("list", nras)
    # for(cur.ras in 1:nras)
    # {
    #   img.list[[cur.ras]] <- img.files[cur.ras]
    #   
    # }
    # 
    # temp.brick <-brick(img.list)
    #
    # 
    # rs <- raster(img.files[1])
    # 
    # for(cur.ras in 2:nras)
    # {
    #   tempras <- raster(img.files[cur.ras])
    #   rs <- stack(rs, tempras)
    #   
    #   #rs.approx <- approxNA(rs)
    #   
    # }
    # temp.brick <-brick(rs)
    # 
    ###Get shapefile for current zone
    tempras <- raster(img.files[1])
    shpdir <- ("G:\\Workspace\\treemap\\mapzones_by_zone")
    zonestr <- subdirs.sub[curdir.sub]
    
    zonenum <-gsub("z", "", zonestr)
    zonenum <- as.numeric(zonenum)
    #zonestr <- gsub("z", "zone", zonestr)
    
    zonestr <- paste("zone", zonenum, sep = "")
    setwd(shpdir)
    zoneshp <- readOGR(shpdir, zonestr)
    
    SpP_ras <- rasterize(zoneshp, tempras, getCover=TRUE)
    SpP_ras[SpP_ras==0] <- NA
    
    # outf <- paste(zonestr, "raster-brick-BIL.grd", sep = "-")
    # setwd("G:/Workspace/treemap/Spatial_data/output")
    # 
    # writeRaster(SpP_ras,outf, bandorder='BIL', overwrite=TRUE)
    # 
    
    for(cur.ras in 1:nras)
    {
      
      setwd(zdir)
      setwd(subdirs.sub[curdir.sub])
      
      tempf <- img.files[cur.ras]
      tempras <- raster(tempf)
      tempras[SpP_ras==0] <- NA
      
      outf <- img.files[cur.ras]
      outf <- gsub(".img", ".tif", outf)
      setwd("G:/Workspace/treemap/Spatial_data/output")
      writeRaster(tempras,outf, bandorder='BIL', overwrite=TRUE, format = "GTiff")
      
    }
    
    #removeTmpFiles(h=24)
    
  }
  
}
rasterTmpFile('mytemp_')
a <- 1+1
# 
# for(curdir in 1:ndirs)
# {
#   setwd(rdir)
#   setwd(subdirs[curdir])
#    
#   zdir <- subdirs[curdir]
#   zdir <- getwd()
#   subdirs.sub <- list.files()
#   
#   ndirs.sub <- length(subdirs.sub)
#   
#   clusterExport(cl, c("ndirs.sub", "subdirs.sub", "zdir"))
#   parLapply(cl, 1:ndirs.sub, writebrick)
#   
# }

zdir <- "G:\\Workspace\\treemap\\Spatial_data\\biophysical_gradients\\allzones"
setwd(zdir)
subdirs.sub <- list.files()
subdirs.sub <- subdirs.sub[c(5, 6, 36, 43:67)]

ndirs.sub <- length(subdirs.sub)

cl <- makeCluster(14)
clusterExport(cl, c("ndirs.sub", "subdirs.sub", "zdir"))
parLapply(cl, 1:ndirs.sub, writebrick)
closeAllConnections()



##On to mosaic-ing the rasters

library(raster)
setwd("G:/Workspace/treemap/Spatial_data/output")

flist <- Sys.glob("*.tif")

vpd.file.index <- grep("vpd", flist)
relhum.file.index <- grep("relhum", flist)
tmax.file.index <- grep("tmax", flist)
tmin.file.index <- grep("tmin", flist)
par.file.index <- grep("par", flist)
ppt.file.index <- grep("ppt", flist)


vpd.flist <- flist[vpd.file.index]


raster.list <- vector("list", length(vpd.flist))
for(i in 1:length(vpd.flist))
{
  raster.list[[i]] <- raster(vpd.flist[i])
  
}

raster.list$fun <- max
raster.list$na.rm <- TRUE

vpd.mosaic <- do.call("mosaic", raster.list)




vpd.raster.orig <- vpd.mosaic

proj4string(vpd.mosaic) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

writeRaster(vpd.mosaic, "vpd-mosaic.tif", format = "GTiff", overwrite = TRUE)


bbox.mosaic <- extent(vpd.mosaic)

##Get forested layer
xminval  <- xmin(vpd.mosaic)
xmaxval  <- xmax(vpd.mosaic)
yminval  <- ymin(vpd.mosaic)
ymaxval  <- ymax(vpd.mosaic)

xquart <- diff(range(c(xminval, xmaxval)))/16 + xminval

xmean <- mean(c(xminval, xmaxval))

ymean <- mean(c(yminval, ymaxval))

yquart <- ymaxval-diff(range(c(yminval, ymaxval)))/16

xmintemp <- xmean-300
xmaxtemp <- xmean+300


ymintemp <- ymean-300
ymaxtemp <- ymean+300


bbox.crop <- bbox.mosaic

bbox.crop[1] <- xmintemp
bbox.crop[2] <- xmaxtemp
bbox.crop[3] <- ymintemp
bbox.crop[4] <- ymaxtemp

vpd.test <- crop(vpd.mosaic, bbox.crop)

##get upper left quarter
xmintemp <- xminval
xmaxtemp <- xquart
ymintemp <- xquart
ymaxtemp <- ymaxval

bbox.crop <- bbox.mosaic

bbox.crop[1] <- xmintemp
bbox.crop[2] <- xmaxtemp
bbox.crop[3] <- ymintemp
bbox.crop[4] <- ymaxtemp

vpd.test <- crop(vpd.mosaic, bbox.crop)
writeRaster(vpd.test, "vpd-test-nweightth.tif", format = "GTiff", overwrite = TRUE)

vpd.nw <- vpd.crop
writeRaster(vpd.nw, "vpd-nw.tif", format = "GTiff", overwrite = TRUE)

###Get forested raster
setwd("H:\\TreeMap2016\\target_data\\national_masks")
forest.raster <- raster("EVG_maskedEVCforest_nodevdist.tif"  )

forest.prj <- proj4string(forest.raster)

extent(vpd.mosaic)
extent(forest.raster)

###Get forested raster
setwd("H:\\TreeMap2016\\target_data\\national_masks")
forest.raster <- raster("EVG_maskedEVCforest_nodevdist.tif"  )
forest.crop <- crop(forest.raster, extent(vpd.mosaic))
setwd("G:/Workspace/treemap/Spatial_data/output")

writeRaster(forest.crop, "forest-crop.tif", format = "GTiff", overwrite = TRUE)
vpd.mosaic[forest.crop < 1] <- NA


writeRaster(vpd.mosaic, "vpd-forest.tif", format = "GTiff", overwrite = TRUE)

forest.crop.temp <- crop(forest.crop, extent(vpd.mosaic))

forest.extend.temp <- extend(forest.crop.temp, extent(vpd.mosaic))




forest.mask.temp <- mask(vpd.mosaic, forest.crop.temp)


ext.forest.mask.temp <- extent(forest.mask.temp)

writeRaster(forest.extend.temp, "forest-mask-extended.tif", type= "GTiff", overwrite=T)


beginCluster(62)
vpd.mask <- mask(vpd.mosaic, forest.extend.temp)
Sys.time()

writeRaster(vpd.mask, "vpd-mask.tif", type= "GTiff", overwrite=T)
Sys.time() 

endCluster()



###Get all layers, loop through zones and write masked output


setwd("G:\\Workspace\\treemap\\Spatial_data\\national_masks")

flist.layers <- Sys.glob("*.tif")
masklayers <- grep("mask", flist.layers)
flist.mask <- flist.layers[masklayers]

n.mask <- length(flist.mask)
r.mask <- raster(flist.mask[1])

for(i in 2:n.mask)
{
  tempraster <- raster(flist.mask[i])
  r.mask <- stack(tempraster, r.mask)
  
  
}





setwd("G:\\Workspace\\treemap\\Spatial_data\\output")

flist.zones <- Sys.glob("*.tif")
tmax.zones <- grep("tmax", flist.zones)
flist.zones.sub <- flist.zones[tmax.zones]

nzones <- length(flist.zones.sub)
getzone <- function(curzone)
{
  
  library(raster)
  library(rgdal)
  library(sp)
  setwd("G:\\Workspace\\treemap\\Spatial_data\\output")
  
  curzone.raster <- raster(flist.zones.sub[curzone])
  
  curext <- extent(curzone.raster)
  
  r1 <- subset(r.mask, 1)
  curcrop <- crop(r1, curext)
  curcrop <- extend(curcrop , curext)
  curzone.raster[curzone.raster < 1] <- NA
  curmask <- mask(curcrop , curzone.raster) 
  nl <- nlayers(curmask)
  tempzone <- flist.zones.sub[curzone]
  tempzone <- gsub("tmax", "", tempzone)
  setwd("G:\\Workspace\\treemap\\Spatial_data\\masked-output")
  for(curlayer in 1:nl)
  {
    tempmask <- subset(curmask, curlayer)
    tempname <- names(tempmask)
    
    tempname <- gsub("[.]", "-", tempname)
    tempname 
    ftemp <- paste(tempname, "-", tempzone, sep = "")
    if(!file.exists(ftemp))
    {
      writeRaster(tempmask, ftemp, format = "GTiff", overwrite = TRUE)
    }
  }
  
}

library(parallel)
cl = makeCluster(11)
Sys.time()
clusterExport(cl, c("flist.zones", "tmax.zones","flist.zones.sub", "nzones" , "r.mask"))
parLapply(cl, 1:nzones, getzone)

stopCluster(cl)

Sys.time()
