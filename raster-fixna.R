library(raster)
library(rgdal)
library(sp)

library(parallel)

setwd("G:\\Workspace\\treemap")

setwd("G:\\Workspace\\treemap\\Spatial_data\\biophysical_gradients\\d.gradients")


rdir <- getwd()
subdirs <- list.files()
ndirs <- length(subdirs)
cl <- makeCluster(16)
clusterExport(cl, "base")
writebrick <- function(curdir.sub)
{
  library(raster)
  library(rgdal)
  library(sp)
  
  
  setwd(zdir)
  setwd(subdirs.sub[curdir.sub])
  
  img.files <- Sys.glob("*.img")
  nras <- length(img.files)
  
  img.list <- vector("list", nras)
  for(cur.ras in 1:nras)
  {
    img.list[[cur.ras]] <- img.files[cur.ras]
    
  }
  
  temp.brick <-brick(img.list)
  
  rs <- raster(img.files[1])
  
  for(cur.ras in 2:nras)
  {
    tempras <- raster(img.files[cur.ras])
    rs <- stack(rs, tempras)
    
    #rs.approx <- approxNA(rs)
    
  }
  temp.brick <-brick(rs)
  
  ###Get shapefile for current zone
  
  shpdir <- ("G:\\Workspace\\treemap\\mapzones_by_zone")
  zonestr <- subdirs.sub[curdir.sub]
  zonestr <- gsub("z", "zone", zonestr)
  
  zoneshp <- readOGR(shpdir, zonestr)
  
  SpP_ras <- rasterize(zoneshp, temp.brick, getCover=TRUE)
  SpP_ras[SpP_ras==0] <- NA
  
  outf <- paste(zonestr, "raster-brick-BIL.grd", sep = "-")
  setwd("G:/Workspace/treemap/Spatial_data/output")
  
  writeRaster(SpP_ras,outf, bandorder='BIL', overwrite=TRUE)
  
  
}

a <- 1+1

for(curdir in 1:ndirs)
{
  setwd(rdir)
  setwd(subdirs[curdir])
  
  zdir <- subdirs[curdir]
  zdir <- getwd()
  subdirs.sub <- list.files()
  
  ndirs.sub <- length(subdirs.sub)
  
  clusterExport(cl, c("ndirs.sub", "subdirs.sub", "zdir"))
  parLapply(cl, 1:ndirs.sub, writebrick)
  
}