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

raster.list$fun <- min
raster.list$na.rm <- TRUE

vpd.mosaic <- do.call("mosaic", raster.list)




