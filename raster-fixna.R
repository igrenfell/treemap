library(raster)
library(rgdal)
library(sp)

setwd("G:\\Workspace\\treemap")

setwd("G:\\Workspace\\treemap\\Spatial_data\\biophysical_gradients\\d.gradients")


rdir <- getwd()
subdirs <- list.files()
ndirs <- length(subdirs)


for(curdir in 1:ndirs)
{
  setwd(rdir)
  setwd(subdirs[curdir])
  subdirs.sub <- list.files()
  
  ndirs.sub <- length(subdirs.sub)
  for(curdir.sub in 1:ndirs.sub)
  {
    setwd(subdirs.sub[curdir.sub])
    
    img.files <- Sys.glob("*.img")
    
    nras <- length(img.files)
    rs <- raster(img.files[1])
    
    for(cur.ras in 2:nras)
    {
      tempras <- raster(img.files[cur.ras])
      rs <- stack(rs, tempras)
      
      rs.approx <- approxNA(rs)
      
    }
    
    
  }
  
  
}