# 10/26/28
# #Attemptimg to implement dynatop, following dynatop guide
# https://cran.r-project.org/web/packages/dynatopGIS/vignettes/dynatopGIS.html 

#install.packages("dynatopGIS")
library(pacman)

p_load(dynatopGIS, tidyverse, raster, rgdal, sp, mapview,
       rgeos, spdplyr, whitebox)
#working from new folder, called dynatopTest

demo_dir <- tempfile("dygis")
dir.create(demo_dir)
#create a new object, with location of meta data file (which it will make)
ctch <- dynatopGIS$new(file.path(demo_dir,"meta.json"))

#load up demo files
dem_file <- system.file("extdata", "SwindaleDTM40m.tif", package="dynatopGIS", mustWork = TRUE)
channel_file <- system.file("extdata", "SwindaleRiverNetwork.shp", package="dynatopGIS", mustWork = TRUE)
sp_lines <- raster::shapefile(channel_file)
head(sp_lines)
property_names <- c(channel_id="identifier",
                    endNode="endNode",
                    startNode="startNode",
                    length="length")


#my actual files
setwd("C:/Users/John/Documents/HBTopModel/HB/knb-lter-hbr.211.2")
dem <- raster("dem1m.tif")
#plot(dem)

setwd("C:/Users/John/Documents/HBTopModel/HB/hbef_wsheds")
w3 <- readOGR(dsn='.',layer = 'hbef_wsheds')

w3 <- w3 %>% filter(WS == "WS3")
plot(w3)

w3_p <- spTransform(w3, crs(dem))
dem_crop <- crop(dem, w3_p)
w3_dem <- mask(dem_crop, w3_p)
raster::writeRaster(w3_dem, "w3_dem.tif", format = "GTiff")

plot(w3_dem, add = TRUE)
plot(stream_p, add = TRUE)
#stream shapefile, from Jensen et al. 2017
setwd("C:/Users/John/Documents/HBTopModel/HB/hbstream/HB_Shapefiles")
stream <- readOGR("hb42_master.shp")
s <- raster::shapefile("hb42_master.shp")
stream_p <- spTransform(stream, crs(dem))

mapview(list(w3_p,stream_p))

stream_c <- crop(stream_p, w3_p)
#make a hillshade
wbt_hillshade(dem = "w3_dem.tif",
              output = "w3_hillshade.tif",
              azimuth = 115)

hill <- raster("w3_hillshade.tif")
  plot(hill, col = "grey")

#adding dem to the project
ctch$add_dem(dem)
ctch$add_channel(stream_c)

ctch$compute_areas()

#easy
test <- gUnaryUnion(stream_c)
stream_c$startNode <- seq(1, length(stream_c$Id), 1)
