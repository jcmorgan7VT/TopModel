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

###########################
#Dynatop test 2/7/23
#https://cran.r-project.org/web/packages/dynatop/vignettes/dynatop.html
install.packages("dynatop")
library(dynatop)
data("Swindale")

names(Swindale)

swindale_model <- Swindale$model
swindale_obs <- Swindale$obs

names(swindale_model)


swindale_model$map$hillslope <- system.file("extdata","Swindale.tif",package="dynatop",mustWork=TRUE)
swindale_model$map$channel <- system.file("extdata","channel.shp",package="dynatop",mustWork=TRUE)
swindale_model$map$channel_id <- system.file("extdata","channel_id.tif",package="dynatop",mustWork=TRUE)


ctch_mdl <- dynatop$new(swindale_model)

#adding discharge observations
ctch_mdl$add_data(swindale_obs)
#running a simulation
ctch_mdl$sim()
ctch_mdl$initialise()$plot_state("s_sz")
sim1 <- ctch_mdl$sim()$get_gauge_flow()
ctch_mdl$plot_state("s_sz")

sim2 <- ctch_mdl$sim()$get_gauge_flow()
out <- merge( merge(swindale_obs,sim1),sim2)
names(out) <- c(names(swindale_obs),'sim_1','sim_2')
plot(out[,c('Flow','sim_1','sim_2')], main="Discharge",ylab="m3/s",legend.loc="topright")

#trying to follow the set up guide again
library(dynatopGIS)
demo_dir <- tempfile("dygis")
dir.create(demo_dir)
#create a new dynatop object
ctch <- dynatopGIS$new(file.path(demo_dir,"meta.json"))

#add catchment data
#specify paths to the DEM and stream flowlines
channel_file <- "C:/Users/John/Documents/VT Research/HBTopModel/HB/hbstream/HB_Shapefiles/hb42_master.shp"

stream <- readOGR(channel_file)

