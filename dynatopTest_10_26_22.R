# 10/26/28
# #Attemptimg to implement dynatop, following dynatop guide
# https://cran.r-project.org/web/packages/dynatopGIS/vignettes/dynatopGIS.html 

#install.packages("dynatopGIS")
# Dynatop implementation 4/24/23
####################################
library(pacman)

p_load(dynatop, dynatopGIS, tidyverse, raster, rgdal, sp, mapview,
       rgeos, spdplyr, igraph)
#working from new folder, called dynatopTest


#create a new object, with location of meta data file (which it will make)
ctch <- dynatopGIS$new(file.path("./dynatopTest","meta.json"))

#my actual files
dem <- raster("C:/Users/John/Documents/VT Research/HBTopModel/HB/knb-lter-hbr.211.2/dem1m.tif")
#plot(dem)

w3 <- readOGR(dsn='C:/Users/John/Documents/VT Research/HBTopModel/HB/hbef_wsheds',layer = 'hbef_wsheds')
w3 <- w3 %>% filter(WS == "WS3")
#plot(w3)
#clipping DEM by W3 boundary
w3_p <- spTransform(w3, crs(dem))
#dem_crop <- crop(dem, w3_p)
#w3_dem <- mask(dem_crop, w3_p)
#raster::writeRaster(w3_dem, "w3_dem.tif", format = "GTiff")
w3_dem <- raster("C:/Users/John/Documents/VT Research/HBTopModel/w3_dem.tif")

#plot(w3_dem, add = TRUE)
#stream shapefile, from Jensen et al. 2017
#original stream shapefile
#stream <- readOGR("./HB/hbstream/HB_Shapefiles/hb42_master.shp")
#streams with start and end nodes
#generated using the archydro tool Generate to/from nodes for lines on a split shapefile of stream
stream <- readOGR("./HB/hbstream/hb42_master_startend.shp")

stream_p <- spTransform(stream, crs(dem))
#mapview(list(w3_p,stream_p))
stream_c <- crop(stream_p, w3_p)
stream_c <- stream_c[,-2]
stream_c$Id <- seq(1, length(stream_c$Id), 1)
stream_c <- stream_c %>% rename("channel_id" = Id,
         "length" = Shape_Leng,
         "endNode" = To_Node,
         "startNode" = From_Node)

property_names <- c(channel_id="channel_id",
                    endNode="endNode",
                    startNode="startNode",
                    length="length")


#adding dem to the project
ctch$add_dem(w3_dem)
#adding stream channel to the project
ctch$add_channel(stream_c, property_names)

ctch$compute_areas()
ctch$plot_layer("dem", add_channel=TRUE)

#fill sinks
ctch$sink_fill()

#terra::plot( ctch$get_layer('filled_dem') - ctch$get_layer('dem'),main="Changes to height")
#compute properties
ctch$compute_properties()
#plot of topographic index
ctch$plot_layer('atb')
ctch$get_layer()
ctch$compute_flow_lengths()
ctch$plot_layer("band")

#calculate HRU
ctch$classify("atb_20","atb",cuts=20)
ctch$plot_layer("atb_20")

#defining HRUs based on multiple layers
ctch$combine_classes("atb_20_band",c("atb_20","band"))
ctch$plot_layer("atb_20_band")
#actually making the dynamic topmodel
ctch$create_model("new_model","atb_20_band","band")

list.files(demo_dir,pattern="new_model*")
ctch$plot_layer("new_model")

#inputs to make the model object
options <- c("transmissivity_profile" = "exp",
             "channel_solver" = "histogram")
#locations of channel, HRU files generated earlier
map <- c("hillslope" = "./dynatopTest/new_model.tif",
         "channel" = "./dynatopTest/channel.shp",
         "channel_id" = "./dynatopTest/channel_id.tif")
#table of hillslope HRU paramters
hru <- read.table()
hillslope <- data.frame("id" = seq(1, 20, 1),
                        "area" = c())

new_model <- list(
  "options" = options,
  "map" = map,
  "channel" = as.data.frame(stream_c), 
  #hillslope
  #flow_direction
  #gauge
  #point_inflow
  #diffuse_inflow
  #precip_input
  #pet_input
)
#YES! SUCCESS!!!!!!! of course it would be so easy! DynatopGIS does make an easy object, in the RDS format
new_model <- readRDS("./dynatopTest/new_model.rds")
new_model$hillslope$atb_bar[new_model$hillslope$atb_bar < 0] <- 0
dynatop$new(new_model)
###########################
#Dynatop test 2/7/23
#https://cran.r-project.org/web/packages/dynatop/vignettes/dynatop.html
install.packages("dynatop")
library(dynatop)
data("Swindale")
model <- Swindale$model

names(model)

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


#the graveyard
##########################
#load up demo files
dem_file <- system.file("extdata", "SwindaleDTM40m.tif", package="dynatopGIS", mustWork = TRUE)
channel_file <- system.file("extdata", "SwindaleRiverNetwork.shp", package="dynatopGIS", mustWork = TRUE)
sp_lines <- raster::shapefile(channel_file)
head(sp_lines)
property_names <- c(channel_id="identifier",
                    endNode="endNode",
                    startNode="startNode",
                    length="length")
#make a hillshade
wbt_hillshade(dem = "w3_dem.tif",
              output = "w3_hillshade.tif",
              azimuth = 115)

hill <- raster("w3_hillshade.tif")
plot(hill, col = "grey")
