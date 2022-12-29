#running top model and making maps for R presentation
#setup
#most of code copied from lab6R4geo.Rmd
library(pacman)

p_load(dynatopGIS, tidyverse, raster, rgdal, sp, mapview, sf,
       rgeos, spdplyr, whitebox, RColorBrewer, topmodel, lubridate)

#reading in dem and watershed shapefiles
dem <- raster("./HB/knb-lter-hbr.211.2/dem1m.tif")
wall <- readOGR(dsn='./HB/hbef_wsheds',layer = 'hbef_wsheds')


w3 <- wall %>% filter(WS == "WS3")

w3_p <- spTransform(w3, crs(dem))
dem_crop <- crop(dem, w3_p)
w3_dem <- mask(dem_crop, w3_p)

#stream shapefile, from Jensen et al. 2017
setwd("C:/Users/John/Documents/HBTopModel/HB/hbstream/HB_Shapefiles")
stream <- readOGR("hb42_master.shp")
s <- raster::shapefile("hb42_master.shp")
stream_p <- spTransform(stream, crs(dem))

#make a hillshade
wbt_hillshade(dem = "w3_dem.tif",
              output = "w3_hillshade.tif",
              azimuth = 115)
setwd("C:/Users/John/Documents/HBTopModel/HB/knb-lter-hbr.211.2")
wbt_hillshade(dem = "dem1m.tif",
              output = "wall_hillshade.tif",
              azimuth = 115)


hill <- raster("w3_hillshade.tif")
bighill <- raster("wall_hillshade.tif")

wall_p <- spTransform(wall, crs(dem))
hill_crop <- crop(bighill, wall_p)
hill_f <- mask(hill_crop, wall_p)
mapview(hill_f, col.regions = brewer.pal(6, "Greys"), legend = FALSE)+
  mapview(w3_p, layer.name = "Watershed 3")
  mapview(stream_p)

#make a nice map of the HB LTER, highlighting watershed 3
ggplot()+
  geom_raster(hill_f, aes(x = x, y = y, fill = z))



#generating topographic index

#convert DEM to matrix
mat <- raster::as.matrix(dem_crop)
mat[mat==-9999] <- NA
#already generated topidx table before
topindex <- topidx(mat, resolution=1)
#write.table(topindex, 'HB_topindex.txt')
#plot of twi
hist(topindex$atb, xlab = "Topographic Index",
     main = "Distribution of Topographic Index",
     breaks = 25)

#topidx <- read.csv("C:/Users/John/Documents/HBTopModel/HB_topindex.txt", sep = " ")
topidx1 <- make.classes(topindex$atb,16)
topidx2 <- make.classes(topindex$area,16)

# the delay function is a bit more tricky because this requires cumulative fractions, but you generate it as follows:

n <- 5 # number of classes; a higher number will result in a smoother histogram
which(mat == min(mat), arr.ind = TRUE)

delay <- flowlength(mat, c(837, 176))*1 #TOD: add the outlet coordinates; otherwise the flowlength will be calculated to the edge of the map.
delay <- make.classes(delay, n)
delay <- delay[n:1,]
delay[,2] <- c(0, cumsum(delay[1:(n-1),2]))

data(huagrahuma)

h <- huagrahuma
#using a combination of variables from example data and HB
#precip
setwd("C:/Users/John/Documents/HBTopModel")
rain <- read.csv("HB/dailyWatershedPrecip1956-2021.csv")
rain$DATE <- ymd(rain$DATE)
rain2 <- filter(rain, watershed == "W3") %>% 
  filter(DATE > ymd("2020-01-01") & DATE < ymd("2021-01-01"))

#actual observed discharge
q <- read.csv("HB/w3_stmflow_2013-2022_5min.csv")
q$DATETIME <- ymd_hms(q$DATETIME)
q2 <- q %>%
  mutate(date = floor_date(DATETIME, unit = "day")) %>%
  group_by(date) %>%
  summarise(
    n = n(),
    q_total = sum(Discharge_ls),
    q_average = mean(Discharge_ls),
    .groups = "drop"
  ) %>% 
  filter(date > ymd("2020-01-01") & date < ymd("2021-01-01"))

#generating PET data
#using air temp to parameterize: better than lazy method
temps <- read.csv("HB/HBEF_air_temp_daily_1957-2022.csv")

temps2 <- filter(temps, STA == "STA6")
temps2$date <-  ymd(temps2$date)
temps3 <- filter(temps2, date > ymd("2020-01-01") & date < ymd("2021-01-01"))
#better way- estimating PET using harmon method: https://vt-hydroinformatics.github.io/modelingintro.html#calculate-pet

#find latitude of site
lat <- 43 + 57/60 #43 degrees and 57 minutes
latrad <- (lat/360) * 2 * pi #convert to radians

PET1 <- dplyr::select(temps3, date, AVE) %>%
  mutate(DOY = yday(date)) %>% #DOY for dates
  mutate(tempvar = (2 * pi / 365) * DOY) %>%
  #declination of the sun above the celestial equator in 
  #radians on day JulDay of the year
  mutate(delta_h = 0.4093 * sin(tempvar - 1.405)) %>% 
  #day length in h
  mutate(daylen = (2 * acos(-tan(delta_h) * tan(latrad)) / 0.2618)) %>% 
  mutate(PET = 29.8 * daylen * 0.611 * exp(17.3 * AVE / 
                                       (AVE + 237.3)) / (AVE + 273.2))  #PET Hamon method

PET <- PET1$PET




test <- topmodel(best, topidx1, h$delay, rain2$Precip, PET, verbose = F, Qobs = NA)

#plot(test, type="l", col="red", ylim = c(0, 30))
#points(q2$q_average, type = 'l')

final <- data.frame("Date" = q2$date,
                    "Actual_Q" = q2$q_average,
                    "Modeled_Q" = test)

final2 <- gather(final, key = "key", value = "value", -Date) %>% 
  filter(key == "Modeled_Q" | key == "Actual_Q")
ggplot(final2)+
  geom_line(aes(x = Date, y = value, color = key))+
  theme_classic()+
  labs(y = "Discharge (L/s)",
       title = "Simulated discharge using topmodel for Paradise Brook, 2020")

## Now sample all parameters at random. We take a sample size of 100

n <- 1000

qs0 <- runif(n, min = 0.0001, max = 0.00025)
lnTe <- runif(n, min = -2, max = 3)
m <- runif(n, min = 0, max = 0.1)
Sr0 <- runif(n, min = 0, max = 0.2)
Srmax <- runif(n, min = 0, max = 0.1)
td <- runif(n, min = 0, max = 3)
vch <- runif(n, min = 100, max = 2500)
vr <- runif(n, min = 100, max = 2500)
k0 <- runif(n, min = 0, max = 10)
CD <- runif(n, min = 0, max = 5)
dt <- 24

parameters <- cbind(qs0,lnTe,m,Sr0,Srmax,td,vch,vr,k0,CD,dt)

best <- parameters[which.max(test),]

test <- topmodel(parameters, topidx1, h$delay, rain2$Precip, PET, verbose = F, Qobs = q2$q_average)
summary(test)
hist(test, xlim = c(-0.2, 0.2), bins = 10)
