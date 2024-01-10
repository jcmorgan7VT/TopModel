#STICr test
library(pacman)
p_load(tidyverse, raster, rgdal, sp, mapview,
       rgeos, spdplyr, tmap, stars, STICr,
       lubridate, gganimate, animation, patchwork)


## Steps that I took to download the STICr package
#install.packages("devtools")
#library(devtools)
#devtools::install_github("HEAL-KGS/STICr")


#analysis of W1 and W2 stic sensors
directory <- "./W1W2loggers_6_14_23"
files <- list.files(directory)

test <- read.csv(paste0(directory, "/", files[1]), skip = 1)

data <- tidy_hobo_data(paste0(directory, "/", files[1]))
data <- filter(data, datetime >= "2023-06-06 14:00:00") %>% 
  filter(datetime <= "2023-06-14 11:00:00")
wet <- classify_wetdry(data, classify_var = "condUncal", method = "absolute", threshold = 50)
wet$stream <- substr(files[1], 1, 2)
wet$dist <- substr(files[1], 4, 6)
wet$datetime <- ymd_hms(wet$datetime)

#removed w1_200 and w1_225 manually, because the loggers failed
for(i in 1:length(files)){
  data <- tidy_hobo_data(paste0(directory, "/", files[i]))
  data <- filter(data, datetime >= "2023-06-06 14:00:00") %>% 
    filter(datetime <= "2023-06-14 11:00:00")
  wet <- classify_wetdry(data, classify_var = "condUncal", method = "absolute", threshold = 40)
  wet$stream <- substr(files[i], 1, 2)
  wet$dist <- substr(files[i], 4, 6)
  wet$datetime <- ymd_hms(wet$datetime)
  if(i == 1) alldat <- wet
  if(i > 1) alldat <- rbind(alldat, wet)
}
#for loop to read in and combine all points

#read in shapefiles of point locations
w1_points <- readOGR(dsn = "./Map files 6_7_23", layer = "W1_points_6_7_23")
w1_points$ORIG_FID <- c(475, 461, 425, 400, 350, 325, 300, 250, 
                        225, 200, 175, 150, 125, 100, 75, 50, 25, 0)
#fixed a duplicate point, remade shapefile 8/23/23
w2_points <- readOGR(dsn = "./Map files 6_7_23", layer = "W2_points_8_23_23")
w2_points$ORIG_FID <- c(325, 300, 275, 250, 225, 200, 175, 150,
                        125, 100, 75, 50, 25, 350, 375, 400 ,0 ,425)
w3_streams <- readOGR(dsn = "./Map files 6_7_23", layer = "W3_stream")

w3 <- readOGR(dsn='C:/Users/John/Documents/VT Research/HBTopModel/HB/hbef_wsheds',layer = 'hbef_wsheds')
w3 <- w3 %>% filter(WS == "WS3")

hill <- raster("./Map files 6_7_23/W3_hillshade.tif")

dem_crop <- crop(hill, w3)
w3_dem <- mask(dem_crop, w3)



tmap_mode("view")
## tmap mode set to plotting
tm_shape(w3_dem)+
  tm_raster(palette = "Greys", colorNA = NULL, style = "cont") +
  tm_shape(w3_streams) +
  tm_lines(col = "StrType", palette = "Blues", scale = 3)+
  tm_shape(w1_points) +
    tm_dots(col = "black", scale = 1)
  # tm_shape(w2_points) +
  #   tm_dots(col = "black", scale = 1) +
  #   tm_facets(along = "time")+
  #   tm_legend(show = FALSE)



#adding coordinates of sensor locations
w1_df <- as(w1_points, "data.frame")
w2_df <- as(w2_points, "data.frame")

w1_df$stream <- "w1"
w2_df$stream <- "w2"

all_w <- rbind(w1_df, w2_df)%>% 
  rename(dist = ORIG_FID) %>% 
  dplyr::select(dist, stream, coords.x1, coords.x2)
all_w$ID <- paste0(all_w$stream, "_", all_w$dist)

#creating ID field
test <- alldat
test$dist <- as.numeric(test$dist)
test$ID <- paste0(test$stream, "_", test$dist)

all_w2 <- dplyr::select(all_w, -c(dist, stream))
joy <- inner_join(test, all_w2, by = "ID")


w3_streams <- as(w3_streams, 'sf')



#STOP HERE FOR RELAUNCHING
##Saving output from above code as formatted csv

#write.csv(joy, "25mformatted.csv")
#8/18/23 found critical issue, all observations at every time are duplicated but can be different
#8/18/23 (30 mins later) I think I fixed it; do not remember how
sample <- filter(joy, datetime == "2023-06-08 14:40:00")
onetime <- (filter(joy, datetime == joy$datetime[1000]))
#8/23/23 determined that w2_350 is duplicated in saved csv, figure out why
length(test$ID[test$ID == "w2_300"])
length(test$ID[test$ID == "w2_350"])

length(all_w$ID[all_w$ID == "w2_300"])
length(all_w$ID[all_w$ID == "w2_350"])
#all code below is analysis for Gordon poster

#figures for poster
#figure out when it was the wettest and driest in each stream
times <- unique(alldat$datetime)
w1_wet <- (filter(alldat, stream == "w1" & datetime == times[1]))
head(w1_wet)

w1_wet <- (filter(alldat, datetime == times[1000]))
length(which(w1_wet$wetdry == "wet"))

#loop to figure out when the wettest time was
for(z in 1:length(times)){
  onetime <- (filter(alldat, datetime == times[z]))
  number_wet <- length(which(onetime$wetdry == "wet"))
  out <- data.frame("datetime" = times[z], "number_wet" = number_wet)
  if(z == 1) final <- out
  if(z > 1) final <- rbind(final, out)
  
}
normal <- final
which(final$datetime[final$number_wet == max(final$number_wet)])
colSort <- function(data, ...) sapply(data, sort, ...)
head(colSort(final, decreasing = TRUE))

#wettest times
#2023-06-13 14:40:00
#2023-06-13 15:50:00 to 2023-06-13 16:30:00
#2023-06-14 09:10:00 to 2023-06-14 10:10:00
wettest <- filter(joy, datetime == "2023-06-13 14:40:00")
ggplot(wettest)+
  geom_point(aes(x = coords.x1, y = coords.x2, color = wetdry), size = 2)+
  scale_color_manual(values = c("black", "blue"),
                     breaks = c("dry", "wet"),
                     name = "STIC Sensors")+
  geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
  scale_linetype_manual(values = c(1,2,3),
                        breaks = c("P", "I", "E"),
                        name = "Stream Type")+
  labs(title = "Wettest network, all sensors")+
  #theme_classic()+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()#remove y axis ticks
  )


#driest time
#right at the beginning, 2023-06-06 18:00:00
driest <- filter(joy, datetime == "2023-06-06 18:00:00")
ggplot(driest)+
  geom_point(aes(x = coords.x1, y = coords.x2, color = wetdry), size = 2)+
  scale_color_manual(values = c("black", "blue"),
                     breaks = c("dry", "wet"),
                     name = "STIC Sensors")+
  geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
  scale_linetype_manual(values = c(1,2,3),
                        breaks = c("P", "I", "E"),
                        name = "Stream Type")+
  labs(title = "Driest network, all sensors")+
  #theme_classic()+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()#remove y axis ticks
  )


#animation for github upload
#6/21/23 adding discharge and precip data
#read in data
q_raw <- read.csv("ws3_Q_6_21_2023.csv", skip = 2)
p_raw <- read.csv("ws3_P_6_21_2023.csv", skip = 3)
#remove dates outside of the extent of STIC deployment
q_raw$date.and.time <- mdy_hm(q_raw$date.and.time)
p_raw$date.time <- mdy_hm(p_raw$date.time)


q_date <- filter(q_raw, date.and.time >= "2023-06-06 14:00:00") %>% 
  filter(date.and.time <= "2023-06-14 11:00:00")

p_date <- filter(p_raw, date.time >= "2023-06-06 14:00:00") %>% 
  filter(date.time <= "2023-06-14 11:00:00")
#convert inches to mm
p_date$mm <- p_date$inches * 25.4
p_date$day <- date(p_date$date.time)

p2 <- p_date %>% 
  group_by(day) %>% 
  summarise(daily_mm = sum(mm))



p <- ggplot(p2, aes(x = day, y = daily_mm))+
  geom_bar(stat = "identity")+
  theme_classic()+
  #transition_time(day) +
  labs(x = "", y = "Precip (mm)")

x <- ggplot(joy)+
  geom_point(aes(x = coords.x1, y = coords.x2, color = wetdry), size = 2)+
  scale_color_manual(values = c("red", "blue"),
                     breaks = c("dry", "wet"))+
  geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
  scale_linetype_manual(values = c(1,2,3),
                        breaks = c("P", "I", "E"))+
   theme_classic()+ 
  transition_time(datetime) +
  labs(title = "Date and Time: {frame_time}")

z <- p / x

animate(p, 
        nframes = 30,         # fewer frames for simplification
        device = "current")


saveHTML(animate(z, 
                 nframes = 30,         # fewer frames for simplification
                 device = "current"), 
         img.name = "gganimate_plot", 
         htmlfile = "index.html")


#barplot animation 7_11_23
#old barplot code
ggplot(p2, aes(x = day, y = daily_mm))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(text = element_text(size = 20))+
  labs(x = "", y = "Precip (mm)")


#new code
p2$day2 <- as.factor(p2$day)
precip <- ggplot(data = p2, aes(x = day, y = daily_mm))+
  geom_bar(stat = "identity")+
  #geom_text(aes(label = med_age), vjust=1.6, color="black", size=5)+
  theme_classic()+
  #theme(axis.text.y=element_blank(),panel.grid=element_blank(),axis.text=element_text(size=10))+
  xlab("")+
  ylab("Precip (mm)")+
  #ggtitle("Median Age by Continent", subtitle = "Source: www.visualcapitalist.com")+
  transition_reveal(day2)+
  shadow_mark()
  #transition_time(day)+
  

anim <- animate(precip)
anim_save("precip_animate.gif", anim)

#gif of streams for coop meeting
#convert flowing or dry into 1 and 0, turn into percentages
perc <- joy
perc$wetdry[perc$wetdry == "dry"] <- 0
perc$wetdry[perc$wetdry == "wet"] <- 1
perc$wetdry <- as.numeric(perc$wetdry)
#144 10 min increments in a day
perc$day <- day(perc$datetime)


perc2 <- perc %>% 
  distinct() %>% 
  group_by(day) %>% 
  group_by(ID, .add = TRUE) %>% 
  summarise(percent_wet = sum(wetdry)/144) %>% 
  ungroup()

perc3 <- left_join(perc, perc2, by = c("day", "ID"))

#I cannot figure out why w2_75 is duplicated??
x <- ggplot(perc3)+
  geom_point(aes(x = coords.x1, y = coords.x2, color = percent_wet), size = 3)+
  
  geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
  scale_linetype_manual(values = c(1,2,3),
                        breaks = c("P", "I", "E"))+
  scale_colour_gradientn(colours = c("white", "orange", "yellow", "green", "mediumblue"))+
  theme(panel.grid = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())+
  transition_time(day) +
  labs(title = "Date and Time: {frame_time}")#+
  #enter_fade()

x <- ggplot(perc3)+
  geom_point(aes(x = coords.x1, y = coords.x2, color = percent_wet), size = 2)+
  geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
  scale_colour_gradientn(colours = c("white", "lightblue", "mediumblue"),
                         name = "Proportion of Time Flowing")+
  
  scale_linetype_manual(values = c(1,2,3),
                        breaks = c("P", "I", "E"),
                        name = "Flow Regime")+
  theme(panel.grid = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())+  
  transition_time(datetime) +
  labs(title = "Date and Time: {frame_time}")

anim <- animate(x)
anim_save("stream_animate.gif", anim)
#####
#animated plots did not work out well, make static plots of wettest and driest time
perc4 <- perc3 %>% 
  group_by(day) %>% 
  summarise(how_wet = sum(percent_wet)) %>% 
  ungroup()

#find min and max

perc4[which.max(perc4$how_wet),]
perc4[which.min(perc4$how_wet),]

wet <- filter(perc3, day == 10)

ggplot(wet)+
  geom_point(aes(x = coords.x1, y = coords.x2, color = percent_wet), size = 2)+
  geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
  scale_colour_gradientn(colours = c("white", "lightblue", "mediumblue"),
                         name = "Proportion of Time Flowing")+
  
  scale_linetype_manual(values = c(1,2,3),
                        breaks = c("P", "I", "E"),
                        name = "Flow Regime")+
  theme(panel.grid = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())+
  labs(title = "Wettest time, 6-10-23")

perc4[which.min(perc4$how_wet),]

dry <- filter(perc3, day == 6)

ggplot(dry)+
  geom_point(aes(x = coords.x1, y = coords.x2, color = percent_wet), size = 2)+
  geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
  scale_colour_gradientn(colours = c("white", "lightblue", "mediumblue"),
                         name = "Proportion of Time Flowing")+
  
  scale_linetype_manual(values = c(1,2,3),
                        breaks = c("P", "I", "E"),
                        name = "Flow Regime")+
  theme(panel.grid = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())+
  labs(title = "Driest time, 6-6-23")

nums <- unique(perc3$day)
for(i in 1:length(unique(perc3$day))){
  dry <- filter(perc3, day == nums[i])
  
  plot <- ggplot(dry)+
    geom_point(aes(x = coords.x1, y = coords.x2, color = percent_wet), size = 2)+
    geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
    scale_colour_gradientn(colours = c("white", "yellow", "green", "mediumblue"),
                           name = "Proportion of Time Flowing",
                           limits = c(0, 1))+
    
    scale_linetype_manual(values = c(1,2,3),
                          breaks = c("P", "I", "E"),
                          name = "Flow Regime")+
    theme(panel.grid = element_blank(),
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())+
    labs(title = paste0(date(dry$datetime[1])))+
    guides(colour = guide_legend(order = 1), 
           linetype = guide_legend(order = 2))
  
  ggsave(paste0(date(dry$datetime[10]), ".png"), plot)
}

dry <- filter(perc3, day == 13)
date(dry$datetime[10])

nums <- unique(perc3$day)
for(i in 1:length(nums)){
  t <- filter(perc3, day == nums[i])
  print(date(t$datetime[1]))
}


########################3
#plots if I only had data from every 100 m, and every hour instead of every 10 mins
###########################
fewer_times <- alldat[seq(1, nrow(alldat), 6), ]


#loop to figure out when the wettest time was
for(z in 1:length(fewer_times$datetime)){
  onetime <- (filter(fewer_times, datetime == times[z]))
  number_wet <- length(which(onetime$wetdry == "wet"))
  out <- data.frame("datetime" = times[z], "number_wet" = number_wet)
  if(z == 1) final <- out
  if(z > 1) final <- rbind(final, out)
  
}
onehr <- final
which(final$datetime[final$number_wet == max(final$number_wet)])
colSort <- function(data, ...) sapply(data, sort, ...)
head(colSort(final, decreasing = TRUE))
#wettest time 2023-06-14 15:00:00 UTC
#driest time "2023-06-07 16:30:00 UTC"

wettest2 <- filter(joy, datetime == "2023-06-14 15:00:00")
ggplot(wettest2)+
  geom_point(aes(x = coords.x1, y = coords.x2, color = wetdry), size = 2)+
  scale_color_manual(values = c("black", "blue"),
                     breaks = c("dry", "wet"),
                     name = "STIC Sensors")+
  geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
  scale_linetype_manual(values = c(1,2,3),
                        breaks = c("P", "I", "E"),
                        name = "Stream Type")+
  labs(title = "Driest network, 2023-06-06 18:00:00")+
  #theme_classic()+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()#remove y axis ticks
  )

#from every 100 m
every100 <- filter(wettest, dist %in% c(0, 100, 200, 300, 400))
ggplot(every100)+
  geom_point(aes(x = coords.x1, y = coords.x2, color = wetdry), size = 2)+
  scale_color_manual(values = c("black", "blue"),
                     breaks = c("dry", "wet"),
                     name = "STIC Sensors")+
  geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
  scale_linetype_manual(values = c(1,2,3),
                        breaks = c("P", "I", "E"),
                        name = "Stream Type")+
  labs(title = "Wettest network, Only sensors every 100m")+
  #theme_classic()+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()#remove y axis ticks
  )

#driest with only 50 m points
every50 <- filter(wettest, dist %in% c(0, 50 ,100, 150, 200, 250, 300, 350, 400, 450, 461))
ggplot(every50)+
  geom_point(aes(x = coords.x1, y = coords.x2, color = wetdry), size = 2)+
  scale_color_manual(values = c("black", "blue"),
                     breaks = c("dry", "wet"),
                     name = "STIC Sensors")+
  geom_sf(data = w3_streams, aes(geometry = geometry, lty = StrType))+
  scale_linetype_manual(values = c(1,2,3),
                        breaks = c("P", "I", "E"),
                        name = "Stream Type")+
  labs(title = "Wettest network, Only sensors every 50m")+
  #theme_classic()+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()#remove y axis ticks
  )
# name the above gganimate example as p

###The GRAVEYARD
## Not run: 

#tm map might not work, so now I will try gganimate
w1 <- as(w1_points, "data.frame")


w1 <- w1 %>% 
  rename(dist = ORIG_FID) %>% 
  dplyr::select(dist, coords.x1, coords.x2)

#dist <- as.numeric(substr(files[1], 4, 6))
dist <- (substr(files[1], 4, 6))

w1$distance <- NA
xandy <- w1 %>% 
  filter(dist == 0)

jot <- left_join(wet, w1, by = dist)

plot(w1$coords.x1, w1$coords.x2)

length(wet$datetime)


#use this on whole datafram
tr <- pivot_wider(test, names_from = datetime, values_from = wetdry)
tr$dist <- 0
tr$dist <- as.numeric(tr$dist)

w1_0 <- filter(w1_points, ORIG_FID == 0) %>% 
  rename(dist = ORIG_FID)
w1_0$dist <- "000"

joined <- left_join(w1_0_df, tr, by = "dist")
library(gifski)
data(NLD_prov)

m1 <- tm_shape(w1_0) + 
  tm_dots(col = c("red", "blue")
          , size = 10,
          breaks = c("dry", "wet"))+
  tm_facets(by = c(free.scales.symbol.size = FALSE, nrow=1,ncol=1))
m1

tmap_animation(m1, delay=40)

#do a join of the data

data(World, metro)

m2 <- tm_shape(World, projection = "+proj=eck4", simplify = 0.5) +
  tm_fill() +
  tm_shape(metro) + 
  tm_bubbles(size = paste0("pop", seq(1970, 2030, by=10)),
             col = "purple",
             border.col = "black", border.alpha = .5,
             scale = 2) +
  tm_facets(free.scales.symbol.size = FALSE, nrow=1,ncol=1) + 
  tm_format("World")

tmap_animation(m2, delay=100, outer.margins = 0)

m4 <- tm_shape(World) +
  tm_polygons() +
  tm_shape(metro) +
  tm_bubbles(col = "red") +
  tm_text("name", ymod = -1) +
  tm_facets(by = "name", free.coords = F, nrow = 1, ncol = 1) +
  tm_layout(panel.show = FALSE, frame = FALSE)

tmap_animation(m4, 
               width=1200, height = 600, fps = 2, outer.margins = 0)

data(NLD_prov)

m1 <- tm_shape(NLD_prov) + 
  tm_polygons("yellow") +
  tm_facets(along = "name")

tmap_animation(m1, delay=40)
