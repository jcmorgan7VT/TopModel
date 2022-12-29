# Load the necessary libraries
library(tidyverse)

# Load the Topmodel data
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


# Define the Topmodel parameters
k <- 0.05
a <- 0.2
b <- 0.3
z <- 3

# Define the Topmodel function
topmodel <- function(data, k, a, b, z, E) {
  # Calculate the storage capacity of the soil
  S <- a * z + b
  
  # Calculate the excess rainfall
  P <- data - k * z
  P[P < 0] <- 0
  
  # Calculate the net infiltration
  I <- P - E
  I[I < 0] <- 0
  
  # Calculate the amount of water that flows into the groundwater
  GW <- I - S
  GW[GW < 0] <- 0
  
  # Return the results as a data frame
  return(data.frame(P, I, S, GW))
}

# Run the Topmodel function on the data and print the results to the console
results <- topmodel(rain2$Precip, k, a, b, z, PET)
head(results)

plot(q2$date, results$GW, type = 'l')
points(q2$date, q2$q_total, type = 'l', col = 'green')
