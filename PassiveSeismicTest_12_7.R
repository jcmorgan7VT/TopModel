#try to make h/v contour plot
library(tidyverse)
setwd("C:/ASCII")

h <- read.table("height.dat")
x <- read.table("x.dat")
v <- read.table("logHV.dat")

h1 <- c(h$V1, h$V2, h$V3)
x1 <- c(x$V1, x$V2, x$V3)
v1 <- c(v$V1, v$V2, v$V3)

data <- data.frame(h1, x1, v1)
colnames(data) <- c("height", "dist", "value")
plot(data)

data <- na.omit(data)
ggplot(data, aes(dist, height))+
  geom_contour_filled(aes(z = value))+
  scale_x_continuous(limits = c(0, 4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 0), expand = c(0, 0)) +
  labs(x = "Distance (m)", y = "Estimated Depth (m)",
       title = "H/V contour plot, Tromino Test")+
  theme_classic()+
  guides(fill=guide_legend(title="log10 H/V"))
