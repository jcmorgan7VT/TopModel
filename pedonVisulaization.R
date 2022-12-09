#testing out soil visualizations
library(pacman)
p_load(soilDB, aqp, sharpshootR, munsell)

# load sample data from the soilDB package
data(loafercreek, package = "soilDB")
# graphical check
par(mar = c(0, 0, 0, 0))
plot(loafercreek, name = "", print.id = FALSE, cex.names = 0.8, axis.line.offset = -4, max.depth = 100)



# slice color data at select depths
s <- slice(loafercreek, c(5, 10, 15, 25, 50, 75) ~ soil_color, strict = FALSE)

# make horizon labels based on slice depth
s$slice <- paste0(s$hzdept, " cm")
s$slice <- factor(s$slice, levels = guessGenHzLevels(s, "slice")$levels)

par(mar = c(4.5, 2.5, 4.5, 0))
aggregateColorPlot(aggregateColor(s, "slice"), label.cex = 0.65, main = "Loafercreek Dry Colors\nDepth Slices", 
                   print.n.hz = TRUE)



# get data from NASIS or similar source
f <- fetchNASIS(rmHzErrors = TRUE)

# an ordered set of series names
soils <- c("ahwahnee", "auberry", "musick", "holland", "shaver", "chaix", "canisrocks")

# extract these soils and normalize taxonname
f.sub <- f[grep(paste0(soils, collapse = "|"), f$taxonname, ignore.case = TRUE), ]
for (x in soils) {
  f.sub$taxonname[grep(x, f.sub$taxonname, ignore.case = TRUE)] <- x
}
# reset levels to order specified above
f.sub$taxonname <- factor(f.sub$taxonname, levels = soils)

par(mar = c(4.5, 5, 4.5, 0))
aggregateColorPlot(aggregateColor(f.sub, "taxonname"), label.cex = 0.65, main = "Soil Color Signatures", 
                   print.n.hz = TRUE, rect.border = NA, print.label = FALSE, horizontal.borders = TRUE)



# load sample data set, a data.frame object with horizon-level data from 10 profiles
data(sp4)
str(sp4)

depths(sp4) <- id ~ top + bottom
class(sp4)

soil.df <- read.csv("soil.csv")

soil <- read.csv("soil.csv")
soil$hzname <- soil$Horizon
soil$soil_color <- mnsl(soil$Color, fix = TRUE)
soil$hzname <- soil$Horizon
soil$soilorder <- soil$soil_name
depths(soil) <- Soil_abrev ~ Top + Bottom

#add names of soil columns as site level data
t <- select(soil.df, Soil_abrev, Soil_name)
t2 <- unique(t[order(t$Soil_abrev),])

site(soil) <- t2$Soil_name

par(mar = c(0,2,0,4), xpd = NA)
plotSPC(soil, cex.names = 1)

x <- fetchOSD(c('appling', 'cecil', 'bonneau'))

plotSPC(soil, cex.names = 1, name.style = 'center-center', 
        width = 0.3, plot.depth.axis = FALSE, hz.depths = TRUE, 
        fixLabelCollisions = TRUE, hz.depths.offset = 0.08)

par(mar = c(0,2,0,4), xpd = NA)

plotSPC(soil, cex.names = 1, axis.line.offset = -0.1, width = 0.3,
        hz.depths = TRUE,
        name.style = "right-center")

par(mar = c(0, 0, 0, 1), bg = 'black', fg = 'white')
plotSPC(soil, width = 0.3, cex.id = 0.66, cex.names = 0.55,
        alt.label = "Soil_abrev", 
        #name = 'hzname', 
        title = "Example soil horizons from Bailey et al. 2014")

idname(soil)
