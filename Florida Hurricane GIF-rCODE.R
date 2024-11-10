install.packages('raster')
install.packages('sf')
install.packages('showtext')
install.packages("sysfonts")
install.packages("ggplot2")
install.packages("jsonlite")

library(raster)
library(sf)
library(showtext)
library(sysfonts)
library(ggplot2)
#Loading the TIFF file

tiffDirectory <- "/Users/godsentizinyon/Downloads/make-the-chart-animated-map/data/nws_precip/"
tiffFilenames <- list.files(tiffDirectory)
precip <- raster(paste0(tiffDirectory, tiffFilenames[1]))

#Loading State boundaries
#Source : http://datafl.ws/7o5

states <- st_read('/Users/godsentizinyon/Downloads/make-the-chart-animated-map/data/cb_2023_us_state_20m/cb_2023_us_state_20m.shp')

#Subsetting for just Florida and the Contiguous United States
florida <- states[states$STATEFP == "12",]
florida <- st_transform(florida, proj4string(precip))


#Contiguous

statesContig <- states[!(states$STATEFP %in% c("02", "15", "72")),]

statesContig <- st_transform(statesContig,proj4string(precip))


# Set 0 values or less to NA
precip[precip <= 0] <- NA

plot(precip)


#We want to setup a blank plotting space that covers the boundary limits of Florida:

#Blank place
par(mar=c(1,1,1,1), oma = c(0,0,0,0))
plot(st_geometry(florida), border = NA)

summary(precip)
range(values(precip), na.rm = TRUE)

#breakvals <- seq(0, 3, by = 0.5)  # Covers the range 0 to 3
#shades <- colorRampPalette(c("lemonchiffon", "lightblue", "blue"))(length(breakvals) - 1)

#cat("Length of breakvals:", length(breakvals), "\n")
#cat("Length of shades:", length(shades), "\n")


# We add precipitation data with plot(). The breaks and cols values define the color scale, based on inches of rain in the past hou
shades <- c('#f4f4f4', '#e2d3dc', '#e4b0a0', '#e58a52')
breakvals <- c(0, .1, .3, 2, 9999)

plot(precip,
     breaks = breakvals,
     col= shades,
     axes= FALSE,
     legend= FALSE,
     add= TRUE)

#Adding State borders:
#Borders

plot(st_geometry(statesContig), asp=1, add=TRUE)

places <- read.csv('/Users/godsentizinyon/Downloads/make-the-chart-animated-map/data/places-of-interest.csv')

places

# Project points
places_sf <- st_as_sf(places,
                      coords = c("longitude", "latitude"),
                      crs = "+proj=longlat")

places_sf_proj <- st_transform(places_sf, proj4string(precip))

font_add("inconsolata", "/Users/godsentizinyon/Downloads/Inconsolata/Inconsolata-Regular.ttf")
showtext_auto()

points(st_coordinates(places_sf_proj), pch=19)
text(st_coordinates(places_sf_proj),
     labels = places_sf_proj$city,
     pos = places_sf_proj$label_pos,
     cex = 1,
     family = "inconsolata")


# Date and time
frames_so_far <- 1
for (fn in tiffFilenames) {
  
  # Load TIFF file
  precip <- raster(paste0(tiffDirectory, fn))
  
  # Set 0 values or less to NA
  precip[precip <= 0] <- NA
  
  png(paste0("/Users/godsentizinyon/Downloads/make-the-chart-animated-map/frames/frame", frames_so_far, ".png"), 
      width = 800, height = 600)
  
  # Blank place
  par(mar=c(1,1,1,1), oma=c(0,0,0,0))
  plot(st_geometry(florida), border=NA)
  
  # Plot TIFF data
  plot(precip, 
       breaks=breakvals,
       col = shades,
       # col=rev(shades),
       # xlim=fl_bbox[c(1,3)],
       # xlim = c(2011684, 3351273),
       # ylim=fl_bbox[c(2,4)], 
       axes=FALSE, legend=FALSE, 
       add=TRUE)
  
  # Borders
  plot(st_geometry(statesContig), asp=1, add=TRUE)
  
  # Places
  points(st_coordinates(places_sf_proj), pch=19)
  text(st_coordinates(places_sf_proj),
       labels = places_sf_proj$city, 
       pos = places_sf_proj$label_pos,
       cex = 1,
       family = "inconsolata")
  
  # Date and time
  frameDT <- strptime(substr(fn, nchar(fn)-18, nchar(fn)-4), 
                      format="%Y%m%d_%H%M%S", 
                      tz="America/New_York")
  text(2050000, -6800000, 
       toupper( format(frameDT, "%B %e") ), 
       pos = 4, cex = 1.25,
       family = "inconsolata")
  text(2050000, -6850000,
       paste0(as.numeric(format(frameDT, "%I")), 
              ":00", 
              tolower(format(frameDT, "%p"))),
       pos = 4, cex = 2.75,
       family = "inconsolata")
  
  
  # Legend
  legwidth <- 10000
  legheight <- 25000
  rect(xleft = rep(2064000, length(shades)),
       xright = rep(2064000, length(shades))+legwidth, 
       ybottom = -7060000 + 1:length(shades)*legheight,
       ytop = -7060000 + 1:length(shades)*legheight+legheight,
       col=shades
  )
  text(x = rep(2062000, length(shades))+legwidth,
       y = -7060000 + 1:length(shades)*legheight + legheight/3+2000, 
       labels = c("Light", "Moderate", "Heavy", "Violent rain"),
       pos = 4, family = "inconsolata")
  
  
  dev.off()
  
  
  frames_so_far <- frames_so_far + 1
}



