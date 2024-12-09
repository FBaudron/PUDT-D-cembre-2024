#' ---
#' title: "Extraction données spatiales"
#' author: "Frédéric Baudron"
#' date: "November 18th, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(terra)
library(geodata)
library(dplyr)


# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\PUDT\\Enquête village\\")

data = read.xlsx("Villages avec esquisses - complete.xlsx", sheet = 1)

# LOAD RASTERS------------------------------------------------------------------

# Country shapefile

cog0 = gadm(country = 'COG', level = 0, path = 'D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions')
cog1 = gadm(country = 'COG', level = 1, path = 'D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions')
cog2 = gadm(country = 'COG', level = 2, path = 'D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\admin-regions')

# plot(cog0)
# plot(cog1)
# plot(cog2)


# Elevation

elevation = geodata::elevation_30s("COG", path = 'D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\elevation')
elevation = terra::crop(elevation, cog0)
elevation = terra::mask(elevation, cog0)
names(elevation) = 'elevation'
# png("Elevation.png", units="in", width=7, height=7.5, res=1000)
par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,1), cex.main=1.95, cex.axis=1.6)
plot(cog0, col='lightgrey', main = 'Altitude (m.a.s.l.)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(elevation, 
     legend=TRUE, axes=F, add=T)
plot(cog1, axes=F, add=T)
points(15.2813, -4.2744, pch=21, bg='orangered', cex=2)
points(11.8664, -4.7692, pch=21, bg='orangered', cex=2)
# dev.off()


# Total rainfall

rain = terra::rast('D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\chirps-rainfall\\longterm-mean-yearly-rainfall.tif')
rain = terra::crop(rain, cog0)
rain = terra::mask(rain, cog0)
rain = terra::resample(rain, elevation)
names(rain) = 'rain'
# png("Total rainfall.png", units="in", width=7, height=7.5, res=1000)
par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,1), cex.main=1.95, cex.axis=1.6)
plot(cog0, col='lightgrey', main = 'Pluviométrie totale (mm/an)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(rain,
     legend=TRUE, axes=F, add=T)
plot(cog1, axes=F, add=T)
points(15.2813, -4.2744, pch=21, bg='orangered', cex=2)
points(11.8664, -4.7692, pch=21, bg='orangered', cex=2)
# dev.off()


# Distance à la route

route = terra::rast('D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\road\\Distance_route_OSM.tif')
route = terra::crop(route, cog0)
route = terra::mask(route, cog0)
route = terra::resample(route, elevation)
names(route) = 'route'
# png("Distance to market.png", units="in", width=7, height=7.5, res=1000)
pal = colorRampPalette(c('wheat', 'orange', 'orangered', 'red'))
par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,1), cex.main=1.95, cex.axis=1.6)
plot(cog0, col='lightgrey', main = 'Distance à la route (km)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(route, 
     col=pal(50), 
     legend=TRUE, axes=F, add=T)
plot(cog1, axes=F, add=T)
points(15.2813, -4.2744, pch=21, bg='orangered', cex=2)
points(11.8664, -4.7692, pch=21, bg='orangered', cex=2)
# dev.off()


# Soc 

soc_5 = terra::rast('D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\isda-soil\\soil_af\\af_soc_0-5cm_30s.tif')
soc_15 = terra::rast('D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\isda-soil\\soil_af\\af_soc_5-15cm_30s.tif')
soc_30 = terra::rast('D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\isda-soil\\soil_af\\af_soc_15-30cm_30s.tif')
soc = (soc_5 * 5 + soc_15 * 10 + soc_30 * 15) / (5 + 10 + 15)
soc = terra::crop(soc, cog0)
soc = terra::mask(soc, cog0)
soc = terra::resample(soc, elevation)
names(soc) = 'soc'
# png("Soil organic carbon.png", units="in", width=7, height=7.5, res=1000)
pal = colorRampPalette(c('wheat', 'orange', 'orangered', 'red'))
par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,1), cex.main=1.95, cex.axis=1.6)
plot(cog0, col='lightgrey', main = 'Carboen organique du sol (g/kg 0-30 cm profondeur)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(soc, 
     col=pal(50), legend=TRUE, axes=F, add=T)
plot(cog1, axes=F, add=T)
points(15.2813, -4.2744, pch=21, bg='orangered', cex=2)
points(11.8664, -4.7692, pch=21, bg='orangered', cex=2)
# dev.off()


# texture

sand_5 = terra::rast('D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\isda-soil\\soil_af\\af_sand_0-5cm_30s.tif')
sand_15 = terra::rast('D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\isda-soil\\soil_af\\af_sand_5-15cm_30s.tif')
sand_30 = terra::rast('D:\\Mes Donnees\\1. Cirad\\PUDT\\Geo\\input-data\\isda-soil\\soil_af\\af_acid-exch_15-30cm_30s.tif')
sand = (sand_5 * 5 + sand_15 * 10 + sand_30 * 15) / (5 + 10 + 15)
sand = terra::crop(sand, cog0)
sand = terra::mask(sand, cog0)
sand = terra::resample(sand, elevation)
names(sand) = 'sand'
# png("Sand.png", units="in", width=7, height=7.5, res=1000)
pal = colorRampPalette(c('wheat', 'orange', 'orangered', 'red'))
par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,1), cex.main=1.95, cex.axis=1.6)
plot(cog0, col='lightgrey', main = 'Sable (%, 0-30 cm profondeur)', panel.first=grid(col="gray", lty="solid"), pax=list(sides=1:2, cex.axis=1.5))
plot(sand, 
     col=pal(50), legend=TRUE, axes=F, add=T)
plot(cog1, axes=F, add=T)
points(15.2813, -4.2744, pch=21, bg='orangered', cex=2)
points(11.8664, -4.7692, pch=21, bg='orangered', cex=2)
# dev.off()


stacked = c(elevation, rain, route, soc, sand)


data_spatial = data %>%
  sf::st_as_sf(coords = c("_Coordonnées.GPS_longitude", "_Coordonnées.GPS_latitude")) %>%
  sf::st_set_crs(4326)


data = cbind(data, terra::extract(stacked, terra::vect(data_spatial)))

# write.xlsx(data, "Enquête village avec données spatiales.xlsx")

# terra::writeRaster(soc, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Enquête village\\rasters\\soc_0-30cm.tiff", overwrite = T)
# terra::writeRaster(cec, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Enquête village\\rasters\\cec_0-30cm.tiff", overwrite = T)
# terra::writeRaster(ph, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Enquête village\\rasters\\ph_0-30cm.tiff", overwrite = T)
# terra::writeRaster(acid_exch, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Enquête village\\rasters\\acid_exch_0-30cm.tiff", overwrite = T)
# terra::writeRaster(sand, "D:\\Mes Donnees\\1. Cirad\\PUDT\\Enquête village\\rasters\\sand_0-30cm.tiff", overwrite = T)




