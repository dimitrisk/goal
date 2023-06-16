## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

## -----------------------------------------------------------------------------
library(devtools)
#devtools::install_github("dimitrisk/goal", quiet=T)
library(goal)
#library(osmdata)
library(raster)

## -----------------------------------------------------------------------------
am = osm.getPOI(inPerioxi = "Mytilene Municipal Unit", inkey = "amenity")
am

## -----------------------------------------------------------------------------
myt = osm.getPOI_usingbb( c(26.547303,39.101658,26.564641,39.113247), inkey ="amenity" )
myt

## -----------------------------------------------------------------------------
amenities = osm.combineAmenities(am)
amenities

## -----------------------------------------------------------------------------
freq1 = osm.getFrequency(amenities, inword = "amenity", removeNA = F)
freq1

## -----------------------------------------------------------------------------
sh = osm.getPOI(inPerioxi = "Mytilene Municipal Unit", inkey = "shop")
sh

#' Merge all shops (Points, Polygons, Multipolygons), and convert them to points.
shops = osm.combineShops(sh)
shops

## -----------------------------------------------------------------------------
freq2 = osm.getFrequency(shops, inword = "shop", removeNA = F)
freq2

## -----------------------------------------------------------------------------
r = osm.CreateEmptyRaster(inPerioxi = "Mytilene Municipal Unit")

#' Get polygon of the area
mypol = osmdata::getbb("Mytilene Municipal Unit", format_out = "sf_polygon")$multipolygon

## -----------------------------------------------------------------------------
density1 = raster::rasterize(shops, r, field=1, fun=sum) # sum
plot(density1)
plot(mypol, add=T)

## -----------------------------------------------------------------------------
density2 = raster::rasterize(amenities, r, field=1, fun=sum) # sum
plot(density2)
plot(mypol, add=T)

## -----------------------------------------------------------------------------
presense1 = raster::rasterize(shops, r, field=1) # presense
plot(presense1)
plot(mypol, add=T)

## -----------------------------------------------------------------------------
presense2 = raster::rasterize(amenities, r, field=1) # presense
plot(presense2)
plot(mypol, add=T)

## ----message=F, warn=F--------------------------------------------------------
#' web map1
library(leaflet)
leaflet(amenities) %>% addTiles() %>%  leaflet::addMarkers(popup = ~as.character(amenity), label = ~as.character(amenity))

#' web map2
library(simplevis)
library(dplyr)
library(palmerpenguins)
library(stars)
simplevis::leaf_sf_col(amenities,  col_var = amenity)

## -----------------------------------------------------------------------------
am = osm.getPOI_usingbb( c(26.547303,39.101658,26.564641,39.113247), inkey ="amenity" ) # Download
mypol_bb = osm.osmdata_result_2_bbox_pol(am) # get polygon of this bounding box.
amenities = osm.combineAmenities(am) # combine all amenities into points.

## -----------------------------------------------------------------------------
mypol2 = osmdata::getbb("Mytilene Municipal Unit", format_out = "sf_polygon")$multipolygon

## -----------------------------------------------------------------------------
q=c(26.545029,39.088569,26.570177,39.116810)
poly = goal::osm.bb_2_pol(inVec=q, outcrs=2100) 
#plot(poly)

## -----------------------------------------------------------------------------
plot(mypol2, border="red")
plot(mypol_bb, add=T)
plot(amenities, add=T)

## -----------------------------------------------------------------------------
plot(mypol_bb )
plot(mypol2, border="red", add=T)
plot(amenities, add=T)

## -----------------------------------------------------------------------------
clip1 = sf::st_intersection(mypol_bb,mypol2)

## -----------------------------------------------------------------------------
plot(clip1)
plot(mypol2, border="red", add=T)
plot(amenities, add=T)

## -----------------------------------------------------------------------------
r = raster(clip1 )

## -----------------------------------------------------------------------------
density1 = raster::rasterize(amenities, r, field=1, fun=sum) # sum
plot(density1)
plot(clip1, add=T)
plot(as_Spatial(amenities), add=T, cex=0.5)

## -----------------------------------------------------------------------------
myr1 = osm.getRoads(inPerioxi = "Mytilene Municipal Unit", 
                    withBB=FALSE, outcrs = 2100)
myr1
plot(myr1, col="blue", main="sfnetworks Roads (by municipality name)")

## -----------------------------------------------------------------------------
myr2 = osm.getRoads(incoordinates = c(26.545029,39.088569,26.570177,39.116810), 
                    withBB=TRUE, outcrs = 2100 )
myr2
plot(myr2, col="grey", main="sfnetworks Roads (by bbox)")

## -----------------------------------------------------------------------------
q=c(26.545029,39.088569,26.570177,39.116810)
net2 = osm.getRoads(q, withBB=TRUE, outcrs=4326)
poly = osm.bb_2_pol(q, outcrs =  4326)
 
net3 = osm.ClipSFnetwork_with_poly(net2, poly)

plot(net3,col="grey", main="Clipped sfnetwork")
plot(poly,add=T)

