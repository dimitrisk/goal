## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

## -----------------------------------------------------------------------------
library(devtools)
#devtools::install_github("dimitrisk/goal", quiet=T)
library(goal)
#library(osmdata)
library(raster)
library(sf)
library(tmaptools)
library(leaflet)
library(dplyr)
library(simplevis)

## -----------------------------------------------------------------------------
am = goal::osm.getPOI(inPerioxi = "Mytilene Municipal Unit", inkey = "amenity")
am

## -----------------------------------------------------------------------------
myt = goal::osm.getPOI_usingbb( c(26.547303,39.101658,26.564641,39.113247), inkey ="amenity" )
myt

## -----------------------------------------------------------------------------
amenities = goal::osm.combineAmenities(am)
amenities

## -----------------------------------------------------------------------------
freq1 = goal::osm.getFrequency(amenities, inword = "amenity", removeNA = F)
freq1

## -----------------------------------------------------------------------------
sh = goal::osm.getPOI(inPerioxi = "Mytilene Municipal Unit", inkey = "shop")
sh

#' Merge all shops (Points, Polygons, Multipolygons), and convert them to points.
shops = goal::osm.combineShops(sh)
shops

## -----------------------------------------------------------------------------
freq2 = goal::osm.getFrequency(shops, inword = "shop", removeNA = F)
freq2

## -----------------------------------------------------------------------------
r = goal::osm.CreateEmptyRaster(inPerioxi = "Mytilene Municipal Unit")

#' Get polygon of the area
#mypol = osmdata::getbb("Mytilene Municipal Unit", format_out = "sf_polygon")$multipolygon
mypol = osmdata::getbb("Mytilene Municipal Unit") %>% bb_poly() %>% sf::st_set_crs(st_crs(shops))  

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

leaflet(amenities) %>% addTiles() %>%  addMarkers(popup = ~as.character(amenity), label = ~as.character(amenity))

#' web map2
#library(simplevis)

#library(palmerpenguins)
#library(stars)
leaf_sf_col(amenities,  col_var = amenity)

## -----------------------------------------------------------------------------
am = goal::osm.getPOI_usingbb( c(26.547303,39.101658,26.564641,39.113247), inkey ="amenity" ) # Download amenities in this area
mypol_bb = goal::osm.osmdata_result_2_bbox_pol(am) %>% st_set_crs(st_crs(shops))  # get polygon of this bounding box.
amenities = goal::osm.combineAmenities(am) # combine all amenities into points.

## -----------------------------------------------------------------------------
#mypol2 = osmdata::getbb("Mytilene Municipal Unit", format_out = "sf_polygon")$multipolygon
mypol2 = osmdata::getbb("Mytilene Municipal Unit")  %>% bb_poly() %>% sf::st_set_crs(st_crs(shops))  

## -----------------------------------------------------------------------------
q=c(26.545029,39.088569,26.570177,39.116810)
poly = goal::osm.bb_2_pol(inVec=q, outcrs=2100)  # outcrs=4326)
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
clip1 = sf::st_intersection( mypol_bb, mypol2 )

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
plot(sf::as_Spatial(amenities), add=T, cex=0.5)

## -----------------------------------------------------------------------------
myr1 = goal::osm.getRoads(inPerioxi = "Mytilene Municipal Unit", 
                    withBB=FALSE, outcrs = 2100)
myr1
plot(myr1, col="blue", main="sfnetworks Roads (by municipality name)")

## -----------------------------------------------------------------------------
myr2 = goal::osm.getRoads(incoordinates = c(26.545029,39.088569,26.570177,39.116810), 
                    withBB=TRUE, outcrs = 2100 )
myr2
plot(myr2, col="grey", main="sfnetworks Roads (by bbox)")

## -----------------------------------------------------------------------------
q=c(26.545029,39.088569,26.570177,39.116810)
net2 = goal::osm.getRoads(q, withBB=TRUE, outcrs=4326)
poly = goal::osm.bb_2_pol(q, outcrs =  4326)
 
net3 = goal::osm.ClipSFnetwork_with_poly(net2, poly)

plot(net3,col="grey", main="Clipped sfnetwork")
plot(poly,add=T)

## -----------------------------------------------------------------------------
library(goal)
mylength = goal::osm.getLength_footway( place="Mytilene Municipal Unit" )
mylength

