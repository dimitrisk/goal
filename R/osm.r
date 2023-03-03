

# ========================   Helper Functions   ========================================

# 
#' @title osm.getRoads
#' @description Get roads of a city
#' of Spatial Feature ("sf") type. 
#' 
#' @param inPerioxi  ssd
#' @return A nice 
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, network
#' @family osm
#' @importFrom dplyr %>%
#' @examples library(goal)
#' 
osm.getRoads = function(inPerioxi="Mytilene Municipal Unit"){
  myt = osmdata::opq("Mytilene Municipal Unit") %>% 
    osmdata::add_osm_feature(
      key = 'highway',
      value = c("trunk", "trunk_link", "primary","primary_link","secondary", "secondary_link", 
                "tertiary","tertiary_link", "residential", "unclassified")) %>% 
    osmdata::osmdata_sf(quiet = FALSE) %>% osmdata::osm_poly2line()
  return(myt)
}


#' @title osm.getPOI
#' @description Get all POIs of a city. Providing just the key (e.x. amenities, shop, ...) we get back an osmdata object
#' of Spatial Feature ("sf") type. 
#' 
#' @param inPerioxi  a perioxi
#' @param inkey  name of the key
#' @return A nice 
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, frequency, POIs
#' @family osm
#' @importFrom dplyr %>%
#' @examples library(goal)
#' 
osm.getPOI = function(inPerioxi="Mytilene Municipal Unit", inkey ="amenity"){
  q <- osmdata::getbb(inPerioxi) %>% osmdata::opq() %>%
    osmdata::add_osm_feature(key =inkey) %>% osmdata::osmdata_sf(quiet = F )
  return(q)
}

#' @title osm.combineAmenities
#' @description Combine all AMENITIES results into single dataset (Points, Polygons, Myltipolygons) (centroids).
#' 
#' @param inam Amenities
#' @return A nice 
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, frequency, POIs
#' @family osm
#' @importFrom dplyr %>%
#' @examples library(goal)
#' 
osm.combineAmenities = function(inam){
  #TODO: osm_id, name, amenity ??
  #TODO: check if incoming is actually of type OSMresult dataset.
  converted_pol = inam$osm_polygons %>% sf::st_centroid() %>% dplyr::select(osm_id, name, amenity)%>% 
    sf::st_transform("+init=epsg:4326") %>% dplyr::mutate(geotype="frompolygon")
  converted_points = inam$osm_points %>% dplyr::select(osm_id, name, amenity)%>% 
    sf::st_transform("+init=epsg:4326")%>% dplyr::mutate(geotype="frompoint")
  converted_multipol = inam$osm_multipolygons %>% dplyr::select(osm_id, name, amenity)%>% 
    sf::st_transform("+init=epsg:4326")%>% dplyr::mutate(geotype="frommultipolygon")
  
  ola = dplyr::bind_rows(converted_pol, converted_points, converted_multipol) %>% sf::st_sf() %>% sf::st_cast('POINT')
  return(ola)
}



#' @title osm.combineShops
#' @description Combine all Shops results into single dataset. It merges `Points`, `Polygons` and `Myltipolygons` into a
#' single dataset of Points using centroids of polygons when necessary.
#' 
#' @param inam a
#' @return A nice 
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, frequency, POIs
#' @family osm
#' @importFrom dplyr %>%
#' @examples library(goal)
#' 
osm.combineShops = function(inam){
  #TODO: osm_id, name, shop ??
  #TODO: check if incoming is actually of type OSMresult dataset.
  
  converted_pol = inam$osm_polygons %>% sf::st_centroid() %>% dplyr::select(osm_id, name, shop)%>% 
    sf::st_transform("+init=epsg:4326") %>% dplyr::mutate(geotype="frompolygon")
  converted_points = inam$osm_points %>% dplyr::select(osm_id, name, shop)%>% 
    sf::st_transform("+init=epsg:4326")%>% dplyr::mutate(geotype="frompoint")
  converted_multipol = inam$osm_multipolygons %>% dplyr::select(osm_id, name, shop)%>% 
    sf::st_transform("+init=epsg:4326")%>% dplyr::mutate(geotype="frommultipolygon")
  
  ola = rbind(converted_pol, converted_points, converted_multipol)%>% sf::st_sf()%>%  sf::st_cast('POINT')
  return(ola)
}


#' @title osm.getFrequency
#' @description Get Frequency Table
#' 
#' @param indf The vec 
#' @param inword Boole 
#' @param removeNA Ad 
#' @return A nice histogram of a single variable with an additional Cumulative Density curve.
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, frequency, POIs
#' @family osm
#' @importFrom dplyr %>%
#' @examples library(goal)
#' 
osm.getFrequency = function(indf, inword="shop", removeNA=T){
  #indf=shops
  #inword="shops"
  #TODO: check if incoming indf is actualy a DataFrame dataset.
  
  FreqPois = indf %>% dplyr::group_by( get(inword) ) %>% dplyr::tally() %>%  dplyr::mutate(freq = 100*prop.table(n)) %>% 
    dplyr::arrange(desc(n)) %>% sf::st_drop_geometry()
  #FreqPois
  names(FreqPois)[1]=inword
  return(FreqPois)
}



#' @title osm.CreateEmptyRaster
#' @description Create an empty raster for an area. It downloads the area from OSM services, and then creates an empty raster of this area.
#'
#' @param inPerioxi The name of an area (string) for searching the OSM services.
#'
#' @return An empty raster that covers the full area
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, raster
#' @family osm
#' @importFrom dplyr %>%
#' @importFrom devtools install_github
#' @examples library(devtools)
#' install_github("dimitrisk/goal")
#' library(goal)
#' result = goal::osm.CreateEmptyRaster(inPerioxi="Mytilene Municipal Unit")
#' result
osm.CreateEmptyRaster = function(inPerioxi="Mytilene Municipal Unit"){
  bb = osmdata::getbb(inPerioxi)
  #bb = osmdata::getbb("Mytilene Municipal Unit")
  #class(bb)
  #TODO: check if incoming is actually of type OSMresult dataset.
  r = raster::raster(xmn=bb[1],xmx=bb[3],ymn=bb[2],ymx=bb[4]  )
  return(r)
}