

# ========================   Helper Functions   ========================================



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




#' @title osm.getPOI_usingbb
#' @description Get all POIs of a city. Providing the bounding box
#' we get back an osmdata object of Spatial Feature ("sf") type.
#'
#' @param inbb A vector of coordinates, representing the bounding box of the area.
#' @param inkey The name of a key such as: `amenities`, `shops`, ...
#' @return An `osmdata` object of Spatial Feature ("sf") type.
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, frequency, POIs
#' @family osm
#' @importFrom dplyr %>%
#' @examples library(goal)
#' myt = osm.getPOI_usingbb( c(26.547303,39.101658,26.564641,39.113247) )
#' myt
osm.getPOI_usingbb = function(inbb=c(26.547303,39.101658,26.564641,39.113247) , inkey ="amenity"){
  q <- osmdata::opq(bbox=inbb) %>% osmdata::add_osm_feature(key=inkey) %>% osmdata::osmdata_sf(quiet=F)
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
#' am = osm.getPOI(inPerioxi = "Mytilene Municipal Unit", inkey = "amenity")
#' amenities = osm.combineAmenities(am)
#' amenities
osm.combineAmenities = function(inam){

  if(!"osmdata" %in% class(inam)){    stop("This is not 'osmdata' object.")  }

  # inam = test
  if(!is.null(inam$osm_polygons) ){
    converted_pol = inam$osm_polygons %>%  sf::st_centroid() %>% sf::st_transform("+init=epsg:4326")%>% dplyr::mutate(geotype = "frompolygon")
    if("name" %in% names(converted_pol)){
      converted_pol = converted_pol %>% dplyr::select(osm_id, name, amenity, geotype)
    }else{
      converted_pol = converted_pol %>% dplyr::select(osm_id,  amenity, geotype)
    }
  }else{
    converted_pol=NULL
  }


  if(!is.null(inam$osm_points) ){
    converted_points = inam$osm_points %>% sf::st_transform("+init=epsg:4326")%>% dplyr::mutate(geotype = "frompoint")
    if("name" %in% names(converted_points)){
      converted_points = converted_points %>% dplyr::select(osm_id, name, amenity, geotype)
    }else{
      converted_points = converted_points %>% dplyr::select(osm_id,  amenity, geotype)
    }
  }else{
    converted_points=NULL
  }

  if(!is.null(inam$osm_multipolygons) ){
    converted_multipol = inam$osm_multipolygons %>%  sf::st_centroid()%>% sf::st_transform("+init=epsg:4326")%>% dplyr::mutate(geotype = "frommultipolygon")
    if("name" %in% names(converted_multipol)){
      converted_multipol = converted_multipol %>% dplyr::select(osm_id, name, amenity, geotype)
    }else{
      converted_multipol = converted_multipol %>% dplyr::select(osm_id,  amenity, geotype)
    }
  }else{
    converted_multipol=NULL
  }

  if(is.null( converted_pol) & is.null(converted_points) & is.null(converted_multipol)){
    stop("These Amenities are NULL. No Points & No polygons & no multipolygons")
  }else{
    ola = dplyr::bind_rows(converted_pol, converted_points, converted_multipol) %>%
      sf::st_sf() %>% sf::st_cast("POINT")
  }

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
#' sh = osm.getPOI(inPerioxi = "Mytilene Municipal Unit", inkey = "shop")
#' sh
#' shops = osm.combineShops(sh)
#' shops
osm.combineShops = function(inam){

  #inam = this_shops
  #inam=this_shops

  if(!"osmdata" %in% class(inam)){ stop("This is not 'osmdata' object.")  }


  if(!is.null(inam$osm_polygons) ){
    converted_pol = inam$osm_polygons %>%  sf::st_centroid() %>% sf::st_transform("+init=epsg:4326")%>% dplyr::mutate(geotype = "frompolygon")
    if("name" %in% names(converted_pol)){
      converted_pol = converted_pol %>% dplyr::select(osm_id, name, shop, geotype)
    }else{
      converted_pol = converted_pol %>% dplyr::select(osm_id,  shop, geotype)
    }
  }else{
    converted_pol=NULL
  }




  if(!is.null(inam$osm_points) ){
    converted_points = inam$osm_points %>% sf::st_transform("+init=epsg:4326")%>% dplyr::mutate(geotype = "frompoint")
    if("name" %in% names(converted_points)){
      converted_points = converted_points %>% dplyr::select(osm_id, name, shop, geotype)
    }else{
      converted_points = converted_points %>% dplyr::select(osm_id,  shop, geotype)
    }
  }else{
    converted_points=NULL
  }






  if(!is.null(inam$osm_multipolygons) ){
    converted_multipol = inam$osm_multipolygons %>% sf::st_centroid()%>% sf::st_transform("+init=epsg:4326")%>% dplyr::mutate(geotype = "frommultipolygon")
    if("name" %in% names(converted_multipol)){
      converted_multipol = converted_multipol %>% dplyr::select(osm_id, name, shop, geotype)
    }else{
      converted_multipol = converted_multipol %>% dplyr::select(osm_id,  shop, geotype)
    }
  }else{
    converted_multipol=NULL
  }





  if(is.null( converted_pol) & is.null(converted_points) & is.null(converted_multipol)){
    stop("These Shops results are NULL. No Points & No polygons & no multipolygons")
  }else{
    ola = rbind(converted_pol, converted_points, converted_multipol)%>% sf::st_sf()%>%  sf::st_cast('POINT')
  }


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
#' sh = osm.getPOI(inPerioxi = "Mytilene Municipal Unit", inkey = "shop")
#' shops = osm.combineShops(sh)
#' freq2 = osm.getFrequency(shops, inword = "shop", removeNA = F)
#' freq2
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
#' #install_github("dimitrisk/goal")
#' library(raster)
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


#' @title osm.myBB_2_pol
#' @description Create a polygon from a BB object
#'
#' @param n the top north latitude
#' @param s the bottom south latitude
#' @param w the most western longitude
#' @param e the most eastern longitude
#' @param bbox either a bounding box
#' @param proj4string either a projection
#'
#' @return An polygon
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap
#' @family osm
#' @importFrom dplyr %>%
#' @importFrom devtools install_github
#' @examples library(devtools)
#' #install_github("dimitrisk/goal")
#' library(raster)
#' library(goal)
#' bb = c(38.1 , 26.5 , 39.05 , 27.7)
#' result = goal::osm.myBB_2_pol(n =bb[3], s =bb[1], w =bb[4], e =bb[2])
#' result
osm.myBB_2_pol = function (n, s, w, e, bbox = NA, proj4string = sp::CRS("+init=epsg:4326")){
  if (all(!is.na(bbox), is.matrix(bbox), dim(bbox) == c(2,2))) {
    n <- bbox[2, 2]
    s <- bbox[2, 1]
    w <- bbox[1, 1]
    e <- bbox[1, 2]
  }
  box1 <- matrix(c(w, s, w, n, e, n, e, s, w, s), ncol = 2, byrow = TRUE)
  box1 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(box1)), ID = 1)), proj4string = proj4string)
  return(box1)
}


#' @title osm.osmdata_result_2_bbox_pol
#' @description Get the bbox of a result, as an SF polygon.
#'
#' @param osmdata_result An `osmdata` result.
#'
#' @return An SF polygon which is the bbox of the `osmdata` result.
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, bbox, polygon
#' @family osm
#' @importFrom dplyr %>%
#' @importFrom stringr str_split
#' @importFrom devtools install_github
#' @examples library(devtools)
#' #install_github("dimitrisk/goal")
#' library(goal)
#' library(sf)
#' library(stringr)
#' am = goal::osm.getPOI_usingbb( c(26.547303,39.101658,26.564641,39.113247), inkey ="amenity" )
#' amenities = goal::osm.combineAmenities(am)
#' thepol = goal::osm.osmdata_result_2_bbox_pol(am)
#'
osm.osmdata_result_2_bbox_pol = function(osmdata_result){
  #osmdata_result = am

  if(!nrow(osmdata_result$osm_points)>0){stop("The 'osmdata_result' used in 'osm.osmdata_result_2_bbox_pol', do not contain any points. I need the points in order to copy their CRS.")}

  bb = osmdata_result$bbox %>% stringr::str_split(",") %>% dplyr::nth(1) %>% as.vector() %>% as.numeric()
  #pol = rgeos::bbox2SP(n =bb[3], s =bb[1], w =bb[4], e =bb[2]) %>% sf::st_as_sf() %>% sf::st_transform(sf::st_crs(osmdata_result$osm_points))
  pol = goal::osm.myBB_2_pol(n =bb[3], s =bb[1], w =bb[4], e =bb[2]) %>% sf::st_as_sf() %>% sf::st_transform(sf::st_crs(osmdata_result$osm_points))
  #pol = bb %>% tmaptools::bb_poly() %>% sf::st_set_crs(sf::st_crs(osmdata_result$osm_points))  #%>% sf::st_transform(sf::st_crs(osmdata_result$osm_points))

  return(pol)
}




#' @title osm.getRoads
#' @description Get the roads of an area.
#' Either by name of area (inPerioxi) or by bounding box (incoordinates).
#'
#' @param incoordinates Four coordinates of the bounding box.
#' @param inPerioxi Name of an area
#' @param withBB Boolean indicating if we shall use a bounding box (incoordinates) or the name of an area (inPerioxi).
#' @param outcrs CRS of the the output
#'
#' @return An sfnetwork
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, bbox, network
#' @family osm
#' @importFrom dplyr %>%
#' @examples library(goal)
#' library(sf)
#' library(sfnetworks)
#' library(osmdata)
#' mynetwork = osm.getRoads(incoordinates= c(26.545029,39.088569,26.570177,39.116810),
#'   withBB=TRUE, outcrs=2100
#' )
#' mynetwork
#'
#' mynetwork2 = osm.getRoads(inPerioxi="Mytilene Municipal Unit",
#'   withBB=FALSE, outcrs=2100
#' )
#' mynetwork2
#'
osm.getRoads = function(incoordinates= c(26.545029,39.088569,26.570177,39.116810),
                        inPerioxi="Mytilene Municipal Unit",
                        withBB=FALSE,
                        outcrs=2100 ){

  if( withBB){
    cat("\nUsing bbox\n")
    myt <- osmdata::opq(bbox=incoordinates) %>%
      osmdata::add_osm_feature(
        key='highway',
        value = c("trunk", "trunk_link", "primary","primary_link","secondary",
                  "secondary_link", "tertiary","tertiary_link","residential",
                  "unclassified")) %>%
      osmdata::osmdata_sf() #%>% osm_poly2line()
  }else{
    cat("\nUsing name of area\n")
    myt <- osmdata::opq(inPerioxi) %>%
      osmdata::add_osm_feature(
        key='highway',
        value = c("trunk", "trunk_link", "primary","primary_link","secondary",
                  "secondary_link", "tertiary","tertiary_link","residential",
                  "unclassified")) %>%
      osmdata::osmdata_sf() #%>% osm_poly2line()
  }


  my_roads = myt$osm_lines %>% dplyr::select(osm_id,name,highway)
  net = sfnetworks::as_sfnetwork(my_roads, directed = FALSE ) #%>%  st_transform(4326)   #%>%  st_transform(2100)

  sf::st_crs(net) <- 4326
  net2 <- sf::st_transform(net, outcrs)
  return(net2)
}







#' @title osm.bb_2_pol
#' @description Bounding Box to Polygon
#' Either by name of area (inPerioxi) or by bounding box (incoordinates).
#'
#' @param inVec Four
#' @param outcrs CRS of the the output
#'
#' @return An sf polygon
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, bbox, network
#' @family osm
#' @importFrom dplyr %>%
#' @examples library(goal)
#' library(sf)
#'
#' q=c(26.545029,39.088569,26.570177,39.116810)
#' poly = goal::osm.bb_2_pol(q, outcrs=4326)
#'
osm.bb_2_pol = function(inVec = c(26.545029,39.088569,26.570177,39.116810), outcrs=4326){
  Poly_Coord_df = data.frame(lon = c(inVec[3],inVec[1]), lat =c(inVec[4],inVec[2]) )
  #result <- Poly_Coord_df %>%  st_as_sf(coords = c("lon", "lat"),  crs = 4326) %>%  st_bbox() %>%  st_as_sfc() %>% st_transform(incrs)
  result = Poly_Coord_df %>%  sf::st_as_sf(coords = c("lon", "lat"),  crs = 4326) %>%
    sf::st_bbox() %>%  sf::st_as_sfc()  %>% sf::st_transform(outcrs)
  #st_crs(result) <- 4326
  return(result)
}


#' @title osm.ClipSFnetwork_with_poly
#' @description Clip sfnetwork by Polygon
#'
#'
#' @param innet sfnetwork to clip
#' @param inpol sf polygon to use for clipping
#'
#' @return An sfnetwork
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, bbox, network, polygon
#' @family osm
#' @importFrom dplyr %>%
#'
#' @examples library(goal)
#' library(sf)
#' library(sfnetworks)
#' library(tidygraph)
#'
#' q=c(26.545029,39.088569,26.570177,39.116810)
#' net2 = osm.getRoads(q, withBB=TRUE, outcrs=4326)
#' poly = osm.bb_2_pol(q, outcrs =  4326)
#'
#' net3 = osm.ClipSFnetwork_with_poly(net2, poly)
#'
#' plot(net3, col="grey")
#' plot(poly, add = TRUE)
osm.ClipSFnetwork_with_poly = function(innet, inpol){
  result = innet %>%
    sfnetworks::activate("edges") %>% sf::st_intersection(inpol) %>%
    sfnetworks::activate("nodes") %>% dplyr::filter(!tidygraph::node_is_isolated())
  return(result)
}





#' @title osm.getLength_footway
#' @description Get length of footway for an area. This is a proxy of walkability score.
#' we get back a number.
#'
#' @param place The name of an area.
#' @return The total length of  footpaths in this area. This may be considered as a proxy for walcability score of this area.
#'
#' @author Dimitris Kavroudakis \email{dimitris123@@gmail.com}
#' @export
#' @keywords openstreetmap, footway, road, walkability, POIs
#' @family osm
#' @importFrom dplyr %>%
#' @examples library(goal)
#' mylength = osm.getLength_footway( place="Mytilene Municipal Unit" )
#' mylength
osm.getLength_footway = function (place="Mytilene Municipal Unit") {

  bb <- osmdata::opq(bbox = place) # Get the bounding box of the place
  query <- osmdata::add_osm_feature(bb, key = 'highway', value = 'footway') # Set up the query
  footpaths <- osmdata::osmdata_sf(query) # Run the query
  footpaths_df <- footpaths$osm_lines # Get the footpaths (lines)

  with_length <- footpaths_df %>%
    dplyr::mutate( length = sf::st_length(footpaths_df$geometry)  ) # Calculate Length


  return( list(
    TotalLength= base::sum(with_length$length),
    MeanLength=base::mean(with_length$length, na.rm=T),
    MedianLength=stats::median(with_length$length, na.rm=T),
    SDLength=stats::sd(with_length$length, na.rm=T),
    VarianceLength=stats::var(with_length$length, na.rm=T),
    QuntilesLength=stats::quantile(with_length$length, c(0,0.25,0.5,0.75,1))
    )

  )
}

#mylength = osm.getLength_footway( place="Mytilene Municipal Unit" )
#mylength
