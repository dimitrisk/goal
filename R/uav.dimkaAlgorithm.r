#library(goal)
#data(goaldata.data2, package ="goal")
#library(sp)
#library(raster)
#one = uav.GenerateTargets(goaldata.data2)
#plot(one$Polygon)
#plot(one$sminos, col="red", add=TRUE)
#plot(one$simia, col="blue", add=TRUE)

#sminos_temp = sp::SpatialPointsDataFrame(goaldata.data2, goaldata.data2, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
#test = sp::spTransform( sminos_temp, CRS( "+init=epsg:2100" ))
#coordinates(test)
#uav.GenerateTargets(xy)
#coordinates(goaldata.data2) <- ~x+y
#raster::crs(goaldata.data2)= CRS( "+proj=longlat +datum=WGS84" )

#is.projected(goaldata.data2)
#is.projected(test)

#test = sp::spTransform( goaldata.data2, CRS( "+init=epsg:2100" ))

#blpi = rgeos::gBuffer(test, width =20 )
#plot(test)
#plot(blpi, add=T)


#library(sqldf)
#library(plotKML)
#library(methods)
#library(ggplot2)
#library(sp)
#library(rgeos)
#library(utils)

#library(goal)
#data(goaldata.data2, package ="goal")
#library(sp)
#library(raster)
#one = goal::uav.GenerateTargets(goaldata.data2)
#plot(one$Polygon)
#plot(one$sminos, col="red", add=TRUE)
#plot(one$simia, col="blue", add=TRUE)

#' uav.GenerateTargets
#'
#' Generate locations for targets (points) inside the study area (polygon) keeping low density of points,
#' covering all parts of the area and following the shape of the study area.
#'
#' @param data2 points of the polygon study area.
#' @param SourceCRS Source CRS of the points
#' @param TargetCRS Target CRS of the points
#' @param ploting Boolean whether to plot the results.
#' @param export Boolean
#' @param dosimplify Boolean whether to simplify the geometry of Polygon of the study area.
#' @param output_folder Path of the export folder
#'
#' @return A list of three objects:
#' \itemize{
#'   \item **Polygon**: A [SpatialPolygonsDataFrame()] of the ConvexHull polygon of the area
#'   \item **simia**: A [SpatialPointsDataFrame()] of the targets.
#'   \item **sminos**: A [SpatialPointsDataFrame()] of the initial data points
#'   \item **lines**: A [SpatialLinesDataFrame()] of the lines used for the calculation
#' }
#' @export
#' @examples library(goal)
#' data(goaldata.data2, package ="goal")
#' library(sp)
#' one = goal::uav.GenerateTargets(goaldata.data2)
#' plot(one$Polygon)
#' plot(one$sminos, col="red", add=TRUE)
#' plot(one$simia, col="blue", add=TRUE)
uav.GenerateTargets = function(data2, SourceCRS="+init=epsg:4326",
                               TargetCRS="+init=epsg:2100",
                               ploting=0, export=0, dosimplify=0, output_folder="Results"){

  #data2=goaldata.data2

  sminos = sp::SpatialPointsDataFrame(data2, data2 , proj4string = sp::CRS(SourceCRS))
  if(!is.projected(sminos)){
    sminos = sp::spTransform( sminos, CRS(TargetCRS))
    cat(sprintf("\nuav.GenerateTargets: reprojecting 'data2' to: '%s'. I continue.\n", TargetCRS))
  }else{
    cat("\nuav.GenerateTargets: 'data2' is projected. I continue.\n")
  }
  #proj4string(sminos) = CRS("+proj=longlat +datum=WGS84")
  #sminos=sp::spTransform( sminos_temp, CRS( "+init=epsg:2100" ))

  pol = rgeos::gConvexHull(sminos)
  nkorifes = nrow(methods::slot(pol@polygons[[1]]@Polygons[[1]], "coords"))

  # Simplify Polygon
  if (dosimplify){
    mytol = 0.0005
    prin = nrow(methods::slot(pol@polygons[[1]]@Polygons[[1]], "coords"))
    pol = rgeos::gSimplify(pol, tol=mytol )
    meta = nrow(methods::slot(pol@polygons[[1]]@Polygons[[1]], "coords"))
    print(paste("dosimplify",prin, "->", meta))
  }

  # Convex Hull
  hull =  rgeos::gConvexHull(pol)
  IDs = sapply(methods::slot(pol, "polygons"), function(x) methods::slot(x, "ID"))
  pol = sp::SpatialPolygonsDataFrame(hull, data.frame(id2=1))

  #hull@data$id2 = 1

  # Generate targets at edges
  co = ggplot2::fortify(pol, region = "id2" )[1:2]
  co = co[-1,] # remove last line

  #if (!setequal(co[1,], co[nrow(co),]) ){
  #  warning("Starting coordinates are not the same as ending coordinates of the polygon")
  #}

  ngonies = nrow(co)

  # Generate diagonials
  combinations = data.frame(t(utils::combn(1:ngonies, 2)))
  combinations$diff = abs(combinations[,1] -combinations[,2]) # find the consequent points (diff==1)

  combinations = combinations[!combinations$diff == 1, ]  #remove consequent points
  combinations = combinations[!combinations$diff == (ngonies-1), ]  #remove first with last connection (because this is a line already in the hull)
  combinations$diff = NULL

  n=nrow(combinations)

  # Line intersections & Koryfes
  l = vector("list", n)
  #results=data.frame()
  for (i in 1:nrow(combinations)){
    row = combinations[i,]
    from_coordinates = co[row[[1]],]
    to_coordinates = co[row[[2]],]
    #results=rbind(results, c(from_coordinates, to_coordinates))
    l[[i]] = sp::Lines(list(sp::Line(rbind(from_coordinates, to_coordinates))), as.character(i))
  }
  #names(results)=c("from_long","from_lat","to_long","to_lat")
  diago = sp::SpatialLines(l, proj4string = CRS(TargetCRS))
  simia_korifes = sp::SpatialPointsDataFrame(data.frame(co), data=data.frame(x=co[,1], y=co[,2]),   proj4string = CRS(TargetCRS))

  r = rgeos::gIntersection(diago, diago, byid=TRUE  )@pointobj
  rc =  data.frame( r@coords[!duplicated(r@coords), ] ) # remove duplicate points from intersection

  # Remove koryfes from coordinates
  jim = data.frame(co)

  if (ngonies>4){
    newrc =   sqldf::sqldf('SELECT * FROM rc EXCEPT SELECT * FROM jim')
  } else{
    newrc =   sqldf::sqldf('SELECT * FROM jim')
    newrc = rbind(newrc, t(rc)[1,])
  }
  simia_diagonion = sp::SpatialPointsDataFrame( newrc , data= data.frame(newrc), proj4string = CRS(TargetCRS) )
  names(simia_diagonion) = c("x","y" )

  #  Polygon Centroids
  lpi = rgeos::gIntersection(pol, diago)         # intersect your line with the polygon
  proj4string(lpi) = sp::proj4string(pol)
  blpi = rgeos::gBuffer(lpi, width = 0.000001 )  # create a very thin polygon  buffer of the intersected line
  #proj4string(blpi) = proj4string(pol)
  dpi = rgeos::gDifference(pol, blpi)            # split using gDifference
  #proj4string(dpi) = proj4string(pol)

  centroids = data.frame()
  for (i in 1:length(dpi@polygons[[1]]@Polygons)){
    this = sp::SpatialPolygons(list(sp::Polygons(list(dpi@polygons[[1]]@Polygons[[i]]), paste(i))))
    center = rgeos::gCentroid(this)
    centroids = rbind(centroids, center@coords)
  }
  centroids=sp::SpatialPointsDataFrame(centroids, data=centroids, proj4string = CRS(TargetCRS))

  # Lines centers
  # Split lines by intersection
  test = rgeos::gIntersection(diago, diago)
  line_centers = data.frame()
  for (i in 1:length(test@lines[[1]]@Lines)){
    this = sp::SpatialLines(list(sp::Lines(list(test@lines[[1]]@Lines[[i]]), paste("i"))), proj4string = CRS(TargetCRS))
    center_of_line = rgeos::gCentroid(this)
    line_centers = rbind(line_centers, center_of_line@coords)
  }
  line_centers_DF=sp::SpatialPointsDataFrame(line_centers, data=line_centers, proj4string = CRS(TargetCRS))

  # Pleures Centers
  combinations = data.frame(t(utils::combn(1:ngonies, 2)))
  combinations$diff = abs(combinations[,1] -combinations[,2]) # find the consequent points (diff==1)
  combinations = combinations[combinations$diff == 1 | combinations$diff == (ngonies-1), ]  #Select consequent points
  n=nrow(combinations)
  l = vector("list", n)

  for (i in 1:nrow(combinations)){
    row = combinations[i,]
    from_coordinates = co[row[[1]],]
    to_coordinates = co[row[[2]],]
    l[[i]] <- sp::Lines(list(sp::Line(rbind(from_coordinates, to_coordinates))), as.character(i))
  }

  pleures = sp::SpatialLines(l, proj4string = CRS(TargetCRS))
  kentra = rgeos::gCentroid(pleures, byid = TRUE)
  mesi_korifon = sp::SpatialPointsDataFrame(data.frame(kentra), data=data.frame(kentra), proj4string = CRS(TargetCRS))

  n_centroids = nrow(centroids) #[OK]
  n_diagonioi = nrow(simia_diagonion) #[OK]
  n_line_centroids = nrow(line_centers_DF) #[OK]
  n_mesi_korifon = nrow(mesi_korifon) #[OK]

  # Remove from simia_diagonion ta simia_korifes
  simia_diagonion = rgeos::gDifference(simia_diagonion ,simia_korifes )
  simia_diagonion = sp::SpatialPointsDataFrame(sp::coordinates(simia_diagonion),
                                               data=data.frame(sp::coordinates(simia_diagonion)),
                                               proj4string = CRS(TargetCRS))
  #plot(simia_diagonion)

  #plot(pol)
  #plot(simia_diagonion, col="red", pch=19, add=TRUE)
  #plot(simia_korifes, add=TRUE)

  centroids@data$type = "Polygons Centroids"
  simia_diagonion@data$type = "Intersections"
  simia_korifes@data$type = "Koryfes"
  line_centers_DF@data$type = "Line Centers"
  mesi_korifon@data$type = "Boundaries Ceners"

  results = rbind(centroids, simia_diagonion, simia_korifes,  line_centers_DF, mesi_korifon )   # spRbind
  results$id = 1:nrow(results@data)

  if(ploting){
    onomata = unique(results$type)
    mycolors = c("dark green","red","cyan","orange","pink")
    plot(results, col=mycolors, main="Results of 'uav.GenerateTargets'")
    plot(pol,  lwd=0.3, add=TRUE )
    plot( diago,  lwd=0.3, add=TRUE, col="purple")
    legend("topright",onomata,col=mycolors,pch=19, cex=1.1)
    box()
  }

  #sp::proj4string(results) = sp::CRS("+proj=longlat +datum=WGS84")
  #results = sp::spTransform(results, sp::CRS("+proj=longlat +datum=WGS84"))

  if(export){
    rgdal::writeOGR(sminos,dsn=paste0(output_folder,"/ArxikaDedomena.shp"),layer="ArxikaDedomena",driver="ESRI Shapefile", overwrite_layer = TRUE)
    #writeSpatialShape(sminos, paste0(output_folder,"/ArxikaDedomena.shp"))#, proj4string = CRS("+proj=longlat +datum=WGS84"))
    # Grammes
    #writeSpatialShape(SpatialLinesDataFrame(diago,data=data.frame(id=1:length(diago))), paste0(output_folder,"/Diagonies.shp"))#, proj4string = CRS("+proj=longlat +datum=WGS84"))
    rgdal::writeOGR(SpatialLinesDataFrame(diago,data=data.frame(id=1:length(diago))),
                    dsn=paste0(output_folder,"/Diagonies.shp"),layer="Diagonies",driver="ESRI Shapefile", overwrite_layer = TRUE)

    # Simeia kai Convex Hull
    #writeSpatialShape(results, paste0(output_folder,"/Targets.shp"))#, proj4string = CRS("+proj=longlat +datum=WGS84"))
    rgdal::writeOGR(results, dsn=paste0(output_folder,"/Targets.shp") ,layer="Targets",driver="ESRI Shapefile", overwrite_layer = TRUE)
    plotKML::kml(results, output_folder, paste0(output_folder,"/Targets.kml"), kmz=F)
    utils::write.csv(results@data, paste0(output_folder,"/Targets.csv"), row.names = F)

    #writeSpatialShape(pol, paste0(output_folder,"/ConvexHull.shp"))#, proj4string = CRS("+proj=longlat +datum=WGS84"))
    rgdal::writeOGR(pol, dsn=paste0(output_folder,"/ConvexHull.shp")  ,layer="ConvexHull",driver="ESRI Shapefile", overwrite_layer = TRUE)
    #print(paste("Total Points:",n_diagonioi+n_centroids+n_line_centroids+n_mesi_korifon)  )
    #cat("\n")
  }
  #print(nrow(results@data))

  return(list(Polygon = pol, simia=results, sminos=sminos, lines = SpatialLinesDataFrame(diago,data=data.frame(id=1:length(diago)))))
} # Generate Targets




#' uav.showFocal
#'
#' Show the Focal map of the study area.
#'
#' @param insimia something
#' @param inpol a simia SpatialPOintsDataFrame
#' @param size the cell size (rows-columns)
#' @param toplot Whether to plot a diagram of the focal density of points in the study area
#' @return a frequency table of number of values per cell of the focal operation
#'
#' @export
#'
#' @examples library(goal)
#' data(goaldata.data2, package ="goal")
#' one = goal::uav.GenerateTargets(goaldata.data2)
#' result = goal::uav.showFocal(one$simia, one$Polygon, size=20, toplot=TRUE)
#' print(result)
uav.showFocal <- function(insimia, inpol, size=20, toplot=TRUE) {
  if(!("id" %in% names(insimia))){stop("showFocal function needs an insimia with one column 'id'") }
  #library(raster)
  mypallete= rev(grDevices::heat.colors(10, alpha = 1))
  box = raster::raster(inpol, nrows=size, ncols=size)
  inr= raster::rasterize(inpol, box, field = 0 )

  # Number of points by cell
  tab = table(raster::cellFromXY(inr, insimia))
  inr[as.numeric(names(tab))] = tab
  table(inr[]) # frequency

  f = raster::focal(inr, w=matrix(1,nrow=3,ncol=3), fun=sum, na.rm=TRUE, pad=TRUE)
  if(toplot){
    plot(f,   col=mypallete)
    plot(inpol, add=TRUE, main=paste0("Number of points: ",length(insimia)))
    points(insimia, pch=20)
    text(insimia, insimia$id, adj=1)
  }
  return(table(f[]))
}


#' uav.removeOnePointByDM
#'
#' Eliminate one point by Distance Matrix of points
#'
#' @param insimia something
#' @return a SpatialPointsDataFrame with 1 reduced point
#'
#' @keywords internal
uav.removeOnePointByDM <- function(insimia) {

  # insimia= goal::uav.GenerateTargets(goaldata.data2)$simia
  # plot(insimia, pch=20)
  # text(insimia, labels=insimia$id, col="red")

  #library(goal)
  # data(goaldata.data2, package ="goal")
  # one = uav.GenerateTargets(goaldata.data2)
  # result = uav.removeOnePointByDM(one$simia)
  # insimia = one$simia
  # print(result)

  if(!("id" %in% names(insimia))){stop("uav.removeOnePointByDM function needs an insimia with one column 'id'") }


  dm = raster::pointDistance(insimia)
  #library(geosphere)
  #dm = geosphere::distm( insimia) # LAT LON
  dm[dm==0 ]=NA
  #dim(dm)
  col = apply(dm, 1, function(x) min(x, na.rm=TRUE)) # point with min distance to others

  item = which(col==min(col))[1]                  # get the first of the found points
  idtoremove = insimia$id[item]

  #print(insimia$id)
  #par(mfrow=c(1,2))
  #plot(insimia, main=paste("Before removing id:",idtoremove))
  #plot(insimia[insimia$id==idtoremove,], pch=10, cex=2,add=T, col="red")
  #text(insimia, labels = insimia$id)
   #print(paste0("Will remove point id:",idtoremove))
  #result =  subset(insimia, !('id' %in% insimia$id[item])) # wrong subseting
  result =insimia[!(insimia$id == idtoremove),]

  #plot(result, main=paste("After removing id:",idtoremove))
  #text(result, labels = result$id)
  print(paste0("Removed point id:",idtoremove))
  return(result)
}



#' uav.DoReduction
#'
#' Do the reduction of targets.
#'
#' @param intargets SpatialPointsDataFrame with the location of targets to reduce
#' @param insize not used so far.
#'
#' @return A list with polygons, points and sminos with the reduced target locations.
#' @export
#' @examples library(goal)
#' data(goaldata.data2)
#' one = goal::uav.GenerateTargets(goaldata.data2)
#' result = uav.DoReduction(one, insize=20)
#' print(result)
uav.DoReduction <- function(intargets, insize=15) {
  # Testing
  #intargets = goal::uav.GenerateTargets(goaldata.data2)
  #insize=10
  #plot(result)
  #plot(simia)
  pol=intargets$Polygon
  simia=intargets$simia
  sminos=intargets$sminos
  while( !(all(as.numeric(names( goal::uav.showFocal(simia, intargets$Polygon, size=insize) ))<=2)) ){
    temp=simia
    simia = uav.removeOnePointByDM(temp)
    #print(simia$id)
  }
  return(simia)
}
