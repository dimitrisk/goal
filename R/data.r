#  ===   goaldata.GRregions   ====

#' The 13 regions of Greece
#'
#'  A [SpatialPolygonsDataFrame]  of the 13 regions of Greece.
#'  The variables in the dataset are as follows:
#' \itemize{
#'   \item **code**: The ID of the region
#'   \item **Name_en**: Name of the region
#'   \item **gdp_capita**: The GDP per capita of the region
#'   \item **GDP_mil**: The GDP of the region in milion euros
#'   \item **pop_dens**: Population density of the region
#'   \item **pop**: Population of the region
#'   \item **total_area**: Total area of the region in \eqn{km^2}
#' }
#' @docType data
#' @source The shapefile of the corresponding polygons is available from
#' the [Open-Data portal](http://geodata.gov.gr/geodata) of
#' the Greek Government.
#' Population and GDP values are available from
#' the
#' [Eurostat](http://epp.eurostat.ec.europa.eu/portal/page/portal/eurostat/home/)
#'
#' @name goaldata.GRregions
#' @examples library(goal)
#' data(goaldata.GRregions)
#' @format A [SpatialPolygonsDataFrame()]  of the 13 regions of Greece
NULL


# ========================   goaldata.roads   =================================

#' A dataset of 40 roads
#'
#' The variables in the dataset are as follows:
#' \itemize{
#'   \item **from**: The id of the origin
#'   \item **to**: The id of the destination
#'   \item **id**: The id of this road
#'   \item **name**: The name of this road
#'   \item **cost**: The cost units of using this road
#'   \item **category**: The category of this road
#' }
#' @docType data
#' @source [Dimitris Kavroudakis](http://www.dimitrisk.gr)
#' @name goaldata.roads
#' @examples library(goal)
#' data(goaldata.roads)
#' @format A [data.frame()] with 40 rows and 6 variables
NULL




# ========================   goaldata.towns   =================================

#' A dataset of 15 towns
#'
#'    The variables in the dataset are as follows:
#' \itemize{
#'   \item **id**: The id of the town
#'   \item **name**: The name of the town
#'   \item **pop**: The population of the town
#'   \item **meanincome**: The meanincome of the town
#' }
#' @docType data
#' @source [Dimitris Kavroudakis](http://www.dimitrisk.gr)
#' @name goaldata.towns
#' @examples library(goal)
#' data(goaldata.towns)
#' @format A [data.frame()] with 15 rows and 4 variables
NULL



# ========================   goaldata.data2   =================================

#' goaldata.data2
#'
#'  A  data2:
#' \itemize{
#'   \item **x**: The x coordinates of the points
#'   \item **y**: The y coordinates of the points
#' }
#' @docType data
#' @source Dimitris Kavroudakis
#'
#' @name goaldata.data2
#' @examples library(goal)
#' data(goaldata.data2)
#' @format A [data.frame()] with two columns (x,y) representing some points in Greece.
NULL
