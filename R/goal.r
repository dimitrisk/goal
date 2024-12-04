#' Analysis of geographical data.
#' Includes 3 group of functions:
#'
#' * geographical analysis (_ga_)
#'
#' * geograhical visualization (_vis_)
#'
#' * network analysis (_net_)
#'
#' Feel free to contact me
#' regarding the library:
#' \email{dimitris123@@gmail.com}
#'
#' @author Assistant Professor
#' Dimitris Kavroudakis \email{dimitris123@@gmail.com},
#' University of the Aegean, Geography Department,
#' @name goal
#' @aliases goal-package
#' @noRd
#' @keywords internal
#' @docType package
#' @title GeOgraphical Analysis Library
"_PACKAGE"

# ========================   OSM   =================================
#source("R/OSMDownload/osmDownload.r")

# =======   Visualization   =======
#source("R/vis.r")

# ============   Data   =====
source("R/data.r")


# ======   Network   =======
# From here
# http://en.wikibooks.org/wiki/Transportation_Geography_and_Network_Science/Characterizing_Graphs
# or
# http://www.jvdz.net/index2.html?/lcc-graph.html&frameMain
#source("R/net.r")



# =======   Geometry   ======
#source("R/geometry.r")

# ========   UAV   ======
#source("R/uav.r")
#source("R/uav.dimkaAlgorithm.r")

source("R/spatial.r")

# ======   Similarity   =======
#source("R/stats.r")

# ======   OSM   =======
source("R/osm.r")
