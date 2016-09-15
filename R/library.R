library(ggmap)
library(gdistance)
library(rworldmap)
library(data.table)
library(SDMTools)
library(igraph)
library(utils)
library(geosphere)
library(shapefiles)
library(rgeos)
library(maptools)
library(rgdal)
library(maps)
library(parallel)

## Functions
#' Obtains Latitutde and Longitude Coordinates.
#'
#' Given a vector of geoegraphic locations, it will return a matrix of latitude and longitude points with rownames that take on the value of `cities`.
#'
#' @param cities A vector of geographic locations.
#' @keywords geocode
#' @export
#' @examples
#' getLatLon(c('New York, New York'))
getLatLon <- function(cities, source = "google"){
	latlon <- NULL
	for (i in cities){
		latlon[[i]] <- geocode(i, source = source)
	}
	latlon <- as.matrix(rbindlist(latlon))
	rownames(latlon) <- cities
	return(latlon)
}

## Functions
#' Obtains distance matrix
#'
#' Given a nx2 matrix of latitude and longitude points, getDistMat will create an nxn matrix of distances between the points using Vincenty's formula.
#'
#' @param latlon An nx2 matrix of latitude and longitude points. See latlon().
#' @keywords Distance Matrix
#' @export
#' @examples
#' latlon <- getLatLon(c('New York, New York', 'Boston, MA'))
#' getDistMat(latlon)
getDistMat <- function(latlon){
  if(is.null(rownames(latlon))){
    rownames(latlon) <- 1:nrow(latlon)
  }
  
	## Get distance between points "as the crow flies" using vincenty's formula
	dist_mat <- matrix(0, nrow = length(latlon[,1]), ncol = length(latlon[,1]))
	for(i in 1:length(latlon[,1])){
		for(j in 1:length(latlon[,1])){
			if(i < j){
				dist_mat[i, j] <- distance(latlon[i, 2], latlon[i, 1], latlon[j, 2], latlon[j, 1])$distance
			}
		}
	}
	dist_mat <- dist_mat + t(dist_mat)
	rownames(dist_mat) <- colnames(dist_mat) <- rownames(latlon)
	return(dist_mat)
}

## Functions
#' Obtains minimum spanning tree.
#'
#' Returns a graph object containing the minimum spanning tree between the points in dist_mat.
#'
#' @param dist_mat A matrix of distances between points.
#' @keywords mst, minimum spanning tree
#' @export
#' @examples
#' latlon <- getLatLon(c('New York, New York', 'Boston, MA'))
#' dist_mat <- getDistMat(latlon)
#' getMST(dist_mat)
getMST <- function(dist_mat){
	cities      <- data.frame(rownames(dist_mat))
	connections <- combn(as.matrix(cities), 2)
	dist        <- vector(mode = "numeric", length = length(connections[1,]))

	for(i in 1:length(connections[1,])){
		dist[i] <- dist_mat[connections[1, i], connections[2, i]]
	}

	connections <- data.frame(cityA = connections[1,], cityB = connections[2,], weight = dist)
	g           <- graph.data.frame(connections, directed = FALSE, vertices = cities)

	mst         <- minimum.spanning.tree(g, weights = E(g)$weights)
	return(mst)
}

## Functions
#' Obtains minimum spannign tree edges for plotting.
#'
#' Returns a list containing coordinates for plotting each edge of the minimum spanning tree using the greater circle path between the points.
#'
#' @param mst A minimum spanning tree graph object.
#' @param latlong The corresponding latlon points for the mst graph.
#' @keywords mst, minimum spanning tree, plot
#' @export
#' @examples
#' latlon <- getLatLon(c('New York, New York', 'Boston, MA'))
#' dist_mat <- getDistMat(latlon)
#' mst <- getMST(dist_mat)
#' getGraphLines(mst, latlon)
getGraphLines <- function(mst, latlon, N = 40, intermediate = TRUE){
  if(is.null(rownames(latlon))){
    rownames(latlon) <- 1:nrow(latlon)
  }
  
	## Gets the greater circle edge between graph points
	mst_data <- as.matrix(get.data.frame(mst))
	graph_lines <- NULL
	if(intermediate == TRUE){
  	for(i in 1:length(mst_data[,1])){
  		graph_lines[[i]] <- gcIntermediate(latlon[mst_data[i, 1], ], latlon[mst_data[i, 2], ], n = N, addStartEnd = TRUE)
  	}
	} else{
	  for(i in 1:length(mst_data[,1])){
	    graph_lines[[i]] <- rbind(latlon[mst_data[i, 1], ], latlon[mst_data[i, 2], ])
	  }
	}
	
	return(graph_lines)
}

## Functions
#' Plots edges of minimum spanning tree.
#'
#' Uses maps from the `maps` package as a background.
#'
#'
#' @param graph_lines A list with each entry containing the coordinates of a minimum spanning tree's edge to plot
#' @param line_col The color the lines should be plotted. Defaults to red.
#' @param map The map from the `maps` package to draw as the background. Defaults to 'county'.
#' @keywords mst, minimum spanning tree, plot
#' @export
#' @examples
#' latlon <- getLatLon(c('New York, New York', 'Boston, MA'))
#' dist_mat <- getDistMat(latlon)
#' mst <- getMST(dist_mat)
#' graph_lines <- getGraphLines(mst, latlon)
#' plotGraphLines(graph_lines)
plotGraphLines <- function(graph_lines, line_col = "red", map = "county"){
	map(map)
	for(i in 1:length(graph_lines)){
		lines(graph_lines[[i]], col = line_col)
	}
}

## Functions
#' Converts list of mst edges to Polylines shapefile.
#'
#' Returns a Polylines shapefile. Allows for easier saving.
#'
#'
#' @param graph_lines A list with each entry containing the coordinates of a minimum spanning tree's edge to plot
#' @keywords mst, minimum spanning tree, shapefile
#' @export
#' @examples
#' latlon <- getLatLon(c('New York, New York', 'Boston, MA'))
#' dist_mat <- getDistMat(latlon)
#' mst <- getMST(dist_mat)
#' graph_lines <- getGraphLines(mst, latlon)
#' graphLines2Polyline(graph_lines)
graphLines2PolyLine <- function(graph_lines){
	graph_lines_df <- NULL
	for(i in 1:length(graph_lines)){
		graph_lines_df[[i]] <- data.table(data.frame(ID = rep(i, length(graph_lines[[i]][, 1])), Long = graph_lines[[i]][, 1], Lat = graph_lines[[i]][, 2]))
	}
	graph_lines_df        <- data.frame(rbindlist(graph_lines_df))
	graph_lines_names     <- data.frame(ID = unique(graph_lines_df$ID))
	graph_lines_shapefile <- convert.to.shapefile(graph_lines_df, graph_lines_names, "ID", 3)
	return(graph_lines_shapefile)
}
