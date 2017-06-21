# geomst

### Description
Given a list of geographic locations, such as cities, `geomst` calculates a minimum spanning tree between the locations. Plotting functionality also included.

### Example
```
latlon <- getLatLon(c('New York, New York', 'Boston, MA')) # Obtains latitude and longitude of locations
dist_mat <- getDistMat(latlon)                             # Gets distance matrix for locations
mst <- getMST(dist_mat)                                    # Gets minimum spanning tree igraph object
graph_lines <- getGraphLines(mst, latlon)                  # Converts igraph object to something that can be plotted on a map
plotGraphLines(graph_lines)                                # Plots minimum spanning tree on a map
graphLines2Polyline(graph_lines)                           # Converts minimum spanning tree to a Polyline object that can be saved as a shapefile
```

### Installation 
To install R package, open R and type:
```
library(devtools)
install_github("lboxell/geomst")
```
