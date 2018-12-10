########################################
# Presentations Day 5                  #
# Dec 5 2018                           #   
########################################

# tmap and tmaptools for thematic maps -------------------------------------
library(tmap)
library(tmaptools)
 
#two types of spatial data: discrete (eg. shapefiles) and continuous(rasters)
#additional variables that describe data called attributes

#using a shapefile
data(World, metro)
names(c(World, metro))

#mapping is similar to ggplot: a base with additional layers
tm_shape(World)+
  tm_polygons("HPI", style = "pretty", colorNA = NULL, id = "name", 
              popup.var = T)+
  tm_shape(metro)+
  tm_dots("black")

#Change some categories
summary(World$HPI)
happy <- c(12.78, 26.48, 44.71)

tm_shape(World)+
  tm_polygons("HPI", breaks = happy, labels = c("happy", "unhappy"))+
  tm_facets(by = "continent")

#more functions
tm_shape(World)+
  tm_polygons("HPI", breaks = happy, border.col = "grey", border.alpha = 0.1)+
  tm_symbols(size = "well_being", col = "grey", border.lwd = NA, alpha = 0.5,
             shape = "footprint", shapes.legend.fill = "blue")

#compare multiple maps
tm_shape(World)+
  tm_polygons(c("economy", "HPI"), breaks = happy, palette = "seq")

#raster examples
data(land)
names(land)

tm_shape(land)+
  tm_raster(col = "cover_cls", title = "Land Cover classes", legend.hist = T,
            legend.hist.title = "Frequency")+
  tmap_style("col_blind")+
  tm_legend(legend.position = c("left", "bottom"))+
  tm_shape(World)+
  tm_borders(col = "black")+
  tm_shape(metro)+
  tm_dots("black")

#change to interactive view
ttm()
#then rerun the script for your map

#saving
#save_tmap(filename)

tm_shape(land)+
  tm_raster(col = "trees", palette = "seq", style = "cont", n = 3)

# Intro to networks using igraph ---------------------------------------------
library(igraph)
library(igraphdata)

data(foodwebs)

#Create empty network
g <- make_empty_graph(directed = F)
#Add vertices
g <- g+vertices('bio', 'eco', 'compsci', 'soc', 'psych', 'econ')
#Plot it
plot(g, vertex.label.dist = 3)

#Add edges
g <- g + edges(c('compsci', 'bio', 'compsci', 'eco', 'compsci', 'econ'))
#edges are added by pairing elements 1&2, 3&4, etc of vector
plot(g, vertex.label.dist = 3)

#You can add edges in separate groups
g <- g+edges(c('bio', 'soc', 'bio', 'psych'))
plot(g, vertex.label.dist = 3)

#There's a better way to do this
edgeList <- matrix(c('compsci', 'bio', 'compsci', 'eco', 'compsci', 'econ'),
                   ncol = 2, byrow = T)

#you can pull edge lists from an existing network object
edgelist2 <- as_edgelist(g, names = T)

gEdgeList <- graph_from_edgelist(edgelist2, directed = F)
plot(gEdgeList, vertex.label.dist = 3)
#Note: isolated vertices can't be added using this method. You have to add them
#manually

#Create adjacency atrix from existing network
adjmat <- as_adjacency_matrix(g)

gadjmat <- graph_from_adjacency_matrix(adjmat, mode = 'undirected')
plot(gadjmat, vertex.label.dist = 3)

#directed networks
adjmat <- matrix(data = 0, nrow = 7, ncol = 7)
species <- c('coyote', 'vulture', 'snake', 'grass', 'bug', 'hawk', 'mouse')
rownames(adjmat) <- colnames(adjmat) <- species

adjmat["grass", "bug"] <- 1
adjmat["bug", "hawk"] <- 1
adjmat["grass", "mouse"] <- 1
adjmat["mouse", "hawk"] <- 1
adjmat["mouse", "coyote"] <- 1
adjmat["hawk", "coyote"] <- 1
adjmat["coyote", "vulture"] <- 1

web <- graph_from_adjacency_matrix(adjmat)
plot(web)

#Weighted networks
#check to see if network is weighted
is_weighted(web)

#Using real data
fwc <- foodwebs$CrystalC

plot(fwc, layout = layout_as_tree, vertex.label.dist = 1.5)
