########################################
# CompBio Presentations Day 4          #
# Dec. 4 2018                          #
########################################

# Package popbio for modeling structured populations---------------------------------
#Contains functions for prediting how population is expected to change

library(popbio)

#Creating a matrix
stages <- c("seed", "juvenile", "flower")
A <- matrix(c(0.5, 0, 50, 0.1, 0.2, 0, 0, 0.6, 0.9), nrow = 3, byrow = T, 
            dimnames = list(stages, stages))

n <- c(0, 0, 10)

#Lets the population go until a stable state is reached
p <- pop.projection(A = A, n = n, iterations = 20)

#plot it
stage.vector.plot(p$stage.vectors)

#Calculate population growth rate
eig <- eigen.analysis(A)

eig$elasticities #elasticity matrix

swampyA <- A
swampyA[2,1] <- 0.05

p2 <- pop.projection(swampyA, n, iterations = 10)
eigswampy <- eigen.analysis(swampyA)

shadyA <- swampyA
shadyA[1,2] <- 0.1

p3 <- pop.projection(A = shadyA, n = n, iterations = 10)

# Extracting WorldClim variables ------------------------------------------
library(rgdal)
library(raster)
library(maps)
library(leaflet)
library(dplyr)

#Create some random points in the Mt Mansfield area
set.seed(802)
long <- runif(10, -72.85, -72.78)
lat <- runif(10, 44.5, 44.6)
# a vector of ID numbers for these coordinates
ID <- 1:10
# bind the long and lat into a dataframe
coords <- data.frame(long, lat)

# visualizing the ten coordinates
leaflet(data=coords) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(~long, ~lat, label=as.character(ID))

# downloading the bioclimatic variables from worldclim at a resolution of 30 seconds
r <- getData("worldclim", var="bio", res=0.5, lon=-72, lat=44)
# lets also get the elevational data associated with the climate data
alt <- getData("worldclim", var="alt", res=.5, lon=-72, lat=44)

plot(alt)

# reduce the layers in our RasterStack to the variables we want to look at
r <- r[[c(1, 12)]] 
# we can name these two layers
names(r) <- c("Tmean", "Prec")

# the steps to extract values for the variables you want from the coordinates:
points <- SpatialPoints(coords, proj4string = r@crs)
# getting temp and precip for the points
clim <- extract(r, points)
# getting the 30s altitude for the points
altS <- extract(alt, points)
# bind it all into one dataframe
climate <- cbind.data.frame(coords, altS, clim)
# what does this look like? 
print(climate)

climate <- mutate(climate, MAT=Tmean/10) %>%
  select((-Tmean))
print(climate)

plot(data = climate, MAT~altS)
