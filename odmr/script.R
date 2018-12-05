## installation

install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("iobis/robis", ref ="obis2")

## basic data access

library(robis)

sf <- occurrence("Spio filicornis")
map_leaflet(sf)

## data exploration

dim(sf)
names(sf)
head(sf)
str(sf)
View(sf)
table(sf$originalScientificName)

## data exploration with dplyr

library(dplyr)

spio <- occurrence("Spio")
spio %>% group_by(species) %>% summarize(n = n(), lat = mean(decimalLatitude))

## quick and dirty mapping

library(ggplot2)

ggplot(spio) + geom_point(aes(decimalLongitude, decimalLatitude, colour = species))

spio %>% filter(species == "Spio blakei") %>% map_leaflet()

## spatial queries

mol <- occurrence("Mollusca", geometry = "POLYGON ((2.54333 51.07247, 2.10388 51.64189, 2.79053 51.80522, 3.36731 51.36149, 2.54333 51.07247))")
map_leaflet(mol)

## visualization using ggplot2

ggplot(mol) + geom_bar(aes(date_year), width = 1)
ggplot(mol) + geom_bar(aes(date_year, fill = class), width = 1) + scale_fill_brewer(palette = "Spectral")

## bathymetry from marmap

library(marmap)
library(plotly)

res <- 0.2
xmin <- 158
xmax <- 180
ymin <- -55
ymax <- -30
nz <- getNOAA.bathy(lon1 = 158, lon2 = 180, lat1 = -55, lat2 = -30, resolution = res * 60)
nz <- t(nz)
x <- seq(xmin + res / 2, xmax - res / 2, by = res)
y <- seq(ymin + res / 2, ymax - res / 2, by = res)

geom <- sprintf("POLYGON ((%s %s, %s %s, %s %s, %s %s, %s %s))", xmin, ymax, xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax)
ha <- occurrence("Hoplostethus atlanticus", geometry = geom)
pc <- occurrence("Parapercis colias", geometry = geom)

plot_ly(z = ~nz, x = ~x, y = ~y) %>%
  add_surface(showscale = FALSE) %>%
  add_trace(data = ha, x = ~decimalLongitude, y = ~decimalLatitude, z = ~-minimumDepthInMeters, marker = list(color = "#ffcc00", size = 3), name = "Hoplostethus atlanticus") %>%
  add_trace(data = pc, x = ~decimalLongitude, y = ~decimalLatitude, z = ~-minimumDepthInMeters, marker = list(color = "#ff3399", size = 3), name = "Parapercis colias")

## export as CSV

write.csv(mol, file = "mollusca.csv", row.names = FALSE)
