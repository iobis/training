# install obistools

if (!require("devtools")) { install.packages("devtools") }
if (!require("obistools")) { devtools::install_github("iobis/obistools") }

# load packages

library(obistools)
library(robis)
library(dplyr)
library(ggplot2)

# points on land

type <- occurrence(resourceid = 3949)

land <- check_onland(type)
leafletmap(land)
land <- check_onland(type, buffer = 10000)
leafletmap(land, popup = "catalogNumber")
land %>% filter(catalogNumber == "dr_id_957531")

# taxon matching

match_taxa(c("Abra alba", "Petinaria koreni", "Vertebrata", "Zonaria marginata"))

# depth check

ger <- occurrence(geometry = "POLYGON ((7.71240 54.34215, 7.93213 53.47497, 9.03076 53.65766, 8.72314 54.31652, 7.71240 54.34215))", fields = c("minimumDepthInMeters", "maximumDepthInMeters", "decimalLongitude", "decimalLatitude"))
problems <- check_depth(ger, depthmargin = 10)
plot(problems$maximumDepthInMeters)
bathymetry <- lookup_xy(problems, shoredistance = FALSE, grids = TRUE, areas = FALSE)$bathymetry

ggplot() +
  geom_point(aes(x = bathymetry, y = problems$maximumDepthInMeters)) +
  geom_abline(intercept = 0) +
  geom_abline(intercept = 10, colour = "red", linetype = "dashed") +
  coord_fixed() +
  xlim(range(problems$maximumDepthInMeters)) +
  ylim(range(problems$maximumDepthInMeters))

# ST diagram - http://bio-oracle.org/

gen <- occurrence("Lagis") # Lagis, Limecola, Marenzelleria
xy <- lookup_xy(gen, shoredistance = FALSE, grids = TRUE, areas = FALSE)
gen <- cbind(gen, xy)

world <- map_data("world")

ggplot(gen) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#aaaaaa") +
  geom_point(aes(decimalLongitude, decimalLatitude, color = species)) +
  scale_colour_brewer(palette = "Paired")

ggplot(gen) +
  geom_point(aes(sssalinity, sstemperature, colour = species)) +
  scale_colour_brewer(palette = "Paired")
