library(robis)
library(obistools)
library(ggplot2)
library(dplyr)

calfin <- occurrence("Calanus finmarchicus", fields = c("decimalLongitude", "decimalLatitude"))
xy <- lookup_xy(calfin[1:150000,], shoredistance = FALSE, grids = TRUE, areas = FALSE)

plot(xy$salinity, xy$temperature)

calfin <- cbind(calfin[1:150000,], xy)
names(calfin) <- c("decimalLongitude", "decimalLatitude", "salinity", "temperature", "bathymetry")

ggplot(calfin) +
  geom_point(aes(decimalLongitude, decimalLatitude, color = decimalLatitude)) +
  scale_colour_distiller(palette = "Spectral")

ggplot(calfin %>% arrange(desc(decimalLatitude))) +
  geom_point(aes(salinity, temperature, colour = decimalLatitude)) +
  scale_colour_distiller(palette = "Spectral")
