library(robis)
molram <- occurrence("Mola ramsayi")

library(ggplot2)
ggplot() + geom_point(data = molram, aes(x = decimalLongitude, y = decimalLatitude))

library(maps)
world <- map_data("world")

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dddddd") +
  geom_point(data = molram, aes(x = decimalLongitude, y = decimalLatitude))

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dddddd") +
  geom_point(data = molram, aes(x = decimalLongitude, y = decimalLatitude)) +
  coord_fixed(1)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dddddd") +
  geom_point(data = molram, aes(x = decimalLongitude, y = decimalLatitude)) +
  coord_fixed(1, xlim = c(0, 180), ylim = c(-60, 0))

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dddddd") +
  geom_point(data = molram, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  coord_fixed(1, xlim = c(0, 180), ylim = c(-60, 0))

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dddddd") +
  geom_point(data = molram, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  coord_fixed(1, xlim = c(0, 180), ylim = c(-60, 0)) +
  scale_color_brewer(palette = "Paired")

dor <- occurrence("Doridoidea")
ggplot() +
  geom_histogram(data = dor, aes(x = yearcollected))

ggplot() +
  geom_histogram(data = dor, aes(x = yearcollected), binwidth = 2)

ggplot() +
  geom_histogram(data = dor, aes(x = yearcollected, fill = family), binwidth = 2) +
  scale_fill_brewer(palette = "Spectral")

ggplot() +
  geom_histogram(data = dor, aes(x = yearcollected, fill = family), binwidth = 2) +
  scale_fill_brewer(palette = "Spectral") +
  xlim(c(1950, 2017))

library(dplyr)
lag <- occurrence("Lagis")
lag_2 <- lag %>% filter(resourceID %in% c(4312, 222))

ggplot() +
  geom_histogram(data = lag_2, aes(x = yearcollected), binwidth = 2) +
  facet_grid(resourceID ~ species)

install.packages("leaflet")

library(leaflet)

leaflet() %>% addTiles()

leaflet() %>% addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}")

library(robis)
abrseg <- occurrence("Abra segmentum")

leaflet() %>%
  addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
  addCircleMarkers(lat = abrseg$decimalLatitude, lng = abrseg$decimalLongitude, radius = 3.5, weight = 0, fillOpacity = 1, fillColor = "#cc3300")

library(robis)
library(leaflet)

acistu <- occurrence("Acipenser sturio")

acistu$qcnum <- qcflags(acistu$qc, c(28))
colors <- c("#ee3300", "#86b300")[acistu$qcnum + 1]
popup <- paste0(acistu$datasetName, "<br/>", acistu$catalogNumber, "<br/><a href=\"http://www.iobis.org/explore/#/dataset/", acistu$resourceID, "\">OBIS dataset page</a>")

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(popup = popup, lat = acistu$decimalLatitude, lng = acistu$decimalLongitude, radius = 3.5, weight = 0, fillColor = colors, fillOpacity = 1)

library(robis)
library(leaflet)

ices <- occurrence(resourceid = 1575, enddate = "1985-01-01")

ices$qcnum <- qcflags(ices$qc, c(27, 29))
colors <- c("#ee3300", "#ff9900", "#86b300")[ices$qcnum + 1]

leaflet() %>%
  addTiles("http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") %>%
  addCircleMarkers(popup = ices$scientificName, lat = ices$decimalLatitude, lng = ices$decimalLongitude, radius = 3.5, weight = 0, fillColor = colors, fillOpacity = 1)

pac <- occurrence("Gadus macrocephalus")
atl <- occurrence("Gadus morhua", year = 2011)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lat = pac$decimalLatitude, lng = pac$decimalLongitude, radius = 3.5, weight = 0, fillOpacity = 1, fillColor = "#ff0066") %>%
  addCircleMarkers(lat = atl$decimalLatitude, lng = atl$decimalLongitude, radius = 3.5, weight = 0, fillOpacity = 1, fillColor = "#0099cc")
