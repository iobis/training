if (!require("ggplot2")) { install.packages("ggplot2") }
if (!require("dplyr")) { install.packages("dplyr") }
if (!require("lubridate")) { install.packages("lubridate") }
if (!require("mgcv")) { install.packages("mgcv") }
if (!require("leaflet")) { install.packages("leaflet") }
if (!require("ggmap")) { install.packages("ggmap") }

# load packages

library(robis)
library(dplyr)
library(ggplot2)
library(lubridate)
library(mgcv)
library(leaflet)
library(ggmap)

# bar plot

pol <- occurrence("Polychaeta", geometry = "POLYGON ((2.56119 51.07506, 2.38953 51.27051, 3.08167 51.55573, 3.32062 51.43090, 3.36731 51.35720, 2.56119 51.07506))")

ggplot(pol %>% filter(!is.na(order))) +
  geom_bar(aes(x = yearcollected, fill = order), width = 1) +
  scale_fill_brewer(palette = "Paired", na.value = "#cccccc") +
  xlim(1950, 2017)

ggplot(pol %>% filter(!is.na(order), institutionCode %in% c("ILVO", "UGent"))) +
  geom_bar(aes(x = yearcollected, fill = order), width = 1) +
  scale_fill_brewer(palette = "Paired", na.value = "#cccccc") +
  facet_grid(institutionCode ~ order) +
  xlim(1950, 2017)

# scatter plot - http://iobis.org/explore/#/dataset/623

l4 <- occurrence(resourceid = 623)
plot(l4$individualCount)

acacla <- l4 %>% filter(aphiaID == 149755, yearcollected <= 2006) %>% mutate(time = decimal_date(as.Date(eventDate)))

ggplot(acacla) +
  geom_point(aes(x = time, y = individualCount))

ggplot(acacla) +
  geom_point(aes(x = time %% 1, y = individualCount))

# GAM model

mod <- gam(data = acacla, log(individualCount) ~ s(time, bs="tp", k=10) + s(time %% 1, bs="cc", k=4))
plot(mod, pages = 1, residuals = T, pch = 19, cex = 0.25, scheme = 1, col = "#FF8000", shade = T, shade.col = "gray90")

# leaflet - http://leaflet-extras.github.io/leaflet-providers/preview

abrseg <- occurrence("Abra segmentum")

leaflet() %>%
  addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
  addCircleMarkers(lat = abrseg$decimalLatitude, lng = abrseg$decimalLongitude, radius = 3.5, weight = 0, fillOpacity = 1, fillColor = "#cc3300")

# quality flags

acistu <- occurrence("Acipenser sturio")

acistu$qcnum <- qcflags(acistu$qc, c(28))
colors <- c("#ee3300", "#86b300")[acistu$qcnum + 1]
popup <- paste0(acistu$datasetName, "<br/>", acistu$catalogNumber, "<br/><a href=\"http://www.iobis.org/explore/#/dataset/", acistu$resourceID, "\">OBIS dataset page</a>")

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(popup = popup, lat = acistu$decimalLatitude, lng = acistu$decimalLongitude, radius = 3.5, weight = 0, fillColor = colors, fillOpacity = 1)

# colors

lag <- occurrence("Lagis")
pal <- colorFactor("Paired", lag$species)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(fillColor = pal(lag$species), popup = lag$species, lat = lag$decimalLatitude, lng = lag$decimalLongitude, radius = 3.5, weight = 0, fillOpacity = 1)

# ggmap

nem <- occurrence("Polychaeta", geometry = "POLYGON ((1.62598 51.85614, 1.66992 50.76426, 3.72437 50.84757, 3.57056 51.90361, 1.62598 51.85614))", fields = c("decimalLongitude", "decimalLatitude", "species"))

sites <- nem %>%
  group_by(lon = round(decimalLongitude, 1), lat = round(decimalLatitude, 1)) %>%
  summarize(species = length(unique(species))) %>%
  arrange(species)

box <-  c(
  min(sites$lon),
  min(sites$lat),
  max(sites$lon),
  max(sites$lat)
)
basemap <- get_map(location = box, color = "bw")

ggmap(basemap) +
  geom_tile(data = sites, aes(x = lon, y = lat, fill = species), size = 0.1, alpha = 0.5) +
  scale_fill_distiller(palette = "Spectral")
