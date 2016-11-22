require(dplyr)
require(vegan)
require(reshape2)
require(RColorBrewer)
require(leaflet)

source("demo_4a.R")

data <- read.csv("data_embos/FixO3_EMBOS_softsubBeach_clean.csv", sep = "\t")
env <- read.csv("data_embos/Provoost_FixO3_EMBOS_beach_env.csv", sep = "\t")

# create stations table

stations <- data %>%
  group_by(stationCode, lat, long) %>%
  summarise() %>%
  arrange(stationCode)

# reshape environmental data

env <- env %>%
  group_by(stationCode, parameter) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  dcast(stationCode ~ parameter, value.var = "value") %>%
  arrange(stationCode)

# environmental data: vector and surface fitting

ord.fit <- envfit(ord ~ Salinity, env, na.rm = TRUE)

plot(ord, type = "n")
orditorp(ord, display = "spec", cex = 0.5, col = "#aaaaaa")
orditorp(ord, display = "sites", cex = 0.7, col = "#cc3300")
plot(ord.fit, col = "#0099ff", cex = 0.8)
with(env, ordisurf(ord, Salinity, add = TRUE, col = "#669900"))

# map clusters

grp <- grp[order(names(grp))]
stations$grp <- grp
stations$color <- brewer.pal(5, "Set1")[stations$grp]

m <- leaflet()
m <- addProviderTiles(m, "CartoDB.Positron")
m <- addCircleMarkers(m, data = stations, radius = 5, weight = 0, fillColor = stations$color, fillOpacity = 1)
m
