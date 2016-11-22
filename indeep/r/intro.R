### introduction

# vectors

a <- 1
class(a)
length(a)

b <- "banana"
b
class(b)

a <- c(1, 2)
a

b <- seq(1, 10)
b
length(b)

# data frames

d <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"))
d
d$a
d[1]
d[1,]
d[,1]

# lists

a <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"))
l <- list(a = a, b = 1)
l
l$a
l[[1]]
l[["a"]]

# reading text files

data <- read.table("data.txt", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
data <- read.csv("data.csv")

# reading excel files

require(xlsx)
data <- read.xlsx("data.xlsx", 1)
data <- read.xlsx("data.xlsx", sheetName = "somesheet")

# installing packages

install.packages("devtools")
devtools::install_github("iobis/robis")
require(robis)

# data exploration

data <- occurrence("Abra")
head(data)
dim(data)
names(data)
summary(data)

# manipulating data

require(dplyr)

data <- occurrence("Abra")
data_species <- data %>% filter(scientificName == "Abra alba" & yearcollected > 2005)

data %>% arrange(datasetName, desc(eventDate))

temp <- data %>% select(scientificName, eventDate, lon = decimalLongitude, lat = decimalLatitude)
temp <- data %>% select(scientificName, locality) %>% distinct()

withzones <- data %>% mutate(zone = .bincode(minimumDepthInMeters, breaks = c(0, 10, 100)))

# pivot tables

require(reshape2)

data <- read.csv("Rockall_ophs.csv")
data2 <- data %>% select_if(function(x) { !all(is.na(x)) })
data3 <- melt(data2, id = "species", variable.name = "station", value.name = "individuals")
data4 <- data3 %>% filter(individuals > 0)

# aggregating data

data <- occurrence("Abra")

data %>% summarise(lat_mean = mean(decimalLatitude), lat_sd = sd(decimalLatitude))
data %>% group_by(scientificName) %>% summarise(records = n(), datasets = n_distinct(datasetName))

# plotting

require(ggplot2)
require(leaflet)

plot(iris)

ggplot(data = iris) + geom_point(aes(x = Sepal.Length, y = Petal.Length))
ggplot(data = iris) + geom_point(aes(x = Sepal.Length, y = Petal.Length, size = Sepal.Width))
ggplot(data = iris) + geom_point(aes(x = Sepal.Length, y = Petal.Length, size = Sepal.Width, colour = Petal.Width))

# mapping

data <- occurrence("Lophelia")

pal <- colorFactor("Spectral", levels = unique(data$datasetName))

leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(data = data %>% select(lat = decimalLatitude, lng = decimalLongitude), radius = 3, weight = 0, color = pal(data$datasetName), fillOpacity = 1)

### data access

require(robis)

data <- occurrence("Cnidaria", startdepth = 1000, geometry = "POLYGON ((-33.92578 40.46367, -33.81592 28.38174, -21.53320 28.32372, -22.39014 40.61395, -33.92578 40.46367))")
leafletmap(data)

taxa <- checklist("Cnidaria", startdepth = 1000, geometry = "POLYGON ((-33.92578 40.46367, -33.81592 28.38174, -21.53320 28.32372, -22.39014 40.61395, -33.92578 40.46367))")

### visualization

# quality flags (1)

data <- occurrence("Acipenser oxyrinchus")
data <- data %>% mutate(score = qcflags(qc, c(28)))

pal <- colorNumeric(colorRamp(c("#cc3300", "#99cc00")), domain = range(data$score))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = data %>% select(lat = decimalLatitude, lng = decimalLongitude), radius = 3, weight = 0, color = pal(data$score), fillOpacity = 1)

# quality flags (2)

data <- occurrence("Verruca stroemia")
data <- data %>% mutate(score = qcflags(qc, c(24, 25, 26, 27, 28)))

pal <- colorNumeric(colorRamp(c("#cc3300", "#ffcc00", "#99cc00")), domain = range(data$score))

leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(data = data %>% select(lat = decimalLatitude, lng = decimalLongitude), radius = 3, weight = 0, color = pal(data$score), fillOpacity = 1)

# netcdf/geotiff

require(ggplot2)
require(raster)

r <- raster("MYD28M_2016-09-01_rgb_3600x1800.TIFF")
plot(r)
r <- raster("A20162452016274.L3m_MO_SST_sst_4km.nc")
plot(r)
data <- occurrence("Millepora alcicornis")
#coords <- data %>% select(decimalLongitude, decimalLatitude)
coords <- data[,2:3]
data$t <- extract(r, coords)
ggplot() + geom_histogram(data = data, aes(x = t), bins = 40)
