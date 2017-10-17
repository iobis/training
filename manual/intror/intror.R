# Text after a # are comments
# Run every line of code line by line by putting the cursor at the line or
# selecting the code you want to run and hitting ctrl+enter in RStudio

# Get help
?data.frame
# search in all help files
??data.frame

# install a package from CRAN
install.packages("dplyr")
install.packages(c("reshape2", "ggplot2", "maptools", "rgdal", "leaflet", "xlsx"))
# load a package
library(ggplot2)

# install a package from github, this requires the devtools package to be installed
# so we do that first
install.packages("devtools")
devtools::install_github("iobis/robis")

# browse package vignettes
browseVignettes(package="ggplot2")

# Directly open a vignette
vignette("ggplot2-specs")

# Vectors: one-dimensional data structures

# single values are vectors of length 1
# numbers
a <- 1
a # [1] 1
class(a) # [1] "numeric"
length(a) # [1] 1

# text
b <- "banana"
b # [1] "banana"
class(b) # [1] "character"

# TRUE/FALSE (= booleans)
d <- FALSE
d # [1] FALSE
class(d) # [1] "logical"

# vector with 2 numbers, all elements in a vector have the same class
a <- c(1, 2)
a # 1 2
class(a) # [1] "numeric"
length(a) # [1] 2

# 1 to 10
b <- 1:10
b # [1] 1  2  3  4  5  6  7  8  9 10
# even numbers to 10
a <- seq(0, 10, by=2)
a # [1]  0  2  4  6  8 10
length(a) # [1] 6

# empty vector is known as NULL
b <- c()
b # [1] NULL

# Matrices, two-dimensional data structures. Again, all elements are of the same class.

matrix(1:6, nrow=3, ncol=2)
#         [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6

# Data frames, in data frames the columns can be of different classes.

d <- data.frame(a=c(5, 6, 7), b=c("x", "y", "z"))
d
#   a b
# 1 5 x
# 2 6 y
# 3 7 z
# select column "a", which is the first column, this returns a vector when possible
d$a # [1] 5 6 7
d[,1] # [1] 5 6 7
d[,"a"] # [1] 5 6 7
d[1] # data frame with only the first column
d[,1,drop=FALSE] # data frame with only the first column
#   a
# 1 5
# 2 6
# 3 7
d[1,] # data frame with only the first row
#   a b
# 1 5 x

# demonstration of the 'dplyr' data frame wrapper
library(dplyr) # install.packages("dplyr")
data(iris)
tbl_df(iris)

# Lists, a collection of objects.
a <- data.frame(a=c(1, 2, 3), b=c("x", "y", "z"))
l <- list(a=a, b=1)
l
# $a
# a b
# 1 1 x
# 2 2 y
# 3 3 z
#
# $b
# [1] 1

# access the second element "b"
l$b # [1] 1
l[[2]] # [1] 1
l[["b"]] # [1] 1

# Writing and reading data
data <- data.frame(x=10:15, y=40:45) # some data

# Delimited text files
write.table(data, "data.txt", sep="\t", dec=".", row.names=FALSE)
data <- read.table("data.txt", header=TRUE, sep="\t", dec=".", stringsAsFactors=FALSE)
# comma , separated
write.csv(data, "data.csv", row.names=FALSE)
data <- read.csv("data.csv", stringsAsFactors=FALSE)
# dotcomma ; separated
write.csv2(data, "data2.csv", row.names=FALSE)
data <- read.csv2("data2.csv", stringsAsFactors=FALSE)

# remove created files
unlink(c("data.txt", "data.csv", "data2.csv"))

# Excel files

library(xlsx) # install.packages("xlsx")
write.xlsx(data, "data.xlsx", sheetName="intro", row.names = FALSE)
data <- read.xlsx("data.xlsx", 1, stringsAsFactors=FALSE)
data <- read.xlsx("data.xlsx", sheetName="intro", stringsAsFactors=FALSE)
unlink("data.xlsx")

# Reading from ZIP files
temp <- tempfile()
download.file("http://ipt.vliz.be/eurobis/archive.do?r=nsbs&v=1.1", temp)
data <- read.table(unz(temp, "occurrence.txt"), sep="\t", header=TRUE, stringsAsFactors=FALSE)
View(data)
unlink(c(temp,"occurrence.txt"))

# Reading shapefiles
# Example requires the 'maptools', 'rgdal' and 'ggplot2' packages for reading and visualizing
library(maptools) # install.packages("maptools")
library(rgdal) # install.packages("rgdal")
library(ggplot2) # install.packages("ggplot2")

download.file("http://iobis.org/geoserver/OBIS/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=OBIS:summaries&outputFormat=SHAPE-ZIP", destfile="summaries.zip")
unzip("summaries.zip")

shape <- readOGR("summaries.shp", layer="summaries")
shape@data$id <- rownames(shape@data)
df <- fortify(shape, region="id")
data <- merge(df, shape@data, by="id")

# plot the number of species
ggplot() +
  geom_polygon(data=data,
            aes(x=long, y=lat, group=group, fill=s),
            color='gray', size=.2) +
  scale_fill_distiller(palette = "Spectral")

# remove all summaries files related to the shapefile
unlink(list.files(".","^summaries[.](zip|shp|shx|dbf|cst|prj)"))

# Inspecting data

library(robis) # devtools::install_github("iobis/robis")
library(dplyr) # install.packages("dplyr")

data <- occurrence("Sargassum")

# for this example, convert back from data frame tbl (dplyr) to standard data frame
data <- as.data.frame(data)

head(data) # first 6 rows
head(data, n = 100) # first 100 rows
dim(data) # dimensions
nrow(data) # nmuber of rows
ncol(data) # number of columns
names(data) # column names
str(data) # structure of the data
summary(data) # summary of the data
View(data) # View the data

# now convert to data frame tbl (dplyr)
data <- tbl_df(data)

data
head(data)
print(data, n = 10)

# Filtering

library(robis) # devtools::install_github("iobis/robis")
library(dplyr) # install.packages("dplyr")

data <- occurrence("Sargassum")
View(data %>% filter(scientificName == "Sargassum muticum" & yearcollected > 2005))

# Reordering
View(data %>% arrange(datasetName, desc(eventDate)))

# Selecting and renaming columns
data %>% select(scientificName, eventDate, lon=decimalLongitude, lat=decimalLatitude)

# `select()` can be used with `distinct()` to find unique combinations of values:
data %>% select(scientificName, locality) %>% distinct()

# Adding columns
data %>% tbl_df %>%
  mutate(zone = .bincode(minimumDepthInMeters, breaks=c(0, 20, 100))) %>%
  select(minimumDepthInMeters, zone) %>%
  filter(!is.na(zone)) %>% print(n=100)

### Aggregation
data %>% summarise(lat_mean = mean(decimalLatitude), lat_sd = sd(decimalLatitude))
data %>% group_by(scientificName) %>% summarise(records=n(), datasets=n_distinct(datasetName))

# Restructuring
# Convert a dataset from OBIS to a matrix format, which is more suitable for community analysis:

library(robis)
library(reshape2)

data <- occurrence(resourceid = 586)
wdata <- dcast(data, locality ~ scientificName, value.var = "individualCount", fun.aggregate = sum)

# And the other way around, from wide format to long format:
ldata <- melt(wdata, variable.name = "scientificName", value.name = "individualCount")

# Plotting

# In this example, data for one species is extracted from an OBIS dataset.
# Density and depth are visualized using the `ggplot2` package:

library(robis)
library(dplyr)
library(reshape2)
library(ggplot2)

data <- occurrence(resourceid = 586)

afil <- data %>% filter(scientificName == "Amphiura filiformis") %>% group_by(locality) %>% summarise(n = mean(individualCount), lon = mean(decimalLongitude), lat = mean(decimalLatitude), depth = mean(minimumDepthInMeters))

ggplot() +
  geom_point(data = afil, aes(lon, lat, size = n, colour = depth)) +
  scale_colour_distiller(palette = "Spectral") +
  theme(panel.background = element_blank()) + coord_fixed(ratio = 1) + scale_size(range = c(2, 12))

# Mapping
# The `leaflet` package can be used to create interactive web based maps.
# The example below shows the results of an outlier analysis of Verruca stroemia occurrences:

library(leaflet)

data <- occurrence("Verruca stroemia")
data$qcnum <- qcflags(data$qc, c(24, 28))

colors <- c("red", "orange", "green")[data$qcnum + 1]

m <- leaflet()
m <- addProviderTiles(m, "CartoDB.Positron")
m <- addCircleMarkers(m, data=data.frame(lat=data$decimalLatitude, lng=data$decimalLongitude), radius=3, weight=0, fillColor=colors, fillOpacity=0.5)
m
