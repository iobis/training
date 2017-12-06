install.packages("devtools")

library(devtools)
install_github("iobis/robis")

library(robis)
ptevol <- occurrence("Pterois volitans")
View(ptevol)

islands <- occurrence(geometry = "POLYGON ((102.68921 6.05862, 102.57111 5.95346, 103.07785 5.49980, 103.25226 5.62555, 103.07648 5.87970, 102.68921 6.05862))")
View(islands)
leafletmap(islands)

roughy_shallow <- occurrence("Hoplostethus atlanticus", enddepth = 400)
leafletmap(roughy_shallow)

lionfish_native <- occurrence("Pterois volitans", enddate = "1980-01-01")
leafletmap(lionfish_native)

crete <- occurrence(resourceid = 3185)
table(crete$order)

names(islands)

table(islands$phylum)
table(islands$family, islands$phylum)

islands$year <- as.numeric(format(as.Date(islands$eventDate), "%Y"))
table(islands$year)

install.packages("ggplot2")
library(ggplot2)

ggplot() +
  geom_histogram(data = islands, aes(x = year, fill = phylum), binwidth = 5) +
  scale_fill_brewer(palette = "Paired")

install.packages("dplyr")
library(dplyr)

library(robis)
lag <- occurrence("Lagis")
leafletmap(lag)
table(lag$species)

lagaus <- lag %>% filter(species == "Lagis australis")
leafletmap(lagaus)

bew <- lag %>% filter(grepl("BEWREMABI", datasetName))
leafletmap(bew)

lagaus %>% select(decimalLongitude, decimalLatitude)
lagaus %>% select(decimalLongitude, decimalLatitude) %>% distinct()
lagaus %>% distinct(decimalLongitude, decimalLatitude)
lagaus %>% select(starts_with("decimal"))

lag %>%
  group_by(species) %>%
  summarize(
    records = n(),
    medlon = round(median(decimalLongitude)),
    medlat = round(median(decimalLatitude)),
    minlon = round(min(decimalLongitude)),
    maxlon = round(max(decimalLongitude)),
    minlat = round(min(decimalLatitude)),
    maxlat = round(max(decimalLatitude))
  )

head(lag %>% arrange(eventDate)) %>% select(eventDate, decimalLongitude, decimalLatitude, datasetName)
head(lag %>% arrange(desc(eventDate))) %>% select(eventDate, decimalLongitude, decimalLatitude, datasetName)

lag_withyear <-lag %>% mutate(year = format(as.Date(eventDate), "%Y"))
