library(openxlsx)
library(gdata)

gbif <- read.table("GBIF_Animalia_NWPtlantic.csv", sep = "\t", header = TRUE, fill = TRUE, quote = "", stringsAsFactors = FALSE)
obis <- read.csv("OBIS_NWPacific_arctic_2017.csv", stringsAsFactors = FALSE)
taxonmatch <- read.xls("GBIF_TaxonMatched.xls", 1, header = TRUE, as.is = TRUE)

# add taxon match to GBIF data

library(dplyr)
gbif_2 <- left_join(gbif, taxonmatch, by = c("species" = "scientificname"))

# map some columns to DwC

library(parsedate)

gbif_3 <- gbif_2 %>% select(
  scientificName = accepted_name_aphia_worms,
  taxonRank = taxonrank,
  decimalLongitude = decimallongitude,
  decimalLatitude = decimallatitude,
  minimumDepthInMeters = depth,
  maximumDepthInMeters = depth,
  eventDate = eventdate,
  institutionCode = institutioncode,
  collectionCode = collectioncode,
  catalogNumber = catalognumber,
  scientificNameID = valid_aphiaid_worms
) %>% mutate(taxonRank = tolower(taxonRank), eventDate = substr(eventDate, 1, 10)) %>% filter(!is.na(scientificName))

obis_2 <- obis %>% select(
  scientificName = ScientificName_accepted,
  decimalLongitude = longitude,
  decimalLatitude = latitude,
  minimumDepthInMeters = depth,
  maximumDepthInMeters = depth,
  eventDate = datecollected
) %>% mutate(eventDate = format(parse_date(eventDate), "%Y-%m-%d"))

# merge GBIF and OBIS

merged <- bind_rows(obis_2, gbif_3)
merged_distinct <- merged %>% distinct(scientificName, eventDate, decimalLongitude, decimalLatitude)
