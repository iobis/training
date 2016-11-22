library(taxizesoap)
library(robis)
library(ggmap)
library(dplyr)
library(ncdf4)
library(raster)
library(stringr)
library(readr)
library(pryr)
library(marmap)
library(lubridate)
library(broom)
library(mregions)
library(rgeos)

# taxonomy

sorby_coll <- read_csv("data/sorby_collection.csv")
glimpse(sorby_coll)

my_sp <- sorby_coll$taxon_name[1]
my_sp_aphia <- get_wormsid(searchterm = my_sp, accepted = FALSE)
my_sp_taxo <- worms_records(ids = my_sp_aphia, marine_only = TRUE)
glimpse(my_sp_taxo)

my_taxa <- unique(sorby_coll$taxon_name)
system.time(
  all_spp_aphia <- get_wormsid(searchterm = my_taxa, accepted = FALSE)
)
table(attr(all_spp_aphia, "match"))
sorby_coll$aphia_id <- as.vector(all_spp_aphia)

aphia_eg <- get_wormsid(searchterm = "cod", searchtype = "common", accepted = FALSE)
aphia_eg <- get_wormsid(searchterm = "cod", searchtype = "common", accepted = FALSE, ask = FALSE)

all_spp_taxa <- tbl_df(worms_records(ids = sorby_coll$aphia_id, marine_only = TRUE))
glimpse(all_spp_taxa)
taxa_to_check <- my_taxa[attr(all_spp_aphia, "match") == "not found"]
worms_records(scientific = taxa_to_check, marine_only = FALSE)

sorby_coll <- left_join(sorby_coll, all_spp_taxa, by = c("aphia_id" = "inputid"))
glimpse(sorby_coll)

sorby_coll <- subset(sorby_coll, !is.na(valid_AphiaID) & !is.na(valid_name))
table(sorby_coll$rank)
sorby_coll <- subset(sorby_coll, rank == "Species")

worms_records(ids = "100982")

get_pisces <- function(aphiaid){
  # get hierarchy for a given aphia ID
  if(exists("aphia_h")){rm(aphia_h)}
  pisces <- FALSE
  try(aphia_h <- worms_hierarchy(ids = aphiaid), silent = T)
  if(exists("aphia_h")){
    if("Pisces" %in% aphia_h$scientificname){
      pisces <- TRUE
    }
  }
  return(pisces)    
}
get_pisces(get_wormsid("Gadus morhua", verbose = FALSE))
sorby_coll$pisces <- with(sorby_coll, sapply(valid_AphiaID, function(valid_AphiaID){get_pisces(valid_AphiaID)}))
glimpse(subset(sorby_coll, pisces == TRUE))

# get occurrences

my_occs <- occurrence(scientificname = sorby_coll$valid_name[1])
bb_occs <- bbox(cbind(my_occs$decimalLongitude, my_occs$decimalLatitude))
bb_occs

world <- map_data("world")
worldmap <- ggplot(world, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  theme(panel.background = element_rect(fill = "steelblue")) +
  coord_equal()
occ_map <- worldmap + geom_point(data = my_occs, aes(x = decimalLongitude, y = decimalLatitude),
                                 colour = "darkorange", shape = 21, alpha = 2/3)
occ_map
occ_map + coord_map("ortho")

table(my_occs$geodeticDatum)

my_map <- get_map(
  location = bb_occs, maptype = "satellite"
)
ggmap(my_map) +
  geom_point(data = my_occs, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkorange", shape = 21, alpha = 2/3)

worldmap + geom_point(data = my_occs,
                      aes(x = decimalLongitude, y = decimalLatitude, colour = originalScientificName), shape = 21, alpha = 2/3)

my_occs$decade <- with(my_occs, 10*round(yearcollected/10, 0))
worldmap + geom_point(data = my_occs,
                      aes(x = decimalLongitude, y = decimalLatitude, colour = decade), shape = 21, alpha = 2/3) +
  scale_colour_gradient(low = "white", high = "darkorange")

sole_occs <- occurrence(scientificname = "Solea solea")
object_size(sole_occs)
object_size(sole_occs[, 1:4])

bb_sole <- bbox(SpatialPoints(cbind(sole_occs$decimalLongitude, sole_occs$decimalLatitude)))
bb_sole
sole_map <- get_map(
  location = bb_sole, maptype = "satellite"
)
(sole_map <- ggmap(sole_map) +
  geom_point(data = sole_occs, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkorange", shape = 21, alpha = 2/3)
)

obis_map <- function(occ_dat, map_type = c("satellite", "world"), map_zoom = NULL, plotit = TRUE){
  
  bb_occ <- bbox(cbind(occ_dat$decimalLongitude, occ_dat$decimalLatitude))
  
  if(map_type == "satellite"){
    if(is.null(map_zoom)){
      base_map <- get_map(location = bb_occ, maptype = "satellite")
    } else {
      base_map <- get_map(location = bb_occ, maptype = "satellite", zoom = map_zoom)
    }
    obis_map <- ggmap(base_map)
  } else if(map_type == "world"){
    base_map <- map_data("world")
    obis_map <- ggplot(base_map, aes(x=long, y=lat)) +
      geom_polygon(aes(group=group)) +
      scale_y_continuous(breaks = (-2:2) * 30) +
      scale_x_continuous(breaks = (-4:4) * 45) +
      theme(panel.background = element_rect(fill = "steelblue")) +
      coord_equal()
  } else {
    stop("map_type must be one of 'satellite' or 'world'",
         call. = FALSE)
  }
  
  # Now add the occurrence points
  obis_map <- obis_map + geom_point(data = occ_dat, aes(x = decimalLongitude, y = decimalLatitude),
                                    colour = "darkorange", shape = 21, alpha = 2/3)
  
  if(plotit == T){print(obis_map)}
  
  return(obis_map)
  
}
sole_map <- obis_map(sole_occs, map_type = "satellite", plotit = FALSE)

sole_summ <- checklist(scientificname = "Solea solea")
sole_summ$records

taxa_summ <- with(sorby_coll, sapply(valid_name, function(valid_name){checklist(scientificname = valid_name)}))
lengths(taxa_summ)
table(lengths(taxa_summ))
lapply(taxa_summ, names)

eg_taxon <- checklist(scientificname = "Ciona intestinalis")
table(eg_taxon$rank_name)
obis_n <- tbl_df(data.frame(
  valid_name = names(taxa_summ),
  obis_n = unlist(lapply(taxa_summ, function(m) sum(m$records)))
))
sorby_coll <- left_join(sorby_coll, obis_n, by = "valid_name")
dplyr::select(sorby_coll, valid_name, obis_n)

ggplot(sorby_coll, aes(x = obis_n)) + geom_histogram() + scale_x_log10()
sum(sorby_coll$obis_n)

allspp_obis <- with(sorby_coll, sapply(valid_name, function(valid_name){
  occurrence(scientificname = valid_name,
             fields = c("scientificName", "decimalLongitude", "decimalLatitude", "depth", "yearcollected"))}))
allspp_obis <- bind_rows(apply(allspp_obis, 2, as.data.frame))
object_size(allspp_obis)
identical(nrow(allspp_obis), sum(sorby_coll$obis_n))
ggplot(allspp_obis, aes(x = depth)) + geom_histogram()
quantile(allspp_obis$depth, 0:4/4, na.rm = T)
sum(allspp_obis$depth == -9, na.rm = T)

ggplot(allspp_obis, aes(x = yearcollected)) + geom_freqpoly(binwidth = 5)

ggplot(allspp_obis, aes(x = yearcollected, colour = scientificName)) + geom_freqpoly(binwidth = 5)+
  theme(legend.position = "top") +
  theme(legend.text = element_text(size = 5)) +
  theme(legend.title = element_blank())

ggplot(allspp_obis, aes(x = yearcollected)) + geom_freqpoly(binwidth = 5) +
  facet_wrap(~ scientificName, scales = "free_y")+
  theme(strip.text.x = element_text(size = 4)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))

worldmap + geom_point(data = allspp_obis,
                      aes(x = decimalLongitude, y = decimalLatitude), colour = "darkorange", shape = 21, alpha = 2/3)

global_grid <- raster(nrows = 180/5, ncol = 360/5, xmn = -180, xmx = 180, ymn = -90, ymx = 90)
lonlat_sp <- cbind(x = allspp_obis$decimalLongitude, y = allspp_obis$decimalLatitude)
gridded_occs <- rasterize(x = lonlat_sp, y = global_grid, fun = "count")
obis.p <- data.frame(rasterToPoints(gridded_occs))
names(obis.p) <- c("longitude", "latitude", "OBIS")
(worldmap + geom_raster(data = obis.p, aes(x = longitude, y = latitude, fill = OBIS)) + 
  scale_fill_gradient(low = "white", high = "darkorange")
)
(gridded_obis <- worldmap + geom_raster(data = obis.p, aes(x = longitude, y = latitude, fill = log10(OBIS))) + 
  scale_fill_gradient(low = "white", high = "darkorange")
)
worldmap + geom_raster(data = obis.p, aes(x = longitude, y = latitude, fill = log10(OBIS)), alpha = 2/3) + 
  scale_fill_gradient(low = "white", high = "darkorange")

round(sort(apply(!is.na(sole_occs), 2, mean)), 3)
table(sole_occs$originalScientificName)
table(as.numeric(sole_occs$coordinateUncertaintyInMeters))
sole_occs$qc[1]
intToBits(sole_occs$qc[1])
as.logical(intToBits(sole_occs$qc[1]))

filter_by_qc_flags <- function(occ_dat, qc_var = "qc", qc_flags){
  
  get_allon_ids <- function(qc_var, qc_flags){
    
    mask <- 2^(qc_flags - 1)
    qc_flags_on <- sapply(qc_var, function(x) {sum(bitwAnd(x, mask) > 0)})
    all_on <- which(qc_flags_on == length(qc_flags))
    all_on	
  }
  
  if(min(qc_flags, na.rm = T) < 1 | max(qc_flags, na.rm = T) > 30 |
     !(class(qc_flags) %in% c("numeric", "integer"))){
    stop("Invalid values for qc_flags, must be integers in the range 1:30",
         call. = FALSE)
  }
  
  
  if(sum(c(8, 9, 20) %in% qc_flags) > 0){
    stop("Flags 8, 9 and 20 are currently disabled and no records would be returned by your query",
         call. = FALSE)
  }
  
  if(qc_var != "qc"){occ_dat <- plyr::rename(occ_dat, setNames('qc', eval(qc_var)))}
  id_all_on <- get_allon_ids(occ_dat$qc, qc_flags)
  
  occ_dat <- occ_dat[id_all_on, ]
  
  if(qc_var != "qc"){occ_dat <- plyr::rename(occ_dat, setNames(eval(qc_var), 'qc'))}
  
  return(occ_dat)
  
}

sole_qc_filt <- filter_by_qc_flags(sole_occs, qc_flags = c(1:7, 11:15))
nrow(sole_occs) - nrow(sole_qc_filt)
sum(sole_occs$depth == -9, na.rm = T)

dodgy_depths <- with(sole_occs, tapply(depth == -9, datasetName, sum, na.rm = T))
dodgy_depths <- data.frame(dataset = names(dodgy_depths), n_depths = as.vector(dodgy_depths))
subset(dodgy_depths, n_depths > 0)

sole_occs_refined <- filter(
  sole_occs, !is.na(depth) & !is.na(yearcollected) & !is.na(individualCount) & depth != -9)
nrow(sole_occs_refined)
nrow(sole_occs) - nrow(sole_occs_refined)

sole_occs_new <- occurrence("Solea solea",
                            fields = c("decimalLongitude", "decimalLatitude", "yearcollected", "depth", "qc"),
                            startdate = as.Date("2010-01-01"), qc = c(1:7, 19)
)

bb_sole <- bbox(SpatialPoints(cbind(sole_occs_refined$decimalLongitude, sole_occs_refined$decimalLatitude)))
sole_bathy <- getNOAA.bathy(
  lon1 = bb_sole[1, 1], lon2 = bb_sole[1, 2],
  lat1 = bb_sole[2, 1], lat2 = bb_sole[2, 2],
  resolution = 1, keep = TRUE, antimeridian = FALSE)
sole_bathy_r <- marmap::as.raster(sole_bathy)
extract(sole_bathy_r, dplyr::select(sole_occs_refined, decimalLongitude, decimalLatitude)[1,])
sole_occs_refined$bottom_depth <- extract(
  sole_bathy_r, dplyr::select(sole_occs_refined, decimalLongitude, decimalLatitude))
sole_occs_refined$bottom_depth <- -sole_occs_refined$bottom_depth
ggplot(sole_occs_refined, aes(x = bottom_depth, y = depth)) + geom_point(colour = "steelblue", alpha = 1/3) +
  geom_abline(slope = 1, intercept = 0) +
  geom_vline(xintercept = 0)

jelly_occs <- occurrence(scientificname = "Aurelia aurita")
jelly_occs <- filter(
  jelly_occs, !is.na(depth) & depth != -9)
nrow(jelly_occs)
bb_jelly <- bbox(SpatialPoints(cbind(jelly_occs$decimalLongitude, jelly_occs$decimalLatitude)))
jelly_occs$bottom_depth <- extract(
  sole_bathy_r, dplyr::select(jelly_occs, decimalLongitude, decimalLatitude))
jelly_occs <- filter(jelly_occs, !is.na(bottom_depth))
jelly_occs$bottom_depth <- -jelly_occs$bottom_depth
sole_jelly <- rbind(
  dplyr::select(sole_occs_refined, scientificName, decimalLongitude, decimalLatitude, depth, bottom_depth),
  dplyr::select(jelly_occs, scientificName, decimalLongitude, decimalLatitude, depth, bottom_depth)
)

(sole_v_jelly <- ggplot(sole_jelly, aes(x = bottom_depth, y = depth)) +
  geom_point(aes(colour = scientificName), alpha = 1/3) +
  geom_smooth(aes(colour = scientificName), method = "lm") +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  geom_abline(slope = 1, intercept = 0) +
  geom_vline(xintercept = 0)
)

get_bottom_depth <- function(occ_dat, bathy_res = 10, bathy_keep = TRUE, bathy_antimerid = FALSE){
  
  # occ_dat is assumed to be a dataframe as returned from robis::occurrence, including as a minimum the fields:
  # decimalLongitude, decimalLatitude, depth
  # Other arguments are passed to marmap. The default resolution is 10 minutes,
  # defaults for keep and antimeridion are T and F respectively
  
  # This gets the bathymetry and converts it to a raster
  bb_occs <- bbox(cbind(occ_dat$decimalLongitude, occ_dat$decimalLatitude))
  occs_bathy <- marmap::as.raster(
    getNOAA.bathy(
      lon1 = bb_occs[1, 1], lon2 = bb_occs[1, 2],
      lat1 = bb_occs[2, 1], lat2 = bb_occs[2, 2],
      resolution = bathy_res, keep = bathy_keep, antimeridian = bathy_antimerid
    )
  )
  
  # This creates the bottom_depth variable in occ dat
  occ_dat$bottom_depth <- -extract(
    occs_bathy, dplyr::select(occ_dat, decimalLongitude, decimalLatitude)
  )
  
  # record the bathymetry resolution used as an attribute of occ_dat
  attr(occ_dat, "bathymetry_res") <- bathy_res
  
  # return the occ_dat dataframe with bottom depth added
  return(occ_dat)
  
}
bathy_test <- get_bottom_depth(sole_occs_refined)

sst_prep <- function(path = "~/.spenv/noaa_sst") {
  x <- file.path(path, "sst.mnmean.nc")
  if (!file.exists(x)) {
    dir.create(dirname(x), recursive = TRUE, showWarnings = FALSE)
    download.file("ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc", destfile = x)
  }
  raster::brick(x, varname = "sst")
}
sst_dat <- sst_prep()
sst_dat
plot(sst_dat)
dim(sst_dat)

sp_extract_gridded_date <- function(x, from = "noaa_sst", latitude = NULL,
                                    longitude = NULL, samp_date = NULL, origin = as.Date("1800-1-1")) {
  
  x <- spenv_guess_latlondate(x, latitude, longitude, samp_date)
  switch(from,
         noaa_sst = {
           mb <- sst_prep()
           out <- list()
           x <- x[ !is.na(x$date), ]
           x$date <- as.Date(x$date)
           x <- x[x$date >= min(mb@z[["Date"]]), ]
           x$lon_adj <- x$longitude
           x$lon_adj[x$lon_adj < 0] <- x$lon_adj[x$lon_adj < 0] + 360
           for (i in seq_len(NROW(x))) {
             out[[i]] <- get_env_par_space_x_time(mb, x[i, ], origin = origin)
           }
           x$sst <- unlist(out)
           x
         }
  )
}

spenv_guess_latlondate <- function(x, lat = NULL, lon = NULL, samp_date = NULL) {
  xnames <- names(x)
  if (is.null(lat) && is.null(lon)) {
    lats <- xnames[grep("^(lat|latitude)$", xnames, ignore.case = TRUE)]
    lngs <- xnames[grep("^(lon|lng|long|longitude)$", xnames, ignore.case = TRUE)]
    
    if (length(lats) == 1 && length(lngs) == 1) {
      if (length(x) > 2) {
        message("Assuming '", lngs, "' and '", lats,
                "' are longitude and latitude, respectively")
      }
      x <- rename(x, setNames('latitude', eval(lats)))
      x <- rename(x, setNames('longitude', eval(lngs)))
    } else {
      stop("Couldn't infer longitude/latitude columns, please specify with 'lat'/'lon' parameters", call. = FALSE)
    }
  } else {
    message("Using user input '", lon, "' and '", lat,
            "' as longitude and latitude, respectively")
    x <- plyr::rename(x, setNames('latitude', eval(lat)))
    x <- plyr::rename(x, setNames('longitude', eval(lon)))
  }
  
  if(is.null(samp_date)){
    dates <- xnames[grep("date", xnames, ignore.case = TRUE)]
    if(length(dates) == 1){
      if(length(x) > 2){
        message("Assuming '", dates, "' are sample dates")
      }
      x <- rename(x, setNames('date', eval(dates)))
    } else {
      stop("Couldn't infer sample date column, please specify with 'date' parameter", call. = FALSE)
    }   
    
  } else {
    message("Using user input '", samp_date, "' as sample date")
    x <- plyr::rename(x, setNames('date', eval(samp_date)))
  }
  
}

get_env_par_space_x_time <- function(
  env_dat, occ_dat, origin = as.Date("1800-1-1")){
  
  # calculate starting julian day for each month in env_dat
  month_intervals <- as.numeric(env_dat@z[["Date"]] - origin)
  # calculate julian day for the focal date (eventDate in occ_dat)
  focal_date <- as.numeric(occ_dat$date - origin)
  
  # extract environmental variable (SST here) for this point
  as.numeric(raster::extract(
    env_dat,
    cbind(occ_dat$lon_adj, occ_dat$latitude),
    layer = findInterval(focal_date, month_intervals),
    nl = 1
  ))
}

sst_prep <- function(path = "~/.spenv/noaa_sst") {
  x <- file.path(path, "sst.mnmean.nc")
  if (!file.exists(x)) {
    dir.create(dirname(x), recursive = TRUE, showWarnings = FALSE)
    download.file("ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc", destfile = x)
  }
  raster::brick(x, varname = "sst")
}

sole_sst <- sp_extract_gridded_date(x = sole_occs_refined,
                                    latitude = "decimalLatitude", longitude = "decimalLongitude", samp_date = "eventDate")
nrow(sole_sst)
sum(sole_occs_refined$yearcollected < 1981)
sole_sst$month <- month(sole_sst$date)
(sole_sst_plot <- (ggplot(sole_sst, aes(x = longitude, y = latitude)) +	
                     geom_point(aes(colour = sst), alpha = 2/3) +
                     scale_colour_gradient(low = "blue", high = "red") +
                     facet_wrap(~ month))
)

sum(!duplicated(dplyr::select(sole_occs_refined, decimalLatitude, decimalLongitude, eventDate)))
sole_occs_refined$lat <- round(sole_occs_refined$decimalLatitude)
sole_occs_refined$lon <- round(sole_occs_refined$decimalLongitude)
sole_occs_refined$yr_month <- with(sole_occs_refined, paste(yearcollected, month(eventDate), sep = "_"))
sum(!duplicated(dplyr::select(sole_occs_refined, lat, lon, yr_month)))

sole_temp <- sole_occs_refined[!duplicated(dplyr::select(sole_occs_refined, lat, lon, yr_month)), ]
sole_sst2 <- sp_extract_gridded_date(x = sole_temp,
                                     latitude = "decimalLatitude", longitude = "decimalLongitude", samp_date = "eventDate")
sole_sst2$loc_date <- with(sole_sst2, paste(lat, lon, yr_month, sep = "_"))
sole_occs_refined$loc_date <- with(sole_occs_refined, paste(lat, lon, yr_month, sep = "_"))
sole_sst2 <- dplyr::select(sole_sst2, loc_date, sst)
sole_occs_refined <- left_join(sole_occs_refined, sole_sst2, by = "loc_date")
rm(sole_temp, sole_sst2)
sole_occs_refined$month <- month(sole_occs_refined$eventDate)
sole_sst_plot <- ggplot(subset(
  sole_occs_refined, !is.na(sst)), aes(x = decimalLongitude, y = decimalLatitude)) +	
  geom_point(aes(colour = sst), alpha = 2/3) +
  scale_colour_gradient(low = "blue", high = "red") +
  facet_wrap(~ month)
fivenum(sole_occs_refined$sst)
(sole_sst_trends <- ggplot(subset(sole_occs_refined, !is.na(sst)), aes(x = yearcollected, y = sst)) +	
  geom_point(colour = "steelblue", alpha = 1/3) +
  geom_smooth(method = "lm", colour = "darkorange") +
  facet_wrap(~ month)
)

env_grid <- raster(read.asciigrid("data/primprod_chla/aq_primprod.asc"))
object_size(env_grid)
plot(env_grid)
range(env_grid@data@values, na.rm = T)
hist(env_grid@data@values)

env_occ <- tbl_df(data.frame(extract(
  env_grid, cbind(sole_occs_refined$decimalLongitude, sole_occs_refined$decimalLatitude), cellnumbers = T)))
sole_occs_refined$chla <- extract(env_grid,
                                  cbind(sole_occs_refined$decimalLongitude, sole_occs_refined$decimalLatitude))
env_grid <- raster(read.asciigrid("data/btemp_k/kg_b_temp.asc"))
plot(env_grid)

range(env_grid@data@values, na.rm = T)
hist(env_grid@data@values)

sole_occs_refined$bottom_temp <- extract(env_grid,
                                         cbind(sole_occs_refined$decimalLongitude, sole_occs_refined$decimalLatitude))
fivenum(sole_occs_refined$bottom_temp)

basking_shark <- occurrence(scientificname = "Cetorhinus maximus",
                            geometry = "POLYGON ((-10 50, -10 60, 0 60, 0 50, -10 50))")
basking_map <- obis_map(basking_shark, map_type = "satellite", map_zoom = 4, plotit = F)
basking_map + geom_rect(aes(xmin = -10, xmax = 0, ymin = 50, ymax = 60), colour = "green", fill = NA)

uk_eez <- mr_shp("MarineRegions:eez", maxFeatures = NULL, filter = "United Kingdom Exclusive Economic Zone")
uk_eez_simple <- SpatialPolygonsDataFrame(gSimplify(uk_eez, tol = 0.01, topologyPreserve = TRUE), data = uk_eez@data)
uk_eez_df <- tidy(uk_eez_simple)

basking_shark <- occurrence(scientificname = "Cetorhinus maximus",
                            geometry = mr_as_wkt(uk_eez_simple))

basking_map <- obis_map(basking_shark, map_type = "satellite", map_zoom = 4, plotit = F)
basking_map +
  geom_polygon(data = uk_eez_df, aes(x = long, y = lat, group = group),
               colour = "green", fill = NA, size = 0.25)

uk_basking_shark <- checklist(scientificname = "Cetorhinus maximus",
                              geometry = mr_as_wkt(uk_eez_simple))

geo <- mr_geojson(key = "MarineRegions:eez", filter = "Albanian Exclusive Economic Zone", maxFeatures = NULL)
alb_eez <- mr_as_wkt(geo, fmt = 5)
alb_taxa <- tbl_df(checklist(geometry = alb_eez))
alb_species <- filter(alb_taxa, rank_name == "Species")

reg_types <- mr_place_types()
sort(reg_types$type)
reg_types[grepl("IHO", reg_types[, 1], ignore.case = TRUE), ]
reg_types[grepl("ICES", reg_types[, 1], ignore.case = TRUE), ]
reg_types[grepl("eez", reg_types[, 1], ignore.case = TRUE), ]
eez_ids <- tbl_df(mr_records_by_type(type = "EEZ"))
iho_ids <- tbl_df(mr_records_by_type(type = "IHO Sea Area"))

all_mr_records_by_type <- function(mr_type){
  
  mr_ids <- mr_records_by_type(type = mr_type)
  if(nrow(mr_ids) == 100){
    new_q <- TRUE
    while(new_q == T){
      next_recs <- mr_records_by_type(type = mr_type, offset = nrow(mr_ids))
      if(length(next_recs) == 0){
        new_q <- FALSE
      } else {
        mr_ids <- rbind(mr_ids, next_recs)
        if(nrow(eez_ids) %% 100 != 0){new_q <- FALSE}				
      }
    }
  }
  return(tbl_df(mr_ids))
  
}

eez_ids <- all_mr_records_by_type(mr_type = "EEZ")
sort(eez_ids$preferredGazetteerName)
tax_list <- tbl_df(checklist(geometry = mr_as_wkt(mr_geojson(key = "MarineRegions:eez", filter = eez_ids$preferredGazetteerName[1]), fmt = 5)))
attr(tax_list, "region_name") <- eez_ids$preferredGazetteerName[1]
attr(tax_list, "region_name")

rnames <- mr_names()
mr_names_search(rnames, "IHO")
iho_regions <- mr_shp(key = "MarineRegions:iho", maxFeatures = 1000)
wgrid <- mr_shp(key = "World:grid5deg", maxFeatures = 1000) 
sort(rnames$name)

region_map <- function(regions, poly_fill = TRUE, fill_by = NULL, plotit = TRUE){
  
  # get the base map
  base_map <- map_data("world")
  region_map <- ggplot(base_map, aes(x=long, y=lat)) +
    geom_polygon(aes(group=group)) +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45) +
    theme(panel.background = element_rect(fill = NA)) +
    coord_equal()
  
  # process the regions data into a dataframe, if necessary; add region names, and plot as polygons
  if(class(regions) == "SpatialPolygonsDataFrame"){
    regions@data$id_new <- rownames(regions@data)
    # Create the dataframe
    regions_df <- tidy(regions)
    # Add missing info - specifically region name
    regions_df <- left_join(regions_df, regions@data, by = c("id" = "id_new"))
    
  } else {
    regions_df <- regions
  }
  
  if(is.null(fill_by)){
    if(poly_fill == TRUE){
      region_map <- region_map +
        geom_polygon(data = regions_df,	
                     aes(x = long, y = lat, group = group, fill = name), alpha = 1/3) +
        theme(legend.position = "top") +
        theme(legend.text = element_text(size = 4)) +
        theme(legend.title = element_blank())
    } else {
      region_map <- region_map +
        geom_polygon(data = regions_df,	
                     aes(x = long, y = lat, group = group), fill = NA, colour = "green")
    }
  } else {
    id_fill <- which(names(regions_df) %in% fill_by)
    names(regions_df)[id_fill] <- "fill_var"
    region_map <- region_map +
      geom_polygon(data = regions_df,	
                   aes(x = long, y = lat, group = group, fill = fill_var)) +
      scale_fill_gradient(name = fill_by) +
      theme(legend.position = "top")
  }
  
  if(plotit == TRUE){print(region_map)}
}

region_map(regions = iho_regions)
region_map(regions = iho_regions, poly_fill = FALSE)

samp_obis <- sample_n(allspp_obis, 10000)
samp_obis_spat <- SpatialPointsDataFrame(
  coords = cbind(samp_obis$decimalLongitude, samp_obis$decimalLatitude),
  data = samp_obis, proj4string = crs(iho_regions))
allspp_obis_regionid <- over(samp_obis_spat, iho_regions)
head(allspp_obis_regionid)
samp_obis$iho_region <- allspp_obis_regionid$name
head(samp_obis)

by_region <- group_by(samp_obis, iho_region)
iho_summaries <- dplyr::summarise(by_region,
                                  n_obis_records = n(),
                                  n_species = n_distinct(scientificName)
)
iho_regions@data$id_new <- rownames(iho_regions@data)
iho_regions_df <- tidy(iho_regions)
iho_regions_df <- left_join(iho_regions_df, iho_regions@data, by = c("id" = "id_new"))
head(iho_regions_df)
iho_regions_df <- left_join(iho_regions_df, iho_summaries, by = c("name" = "iho_region"))
region_map(iho_regions_df, fill_by = "n_obis_records")
iho_regions_df$log_OBIS_records <- log10(iho_regions_df$n_obis_records)
region_map(iho_regions_df, fill_by = "log_OBIS_records")


n_atlantic <- subset(iho_regions, name == "North Atlantic Ocean")
region_map(n_atlantic)
region_bb <- bbox(n_atlantic)
region_bathy <- marmap::as.raster(
  getNOAA.bathy(
    lon1 = region_bb[1, 1], lon2 = region_bb[1, 2],
    lat1 = region_bb[2, 1], lat2 = region_bb[2, 2],
    resolution = 10, keep = TRUE, antimeridian = FALSE
  )
)
plot(region_bathy)

region_bathy <- mask(region_bathy, n_atlantic)
plot(region_bathy)

cellStats(region_bathy, stat = range)
cellStats(region_bathy > 0, stat = sum)

mindepth <- 0
maxdepth <- -500
cellStats(region_bathy <= mindepth & region_bathy >= maxdepth, stat = sum)
id_out_of_depth <- which(values(region_bathy) < maxdepth | values(region_bathy) > mindepth)
region_bathy_cut <- region_bathy
region_bathy_cut[id_out_of_depth] <- NA
plot(region_bathy_cut)

region_bathy_poly <- rasterToPolygons(region_bathy, fun = function(x){x <= mindepth & x >= maxdepth})
region_bathy_poly <- gUnaryUnion(region_bathy_poly)
plot(region_bathy_poly)

value_within_region <- extract(region_bathy, y = region_bathy_poly, df = T)
head(value_within_region)

names(value_within_region)[2] <- "env_variable"
range(value_within_region$env_variable)

bt_within_region <- extract(env_grid, y = region_bathy_poly, df = T)
head(bt_within_region)
fivenum(bt_within_region[,2])
min_bt_poly <- rasterToPolygons(env_grid, fun = function(x){x >= 15})
min_bt_poly <- gUnaryUnion(min_bt_poly)
plot(min_bt_poly)
region_bathy_poly <- rasterToPolygons(region_bathy, fun = function(x){x < -1000})
region_bathy_poly <- gUnaryUnion(region_bathy_poly)
plot(region_bathy_poly)

deep_mammals <- checklist(scientificname = "Mammalia", geometry = writeWKT(region_bathy_poly))
deep_mammals <- tbl_df(subset(deep_mammals, rank_name == "Species"))
deep_mammals


