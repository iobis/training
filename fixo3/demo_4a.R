require(vegan)
require(reshape2)

# in this dataset, biomass records have been removed,
# and replicates have been averaged

data <- read.csv("data_embos/EMBOSdata.csv", sep = "\t")

# reshape data (row to columns)

data <- dcast(data, stationCode ~ WoRMS_scientificName, value.var = "count")

# use station code column as row names

row.names(data) <- data$stationCode
data <- data[,-1]

# fill empty cells

data[is.na(data)] <- 0

# MDS

ord <- metaMDS(data)
plot(ord, type = "n")
orditorp(ord, display = "spec", cex = 0.5, col = "#aaaaaa")
orditorp(ord, display = "sites", cex = 0.7, col = "#cc3300")

# clustering

dis <- vegdist(data) # Bray-Curtis
image(as.matrix(dis))
clus <- hclust(dis)
plot(clus)
rect.hclust(clus, 5)

grp <- cutree(clus, 5) # cut tree

# biodiversity indices

diversity(data, index = "shannon")
diversity(data, index = "simpson")

barplot(diversity(data), las = 2, cex.names = 0.5, cex.axis = 0.5, cex.lab = 0.7, ylab = "Shannon")
