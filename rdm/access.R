# install robis

if (!require("devtools")) { install.packages("devtools") }
if (!require("robis")) { devtools::install_github("iobis/robis") }

# load robis

library(robis)

# fetching all occurrences for a taxon

ptevol <- occurrence("Pterois volitans")

View(ptevol)
leafletmap(ptevol)
leafletmap(ptevol, popup = "catalogNumber")

# geometry filter - http://iobis.org/maptool

pol <- occurrence("Polychaeta", geometry = "POLYGON ((2.56119 51.07506, 2.38953 51.27051, 3.08167 51.55573, 3.32062 51.43090, 3.36731 51.35720, 2.56119 51.07506))")
leafletmap(pol)
table(pol$genus)

# depth filter

roughy <- occurrence("Hoplostethus atlanticus", startdepth = 400)
leafletmap(roughy)

# date filter

lionfish_native <- occurrence("Pterois volitans", enddate = "1980-01-01")
leafletmap(lionfish_native)

lionfish_recent <- occurrence("Pterois volitans", startdate = "2015-01-01")
leafletmap(lionfish_recent)

# by dataset - http://iobis.org/explore/#/dataset/4307

ccz <- occurrence(resourceid = 4307)
leafletmap(ccz)

# reading directly from IPT - http://ipt.vliz.be/eurobis/resource?r=aegean_macro_fau

if (!require("finch")) { install.packages("finch") }

archive <- dwca_read("http://ipt.vliz.be/eurobis/archive.do?r=aegean_macro_fau&v=1.1", read = TRUE, force = TRUE)
aegean <- archive$data$occurrence.txt
leafletmap(aegean)
