# Module 6: Conducting Quality Control

## Introduction

You have already learned about data standards, and how to apply these standards to format datasets. In this module you will learn how to run quality control (QC) checks to verify any required or important step was not skipped and to help ensure our data is the best quality it can be before publishing to OBIS.

## Learning Outcomes

After successful completion of this module, you should be able to:

* List potential quality flags, navigate to and understand QC flag reports, use R to inspect QC flags, identify the reasons why records are dropped from a dataset
* Run quality control checks on your own data using R
* Check published datasets for quality control flags using R packages
* Resolve uncertain temporal scope or uncertain eventDate
* Provide locality generalizations or estimates for missing latitude/longitude coordinates
* Resolve uncertainties related to taxa records, including when only fragments of organisms are recorded, or species marked as non-marine

## How to Proceed

To succeed in this Module, you need to successfully complete the following lessons and exercises:

* Lesson 1: Introduction to Quality Control
* Lesson 2: Resolving temporal uncertainties
* Lesson 3: Resolving uncertain localities
* Lesson 4: Uncertain taxonomic identification and measurement uncertainties
* [Exercise 6-1: Practice georeferencing](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32968)
* [Exercise 6-2: Conduct QC checks](https://classroom.oceanteacher.org/mod/assign/view.php?id=32969)
* as well successfully complete Quiz 6 with a score of ≥80%
* [Quiz 6](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32970)

## Introduction to Quality Control

OBIS ignores records that do not meet a number of standards. For example, all species names need to be matched against an authoritative taxonomic register, such as the World Register of Marine Species. In addition, quality is checked against the OBIS-required fields as well as against any impossible values. OBIS checks, rejects, and reports the data quality back to OBIS nodes, but never changes records. A number of quality control (QC) tools have been developed to help you, such as a QC tool for species names and a QC tool for geography and data format

## Why are records dropped?

How does OBIS determine which records are dropped and which are not? There are a number of potential reasons a record will be dropped entirely, including:

* The species is not marine
* The scientificName or scientificNameID did not match with WoRMS
* Issues with coordinates:
  * No coordinates given
  * decimalLatitude or decimalLongitude out of range
  * The coordinate is zero

For each dataset published, a quality report is generated where the number of dropped records and other quality issues will be flagged. Some flags will only provide warnings and will not cause a record to be dropped. A complete list of QC flags can be found [here](https://github.com/iobis/obis-qc/blob/master/obisqc/util/flags.py) but broadly speaking, potential flags relate to issues with location (coordinate format), event data (time, start and end dates), depth values being out of range, taxonomic issues (e.g. WoRMS name matching, non-marine or terrestrial species). QC reports can be found attached to individual datasets or when searching for data in OBIS. For example, if we searched for ‘Crustacea’ records, the following data quality report is given:

You can see that >114,000 Crustacean records have been dropped, mostly due to records missing coordinates or species being flagged as non-marine. Because species are determined as being marine by WoRMS, sometimes species are marked as not_marine erroneously. We will return to this later in this module.

Generally, it's important to understand that records in your dataset may be rejected and not published (i.e. dropped) if the quality does not meet certain expectations. In other cases, quality flags are attached to individual occurrence records. To minimize the number of records dropped, be careful when formatting your data so that you are meeting the requirements. By running quality control checks before you publish, you can minimize the likelihood of records being dropped or flagged. Let's look a little more at the different QC flags that can be produced, and where to find flags in downloaded data.

## Inspecting QC Flags in downloaded data

As we mentioned before, there are a number of QC flags that can tell you what sort of warnings are associated with a particular dataset or individual record. The QC checks OBIS implements as well as the associated flags are summarized [here](https://github.com/iobis/obis-qc).

There are several ways to inspect the QC flags associated with a specific dataset or any other subset of data. Data downloaded through the [OBIS mapper ](https://mapper.obis.org/)or the [robis R package ](https://github.com/iobis/robis)will include a column named flags which contains a comma-separated list of flags for each record. There is also a data quality panel on the dataset pages that has a flag icon, which can be clicked to get an overview of all flags and the number of records affected. See below:

Clicking this flag will open a table that lists the specific quality flags associated with the dataset, as well as any annotations added by the WoRMS annotated names list. When OBIS receives a scientific name that cannot be matched with WoRMS automatically, it is sent to the WoRMS team. The WoRMS team will then annotate the name to indicate if and how the name can be fixed. The picture below shows such a table with QC flags.

Clicking any of these flags in orange will take you to another table showing the specific affected records. For example, below is a list of records from a single dataset which have the no_match flag, indicating that no LSID or an invalid LSID was provided, and the name could not be matched with WoRMS. The column originalScientificName contains the problematic names, as scientificName is used for the matched name.

At the top of the page, there’s a button to open the occurrence records in the [OBIS mapper ](https://mapper.obis.org/)where they can be downloaded as CSV. The occurrence table also has the flags column, so when inspecting non-matching names for example it’s easy to check if the names at hand have any WoRMS annotations:

Next, let's look at how to inspect QC flags using R.

## Inspecting QC Flags with R

Inspecting flags using R is also very easy. The example below fetches the data from a single dataset, and lists the flags and the number of records affected. Notice that the occurrence() call has dropped = TRUE to make sure that any dropped records are included in the results:

```r
library(robis)
library(tidyr)
library(dplyr)

# fetch all records for a dataset
df <- occurrence(datasetid = "f3d7798e-7bf2-4b85-8ed4-18f2c1849d7d", dropped = TRUE)

# unnest flags
df_long <- df %>%
  mutate(flags = strsplit(flags, ",")) %>%
  unnest(flags)

# get frequency per flag
data.frame(table(df_long$flags))

 Var1                          Freq
1  depth_exceeds_bath              78
2  no_accepted_name               17
3  no_depth                        5
4  no_match                      138
5  not_marine                      2
6  on_land                         1
7  worms_annotation_await_editor   5
8  worms_annotation_reject_ambiguous  2
9  worms_annotation_reject_habitat  2
10 worms_annotation_todo           9
11 worms_annotation_unresolvable   7
```

So how can you help prevent or minimize the number of potential QC flags on your dataset? Let's take a look at the types of tools you can use to run QC checks.

## Conducting QC

There are many different tools you can use to help you perform quality checks on your data. These include:

* R package [**obistools**](https://github.com/iobis/obistools)
* R package and function [**Hmisc:: describe**](https://www.rdocumentation.org/packages/Hmisc/versions/5.0-1/topics/describe)
  * Can give important summary statistics and identify numbers that don’t match
* LifeWatch & EMODnet Biocheck
* [Web UI](https://rshiny.lifewatch.be/BioCheck/) built on obistools. This tool requires your dataset to be published on an IPT (e.g., a test IPT such as [https://ipt.gbif.org/](https://ipt.gbif.org/) where your dataset will not be harvested by GBIF or OBIS). Conducts additional checks that are not available in obistools.
* [R package](https://github.com/EMODnet/EMODnetBiocheck)
* [Lifewatch data services](https://www.lifewatch.be/data-services/)
* The US Integrated Ocean Observing System [**Standardizing Marine Bio Data Guide**](https://github.com/ioos/bio_data_guide/blob/main/datasets/TPWD_HARC_BagSeine/TPWD_HARC_BagSeine_OBISENV.md)
* [WoRMS taxon match tool](https://www.marinespecies.org/aphia.php?p=match)
* [GBIF data validator](https://www.gbif.org/tools/data-validator)
* [Python library for OBIS QC](https://github.com/cioos-siooc/pyobistools) developed by Canadian Integrated Ocean Observing System
* Excel Conditional Formatting tool - identify duplicated data
  * Excel > Home > Conditional Formatting > Highlight cells Rules > Duplicate values…

Because there are so many potential tools, let's look at the first two tools: obistools and Hmisc.

We have also developed a QC tutorial that can be used to assist you with Exercise 6-2, it demonstrates many of the QC steps outlined in this module and can be found at <https://shiny.obis.org/obislearn/>.

## Running quality control checks with obistools

Within the **obistools** package, there are a number of useful functions you can use to check your dataset. Each function is designed to give you feedback on different elements of your data, and to report any inconsistencies.

Installing **obistools** requires the **devtools** package. Use the following code to install both packages:

```r
install.packages("devtools")
devtools::install_github("iobis/obistools")
```

If you have difficulty installing **obistools**, please try updating your R packages, in particular the **vctrs** package. This can be done in RStudio in the Packages tab ("update" button) or by using the **update.packages()** command (you can choose which packages to update). If you cannot install **obistools** please reach out to <helpdesk@obis.org> and we will help you.

We recommend following this general procedure to start your QC checks:

1. Check that taxa match with WoRMS using [**obistools::match_taxa**](https://github.com/iobis/obistools)

Example code:
```r
# this code conducts the taxon matching by telling the function to only match the unique species names in the scientificName column
worms <- match_taxa(unique(occur$scientificName), ask=T)
# this piece of code merges the worms dataframe with the occurrence table (occur) by matching according to scientificName
occur_match <- merge(occur, worms, by="scientificName", all=T)
```

2. Check that all required fields are present in the Event and Occurrence tables with [**obistools::check_fields**](https://github.com/iobis/obistools)
Example:

3. Check coordinates with [**obistools::plot_map**](https://github.com/iobis/obistools)

* Plot them on a map to identify any points that appear outside the scope of the dataset. Using obistools:plot_map_leaflet() will additionally allow you to identify the row number for a particular data point.
* Check that points are not on land [**obistools::check_onland**](https://github.com/iobis/obistools)
The output of this function will return the rows in the dataframe that are on land, for example:

* Ensure depth ranges are valid [**obistools::check_depth**](https://github.com/iobis/obistools)
You can specify a value to determine how much a given depth can deviate from the reference bathymetry layer to account for differences between the (potentially) coarser reference and the measurement taken in the real world.

4. Check that the **eventID** and **parentEventID** are present and corresponding [**obistools::check_eventids**](https://github.com/iobis/obistools)

5. Ensure all **eventIDs** in **extensions** have matching **eventIDs** in the **core** table [**obistools::check_extension_eventids**](https://github.com/iobis/obistools)

6. Check that eventDate is formatted properly [**obistools::check_eventdate**](https://github.com/iobis/obistools)
This may identify either rows that do not have any eventDate (e.g. a parentEvent that doesn't contain any date information) or dates that were incorrectly formatted.

Now let's look at how you can use the Hmisc package. This package will help us identify statistical outliers and other anomalies in the data.

## Running quality control checks with Hmisc

The R package [Hmisc](https://cran.r-project.org/web/packages/Hmisc/index.html) has the function [describe](https://rdrr.io/cran/Hmisc/man/describe.html) which can help you identify several discrepancies in your dataset.

It will summarize each of your variables for a given dataset. This can help you quickly identify any missing data and ensure the number of unique IDs is correct. For example, in an Occurrence table with 1000 records, there should be 1000 unique occurrenceIDs.

In the example output below we are checking an Occurrence data table. We can see that there are 407 records in total. There is only **one **unique CollectionCode,**27** unique eventIDs, and**407** unique occurrenceIDs. No records are missing any of these identifiers. From this output, it looks like the Occurrence **table is okay. However, we would still have to check coordinates, formatting, etc. as we learned on the previous page.

```r
library(Hmisc)

data <- read.csv("example_data_occur.csv")

describe(data)

 12 Variables  407 Observations
------------------------------------------------------------------------------------------------------------------
CollectionCode
       n missing distinct value
     407       0        1 BIOFUN1

Value      BIOFUN1
Frequency  407
Proportion   1
------------------------------------------------------------------------------------------------------------------
eventID
       n missing distinct
     407       0       27

lowest : BIOFUN1_BF1A01 BIOFUN1_BF1A02 BIOFUN1_BF1A03 BIOFUN1_BF1A04 BIOFUN1_BF1A05
highest: BIOFUN1_BF1M3  BIOFUN1_BF1M4  BIOFUN1_BF1M6  BIOFUN1_BF1M8  BIOFUN1_BF1M9
------------------------------------------------------------------------------------------------------------------
occurrenceID
       n missing distinct
     407       0      407

lowest : CSIC_BIOFUN1_1   CSIC_BIOFUN1_10  CSIC_BIOFUN1_100 CSIC_BIOFUN1_101 CSIC_BIOFUN1_102
highest: CSIC_BIOFUN1_95  CSIC_BIOFUN1_96  CSIC_BIOFUN1_97  CSIC_BIOFUN1_98  CSIC_BIOFUN1_99
```

## Lesson summary

In this lesson we learned about the many tools available to conduct QC checks on our data, as well as why certain records might get dropped. As a reminder, we also learned about an online QC tutorial that can assist **you with Exercise 6-2, it allows you to run R code for the QC steps outlined in this module and can be found at <https://shiny.obis.org/obislearn/>.

For a demonstration of how to run these QC checks in R, watch the following video (<https://youtu.be/sNzipC6-r90>)

Play Video

An additional note on the match_taxa function from the video above:

We can add extra parameters to our functions when matching taxonomic names to WoRMS using R. By adding the **ask** parameter, we will ensure pop-up questions will appear during taxon matching in cases where there are multiple matches. In some cases, taxa there may not be multiple matches but the WoRMS list returned might have scientific names that have resolved typos, etc. Then when we want to merge the two lists, taxa with typos or other errors will not be matched to its corresponding WoRMS LSITD. This is where we can add the**"all=T"** to the merge function, which will add extra rows to the output, one for each row in the occurrence file that has no matching row in **worms**. Of course, we will then have to manually check the **resulting list to see if any extra rows have been added. See the lines of code below for how to add these parameters. The example code presented earlier in this lesson included these additions to the code.

```r
worms <- match_taxa(unique(occur$scientificName), ask=T)
occur_match <- merge(occur, worms, by="scientificName", all=T)
```

In the next few lessons we will learn how to handle some common QC issues that might arise, including uncertainties about location, date, taxonomic, and measurement data.

## Lesson 2: Resolving temporal uncertainties

## Uncertain temporal ranges

There are times when the **eventDate** or the temporal scope of a dataset is either in question, or provided in an invalid format (e.g. textual description). In this lesson we will learn how to resolve different issues related to the date and time of a dataset. We will look at:

* Providing date ranges
* Dealing with limited date information
* Text descriptions of dates
* Historical dates

Let's start by looking at date ranges.

## Date ranges

If you are uncertain about the exact date an event took place, or the date was only provided as a range (e.g. March to June 1982), date ranges can be provided in the **eventDate** field to capture this uncertainty.

You should use ISO 8601 format for date ranges only if you are certain on the date range. Do not include a date range if you are making assumptions or guesses. However, notes about any appropriate assumptions or interpretations on date ranges can be documented in the **eventRemarks** field. Such assumptions could include interpretations of log books where the event range is known, but the exact date of sub-events or sampling are not documented.

Be careful when entering date ranges because the start and end are inclusive. For example, entering 1870/1875-08-04 is equivalent to any date between 1870 and 1875-08-04. You may use date ranges in this way to capture some level of uncertainty in when an event occurred. Be sure to always document the original date description in **verbatimEventDate**.

### Examples

| Provided date | ISO 8601 formatted date range |
| --- | --- |
| April to August 1982 | 1982-04/1982-08 |
| Winter 1830-1831 (with notes about locality to infer season) | 1830-11/1831-02 |
| 1 Sept - 15 Sept 2008 | 2008-09-01/2008-09-15 |
| May 16, 1731, 6PM to May 17, 3AM | 1731-05-16T18:00/1731-05-17T03:00 |

Now let's take a look at what to do if you have limited information about the **eventDate**.

## Limited date information

If only parts of the date are known (e.g., year but not month and day), you may provide the date to eventDate in ISO 8601 format while excluding the unknown elements. It is good practice to also populate the dwc:[year](https://dwc.tdwg.org/terms/), dwc:[month](https://dwc.tdwg.org/terms/) and dwc:[day](https://dwc.tdwg.org/terms/) fields with known information.

**Important note: Do not use zero to populate incomplete dates.** Simply end the date with the known information (e.g., 2011-03 instead of 2011-03-00). Additionally, if the year is unknown, you should only populate the dwc:month and dwc:day fields because eventDate cannot be formatted to exclude year. In these cases, eventDate is not necessary to fill.

### Examples

| Known date elements | Formatted date |
| --- | --- |
| year: 2002 | eventDate 2002 year 2002 month blank day blank |
| year: 2013 month: October | eventDate 2013-10 year 2013 month 10 day blank |
| month: February day: 12 | eventDate blank year blank month 2 day 12 |
| year: 1999 day: 20 | eventDate blank year 1999 month blank day 20 |

Next, let's look at some examples for when date is only provided as a written description.

## Text descriptions of dates

If the date in your dataset was provided as a **textual description** that can be accurately interpreted, include the text description in the verbatimEventDate field. Then provide the interpreted date in ISO 8601 format in the eventDate field. Be sure to document any other important information in ‘eventRemarks’.

### Examples

| Example description | Interpreted date |
| --- | --- |
| 10th day of the 20 day cruise beginning 4 June 1951 | 1951-06-13 |
| In the interval of the university examinations from May 18 to 27, 1938 | 1938-05-18/1938-05-27 |
| Sunday May 16th 1824. At 3 PM saw a fish... got her killed about 1/2 after 5PM. | 1824-05-16T15:00/1824-05-16T17:30 |


### We will conclude this lesson by discussing how to handle historical dates, dates predating the 1500s.

## Historical dates

There can be difficulties formatting dates from historical data for a number of reasons. Some of these difficulties arise from the change in calendar systems, from the [Julian calendar](https://en.wikipedia.org/wiki/Julian_calendar) to the currently used (by most countries) [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar) metric system. This change was implemented in 1582, so any datasets predating this year must be converted to the Julian calendar system. Additionally - there is [no year zero,](https://en.wikipedia.org/wiki/Year_zero) only -1 and 1, where -1 is BCE (Before Common Era) and 1 is CE (Common Era).

For historical dates that do not conform to the ISO 8601 format and to accommodate challenges associated with them, the OBIS Historical Data Project Team has developed the following recommendations:

* Always populate verbatimEventDate with the originally documented date so that it can be preserved. Place converted dates that align to ISO 8601 in the `eventDate` field, and document the changes you made to the original in `eventRemarks`
* When the exact date is unknown, provide a date range, e.g. the period 21 November 1521 to 29 August 1612 records as 1521-11-21/1612-08-29*
* For **archaeological data**, you can use a combination of terms from the Darwin Core class [GeologicalContext ](https://dwc.tdwg.org/terms/)and the [Chronometric Age Extension](https://tdwg.github.io/chrono/). GeologicalContext terms can be used to capture some information, however the Chronometric Age extension allows for better description of the time period (e.g. age, period, etc.), and links to the Event core table. For such records, eventDate would be populated with the date of collection. Note however that currently, the Chronometric Age extension **will not be aggregated** when publishing to OBIS, but the extension will be available when an individual dataset is downloaded.
* If the historical record contains uncertain or sensitive location information, generalize the location information using polygons or lines as described in the next Lesson

For historical data originating from old records, such as ship logs or other archival records, we understand there can be additional issues in interpreting and formatting data according to DwC standards. We have already reviewed some solutions you can use (date range, partial date information), but we understand that these issues can vary wildly. If you need further help with historical data formatting, we currently recommend [submitting a Github issue](https://github.com/iobis/obis-issues/issues) to get assistance with such issues.

## Lesson summary

In this lesson we learned how to deal with some common challenges related to dates and time, specifically:

* How to provide a range of dates to capture uncertainty or a particular time period
* How to deal with partial date information
* How to handle text descriptions of dates
* How to deal with historical data
 
In the next lesson we will learn how to resolve ambiguities around geographic locations.

## Lesson 3: Resolving uncertain localities

## Uncertain spatial extent

Sometimes locality information can be difficult to interpret, especially if records originate from historical data with vague descriptions, or descriptions/names of areas that no longer exist. If your dataset is missing decimalLongitude and decimalLatitude, but the locality name is given, there are a number of approaches you can take.

We will go over four alternatives you can use to resolve geographic uncertainties and obtain georeferencing information, depending on the information available to you:

* OBIS Maptool
* Getty Thesaurus and/or Google Maps
* Marine Regions Gazetteer
* Tools for dealing with textual descriptions of locations

## OBIS Maptool

One approach to resolving geographic ambiguity is to generalize the location using polygons or lines, allowing you to capture a geographic area or a transect. You can use the [OBIS Map Tool](https://obis.org/maptool/) to obtain a [Well-Known Text (WKT)](https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry) string for point, line, or polygon features, which will be placed in the footprintWKT field. WKT strings are representations of the shape of the location and are placed in the Darwin Core Location class [footprintWKT](https://dwc.tdwg.org/terms/) field. This is particularly useful for tracks, transects, tows, trawls, habitat extent, or when the exact location is not known. You can also use a polygon to generalize sensitive locality information, for example if the exact location cannot be shared.

WKT strings can be created using the Map Tool’s Generate WKT function. This Map Tool can also calculate a midpoint and a radius for line or polygon features, which can then be added to the decimalLongitude, decimalLatitude, and coordinateUncertaintyInMeters fields, respectively. Alternatively, you can use the [obistools::calculate_centroid](https://github.com/iobis/obistools) function to calculate the centroid and radius for WKT polygons. To visualize and share WKT strings, [this wktmap tool](https://wktmap.com/) developed by Pieter Provoost can be used. Within the OBIS Map Tool, you can also access the [Marine Regions Gazetteer](https://www.marineregions.org/gazetteer.php?p=search) which will additionally help you find locations. We'll go over details about the Marine Regions Gazetteer later in this lesson.

Whenever you are using a WKT string, it is important to place the corresponding projection in the [footprintSRS ](https://dwc.tdwg.org/terms/)field. Note the accepted spatial reference system for OBIS is EPSG:4326 (WGS84). This is the spatial reference used by the OBIS Map Tool, Google Maps, and is commonly used in many other map services.

For a walkthrough on how to use the OBIS Map Tool, watch the video tutorial below (available at <https://youtu.be/XM23WEvE364>). This video covers estimating coordinates, using the line and polygon tool, and obtaining and exporting WKT strings.


## Using the Getty Thesaurus and Google Maps

Using the [Getty Thesaurus of Geographic Names® Onlines](https://www.getty.edu/research/tools/vocabularies/tgn/) and [Google Maps](https://www.google.com/maps/) for georeferencing is relatively straight forward as long as you have the name of a locality to work with.

For an example, let's look at how we would georeference a locality documented only as "Cook Strait, New Zealand". The first step is to search for our location by name and nation (if relevant) in the search tool. There are convenient Lookup menus that can be used to help narrow down your serach.

There is only one result for this search, and clicking on it will bring us to a page where we can obtain decimalLatitude and decimalLongitdue. **Note:** Always be sure to fill in the georeferenceSources field to indicate the sources you used to obtain locality information! This is important for any georeferencing you do.

For Google Maps, the coordinates can either be found in the url after searching for a location, or as a pop-up when you click on the map (see images below).

Note that whenever you record coordinates that were not the exact location (i.e. estimated using a thesaurus, a gazetteer or other methods like Google), you should always include georeferencing information in [georeferenceRemarks](https://dwc.tdwg.org/terms/), as well as the [locationAccordingTo](https://dwc.tdwg.org/terms/) field. The georeferenceRemarks field should be populated with a brief statement regarding any assumptions you made during georeferencing. [locationAccordingTo](https://dwc.tdwg.org/terms/) will hold the name of the georeferencing tool you used, e.g. Getty Thesaurus of Geographic Names.

## The Marine Regions Gazetteer

[Marine Regions](https://www.marineregions.org/) offers a [marine gazetteer search engine](https://www.marineregions.org/gazetteer.php?p=search) to obtain geographic information and unique identifiers for marine regions. Once you have navigated to the [gazetteer search engine](https://www.marineregions.org/gazetteer.php?p=search), you have two options to search by: enter the name of the desired locality, or enter an MRGID code.

Most likely you will have a locality name but not an MRGID. You may also select a [placetype](https://www.marineregions.org/gazetteer.php?p=popup&name=placetype) to search instead for types of regions that may be physical (e.g., seamount, bay, fjord, etc.) or administrative (e.g., exclusive economic zones, countries, etc). You can specify specific sources if known (e.g., published paper, organization, etc.). Finally, you may also provide a latitude/longitude coordinate with a radius to obtain a list of regions near that point. A reminder that you should fill in the **georeferenceSources** field to indicate the source(s) you used to obtain locality information.

For example, let’s look at the [IHO Bay of Fundy locality](http://marineregions.org/mrgid/4289). Inspecting the page, there is a lot of useful information we can use. We can populate the following OBIS fields for our dataset, copying the information outlined in the red boxes from the figure below:

(1) **locationID** from MRGID: [**http://marineregions.org/mrgid/4289**](http://marineregions.org/mrgid/4289)

(2) **decimalLatitude** and **decimalLongitude** latitude and longitude coordinates of the location’s midpoint in decimal degrees: 44.97985204, -65.80601556

(3) **coordinateUncertaintyInMeters** precision: 196726 meters

Since we are obtaining all this locality data from Marine Regions, we must also populate the **locationAccordingTo** field. Here, we will provide the name of the gazetteer we used to obtain the coordinates for the locality - in this case you would write “Marine Regions”. In **georeferenceRemarks** we must document that the coordinates are the region’s midpoint, that locality information was inferred by geographic name, and, where applicable, place the original locality name in the **verbatimLocality** field. **georeferenceSources** will also be populated with the name of the gazetteer (Marine Regions in this case), and any other additional tools you used to complete the georeferencing.

As mentioned earlier, you can also access the Marine Regions Gazetteer through the OBIS Maptool. The video below (available <https://youtu.be/CnefukzhnEM>) demonstrates using the embedded Marine Regions search function, as well as how to use the Gazetteer itself.

### Play Video

## Text descriptions of locations

If you have data that only provides locality information in a text-format, you can try using the [GEOLocate Web Application](https://geo-locate.org/web/default.html). You can use this tool for one location at a time with the [Standard Client](https://geo-locate.org/web/WebGeoref.aspx), or upload a CSV file for [batch processing](https://geo-locate.org/web/WebFileGeoref.aspx). This tool lets you enter text descriptions in the “Locality String” field, and other relevant locality information (e.g. country, state, county) to obtain geographic coordinates.

Alternatively, you can use this [Biodiversity Enhanced Location Services](https://localityservice.uc.r.appspot.com/) tool developed by VertNet. It can translate textual descriptions and provide **decimalLatitude**, **decimalLongitude**, **geodeticDatum**, and **coordinateUncertaintyInMeters** as a csv sent to an email address. For more information on this service, see the associated [GitHub](https://github.com/VertNet/bels).

Remember, OBIS is built on georeferenced biodiversity records, therefore, it is essential that the locality information in your dataset is accurate (or as precise as possible)!

## Lesson summary

In this lesson we learned about the different tools that can be used to georeference or generalize an ambiguous or sensitive locality. To practice what you have learned in this lesson, complete Exercise 6-1. When you are ready, move on to Lesson 4 which deals with low confidence taxonomic identification.

## Lesson 4: Uncertain taxonomic identification and measurement uncertainties

## Uncertainties regarding taxonomic records

In this lesson we will learn how to deal with a variety of issues related to a taxonomic record. Such issues will include taxon identification that has low confidence, uncertainties regarding the count of sample organisms, and what to do if you encounter non-marine species.

Let's start with low confidence taxonomic information.

## Low confidence taxonomic identification

For a variety of reasons, the taxonomic identification of a species may have low confidence. This includes when the scientific name contains qualifiers such as cf., ?, or aff. In these cases of low confidence taxonomic identifications, then you should:

* Put the name of the lowest possible taxon rank that can be determined with high-confidence in **scientificName** (usually Genus in these cases)
* Put qualifiers in [identificationQualifier](https://dwc.tdwg.org/terms/) (e.g., cf., aff.)
* Put the species name in [specificEpithet](https://dwc.tdwg.org/terms/)

* Place the rank of the taxon documented in **scientificName** (e.g., genus) in [taxonRank](https://dwc.tdwg.org/terms/)
* Document any relevant comments in [taxonRemarks](https://dwc.tdwg.org/terms/) or [identificationRemarks](https://dwc.tdwg.org/terms/)

For example, let's say there was a specimen recorded as **Pterois** cf.**volitans**. The associated occurrence record would have the following taxonomic information:

* **scientificName** = Pterois
* **identificationQualifier** = cf. volitans
* **specificEpithet** = leave blank
* **scientificNameID** = the one for Pterois
* **taxonRank** = genus

If the provided genus name is unaccepted in WoRMS, it is okay to use the unaccepted name in this field.**scientificNameID** should contain the WoRMS LSID for the genus. There is a new Darwin Core term [verbatimIdentification](https://dwc.tdwg.org/terms/) meant for containing the originally documented name, however this term is not yet implemented in OBIS so if you populate this field it will not be indexed alongside your data. Therefore you can use **originalNameUsage** to document original species names.

The use and definitions for additional Open Nomenclature (ON) signs (**identificationQualifier**) can be found in [Open Nomenclature in the biodiversity era](https://doi.org/10.1111/2041-210X.12594), which provides examples for using the main Open Nomenclature qualifiers associated with physical specimens. Whereas the publication [Recommendations for the Standardisation of Open Taxonomic Nomenclature for Image-Based Identiﬁcations](https://www.frontiersin.org/articles/10.3389/fmars.2021.620702/full) provides examples and definitions for **identificationQualifier**s for non-physical specimens (image-based).

## Changes in taxonomic classification

Taxonomic classification can change over time - so what does that mean for your datasets when records change classification?

Because OBIS relies on WoRMS as the taxonomic backbone, changes in taxonomic classification will be updated between a day to a few weeks from the date of change, unless triggered manually. This means that the WoRMS LSID associated with a species in question will be used to automatically populate the taxonomic classification with the updated information.

We recognize that there may be issues or update delays when larger changes occur (e.g. Family level splits). Then, records that are only identified to the Family level may not get updated properly. For example, there was a shift in coral taxonomy where the family Nephtheidae was split into Capnellidae and Alyconiidae for species occurring in the Northern Atlantic. While species that were identified as belonging to Nephtheidae have now been updated to belong to one of those two families, records that were only identified down to family (i.e., Nephtheidae) are still documented as Nephtheidae. Unfortunately, there is currently no solution for dealing with splits like this, aside from contacting data providers and asking them to change the taxonomy, or updating your dataset yourself to reflect the changes. We will learn how to update published datasets in Module 7.

Next let's look at what to do when faced with a flag that a taxon in your dataset is non-marine.

## Non-marine species

If you encounter a flag or an error that a taxon is not marine, please confirm first whether the species is actually freshwater by cross referencing with [WoRMS ](https://www.marinespecies.org/index.php)or [IRMNG](https://www.irmng.org/). If the species is not marine (i.e. belongs to a non-marine genus), check with the data provider as necessary for possible misidentification. If the taxon was marked as non-marine in error, you can contact the WoRMS Data Management Team at [info@marinespecies.org](mailto:info@marinespecies.org) to discuss adding the taxon to the WoRMS register. You will be required to provide documentation in this case to confirm the marine status of the taxon. Otherwise, records marked as non-marine will be dropped from the published dataset, and this will be flagged in the data quality associated with your dataset.

Let’s consider an example within [this dataset](https://obis.org/dataset/9fbaeb21-a0dc-4a29-8237-1cd7ada266e0) on benthic macroalgae. Inspecting the data quality report we can see there are three dropped records due to species not being marine.

Clicking on the dropped records we can see which three species were dropped. By scrolling to the right of the table, we can see these records have two quality flags: NO_DEPTH and NOT_MARINE.

Let’s take a look at the first species, **Pseudochantransia venezuelensis**. When we search for this species on [WoRMS](https://www.marinespecies.org/aphia.php?p=taxdetails&id=836900) we can see that the species is marked as freshwater only.

Cross-referencing with IRMNG, if we search for the genus, we can see that marine and brackish are stricken out, indicating the species is not marine.

If you have species that are marked as non-marine in these registers but are either supposed to be marine, or were found in a marine environment, then, as mentioned, you should contact WoRMS to discuss adding it to the register. For additions and/or edits to environmental or distribution records of a species, contact the WoRMS Data Management Team at [info@marinespecies.org](mailto:info@marinespecies.org) with your request along with your record or publication substantiating the addition/change.

Now let's consider cases where there is uncertainty regarding a measurement about an organism.

## Uncertain taxonomic measurements

In some cases only fragments of organisms were found, or only a range of individuals is known. This can make it difficult to know how to properly populate the individualCount field.

In cases where only certain body parts (e.g., head, tail, arm) are recorded, you can incorporate this information in **measurementTypeID** by finding a code that documents sub-components of biological entities. If such a code for your sub-component does not exist, you can request the [creation of a P01 code](https://github.com/nvs-vocabs/OBISVocabs/issues). We will review more specifics about finding **measurementTypeID** codes in Module 5. If you are uncertain whether fragments are from the same individual, a range can be provided and the uncertainty can be recorded in **occurrenceRemarks**.

When providing a range to estimate the count, there are two [suggested](https://github.com/tdwg/dwc/issues/338) approaches:

Use eMoF: place the data in the extendedMeasurementOrFact extension. This requires an additional two rows per Occurrence. One that will document the minimumIndividualCount and to document maximumIndividualCount.

Place data into **dynamicProperties**: Include the information in the Occurrence record itself, with no extension, and instead document it in **dynamicProperties**, with a value such as {“minimumIndividualCount”:0, “maximumIndividualCount”:5}

Note that documenting the data in ‘dynamicProperties’ means the**information will not be machine readable** and may be more difficult for users to extract.

Abundance vs Count data: A brief clarification on abundance and count data: abundance is the number of individuals within an area or volume. This type of data is recorded in **organismQuantity**, where [organismQuantityType](https://dwc.tdwg.org/terms/) should be used to specify the type of quantity (e.g., individuals, percent biomass, Braun Blanquet Scale, etc.). However, if the value is just the number of individuals without reference to a space (i.e. count), this information can be recorded in **individualCount**.

Using the **organismQuantity** and **organismQuantityType** fields is recommended in most cases as it allows you to record more details about the occurrence.

Finally, populate the **occurrenceRemarks** field to document all information about the decisions you made during this process.

## Lesson summary

In this lesson we learned how to deal with uncertainties about taxon occurrences - low confidence identification, questionable marine status, and uncertain organism quantity.

## End of Module

You have completed Module 6! Practice everything you have learned in this module by completing Exercise 6-1 and 6-2 to identify and correct as many quality control issues as possible.

In the next module we will learn how to publish our datasets. This step can be taken when you are sure the dataset will meet the quality control standards discussed in this module as best as possible. However, we also encourage you to publish data even if there are some things you need to update later (e.g. obtaining **measurementTypeIDs**). You can always update datasets later with more data, or any changes that need to be made (e.g. change in taxonomy).
