# Module 8: Accessing OBIS data

## Introduction

In this module you will learn how to access and cite data from OBIS obtained from various places, including the use of the OBIS Mapper, R packages, and the OBIS homepage.

## Learning Outcomes

After successful completion of this module, you should be able to:

* Use the Mapper to download data, e.g. specific to a taxon, region, time, or type of data quality
* Use the `robis` R package to download data from OBIS
* Use the OBIS API to find data
* Locate a dataset in OBIS by name
* Identify data provider names and find contact information
* Construct citations for data from OBIS, including citations for downloads of many OBIS datasets
* Understand how to navigate data downloads

## How to Proceed

To succeed in this module, you need to successfully complete the following lessons and exercises:

* Lesson 1: Accessing data
* Lesson 2: Interpreting and citing OBIS data
* [Exercise 8-1: Find OBIS data](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32990)
* Quiz 8 (score ≥ 80%): [Quiz 8](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32991)

## Lesson 1: Accessing data

OBIS has over 100 million records of marine data accessible for downloading. To download data from OBIS, you have several options:

* [OBIS Mapper](https://mapper.obis.org/)
* [OBIS homepage](https://obis.org/) or [advanced dataset search](https://obis.org/datasets)
* The [R package `robis`](https://github.com/iobis/robis)
* OBIS [API](https://api.obis.org/)
* Full data exports
* IPT

**Note:** When you download data from the Mapper or full export, the data you receive is flattened into one table with occurrence plus event data. eMoF and DNADerivedData extension tables are separate upon request. However, when you download a dataset from the OBIS homepage or dataset page, all tables (Event, Occurrence, eMoF, DNADerivedData) are included as separate files.

### OBIS Mapper

The OBIS Mapper is accessible from [https://mapper.obis.org](https://mapper.obis.org/) and allows users to visualize and inspect subsets of OBIS data. A variety of filters are available (taxonomic, geographic, time, data quality), and multiple layers can be combined in a single view. You can download individual layers as CSV files.

When you download data from the mapper, you will be given the option to include eMoF and/or DNADerivedData extensions alongside the Event and Occurrence data. You must check the boxes for the extensions you want to include in your download.

After downloading, you will notice that the Event and Occurrence data is flattened into one table called `Occurrence.csv`. Upon inspecting this file in your viewer of choice, you will see it contains all 225 possible DwC fields, although not every field will contain data for each observation. Any extensions you checked will be downloaded as separate data files.

Next, let's learn how to use the `robis` package.

### R package

#### Data access via R package: `robis`

The documentation for the `robis` package can be found at [https://github.com/iobis/robis](https://github.com/iobis/robis). This package was developed to facilitate connecting to the OBIS API from R. The package can be installed from CRAN or from GitHub (latest development version). The package documentation includes a function reference as well as a [getting started vignette](https://iobis.github.io/robis/articles/getting-started.html).

As a quick example of what the package can do, you can obtain raw occurrence data by feeding a taxon name or AphiaID to the `occurrence()` function.

```r
library(robis)

moll <- occurrence("Mollusca")
write.csv(moll, "mollusca-obis.csv")
```

This file will be saved to your working directory. After opening the file, you will notice that the fields in the download do not include every possible field, but instead only those where information has been recorded by data providers, plus the fields added by OBIS’s quality control pipeline. We will learn about these fields later in this lesson.

### OBIS API

#### Data access: OBIS API

Both the Mapper and the R package are based on the [OBIS API](https://api.obis.org/), which can also be accessed directly to find and download data. However, we do not recommend using the API in this way to download data; it is best used for quick data summaries. When using the API directly, you can filter by the following options:

* Occurrence
* Taxon
* Checklist
* Node
* Dataset
* Institute
* Area
* Country
* Facet
* Statistics

Each of these sections has dropdowns where you can specify information to filter by. You will have to click the "Try it Out" button to enable filters. An interesting variable you can facet by is depth. This will allow you to obtain the number of records by depth bin for all records in OBIS, a specific species, and/or a specific area. There are also a number of summary statistics you can obtain.

When you have entered all the information you are interested in filtering by, scroll down and click the “Execute” button in the same box (i.e. do not scroll to the end of the page, only to the end of the section you are applying filters to). This will produce a response detailing how many records match your criteria, as well as information for some of the headers from the data (e.g., `basisOfRecord`, `Order`, `genus`). A download button will be available for you to download the data.

When searching with the API, you may need to know certain identifiers, including:

* AphiaID — obtainable from the WoRMS page of a taxa of interest (for example, the AphiaID for [Mollusca](https://www.marinespecies.org/aphia.php?p=taxdetails&id=51) is `51`)
* Dataset UUID — can be obtained from the URL on individual dataset pages, e.g. [this dataset’s UUID](https://obis.org/dataset/5061d21c-6161-4ea2-a8d4-38f8285dfc47) is `5061d21c-6161-4ea2-a8d4-38f8285dfc47`
* Area ID
* Institute ID — this should be the Ocean Expert ID (for example, the ID for [NOAA Fisheries Service, Southeast Regional Office St. Petersburg](https://oceanexpert.org/institution/7532) is `7532`)
* OBIS node UUID

### OBIS Homepage

#### Data access: OBIS homepage

From the OBIS homepage, you can search for data in the search bar in the middle of the page. You can search by particular taxonomic groups, common names, dataset names, OBIS nodes, institute name, areas (e.g., Exclusive Economic Zone (EEZ)), or the data provider’s country.

When you search by dataset, an additional option appears for [advanced search options](https://obis.org/datasets). This allows you to identify specific datasets and apply filters for OBIS nodes and whether datasets include extensions.

Regardless of whether you found a dataset through the homepage or the advanced dataset search, you can navigate to individual dataset pages. For individual dataset pages (instead of aggregate pages such as a Family), there are three buttons available:

* **Report issue** — allows you to report any issues with the dataset in question
* **Source DwC-A** — download the dataset as a Darwin Core Archive file; this provides all data tables as separate files within a zipped folder
* **To mapper** — opens another browser tab with the data shown in the Mapper

If you searched for aggregate datasets (e.g., all Crustacea records, all records from OBIS-Canada, etc.), the **Source DwC-A** button will not be available. To download these data subsets, you must click **To mapper** and then download the data from the Mapper as a CSV.

### Full exports

To obtain a full export of OBIS data, navigate to the OBIS homepage, click on Data from the top navigation bar, then select [Data Access](https://obis.org/data/access/) from the dropdown menu.

Here you can download all occurrence records as a CSV or Parquet file. Note the disclaimer that such exports will not include measurement data, dropped records, or absence records. As with downloads from the Mapper, the exported file is a single Occurrence table. This table includes all provided Event and Occurrence data, as well as 68 fields added by the OBIS Quality Control Pipeline, including taxonomic information obtained from WoRMS.

### Watch some videos

Watch the videos below for demonstrations on how to access OBIS data:

* [How to access OBIS data from the homepage portal and Mapper](https://youtu.be/9PSPEtqgjUI)
* [How to access OBIS data via R, using the `obistools` and `Hmisc` packages](https://youtu.be/8Ep4fGICQWU)
* [How to access OBIS data from the API](https://youtu.be/Hocr3N6zpH0)

### Finding DNA sequence data

#### Accessing DNA data

As we learned earlier, you can download DNADerivedData extensions from OBIS using the Mapper. We recommend using the R package `robis` to obtain sequence data. Use the `occurrence()` function and set `extensions` and/or `hasextensions` to `DNADerivedData` to ensure extension records are included in the results. `hasextensions` will exclude any occurrence that does not have the specified extension, while `extensions` specifies which extensions to include.

To obtain the DNA data, you have to extract the information from the extension using the `unnest_extension()` function. You can specify as many fields from the Occurrence table as you want, and pass them to the `fields` parameter.

```r
dna_occ <- occurrence("Dinophyceae", hasextensions = "DNADerivedData", extensions = "DNADerivedData")
dnaseqs <- unnest_extension(dna_occ, "DNADerivedData", fields = c("id", "phylum", "class", "family", "genus", "species"))
```

You also have the option to search for sequences or related sequences in OBIS by using the [OBIS Sequence Search](https://sequence.obis.org/). This is a prototype tool, so use caution as it is not always up to date. To use the tool, copy your sequence in the provided box, then press the Search button.

Results will appear below the search box, providing taxonomic information on all available related sequences in order of alignment score. The location of sequences will also be shown on the map above. Finally, a link to the associated dataset is provided in the far right column.

Note that you can also change the Minimum Alignment Score slider in the map view to adjust which sequences are shown.

## Lesson summary

In this lesson we learned how to obtain data from OBIS using:

* OBIS Mapper
* OBIS homepage
* `robis` R package
* OBIS API
* an IPT instance
* OBIS sequence search tool

In the next lesson we will learn how to interpret and cite data obtained from OBIS.

## Lesson 2: Interpreting and citing OBIS data

### Interpreting downloaded data

In general, the field names you will see when you download data from OBIS are the same as those seen during the data formatting and publishing process. When you download data from the [Mapper](https://mapper.obis.org/), you will see all 225 possible Darwin Core fields.

Downloading data from an IPT or full export will include only the fields provided by the data provider, formatted as one Occurrence file (or separate files for individual datasets). Some fields are added through the OBIS quality control pipeline, including taxonomic information from WoRMS and the fields `flags`, `bathymetry`, and `dropped`. As mentioned in Module 4, the fields `flags` and `dropped` will list quality control issues or indicate if the record was dropped. Some of the fields that the OBIS QC pipeline adds are shown below, but for a complete list see [here](https://obis.org/data/access/). For the full list of all Darwin Core terms and their definitions, please reference the [Darwin Core Quick Reference Guide](https://dwc.tdwg.org/terms/).

Sometimes you may want to contact a data provider for extra information regarding their dataset. Let's take a look at that next.

### Contacting data providers

To contact a data provider, navigate to the page for the individual dataset in question (for example, [https://obis.org/dataset/80479e14-2730-436d-acaa-b63bdc7dd06f](https://obis.org/dataset/80479e14-2730-436d-acaa-b63bdc7dd06f)). Under the “Contacts” section, there will be a list of individuals you can contact. Clicking any name will direct you to your system’s default email program.

If you are the node manager and need to contact the data provider about a particular dataset, contact information should also be provided in the metadata.

### Citing data

Depending on how you access data from OBIS, there are different ways you should cite downloaded datasets.

General OBIS citation:

> OBIS (YEAR) Ocean Biodiversity Information System. Intergovernmental Oceanographic Commission of UNESCO. [www.obis.org](http://www.obis.org/).

For individual datasets retrieved from OBIS (dataset citations are available in the zip downloads as an HTML file):

> [Dataset citation available from metadata] [Data provider details] [Dataset] (Available: Ocean Biodiversity Information System. Intergovernmental Oceanographic Commission of UNESCO. [www.obis.org](http://www.obis.org/). Accessed: YYYY-MM-DD).

For example:

> Sousa Pinto, I., Viera, R. (Year: if not provided use year from dataset publication date) Monitoring of the intertidal biodiversity of rocky beaches with schools in Portugal 2005-2010. CIIMAR - Interdisciplinary Centre of Marine and Environmental Research, Porto. [Dataset] (Available: Ocean Biodiversity Information System. Intergovernmental Oceanographic Commission of UNESCO. [www.obis.org](http://www.obis.org/). Accessed: 2015-01-01)

When data represents a subset of many datasets taken from the integrated OBIS database (e.g. downloaded from the [Mapper](https://mapper.obis.org/)), you can, in addition to citing the individual datasets (and taking into account the restrictions set at each dataset level), also cite the OBIS database as follows:

> OBIS (YEAR) [Data e.g. Distribution records of Eledone cirrhosa (Lamarck, 1798)] [Dataset] (Available: Ocean Biodiversity Information System. Intergovernmental Oceanographic Commission of UNESCO. [www.obis.org](http://www.obis.org/). Accessed: YYYY-MM-DD)

The derived information products from OBIS are published under the CC-0 license and can be cited as follows:

> OBIS (YEAR) [Information product e.g. Global map showing the Hulbert index in a gridded view of hexagonal cells] [Map] (Available: Ocean Biodiversity Information System. Intergovernmental Oceanographic Commission of UNESCO. [www.obis.org](http://www.obis.org/). Accessed: YYYY-MM-DD)

## Lesson summary

In this lesson we learned how to interpret the different data fields in downloaded data. We also learned how to cite data obtained from OBIS. Practice the different ways of accessing OBIS data in Exercise 8-1 and then complete this module's quiz.

This brings us to the end of the course! To finish the course, please ensure all quizzes and exercises are successfully completed, and fill out the feedback form so we can continue to improve this course.
