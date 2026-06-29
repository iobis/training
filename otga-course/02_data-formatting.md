# Module 2: Introduction to data formatting

## Introduction

In this module, you will learn about the necessary first steps required to format data tables that will adhere to Darwin Core standards. You will learn how to match taxonomic names to an authoritative register, how to construct identifiers discussed in Module 1 (occurrenceID, eventID), and how to standardize data (e.g., dates, coordinates). You will also learn how to address missing data for required fields.

## Learning Outcomes

After successful completion of this module, you should be able to:

- Prepare a list of species/taxa for input into WoRMS, resolve ambiguous or non-matches from WoRMS taxon match, obtain LSIDs from WoRMS Taxon Tool, and attach LSIDs to your dataset
- Identify hierarchical elements used to construct eventID and put them together to create unique eventIDs
- Construct unique occurrenceIDs
- Format event dates to ISO 8601 standards including time zones and time ranges
- Convert degrees/minutes/seconds coordinates or UTM into decimal degrees

## How to Proceed

To succeed in this Module, you need to successfully complete the following lessons and exercises:

- Lesson 1: Determining dataset structure
- Lesson 2: Understanding and constructing Identifiers
- Lesson 3: Taxon match
- Lesson 4: Addressing common formatting issues
- [Exercise 2-1: Dataset structures](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32921)
- [Exercise 2-2: Construct eventID](https://classroom.oceanteacher.org/mod/assign/view.php?id=32922)
- [Exercise 2-3: WoRMS Taxon names matching](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32923)
- Successfully complete Quiz 2 with a score of ≥80%
- [Quiz 2](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32925)

## Lesson 1: Determining dataset structure

## Introduction

Because formatting data in a standardized way can be challenging, this module starts with an overview of a dataset structure. Deciding on your dataset structure is one of the first steps towards getting your data ready for publishing. We've already been introduced to the different Darwin Core data table types, and it is important to determine which structure (core + extension table(s)) best suits your dataset next. Then, once you have decided on the dataset structure, you can continue formatting your data.

As a review, there are two possible types of core data tables for OBIS: Occurrence core and Event core. Let's look at each one, and when either should be chosen as your core data table.

## When to use Occurrence Core

Occurrence Core datasets describe observations and specimen records and cover instances when:

- No information on how the data was sampled or how samples were processed is available
- No abiotic measurements are taken or provided
- You have eDNA and DNA-derived data
- Biological measurements are made on individual specimens (each specimen is a single occurrence record)

Occurrence Core is also often the preferred structure for museum collections, citations of occurrences from literature, and sampling activities.

Datasets formatted in Occurrence Core can use the eMoF Extension for when you have biotic measurements or facts about your specimen. The DNA derived data extension can, and should, be used to link to DNA sequences and other bioinformatic data to the Occurrence core. The identifier that links Occurrence Core to the extension(s) is the occurrenceID.

Next, let's learn when Event core should be used.

## When to use Event Core

Event Core describes when and where a specific sampling event happened and contains information such as location and date. Event Core is often used to organize your data tables when there are more than one sampling occasion and/or location, and different occurrences linked to each sampling. This organization follows the rationale of most ecological studies and typical marine sampling design. It covers:

- When specific details are known about how a biological sample was taken and processed. These details can then be defined in the eMoF Extension with the [Q01 vocabulary](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/Q01/)
- When the dataset contains abiotic measurements, or other/biological measurements which are related to an entire sample (not a single specimen). For example a biomass measurement for an entire sample, not each species within the sample

Event Core can be used in combination with the Occurrence and eMoF extensions. The identifier that links Event Core to the extension is the eventID. parentEventID can also be used to give information on hierarchical sampling. occurrenceID can be used in datasets with Event Core in order to link information between the Occurrence extension and the eMoF extension.

## Lesson summary

As established in Module 1, OBIS relies on datasets being formatted with relations to one another. Simply, it means that data tables are connected to each other by identifiers and each data table contains a set of data about a different measurement (or fact) characterizing either the occurrence or the sampling (observation/occurrence). The ENV-DATA approach that OBIS implements means your dataset will have a core table and (optionally) extension table(s). In this lesson we learned more about the core tables integrated by OBIS (Occurrence or Event), and the associated potential extension tables(s) (eMoF, Occurrence, DNA) that are linked to core tables by the use of identifying ID codes. These codes could be either eventID or occurrenceID. We will learn about these important identifiers, and how to construct them, in the next lesson.

To practice your understanding on when to select Occurrence vs Event core, complete Exercise 2-1. See the flowchart below for an additional summary of this lesson and/or to help you in the Exercise.

## Lesson 2: Understanding and constructing Identifiers

## Introduction to Identifiers

As we know, there are two important identifiers that link your data tables: occurrenceID and eventID.

If your core file is based on occurrences (e.g., a record of one or more taxa specimens), then any extensions are linked with occurrenceID. If your core file is based on events (e.g., a sampling event, cruise, observation, etc.), then the linking identifier is eventID. In the Core tables, identifiers are always unique, which means, they do not repeat and each line has a different identifier. On the other hand, multiple records in an extension file can have the same identifier which will link them to the same event or occurrence record (depending on which is the Core). The different linking identifiers are shown in the figure below.

Let's learn how to construct each identifier, beginning with eventID.

## Construct eventID

Using a unique identifier for each physical sample or subsample in your dataset taken at each location and time is highly recommended to ensure sample traceability and data provenance. eventID is an identifier for an individual sampling or observation event, whereas parentEventID is an identifier for a parent event, which is composed of one or more linked (child) events (eventIDs).

eventID can be used to distinguish between replicated samples and/or sub-samples. It is important to make sure each replicate sample receives a unique eventID, which could be based on an already existing unique sample ID in your dataset. Sample ID can also be recorded in materialSampleID, as OBIS does not need to have separate eventIDs and materialSampleIDs. Rather OBIS can treat these two terms as equivalent. But you must still fill in the eventID field if you want to use materialSampleID, as OBIS only uses eventID and parentEventID for structuring datasets, not sample ID. This does not prevent you from using the field if you would like to!

 eventIDs can be machine-generated or human-generated. There are pros and cons to each, but we focus on human-generated IDs in this course.

If you do not already have a sample ID, creating a unique eventID for your samplings can be as straightforward as combining different fields from your data. When constructing eventIDs, ask yourself what is the main information about a sampling event that helps you identify it? For instance, it is helpful when we know the location, date, project, habitat, type of subsample, etc. You can build your eventID code based on this information and ensure IDs will not be repeated for different events (e.g., will result in a unique identifier). When initially constructing eventIDs, there may be rows of data that come from the same event, in this case it's okay for them to have the same eventID. We will learn how to deal with this when we separate occurrences and event information in Module 3.

Note You should consider carefully what combination of fields will generate a unique eventID for each unique event. Combinations including project, date, time, location, and depth are common elements to help generate such unique codes.

Including the [event type](https://dwc.tdwg.org/terms/) can also be useful for datasets with hierarchical sampling methods (e.g., samples taken from a station within a cruise). Repeating the parentEventID (using : or _ as a delimiter) can also make the structure of the dataset easier to understand. Nesting event information in this way also allows you to reduce redundancy while still providing information relevant to each level of sampling.

Broadly, an eventID can take the form of [parentEventID]_[sample type]_[sample ID]

Thus to construct a unique eventID for parent and child events, you can join relevant sampling information. Possible configurations (with examples) could include:

- Project_cruise_station_date_sample
- STAR_arcticsea_st3520_1989-04-04_s01
- Project_habitat_Genus_species_year_sampletype_samplenumber
- BEE_seamount_Genus_species_2013_cruise_s123
- Institution_year_location_samplemethod_sample
- Concordia_2003_Coast_Station1_seine_s01
- Concordia_2003_Coast_Station1_trap_s01

These examples are not exhaustive and other similarly structured variations that fit your data are acceptable. Consider also including year within your eventIDs to ensure codes remain globally unique in subsequent years, which is particularly useful if your sampling protocol is repeated temporally.

Information related to your sampling events can be assigned to the highest relevant event level in order to avoid repetition of information because parent event information is passed to child events. For example, if all samples taken from a station occurred at the same depth, this information can be listed once. Variation between samples (e.g., exact time or coordinates) can also be easily reflected for each event. See the table below for a simple demonstration. However for datasets that will also be harvested by GBIF, we recommend populating both parent and child events because child events in GBIF do not currently inherit information from the parent event like they do in OBIS.

### Examples

### Watch a video

Watch the video below for a demonstration on how to construct eventIDs (available at <https://youtu.be/Upt6LPJ0Bn8>)

<https://youtu.be/Upt6LPJ0Bn8>

## Construct occurrenceID

Similar to eventID, occurrenceID is an identifier to distinguish between occurrence records where each occurrenceID must be unique. Because occurrenceID is a required term, you may have to construct a persistent and globally unique identifier for each of your data records if none already exists (e.g., if records were not labeled with unique identifiers before, such as during sample processing or image/sensor detection).

There are no standardized guidelines yet on designing the persistence of this ID, the level of uniqueness (from within a dataset to globally in OBIS), and the precise algorithm and format for generating the ID. But in the absence of a persistent globally unique identifier, one can be constructed by combining the eventID with institutionCode, the collectionCode and/or the catalogNumber (or autonumber in the absence of a catalogNumber). This will be similar to how eventID is constructed. You may also follow [Life Science Identifiers](https://www.labkey.org/Documentation/wiki-page.view?name=lsidOverview) guidelines. The inclusion of occurrenceID is also a required term for OBIS datasets.

An important consideration for museum specimens: there is the possibility that the institution a specimen is housed at may change. Therefore you may consider omitting institution identifiers within an occurrenceID, because occurrenceID should not change over time.

### Example

Data from [*Algal community on the pneumatophores of mangrove trees of Gazi Bay in July and August 1987*](http://ipt.vliz.be/eurobis/resource?r=vegetation_gazi_bay_kenya_1987).

| occurrenceID | basisOfRecord | institutionCode | collectionCode | catalogNumber |
| --- | --- | --- | --- | --- |
| Ugent_Vegetation_Gazi_Bay(Kenya)1987_7553 | HumanObservation | Ugent | Vegetation_Gazi_Bay(Kenya)1987 | Ugent_Vegetation_Gazi_Bay(Kenya)1987_7553 |
| Ugent_Vegetation_Gazi_Bay(Kenya)1987_7554 | HumanObservation | Ugent | Vegetation_Gazi_Bay(Kenya)1987 | Ugent_Vegetation_Gazi_Bay(Kenya)1987_7554 |
| Ugent_Vegetation_Gazi_Bay(Kenya)1987_7555 | HumanObservation | Ugent | Vegetation_Gazi_Bay(Kenya)1987 | Ugent_Vegetation_Gazi_Bay(Kenya)1987_7555 |

### Watch a video

Watch the video below for an example of how to construct an occurrenceID, which starts at 1:52 and can also be found at <https://youtu.be/G_AmAmS7ILc?t=112>

<https://youtu.be/G_AmAmS7ILc?t=112>

## Lesson Summary

Let's summarize what we have learned about identifiers:

Identifiers link information between data tables

Identifiers should be unique and persistent, they should not change over time

You can construct identifiers from information in your dataset

Now that we have a better understanding of identifiers, practice constructing eventIDs for an example dataset in Exercise 2-2. Then, we will learn about another type of identifier in the next lesson: taxonomic identifiers.

## Lesson 3: Taxon match

## Introduction to Taxonomic Identifiers

In the previous lesson we learned about the identifiers that link our data tables: occurrenceID and eventID . In this lesson we will learn about the taxonomic identifier you should use to fill the Darwin Core field scientificNameID.

OBIS requires all your specimens to be classified and matched against an authoritative taxonomic register. This effectively attaches unique stable identifiers (and digitally traceable) to each of your species. Meaning, if a taxonomic ranking or a species name changes in the future, there will be no question as to which species your dataset is actually referring to. Matching to registers also helps to avoid misspelled or unused terms.

OBIS currently accepts identifiers from three authoritative lists:

[World Register Marine Species (WoRMS)](https://www.marinespecies.org/) LSIDs

[Integrated Taxonomic Information System (ITIS)](https://www.itis.gov/) TSNs

[Barcode of Life Data Systems (BOLD)](https://biodiversitygenomics.net/projects/bold/) and [NCBI](https://www.ncbi.nlm.nih.gov/) identifiers

The identifiers (LSID, TSN, ID) from these registers will be used to populate the scientificNameID field. OBIS can accept other LSIDS besides WoRMS, as long as they are mapped in WoRMS. If you would like to include multiple identifiers, please use a concatenated list where each register is clearly identified (e.g. [urn:lsid:itis.gov:itis_tsn:12345](urn:lsid:itis.gov:itis_tsn:12345), NCBI:12345, BOLD:12345). You can also use the [Interim Register of Marine and Nonmarine Genera (IRMNG)](https://www.irmng.org/aphia.php?p=search) to distinguish marine genera from freshwater genera.

You should prioritize using LSIDs because they are unique identifiers that indicate the authority the ID comes from. We recommend obtaining LSIDs from WoRMS because this is the taxonomic backbone that OBIS relies on, and it is built on marine systems as well as linked to the other taxonomic authoritative lists. Let's take a look at how to go about matching a list of scientific names to a register like WoRMS.

## Taxon Matching Workflow

To match the scientific names in your dataset, you should follow this Name Matching workflow:

This workflow can be broken into two main steps 1) Match with WoRMS, 2) Match with other registers.

Let's start by taking a look at how to match to WoRMS by watching the video below on how to use the WoRMS Taxon Match tool (available <https://youtu.be/jJ8nlMlg-cY>):

 <https://youtu.be/jJ8nlMlg-cY>

Now let's review the steps involved in matching to WoRMS.

## Matching with WoRMS

The procedure for matching to WoRMS and then attaching successful matches back to your data can be simplified to:

- Prepare a file (.csv, .txt, .xlsx, etc.) with the list of your specimens/taxa
- Upload the file to WoRMS taxon match tool
- Check relevant boxes
- Review returned file
- Resolve any ambiguous matches
- Download file and identify data to include in your Occurrence data table for OBIS (LSIDs, taxonomic fields, etc.)
- Attach LSIDs back to your data using e.g.:
  - R (merge)
  - Excel (vlookup)

The taxon match tool of the World Register of Marine Species (WoRMS) is an automatic way to match and download the taxonomic information about your occurrence records, without having to look for each name in the site. It is available at [http://www.marinespecies.org/aphia.php?p=match](http://www.marinespecies.org/aphia.php?p=match). The WoRMS taxon match will compare your taxon list to the taxa available in WoRMS.

This taxon match takes into account exact matches and fuzzy matches. Fuzzy matches include possible spelling variations of a name available in WoRMS. WoRMS also identifies ambiguous matches, indicating that several potential matching options are available (e.g. homonyms). You can check these ambiguous matches and select the correct one, based on e.g., the general group information (a sponge dataset) or the authority. If this would be impossible with the available information (e.g., missing authority or very diverse dataset), then you need to contact the data provider for clarification.

For performance reasons, the limit is set to 1,500 rows for the taxon match tool. Larger files can be sent to [info@marinespecies.org](mailto:info@marinespecies.org) and will be returned as quickly as possible. In case you have recorded a taxon that is not registered in WoRMS (e.g., newly discovered species), you should contact them so the database can be updated.

After matching, the tool will return you a file with the AphiaIDs, LSIDs, valid names, authorities, classification, and any other output you have selected.

**Reminder:**The WoRMS LSID is used to populate DwC:scientificNameID.

A complete online manual is available at [http://www.marinespecies.org/tutorial/taxonmatch.php](http://www.marinespecies.org/tutorial/taxonmatch.php).

## Match with other registers

If you do not find a match with WoRMS and there are no spelling or formatting errors, you should next check other registers.

The [LifeWatch taxon match](http://www.lifewatch.be/data-services/) compares your taxon list to multiple taxonomic standards. Matching with multiple registers gives an indication of the correct spelling of a name, regardless of its environment. If a name would not appear in any of the registers, this could indicate a mistake in the scientific name and the name should go back to the provider for additional checking/verification. You will need to create a login to use the LifeWatch taxon match.

Contrary to the WoRMS taxon match, when several matching options are available, the LifeWatch taxon match only mentions “no exact match found, multiple possibilities” instead of listing the available options. If multiple options are available, these should be looked up and matched manually.

Currently, the LifeWatch web service matches the scientific names with the following taxonomic registers:

[World Register of Marine Species - WoRMS](https://www.marinespecies.org/aphia.php?p=match)

[Catalogue of Life - COL](https://www.catalogueoflife.org/data/search?facet=rank&facet=issue&facet=status&facet=nomStatus&facet=nameType&facet=field&facet=authorship&facet=extinct&facet=environment&limit=50&offset=0&sortBy=taxonomic)

[Integrated Taxonomic Information System - ITIS](https://www.itis.gov/)

[Pan-European Species-directories Infrastructure - PESI](http://www.eu-nomen.eu/portal/)

[Index Fungorum – IF](https://www.indexfungorum.org/)

[International Plant Names Index – IPNI](https://www.ipni.org/)

[Global Names Index - GNI](https://globalnames.org/apps/gni/)

[Paleobiology Database - PaleoDB](https://paleobiodb.org/)

Keep in mind that it can take some time for the service to process your file, which is why we recommend using the WoRMS taxon match service first.

After cross-referencing with LifeWatch, if you still have non matching species, the next step is to confirm if the taxa are marine.

## Non-marine taxa

To check whether a taxon is marine or not, you can cross-reference with the Interim Register of Marine and Non-marine Genera (IRMNG). These matching services are available through [http://www.irmng.org/](http://www.irmng.org/) (as well as through the [LifeWatch taxon match](http://www.lifewatch.be/data-services/)).

This service allows you to search for a genus (or other taxonomic rank when you uncheck the “genera” box, see below) to check if it is known to be marine, brackish, freshwater, or terrestrial. You can find this information in the row labeled “Environment”. If the taxon is marine but is not listed in WoRMS, you may have to contact the WoRMS data management team (<info@marinespecies.org>) to have the taxon added to the WoRMS register. You will have to provide supporting information confirming taxonomic and marine status.

## Watch videos

The video below demonstrates several examples of how to resolve fuzzy or ambiguous matches from WoRMS, including how to deal with non-marine species (available at <https://youtu.be/yZKwtr14JVM>):

Play Video

This video reviews an example of species synonyms (available at <https://youtu.be/-vIkS8U7Crk>):

Play Video

Let's next look at a summary for all the tools available to you to complete taxon matching.

## Tools for Taxon matching

There are a number of tools available to help you match a list of names to WoRMS. We have summarized these tools the table below, as well as advantages and disadvantages for each.

| Tool | Advantage | Disadvantage |
| --- | --- | --- |
| [WoRMS taxon match](https://www.marinespecies.org/aphia.php?p=match) | Accessible online Does not require coding knowledge | Requires rematch information back to your data ​​The limit is set to 1,500 rows |
| [R package obistools](https://github.com/iobis/obistools)::match_taxa | Produces same output as WoRMS taxon match Already in R so easier to merge back with data Allow user to match taxa interactively Do not need to loop over data | Outputs a tibble for taxa names specified Requires knowledge of R |
| [R package worrms](https://cran.r-project.org/web/packages/worrms/index.html)::wm_records_taxamatch | Outputs all taxonomic information from WoRMS (taxonomic hierarchy, rank, authority) No limit | Outputs a tibble for each taxa name specified Requires knowledge of R |
| [Python pyworms](https://github.com/iobis/pyworms) | Simple and easy to understand output | Requires knowledge of python Challenging to deal with when multiple matches occur |

Practice using the WoRMS Taxon Match tool in Exercise 2-3, and then move to the next lesson where we will learn about standardizing dates, coordinates, and how to resolve missing required fields.

## Lesson 4: Addressing common formatting issues

## Common formatting issues

In this lesson we review a few common challenges you may encounter during data formatting, including:

- Standardizing dates and times
- Converting coordinates
- Missing required fields

We will look at each topic in preparation for formatting core and extension data tables in the next module. You may find it easier to standardize data formatting before creating your data tables, or you may find it easier to do this after separating data into the relevant data table. For this course, we will discuss standardizing data first. (Although during the data formatting exercises you may notice some fields are not yet standardized, this is intentional).

## Dates and times

The date and time at which an event took place or an occurrence was recorded go in eventDate. This field uses the [ISO 8601 standard](https://en.wikipedia.org/wiki/ISO_8601) and we recommend using the extended ISO 8601 format with hyphens. Note that all dates in OBIS become translated to UTC during the quality control process implemented by OBIS. Formatting your dates correctly ensures there will be no errors during this process.

ISO 8601 dates can represent moments in time at different resolutions, as well as time intervals, which use / as a separator. Date and times are separated by T. Timezones can be indicated at the end by using + or - the number of hours offset from UTC. If no timezone is indicated, then the time is assumed to be local time. When a date/time is recorded in UTC, a Z should be added at the end. Times must also be written in the 24-hour clock system. If you do not know the time, you do not have to provide it. Please do not indicate unknown times as “00:00” as this indicates midnight.

Not every piece of time information is necessary, but a generalization of how to format dates and times looks like:

`YYYY-MM-DDT[hh]:[mm]:[ss][+/-XX OR Z]`

Some specific examples of acceptable ISO 8601 dates are:

**Dates:**

- 1948-09-13
- 1993-01/02
- 1993-01
- 1993

**Dates with Specific Times:**

- 1973-02-28T15:25:00
- 2008-04-25T09:53

**Dates with Time Zones:**

- 2005-08-31T12:11+12
- 2013-02-16T04:28Z

**Date and Time Intervals:**

- 1993-01-26T04:39+12/1993-01-26T05:48+12

It is important to note that ISO 8601 ordinal dates (YYYY-DDD) and week dates (YYYY-Www-D), are not supported by OBIS. Additionally, ISO 8601 guidelines for [durations](https://en.wikipedia.org/wiki/ISO_8601) should not be used. Durations for an event (e.g., length of observation) can instead be indicated with the DwC terms [startDayOfYear](https://dwc.tdwg.org/list/) and [endDayOfYear](https://dwc.tdwg.org/list/). Durations refer to the actual length of time an event (e.g., occurrence) occurred, whereas intervals indicate the time period during which an event was recorded.

A note about intervals… Take care when entering date intervals as, for example, entering 1960/1975-08-04 indicates that the event or observation started any time in 1960, and ended any time on 1975-08-04. If you know the exact date and time, you should specify that information. This also helps for continuous samplings and time-series integrated datasets.

If you have a mix of dates and times for different aspects of a sampling event, you can embed this information in the Event Core table using hierarchies of date structure. To do this, you can use separate records for events, and specify each event date individually.

It is good practice to place the original event date/time description into the verbatimEventDate field. Any modifications you make during data formatting should be recorded in the eventRemarks field, and we recommend taking good notes in a Documentation sheet (e.g. an extra tab).

You may watch this video for a demonstration on formatting dates according to ISO 8601 standards (<https://youtu.be/0xIJWdIeTxo>)

 <https://youtu.be/0xIJWdIeTxo>

## Converting coordinates

All coordinates provided in the decimalLatitude or decimalLongitude fields in OBIS must be provided in decimal degrees. To convert coordinates from degrees-minutes-seconds into decimal degrees, you can use the OBIS [Coordinate Conversion tool](https://obis.shinyapps.io/coordinates/). This tool will convert any coordinate or list of coordinates from degrees-minutes-seconds to decimal degrees, even partial coordinates. Simply copy and paste your coordinates into the box provided and click Convert. For example:

Watch this video for a demonstration on how to use the OBIS coordinate conversion tool (<https://youtu.be/E_TkWIUcoJw>)

 <https://youtu.be/E_TkWIUcoJw>

## Missing required fields

As a reminder, there are 8 required fields to publish a dataset to OBIS. To resolve missing fields marked as required by OBIS, follow the guidelines below for each term.

### occurrenceID or eventID

Create a unique occurrenceID for each of your observations following the guidelines in this module. Recall that these IDs can be generated by combining dates, location names, sampling methods, and/or existing identifiers in the dataset.

### eventDate

Ensure your eventDate is specified for each event, formatted according to [ISO 8601 standards](https://en.wikipedia.org/wiki/ISO_8601) (e.g., YYYY-MM-DD) as we learned earlier in this lesson. For any eventDate that is inferred from literature, you should document the original date in the verbatimEventDate field.

### decimalLongitude and decimalLatitude

Make sure all coordinates are converted into decimal degrees, as we learned previously. If you do not have specific coordinate data then you must approximate the coordinates based on locality name. You can use the [Marine Regions gazetteer](https://www.marineregions.org/gazetteer.php?p=search) to search for your region of interest and obtain midpoint coordinates. Guidelines for using this tool and for dealing with uncertain geolocations can be found on the [OBIS manual](https://manual.obis.org/common_qc.html), and we will learn more about this in Module 6. You will need to document georeferencing processes in the georeferenceRemarks field if you estimate coordinates.

### scientificName

This field should contain only the originally documented scientific name down to the lowest possible taxon rank, even if it is a current synonym. Class or even Kingdom levels are accepted if more specific taxonomic levels are unknown. Comments about misspellings, etc. can be documented in the taxonRemarks field. You may encounter challenges filling this field if the species name is based on description or if its taxonomy was uncertain at the time of sampling. For such uncertain taxonomy situations, see guidelines in Module 6 or the relevant section of the [OBIS Manual](https://manual.obis.org/common_qc.html).

### scientificNameID (strongly recommended)

If you cannot obtain the required Life Science Identifier (LSID) from taxon matching with WoRMS then you must contact World Register Marine Species (<info@marinespecies.org>) to have an LSID created for your taxon. You will need to confirm that the species is marine. OBIS does not currently parse LSIDs from other sources (e.g., [Integrated Taxonomic Information System](https://www.itis.gov/), [Catalog of Life](https://www.catalogueoflife.org/)), but if you want to include other LSIDs alongside the WoRMS LSID, they must be specified in a predictable format.

### occurrenceStatus

Because occurrenceStatus is a binary field (“presence” or “absence”) this field can usually be easily inferred by data. If there are associated measurements or a record of an observation, the taxon in question would be present. If a particular species/taxa is present in one sample, but missing from another, then you could identify that species as absent from the second sample.

### basisOfRecord

basisOfRecord distinguishes what type of record is in your data. For records pertaining to a collected or stored specimen, you must choose one of the following terms: PreservedSpecimen, FossilSpecimen, LivingSpecimen. For records pertaining to an observation in the wild, you should use HumanObservation (e.g., observation in the wild) MachineObservation (e.g., photograph, DNA sequences). For records pertaining to DNA-derived data, you should use MaterialSample. For records pertaining to literature data, basisOfRecord should always reflect the evidence upon which the Occurrence record was based. For example, a researcher’s record based on photographs should specify MachineObservation, otherwise specifications should be HumanObservation (see [relevant GitHub discussion](https://github.com/tdwg/dwc-qa/issues/134)).

## Lesson Summary

In this lesson we reviewed some common challenges you might encounter during data formatting. We focused on standardizing dates to ISO 8601, converting coordinates to decimal degrees, and addressing missing required fields.

## End of Module

You have completed Module 2! Please complete Exercises 2-1, 2-2, 2-3 if you haven't already, as well as Quiz 2 before moving on to Module 3, where we will learn how to create the Event, Occurrence, and extendedMeasurementOrFact tables.
