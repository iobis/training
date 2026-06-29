# Module 1: Introduction to data publishing and Darwin Core

## Introduction

In this module you will learn about the data standards that OBIS adheres to: Darwin Core and Ecological Metadata Language (EML). You will first learn why it is important and how it can be beneficial to publish your data to OBIS, how sensitive data can be handled, and see an introduction to the OBIS data life cycle. The bulk of this module will focus on learning about Darwin Core, as adhering to data standards is part of what makes OBIS an important and community endorsed resource for open access, quality-controlled ocean data.

## Learning Outcomes

After successful completion of this module, you should be able to:

* Identify the five steps to publishing data in OBIS, list the types of marine data OBIS accepts
* Understand the four FAIR principles, explain why sharing data is important and why data standards are beneficial
* Identify qualities of sensitive data and know how to generalize sensitive location data
* Understand Darwin Core principles and know where to find Darwin Core terms
* Understand the difference between flat vs relational databases
* Explain how core tables are linked with extension tables
* Explain how star schema works to reduce redundancy
* Understand how Darwin Core extension files are linked to the core file

## How to Proceed

To succeed in this Module, you need to complete the following lessons and exercise:

* Lesson 1: The who, what, where, why, and how regarding data contributions
* Lesson 2: Introducing biodiversity standards
* Lesson 3: The Darwin-Core Archive: Core tables and Extensions
* [Exercise 1-1: Map terms to Darwin Core](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32911)
* as well successfully complete Quiz 1 with a score of ≥80%
* [Quiz 1](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32912)

## Lesson 1: The who, what, where, why, and how regarding data contributions

## What kind of data can you publish to OBIS?

Since 2000, OBIS has accepted, curated and published marine biodiversity data obtained from varied sources and methods. There is a common misconception that OBIS only accepts species occurrence data - however, this is not true! OBIS can accept many types of marine data including:

* Occurrence and distribution
* Presence/Absence
* Abundance, individual count
* Biomass, size, and life stage
* Abiotic measurements
* Biotic measurements
* Sampling methods and study area
* Sample processing protocols
* Genetic data including sequences
* Data originating from historical records and time series
* Tracking data
* Habitat data
* Acoustic data
* Imaging data
* Metadata describing the dataset and any affiliated project or programme and personnel
* And many other variables associated to the sighting of marine organisms

If you have any of these types of marine data you can contribute these to OBIS! OBIS repository and standards are prepared to cover all data linked to marine life and its community of practice. OBIS accepts data from any organization, consortium, project or individual who wants to contribute data. OBIS Data Sources are the authors, editors, and/or organizations that have published one or more datasets through OBIS, they remain the owners or custodians of the data, not OBIS!

## Five steps to publish

OBIS harvests and publishes data from recognized Integrated Publishing Toolkits (IPTs, "digital repositories") from OBIS nodes or [GBIF ](https://www.gbif.org/)publishers. If you own data or have the right to publish data in OBIS, which means the data will become openly available, you can contact the [OBIS secretariat or one of the OBIS nodes](https://obis.org/contact/) (or additionally a GBIF publisher). Your organization or programme can also [become an OBIS node](https://manual.obis.org/nodes.html). An OBIS node usually publishes data from multiple data holders, effectively being a connection in a network of data providers. You may have to first find a [relevant node](https://obis.org/contact/) before you get your data ready to publish. OBIS nodes can cover countries, regions, themes, and/or communities. If you cannot decide on who to contact, please, reach out to the OBIS Secretariat for support.

To publish a dataset to OBIS, there are five main steps you must go through:

1. Identify which OBIS node is best suited to host your published data. If you would like to publish to GBIF at the same time, that is also possible. Details about this will be covered in Module 7.
2. Determine the structure of your data and which format will best suit your dataset. OBIS follows Darwin Core Archive (DwC-A) standards for datasets, and currently adopts a star schema format for data organization. This format is based on relational databases. If you are unfamiliar with such database structures, or would like to refamiliarize yourself with them, we will review that later in this module.
3. Format your data according to OBIS and DwC-A standards and guidelines. We will cover formatting in Modules 2, 3, and 4, with specific guides on using controlled vocabularies in Module 5.
4. Run a series of quality control measures to ensure you are not missing any required information and that all standards are being met. This helps ensure all data published in OBIS is formatted in a standardized way. When published in OBIS, OBIS provides a quality report to inform data owners and users of any quality control issues. By completing quality control before you publish your dataset you ensure there are fewer errors to fix later. We will review quality control steps in Module 6.
5. Once your dataset is ready for publishing, the relevant metadata must be filled in, and then published on the previously identified IPT. Both topics will be covered in Module 7.

## Why publish data & The FAIR Principles

It is important to publish and ensure your dataset follows a universal standard for several reasons. The [FAIR guiding principles](https://www.nature.com/articles/sdata201618) for scientific data management and stewardship provide a good framework to understand the reasoning behind publishing standardized data. FAIR stands for Findable, Accessible, Interoperable, and Reusable. Let’s understand each aspect within the FAIR framework and how it is linked to publishing data in OBIS.

### F - Findable

Even if you publish your dataset on its own, publishing your data with OBIS will make your data more Findable (and Accessible) to a wider audience you might not have otherwise reached. By publishing your dataset to OBIS you are adding to a global database where your data can be found and analyzed alongside thousands of other datasets. For example, a dataset on [marine invasive species in Venezuela](https://obis.org/dataset/98ea98e8-356b-4dd5-87c6-4eeb7e3cf588) was published July 20, 2022 and as of October 5, 2022 records of this dataset were included in 1,873 data download requests. This can save you time rather than handling individual data requests. OBIS can also provide a Digital Object Identifier (DOI) to your dataset, which means that it can be easily found and tracked throughout the different information systems.

### A - Accessible

Similar to being Findable, OBIS makes your datasets more Accessible. Each dataset is given an identifier when you upload it on an IPT (covered in Module 7). Thus when users obtain data from OBIS, the original dataset can easily be identified, accessed, and downloaded. Data from OBIS is accessible in numerous ways (covered in Module 8), giving data users multiple avenues to potentially access your data.

### I - Interoperable

Using a standardized data format with controlled vocabularies will ensure your data are more Interoperable - more easily interpreted and processed by computers and humans alike. Increasingly, scientists use computer programs to conduct e-Science and collect data with algorithms. Formatting your data for OBIS will ensure it can be read and accessed by such programs as well as understood by users. OBIS standards also facilitates the connection between your data and other ocean and geospatial information systems.

### R - Reusable

Publishing your data allows it to be Reused according to your chosen data usage license. Very likely you expended a lot of resources and time to collect your data and it would be a waste to leave your unique data inaccessible for current and future generations. Likewise, it is better to preserve any data processing done to ensure your dataset is reproducible and/or verifiable. Finally, data in OBIS is often used in several assessment processes and used as information to support policymakers around the globe in making informed decisions.

## Data publishing benefits

There are many other benefits of submitting data to OBIS, even if you haven’t published any other work on it yet (e.g. scientific papers). This includes:

* Your dataset can be associated with a DOI, allowing for your dataset to be more easily tracked and cited. By ensuring your dataset citation is complete you will ensure you are being acknowledged properly. DOI generation will be covered in Module 7.
* Publishing your dataset with OBIS makes it easier to set it up as a [Data paper](https://www.gbif.org/data-papers) or to add your data as supplementary material to your manuscripts, which generates value for you and other researchers.
* When following OBIS publishing requirements, your data will adhere to FAIR standards. OBIS repository also ensures digital safety and promotes a legacy for future generations, not always possible in local repositories.
* You have the means and tools to control modifications in your data and decide when it is published publicly, making it possible to incorporate project or institutional embargos.
* There are social benefits to data publishing as your work becomes integrated into a wider dataset and a community of data providers. It gives both you and your data more visibility. This can lead to more opportunities for collaboration and further career development as a researcher or professional.
* Your data can be incorporated into larger analyses to better understand global ocean biodiversity, helping to shape regional and international policies.
* OBIS provides a large set of tools to further visualize and process your data once published. These scripts and guides markedly optimize our capacity to deliver meaningful information to society.

## How to handle sensitive data

Sometimes your dataset may contain sensitive information (e.g., location data on endangered or poached species), or perhaps your organization does not want certain details publicly accessible. Types of sensitive data include:

* Location data on endangered or protected species
* Information regarding a commonly poached species
* Species or locations that have an economic impact (positive or negative)

To accommodate sensitivity but still be able to contribute to OBIS, we suggest:

* Generalizing location information by: Obtaining regional coordinates using [MarineRegions](http://www.marineregions.org/gazetteer.php?p=search), [Getty Thesaurus of Geographic Names](http://www.getty.edu/research/tools/vocabularies/tgn/), or [Google Maps](http://maps.google.com/)
* Using the [OBIS Map tool](https://obis.org/maptool/) to generate a polygon area with a Well-Known Text (WKT) representation of the geometry to paste into the footprintWKT field
* Delay timing of publication
* Submit your dataset, but mark it as private in the IPT so it is not published right away (i.e., until you set it as public). Alternatively, you can set a password on your dataset in order to share with specific individuals. Note that setting passwords will require some coordination with the IPT manager. By submitting your data to an IPT but not immediately publishing it, you can ensure that the dataset will be in a place to be incorporated at a later date when it is ready to be made public. This not only saves time and helps retain details while relatively fresh in your mind, but also ensures the dataset is still ready to be mobilized in case jobs are changed at a later date.

Details, procedures, and use for each of these tools will be covered in Modules 6 and 7. GBIF has also created the following [Best Practices for Generalizing Sensitive data](https://docs.gbif.org/sensitive-species-best-practices/master/en/) which can provide you with additional guidance, citation below:

Chapman AD (2020) Current Best Practices for Generalizing Sensitive Species Occurrence Data. Copenhagen: GBIF Secretariat. [https://doi.org/10.15468/doc-5jp4-5g10](https://doi.org/10.15468/doc-5jp4-5g10).

## Lesson 2: Introducing biodiversity standards

## Introduction to biodiversity standards

From the very beginning, OBIS has championed the use of international standards for biogeographic data. Without agreement on the application of standards and protocols, OBIS would not have been able to build a large central database. OBIS uses the following standards:

* Darwin Core
* Ecological Metadata Language
* Darwin Core Archive and dataset structure

For those wishing to publish biodiversity-based data to OBIS (or GBIF which uses similar standards), following the above data standards will make published data more easily discoverable, fit for use, less ambiguous and more understandable by data users, more interoperable by APIs and data analysis tools, more easily integrated into online resources, and ensure the data adheres with [FAIR data principles.](https://www.go-fair.org/fair-principles/)

The following pages of this lesson will review each of the above data standards in turn. We will learn how to actually apply these standards to format data in Modules 2 and 3.

## Darwin Core

### Introduction to Darwin Core

[Darwin Core](http://rs.tdwg.org/dwc/) (DwC) is a body of standards (i.e., identifiers, labels, definitions) that facilitate sharing biodiversity informatics and the adaptation of data about life to the digital world. It provides stable [terms](https://dwc.tdwg.org/terms/) and vocabularies related to biological objects/data and their collection, being built and continuously updated by a large international community of data providers and practitioners. Darwin Core is maintained by [TDWG (Biodiversity Information Standards, formerly The International Working Group on Taxonomic Databases)](http://tdwg.org/). Stable terms and vocabularies are important for ensuring the datasets in OBIS remain adequate for use by ensuring consistency of interpretation. By following Darwin Core standards, both data providers and users can be certain of the definition and quality of data.

### A Brief History on DwC and OBIS

The old [OBIS schema](http://old.iobis.org/node/304) was an OBIS extension to Darwin Core 1.2., which was based on [Simple Darwin Core](http://rs.tdwg.org/dwc/terms/simple/), a subset of Darwin Core that does not allow any structure beyond rows and columns. This old schema added some terms which were important for OBIS, but were not supported by Darwin Core at the time (e.g., start and end date and start and end latitude and longitude, depth range, lifestage, and terms for abundance, biomass and sample size).

In 2009, the Executive Committee of TDWG announced their ratification of an updated version of Darwin Core as a [TDWG Standard](https://www.tdwg.org/community/dwc/). Ratified Darwin Core unifies specializations and innovations emerging from diverse communities, and provides guidelines for ongoing enhancement. The [Darwin Core Quick Reference Guide](https://dwc.tdwg.org/terms/) links to TDWG’s term definitions and related practices for Ratified Darwin Core.

In December 2013, the [3rd session of the IODE Steering Group for OBIS](https://obis.org/about/) agreed to transition OBIS globally to the TDWG-Ratified version of Darwin Core, and the mapping of the (old) OBIS specific terms to Darwin Core can be found [here](http://rs.tdwg.org/dwc/terms/history/versions/index.htm).

Next we will discuss the relevance of various DwC term classes.

## Darwin Core Classes

DwC terms correspond to the column names of your dataset (as in an Excel table or sample spreadsheet) and can be grouped according to class type for convenience, e.g., Taxa, Occurrence, Record, Location, etc. It is important to use DwC field names because only columns using the exact Darwin Core terms as headers will be recognized by OBIS. We'll see more about these mappings during publishing in Module 7.

There are currently seven required and one strongly recommended DwC terms for publishing datasets to OBIS:

1. occurrenceID
2. eventDate
3. decimalLongitude
4. decimalLatitude
5. scientificName
6. scientificNameID (strongly recommended)
7. occurrenceStatus
8. basisOfRecord

A list of all possible Darwin Core terms and their definitions can be found in the [Darwin Core Quick Reference Guide on TDWG](https://dwc.tdwg.org/terms/). We recommend bookmarking this page as it can be a valuable resource to refer back to when formatting different datasets. You can add columns to your dataset according to the available DwC terms, and map your datasheet column names to DwC terms. Note that mapping columns may be one to one, one to many, or many to one, depending on how data was originally recorded. We'll see examples of this throughout the course.

However, OBIS does not currently parse all DwC terms. This doesn’t mean you cannot include them in your dataset, just know that they may not be parsed when you publish to OBIS - they will remain in your source dataset though. There is a convenient [checklist of OBIS-accepted terms](https://manual.obis.org/checklist.html) available in the OBIS manual.

In the next few pages, we will review most of the relevant Darwin Core terms within different classes to consider when contributing to OBIS, along with guidelines regarding their use. Understanding the meaning of each term will help you organize and format your dataset in a way that not only you, but many other users will be able to interpret it.

There are seven main Darwin Core term class types that are important for OBIS: Taxon,  Identification,  Occurrence, Record, Location, Event, and Material Sample. We will review each of these classes, the important terms within them, and the associated guidelines for their use.

## Taxon and Identification Terms

We begin by reviewing DwC terms related to the classes Taxon and Identification because these class types usually contain associated information. Note there is no relationship between terms presented below, they are presented side by side for ease of reference.

| Taxon | Identification |
| --- | --- |
| scientificName | identifiedBy |
| scientificNameID | dateIdentified |
| scientificNameAuthorship | identificationReferences |
| kingdom | identificationRemarks |
| taxonRank | identificationQualifier |
| taxonRemarks | typeStatus |

scientificName (required term) should always contain the originally recorded full scientific name, even if the name is currently a synonym. This is necessary to be able to track back records to the original dataset. The name should include authorship and date information if known and should be the lowest possible taxonomic rank that can be determined, preferably at species level or lower, but higher ranks, such as genus, family, order, class etc. are also acceptable. You may still populate scientificNameAuthorship for authorship, but it is preferred to include authorship in scientificName as well.

The scientificName term should only contain the name and not identification qualifications (such as ?, confer or affinity), which should instead be supplied in the identificationQualifier term, see examples below. taxonRemarks can capture comments or notes about the taxon or name.

A [WoRMS](http://www.marinespecies.org/) LSID should be added in scientificNameID (a strongly recommended term for each occurrence record). We review how to navigate the WoRMS interface and obtain LSIDs in Module 2. OBIS will use the WoRMS identifier to pull the taxonomic information from the World Register of Marine Species (WoRMS) into OBIS and attach it to your dataset. This information includes:

* Taxonomic classification (kingdom through species)
* The accepted name in case of invalid names or synonyms
* AphiaID
* IUCN red list category

LifeScience Identifiers (LSIDs) are persistent, location-independent, resource identifiers for uniquely naming biologically significant resources. More information on LSIDs can be found at [www.lsid.info](http://www.lsid.info/). For example, the WoRMS LSID for Solea solea is: [urn:lsid:marinespecies.org:taxname:127160](urn:lsid:marinespecies.org:taxname:127160), and can be found at the top of each WoRMS taxon page, e.g. [*Solea solea*](http://marinespecies.org/aphia.php?p=taxdetails&id=127160).

kingdom and taxonRank can help us in identifying the provided scientificName in case the name is not available in WoRMS. kingdom in particular can help us find alternative genus-species combinations and avoids linking the name to homonyms (and is recommended by both OBIS and GBIF). Please contact the WoRMS data management team ([info@marinespecies.org](mailto:info@marinespecies.org)) in case the scientificName is missing in WoRMS. kingdom and taxonRank are not necessary when a correct scientificNameID is provided.

OBIS recommends providing information about how identification was made, for example by which ID key, species guide or expert; and by which method (e.g morphology vs. genomics), etc. The person’s name who made the taxonomic identification can go in identifiedBy and when in dateIdentified. Use the ISO 8601:2004(E) standard for date and time (we will review use of this standard later). A list of references, such as field guides used for the identification can be listed in identificationReferences. Any other information, such as identification methods, can be added to identificationRemarks.

If the record represents a nomenclatural type specimen, the term typeStatus can be used, e.g. for holotype, syntype, etc.

In case of uncertain identifications, and the scientific name contains qualifiers such as cf., ? or aff., then this name should go in identificationQualifier, and scientificName should contain the name of the lowest possible taxon rank that refers to the most accurate identification. E.g. if the specimen was accurately identified down to genus level, but not species level, then the scientificName should contain the name of the genus, the scientificNameID should contain the LSID of the genus, and the identificationQualifier should contain the uncertain species name combined with ? or other qualifiers. The table below shows a few examples:

The use and definitions for additional NO signs (identificationQualifier) can be found in [Open Nomenclature in the biodiversity era](https://doi.org/10.1111/2041-210X.12594), which provides examples for using the main Open Nomenclature qualifiers associated with physical specimens. The publication [Recommendations for the Standardisation of Open Taxonomic Nomenclature for Image-Based Identiﬁcations](https://www.frontiersin.org/articles/10.3389/fmars.2021.620702/full) provides examples and definitions for identificationQualifiers for non-physical specimens (image-based).

## Examples

The following example is from [*Benthic fauna around Franz Josef Land*](http://ipt.vliz.be/eurobis/resource?r=largenet_k2).

| scientificNameID | scientificName | kingdom | phylum | class | order | family | genus | specificEpithet | scientificNameAuthorship |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| [urn:lsid:marinespecies.org:taxname:142004](urn:lsid:marinespecies.org:taxname:142004) | Yoldiella nana | Animalia | Mollusca | Annelida | Nuculanoida | Yoldiidae | Yoldiella | nana | Sars M., 1865 |
| [urn:lsid:marinespecies.org:taxname:140584](urn:lsid:marinespecies.org:taxname:140584) | Ennucula tenuis | Animalia | Mollusca | Annelida | Nuculanoida | Nuculidae | Ennucula | tenuis | Montagu, 1808 |
| [urn:lsid:marinespecies.org:taxname:131573](urn:lsid:marinespecies.org:taxname:131573) | Terebellides stroemii | Animalia | Annelida | Polychaeta | Terebellida | Trichobranchidae | Terebellides | stroemii | Sars, 1835 |

The examples below demonstrate how to capture taxonomic uncertainty with DwC terms:

| scientificName | scientificNameID | taxonRank | identificationQualifier | verbatimIdentification |
| --- | --- | --- | --- | --- |
| Pelagia Péron & Lesueur, 1810 | [urn:lsid:marinespecies.org:taxname:135262](urn:lsid:marinespecies.org:taxname:135262) | genus | gen. nov. | Pelagia gen. nov. |
| Pelagia benovici Piraino, Aglieri, Scorrano & Boero, 2014 | [urn:lsid:marinespecies.org:taxname:851656](urn:lsid:marinespecies.org:taxname:851656) | species | sp. nov | Pelagia benovici sp. nov |
| Gadus Linnaeus, 1758 | [urn:lsid:marinespecies.org:taxname:125732](urn:lsid:marinespecies.org:taxname:125732) | genus | cf. morhua | Gadus cf. morhua |
| Aristeidae Wood-Mason in Wood-Mason & Alcock, 1891 | [urn:lsid:marinespecies.org:taxname:106725](urn:lsid:marinespecies.org:taxname:106725) | family | stet. | Aristeidae stet. |

## Occurrence Terms

The following DwC terms are related to the Class Occurrence:

* occurrenceID
* occurrenceStatus
* recordedBy
* individualCount (OBIS recommends to add measurements to ExtendedMeasurementOrFact, eMoF)
* organismQuantity (OBIS recommends to add measurements to eMoF)
* organismQuantityType (OBIS recommends to add measurements to eMoF)
* sex (OBIS recommends to add measurements to eMoF)
* lifeStage (OBIS recommends to add measurements to eMoF)
* behavior
* associatedTaxa
* occurrenceRemarks
* associatedMedia
* associatedReferences
* associatedSequences
* catalogNumber
* preparations

occurrenceID (required term) is an identifier for the occurrence record and should be persistent and globally unique. If the dataset does not yet contain (globally unique) occurrenceIDs, then they should be created. We will review identifier creation in Module 2.

occurrenceStatus (required term) is a statement about the occurrence or sighting of a taxon at a location and at a specific time. It is an important term because it allows us to distinguish between presence and absence records. It is a required term and should be filled in with either present or absent.

A few terms related to quantity: organismQuantity and organismQuantityType, have been added to the TDWG ratified Darwin Core. This is a lot more versatile than the older individualCount field. However, OBIS recommends the use of the extendedMeasurementorFact extension for quantitative measurements because of the standardization of terms and the fact that you can link them to sampling events and factual sampling information. We will review use of this extension in Module 3.

In the case where specimens were collected and stored (e.g. museum collections), the catalogNumber and preparations terms provide the identifier for the record in the collection and to document the preparation and preservation methods. The term [typeStatus ](https://dwc.tdwg.org/terms/)can be used in this context too (listed on the previous page as a DwC Class:Identification term).

associatedMedia, associatedReferences and associatedSequences can contain global unique identifiers or URIs pointing to respectively associated media (e.g. online image or video), associated literature (e.g. DOIs), or genetic sequence information (e.g. GenBANK ID).

associatedTaxa include a list (concatenated and separated) of identifiers or names of taxa and their associations with the Occurrence, e.g. the species occurrence was associated with the presence of kelp such as Laminaria digitata.

Data columns recording an organism's sex, life stage, and/or behaviour should be populated with controlled vocabulary. It is recommended to include the columns in preferably both the Occurrence table and the extendedMeasurementOrFact table. The recommended vocabulary for sex can be found in [BODC vocabulary collection S10](http://vocab.nerc.ac.uk/collection/S10/current/), for lifeStage in [BODC ](http://vocab.nerc.ac.uk/collection/S11/current/)[vocabulary collection S11](http://vocab.nerc.ac.uk/collection/S10/current/), and for behavior in [ICES Behaviour collection](https://vocab.ices.dk/services/api/Code/d0268a96-afc9-436e-a4db-4f61c2b9f6ba). We will go over use of controlled vocabularies in more detail in Module 5.

occurrenceRemarks can hold additional information about the Occurrence.

recordedBy can hold a list (concatenated and separated) of names of people, groups, or organizations responsible for recording the original Occurrence. The primary collector or observer, especially one with a personal identifier (recordedByID), should be listed first.

## Record level terms

The following DwC terms are related to the Class Record level:

* basisOfRecord
* institutionCode
* collectionCode
* collectionID
* bibliographicCitation
* modified
* dataGeneralizations

basisOfRecord (required term) specifies the nature of the record, i.e. whether the occurrence record is based on a stored specimen or an observation. A full list of vocabularies with definitions for this term can be found [here](https://rs.gbif.org/vocabulary/dwc/basis_of_record_2022-02-02.xml). In case the sampled specimen is stored in a collection (e.g. at a museum, university, research institute), the options are:

* PreservedSpecimen (e.g. preserved in ethanol, tissue etc.)
* FossilSpecimen (fossil, which allows OBIS to make the distinction between the date of collection and the time period the specimen was assumed alive)
* LivingSpecimen (an intentionally kept/cultivated living specimen e.g. in an aquarium or culture collection)

In case no specimen is deposited, the basisOfRecord can be HumanObservation (e.g bird sighting, benthic sample but specimens were discarded after counting), MachineObservation (e.g. for occurrences based on automated sensors such as image recognition, etc.), or MaterialSample (e.g. physical sample was taken, and may have been preserved or destroyed). For records pertaining to genetic samples, basisOfRecord should be MaterialSample (e.g. in the DNA-derived data extension).

When the basisOfRecord is either a preservedSpecimen, LivingSpecimen, or FossilSpecimen please also add the institutionCode, collectionCode, and catalogNumber, which will enable people to visit the collection and re-examine the material. Sometimes, in the case of living specimens, a dataset can contain records pointing to the origin, the in-situ sampling position as well as a record referring to the ex-situ collection. In this case please add the event type information in eventRemarks.

institutionCode identifies the custodian institute (often by acronym), collectionCode identifies the collection or dataset within that institute. Collections cannot belong to multiple institutes, so all records within a collection should have the same institutionCode. The collectionID is an identifier for the record within the dataset or collection.

bibliographicCitation allows for providing different citations on record level, while a single citation for the entire dataset can and should be provided in the metadata. The citation at record level can have the format of a chapter in a book, where the book is the dataset citation. The record citation will have preference over the dataset citation. We do not, however, recommend that you create different citations for every record, as this will explode the number of citations and will hamper the re-use of data.

modified is the most recent date-time on which the resource was changed. It is required to use the ISO 8601:2004(E) standard, which will be discussed in Module 2 and Module 6.

dataGeneralizations refers to actions taken to make the shared data less specific or complete than in its original form. Populating this field suggests that alternative data of higher quality may (or may not) be available on request. This can be the case for occurrences of vulnerable or endangered species where their positions are converted to the center of grid cells.

## Location Terms

The following DwC terms are related to the Class Location:

* decimalLatitude
* decimalLongitude
* coordinateUncertaintyInMeters
* geodeticDatum
* footprintWKT
* minimumDepthInMeters
* maximumDepthInMeters
* locality
* waterBody
* islandGroup
* island
* country
* locationAccordingTo
* locationRemarks
* locationID

decimalLatitude and decimalLongitude (required terms) are the geographic latitude and longitude (in decimal degrees), using the spatial reference system given in geodeticDatum of the geographic center of a Location. The number of decimals should be appropriate for the level of uncertainty in coordinateUncertaintyInMeters (at least within an order of magnitude). For decimalLatitude, positive values are north of the Equator, negative values are south of it. All values lie between -90 and 90, inclusive. Regarding decimalLongitude, positive values are east of the Greenwich Meridian, and negative values are west of it. All values lie between -180 and 180, inclusive.

coordinateUncertaintyInMeters is the radius of the smallest circle around the given position containing the whole location

In OBIS, the spatial reference system to be documented in geodeticDatum is [EPSG:4326](https://epsg.io/4326). Coordinates in degrees/minutes/seconds can be converted to decimal degrees using the OBIS [coordinates tool.](https://obis.shinyapps.io/coordinates/) We also provide a [tool](https://obis.org/maptool) to check coordinates or to determine coordinates for a location (point, transect, or polygon) on a map. This tool also allows geocoding location names using [marineregions.org](http://www.marineregions.org/).

The name of the place or location can be provided in locality, and if possible linked by a locationID using a persistent ID from a gazetter, such as the MRGID from [MarineRegions](http://www.marineregions.org/gazetteer.php?p=search). If the species occurrence only contains the name of the locality, but not the exact coordinates, we recommend using a geocoding service to obtain the coordinates. [Marine Regions](http://www.marineregions.org/) has a [search interface](http://www.marineregions.org/gazetteer.php?p=search) for geographic names, and provides coordinates and often precision in meters, which can go into coordinateUncertaintyInMeters. Another option is to use the [Getty Thesaurus of Geographic Names](http://www.getty.edu/research/tools/vocabularies/tgn/) or [Google Maps](http://maps.google.com/). We will go over details on how to deal with uncertain locations in Module 6.

Additional information about the locality can also be stored in DwC terms such as waterBody, islandGroup, island and country. locationAccordingTo should provide the name of the gazetteer that is used to obtain the coordinates for the locality.

locationID is an identifier for the set of location information (e.g. station ID, or MRGID from [marineregions](http://www.marineregions.org/)), for example, the [Balearic Plain](http://www.marineregions.org/gazetteer.php?p=details&id=3956) has MRGID: [http://marineregions.org/mrgid/3956](http://marineregions.org/mrgid/3956).

A [Well-Known Text](https://en.wikipedia.org/wiki/Well-known_text) (WKT) representation of the shape of the location can be provided in footprintWKT. This is particularly useful for tracks, transects, tows, trawls, habitat extent or when an exact location is not known. WKT strings can be created using our [WKT tool](https://obis.org/maptool). This tool also calculates a midpoint and a radius, which can then be added to decimalLongitude, decimalLatitude, and coordinateUncertaintyInMeters respectively. There is also an [R tool](https://github.com/iobis/obistools) to calculate the centroid and radius for WKT polygons. [wktmap.com](https://wktmap.com/) can be used to visualize and share WKT strings.

Keep in mind while filling in minimumDepthInMeters and maximumDepthInMeters that this should be the depth at which the sample was taken and not the water column depth at that location. When filling in any depth fields (minimumDepthInMeters, maximumDepthInMeters, [minimumDistanceAboveSurfaceInMeters](http://rs.tdwg.org/dwc/terms/minimumDistanceAboveSurfaceInMeters), and [maximumDistanceAboveSurfaceInMeters](http://rs.tdwg.org/dwc/terms/maximumDistanceAboveSurfaceInMeters)), you should also consider which information is needed to fully understand the data. For a discussion and diagram of different potential scenarios and how to record depth values, please see the [Location section of the OBIS Manual](https://manual.obis.org/darwin_core.html).

## Event & Sampling Terms

The only term associated with class MaterialSample is materialSampleID. However, the following DwC terms are related to the Class Event:

* parentEventID
* eventID
* eventDate
* type
* habitat
* samplingProtocol (OBIS recommends to add sampling facts to eMoF)
* sampleSizeValue (OBIS recommends to add sampling facts to eMoF)
* SampleSizeUnit (OBIS recommends to add sampling facts to eMoF)
* samplingEffort (OBIS recommends to add sampling facts to eMoF)

eventID is an identifier for the sampling or observation event. parentEventID is an identifier for a parent event, which is composed of one or more sub-sampling (child) events (eventIDs). We will review details on how these terms can be constructed in Module 2. But know that some examples of eventIDs include: STAR_arcticsea_st3520_1989-04-04_s01, cruise1_station1_tow1.

The date and time at which an occurrence was recorded goes in eventDate. For now all you need to know is this term uses the [ISO 8601 standard](https://en.wikipedia.org/wiki/ISO_8601) and OBIS recommends using the extended ISO 8601 format with hyphens (e.g. 2021-03-21). We will cover more specific guidelines on date formatting in Module 2, as well as in Module 6.

habitat is a category or description of the habitat or ecosystem in which the Event occurred and the organism was sighted (e.g. benthos, seamount, hydrothermal vent, seagrass, rocky shore, intertidal, ship wreck etc.). This information can also be recorded in the extendedMeasurementOrFact extension table ([eMoF](http://rs.gbif.org/extension/obis/extended_measurement_or_fact.xml)) using controlled vocabulary.

Information on sampleSizeValue and sampleSizeUnit is very important when an organism quantity is specified. However, with OBIS-ENV-DATA it was felt that the eMoF extension would be better suited than the DwC Event Core to store the sampled area and/or volume because in some cases sampleSize by itself may not be detailed enough to allow interpretation of the sample. For instance, in the case of a plankton tow, the volume of water that passed through the net is relevant. In case of Niskin bottles, the volume of sieved water is more relevant than the actual volume in the bottle. In these examples, as well as generally when recording sampling effort for all protocols, eMoF enables greater flexibility to define parameters, as well as the ability to describe the entire sample and treatment protocol through multiple parameters. eMoF also allows you to standardize your terms to a controlled vocabulary.

While vocabularies can be found in the [NERC Vocabulary Server](http://vocab.nerc.ac.uk/search_nvs/), also available through [SeaDataNet](https://vocab.seadatanet.org/p01-facet-search), we will review specifics on how to find and use controlled vocabularies in Module 5.

We will now move on to Lesson 3 where we will learn how Darwin Core data columns are packaged into core and extension tables, and how these tables are then packaged together.

## Lesson 3: The Darwin-Core Archive: Core tables and Extensions

## Darwin Core Archive

Now that we understand the basics of Darwin Core terms, let's look at how to standardize the structure datasheets, and how they become packaged together.

The Darwin Core Archive (DwC-A) is the standard for packaging and publishing biodiversity data using Darwin Core terms. It is the preferred format for publishing data in OBIS and GBIF. You can read more details about the format in the [Darwin Core text guide](https://dwc.tdwg.org/text/). Simply, a Darwin Core Archive is a zipped file containing a number of text files, including data tables formatted as CSV. The data tables contain the columns with DwC terms as column names, where each table contains related information, e.g. all columns related to the occurrence, or all columns related to recording the event. We'll learn about the different data table types in this lesson.

The conceptual data model of the Darwin Core Archive is a star schema with a single core table, for example, containing occurrence records or event records at the center of the star. Extension tables can optionally be associated with the core table. Note in this star schema it is not possible to link extension tables to other extension tables (to form a so-called snowflake schema). There is a one-to-many relationship between the core and extension records, so each core record can have zero or more extension records linked to it, and each extension record must be linked to exactly one core record. Definitions for all core and extension tables can be found [here](http://rs.gbif.org/) (but note that more tables than what are currently integrated by OBIS are listed).

Besides data tables, a Darwin Core Archive also contains two XML files: one file which describes the archive and data file structure (meta.xml), and one file which contains the dataset’s metadata (eml.xml). The figure below shows a representation of this star schema.

In OBIS, there are a few data table types you can choose from that will hold your data, including:

* Occurrence core or extension
* Event core
* MeasurementOrFact or extendedMeasurementOrFact (eMoF)
* DNA Derived Data

## Darwin Core-Archive data tables

Let's briefly look at the four main data tables in an OBIS Darwin Core-Archive so we have a better understanding of how data might be separated.

### Occurrence table

Occurrence data tables are the simplest and describe the biological observations. This table can be a core table or an extension table. Each row will minimally record will the taxon name, its presence or absence, and the type of record (basisOfRecord). Where this table is the core, location information (date, coordinates, location name) can also be included.

### Event table

In the current schema, Event data tables will always be a core table, never an extension. This table is meant to provide only information about the events, i.e. the when (date, time, geologic period, etc.) and where (location, coordinates, depth, etc.) a specific sampling event occurred. It is particularly useful to describe situations where hierarchical event sampling has taken place, and when multiple samples are associated with different events (e.g. many species presence per planktonic tows at different sites).

### (Extended)MeasurementOrFact table

This table is always an extension and is meant to hold all measurement data, whether biological, abiotic, environmental, etc. It is formatted in a "long" format instead of wide, where each measurement is a new row within the columns measurementType, measurementUnit, and measurementValue. We'll learn about how to populate this table in Module 3.

### DNA derived data table

This table is always an extension, specifically an extension to the Occurrence table. In the current schema, when a dataset is based on DNA data, it will always be Occurrence core + DNA derived data extension. This extension table is meant to hold information that describes the DNA data, e.g. DNA sequences, methodology, primers, etc. More information about this table will be covered in Module 4.

Now we know more about the different data tables, but how do we know when to use which core type (Occurrence vs Event), or which extension to use? For now, know that you must have one core data table, and either zero, one, or more extensions. We will cover specifics on when to use which core in Module 2. Let's move on to an innovation by OBIS that builds on the star schema on the previous page.

## Breaking the star schema: ENV-DATA approach

We learned before that extension files cannot be linked to one another in the traditional Darwin Core Archive star schema. However, data collected as part of marine biological research often include measurements of habitat features (such as physical and chemical parameters of the environment), biotic and biometric measurements (such as body size, abundance, biomass), as well as details regarding the nature of the sampling or observation methods, equipment, and sampling effort. Linking all this information can be difficult when limited to only the star schema.

In the past, OBIS relied solely on the [Occurrence Core](http://rs.gbif.org/core/dwc_occurrence_2015-07-02.xml), and additional measurements were added in a structured format (e.g. JSON) to the Darwin Core term dynamicProperties inside the occurrence records. This approach had significant downsides: the format is difficult to construct and deconstruct, there is no standardization of terms, and attributes that are shared by multiple records (think sampling methodology) have to be repeated many times. The formatting problem can be addressed by moving measurements to a [MeasurementOrFacts](http://rs.gbif.org/extension/dwc/measurements_or_facts.xml) extension table, but that doesn’t solve the redundancy and standardization problems.

With the release and adoption of a new core type ([Event Core](http://rs.gbif.org/core/dwc_event_2015_05_29.xml)), it became possible to associate measurements with nested events (such as cruises, stations, and samples), but the restrictive star schema of Darwin Core archive prohibited associating measurements with the event records in the Event core as well as with the occurrence records in the Occurrence extension. For this reason, an extended version of the existing MeasurementOrFact extension was created.

Let's learn how this extension breaks the star schema to allow links between extensions.

## Introducing the ExtendedMeasurementOrFact Extension (eMoF)

As part of the IODE pilot project [Expanding OBIS with environmental data OBIS-ENV-DATA](https://www.iode.org/index.php?option=com_content&view=article&id=463&Itemid=100200), OBIS introduced a custom [ExtendedMeasurementOrFact](http://rs.gbif.org/extension/obis/extended_measurement_or_fact.xml) (eMoF) extension, which extends the existing [MeasurementOrFact](http://rs.gbif.org/extension/dwc/measurements_or_facts.xml) extension with 4 new terms:

* occurrenceID
* measurementTypeID
* measurementValueID
* measurementUnitID

The last three are identifier fields which can be populated with controlled vocabulary, more details on how to select vocabulary is covered in Module 5.

The occurrenceID term is used to circumvent the limitations of the star schema, and link measurement records in the eMoF extension to occurrence records in the Occurrence extension. Note that in order to comply with the Darwin Core Archive standard, these records still need to link to a specific event listed in the Event core table as well. Thanks to this term we can now store a variety of measurements and facts linked to either events or occurrences! This could include:

* organism quantifications (e.g. counts, abundance, biomass, % live cover, etc.)
* species biometrics (e.g. body length, weight, etc.)
* facts documenting a specimen (e.g. living/dead, behaviour, invasiveness, etc.)
* abiotic measurements (e.g. temperature, salinity, oxygen, sediment grain size, habitat features)
* facts documenting the sampling activity (e.g. sampling device, sampled area, sampled volume, sieve mesh size).

The diagram below demonstrates these links. There is the Event core which has information related to the different types of events, then linked to the events are facts about the sampling methods (device type, etc.) which go in the eMoF, and information regarding occurrences in the Occurrence extension. We can see that facts or measurements related to the occurrences are also placed in the eMoF, but linked to the Occurrence via occurrenceID.

This structure is based on relational databases. If you are unfamiliar or uncertain about how such database structures work, we review this concept on the next page. If you are already familiar with this, you can skip ahead to learn how these structures reduce redundancy in data that is published.

## Relational databases (optional)

If you are not familiar with relational databases, it can be difficult to understand the framework underlying OBIS. This section will help you understand relational databases and how they relate to OBIS, to the data you will format for OBIS, and to the data you may download from OBIS.

Why do we use relational databases in the first place? You are probably familiar with flat databases which contain all data in one table - this is likely how your own data are formatted. Relational databases instead consist of multiple data tables that each contain **related** information (inter-connected). When all this information is presented in one table, the table becomes larger, very complicated, and the likelihood of data duplication increases. Relational databases seek to simplify complexities and reduce redundancy by allowing information to be self-contained, but linked to each other.

You can think of a relational database as separate Excel sheets or data tables that are connected to each other by at least one term (commonly referenced as “ID”). OBIS relational databases are organized around one “core” table, whereas others are “extensions” of this core. There is always a **relationship** linking core and extension tables.

Let’s review core and extension tables and how we use them for OBIS.

OBIS core tables contain information that is applicable to **all** extension tables, and extension tables contain **more information** about the records within the Core table. Each table, whether core or extension, contains records and attributes. Each row is a record (e.g., a sampling event, a species’ occurrence), whereas each column is an attribute (e.g., a date, a measurement).

Records are linked between tables by the use of **identifiers** (e.g., **occurrenceID**or **eventID0** in OBIS). A description of measurements pertaining to a record in an Extension table will have the same identifier as the record it is describing in the Core table. By using identifiers to link records, we reduce data repetition.

In the Darwin Core format that OBIS uses, **the core table is either Event or Occurrence**, and datasets can have one, none, or more extension tables. Further explanation of data formatting in OBIS is covered in Module 2 and 3 as well as the Data Formatting section of the [**OBIS Manual**](https://manual.obis.org/).

### Relational database example

We will look at a simple relational database used by a fictional country that tracks student performance in three different courses between three schools. Rather than trying to contain information about each school, course, and student performance in one place, this information is split into three separate tables. We see that the pink table below gives us information about each school - its name, and the district it belongs to. Each school also has a schoolID, an identifier linking to the blue table where we can see student performance (course mean) in each course, the class size, and year. You will notice that the course mean and class size are bundled under columns called measurementType and measurementValue, this is to simplify all measurements into a long format as is done in the eMoF. We'll see more about this in Module 3. This  eMoF structure is integral to reducing repeated data, especially when one dataset has reoccurring information. Finally, we see that the courseID in the blue table links to the yellow one with the courseID identifier, giving us information about each course.

A fourth table could easily be created to track total school population size through time. In contrast, if this information was only presented in the pink Schools in Country table, the school information would be duplicated as you add rows for each year. In this way, you can easily see how useful relational databases are. Of course, this is a simplified example that does not implement the star schema we learned about, but it demonstrates how related tables can be linked by identifiers to reduce table complexity and data replication.

Let's continue on with how such structuring reduces redundancy in OBIS datasets.

## Reducing Redundancy

Using the OBIS data structure (ENV-DATA) allows you to avoid redundancy and data duplication within your dataset. We can limit the repetition of data by applying the ENV-DATA approach, which delineates relationships between the core table and extension tables.

For example, let us consider the dates of a ship cruise that carried out a series of bottom trawls. The sampling information (e.g., date range, equipment used, etc.) for each species collected in these trawls is the same and corresponds to the log of the sampling station where all the organisms were recorded. We know we are dealing with unique sampling events because for each new sampling station, you are going to have a totally new set of sampling information, and thus, occurrences will be organized around an Event core. So, our Event core table will contain all information related to the sampling events (e.g., date, geographical coordinates, habitat, depth, etc.), and the taxa information will be placed in the Occurrence table.

Then, information pertaining to each collected specimen/taxon (e.g., abundance, biomass, sampling methods, etc.) will be placed in an extension, another spreadsheet called the (Extended)MeasurementOrFact table. Here, measurements for each specimen/taxon and sample will occur on a separate record. All these records (rows in the spreadsheet) will be linked to a specific sampling event or occurrence by an identifier - the eventID (to link to an Event) or occurrenceID (to link to an Occurrence). If we were to put this data in one file, the fields related to date and location (e.g., eventDate, decimalLongitude, decimalLatitude, etc.) would be repeated for each species.

Let’s consider another example. If you took one sea temperature measurement where you also took your intertidal sample, each organism found in that sample would have the **same** temperature measurement. By linking such measurements to the Event table instead of in the Occurrence table, we are able to reduce the amount of data being repeated.

An advantage of structuring data this way is that if any mistakes are made, you only need to correct it once! So you can see that using relational Event structures (when applicable) in combination with extension files can really simplify and reduce the number of times data are repeated.

Caveat:** we would like to note that in some cases, data duplication may occur due to the star schema structure. For example, when publishing DNA-derived data, Occurrence core will have to be used, which necessitates the repetition of event data for each occurrence record.**

## Relevant Resources

The following resources and references can provide additional information:

* [Darwin Core Quick Reference Guide](https://dwc.tdwg.org/terms/)
* [Darwin Core text guide](https://dwc.tdwg.org/text/)
* De Pooter et al. 2017. Toward a new data standard for combined marine biological and environmental datasets - expanding OBIS beyond species occurrences. Biodiversity Data Journal 5: e10989. hdl.handle.net/10.3897/BDJ.5.e10989
* Duncan et al. (2021). A standard approach to structuring classified habitat data using the Darwin Core Extended Measurement or Fact Extension. EMODnet report. [https://www.emodnet-seabedhabitats.eu/resources/documents-and-outreach/#h3298bcd0a15741a8a0ac1c8b4576f7c5](https://www.emodnet-seabedhabitats.eu/resources/documents-and-outreach/) (note you must refine search to Technical Reports from 2021 to identify this report as it doe snot have an individual link)

## End of Module

You've completed this module! Please complete Exercise 1-1 and Quiz 1, and then proceed to the next module.