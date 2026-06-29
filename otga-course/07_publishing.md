# Module 7: Publishing datasets using the IPT

## Introduction

In this module you will learn how to publish datasets to OBIS using an Integrated Publishing Toolkit (IPT).

## Learning Outcomes

After successful completion of this module, you should be able to:

* Describe process for publishing data to OBIS
* Select the best data license for your dataset
* Locate the most appropriate IPT
* Create a resource in an IPT and publish it, download data files from IPT
* Provide metadata to publish dataset on IPT, update your dataset in the hosted IPT
* Understand responsibilities for managing an IPT
* Add DOI to your dataset, configure IPT to allow DOI assignment
* Publish a dataset with GBIF and OBIS at the same time,
* Describe difference between OBIS and GBIF, understand differences between requirements for publishing

## How to Proceed

To succeed in this Module, you need to successfully complete the following lessons and exercises:

* Lesson 1: Uploading data to the IPT
* Lesson 2: Populating IPT metadata and making the dataset public
* Lesson 3: Publishing on the IPT
* Lesson 4: Maintaining published datasets and publishing to GBIF
* [Exercise 7-1: Fill in IPT metadata](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32980)
* as well as successfully complete Quiz 7 with a score of ≥80%
* [Quiz 7](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32981)

## Lesson 1: Uploading data to the IPT

## Introduction to the IPT

Once you have finished formatting your data (Modules 2-4) and have conducted some basic quality control checks (Module 6), you are ready to publish your dataset to OBIS!

OBIS nodes can accept any data files from data providers, and these are published on their respective OBIS node Integrated Publishing Toolkit (IPT), after being formatted and checked. Data from [node IPTs](https://ipt.iobis.org/) are then harvested by central OBIS to become accessible via the global database. The Integrated Publishing Toolkit (IPT) is developed and maintained by the Global Biodiversity Information Facility (GBIF). While GBIF maintains an [IPT manual](https://www.gbif.org/ipt), we will learn specific OBIS instructions in this module.

There are a few steps involved to publish a dataset, you must:

* Identify the most relevant [IPT](https://ipt.iobis.org/) for your [OBIS node](https://obis.org/contact/) (you may have to contact your node manager to confirm IPT location)
* Login to the IPT, or have the node manager create an account for you if you do not have one, so you can upload your dataset(s)
* Map each of your fields to Darwin Core terms. This should be relatively straight forward if you have done this already during data formatting
* Fill all relevant metadata to help users understand and cite your dataset, to assure data providers and institutions are properly cited
* Publish and make your data public

We will review details for each of these steps in the subsequent pages and lessons. Let's start by learning what an IPT is and how to use it.

## Introducing the IPT

Biodiversity datasets and their metadata are published in OBIS using the Integrated Publishing Toolkit (IPT), developed by GBIF. The IPT is an open-source web application that can be customized by an OBIS node manager (if you are a node manager see OBIS manual for IPT admin details). An IPT-instance is used to publish and register all datasets. In general, the IPT software assists users in mapping data to valid Darwin Core terms, as well as archiving and compressing the Darwin Core content with:

* A descriptor file: **meta.xml** that maps the core and extensions files to Darwin Core terms, and describes how the core and extensions files are linked
* The **eml.xml** file, which contains the dataset metadata in [Ecological Metadata Language](https://en.wikipedia.org/wiki/Ecological_Metadata_Language) (EML) format. We will learn about EML and metadata later in this module.

All these components (i.e., core file, extension files, descriptor file, and metadata file) become compressed together as a .zip file, and comprise the Darwin Core Archive.

To be able to create and manage your own dataset (called a “resource” by GBIF on the IPT), you will need a user account. Once you have determined which [OBIS node IPT](https://ipt.iobis.org/) is suited for your dataset, you can contact your node manager to create an associated account for you. There will be a link on the sign-in page that will direct you to the IPT’s administrator to contact them. If your node’s IPT is not listed here, you will have to [contact the node manager](https://obis.org/contact/) to get the link to their IPT.

Next we will learn how to create a resource on the IPT.

## Creating a resource on the IPT

Once you have your account, log in at the top of the IPT page. Click on the tab Manage resources: it will display all the datasets you are managing and will be empty at first. You can create a new resource at the bottom of the page. The [GBIF IPT manual](https://github.com/gbif/ipt/wiki/IPT2ManualNotes.wiki) has more detailed instructions, but we will learn the fundamentals here.

After clicking "Create New", the first thing that needs to be completed is the shortname of your resource. This shortname uniquely identifies your resource (i.e. dataset) and will eventually show up in the URL of this resource on IPT. These shortname identifiers are also used to create folders on the IPT and **they cannot be changed**. We therefore advise that the shortname:

* is unique, descriptive and short (max. 100 characters)
* does not contain a space, comma, accents or special characters

Some good examples of shortnames include VLIZ_benthos_NorthSea_2000 and UBC_algae_specimens.

Note: When you would delete a resource, please inform your node manager of this action! If you create a test file, please include **_test** at the end of your shortname.

Then select the type of data you are uploading: Occurrence, Checklist, Sampling-Event (i.e. Event Core), Metadata only, or Other. Note that [Checklist datasets](https://www.gbif.org/data-quality-requirements-checklists) are accepted by GBIF, but not currently implemented in OBIS. However, you can still have checklist data hosted on OBIS IPTs.

You can also create an entirely new resource by uploading an existing archived resource. See the IPT manual section [Upload a DwC-A](https://github.com/gbif/ipt/wiki/IPT2ManualNotes.wiki) for instructions.

Please note the IPT has a 100MB file upload limit, however, there is no limit to the size of a Darwin Core Archive that the IPT can export/publish. Refer to the [File upload](https://github.com/gbif/ipt/wiki/IPT2ManualNotes.wiki) section in the IPT manual, to find out how to work around the file upload limit.

Once you have created your resource, you will see an empty resource overview page. We next have to upload our data.

## Uploading data to the IPT

Uploading your source file to the IPT is fortunately easy. From the Resource Overview page, under Source Data, click on Choose File. This is where you will select and add the files containing your Core table and (if applicable) extensions.

You might want to compress/zip your source file first to improve the upload speed of large files. The IPT will unzip them automatically once received. Follow the [IPT manual](https://github.com/gbif/ipt/wiki/IPT2ManualNotes.wiki) for more detailed instructions (including the option to use multiple source files or to upload via a direct database connection). Accepted formats are delimited text files (csv, tab and files using any other delimiter), either directly or compressed as zip or gzip.

Let’s follow an example dataset with an Event core. The first file we will upload will be a .csv file containing the Event core table. After it’s been selected, we click “Add”.

A source file detail page will then be shown, displaying how the IPT has interpreted our file (number of columns, rows, header rows, character encoding, delimiters, etc.). Here you can provide information about how the data table is encoded, how many header rows exist, the type of delimiters, and what type of character encoding you used. You may need to double-check the [character encoding](https://en.wikipedia.org/wiki/Character_encoding) for your file, especially if you used any special characters (e.g., in species or place names). **UTF-8** is one of the most common encoding standards, and you can select this encoding when saving files, depending on the software used:

* Windows: MS Excel: select Save as. From the drop-down select the “CSV UTF-8 (Comma delimited)” option
* Windows: Notepad: Click on File, then Save as
  * In the drop-down menu “Save as type” drop-down, select All Files
  * In the “Encoding” drop-down on the bottom right, select UTF-8.
  * Be sure to name your file using the .csv extension (e.g., data.csv).
* Mac: Numbers: Select File then Export to... > CSV.
  * Click Advanced options
  * Click the drop-down menu next to Text Encoding and select Unicode (UTF-8)
  * Click Next, then finally name your file, select a location, and click Save

Once you have specified all the above information, click the preview button to verify everything is correct, click anywhere on the screen to exit the preview, then click save. This will redirect you back to the Overview page for your dataset. You can add multiple files for each of your data tables by clicking the three vertical dots to the right and “+ Add”. We will add a .csv file for an Occurrence extension, as well as one for the eMoF extension.

Next we will map our data fields to Darwin Core in the IPT.

## Mapping data columns to Darwin Core on the IPT

Because biodiversity data are published in the [Darwin Core](https://manual.obis.org/darwincore) standard, we must map our data fields to the Darwin Core (DwC) standards and vocabulary. As we have mentioned earlier in this course, the DwC standard includes a list of defined terms and allows your data to be understood and used by others. It also allows an aggregator like OBIS or GBIF to digitally integrate your data with other datasets. DwC mapping is the process of linking the fields in your resource file with the appropriate Darwin Core terms. We learned that can be one of the most challenging steps in publishing data for two reasons:

* The list of DwC terms can be overwhelming, so it might be difficult to select the ones that are appropriate for your dataset
* The IPT currently only allows one-to-one mapping of fields, so the ease of mapping will depend on your database structure and on the feasibility of exporting as close to DwC standards as possible. You can contact your node manager or the OBIS secretariat at [info@iobis.org](mailto:info@iobis.org) to help guide you through the steps, review your mapping, suggest terms etc. You are also welcome to post questions in the [OBIS Slack](https://join.slack.com/t/obishq/shared_invite/zt-1nrokd987-aSah0CfXb5MoY10t1olnhQ).

To add the DwC mappings, click the three vertical dots to the right of this section and select “+ Add”.

A popup window will allow you to select your Core type to facilitate mapping to Occurrence or Event core. In this example, we will select Darwin Core Event for the Event core table. After confirming which table you'd like to map to, press Save.

This will bring you to the DwC mapping page. If you have already named several of your fields with DwC terms, they will have been auto-mapped. It is important to review each DwC class (Record, Event, Location, etc.) and ensure any mapped and unmapped fields are mapped to the correct DwC term. When you select a term, a drop-down menu will appear where you can select the appropriate field from your dataset. It is good practice to double-check that the auto-mapped fields are mapped correctly.

Once you are done mapping, any unmapped fields will appear at the bottom of the page for you to check. If there is no DwC term to map these terms to, that is okay, but the data will not be published alongside the rest of your dataset. Consider moving these unmapped fields to either **dynamicProperties** or to one of the extensions (e.g., eMoF), whichever is most applicable. Such data would still be available if the dataset was downloaded directly from the IPT.

Finally, click Save. You can return to the Overview page by clicking Back. To add DwC mappings for the other files (Occurrence and eMoF), click the same “+ Add” button and go through the same process for each extension table you have.

The IPT may identify Redundant terms if certain terms appear in the e.g., Event core and Occurrence extension. If your Occurrence extension (or core) contains information about **individualCount** and **organismQuantity**, you can map such fields in both the Occurrence and the eMoF as a **measurementType**.

Let's watch a video of the publishing process so far.

## Watch a video

Watch this video for a walkthrough of all steps to upload source data to an IPT, and how to map terms to Darwin Core (available <https://youtu.be/i2P8mjo128o>).

Play Video

In this lesson we learned what an IPT is, how to use it to upload data tables, and map our fields to Darwin Core.

Next we will learn how to fill metadata information so that our dataset can be understood by other users.

## Lesson 2: Populating IPT metadata and making the dataset public

## Metadata on the IPT

Metadata enables users to discover, assess, understand, and attribute your dataset for their particular needs, so it is beneficial to invest some time providing accurate and complete information here. Think about what kind of information you would want to know about a dataset if you were looking at one, or looking for datasets that fulfill a particular criteria, then apply the same logic to provide such information for your dataset. The more complete the metadata description, the better your dataset will be understood by multiple users.

To populate metadata fields, navigate to the IPT resource overview page on the IPT, then to Metadata and click Edit to open the metadata editor. Any information you provide here will be visible on the resource homepage and bundled together with your data when you publish.

Let's start by watching a video showing how to navigate and populate metadata fields. The video below (available <https://youtu.be/oAoQsDkZpS8>) provides a demonstration and discussion for filling in metadata on the IPT:

Play Video

We mentioned earlier in this course that OBIS adheres to the Ecological Metadata Language, let's learn about this standard and how it applies to your metadata next.

## Ecological Metadata Language

As we have learned, OBIS (and GBIF) uses the [Ecological Metadata Language (EML)](https://eml.ecoinformatics.org/) as its metadata standard. This standard was developed specifically for the earth, environmental, and ecological sciences. EML is implemented as [XML (eXtensible Markup Language)](https://en.wikipedia.org/wiki/XML) and is based on prior work done by the Ecological Society of America and associated efforts. OBIS specifically uses the [GBIF EML profile (version 1.1)](http://rs.gbif.org/schema/eml-gbif-profile/1.1/eml-gbif-profile.xsd).

To populate metadata on an IPT, you can either fill in the fields directly through the metadata editor, as demonstrated in the previous video, or you can write and upload a script in EML using R or python (e.g., using the R [eml](https://github.com/ropensci/eml) package). Regardless of which option you choose, it is good practice for your metadata to cover the following types of information:

* Geographic coverage
* Taxonomic coverage
* Temporal coverage
* Methodologies

For OBIS, the following **4 terms are the bare minimum required**: **Title**, **Citation**, **Contact** and **Abstract**. In addition to the above, there are several other categories for metadata you can provide, which includes basic information about the:

* Dataset and data provider
* Keywords
* Hosting institution information
* Information regarding associated project(s)
* Sampling methods
* How to cite the dataset
* Museum collection (if applicable)
* Other external links (e.g. a homepage) or additional metadata

Next let's look at some specific EML terms.

## EML terms

Below is an overview of all the EML terms used to describe datasets, as well as the associated metadata category that is found on the IPT. You may use this table as a reference. Keep in mind the IPT usually has pop up definitions for the various terms during metadata documentation.

| **EML term** | **IPT metadata category(s)** | **Description** |
| --- | --- | --- |
| title [xml:lang="..."] | Basic Metadata | A good descriptive title to provide the user with valuable information, making data discovery easier. Multiple titles may be provided, particularly when trying to express the title in more than one language (use the “xml:lang” attribute to indicate the language if not English/en). |
| creator ; metadataProvider ; associatedParty ; contact | Basic Metadata; Associated Parties | These are the people and organizations responsible for the dataset resource, either as the creator, the metadata provider, contact person or any other association. The following details can be provided: individualName givenName surName organizationName: Name of the institution. positionName: to be used as alternative to persons names (leave individualName blank and use positionName instead e.g. data manager). address deliveryPoint city administrativeArea postalCode country phone electronicMailAddress onlineUrl: personal website role: used with associatedParty to indicate the role of the associated person or organization. userID: e.g. ORCID. directory |
| pubDate | | The date that the resource was published. Use ISO 8601. |
| language | Basic Metadata | The language in which the resource (not the metadata document) is written. Use ISO language code. |
| abstract | Basic Metadata | Brief description of the data resource. |
| keywordSet | Keywords | Includes the following: keyword: Note only one keyword per keyword field is allowed. keywordThesaurus : e.g. ASFA |
| additionalInfo | | OBIS checks this EML field for harvesting. It should contain marine, harvested by iOBIS. |
| coverage | | Includes geographic, temporal, and taxonomic coverage |
| | Geographic Coverage | geographicDescription: a short text description of the area. E.g. the river mounth of the Scheldt Estuary. boundingCoordinates westBoundingCoordinate eastBoundingCoordinate northBoundingCoordinate southBoundingCoordinate |
| | Temporal Coverage | Use ISO 8601 singleDateTime rangeOfDates beginDate calendarDate endDate calendarDate |
| | Taxonomic Coverage | Taxonomic information about the dataset, can include a species list. generalTaxonomicCoverage taxonomicClassification taxonRankName taxonRankValue commonName |
| intellectualRights | Basic Metadata | Statement about IPR, Copyright or various Property Rights. Also read the [guidelines on the sharing and use of data in OBIS](https://manual.obis.org/policy.html) |
| purpose | Additional Metadata | A description of the purpose of this dataset |
| methods | Sampling Methods | Includes: methodStep: Descriptions of procedures, relevant literature, software, instrumentation, source data, and any quality control measures taken sampling: Description of sampling procedures including geographic, temporal and taxonomic coverage of the study. studyExtent: Description of the specific sampling area, the sampling frequency (temporal boundaries, frequency of occurrence), and groups of living organisms sampled (taxonomic coverage) samplingDescription: Description of sampling procedures, similar to the one in the methods section of a journal article qualityControl: Description of actions taken to either control/ assess data quality resulting from the associated method step |
| project | Project Data | title identifier personnel: The personnel field is used to document people involved in a research project by providing contact information and their role in the project description funding: The funding field is used to provide information about funding sources for the project such as: grant and contract numbers; names and addresses of funding sources. studyAreaDescription designDescription: The description of research design |
| maintenance | Additional Metadata | description maintenanceUpdateFrequency |
| additionalMetadata | Basic Metadata, Citations, Collection Data | metadata dateStamp: The dateTime the metadata document was created or modified (ISO 8601) metadataLanguage: The language in which the metadata document is written (not the resource being described) hierarchyLevel citation: A single citation for use when citing the dataset. The IPT can also auto-generate a citation based on the metadata (people, title, organization, onlineURL, DOI etc). bibliography: A list of citations that form a bibliography on literature related / used in the dataset resourceLogoUrl: URL of the logo associated with a dataset. parentCollectionIdentifier collectionIdentifier formationPeriod: Text description of the time period during which the collection was assembled. E.g., “Victorian”, or “1922 - 1932”, or “c. 1750”. livingTimePeriod: Time period during which biological material was alive (for palaeontological collections). specimenPreservationMethod physical objectName characterEncoding dataFormat externallyDefinedFormat formatName distribution: URL links online url function="download" url function="information" |
| alternateIdentifier | | A Universally Unique Identifier (UUID) for the EML document and not for the dataset. This term is optional. |

## Lesson summary

In this lesson we learned about Ecological Metadata Language, and how to fill metadata information on the IPT. Now that your dataset is uploaded, properly mapped to DwC, and all the metadata is filled in, the data can be published and made public on OBIS! We'll look at that in the next lesson.

## Lesson 3: Publishing on the IPT

## Publish on the IPT

To publish a dataset, make sure you have navigated to the resource overview page, then go to the Publication section, click the vertical dots, and select Publish.

The IPT will now combine the data with the metadata to package it as a standardized zip-file called a “Darwin Core Archive”.

Note: **Hitting the “publish” button does not mean that your dataset is available to everyone!** It is still private, with access limited to the resource managers. It will only be publicly available when you have changed Visibility to Public. You can choose to do this immediately or on a set date.

Back on the Resource Overview page > Published Release, you can see the details of your first published dataset, including the publication date and the version number. Since your dataset is published privately, the only thing left to do is to click Visibility to Public to make it available to everyone. **Warning**: **please do not do this with a test dataset**.

It is now listed on the IPT homepage and you can share and link to it, e.g. [http://ipt.vliz.be/resource.do?r=kielbay70](http://ipt.vliz.be/resource.do?r=kielbay70). This is a good time to notify any regional or thematic network you are involved in, which may be interested in your dataset.

## Note about dataset versioning

Your published dataset is a static snapshot of your data and **will not change** until you upload an updated source file and click publish again or publish a new version (**do ***not***create a new resource**). This procedure has the advantage that your dataset is always available, does not require a live internet connection to your database and can be easily shared. It also allows you to control the publication process more precisely: version 1, version 2, etc. and users are informed of how recent the data are via the last publication date. We'll discuss maintenance of datasets in the next lesson.

To view an older version of the metadata about the resource on the IPT, just add the trailing parameter &v=n to the URL where **v** stands for “version”, and **n** gets replaced by the version number, e.g., [http://ipt.vliz.be/ilvo/resource.do?r=zoopl_bpns**&**v=1](http://ipt.vliz.be/ilvo/resource.do?r=zoopl_bpns&v=1). In this way, specific versions of a resource’s EML, RTF, and DwC-A files can be retrieved. Please note, the IPT’s Archival Mode must be turned on in order for old versions of DwC-A to be stored (see [Configure IPT settings](https://ipt.gbif.org/manual/en/ipt/latest/administration) section of the IPT manual).

## Watch a video

For an overview of how to publish on the IPT, watch the first minute of the video below (available at <https://youtu.be/HciufRG9hiI>). This video also contains information on how to publish to GBIF simultaneously, a topic that we will discuss in the next lesson.

Play Video

## Data Licenses

Finally, when publishing you must choose a data license for any data made available to OBIS under one of the following Creative Commons licenses (in order of preference):

* [CC0](https://creativecommons.org/publicdomain/zero/1.0/) - data may be used without restrictions
* [CC-BY](https://creativecommons.org/licenses/by/4.0/) - data are available for any use if proper attribution and credit is given
* [CC-BY-NC](https://creativecommons.org/licenses/by-nc/4.0/) - data may be used for any non-commercial use as long as proper attribution/credit is given

You may need to consult with your organization if there are any copyright concerns. For more information on the different Creative Commons license types see [About the licenses](https://creativecommons.org/licenses/).

## Lesson 4: Maintaining published datasets and publishing to GBIF

## Maintaining resources on the IPT

Once your dataset is published and made public, you can share it and update it as often as you need to. We'll first go over how to assign a DOI to your dataset to make sharing easier, then discuss other ways of sharing and maintenance.

### Adding a DOI to datasets

DOIs are important for digitally tracking your dataset. Fortunately you can easily reserve a DOI for your dataset if the IPT administrator has configured the IPT accordingly. The IPT administrator must enable the capacity for users to reserve DOIs. To do this they first need a [DataCite account](https://datacite.org/) associated with an Organization. Only one DataCite account can be used to register DOIs in this manner (i.e. IPT users do *not* need an account). The IPT’s archival mode, configurable on the IPT settings page, must also be turned on to enable this feature (note that enabling this mode will use more disk space). For more information see the [IPT administration manual](https://ipt.gbif.org/manual/en/ipt/latest/administration).

Once this has been configured, you can easily reserve a DOI for a dataset by navigating to the Manage Resources tab, then select the dataset for which you wish to reserve a DOI. On the overview page for the dataset, scroll to the Publication section, click the three vertical dots and select “Reserve DOI”.

Let's next review how to update your dataset with a new version.

## Updating datasets

If, after publishing, your dataset has changed - extra information was added, new fields were mapped to Darwin Core, you found a more representative **measurementTypeID** code - whatever the reason, you would like to update your file. How do we do this? Fortunately, the process is largely the same as publishing your first version which we learned how to do in the previous lesson. Follow the steps below:

* Log in to the IPT where your dataset is hosted
* Under the Manage Resources tab, locate your dataset
* Upload new source files to overwrite the previous ones
* Complete the Darwin Core mapping, and update any metadata that may have changed

In the Publication section, click Publish

The new version will be automatically updated! The versioning may change from e.g. v1 to v1.2, or v2, etc. depending on if major changes were made, or only minor ones. There will be a pop up requesting you to confirm and describe the updates:

Note: A new DOI will be generated only if you generate it yourself on the new version. Deciding when to generate a new DOI is up to you, but generally you should generate a new DOI when there have been major changes to your dataset, such as significant changes in your metadata or a move to a 2.0 resource version within the IPT.

What if you want to make your dataset accessible to OBIS *and* GBIF? Let's learn how to do that on the next page.

## Publishing to both OBIS and GBIF

Your dataset will only be harvested and published to GBIF when you change the Registration to Registered. Keep in mind this step is not needed for OBIS to harvest your datasets. However, please do **not** register your dataset with GBIF if your dataset is already published in GBIF by another publisher.

Another thing to keep in mind is that The IPT itself must be registered with GBIF in order to publish to GBIF. The node manager can do this (see OBIS manual for IPT admin guidelines).

## Differences between OBIS and GBIF

There are a few differences between OBIS and GBIF you should be aware of. Perhaps the most obvious difference is that OBIS focuses on marine data, whereas GBIF includes broader biodiversity data. However, there are differences between quality control requirements for published datasets. A complete list of GBIF’s quality control checks can be found [here](https://data-blog.gbif.org/post/issues-and-flags/), and a guide on GBIF’s publishing process is [here](https://www.gbif.org/publishing-data).

OBIS currently accepts two core data table types: Occurrence core and Event core. GBIF includes both [Occurrence](https://www.gbif.org/data-quality-requirements-occurrences) and [Event](https://www.gbif.org/data-quality-requirements-sampling-events) core, with one additional core type, [Checklists](https://www.gbif.org/data-quality-requirements-checklists). GBIF is also developing a [new data model](https://www.gbif.org/composition/HjlTr705BctcnaZkcjRJq/data-model-principal-composition) to expand data publishing capabilities.

Some of the other main differences in how OBIS and GBIF structure and publish datasets to be aware of include:

* OBIS uses [WoRMS](https://www.marinespecies.org/index.php) as the exclusive taxonomic backbone, whereas GBIF uses *[*Catalog of Life*](https://www.catalogueoflife.org/)
* The OBIS-ENV-DATA structure, the eMoF extension, and the DNA Derived data extensions are not currently included in GBIF downloads (e.g., [this dataset description](https://www.gbif.org/dataset/be77e203-486c-4651-91b9-8347968b728c)). This data can still be published alongside your dataset, and is available when it is downloaded from the Source archive, but it will not be included in a GBIF Annotated Archive download
* OBIS conducts some QC procedures that GBIF does not, including:
  * Checking validity of depth measurements
  * Checking validity of WoRMS LSID
  * Identifying if taxa are exclusively freshwater or terrestrial
* GBIF includes most of the same [data standards](https://www.gbif.org/standards) as OBIS (Darwin Core, EML), however GBIF also follows the *[*Biological Collection Access Service (BioCASE/ABCD)*](https://www.tdwg.org/standards/abcd/)

See below for a quick reference on which terms are required or recommended in OBIS and GBIF for Occurrence and Event tables.

### Event Table

| Term | Status in OBIS | Status in GBIF |
| --- | --- | --- |
| eventID | required | required |
| eventDate | required | required |
| decimalLatitude & decimalLongitude | required | strongly recommended |
| samplingProtocol | strongly recommended | required |
| samplingSizeValue & samplingSizeUnit | strongly recommended | strongly recommended |
| countryCode | strongly recommended | strongly recommended |
| parentEventID | strongly recommended | strongly recommended |
| samplingEffort | strongly recommended | strongly recommended |
| locationID | strongly recommended | strongly recommended |
| coordinateUncertaintyInMeters | strongly recommended | strongly recommended |
| geodeticDatum | recommended | strongly recommended |
| footprintWKT | recommended | strongly recommended |
| occurrenceStatus | required in occurrence extension | strongly recommended |

Occurrence table

| Term | Status in OBIS | Status in GBIF |
| --- | --- | --- |
| occurrenceID | required | required |
| eventDate | required | required |
| scientificName | required | required |
| basisOfRecord | required | required |
| kingdom | recommended | required |
| decimalLatitude & decimalLongitude | required | strongly recommended |
| scientificNameID | required | not required, accepted |
| occurrenceStatus | required | not required, accepted |
| taxonRank | strongly recommended | strongly recommended |
| coordinateUncertaintyInMeters | strongly recommended | strongly recommended |
| individualCount, organismQuantity & organismQuantityType | strongly recommended | strongly recommended |
| geodeticDatum | recommended | strongly recommended |
| eventTime | recommended | not required, accepted |
| countryCode | not required, accepted | strongly recommended |
| informationWithheld | not required, accepted | not required, accepted |
| dataGeneralizations | not required, accepted | not required, accepted |
| country | not required, accepted | not required, accepted |

## Watch a video

This video (available <https://youtu.be/HciufRG9hiI>) will walk you through the process of publishing to GBIF as well as OBIS, how to become a GBIF publisher, and how to publish data hosted on a GBIF IPT to OBIS. It starts at 1:07.

Play Video

In this lesson we learned how to add DOIs to our datasets, how to update a dataset with a new file and version, and how to register our dataset with GBIF.

## End of Module

You have completed Module 7! To help you think about how metadata is recorded, complete Exercise 7-1

In the next module we come to the end of the OBIS data life cycle and learn how to access data from OBIS.
