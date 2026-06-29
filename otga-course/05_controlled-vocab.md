# Module 5: Controlled vocabulary

## Introduction

In this module you will learn more about controlled vocabulary, specifically vocabulary used to populate the measurement ID fields in the extendedMeasurementOrFact table.

## Learning Outcomes

After successful completion of this module, you should be able to:

* Understand why controlled vocabularies are important
* Understand the concepts behind the structure of vocabulary codes
* Select the most appropriate measurement ID code for your data
* Be able to request creation of new codes when necessary

## How to Proceed

To succeed in this Module, you need to successfully complete the following lessons and exercises:

* Lesson 1: Introduction to controlled vocabulary
* Lesson 2: Populating `measurementValueID`, `measurementUnitID`, and `measurementTypeID`
* Lesson 3: Select P01 codes for `measurementTypeID`
* [Exercise 5-1: Find `measurementTypeIDs`](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32959)
* as well successfully complete Quiz 5 with a score of ≥80%
* [Quiz 5](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32960)

## Lesson 1: Using controlled vocabulary

We have already discussed the importance of using standards to format our data in this course. Now we will learn more about the controlled vocabularies you can use to help label the variables and measurements within your dataset.

Let’s start by understanding why such labels and vocabularies are important.

As we have learned earlier in this course, the extendedMeasurementOrFact (eMoF) terms `measurementType`, `measurementValue`, and `measurementUnit` are completely unconstrained and can be populated with free text. While free text offers the advantage of capturing complex and unclassified information, there is inevitable semantic heterogeneity (e.g., of spelling, wording, or language) that becomes a challenge for effective data interoperability and analysis.

For example, if you were interested in finding all records related to weight measurements, you would have to try to account for all the different ways “weight” was recorded by data providers (weight, wgt, Weight, wet weight, dry weight, etc.).

Try it! Use the[ OBIS Measurement Type search tool](https://mof.obis.org/) to see the diversity of `measurementTypes` that exist across published datasets in OBIS. Note that any `measurementTypeIDs` listed in this tool are solely for consultation purposes. In some cases codes may have been incorrectly chosen for the associated `measurementType`. You should always choose `measurementTypeIDs` based on your own data and the guidelines in this module.

## Important measurement IDs for OBIS

The 3 identifier terms `measurementTypeID`, `measurementValueID`, and `measurementUnitID` are used to standardize the measurement types, values and units used in the eMoF table.

These three terms should be populated using controlled vocabularies referenced using machine readable Unique Resource Identifiers (URIs). For OBIS, we recommend using the internationally recognized [NERC Vocabulary Server](http://www.bodc.ac.uk/resources/products/web_services/vocab/), developed by the British Oceanographic Data Centre (BODC). However do note that the search interface for this server does not work like Google, and you must be careful when searching with keywords. We will learn more about searching this server later in the module. The server can also be accessed through:

* SeaDataNet facet search [https://vocab.seadatanet.org/p01-facet-search](https://vocab.seadatanet.org/p01-facet-search)
  * We strongly recommend using this search interface when selecting vocabulary for `measurementTypeID`, unless you are comfortable with accessing the NERC Vocabulary Server
* NERC Vocabulary Server search [https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search)
* Semantic Model Vocabulary Builder [https://www.bodc.ac.uk/resources/vocabularies/vocabulary_builder/](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_builder/)

Controlled vocabularies are incredibly important to ensure data are interoperable - readable by both humans and machines and that the information is presented in an unambiguous manner. Vocabulary collections like NERC NVS2 compile vocabularies from different institutions, authorities, and communities (e.g., [ISO](https://termcoord.eu/terminology-iso-standards/), [ICES](https://www.ices.dk/data/vocabularies/pages/default.aspx), [EUNIS](https://eunis.eea.europa.eu/), [SeaDataNet](https://www.seadatanet.org/)), allowing you to map your data to them. In this way, you could search for a single `measurementTypeID` and obtain all related records, regardless of differences in wording or language used in the data.

Let’s learn more about the NERC Vocabulary Server (NVS).

## NERC Vocabulary Server (NVS)

Each vocabulary “term” in NVS is a concept that describes a specific idea or meaning. For consistency, we will refer to individual vocabularies in NVS as concepts. Concepts within NVS are organized into collections that group concepts with commonalities (e.g. all concepts pertaining to units).

Sometimes collections contain concepts that are deprecated. Terms can be deprecated due to duplication of concepts, or when a term becomes obsolete. You should not use any deprecated concepts for any measurement ID, however you do not need to update old datasets if a concept used has since become deprecated. Deprecated concepts can be identified from lists on NVS because their identifier will have a red warning symbol, and the page for the term itself will indicate the concept is deprecated in red lettering (see below). Deprecated concepts will also have a replacedBy concept that could be read automatically by software. Unfortunately, there is currently no notification system in place to automatically warn you if a previously used concept has become deprecated. It is good practice to occasionally confirm the concepts you or your institution use are still available for use.

One of the more important collections for OBIS is the [P01 collection](http://vocab.nerc.ac.uk/search_nvs/P01/?searchstr=&options=identifier,preflabel,altlabel,status_accepted&rbaddfilter=inc&searchstr2=), we'll talk about that next.

## P01 Collection

The [P01 ](http://vocab.nerc.ac.uk/search_nvs/P01/?searchstr=&options=identifier,preflabel,altlabel,status_accepted&rbaddfilter=inc&searchstr2=)is a large collection with >45,000 concepts. Each concept within this collection is composed of different elements that, together, construct a label you can use for a measurement type. Frequently we refer to concepts from the P01 collection as a “P01 code”. We use P01 codes to populate the `measurementTypeID` field.

A semantic model underlies each P01 code and the elements that compose them. There are 5 potential elements in this semantic model that, together, represent a specific concept to be used as a label. We will look at this model and each piece of it below.

1. **Property/attribute:**the measurement or observation of either an object of interest or a matrix, or both
2. **Object of interest:**a chemical object, a biological object, a physical phenomenon, or a material object
3. **In relation to:**how the measurement is related to the environment
4. **Environmental matrix:**what environment the measurement is in (e.g. water body, seabed); needed for most environmental measurements, but may not be necessary for e.g. biological measurements
5. **Method:**any specific methods used that are important to interpret the measurement

Not every element is required, but it is important to think about each piece of the model and how it may or may not apply to your measurement. We’ll look at this in more detail when we learn how to populate the `measurementTypeID`  field in Lessons 2 and 3.

## Lesson summary

In this lesson we learned why using measurement IDs is important, so that heterogeneities in the way data is recorded can be accounted for. We also learned about the semantic model underlying one of the bigger vocabulary collections important to OBIS, the P01 collection.

Next, let's look at how to populate `measurementUnitID`, `measurementValueID`, and the `measurementTypeID` fields in the next lesson.

# Lesson 2: Populating measurementValueID, measurementUnitID, and measurementTypeID

Selecting concepts to populate the `measurementUnitID` and `measurementValueID` fields tend to be more straightforward than populating `measurementTypeID`, so we will start this lesson with guidelines on how to select concepts for these two fields.

An important thing to remember is that when you populate any of these three measurement ID fields, the entire URI should be included. For example [http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL01/ instead of just SDBIOL01](http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL01/).

## Populating measurementUnitID

The `measurementUnitID` field is likely the easiest measurement ID field to populate. It is used to provide a URI for the unit associated with the value provided to `measurementValue` (e.g. cm, kg, kg/m2). This field should be populated with concepts from the [P06 ](https://vocab.nerc.ac.uk/search_nvs/P06/?searchstr=&options=identifier,preflabel,altlabel,status_accepted&rbaddfilter=inc&searchstr2=)collection, titled "BODC-approved data storage units". Documentation for this collection is found: [https://github.com/nvs-vocabs/P06](https://github.com/nvs-vocabs/P06).

The entire vocabulary list, including deprecated terms can be found: [http://vocab.nerc.ac.uk/collection/P06/current](http://vocab.nerc.ac.uk/collection/P06/current), but you can search for terms here: [https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/P06/](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/P06/). We strongly recommend using the second link to avoid potentially selecting deprecated terms.

Examples:

* Metres: [http://vocab.nerc.ac.uk/collection/P06/current/ULAA/](http://vocab.nerc.ac.uk/collection/P06/current/ULAA/)
* Days: [http://vocab.nerc.ac.uk/collection/P06/current/UTAA/](http://vocab.nerc.ac.uk/collection/P06/current/UTAA/)
* Metres per second: [http://vocab.nerc.ac.uk/collection/P06/current/UVAA/](http://vocab.nerc.ac.uk/collection/P06/current/UVAA/)
* Percent: [http://vocab.nerc.ac.uk/collection/P06/current/UPCT/](http://vocab.nerc.ac.uk/collection/P06/current/UPCT/)
* Milligrams per litre: [http://vocab.nerc.ac.uk/collection/P06/current/UMGL/](http://vocab.nerc.ac.uk/collection/P06/current/UMGL/)

## Populating measurementValueID

The `measurementValueID` field is used to provide an identifying code for `measurementValues` that are non-numerical (e.g. sampling related, sex or life stage designation, etc.).

Note: it is not used for standardizing numeric measurements!

Unlike `measurementUnitID`, there is more than one collection which may be used to search for and use concepts from. The collection is dependent on which type of `measurementValue` you have. We provide some common, non-exhaustive examples in the table below.

| Type of `measurementValue` | Collection | Collection Documentation | Complete Vocabulary List |
| --- | --- | --- | --- |
| Sex (gender) | [S10](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/S10/) | [https://github.com/nvs-vocabs/S10](https://github.com/nvs-vocabs/S10) | [http://vocab.nerc.ac.uk/collection/S10/current/](http://vocab.nerc.ac.uk/collection/S10/current/) |
| Lifestage | [S11](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/S11/) | [https://github.com/nvs-vocabs/S11](https://github.com/nvs-vocabs/S11) | [http://vocab.nerc.ac.uk/collection/S11/current/](http://vocab.nerc.ac.uk/collection/S11/current/) |
| Sampling instruments and sensors (SeaVoX Device Catalogue) | [L22](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/L22/) | [https://github.com/nvs-vocabs/L22](https://github.com/nvs-vocabs/L22) | [http://vocab.nerc.ac.uk/collection/L22/current](http://vocab.nerc.ac.uk/collection/L22/current) |
| Sampling instrument categories (SeaDataNet device categories) | [L05](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/L05/) | [https://github.com/nvs-vocabs/L05](https://github.com/nvs-vocabs/L05) | [http://vocab.nerc.ac.uk/collection/L05/current](http://vocab.nerc.ac.uk/collection/L05/current) |
| Vessels (ICES Platform Codes) | [C17](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/C17/) | - | [http://vocab.nerc.ac.uk/collection/C17/current](http://vocab.nerc.ac.uk/collection/C17/current) |
| European Nature Information System Level 3 Habitats | [C35](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/C35/) | - | [https://vocab.nerc.ac.uk/collection/C35/current/](https://vocab.nerc.ac.uk/collection/C35/current/) |

Behaviour** descriptions or notes can be associated with an identifier from the **[ICES Behaviour collection](https://vocab.ices.dk/?codetypeguid=d0268a96-afc9-436e-a4db-4f61c2b9f6ba)**. See video below for details.

You can also populate this field with references to papers or manuals that document the sampling protocol used to obtain the measurement. To do this you should ensure the link provided is machine readable, so we recommend using:

* The DOI of the paper/manual
* The handle for publications on IOC's [Ocean Best Practices repository](https://www.oceanbestpractices.org/repository/), (e.g. [http://hdl.handle.net/11329/304](http://hdl.handle.net/11329/304))

A video walking you through examples of how to select vocabulary for `measurementValueID` is available below and through the OBIS YouTube Vocabulary Series, playlist here: https://www.youtube.com/playlist?list=PLlgUwSvpCFS4hADB7Slf44V1KJauEU6Ul. Some of the videos on this playlist are still under review by the OBIS Vocabulary team so be aware minor updates may occur in the coming months, including the video below.

  https://www.youtube.com/watch?v=_I235UwoH-o&list=PLlgUwSvpCFS4hADB7Slf44V1KJauEU6Ul&index=6&t=5s

## Populating measurementTypeID

The `measurementTypeID` field should **only **be populated with codes from the **[P01 collection](http://vocab.nerc.ac.uk/collection/P01/current/)**.

As you learned in Lesson 1, P01 concept codes are complex in that they are based on a semantic model with several associated elements and/or sub-elements (see **[P01 wheel](https://github.com/nvs-vocabs/P01/blob/master/P01_wheel.pdf)** for example of how these elements make one concept together). Together, these elements unambiguously describe a measurement type.

It is important to remember that each element within a P01 code is meant to describe an aspect of the measurement: what is the measurement, what is the object or entity being measured, in what environment was the measurement taken, by what kind of methods, etc.? By taking together all these elements, we are able to have a unique and specific description to differentiate one measurement from another. More documentation about the P01 code and the semantic model it is based on can be found **[here](https://github.com/nvs-vocabs/P01)**.

There are several ways of searching for a P01 code, for the purposes of this course, **we will be using the [SeaDataNet P01 Facet Search](https://vocab.seadatanet.org/p01-facet-search).**You will learn how to use this search tool in Lesson 3. Keep in mind that its search function, like the NERC Vocabulary Server, works with keywords rather than free text search like we are accustomed to with Google. You may have to adjust your search approach when using the tool.

During searches for measurement types related to an occurrence, you may notice that specific taxonomic options are available to you, e.g., "abundance of Notommata". For OBIS, **all P01 codes should be generalized** - i.e. do **not** select species-specific codes. Instead, only **choose codes for “biological entities specified elsewhere”**! This is due to the Darwin Core Archive structure - taxonomic identification is already specified in the Occurrence table, but measurements are recorded in the ExtendedMeasurementOrFact table.

When you become comfortable with your understanding of P01 codes, you can also use the**[ BODC Vocabulary Builder](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_builder/)** or simply search for terms directly on the **[NERC Vocabulary Server](http://vocab.nerc.ac.uk/search_nvs/)**.

## Lesson summary

In this lesson we learned which NVS collections can be used to populate the `measurementUnitID`, `measurementValueID`, and `measurementTypeID` fields.

| Measurement ID | NERC Collection |
| --- | --- |
| `measurementUnitID` | P06 |
| `measurementValueID` | S10, S11, L22, L05, C17, C35, ICES … |
| `measurementTypeID` | P01 |

Next let’s specifically focus on how to select P01 codes for `measurementTypeID`, because this field can be the most difficult to deal with.

# Lesson 3: Selecting measurement IDs for measurementTypeID

As we have established, concepts from the P01 collection should be used to populate the `measurementTypeID` field.

Because the semantic model P01 codes are based on contains five different elements to consider, we have created a decision tree to help guide you in selecting the best P01 code to represent your data. This decision tree should be used in conjunction with the **[SeaDataNet Facet Search](https://vocab.seadatanet.org/p01-facet-search)**. The entire decision tree can be found in the **[Resources for Exercises](https://classroom.oceanteacher.org/course/view.php?id=1196&section=1)** section on the Course Overview as well as on the **[OBIS Manual](https://manual.obis.org/vocabulary.html)**, but we will look at relevant pieces step by step.

We will focus on three types of measurements, biological, chemical, and physical measurements.

## Selecting P01 codes for biological measurements: part 1

Biological measurements can include any measurement related to a biological entity, e.g. length, biomass, abundance, etc. As an example exercise, we will apply the guidelines below to the measurement “**abundance per unit area of the bed**”.

For biological measurements, after navigating to the **[SeaDataNet Facet Search](https://vocab.seadatanet.org/p01-facet-search)**, the first step is to search for and/or select “biological entity specified elsewhere”. Let’s recall from the previous lesson that we **do not **want to choose P01 codes that are taxon specific (e.g. Wet weight biomass of Hydrobia ventrosa), because taxa are already identified in the Occurrence table.

You can first use the Free Search box to search for “biological entity specified elsewhere”, and then from the Biological Entity section (you may have to scroll down), make sure you select “biological entity specified elsewhere” (pictured below). This will ensure all search results are directly relevant. For our example measurement, the measurement is not related to a sub-component of a biological entity (e.g. bell, mantle, central disc, etc.) so we will select “biological entity specified elsewhere”, not one that includes a sub-component.

## Biological measurements: part 2

Once you have selected “biological entity specified elsewhere”, the next filter we want to apply is for our measurement property. One aspect here you want to consider simultaneously is if your measurement is statistically derived - is it an average, minimum, variance, etc.? Search by keyword for your measurement in the Free Search box or select it from the Measurement Property section to apply the filter.

For our example, we will select “Abundance” from Measurement Property. Now the search result list should be considerably shorter. There are only 5 results for our example measurement on abundance.

You may be able to already find the best P01 code at this step. However, depending on the complexity of your measurement you may need to consider the matrix (or environment, e.g. water body, bed), the methodology, and the relationship between the matrix and the measurement (see below sections of the decision tree). These aspects are not as frequently relevant for biological measurements in OBIS data, but it is important to still give thought if they are applicable to your measurement type. This could apply to measurements of organisms within, for example, a certain volume of the water body. As we learned already, methods are important to include in your `measurementTypeID` if they are integral for understanding the measurement.

In our example, the measurement is “abundance per unit area of the bed”. The matrix here is the bed. When we select “bed” from Matrices, only one result is left:  **[http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL02/](http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL02/)**. This matches the description of our measurement. It is not too specific, nor too generic.

A few examples of codes that are more generic vs more complex are outlined in the table below. Note that it is possible to over filter and end up with a list of codes that do not fit your measurement. In these cases, you can remove filters to obtain a broader search result. We will look at an example of this situation when we review physical measurements.

| Generic P01 code examples | Complex P01 code examples |
| --- | --- |
| [Sex of biological entity specified elsewhere](http://vocab.nerc.ac.uk/collection/P01/current/ENTSEX01/) | [Wet weight biomass of biological entity specified elsewhere per unit volume of the water body](http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL04/) |
| [Length (fork length) of biological entity specified elsewhere](http://vocab.nerc.ac.uk/collection/P01/current/FL01XX01/) | [Cell volume of biological entity specified elsewhere by cell size measurements and computation using stereometric formulas](http://vocab.nerc.ac.uk/collection/P01/current/CELLVOLM/) |
| [Trophic status of biological entity specified elsewhere](http://vocab.nerc.ac.uk/collection/P01/current/CTROPH01/) | [Abundance of biological entity specified elsewhere per unit volume of the sediment](https://vocab.seadatanet.org/bandit/browse_step.php?&step=009BE007117_012MAT00136) |
| [Count (in assayed sample) of biological entity specified elsewhere](http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/) | [Wet weight biomass of biological entity specified elsewhere per unit length sampled of the water body](http://vocab.nerc.ac.uk/collection/P01/current/WWBIOL01/) |

If you cannot find a P01 code that represents your data, you **can request one or leave the field blank and update the dataset later**when the applicable code(s) have been created. We will go over details for how to do this at the end of this lesson. For now, we will move on to guidelines for chemical measurements.

## Chemical measurements: part 1

Identifying codes for chemical measurements starts a little differently from finding biological measurements. As an example, we will find a P01 code for the**concentration of lead within an organism**. After navigating to the **[SeaDataNet Facet Search](https://vocab.seadatanet.org/p01-facet-search)**, we will first search for the chemical that is being measured using the Free Search box: lead. Also, select the chemical from the Chemical Substance section, similar to the procedure you followed during the biological measurement search.

After this, you should select and/or search for the measurement property. In many cases for chemical measurements this will be concentration. Because concentration is available from Measurement Property, we will select it from here for our example. Otherwise we could search by keyword.

## Chemical measurements: part 2

Next, consider the matrix for your measurement. Was it taken from a water sample (water body), from within an organism (biota [biological entity specified elsewhere), or from the sediment? Here it is also important to consider whether your measurement represents a sub-sphere or sub-phase of the matrix. This would apply if you used a filter to target particulates **<**or **>**a particular size (e.g. water body [particulate 0.8-51um phase]).

Use keywords to search for your matrix, and then select the most appropriate matrix from the Matrices section to apply the filter. For our example, the concentration was within an organism - so the matrix would be biota. Immediately we notice that many of the codes listed are species-specific. We should now apply a filter to ensure we only have codes for “biological entity specified elsewhere” by adding these keywords to the Free Search. This brings the search results down from **>**300 to just one: **[http://vocab.nerc.ac.uk/collection/P01/current/PBBIOTUK/](http://vocab.nerc.ac.uk/collection/P01/current/PBBIOTUK/)** which represents our example measurement.

After specifying the matrix, the search will produce much fewer results. As the last step, consider the methods used to obtain the measurement. If applicable, either add keywords for this method to the Free Search, or manually look through the results list. Consider any applicable methods, including those used to prepare samples (e.g. filtration, acidification), analysis required to obtain the measurement (e.g. spectroscopy), or processing methods (e.g. normalization). Such methods can be integral for interpreting and comparing results. For example, the concentration of chlorophyll a can be determined by many different methods, including acetone extraction, high performance liquid chromatography, fluorometry, etc. These nuances are very important to capture in the P01 code you choose.

Unlike the guidance for the P01 codes applying to biological measurements, for chemical measurements you should choose a specific code with respect to the methods and matrix, rather than a more generic one. Note that currently there are limited P01 codes for chemical substances, so you may need to request code creation for your specific case. We will learn how to request terms (e.g. P01 codes) later in this lesson.

A brief reminder that **it is possible to over filter.** You may want to be cautious when applying specific filters, and be prepared to remove filters if search results become too narrow. We will see an example of this next, where we will look at the steps for physical measurements. These will share some similarities to those for chemical substances.

## Physical measurements: part 1

Physical measurements include, but are not limited to, abiotic measurements frequently related to measuring the environment, e.g. water temperature, salinity. For an example exercise, we will use the **depth measured with an echo sounder**as the example measurement. Like before, use the **[SeaDataNet Facet Search](https://vocab.seadatanet.org/p01-facet-search)** and search by keywords in the Free Search box first. In this example, we will search for “depth”. As a reminder, make sure to also consider whether your measurement is statistically derived and use those as keywords when applicable (e.g. mean, standard deviation, maximum, etc.). Our example measurement is not statistically derived, so we will move on.

After searching for depth, we immediately notice a variety in the names of P01 codes related to depth. There is depth (spatial coordinate), sea-floor depth, depth, etc. If we select “Depth” from the Measurement Property, our search results get narrowed down to two: “Depth maximum of biological entity specified elsewhere on the bed by epibenthic sampling” and “Depth minimum of biological entity specified elsewhere **on the bed by epibenthic sampling”. Neither of these represent our measurement! This is our first example of over filtering. Remove the “Depth” filter, and only keep the Free Search filter.

The next question to ask yourself is: what is the entity being measured? This is different from chemical measurements, because in many cases the matrix itself (i.e. water, air, sediment, biota, etc) is what is being measured. If this is the case, be sure to also consider if there are sub-groups to the matrix your measurement applies to (e.g. filtrate <2um, see **[S22 ](http://vocab.nerc.ac.uk/search_nvs/S22/?searchstr=&options=identifier,preflabel,altlabel,status_accepted&rbaddfilter=inc&searchstr2=)**and **[S24 ](http://vocab.nerc.ac.uk/search_nvs/S24/?searchstr=&options=identifier,preflabel,altlabel,status_accepted&rbaddfilter=inc&searchstr2=)** for more examples). If your measurement pertains to another physical entity, use this entity as a keyword. For a list of all potential physical entities, see the **[S29 collection](http://vocab.nerc.ac.uk/search_nvs/S29/?searchstr=&options=identifier,preflabel,altlabel,status_accepted&rbaddfilter=inc&searchstr2=)**. Note you should not use any concepts from these other collections to populate the `measurementTypeID` field, they are provided here as an example of where you can find more information about the elements that compose a P01 code.

Then consider the relationship of the physical entity to the matrix. If the entity is the matrix, skip this step. To help you in this step, think about the units of your measurement; you can consult the **[S02 collection ](http://vocab.nerc.ac.uk/search_nvs/S02/?searchstr=&options=identifier,preflabel,altlabel,status_accepted&rbaddfilter=inc&searchstr2=)**for examples of matrix-measurement relationships. Select appropriate matrices and measurement-matrix relationships from the facet search to help narrow down the results. In our example, the measurement is the depth of the water body - so the matrix itself is the entity being measured. We can select “water body” from Matrices.

## Physical measurements: part 2

Next, you can manually read through results to identify P01 codes that represent your measurement. Be sure while doing so to consider the methods applied when you generated the measurement. For example, titration methods, calibration techniques, etc. As we learned when discussing chemical measurements, including methodologies in our P01 codes makes the measurements more easily comparable and understandable.

For our example, an echo sounder was used. Let’s try using “echo sounder” as a search term. Now there are only 18 results, many of which include a correction to the measurement (e.g. Del Grosso, Kuwahara, etc.). Yet we cannot confirm if our example measurement used any correction methods, so we will not select any of those codes. This leaves us with two generic codes for echo sounders:

* [Sea-floor depth (below unspecified datum) {bathymetric depth} in the water body by echo sounder ](http://vocab.nerc.ac.uk/collection/P01/current/MBANESND/)
* [Sea-floor depth (below instantaneous sea level) {bathymetric depth} in the water body by echo sounder](http://vocab.nerc.ac.uk/collection/P01/current/MBANZZ01/)

The first of these two codes has the following definition “**The distance from the seafloor to an unspecified representation of the sea surface elevation measured by echo sounder. Sound velocity corrections may or may not have been applied.**” This implies that there was some other representation of the sea surface.

The second code does not specify a definition, however, so we must interpret it based on the title. “**below instantaneous sea level**” refers to the depth below sea level at the moment it was taken (i.e. instantaneous). This code is the most generic while still being representative of our measurement, so we will select this one. This is a good example of codes that require some level of specificity, but not too much specificity - also demonstrating the caution you must take when selecting codes to ensure they accurately represent your measurement.

But what do you do if you reach the end of this process, for any type of measurement, and cannot find a P01 code that accurately represents your measurement?

## No suitable P01 codes found

If you cannot find a suitable code for your `measurementTypeID` (or **`measurementValueID`,** `measurementUnitID` ), you can request a code to be created for you! Before doing so, make sure you have not over filtered the search results like we saw in the previous example. It can be worth removing some filters to first confirm there is no suitable code. Then to request new terms, submit a request to the **[OBIS Vocabulary GitHub repository](https://github.com/nvs-vocabs/OBISVocabs/issues)**. Requests can also be emailed to <vocab.services@bodc.ac.uk> if you have a large number of vocabulary requests, or you cannot access GitHub. However, we strongly recommend you use GitHub if you can, as it allows for a dialogue about requests as required, longer-term documentation, and can be relevant for other users who may be interested in the same type of code creation. You may also register with the **[BODC Vocabulary Builder](https://www.bodc.ac.uk/resources/vocabularies/vocabulary_builder/)** to request new terms. These requests should be based on combinations of existing concepts.

Note that you should **not**choose a vocabulary that only partially reflects your data. It is better to leave the field empty, and populate it later once a code is created than to use an incorrect code.

Finally, if you are unsure about whether a code fits your specific case, please feel free to ask questions on the aforementioned GitHUb repository, the OBIS helpdesk email (<helpdesk@obis.org>), or the Vocab channel on the **[OBIS Slack](https://join.slack.com/t/obishq/shared_invite/zt-1yiucrrrq-RZRPU7c4rm7OungiBseWVA)**.

## Lesson summary & Video Links

In this lesson you have learned how to choose `measurementTypeIDs` to describe your measurement data. We learned that, although choosing a P01 code can feel overwhelming due to the complexity of the underlying structure, breaking it down into steps helps us think about and identify the most appropriate P01 code for our situation. We also learned that if we are stuck or need to request a P01 code be created, we can do so using the OBIS Slack or Vocabulary GitHub Repository, respectively.

Practice using the guidelines in this module to select P01 codes for different measurements in Exercise 5-1. You may also watch the three videos below for more examples on how to select `measurementTypeIDs` for biological, physical, or chemical measurements. Each video provides 3 examples of varying complexity.

Biological measurements (available <https://www.youtube.com/watch?v=AfTh8UMxj7g&list=PLlgUwSvpCFS4hADB7Slf44V1KJauEU6Ul&index=2>)

Chemical measurements (available <https://www.youtube.com/watch?v=4cMM_WT9SeA&list=PLlgUwSvpCFS4hADB7Slf44V1KJauEU6Ul&index=3>)

Physical measurements (available <https://www.youtube.com/watch?v=S4-5OYpsU50&list=PLlgUwSvpCFS4hADB7Slf44V1KJauEU6Ul&index=4>)

## End of Module

You have completed Module 5! After completing Exercise 5-1 and Quiz 5, move on to Module 6. In the next module we will learn how to run some quality control checks on our dataset, as well as how to address some common issues that come up during data formatting.
