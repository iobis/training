# Module 3: Formatting data tables

## Introduction

In this module, you will learn how to format the core and extension data tables so that they adhere to Darwin Core standards. This will include how to map data field names to Darwin Core terms.

## Learning Outcomes

After successful completion of this module, you should be able to:

- Map (or rename) original data field names to Darwin Core terms
- Identify which data goes into Occurrence, Event, and extendedMeasurementOrFact tables
- Restructure original data to follow DwC standards by creating Core and extension tables

## How to Proceed

To succeed in this Module, you need to successfully complete the following lessons and exercises:

- Lesson 1: Mapping DwC terms
- Lesson 2: How to format Event table
- Lesson 3: How to format Occurrence table
- Lesson 4: How to format extendedMeasurementOrFact table
- [*Exercise 3-1: Create the Event core and Occurrence extension*](https://classroom.oceanteacher.org/mod/assign/view.php?id=32935)
- [*Exercise 3-2: Create the eMoF extension table*](https://classroom.oceanteacher.org/mod/assign/view.php?id=32936)
- Successfully complete Quiz 3 with a score of ≥80%
- [*Quiz 3*](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32937)

## Lesson 1: Mapping DwC terms

## Mapping terms to Darwin Core

Throughout this course thus far we have learned about Darwin Core terms and the importance of using standardized terminology. There are many possible ways to set up a datasheet, and if you are new to OBIS you likely did not use controlled Darwin Core (DwC) terms or vocabulary before samples were collected. Thus you will likely have to map your data fields to DwC. We recommend documenting your choices in a Documentation sheet so you have a reference to go back to should the need arise. Such a document should include notes on the choices you made, as well as any actions you had to take (e.g. separate one column into many, convert dates or coordinates, etc.). You should also **always save one copy of the original dataset** before any changes were made.

Mapping as many of your data fields to DwC before creating the core and extension tables will help make this process easier, so we will look at some examples of commonly used data field names and the recommended DwC term(s) you can map to.

## Darwin Core mapping examples

The following table provides non-exhaustive examples of original data field names and their associated DwC term. For a complete list of potential DwC terms, see the *[*Darwin Core Quick Reference Guide*](https://dwc.tdwg.org/terms/)*. If you ever have difficulty in mapping your data fields to a Darwin Core term, please do not hesitate to reach out to the OBIS helpdesk@obis.org or ask questions on the OBIS Slack (link in Course Resources).

| Original term(s) | DwC term(s) |
| --- | --- |
| Date, time | eventDate |
| Species, g_s, taxa | scientificName |
| Any biotic/abiotic measurements | measurementType, measurementValue, measurementUnit (reviewed later in this module) |
| Depth | maximumDepthInMeters and/or minimumDepthInMeters |
| Lat/Latitude, Lon/Long/Longitude, dd, coordinates | decimalLatitude, decimalLongitude |
| Location | locality |
| Presence, absence | occurrenceStatus |
| Type of record/specimen | basisofRecord |
| Person/ people that recorded the original Occurrence | recordedBy |
| ORCID of person/ people that recorded the original Occurrence | recordedByID |
| Person/ people that identified the organism | identifiedBy |
| ORCID of person/ people that identified the organism | identifiedByID |
| Data collector, data creator | recordedBy |
| Taxonomist, identifier | identifiedBy |
| Record number, sample number, observation number | occurrenceID |

## Including data as verbatim

Very often it is helpful to keep data as it was originally documented in "verbatim" DwC fields. This ensures anyone looking at your dataset will understand how the original data was interpreted. For example, if coordinates were originally recorded in degrees, minutes, seconds, it is good practice to keep this original field and rename it to **verbatimLatitude**/**verbtaimLongitude**/**verbatimCoordinates**, as applicable.

There are a number of other verbatim fields you may consider including:

- [verbatimEventDate](https://dwc.tdwg.org/terms/)
- [verbatimLocality](https://dwc.tdwg.org/terms/)
- [verbatimDepth](https://dwc.tdwg.org/terms/)
- [verbatimElevation](https://dwc.tdwg.org/terms/)
- [verbatimIdentification](https://dwc.tdwg.org/terms/)
- [verbatimLabel](https://dwc.tdwg.org/terms/)

Note that you do not have to map every field in your original data to DwC for publishing. For example, a column for notes taken in the field or locations of specimens within storage facilities might not be relevant for publishing, and can be excluded from this mapping process.

## Lesson Summary

In this lesson we have learned how to map (or rename) original data fields to DwC terms.

Now that we know how to create identifiers, how to standardize data, and how to map our fields to DwC, let's learn how to format a data table, starting with the Event core.

## Lesson 2: How to format Event table

## Formatting Event Tables

If your dataset uses an Event core structure (see Module 2), follow this process to format the data table:

- Create eventIDs for each record.
- Add and fill the parentEventID and eventRemarks fields as applicable.
- Identify the hierarchical event structure in your data, if present, and create new records for parent events.
- Add and fill any other relevant fields.
- Identify all columns in your data that will match with Darwin Core Event fields.
- Include any relevant abiotic measurements (ENV-DATA) related to sampling events (e.g. sampling protocols). We will add these to the eMoF table later.
- Copy these columns to a new sheet and name it Event.
- Delete duplicate data so only unique events are left.
- Ensure dates, times, and location information is standardized (as discussed in Module 2).
- Map any remaining fields to Darwin Core.

We will refer to using Microsoft Excel to reorganize data tables, but you can use other spreadsheet softwares or use R/Python. It helps to format table cells to “text only” in Excel to avoid unwanted reconfiguration and changes in the original data by the software.

Let's watch a video demonstrating the steps above before taking a look at each in detail.

Play Video

- (Available at https://youtu.be/jyy6QO_p7v8)

After watching the video, let's review each of the steps before moving on to the next lesson where we will learn how to format Occurrence tables.

## Steps 1-4

## Step 1: Create eventIDs

Your original dataset likely contains many fields, some may be related to Event data and some will be related to Occurrence records. Before moving any fields or adding information, we must first ensure unique **eventIDs** are attached to each record. Create a copy of your original dataset (note: you should **never** make changes to your original data sheet! Keep it intact for later reference), and add a new, blank column named " **eventID**". Follow guidelines from Module 2 to **create unique** **eventIDs** **for records belonging to different sampling events if you do not already have an identifier for them. If you already have unique identifiers for each record, copy these identifiers into the eventID column.

As a reminder, all records belonging to the same sampling event will have the same **eventID**. It is okay to have records with the same **eventID **at this step.

## Step 2-3: Identify event hierarchy, create parentEventID and eventRemarks fields

Now that unique identifiers have been created, **identify the hierarchy levels** in your events. What types of events are nested in each other? Which event type would be at the highest level, the lowest? You might not yet have records for events higher in the hierarchy (i.e. parent events). This is okay, we will create them. It is easier if you recall the rationale from your sampling design to build the events and parent events. If you were not the one that developed the study, you may find this information in related documentation, such as in the manuscript, project report etc.

Add two new columns named **parentEventID** and [eventType](https://dwc.tdwg.org/terms/). For each of your records, indicate what type of event occurred - a subsample, sample, a site or station visit, a cruise, a deployment, etc. Write this event type in the **eventType** field and try to use suggested controlled vocabulary. You may also include event information in **eventRemarks**.

Now that you have identified your event hierarchy structure, we need to create records for the events at the higher hierarchy levels, the parent events. Add new records for each parent event as applicable. The other data fields (**eventDate**, **decimalLongitude**, etc.) can either be left blank or filled in as relevant. You should provide the most relevant information for each sampling event. Information provided to parent events will be passed to child events only when the child event fields are blank.

For example, coordinates do not need to be repeated in a parent event if the samples have more specific coordinates, but you are welcome to also provide generalized coordinates for parent events if available. However, if many samples were taken at one location, then a parent event (e.g. station) may contain the coordinates for all child events, thus leaving the sample event records with blank coordinate fields. However do note that it is okay to populate parent *and* child events with the same information particularly if the dataset will be harvested by GBIF, because GBIF does not currently pass parent event information to child events.

## Step 4: Identify relevant Event fields

Now that all records have attached **eventIDs **&** parentEventIDs**, let's ensure only our Event table only contains event-specific data. As a reminder, an Event table is required to include the following terms:

- eventDate
- eventID
- parentEventID (if applicable)
- decimalLatitude
- decimalLongitude

Other terms you should consider adding are grouped by their associated Darwin Core class below:

- Class Event | DwC:eventRemarks
- Class Event | DwC:eventType
- Class Event | DwC:year
- Class Event | DwC:month
- Class Event | DwC:day
- Class Event | DwC:type
- Class Location | DwC:country
- Class Location | DwC:island
- Class Location | DwC:coordinateUncertaintyInMeters
- Class Location | DwC:countryCode
- Class Location | DwC:footprintWKT
- Class Location | DwC:geodeticDatum
- Class Location | DwC:islandGroup
- Class Location | DwC:locality
- Class Location | DwC:locationAccordingTo
- Class Location | DwC:locationID
- Class Location | DwC:locationRemarks
- Class Location | DwC:maximumDepthInMeters
- Class Location | DwC:minimumDepthInMeters
- Class Location | DwC:stateProvince
- Class Location | DwC:verbatimCoordinates
- Class Location | DwC:verbatimDepth
- Class Location | DwC:waterBody

Look for and identify which of your data columns match or relate to any of these terms and then move to step 5. These terms (or columns) are not required to upload your data into OBIS, however, consider including them if you have the information.

## Steps 5-8

## Step 5: Copy relevant fields

Create a new, blank spreadsheet and rename it to "Event". You will then have 2-3 sheets in your spreadsheet file: the original data untouched (or saved in a separate file altogether), the working copy of the original datasheet that we just added eventIDs to, and a blank sheet called Event. Next, select all the previously identified relevant Event fields from the working sheet and copy them into the Event datasheet. For now, please include any fields related to measurements regarding your events (biotic measurements on observations will be handled in the next lesson on Occurrence data tables). This may include sampling protocols, sampling gear, temperature, salinity, etc. Be sure that the measurement is associated with the event, and not an occurrence (e.g. some temperature data or other abiotic measurements might be associated with an occurrence rather than an event). Measurement (meta)data will be organized separately after the Event and Occurrence tables are completed, but it will be easier to associate them with the proper eventID by doing this step.

## Step 6: Delete duplicate data

Use Microsoft Excel's Remove Duplicates function from the Data tab on the ribbon to ensure there are no repeated eventIDs in this table and only unique events are left.

## Step 7: Fill in other relevant fields and standardize information

Fill in any other event fields with relevant information. Then confirm that dates, location information, etc. all match with data standards (ISO 8601 for dates) as we learned in Module 2.

## Step 8: Map unmapped terms to Darwin Core

You must rename your columns to match with Darwin Core terms. As mentioned previously, we recommend keeping notes on this process by creating a separate sheet and documenting the original field name, the Darwin Core name you mapped it to, and your reasons why. Call this the "**Documentation sheet**" and make use of it as you proceed with the data formatting process. It is very important to list and describe all the modifications carried out along with formatting and curating your data tables. It will help you track not only the quality of data but also date back if any procedure was mistaken.

Keep in mind you may need to split one column into two or more to match with Darwin Core terms, or combine multiple columns into one. See below for an example of the Documentation sheet for Event data.

We will practice formatting an Event table in Exercise 3-1. But first let's learn about the Occurrence table in the next lesson.

## Lesson 3. How to format Occurrence table

If your dataset structure will have an Occurrence core, or has an Occurrence extension (all OBIS data have at least one occurrence record associated, regardless of what organization structure you have chosen), you should follow the below procedure to format your file.

- Identify columns in your raw data that match with Occurrence fields

- Include columns with measurements for now, but they will be moved to an eMoF table(s)

- **Copy** these columns to a new sheet named Occurrence (it is good practice to never make changes to your original datasheet)

- Create and add **occurrenceIDs** for each unique occurrence record

- Add and fill **basisOfRecord** and **occurrenceStatus** fields

- Ensure your column names map to Darwin Core terms

Let's take a closer look at each of these steps by first watching the video below.

Play Video

- (Available at https://youtu.be/G_AmAmS7ILc)

Now let's look at each step in detail.

## Steps 1-5

## Step 1: Identify relevant fields

Begin by **identifying all fields relevant to observations or occurrences **in the working copy of the datasheet. These may include taxon names, biotic measurements, identification information, hosting institution information, etc. You will need to map your raw data terms to Darwin Core terms if they haven't been yet, but first, let's look at which terms are most relevant for an Occurrence core or extension table. There are several Darwin Core terms that are **required **in Occurrence data tables, including:

- [*occurrenceID*](https://dwc.tdwg.org/terms/)
- [*eventID*](https://dwc.tdwg.org/terms/) *(required for Occurrence extension, not required for Occurrence Core)*
- [*occurrenceStatus*](https://dwc.tdwg.org/terms/)
- [*basisOfRecord*](https://dwc.tdwg.org/terms/)
- [*scientificName*](https://dwc.tdwg.org/terms/)
- [*scientificNameID*](https://dwc.tdwg.org/terms/) *(strongly recommended)*
- [*eventDate*](https://dwc.tdwg.org/terms/) *(not required for Occurrence extension, required for Occurrence Core)*
- [*decimalLatitude*](https://dwc.tdwg.org/terms/) *(not required for Occurrence extension)*
- [*decimalLongitude*](https://dwc.tdwg.org/terms/) *(not required for Occurrence extension)*

While these are the bare minimum, you should strongly consider adding other terms if you have the corresponding information/data in your dataset or documentation. These are identified by their associated Darwin Core class below, and found [here](https://dwc.tdwg.org/terms/).

- Class Occurrence | DwC: [*associatedMedia*](https://dwc.tdwg.org/terms/)
- Class Occurrence | DwC: [*associatedReferences*](http://rs.tdwg.org/dwc/terms/associatedReferences)
- Class Occurrence | DwC: [*associatedSequences*](http://rs.tdwg.org/dwc/terms/associatedSequences)
- Class Occurrence | DwC: [*associatedTaxa*](https://dwc.tdwg.org/terms/)
- Class Occurrence | DwC: [*preparations*](https://dwc.tdwg.org/terms/)
- Class Occurrence | DwC: [*recordedBy*](https://dwc.tdwg.org/terms/)
- Class Occurrence | DwC: [*materialSample*](https://dwc.tdwg.org/terms/)
- Class Occurrence | DwC: [*materialSampleID*](https://dwc.tdwg.org/terms/)
- Class Record | DwC: [*bibliographicCitation*](https://dwc.tdwg.org/terms/)
- Class Record | DwC: [*catalogNumber*](https://dwc.tdwg.org/terms/)
- Class Record | DwC: [*collectionCode*](https://dwc.tdwg.org/terms/)
- Class Record | DwC: [*collectionID*](https://dwc.tdwg.org/terms/)
- Class Record | DwC: [*dataGeneralizations*](https://dwc.tdwg.org/terms/)
- Class Record | DwC: [*datasetName*](https://dwc.tdwg.org/terms/)
- Class Record | DwC: [*institutionCode*](https://dwc.tdwg.org/terms/)
- Class Record | DwC: [*modified*](https://dwc.tdwg.org/terms/)
- Class Taxon | DwC: [*kingdom*](https://dwc.tdwg.org/terms/)
- Class Taxon | DwC: [*scientificNameAuthorship*](https://dwc.tdwg.org/terms/)
- Class Taxon | DwC: [*taxonRank*](https://dwc.tdwg.org/terms/)
- Class Taxon | DwC: [*taxonRemarks*](https://dwc.tdwg.org/terms/)

Any fields you have related to biotic measurements (e.g., sex, lifestage, biomass, length) will also be included in the extendedMeasurementOrFact table. Such measurements can remain in the Occurrence table as long as they can be mapped to the appropriate DwC term (e.g. DwC:Occurrence:sex). This is important because not every data aggregator outside of OBIS indexes the eMoF table, so otherwise this information may be lost.

## Step 2: Copy relevant fields

Create a new, blank spreadsheet and rename it to "Occurrence". After identifying the relevant data columns in Step 1, copy them into the new sheet. Make sure to include the **eventIDs** so occurrence records can be linked back to the Event table. We would like to again emphasize that you should never make changes to your original data sheet. It is important to keep the original data intact in case you need to go back to reference data. This is why we are using a working copy of the data.

## Step 3: Create occurrenceIDs

Add a new, blank column and name it "occurrenceID". Follow guidelines from Module 2 to create unique **occurrenceIDs** for each record if you do not already have an identifier for them. If you already have unique identifiers for each record, copy these identifiers into the **occurrenceID** column. Remember **occurrenceIDs** can be created by adding information to an **eventID**.

## Step 4: Fill basisOfRecord and occurrenceStatus

Create two new columns for **basisOfRecord** and **occurrenceStatus** and populate these for each record. As a reminder, **occurrenceStatus** must be either **present** or **absent** and **basisOfRecord** must be one of: **PreservedSpecimen**, **FossilSpecimen**, **LivingSpecimen**, **MaterialSample**, **Event**, **HumanObservation**, **MachineObservation**, **Taxon**, **Occurrence**, **MaterialCitation**.

## Step 5: Map terms to Darwin Core

As mentioned previously, column headers must map to Darwin Core terms if you have not already done so. Continuing to build on the Documentation sheet, you might add comments such as below.

Practice creating the Event and Occurrence table in Exercise 3-1. Once you're done, return to Lesson 4 to learn how to format the extendedMeasurementOrFact table.

## Lesson 4: How to format extendedMeasurementOrFact table

## Formatting the eMoF

To review, any data related to abiotic or biotic measurements, including sampling information and protocols should be included in the extendedMeasurementOrFact (eMoF) table. Measurement data could also go into the *[*MeasurementOrFact*](https://rs.gbif.org/extension/dwc/measurements_or_facts_2022-02-02.xml)* (MoF) extension, however OBIS recommends using the *[*extendedMeasurementOrFact*](https://rs.gbif.org/extension/obis/extended_measurement_or_fact.xml)* instead, particularly if your data is based on an Event core table.

Structuring your data to fit the eMoF extension may be one of the more confusing processes during data formatting. Rather than documenting each of your measurements in separate columns (e.g., columns for biomass, abundance, length, gear size, percent cover, etc.), these **measurements will be condensed into one column: **measurementValue**.

Then **measurementType** describes what the measurement actually is, for example whether it is an abundance value, length, percent cover, or any other biotic/abiotic measurement. **measurementUnit** is used to indicate the unit of the measurement.

By linking **measurementType** and **measurementValue** with the identifiers **eventID** and/or **occurrenceID**, you can have measurements linked to *one* event (e.g. temperature, salinity), measurements linked to occurrence records (e.g. length, weight), as well as sampling facts that are linked to events (protocol, size, gear, etc.). Information specifically related to how samples were taken will have the measurementTypes: **sampleSizeValue**, **sampleSizeUnit**, **samplingEffort**, and **samplingProtocol**.

Similar to Event and Occurrence tables, we can follow this process to build our eMoF extension table:

1. Create a blank sheet and name it eMoF.

2. Add 9 column headers for:

- eventID
- occurrenceID
- measurementType
- measurementValue
- measurementUnit
- measurementTypeID
- measurementValueID
- measurementUnitID
- measurementRemarks

Copy eventID and occurrenceID values for measurements from your Occurrence table and paste them into the associated fields in your new, blank eMoF table.

Note that measurements associated with an occurrence **must have the corresponding occurrenceID and eventID**; measurements associated with an event must have the corresponding eventID.

Copy the first column containing **measurement values**, and paste it into the **measurementValue** field.

Then, fill **measurementType** with the name of the variable (e.g., count, length, etc.).

Add the unit of measurements, where applicable, to the **measurementUnit** field (e.g. individuals, cm, lb, etc.).

For any other measurements related to the occurrences (or events), repeat steps 3-5.

Repeat steps 3-6 for any measurements in the Event table.

Fill measurement ID fields **measurementTypeID**, **measurementUnitID**, and **measurementValueID** (details on how to find these identifiers will be covered in Module 5).

Let's watch a video demonstration of this process.

Play Video

- (Available at https://youtu.be/EjM0HRrF1B4?si=_nh5RRgU3KuDUrbi)

Now we will review again each step in this process.

## Steps 1-5

## Step 1-2: Create a blank sheet and add 9 column headers

In your spreadsheet software, **create a blank sheet named "eMoF"**. Label 9 columns with the headers: **eventID**, **occurrenceID**, **measurementType**, **measurementValue**, **measurementUnit**, **measurementTypeID**, **measurementValueID**, **measurementUnitID**, **measurementRemarks

## Step 3: Copy identifiers from the Occurrence table

Go to your Occurrence table and **copy all **eventIDs** and **occurrenceIDs**** (i.e. copy the entire column). Paste these into the **eventID** and **occurrenceID** column of the eMoF table. This ensures the measurements we will paste will be associated with the correct event and occurrence record.

## Step 4-5: Copy the first measurement variable and add units

From your Occurrence table, identify the first column that contains measurement data. It doesn't matter which one you choose first, but it's important to do one measurement at a time.** Copy (or cut) the column of measurement values** and paste into the **measurementValue** column in the eMoF sheet. In the **measurementType** column, be sure to indicate for each row what the type of measurement was. You can use the original column header to fill this information in (e.g. length).

Then in the **measurementUnit** column, fill in the the units for this measurement variable, being sure to include every record.

## Steps 6-8

## Step 6: Repeat steps 3-5 for additional measurements

We will repeat the previous steps for any additional columns containing measurements on your occurrences. Begin with step 3 and copy the **eventID** and **occurrenceID**. **Paste these IDs below the last record**. This will stack the measurements into a "long" format.

Then, copy or cut the next set of measurement values**, pasting again into **measurementVaue**. Fill in the **measurementType** and **measurementUnit** columns as before. **Continue to repeat this process for all measurements until all have been moved into the eMoF table.

## Step 7: Repeat for Event measurements

Repeat steps 3-5 for any measurements in your Event table**, ensuring to copy the corresponding **eventIDs**. Recall that measurements associated with an event **must** have the corresponding **eventID**, but they may not need a corresponding **occurrenceID**.

## Step 8: Fill measurement ID fields ** **measurementTypeID**, **measurementUnitID**, and **measurementValueID

After you have completed formatting your measurement data, it is important to add identifiers, or Unique Resource Identifiers (URIs), associated with the measurement type, values, and units. This will help account for the heterogeneity that occurs when filling the fields, particularly the **measurementType** field. For example, there are many ways one may write "length" (len, length, fork length, Length, etc.). By attaching the same URI or identifier to your measurements, we know that all measurements with the same identifier are the same type of measurement.

For now, all you need to know is that you should try to find a **measurementTypeID** URI that belongs to NERC's *[*P01 collection*](http://vocab.nerc.ac.uk/search_nvs/P01/?searchstr=&options=identifier,preflabel,altlabel,status_accepted&rbaddfilter=inc&searchstr2=)*. We will come back to specific guidelines for choosing measurement ID identifiers in Module 5.

## End of Module

In this Module we learned how to format the Event, Occurrence, and eMoF tables. We learned about identifying hierarchical event structure and removing duplicate event records, how to identify occurrence data within a dataset, and** that formatting the eMoF likely entails switching data from a wide or unstacked format (where each measurement was a separate column) to a long or stacked format (measurements are stored in the **measurementType**, **measurementUnit**, and **measurementValue** columns).

Practice what you have learned by completing Exercise 3-1 if you haven't already done so, and Exercise 3-2 to create an eMoF table. Complete Quiz 3, then, in the next module we will learn how to format a special type of data to Darwin Core: DNA derived data.
