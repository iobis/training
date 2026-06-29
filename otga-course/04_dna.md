Module 4: DNA derived data

# Module 4

# Introduction

In this module you will learn how to format DNA derived data. This includes data obtained from eDNA sampling, qPCR, or DNA from individuals. We will follow an example to help demonstrate the formatting process, and highlight areas you should be particularly careful of.

# Learning Outcomes

After successful completion of this module, you should be able to:

- Understand the different categories of genetic data
- Prepare DNA and sequence data for publishing to OBIS by
- Using an Occurrence core
- Recording genetic information in the DNA-Derived Data extension
- Identify relevant fields for eDNA/metabarcoding data compared to qPCR data

# How to Proceed

To succeed in this Module, you need to successfully complete the following lessons and exercises:

- Lesson 1: Introducing DNA data
- Lesson 2: Types of genetic data
- Lesson 3: Compiling DNA data for categories I and II: eDNA and metabarcoding
- Lesson 4: Compiling DNA data for category III: qPCR data
- Lesson 5: Categories I and II example: eDNA
- [Exercise 4-1: Describe DNA data fields](https://classroom.oceanteacher.org/mod/assign/view.php?id=32948)

as well successfully complete Quiz 4 with a score of ≥80%.

[Quiz 4](https://classroom.oceanteacher.org/mod/quiz/view.php?id=32949)

# Lesson 1: Introducing DNA data

## Introduction to DNA data

DNA derived data are increasingly being used to document taxon occurrences. This genetic data may come from a sampling event, an individual organism, may be linked to physical material (or not), or may result from DNA detection methods e.g., metabarcoding or qPCR (Quantitative Polymerase Chain Reaction). Thus genetic data may reflect a single organism, or may include information from bulk samples with many individuals. Still, DNA-derived occurrence data of species should be documented as standardized and as reproducible as possible.

To ensure DNA data are useful to the broadest possible community, a community guide entitled [Publishing DNA-derived data through biodiversity data platforms](https://docs.gbif-uat.org/publishing-dna-derived-data/1.0/en/) was published by GBIF, OBIS, and others. This guide is supported by the [DNA derived data extension for Darwin Core](http://rs.gbif.org/extension/gbif/1.0/dna_derived_data_2021-07-05.xml), which incorporates MIxS (Minimum Information about any (X) Sequence) terms into the Darwin Core standard. We will review important aspects of this guide and extension in this module.

Note that because guidelines for DNA data are still evolving, there may be updates to the publishing guide in the future.

## Understanding DNA-based occurrence

Occurrences from DNA sampling are by nature derived data. This means that, most commonly, the observations did not result from a direct detection by visual methods or by catching individuals. Many decisions are made in the process of collecting and analyzing DNA information that may affect the results. It is important to record as much metadata on these sampling, sample processing, and data analysis choices as possible to ensure that, in the future, we can evaluate if the DNA data is reliable and comparable to other studies. The DNA-derived data extension to Darwin Core was developed to allow this information to be recorded.

It is important to understand what the recorded species information in the DNA derived data extension represents. The amount of information that can be acquired from DNA sampling and next generation sequencing methods is huge, often leading to lists of hundreds of species and thousands of sequences per sample. Sequences (or “reads”) acquired from the sequencing machine, that have not been analyzed, are called “raw” sequences, and still require in-silico analysis by bioinformatics before they can be given species names.

Let's understand more about these raw DNA sequences, but first see the figure below for a simplification of the process from DNA sample collection to species identification.

Figure adapted from Figure 2 in Publishing DNA-derived data guide (Abarenkov et al. 2023 [https://doi.org/10.35035/doc-vf1a-nr22](https://doi.org/10.35035/doc-vf1a-nr22)).

## Raw DNA sequences

Raw sequence information is already commonly shared publicly to sequence databases in the International Nucleotide Sequence Database Collaboration (INSDC). There are three databases in the collaboration: the National Center for Biotechnology Information in the USA (NCBI), the European Bioinformatics Institute of the European Molecular Biology Laboratory (EMBL-EBI), and the DNA Data Bank of Japan (DDBJ).

Sequences in these databases can be openly accessed, analyzed, and used for comparisons. However, as they are not processed, it is not usually possible to search for species, location, sampling time, etc., because the data still requires a complex analysis.

To acquire species information the raw sequences must be analyzed. This commonly includes quality control, error correction, and taxonomic annotation. Taxonomic annotation is done by comparing the sequence to a reference database containing known named sequences from the same genetic region. Finally, the quality controlled sequences (i.e. true sequences) are called Amplicon Sequence Variants (ASVs). If the sequences are then also clustered to form larger groups (i.e. using specific algorithms or percent similarity) they are called Operational Taxonomic Units (OTUs) and can be interpreted to represent a taxonomic group like a species.

ASVs contain more information as they have not been clustered, even if the sequences differ by one nucleotide. On the other hand, OTUs are usually clustered based on 97% sequence similarity.

The DNA-derived data extension allows us to record and store ASV and OTU information linked to species annotations and include rich metadata. This ensures that the data can be searched and accessed in a more flexible manner, and therefore opens up a host of different possibilities for large data analysis!

As we mentioned earlier, MIxS standards were incorporated into the Darwin Core guidelines. Let's review these terms now.

## MIxS and DwC

An important note is that data to INSDC and EMBL-EBI is deposited using the Minimum information about any (x) nucleotide sequence (MIxS, Yilmaz et al, 2011), the core standard developed by the Genomics Standard Consortium (GSC). This standard consists of checklists for describing genomes (MIGS), metagenomes (MIMS) and marker sequences (MIMARKS). As mentioned, a [mapping between DwC and MiXs](https://github.com/tdwg/gbwg/tree/main/dwc-mixs/mapping) has been made to ensure interoperability of the two standards, see the table below for how the terms compare.

| MiXs terms | Associated DwC terms |
| --- | --- |
| lat_lon | verbatimCoordinates decimalLatitude decimalLongitude verbatimLatitude verbatimLongitude |
| depth | verbatimDepth minimumDepthInMeters maximumDepthInMeters |
| alt | minimumDistanceAboveSurfaceInMeters maximumDistanceAboveSurfaceInMeters |
| elev | verbatimElevation minimumElevationInMeters maximumElevationInMeters |
| geo_loc_name | country waterBody higherGeography |
| collection_date | eventDate |
| source_mat_id | materialSampleID |
| biotic_relationship | associatedOrganisms |
| samp_collect_device | samplingProtocol |
| samp_mat_process | samplingProtocol |
| samp_size | sampleSizeValue sampleSizeUnit |
| url | references associatedMedia associatedOccurrences associatedOrganisms associatedReferences associatedSequences associatedTaxa |

## Lesson summary

In this lesson we began learning about DNA-derived data, what it is, what are raw sequences, and we gained some basic understanding that DNA data needs to be processed before being formatted to DwC.

Next, let's learn about the five types of DNA-derived data, and which ones can be published to OBIS.

# Lesson 2: Types of genetic data

## Types of DNA data

Before we get into details about how to format DNA-derived data, it is good to recognize that there are 5 categories for which genetic data could fall into:

- DNA-derived occurrences
- Enriched occurrences
- Targeted species detection
- Name references
- Metadata only

The figure below provides a graphical representation of each of these categories.

Figure adapted from Figure 6 in Publishing DNA-derived data guide (Abarenkov et al. 2023 [https://doi.org/10.35035/doc-vf1a-nr22](https://doi.org/10.35035/doc-vf1a-nr22)).

But how do you know which type of data you have?

To determine which category your data falls into, follow the decision tree below, (adapted from the [Data packaging and mapping](https://docs.gbif.org/publishing-dna-derived-data/1.0/en/) section of the Publishing DNA-derived data guide).

We will review formatting guidelines for eDNA (Category I) and qPCR (Category III) data in Lessons 3 and 4, respectively. First, let’s take a closer look at the five categories, and what kind of data falls into each.

## Category I: DNA-based occurrences

This category contains data where the only evidence of an occurrence or presence of an organism is through the detection of its DNA through sequencing. In these cases, no observation of a specimen occurred - the detection is solely DNA-based.

Commonly, category I includes DNA that is sampled directly from the environment (e.g. water, soil, and air samples), but can also be, for example, an analysis of stomach contents of an organism. However, the occurrences in this category have not been detected directly and therefore it is highly important to understand how the species information was acquired, and to evaluate the reliability and comparability of the data.

In principle, ASVs/OTUs can be the only indication of species presence - a species annotation is not required. Currently, reference databases are still incomplete (i.e. missing sequences for many species) and therefore many sequences will remain unknown. The most important information to record is each individual sequence. These sequences can then be easily compared and analysed in future studies.

It is also important to note, that due to the nature of sequencing, all quantities of reads recorded in this category are relative; i.e. the quantity is always related to the total amount of reads acquired in each sample, and does not directly reflect the quantity of that sequence in the original sample. So for each occurrence it is required to record the amount of that specific sequence that was detected in a sample, as well as the total amount of reads in that sample (e.g. indicating that 12% of all reads had this sequence).

Metagenomics, metabarcoding, and eDNA works typically fall into this category.

## Category II: Enriched occurrences

An “enriched occurrence” is when genetic material is associated with an observation or a specimen. A DNA sequence is not the only evidence of the occurrence, rather the associated specimen was also observed in some way (e.g., physical observation, collected specimen, etc.).

Studies that contain reference material and genetic sequences fall into this category, for example barcoding of museum specimens and some metabarcoding studies.

Datasets belonging to Categories I and II are usually similar in data formatting and thus will have similar recommendations. We will look at an example of how to format such data with an eDNA use case in Lesson 5.

## Category III: Targeted species detection

This category contains datasets that have used a targeted qPCR/ddPCR assay to detect the presence (or absence) of a specific organism in a sample. This approach is frequently used to detect the presence of invasive species (e.g. [American bullfrog, Ficetola et al. 2008](https://royalsocietypublishing.org/doi/10.1098/rsbl.2008.0118)) and thus can be a very valuable genetic tool. Observations detected through qPCR are (likely) not associated with the genetic material (i.e. the sequence). With qPCR an occurrence is recorded when a known sequence is found in the sample using PCR. The analysis does not include sequencing of the PCR products, therefore the analysis (and metadata) requirements differ considerably from category I and II. Importantly,  the genetic material itself (e.g., a DNA sequence) is not usually associated with the occurrence record because the analysis does not include sequencing.

Data in this category are sensitive to the detection limit of the assay (the minimum amount of DNA required to detect presence of target DNA), as well as contamination from field and lab procedures. This can make it difficult to have high confidence in an absence. When preparing data in this category, it is important to provide enough information to be able to evaluate the records.

## Category IV: Name references

In reference databases, sequences can be clustered by similarity and given an ID. These IDs can then be used to index other unknown sequences, similar to how OBIS uses scientificNameID to categorize species names. These types of IDs can be registered in GBIF as Name References, but because they, by definition, do not have corresponding Linnaean names (e.g. a taxonomic identification), we do not currently publish this data to OBIS. This does not apply to unknown sequences in a typical study, but only the list of names defined in well-established stable genetic reference databases.

Datasets from this category are thus not relevant for OBIS at this time.

NOTE: if you have this type of data and want to publish it to GBIF, please see the [publishing guide for name references](https://docs.gbif-uat.org/publishing-dna-derived-data/en/), otherwise we won’t be focusing on this type of data in this course.

## Category V: Metadata-only

This last category contains data about the data, very similar to the metadata we will learn about in Module 7: Publishing to OBIS. It can include a description of the dataset, methods used in the field and/or laboratory, sequencing methods, authors and affiliations, taxonomic, temporal, and geographical scope, and the purpose of the dataset. In some cases, you may want to indicate that a study has been conducted and that the raw sequence data is available, but you might not have the occurrences (i.e. ASVs/OTUs) available yet. In these cases you can publish metadata only, and then add the occurrence information later when it becomes available. A link can be added to the raw sequences submitted to public sequencing databases with more rich metadata. Submitting Metadata-only datasets can be useful so that your data will be discoverable even before you have occurrence records, and can make the publishing process easier.

Guidelines for this type of data should follow general metadata recommendations in Module 7. Be sure to provide as many details as possible, especially when describing field, laboratory, and bioinformatics methods. Alternatively, provide a link to the protocols if they are already published in detail elsewhere (e.g. [protocols.io](https://protocols.io/) or [NEON protocols collection](https://www.neonscience.org/data-collection)).

## Lesson Summary

Now that we understand the different types of genetic data, let’s look at the specific guidelines on how to format DNA-derived data so that it can be published to OBIS.

# Lesson 3: Compiling DNA data for categories I and II: eDNA and metabarcoding

Genetic data may now be published using either the Occurrence core and DNA-derived data extension, or by utilising the Event core and adding both an occurrence extension and the DNA-derived data extension. eDNA and DNA derived data are linked to the Occurrence core data table with the use of occurrenceID and/or eventID. The DNA derived data can also be linked to both the Event core (by using eventID) and the Occurrence extension (by using the occurrenceID).

Use of the Event core can help in decreasing the amount of data, as metadata linked to samples is recorded only once. Additionally, a [new data model](https://www.gbif.org/new-data-model) is being developed by GBIF and the OBIS community that would enable the linking of multiple different datatypes and tables, and will get rid of the star schema, enabling the linking of one sequence to multiple occurrences, therefore decreasing the amount of data needed. However as it is not implemented yet (as of September 2025), we focus on the current Darwin Core recommendations here.

Each record (row) in the Occurrence table will contain information about the organisms (or unknown ASVs/OTUs) in the sample, including:

- The taxonomy and quantity of detected sequences
- The total quantity of the reads in a sample (to enable calculation of the relative abundance)
- Location where samples were collected (or recorded in the Event table)
- Time when samples were collected (or recorded in the Event table)
- References for the identification procedure
- The DNA sequence that defines that occurrence
- Links to the generated raw sequences

The [DNA-Derived Data extension](http://rs.gbif.org/extension/gbif/1.0/dna_derived_data_2021-07-05.xml) will contain important information about sequencing methods and the sequence (e.g., concentration, annealing temperatures, primer information, etc.). It is important to note that this extension may change as guidelines are refined. Thus using this extension may require remapping in the future.

In this lesson we will focus on how to compile and format data only for DNA data belonging to category I or II.

## DNA data files

To format your genetically derived data to publish to OBIS, make sure you have the information on the sequence and possible taxonomy for each occurrence record associated with a DNA sample.

Genetic data is often recorded in multiple different files, and you might receive or work with this type of format:

- an OTU-table,
- a taxonomy table,
- a sample information table, and
- a .fasta file with sequences

The OTU-table is a sequence by sample table (Table 1), which records the quantity of each unique sequence found in each sample. Sequences are usually referred to by an ID, which is unique only in the dataset (e.g. asv1, asv2, asv3 …).

Table 1. An example of an OTU/ASV table for ASVs (unique sequences are rows), indicating how many copies of each sequence was found in each sample (columns). In this case the column sums would give you the total read count for each sample. The table may also be transposed (OTUs/ASVs as columns and samples as rows).

|   | Sample1 | Sample2 | Sample3 | Sample… |
| --- | --- | --- | --- | --- |
| asv1 | 12560 | 123 | 0 | … |
| asv2 | 4 | 3 | 0 | … |
| asv3 | 459 | 45 | 650 | … |
| asv… | … | … | … | … |

The taxonomy table (Table 2) is a sequence by taxonomy table, which records the taxonomy linked to each unique sequence, as defined by the annotation method.

Table 2. The taxonomy table will typically record the taxonomy of each ASV/OTU as assigned in the bioinformatic analysis.

|  | Kingdom | Phylum | Class | Order | Family | Genus | Species |
| --- | --- | --- | --- | --- | --- | --- | --- |
| asv1 | Eukaryota | Mollusca | Bivalvia | Ostreida | Ostreidae | Dendostrea | Dendostrea frons |
| asv2 | Eukaryota | Chordata | Actinopteri | Chaetodontiformes | Leiognathidae | Equulites |  |
| asv3 | Eukaryota | Chordata |  |  |  |  |  |
| asv… | … | … | … | … | … | … | … |

The sample information table (Table 3) records the metadata of each sample (e.g. location, time, and collection method).

Table 3. An example of a typical sample information table, which will give you information to differentiate the samples in a dataset from each other.

|  | Date | Filter | Amount | Location | Depth | Replicate |
| --- | --- | --- | --- | --- | --- | --- |
| Sample1 | 2023-07-05 | 0.2 | 1200 | Site1 | 10 | 1 |
| Sample2 | 2023-08-03 | 0.2 | 800 | Site2 | 10 | 1 |
| Sample3 | 2023-08-03 | 0.8 | 5300 | Site1 | 100 | 1 |
| Sample.. | … | … | … | … | … | … |

 Finally the .fasta file records the actual DNA sequence that is linked to each sequence id. An example of a .fasta file format:

```
>asv1
ATTCGGGGTTCAACTTTGAATTTTTAACCGGCTACCTGGGAACTTACGATAATTCCG
>asv2
ATTCGGACCTGGGAACTTACGATAATTCCGCTTGAACTGGGACTGAACTTTGAACG
>asv3
CTAAGTAACCTGGAACTTAGGGGTAAAAGTCCGTCAACTGAGAGAGTCGAGAACG
…
```

This fasta file is not always provided, but is critical to fully benefit from the possibilities of DNA data.

## DNA files to Darwin Core

The examples on the previous page show how DNA-derived data contains multiple types of information that is often recorded in separate, but interconnected tables.

In the Darwin Core archive format, each unique sequence by sample combination is considered one occurrence. Therefore the data from these tables will need to be formatted to the “long format”, including a row for each sequence in each sample. See the figure below for a demonstration of how this can be done.

Once you have all your information in one place, we can begin data formatting for OBIS, as some columns will be placed in the Occurrence table, and others in the DNA Derived Data extension table.

## Beginning DNA occurrence dataset formatting

As we begin the data formatting process, we will first fill in the Occurrence table, and then complete the DNA Derived Data extension (as well as the eMoF extension, if applicable, for any measurements taken).

Due to the high quantities of occurrence records from a DNA sample, the data formatting is usually done with scripts using, for example, R or Python. A few examples of this are available at (here links to the [monterey ](https://github.com/iobis/dataset-edna)and [uk ](https://gitserver.mba.ac.uk/external/data_team/eDNA)examples). Note that these scripts are now out of date in terms of how to deal with unknown sequences, however they are still a good example for how to automate data formatting for DNA data.

See the quick start figure below for an overview of how to format (e)DNA for OBIS.

Next, let’s look at formatting the Occurrence table.

## Format Occurrence core table

Formatting the Occurrence core table will generally follow the guidelines we learned in Modules 2 and 3. As you will recall, required terms for Occurrence core tables include:

- occurrenceID
- scientificName
- scientificNameID (strongly recommended)
- basisOfRecord
- occurrenceStatus
- eventDate
- decimalLatitude
- decimalLongitude

However, the following additional terms are strongly recommended to additionally include for DNA datasets. Please see the notes below for some important differences in term usage for DNA data.

- Class Occurrence | DwC: [organismQuantity](https://dwc.tdwg.org/terms/) (the regular DwC definition does not apply for DNA data, see below)
- Class Occurrence | DwC: [organismQuantityType](https://dwc.tdwg.org/terms/) (the regular DwC definition does not apply for DNA data, see below)
- Class Occurrence | DwC: [associatedSequences](https://dwc.tdwg.org/terms/)
- Class Identification | DwC: [identificationRemarks](http://rs.tdwg.org/dwc/terms/identificationRemarks)
- Class Identification | DwC: [identificationReferences](http://rs.tdwg.org/dwc/terms/identificationReferences)
- Class Identification | DwC: [verbatimIdentification](http://rs.tdwg.org/dwc/terms/verbatimIdentification)
- Class Taxon | DwC: [taxonConceptID](http://rs.tdwg.org/dwc/terms/taxonConceptID)
- Class Event | DwC: [sampleSizeValue](https://dwc.tdwg.org/terms/) (note the regular DwC definition does not apply for DNA data, see below)
- Class Event | DwC: [sampleSizeUnit](https://dwc.tdwg.org/terms/) (note the regular DwC definition does not apply for DNA data, see below)
- Class Event | DwC: [samplingProtocol](https://dwc.tdwg.org/terms/)
- Class Material Sample | DwC: [materialSampleID](https://dwc.tdwg.org/terms/)

Here it is important to understand the meaning of the abundance values recorded in organismQuantity and sampleSizeValue. As mentioned before, the quantities recorded with sequencing studies always represent relative abundance to the total reads in the sample, and cannot be directly compared across samples. This is due to the nature of the sample processing protocol and the amplification of DNA with PCR, which biases the original quantities.

In the field organismQuantity, you will record the amount of that unique sequence in that specific sample (i.e. 33 reads). In the field sampleSizeValue, you will record the total number of all reads in that specific sample (i.e. 15310 reads). This information will allow the person accessing the data to calculate the relative abundance of that sequence in the sample.

Both fields organismQuantityType, and sampleSizeUnit, get the value “DNA sequence reads”, as it is of high importance that sequence abundances are not confused with organism abundances recorded by traditional methods. The abundance information can usually be found in the “OTU-table”.

associatedSequences should contain a reference to the URL domain where genetic sequence information associated with the Occurrence can be found, e.g. a link, identifier, or list (concatenated and separated) of identifiers. Can link to archived raw barcode reads and/or associated genome sequences, like a public repository. It is recommended that links contain the domain name (e.g. NCBI) in the URL, for example: [https://www.ncbi.nlm.nih.gov/bioproject/PRJNA887898/](https://www.ncbi.nlm.nih.gov/bioproject/PRJNA887898/). The actual sequence of the occurrence will be documented in the DNA Derived Data extension.

identificationRemarks should be used to record information on how the taxonomic information of the occurrence was reached against which reference database, and, if possible, with which confidence. For example “RDP annotation confidence: 0.96, against reference database: GTDB”. This information should be recorded in the bioinformatic protocol of the study. Note: this information will also be recorded in the DNA derived extension in the fields otu_seq_comp_appr and otu_db.

identificationReferences should include a link to the bioinformatic pipeline or publication where the identification process is explained in detail.

taxonConceptID should include the taxonomic ID of the sequence (non-Linnean). Often genetic sequences can be assigned an ID linked to a reference database that is not a linnean name. These taxonomic names or IDs from other taxonomic databases (like the NCBI taxonomic database) can be recorded in this field. For example: NCBI:txid9771. The name linked to this ID can then be recorded in the field verbatimIdentification. We will discuss recording unknown sequences later in this lesson.

samplingProtocol can contain free-text that briefly describes the methods used to obtain the sample, or a link to a protocol that is recorded elsewhere.

We will look at an example dataset that demonstrates usage of these terms in Lesson 5. Let's next look at guidelines for the DNA Derived Data extension.

## Format DNA Derived Data extension table

As mentioned earlier in this module, the DNADerivedData extension is meant to capture information related to the sampled DNA, including sampling, processing, and other bioinformatic methods.

The following (free-text) terms are strongly recommended to include in the DNADerivedData extension for eDNA data. We have bolded some terms of particular interest, details on these are provided below.

- DNA Derived | DwC: DNA_sequence
- DNA Derived | DwC: sop
- DNA Derived | DwC: target_gene
- DNA Derived | DwC: target_subfragment
- DNA Derived | DwC: pcr_primer_forward
- DNA Derived | DwC: pcr_primer_reverse
- DNA Derived | DwC: pcr_primer_name_forward
- DNA Derived | DwC: pcr_primer_name_reverse
- DNA Derived | DwC: pcr_primer_reference
- DNA Derived | DwC: Pcr_cond
- DNA Derived | DwC: annealingTemp
- DNA Derived | DwC: annealinTempUnit
- DNA Derived | DwC: ampliconSize
- DNA Derived | DwC: env_broad_scale
- DNA Derived | DwC: env_local_scale
- DNA Derived | DwC: env_medium
- DNA Derived | DwC: lib_layout
- DNA Derived | DwC: seq_meth
- DNA Derived | DwC: otu_class_appr
- DNA Derived | DwC: otu_seq_comp_appr
- DNA Derived | DwC: otu_db

For a complete list of terms, as well as descriptions for those above, see the [DwC DNA Derived Data extension page.](http://rs.gbif.org/extension/gbif/1.0/dna_derived_data_2021-07-05.xml)

The most important field in the extension is the DNA_sequence field, where the ASV/OTU sequence will be recorded. This field can then be searched with sequence alignment methods to, for example, find closely related sequences recorded in other studies, and will allow very powerful data comparison and analysis in the future. It will also make your sequence available in the [OBIS sequence search tool](https://sequence.obis.org/) (discussed later in this module).

The remaining metadata fields will help the person accessing the data to filter out the data of interest (e.g. specific genetic region with target_gene, target_subfragment, or pcr_primer fields), link to the public sequence databases with the MixS specific fields (e.g. env_ fields), and evaluate the reliability of the sequence annotation method (e.g. otu_ fields).

It is recommended to use [Environment Ontology (ENVO)’s biome classes](https://ontobee.org/ontology/ENVO?iri=http://purl.obolibrary.org/obo/ENVO_00000428) to describe the environmental system from which the sample was extracted. Like other identifiers, it is important to provide the exact ENVO reference identifier. Environmental systems are described in the two fields env_broad_scale and env_local_scale. The env_broad_scale is meant to provide a coarse resolution for which environment your sample came from. For data being published to OBIS, it is likely that this will be [marine biome (ENVO:00000447)](https://ontobee.org/ontology/ENVO?iri=http://purl.obolibrary.org/obo/ENVO_00000447). In general, we recommend selecting a biome subclass from ENVO. Then, of course, for the local scale you should identify the specific environment your sample was obtained from (e.g., coastal water, benthic zone, etc.). See the screenshot below for an example of ENVO's marine biomes. You may have to search by keywords for your specific biome.

Next we will discuss the Event core and then the extendedMeasurementOrFact extension.

## Event core with DNA data

Using Event core with DNA derived data may be helpful in cases where there is a lot of different information linked to the same samples. Use the guidelines described in [Module 3 Lesson 2](https://classroom.oceanteacher.org/mod/book/view.php?id=32934&amp;chapterid=8484) to create your Event table.

The workflow and required terms are in practice the same as described previously. As each Occurrence depicts a sequence in a location and time, it is necessary to copy the occurrence for each sample (i.e. format the OTU table with taxonomic information into a long format dataset). The main difference is that now the location and sampling information is captured within the Event table, and the Occurrence table will also contain an eventID.

Additionally, by adding the occurrenceID to the DNA derived data table, a link between the Occurrence extension table and the Event core table can be made.

The following image shows the general structure of a dataset using Event core, Occurrence extension, and DNA derived data extension.

## Format eMoF table

If you have taken any measurements related to the specimen, the environment, or related to the sampling protocol that aren’t DNA-specific, please record this information in the extendedMeasurementorFact (eMoF) extension table. Environmental measurements could include water temperature and salinity.

For eMoF table guidelines, please see [Module 3 Lesson 4](https://classroom.oceanteacher.org/mod/book/view.php?id=32934&chapterid=8489).

As we have learned, including taxonomic information is important to ensure our datasets are interpretable. But naming our DNA sequences isn't always so straight-forward, especially if we have uncharacterized sequences in our datasets. Let's discuss that next.

## Taxonomic sequence names: known or unknown sequences

The taxonomic names and link to LSIDs from WoRMS are added in the same way as any other dataset (see guidelines for populating scientificNameID, [Module 2](https://classroom.oceanteacher.org/course/section.php?id=10264)). However, it is good to note that with genetic data, the names can be less defined, i.e. often containing subscripts like “unknown species”, “environmental sample” etc. As mentioned, the OBIS database keeps Linnaean names as the standard; allowing easy comparison and search by species names. To search for the LSIDs of your names you can use the R packages [worrms](https://github.com/ropensci/worrms)::wm_records_names, [taxize](https://github.com/ropensci/taxize)::[get_wormsid](https://rdrr.io/cran/taxize/man/get_wormsid.html), and taxize::[get_boldid](https://rdrr.io/cran/taxize/man/get_boldid.html). As a reminder from Module 2, names that are not well defined should be added at a higher taxonomic level. The original annotated name can still be included in the field verbatimIdentification.

## Dealing with unknown sequences

It is important to understand the significance of unknown and uncharacterized sequences in genetic studies. Sequences are given taxonomic names based on comparisons to a reference database. The reference databases contain sequences that have been submitted with a name. Ideally, the reference database is a collection of sequences that are derived from vouchered, morphologically identified specimens. Notably, currently this is often not the case and sequences can also have erratic annotations. Furthermore, only a small portion of species have sequences in reference databases. Due to this reason, typically many sequences in any given study will remain uncharacterized. This is especially the case for tropical regions with high biodiversity. By recording all sequences, including uncharacterized sequences, we make sure that the information is not lost, even if the annotation is currently incorrect or missing. These uncharacterized sequences can then still be compared to other studies, and can be given a taxonomic name as more specimens are sequenced and added to the reference databases.

For unknown sequences it is required to populate the scientificName field with “Incertae sedis”, or the lowest taxonomic information, if available. For example, if it is only known which Class a sequence belongs to, populate scientificName with the associated Class name. Similarly, scientificNameID should be populated with the WoRMS LSID for the name given to scientificName. For records recorded as Incertae sedis, scientificNameID should be populated with [urn:lsid:marinespecies.org:taxname:12](https://www.marinespecies.org/aphia.php?p=taxdetails&id=12). We recommend also populating verbatimIdentification with the name that was originally documented (e.g. phototrophic eukaryote).

## Sequences without a WoRMS-ID (AphiaID)

WoRMS keeps up-to-date records of linnean names of marine species, confirmed by experts in each taxonomic group. However, WoRMS is in the process of adding more protists and prokaryotes to their database. Within (e)DNA datasets there are usually many taxonomic assignments that cannot be found in the WoRMS database currently (i.e. no AphiaID available).

In many cases due to GBIF combining several taxonomic backbones, a name might be available and mapped in GBIF, making sharing datasets between GBIF and OBIS complicated. Therefore, the submitter of the data may in these cases record the known species name, in scientificName, the ID of the taxon in another database in taxonConceptID and leave scientificNameID empty. (Note; this is different from the case above, where no assignment has been made, or the assignment has only been made to a higher taxonomic level.)

The OBIS ingestion workflow will be updated soon (end 2025) to allow searching for higher taxonomic levels that may be available in WoRMS and automatically adding these to the dataset, as well as the name of the corresponding scientificName, while keeping the original submitted species name in originalScientificName. In this way, all known species names are kept, while also enabling the mapping of the full datasets to OBIS with the best available information.

## Control Samples

The recommended way to connect control samples to your dataset, is to submit them as a separate files within the same dataset, that will not be mapped to avoid that they end up in the GBIF or OBIS occurrence indexes. In this way they are available for anyone who is looking for further information on your dataset, but it will not be confusing in the global OBIS search.

Currently (October 2025), this feature has not been implemented yet in IPT (see [https://github.com/gbif/ipt/issues/2109](https://github.com/gbif/ipt/issues/2109)).

## Lesson summary

We have learned what fields are required and strongly recommended when formatting DNA data belonging to Categories I and II. We better understand the Occurrence core + DNA derived data extension + (optionally) extendedMeasurementOrFact extension structure or Event core structure for DNA datasets.

A useful tool for (e)DNA data is the [metabarcoding data toolkit](https://mdt.obis.org/). The purpose of the toolkit is to simplify data formatting of (e)DNA data. A comprehensive [manual](https://docs.gbif-uat.org/mdt-user-guide/en/) is available, and the toolkit lets you upload your typical data files:

- An OTU table
- A taxonomy table
- A fasta file
- A sample information table
- Metadata file

See the [OBIS Manual](https://manual.obis.org/dna_data.html) for more details about this toolkit.

Before we look at an example that implements the guidelines covered in this Lesson, let's learn how to format data belonging to Category III, qPCR data.

# Lesson 4: Compiling DNA data for category III: qPCR data

## Introduction to compiling qPCR data

Compiling qPCR data is a little bit different than compiling eDNA or metabarcoding data. One of the main differences is that there are no sequences recorded in the DNA_sequence field of the DNA derived data extension.

Instead, occurrences are based on detections made using species-specific primers and either qPCR (Quantitative Polymerase Chain Reaction) or ddPCR (Droplet-Digital Polymerase Chain Reaction), no sequencing is done. Both of these methods measure the quantity of a marker in a sample. Usually, a standard curve is added to each qPCR run to calculate the absolute copy numbers from the results.

It is very important to document the methods used for this type of data because the results can be sensitive to the specificity of the primers/assays used. Therefore documenting as much detail as possible on the methodologies is important to ensure data interpretability.

Let’s now go over some important aspects to consider while formatting this type of data, beginning with the Occurrence core.

## Format Occurrence core for qPCR data

As usual, when formatting the Occurrence table we will need to populate the required terms:

- occurrenceID
- scientificName
- scientificNameID (strongly recommended)
- basisOfRecord
- occurrenceStatus
- eventDate
- decimalLatitude
- decimalLongitude

Additional terms we strongly recommend including are:

- Class Occurrence | DwC:[recordedBy](https://dwc.tdwg.org/terms/)
- Class Occurrence | DwC: [organismQuantity](https://dwc.tdwg.org/terms/) (different from categories I and II, see below)
- Class Occurrence | DwC: [organismQuantityType](https://dwc.tdwg.org/terms/) (different from categories I and II, see below)
- Class Event | DwC: [sampleSizeValue](https://dwc.tdwg.org/terms/) (different from categories I and II, see below)
- Class Event | DwC: [sampleSizeUnit](https://dwc.tdwg.org/terms/) (different from categories I and II, see below)
- Class Event | DwC: [samplingProtocol](https://dwc.tdwg.org/terms/)
- Class Material Sample | DwC:[materialSampleID](https://dwc.tdwg.org/terms/)

In the case of ddPCR, organismQuantity refers to the number of positive droplets/chambers in the sample, and organismQuantityType is the partition type (e.g., ddPCR droplets, dPCR chambers).

sampleSizeValue will be populated with the number of accepted partitions, e.g. meaning accepted droplets in ddPCR or chambers in dPCR. sampleSizeUnit is the partition type, which should be the same as organismQuantityType. All four of these fields are particularly important to include for ddPCR data.

In case of qPCR, these fields can be used for recording e.g. the number of copies that were calculated for the target gene in the sample. In this case organismQuantityType needs to contain the exact type of the measurement reported in the results. The field accepts any string, but the best practice would be to add a URI pointing to a vocabulary, as is done in the extendedMeasurementOrFact extension. The terms sampleSizeValue and sampleSizeUnit would not be used in this case.

materialSampleID should contain an identifier for the MaterialSample (i.e. occurrence record), rather than a digital record of the material sample. If an ID was obtained from a nucleotide archive, use the associated biosample ID. Otherwise, construct a persistent unique identifier from a combination of elements in the data that will make the materialSampleID globally unique, similar to how we learned to construct eventIDs and occurrenceIDs.

recordedBy can be populated with the names of the people, groups, or organizations responsible for recording the original Occurrence. You can use a concatenated list for multiple names, by separating values with a vertical bar (' | ').

Now let's move on to the DNA Derived data extension.

## Format DNA derived data extension for qPCR data

As we know, the DNADerivedData extension will capture important information related to the sampling, processing, and bioinformatic methods. For qPCR  datasets, it is strongly recommended to document as much detail as possible, particularly details about the PCR primers used and the target gene. We recommend you include the following terms, where relevant. For term descriptions see [DNA derived data extension](https://rs.gbif.org/extension/gbif/1.0/dna_derived_data_2022-02-23.xml). We have again bolded some terms of particular interest.

Terms related to the sampling event:

- DNA Derived | DwC: env_broad_scale (same as categories I and II)
- DNA Derived | DwC: env_local_scale (same as categories I and II)
- DNA Derived | DwC: env_medium
- DNA Derived | DwC: samp_collect_device
- DNA Derived | DwC: samp_mat_process
- DNA Derived | DwC: samp_size
- DNA Derived | DwC: size_frac

Terms related to DNA and PCR methods:

- DNA Derived | DwC: sop
- DNA Derived | DwC: concentration
- DNA Derived | DwC: concentrationUnit
- DNA Derived | DwC: methodDeterminationConcentrationAndRatios
- DNA Derived | DwC: contaminationAssessment
- DNA Derived | DwC: target_gene
- DNA Derived | DwC: target_subfragment
- DNA Derived | DwC: ampliconSize
- DNA Derived | DwC: amplificationReactionVolume
- DNA Derived | DwC: amplificationReactionVolumeUnit
- DNA Derived | DwC: baselineValue
- DNA Derived | DwC: automaticBaselineValue
- DNA Derived | DwC: automaticThresholdQuantificationCycle
- DNA Derived | DwC: thresholdQuantificationCycle
- DNA Derived | DwC: pcr_analysis_software
- DNA Derived | DwC: pcr_primer_forward
- DNA Derived | DwC: pcr_primer_reverse
- DNA Derived | DwC: pcr_primer_name_forward
- DNA Derived | DwC: pcr_primer_name_reverse
- DNA Derived | DwC: pcr_primer_reference
- DNA Derived | DwC: pcr_cond
- DNA Derived | DwC: pcr_primer_lod
- DNA Derived | DwC: pcr_primer_loq
- DNA Derived | DwC: annealingTemp
- DNA Derived | DwC: annealinTempUnit
- DNA Derived | DwC: probeQuencher
- DNA Derived | DwC: probeReporter
- DNA Derived | DwC: quantificationCycle
- DNA Derived | DwC: ratioOfAbsorbance260_230
- DNA Derived | DwC: ratioOfAbsorbance260_280

There are many specialized qPCR terms that are possible to add to the dataset.

The terms concentration, concentrationUnit, ratioOfAbsorbance260_230, ratioOfAbsorbance260_280, and methodDeterminationConcentrationAndRatios are related to the original DNA sample before qPCR analysis, and can be useful for evaluating the prevalence of the marker in the sample as well as the purity of the DNA for any indication of PCR inhibition.

As with the metabarcoding dataset, the details of the PCR conditions and primers can be recorded in the multiple pcr_ terms as well as target_ terms, and amplificationReactionVolume and amplificationReactionVolumeUnit.

The main terms that are important for the quantification information and are different from the metabarcoding dataset are baselineValue, thresholdQuantificationCycle and quantificationCycle. The terms pcr_primer_lod, pcr_primer_loq, probeQuencher, probeReporter are additional terms specific for qPCR assays. The baselineValue indicates the number of cycles below which the signal is considered only background noise. The quantificationCycle is the most important and indicates at which cycle the particular sample crossed the detection threshold, this will be different for each sample. It is recommended to record this information, but not all of this may be easily available.

## Lesson summary

In this lesson we learned important distinctions for how to prepare qPCR or ddPCR data for publishing to OBIS. Unfortunately, very few such data has been mobilized to OBIS thus far. If you have qPCR data you'd like to publish to OBIS, we would be happy to help answer additional questions not addressed in this course.

To conclude this module, we will look at an example eDNA dataset to see how it aligns to DwC according to the previously discussed guidelines.

# Lesson 5: Categories I and II example: eDNA

## Category I and II example dataset: eDNA

For this lesson we will demonstrate how to format data according to the standards discussed previously, focusing on data that falls into either Category I or II with real datasets. The dataset is [“18S Monterey Bay Time Series: an eDNA data set from Monterey Bay, California, including years 2006, 2013 - 2016’](https://ipt-obis.gbif.us/resource?r=18s_monterey_bay_time_series_edna). The data from this study originate from marine filtered seawater samples that have undergone metabarcoding of the 18S V9 region.

The formatting of this dataset is demonstrated in detail in the GitHub repository: [https://github.com/iobis/dataset-edna](https://github.com/iobis/dataset-edna). A selection of samples from this collection were included in the publication "[Environmental DNA reveals seasonal shifts and potential interactions in a marine community](https://www.nature.com/articles/s41467-019-14105-1)" which was published with open access in Nature Communications in 2020.

As we know, genetic datasets are usually constructed around an Occurrence core table, so let’s start with that.

## Format eDNA example: Occurrence core

The first step is to populate the Occurrence core with all the required and highly recommended fields, as well as considering the eDNA and DNA specific fields. The Occurrence core contains the taxonomic identification of each ASV (Amplicon Sequence Variant) observed.

We will document the number of reads, as well as relevant metadata including the sample collection location, references for the identification procedure, and links to archived sequences.

As we know from Module 3, occurrenceID and basisOfRecord are some of the required Occurrence core terms. The occurrenceID was built from elements of the data, and because we are dealing with DNA material, the basisOfRecord will be MaterialSample.

We additionally want to include the highly recommended fields organismQuantity, organismQuantityType, sampleSizeValue, and sampleSizeUnit. These fields contain the number of DNA sequence reads. In metabarcoding data, recall that the difference between organismQuantity and sampleSizeValue is that organismQuantity refers to the number of reads of the sequence in the sample. sampleSizeValue is the number of total reads in the sample. It is important to include both values because you can calculate the relative abundance of a sequence within the total sample.

You can see that a reference to the project and its sequences is provided in associatedSequences, making it easily accessible.

A selection of samples from this sample/plate were included in another publication (Djurhuus et al., 2020),which is recorded in identificationReferences along with the GitHub repository where the data can be found.

(Note: tables below are split for simplified viewing)

| occurrenceID | basisOfRecord | organismQuantity | OrganismQuantityType | associatedSequences |
| --- | --- | --- | --- | --- |
| 11216c01_12_edna_1_S_occ1 | MaterialSample | 19312 | DNA sequence reads | NCBI BioProject acc. nr. PRJNA433203 |
| 11216c01_12_edna_2_S_occ1 | MaterialSample | 16491 | DNA sequence reads | NCBI BioProject acc. nr. PRJNA433203 |
| 11216c01_12_edna_3_S_occ1 | MaterialSample | 21670 | DNA sequence reads | NCBI BioProject acc. nr. PRJNA433203 |

| sampleSizeValue | sampleSizeUnit | identificationReferences | identificationRemarks |
| --- | --- | --- | --- |
| 147220 | DNA sequence reads | GitHub repository [Djurhuus et al. 2020](https://doi.org/10.1038/s41467-019-14105-1) | Genbank nr Release 221 September 20 2017 |
| 121419 | DNA sequence reads | GitHub repository [Djurhuus et al. 2020](https://doi.org/10.1038/s41467-019-14105-1) | Genbank nr Release 221 September 20 2017 |
| 161525 | DNA sequence reads | GitHub repository [Djurhuus et al. 2020](https://doi.org/10.1038/s41467-019-14105-1) | Genbank nr Release 221 September 20 2017 |

| scientificName | scientificNameID | kingdom | phylum | class | order | family | genus |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Paracalanus | urn:lsid:marinespecies.org:taxname:104196 | Eukaryota | Arthropoda | Hexanauplia | Calanoida | Paracalanidae | Paracalanus |
| Paracalanus | urn:lsid:marinespecies.org:taxname:104196 | Eukaryota | Arthropoda | Hexanauplia | Calanoida | Paracalanidae | Paracalanus |
| Paracalanus | urn:lsid:marinespecies.org:taxname:104196 | Eukaryota | Arthropoda | Hexanauplia | Calanoida | Paracalanidae | Paracalanus |

Now let’s look at how to create the DNADerivedData extension for this dataset.

## Format eDNA example: DNADerivedData extension

Next, we can create the DNA Derived Data extension which will be connected to the Occurrence core with the use of occurrenceID. This extension contains the DNA sequences and relevant DNA metadata, including sequencing procedures, primers used and Standard Operating Procedures (SOPs).

We learned in Lesson 3 that env_broad_scale is populated with a biome term, and because we know samples from this dataset were taken from filtered seawater, we will choose [marine biome (ENVO:00000447) ](https://ontobee.org/ontology/ENVO?iri=http://purl.obolibrary.org/obo/ENVO_00000447)for this field. For env_local_scale, samples were collected at a nearshore station, indicating we will want to search for a term related to the coast. We strongly recommend searching by keywords rather than attempting to search through the drop down menus in the tree. Searching for the keyword “coast”, we see that the 7th result relates to coastal seawater, and variants thereof. This result is more specific than the first result, [coast (ENVO:01000687)](https://ontobee.org/ontology/ENVO?iri=http://purl.obolibrary.org/obo/ENVO_01000687), so we will select the term [coastal water (ENVO:00001250)](https://ontobee.org/ontology/ENVO?iri=http://purl.obolibrary.org/obo/ENVO_00002150) to populate env_local_scale.

The samples from this example dataset were collected by CTD rosette and filtered by a peristaltic pump system. Illumina MiSeq metabarcoding was applied for the target_gene 18S and the target_subfragment, V9 region. URLs are provided for all the protocols followed for nucleic acids extraction and amplification.

For a detailed description of the steps taken to process the data, including algorithms used, see the original publication. Adding Operational Taxonomic Unit (OTU) related data are highly recommended and should be as complete as possible.

The tables below show how to fill some of the recommended fields for the DNADerivedData extension (again split to make viewing easier).

| occurrenceID | env_broad_scale | env_local_scale | env_medium |
| --- | --- | --- | --- |
| 11216c01_12_edna_1_S_occ1 | marine biome (ENVO:00000447) | coastal water (ENVO:00001250) | waterborne particulate matter (ENVO:01000436) |
| 11216c01_12_edna_2_S_occ1 | marine biome (ENVO:00000447) | coastal water (ENVO:00001250) | waterborne particulate matter (ENVO:01000436) |
| 11216c01_12_edna_3_S_occ1 | marine biome (ENVO:00000447) | coastal water (ENVO:00001250) | waterborne particulate matter (ENVO:01000436) |

| samp_vol_we_dna_ext | nucl_acid_ext | nucl_acid_amp | lib_layout | target_gene |
| --- | --- | --- | --- | --- |
| 1000ml | [dx.doi.org/10.17504/protocols.io.xjufknw](https://www.protocols.io/view/mbari-environmental-dna-edna-extraction-using-qiag-bp2l647d5vqe/v1) | [dx.doi.org/10.17504/protocols.io.n2vdge6](https://www.protocols.io/view/environmental-dna-edna-18s-metabarcoding-illumina-6qpvrydzgmkn/v2) | paired | 18S |
| 1000ml | [dx.doi.org/10.17504/protocols.io.xjufknw](https://www.protocols.io/view/mbari-environmental-dna-edna-extraction-using-qiag-bp2l647d5vqe/v1) | [dx.doi.org/10.17504/protocols.io.n2vdge6](https://www.protocols.io/view/environmental-dna-edna-18s-metabarcoding-illumina-6qpvrydzgmkn/v2) | paired | 18S |
| 1000ml | [dx.doi.org/10.17504/protocols.io.xjufknw](https://www.protocols.io/view/mbari-environmental-dna-edna-extraction-using-qiag-bp2l647d5vqe/v1) | [dx.doi.org/10.17504/protocols.io.n2vdge6](https://www.protocols.io/view/environmental-dna-edna-18s-metabarcoding-illumina-6qpvrydzgmkn/v2) | paired | 18S |

| target_subfragment | seq_meth | otu_class_appr | otu_seq_comp_appr |
| --- | --- | --- | --- |
| V9 | Illumina MiSeq 2x250 | dada2;1.14.0;ASV | blast;2.9.0+;80% identity;e-value cutoff: x MEGAN6;6.18.5;bitscore: 100 :2% |
| V9 | Illumina MiSeq 2x250 | dada2;1.14.0;ASV | blast;2.9.0+;80% identity;e-value cutoff: x MEGAN6;6.18.5;bitscore: 100 :2% |
| V9 | Illumina MiSeq 2x250 | dada2;1.14.0;ASV | blast;2.9.0+;80% identity;e-value cutoff: x MEGAN6;6.18.5;bitscore: 100 :2% |

| otu_db | sop | DNA_sequence |
| --- | --- | --- |
| Genbank nr;221 | [dx.doi.org/10.17504/protocols.io.xjufknw](https://www.protocols.io/view/mbari-environmental-dna-edna-extraction-using-qiag-bp2l647d5vqe/v1) or GitHub repository | GCTACTACCGATT… |
| Genbank nr;221 | [dx.doi.org/10.17504/protocols.io.xjufknw](https://www.protocols.io/view/mbari-environmental-dna-edna-extraction-using-qiag-bp2l647d5vqe/v1) or GitHub repository | GCTACTACCGATT… |
| Genbank nr;221 | [dx.doi.org/10.17504/protocols.io.xjufknw](https://www.protocols.io/view/mbari-environmental-dna-edna-extraction-using-qiag-bp2l647d5vqe/v1) or GitHub repository | GCTACTACCGATT… |

| pcr_primer_forward | pcr_primer_reverse | pcr_primer_name_forward | pcr_primer_name_reverse | pcr_primer_reference |
| --- | --- | --- | --- | --- |
| GTACACACCGCCCGTC | TGATCCTTCTGCAGGTTCACCTAC | 1391f | EukBr | Amaral-Zettler et al. 2009 |
| GTACACACCGCCCGTC | TGATCCTTCTGCAGGTTCACCTAC | 1391f | EukBr | Amaral-Zettler et al. 2009 |
| GTACACACCGCCCGTC | TGATCCTTCTGCAGGTTCACCTAC | 1391f | EukBr | Amaral-Zettler et al. 2009 |

## Where to find information

Sometimes the information you’re looking for may not be simply presented in easy-to-find tables or figures. You may have to read associated published papers for methodology if you are not the data provider. As an example, here we have highlighted where some of this information can usually be found in a publication.

## Lesson summary

In this lesson we reviewed an example dataset and how it aligns to DwC so that it can be published to OBIS. We recognize the growing importance of making genetic data accessible and usable for a variety of purposes, thus this module has introduced and familiarized us with genetic data and how to format it so it is more interpretable and interoperable.

# End of Module

Note that OBIS has been developing a bioinformatics pipeline to facilitate publication of DNA data into OBIS. The pipeline was initially developed for the [PacMAN project (Pacific Islands Marine Bioinvasions Alert Network)](https://pacman.obis.org/). See the [OBIS Manual](https://manual.obis.org/dna_data.html) for more details on this pipeline, if interested.

Now you have completed Module 4! Please complete Exercise 4-1 and Quiz 4 before moving on to Module 5.

In the next module we will build on principals of interoperability and learn more details on how to find and select appropriate controlled vocabulary.
