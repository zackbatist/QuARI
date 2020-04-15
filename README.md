# **F**lexible **Li**thic **R**ecording **T**ool [FLiRT]
FLiRT is a database interface designed to facilitate the irregular, iterative, and non-linear workflows that are commonly employed in lithic analysis. Specifically, it is designed to facilitate workflows that tack back and forth between recording assemblage-level and artefact-level data – often beginning with the former and subsequently adding the latter – and that may involve addition of particular kinds of data for limited subsets of artefacts.

This is accomplished by leveraging R’s flexible and powerful data wrangling capabilities and Shiny’s reactive programming model that facilitates dynamic transformation and re-rendering of data in the web browser. FLiRT is designed to communicate directly with a MariaDB/MySQL relational database hosted on an independent server in order to maintain consistency and integrity of data across tables, allow multi-user access and foster collaboration (especially long-distance collaboration when used as an interface for databases hosted on internet-connected servers), and produce data that are centralized and always up-to-date.

The continuously updated projection of the database seen in the user interface is a ‘total’, ‘complete’ or ‘ideal’ state, whereby there is a balanced isomorphism between all assemblage level and artifact level records. In such a state, each assemblage-level record (e.g., of an excavation context) exists alongside a series of corresponding artifact-level records whose quantity equals the value stored in the assemblage record’s ‘Quantity’ field. The total number of records in the artifact table equals the sum of all quantities in the assemblage table, and the quantities in the assemblage table equal the number of rows for each locus in the artifact table.

In cases where only assemblage-level data exist or are input, FLiRT uses ‘ghost’ records in the artifact-level table, which are not associated with any specific lithic, but exist nonetheless to satisfy the balance of the total state. That is, in keeping with database logic, no assemblage can exist without unique records for each of its constituent elements, even when that assemblage is entered into the database as an assemblage rather than as a series of elements. FLiRT solves this problem by generating the individual artifact records implied by any assemblage record, but in the absence of individually identified artifacts in each assemblage, that assemblage is simply assigned a range of artifact IDs that correspond to the quantity of artifacts recorded. When it is deemed necessary to give a specific lithic a unique identity – in order to record its unique characteristics and make it readily identifiable so that it can be recalled individually from a largely undifferentiated collection – an undesignated ghost record is given substance through the assignment of a particular ArtefactID to a particular artifact with particular characteristics; information about any characteristics assessed is recorded, and that ArtefactID is also written on the physical object to identify it.

Like Schrödinger’s cat, ghost records exist in a potential state until it is deemed necessary to ‘open the box’, thereby reifying the record as a representation of a particular object whose unique identity can be recognized with absolute certainty. These “Schrödinger’s lithics” make it possible to efficiently record both lithics that will never be individually examined in detail and particular identified lithics that have recorded characteristics.

FLiRT also allows users to see and update records pertaining to photographs and illustrations of various lithics; unique identifiers for each photo and drawing, as well as pertinent data (e.g. filename, attribution, notes, etc), can be created and associated with any given ArtefactID.


# Installation
## Shiny Server

## Running from within RStudio

## Establishing connection with a database
[keys.R explanation here]

# Database Schema
FLiRT is designed to operate alongside a particular database schema. The schema conforms to typical database structures implemented by archaeological projects, but the code can be easily modified to suit variations.

## Three-level lithic analysis

# Future work
- Better filtering of input fields
- Batch editing records with artefact-level data already assigned to them
- More detailed activity log entries
- Enable user to show/hide/rearrange columns
- More effective visual cues for when edits have been applied

