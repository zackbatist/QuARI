## About QuARI
QuARI is based on workflows experienced at the Stelida Naxos Archaeological Project (SNAP), which formed the basis of the [template database](https://github.com/zackbatist/QuARI/blob/master/QuARI-template.sql). However the app can be modified to suit alternative research protocols and database schemas.

By default, QuARI deals with lithic assemblages from both survey and excavation contexts. Lithic material from survey includes collections from _transect_, _grid_ and _grab samples_ resulting from three different sampling strategies; the first two are 1m<sup>2</sup> dogleash collections while the latter are isolated finds. Excavations are divided into _trenches_ that are excavated by _contexts_; lithic material is thus associated with individual contexts.

Lithics are classified by _blank_ and _modification_ and assigned to a chronological _period_ where possible in Level 2 analysis. In Level 3 analysis they are assigned individual _ArtifactID_ numbers, and additional information (_raw material_, _weathering_, _patination_ and _burned_) is recorded.

QuARI enables recording both assemblage and artifact data. In this demo you can add either Level 2 or Level 3 data (i.e., information about groups of artifacts or about individual artifacts), and the database will update accordingly.

## Instructions
### Find records
1. Enter search parameters using the dropdown menus on the main page. More than one value may be selected from each dropdown menu. Press 'Clear Inputs' to clear reset the selected values.
2. Press the 'Find' button.
3. All matching results from the Level 2 table will be populated under the Level 2 tab.
4. The Level 3 tab will be populated with records based on the selection of rows in the Level 2 tab.
5. Select the cell intersecting the Level 3 record and the Photos or Illustrations columns to populate the Photos and Illustrations tabs, respectively.

### Create new records
1. Press the Create New button on the main page. The Create New Records box will appear.
2. Select the desired values using the dropdown menus. Currently only one value from each dropdown may be used at a time.
3. When all values are properly configured, Press the 'Create' button to create new records containing those values.
4. If records matching the new record values already exist in the database, you will be prompted to confirm the creation of additional records.

**Note:** The Quantity field must contain a positive integer value.

### Batch add Level 3 values
1. Select rows from the Level 2 tab.
2. Press the button labelled 'Add artefact-level data for items in selected rows'.
3. In the screen that appears, select the Level 3 values you wish to add to the selected records.
4. Once all desired values have been selected, press 'Apply update' to batch edit the Level 3 records and apply the changes.

**Note:** Only records that have no Level 3 values can be edited in this way.

### Edit individual Level 3 data
1. In the Level 3 tab, double click on a cell to render it editable.
2. Changes are saved after moving the cursor out of the edited cell.

### Add photos and illustrations records
1. When viewing the photos and illustrations records for a particular artefact, populate the text input fields with desired values.
2. Press the 'Add new' button to write the new record to the database.

## Features on the horizon
- Batch add and edit photo and illustration records
- Allow edits to Level 2 values, individually and batched
- Add new values to the dropdown menus (i.e. period, blank, technique, modification, raw material, etc) values on the fly
- Add buttons to the Level 3 table that navigate to the Photos and Illustrations tabs
- Autocomplete or restrict input values when editing cells in the Level 3 table

