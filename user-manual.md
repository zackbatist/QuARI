## About QuARI

This is a demo of the [Queryable Artifact Record Interface (QuARI)](https://github.com/zackbatist/QuARI), a database interface designed to enable both assemblage-level and artifact-level data entry. The database with which this demo interfaces is modeled on the schema for the Stelida Naxos Archaeological Project (SNAP). It incorporates fictionalized data on lithic assemblages from both survey and excavation contexts.

Lithic material from survey includes collections from _transect_, _grid_ and _grab samples_ resulting from three different sampling strategies; the first two are 1m<sup>2</sup> dogleash collections while the latter are isolated finds. Excavations are divided into _trenches_ that are excavated by _contexts_; lithic material is thus associated with individual contexts.

Lithics are classified by _blank_ and _modification_ and assigned to a chronological _period_ where possible in Level 2 analysis. In Level 3 analysis they are assigned individual _ArtifactID_ numbers, and additional information (_raw material_, _weathering_, _patination_ and _burned_) is recorded.

QuARI enables recording both assemblage and artifact data. In this demo you can add either Level 2 or Level 3 data (i.e., information about groups of artifacts or about individual artifacts), and the database will update accordingly.

**This live instance of QuARI is for demonstration purposes only. All new data will be wiped daily.**

## Instructions

### View records

1.  Enter search parameters using the dropdown menus on the main page. More than one value may be selected from each dropdown menu. Press 'Clear Inputs' to clear reset the selected values.
2.  Press the 'Find' button.
3.  All matching results from the Level 2 table will be populated under the Level 2 tab.
4.  Selecting rows in the Level 2 tab will populate the Level 3 tab accordingly.
5.  Pressing the 'Photos' or 'Illustrations' buttons within the Level 3 table will populate the Photos and Illustrations tabs with records corresponding with the selected artefact.

### Create new records

1.  Press the 'Create New' button on the main page. The Create New Records box will appear.
2.  Select the desired values using the dropdown menus.
3.  When all values are properly configured, Press the 'Create' button to create new records containing those values. Both Level 2 and Level 3 records are created.
4.  If records matching the new record values already exist in the database, you will be prompted to confirm the creation of additional records.

**Note:** The Quantity field must contain a positive integer value.

**Note:** To minimize risk of data loss, there is no facility for deleting records. We encourage bad records to be flagged for review and deletion by accessing the database using another client.

### Batch add Level 3 values

1.  Select rows from the Level 2 tab.
2.  Press the button labelled 'Add artefact-level data for items in selected rows'.
3.  In the screen that appears, select the Level 3 values you wish to add to the selected records.
4.  Once all desired values have been selected, press 'Apply update' to batch edit the Level 3 records and apply the changes.

**Note:** To safeguard data integrity, only records that do not yet have _any_ Level 3 values can be edited in this way.

### Edit individual Level 3 records

1.  In the Level 3 tab, double-click on a cell to render it editable.
2.  Changes are saved after moving the cursor out of the edited cell.

### Add photos and illustrations records

1.  In the Level 3 tab, press the 'Photos' or 'Illustrations' buttons, which will switch you over to the Photos or Illustrations tabs, respectively. Any existing photos or illustrations corresponding with the selected artifact will be populated within the Photos or Illustrations tab.
2.  Populate the text input fields with desired values, and press the 'Add new' button to write the new record to the database.
3.  Once these records are added, they may be edited like the Level 3 data.

**The code us under active development. See our [GitHub page](https://github.com/zackbatist/QuARI/) for details about our upcoming plans, to submit a bug report, to request additional features, or to ask a question!**

## Features on the horizon

- Barch add and edit photo and illustration records
- Allow edits to Level 2 values, individually and batched
- Add ability to add new values to the dropdown menus (i.e. period, blank, technique, modification, raw material, etc) on the fly
- Autocomplete or restrict input values when editing cells in the Level 3 table
