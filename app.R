#FLiRT - Version current 12/2020

#load necessary packages
library(renv)
library(shiny)
library(xtable)
library(DT)
library(dplyr)
library(reshape2)
library(DBI)
library(RMariaDB)
library(splitstackshape)
library(devtools)
library(pool)
library(purrr)
library(shinyjs)
library(stringr)
library(glue)


#get log-in info for database
source("keys.R")

#define pool handler by pool on global level
pool <- pool::dbPool(drv = dbDriver("MariaDB"),
                     dbname = dbnamex,
                     host = hostx,
                     port = portx,
                     user = userx,
                     password = passwordx)

onStop(function() {
    print("DB closed")
    poolClose(pool)
}) # important!

# This function will create the buttons for the datatable; they will be unique
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}


#read unmodified versions of allloci, blanks and modifications tables from db - specifically for the input selection (these will subsequently be dynamically linked to Level2, so that when new loci/blanks/modifications/periods/contexts/trenches are entered these tables update w/ the new options)
allloci <- dbReadTable(pool, 'allloci')
allloci
blanks <- dbReadTable(pool, 'blanks_excavation')
blanks
modifications <- dbReadTable(pool, 'modifications_excavation')
modifications <- unique(modifications$Modification)[c(11,1:10,12:26)] #re-order so that 'No modification' is first
periods <<- dbReadTable(pool, 'dating')
#periods <<- periods[c(6,1:5),]
activitylog <- dbReadTable(pool, 'activitylog')
activitylog
contexts <- dbReadTable(pool, 'contexts')
trenches <- contexts[2:5]
transectcollectionpoints <- dbReadTable(pool, 'transectcollectionpoints')
transects <- as.character(sort(as.numeric(unique(transectcollectionpoints$Transect))))
gridcollectionpoints <- dbReadTable(pool, 'gridcollectionpoints')
grids <- as.character(sort(unique(gridcollectionpoints$Grid)))
grabs <- dbReadTable(pool, 'grabsamples')
XFinds <- dbReadTable(pool, 'xfinds')

#function to select trench and return associated contexts
chooseTrench <- function(trench) {
    trenches[trenches$Trench == trench,]$Context
}
chooseTransect <- function(transect) {
    transectcollectionpoints[transectcollectionpoints$Transect == transect,]$CollectionPointID
}
chooseGrid <- function(grid) {
    gridcollectionpoints[gridcollectionpoints$Grid == grid,]$CollectionPointID
}


shinyApp(
    ui <- fluidPage(#theme = shinytheme("cerulean"),
        tags$head(tags$style(
            HTML("input[type='search']:disabled {visibility:hidden}")
        )),
        tabsetPanel(id = "OverallTabs", type = "tabs",
                    tabPanel("Find",
                             titlePanel("SNAP Lithics Processing"
                             ),
                             fluidRow(
                                 #these (below) only are submitted once 'query' button is clicked, but then update dynamically as filters are updated
                                 column(width = 1,
                                        selectizeInput("LocusType", "Locus Type", choices = c("Context","Transect","Grid","Grab"), multiple = FALSE, selected = "Context")),
                                 column(width = 2,
                                        selectizeInput("Unit", "Unit", choices = NULL)),
                                 column(width = 2,
                                        selectizeInput("Locus", "Locus", choices = NULL)),
                                 column(width = 2,
                                        selectizeInput("Period", "Period", choices = c("", periods$Period), multiple = TRUE, selected = "")),
                                 column(width = 2,
                                        selectizeInput("Blank", "Blank", choices = c("", blanks$Blank), multiple = TRUE, selected = "")),
                                 column(width = 2,
                                        selectizeInput("Modification", "Modification", choices = c("", modifications), multiple = TRUE, selected = ""))
                             ),
                             fluidRow(
                                 column(width = 8,
                                        actionButton("query", "Find"),
                                        actionButton("ClearInputs", "Clear Inputs"),
                                        actionButton("NewRecordButton", "Create New"),
                                        uiOutput("CopyToNewRecord")
                                 )
                             ),
                             fluidRow(
                                 br(),
                                 column(width = 8,
                                        wellPanel(htmlOutput("SummaryInfo"))
                                 )
                             ),
                             hr(),
                             tabsetPanel(id = "myTabs", type = "tabs",
                                         tabPanel("Level 2 Selection",
                                                  br(),
                                                  DT::dataTableOutput("Level2Table"),
                                                  actionButton("EditButton", "Add artefact-level data for items in selected rows")
                                         ),
                                         tabPanel("Level 3 Selection",
                                                  br(),
                                                  DT::dataTableOutput("Level3Table")
                                         ),
                                         tabPanel("Photos",
                                                  fluidRow(
                                                      br(),
                                                      column(width = 2,
                                                             textInput("newPhotoFilename", "Filename (incl. extension)")),
                                                      column(width = 2,
                                                             textInput("newPhotoPhotographer", "Photographer")),
                                                      column(width = 2,
                                                             textInput("newPhotoCamera", "Camera")),
                                                      column(width = 2,
                                                             dateInput("newPhotoDate", "Date")),
                                                      column(width = 2,
                                                             textInput("newPhotoNotes", "Notes"))
                                                  ),
                                                  fluidRow(
                                                      column(width = 2,
                                                             actionButton("newPhoto", "Add new"))),
                                                  hr(),
                                                  br(),
                                                  DT::dataTableOutput("PhotosTable")
                                         ),
                                         tabPanel("Illustrations",
                                                  fluidRow(
                                                      br(),
                                                      column(width = 2,
                                                             textInput("newIllustrationFilename", "Filename (incl. extension) of digital copy")),
                                                      column(width = 2,
                                                             textInput("newIllustrationIllustrator", "Illustrator")),
                                                      column(width = 2,
                                                             dateInput("newIllustrationDate", "Date illustrated")),
                                                      column(width = 2,
                                                             textInput("newIllustrationYear", "Year illustrated")),
                                                      column(width = 2,
                                                             textInput("newIllustrationNotes", "Notes"))
                                                  ),
                                                  fluidRow(
                                                      column(width = 2,
                                                             actionButton("newIllustration", "Add new"))
                                                  ),
                                                  hr(),
                                                  br(),
                                                  DT::dataTableOutput("IllustrationsTable")
                                         )
                             )
                    ),
                    tabPanel("Activity Log",
                             titlePanel("Activity Log"),
                             br(),
                             DT::dataTableOutput("ActivityLogDisplay")
                    )
        )
    ),
    
    server <- function(input, output, session){
        Level2 <- dbReadTable(pool, 'level2')
        output$NewLevel2Table <- DT::renderDataTable(
            datatable(Level2[,-1], rownames = FALSE))
        
        Level3 <- dbReadTable(pool, 'level3')
        output$NewLevel3Table <- DT::renderDataTable(
            datatable(Level3[,-1], rownames = FALSE))
        
        # Filtering units and loci based on prior selection
            observeEvent(input$LocusType, {
                if (input$LocusType == "Context") {
                    updateSelectizeInput(session, "Unit", label = "Trench", choices = as.character(unique(sort(trenches$Trench))), selected = NULL, server = FALSE)
                }
                if (input$LocusType == "Transect") {
                    updateSelectizeInput(session, "Unit", label = "Transect", choices = as.character(unique(sort(transectcollectionpoints$Transect))), selected = NULL, server = FALSE)
                }
                if (input$LocusType == "Grid") {
                    updateSelectizeInput(session, "Unit", label = "Grid", choices = as.character(unique(sort(gridcollectionpoints$Grid))), selected = NULL, server = FALSE)
                }
                if (input$LocusType == "Grab") {
                    updateSelectizeInput(session, "Unit", label = "", choices = c(""), selected = NULL, server = FALSE)
                }
            })
            
            
            observeEvent(input$Unit, {
                if (input$LocusType == "Context") {
                    LocusChoices <- chooseTrench(input$Unit)
                    updateSelectizeInput(session, "Locus", label = "Context", choices = as.character(unique(sort(LocusChoices))))
                }
                
                if (input$LocusType == "Transect") {
                    LocusChoices <- chooseTransect(input$Unit)
                    updateSelectizeInput(session, "Locus", label = "Transect Collection Point", choices = as.character(unique(sort(LocusChoices))))
                }
                if (input$LocusType == "Grid") {
                    LocusChoices <- chooseGrid(input$Unit)
                    updateSelectizeInput(session, "Locus", label = "Grid Collection Point", choices = as.character(unique(sort(LocusChoices))))
                }
                if (input$LocusType == "Grab") {
                    updateSelectizeInput(session, "Locus", label = "Grab", choices = as.character(unique(sort(grabs$CollectionPointID))), selected = NULL, server = FALSE)
                }
            })
        
        # function to be used below to filter for response to query
        QueryResults <- function(QueryInputs) {
            Level2 <- dbReadTable(pool, 'level2')
            filtered <- Level2
            #add filter for locus type (transect and trench) here
            ifelse(QueryInputs$Locus != "", 
                   filtered <- filtered %>% filter(Locus %in% QueryInputs$Locus), filtered)
            ifelse(QueryInputs$Blank != "",
                   filtered <- filtered %>% filter(Blank %in% QueryInputs$Blank), filtered)
            ifelse(QueryInputs$Modification != "",
                   filtered <- filtered %>% filter(Modification %in% QueryInputs$Modification), filtered)
            ifelse(QueryInputs$Period != "", 
                   filtered <- filtered %>% filter(Period %in% QueryInputs$Period), filtered)
            filtered
        }
        
        # filter the data tables based on user inputs
        observeEvent(input$query, {
            QueryTermLocus <- if(!is.null(input$Locus)) {input$Locus} else {""}
            QueryTermPeriod <- if(!is.null(input$Period)) {input$Period} else {""}
            QueryTermBlank <- if(!is.null(input$Blank)) {input$Blank} else {""}
            QueryTermModification <- if(!is.null(input$Modification)) {input$Modification} else {""}
            QueryInputs <<- list(Locus = QueryTermLocus, Blank = QueryTermBlank, Modification = QueryTermModification, Period = QueryTermPeriod)
            CurrentResults <<- QueryResults(QueryInputs)
            Level2 <- dbReadTable(pool, 'level2')
            EmptyDT <- filter(CurrentResults, Locus=="blah")
            
            if (nrow(CurrentResults) == nrow(Level2)) {
                output$Level2Table <- renderDataTable(datatable(EmptyDT))
                output$SummaryInfo <- renderText({
                    HTML(paste0("Identical to the complete set of Level 2 records"))
                })
            }
            
            if (nrow(CurrentResults) == 0) {
                output$SummaryInfo <- renderText({
                    HTML(paste0("No records found matching the criteria specified."))
                    })
                output$Level2Table <- renderDataTable(datatable(CurrentResults))
            }
            
            if (nrow(CurrentResults) > 0 & nrow(CurrentResults) != nrow(Level2)) {
                
                output$Level2Table <- DT::renderDataTable(
                    datatable(CurrentResults[,-1], escape = FALSE, rownames = FALSE, selection = list(mode = "multiple", target = "row"), editable = TRUE, options = list(autowidth = TRUE, searching = FALSE, columnDefs = list(list(targets=c(6), width='50'), list(targets=c(2,3,4,5), width='100')))))
                
                
                ## query should return summary info about the query results ##
                #Take object resulting from query (filtered table) and use colSums() on Quantity column to return total number of artifacts returned by query
                #Take locus IDs from filtered table and use those to filter Level3 table; with resulting object (filtered Level3 table), use summary() or whatever to count number of artifacts with associated Illustration IDs and/or associated Photo IDs to return number of artifacts from query that have been drawn and/or photo'd
                CurrentResults$Locus
                Level3 <- dbReadTable(pool, 'level3')
                Level3Summary <- filter(Level3, Locus %in% CurrentResults$Locus)
                IllustrationSummary <- sum(!is.na(Level3Summary$Illustration) & !is.null(Level3Summary$Illustration) & Level3Summary$Illustration != "")
                PhotoSummary <- sum(!is.na(Level3Summary$Photos))  
                output$SummaryInfo <- renderText({
                    HTML(paste0("Total artifacts:<b>" , sum(CurrentResults$Quantity), "</b>Total drawn:<b>", IllustrationSummary, "</b>Total photographed:<b>", PhotoSummary,"</b>", sep=" "))
                })
                
                
                observe({
                    SelectedRowEdit <<- input$Level2Table_rows_selected
                })
                
                to_index <<- CurrentResults
            }
        })
        #-----/QueryLookup-----#
        
        #-----TableFilters-----#
        # Select a row or cell to generate new table with rows that are to be filled on
        # When a Level 2 row is selected, generate Level3FilterResults; then check to see if nrow(Level3FilterResults) is equal to the quantity in the selected Level 2 row. If it is, no further action needed (Level 3 table generated anyway).  If the number of existing Level 3 records is less than the quantity in the selected Level 2 row, generate Level 3 records to make up the difference.  Compare sel$Quantity to nrow(Level3FilterResults). If nrow(Level3FilterResults) < sel$Quantity , calculate difference, and then get updated Level3 table and generate next n records, filling them with values from sel. For each selected row in sel, go through this process separately (in for loop?).
        
        observe({
            sel <- input$Level2Table_rows_selected
            
            if (length(sel)) {  #alternative that allows selection of multiple rows
                Level2IndexValues <- sel
                Level3Selection <<- to_index[Level2IndexValues, c(2,4,5,6,7,3)]
                Level3 <- dbReadTable(pool, 'level3')
                
                Level3FilterResults <<- filter(Level3, Locus %in% Level3Selection[,1] & Period %in% Level3Selection[,2] & Blank %in% Level3Selection[,3] & Modification %in% Level3Selection[,4])
                output$Level3Table <- DT::renderDataTable(
                    datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,5)))))
                
                if (nrow(Level3FilterResults) < sum(Level3Selection$Quantity)) {
                    j <- 1
                    for (j in (1:nrow(Level3Selection))) {
                        ArtefactIDQuantityDifference <<- Level3Selection$Quantity[j] - nrow(Level3FilterResults[Level3FilterResults$Locus == Level3Selection$Locus[j] & Level3FilterResults$Blank == Level3Selection$Blank[j] & Level3FilterResults$Modification == Level3Selection$Modification[j] & Level3FilterResults$Period == Level3Selection$Period[j],])
                        NewArtefactRecordsFromDifference <<- data.frame(LocusType = as.character(), Locus = as.character(), Period = as.character(), Blank = as.character(), Modification = as.character(), ArtefactID = as.character(), stringsAsFactors = FALSE) 
                        getnewARid <- glue::glue_sql("SELECT MAX(ArtefactID) FROM `level3`", .con = pool)
                        latestARid <- as.character(dbGetQuery(pool, getnewARid))
                        latestARidnum <- as.numeric((str_extract(latestARid, "[0-9]+")))
                        newARid <- latestARidnum + 1
                        #generate first one, then increment up from there to ArtefactIDQuantityDifference
                        ARidseq <- seq(newARid, length.out=ArtefactIDQuantityDifference, by=1)
                        ARidstring <<- as.character(paste0("AR", str_pad(ARidseq, 6, pad="0"))) 
                        
                        l<-1
                        for (l in (1:ArtefactIDQuantityDifference)) {
                            NewArtefactRecordsFromDifference[l,] <- cbind(Level3Selection %>% filter(Locus %in% Level3Selection$Locus[j] & Blank %in% Level3Selection$Blank[j] & Modification %in% Level3Selection$Modification[j] & Period %in% Level3Selection$Period[j]) %>% select(LocusType, Locus, Period, Blank, Modification), data.frame(ArtefactID = ""), stringsAsFactors = F) #need to add a placeholder column for artefact ID  
                            NewArtefactRecordsFromDifference[l,]$ArtefactID <- ARidstring[l]
                        }
                    }
                    
                    write_level3FromDifference <<- glue::glue_sql("INSERT INTO `level3` (`ArtefactID`,`LocusType`, `Locus`, `Period`, `Blank`, `Modification`) VALUES ({NewArtefactRecordsFromDifference$ArtefactID},{NewArtefactRecordsFromDifference$LocusType}, {NewArtefactRecordsFromDifference$Locus}, {NewArtefactRecordsFromDifference$Period}, {NewArtefactRecordsFromDifference$Blank}, {NewArtefactRecordsFromDifference$Modification})", .con = pool)
                    k <- 1
                    for (k in (1:length(write_level3FromDifference))) {
                        dbExecute(pool, sqlInterpolate(ANSI(), write_level3FromDifference[k]))
                    }
                    
                    Level3 <- dbReadTable(pool, 'level3')
                    Level3FilterResults <<- filter(Level3, Locus %in% Level3Selection[,1] & Period %in% Level3Selection[,2] & Blank %in% Level3Selection[,3] & Modification %in% Level3Selection[,4])
                    
                    output$Level3Table <- DT::renderDataTable(
                        datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,5)))))
                }
                else {
                    output$Level3Table <- DT::renderDataTable(
                        datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,5)))))
                }
                
                
                # If the 'Photo' or 'Illustration' cell is selected in the Level 3 table, that generates a query about any associated photos or illustrations.  That query returns a table of existing photos/illustrations, into which new photos/illustrations can be entered if necessary. If new ones are entered, check db to generate next photo/illustration ID and assign that to the one(s) entered.        
                
                
                observe({
                    SelectedCells <- input$Level3Table_cells_selected   #col 7 comes from selection of illustration, 8 from photo
                    if (length(SelectedCells)) {
                        Level3 <- dbReadTable(pool, 'level3')
                        Level3IndexValues <<- ifelse(SelectedCells[1,2] %in% c(7,8), SelectedCells, c(0,0))
                        Photos <- dbReadTable(pool, 'photos')
                        Illustrations <- dbReadTable(pool, 'illustrations')
                        PhotosFilterResults <<- filter(Photos, ArtefactID=="None")
                        IllustrationsFilterResults <<- filter(Illustrations, ArtefactID=="None") 
                        
                        if (Level3IndexValues[1] == 0){
                            Photos <- dbReadTable(pool, 'photos')
                            output$PhotosTable <- DT::renderDataTable(
                                datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row")))
                            
                            Illustrations <- dbReadTable(pool, 'illustrations')
                            output$IllustrationsTable <- DT::renderDataTable(
                                datatable(IllustrationsFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row")))
                        }
                        else {
                            PhotosSelection <<- unlist(Level3FilterResults[Level3IndexValues[1], 7])
                            PhotosSelection_str <<- as.character(PhotosSelection)
                            Photos <- dbReadTable(pool, 'photos')
                            PhotosFilterResults <<- filter(Photos, ArtefactID==PhotosSelection)
                            output$PhotosTable <- DT::renderDataTable(
                                datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row"), editable = TRUE))
                            
                            IllustrationsSelection <<- unlist(Level3FilterResults[Level3IndexValues[1], 7])
                            IllustrationsSelection_str <<- as.character(IllustrationsSelection)
                            Illustrations <- dbReadTable(pool, 'illustrations')
                            IllustrationsFilterResults <<- filter(Illustrations, ArtefactID==IllustrationsSelection)
                            output$IllustrationsTable <- DT::renderDataTable(
                                datatable(IllustrationsFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row"), editable = TRUE))
                        }
                        
                    }
                })
                
            }
            else {
                Level3 <- dbReadTable(pool, 'level3')
                Level3FilterResults <- filter(Level3, Blank=="None")
                output$Level3Table <- DT::renderDataTable(
                    datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,5)))))
                
                Photos <- dbReadTable(pool, 'photos')
                PhotosFilterResults <- filter(Photos, ArtefactID=="None")
                output$PhotosTable <- DT::renderDataTable(
                    datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row")))
                
                Illustrations <- dbReadTable(pool, 'illustrations')
                IllustrationsFilterResults <- filter(Illustrations, ArtefactID=="None")
                output$IllustrationsTable <- DT::renderDataTable(
                    datatable(IllustrationsFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row")))
            }
        })
        
        observeEvent(input$newPhoto, once=F, {
            newPhotoResponses <- reactiveValues(
                NewPhotoFilename = input$newPhotoFilename,
                NewPhotoPhotographer = input$newPhotoPhotographer,
                NewPhotoCamera = input$newPhotoCamera,
                NewPhotoDate = input$newPhotoDate,
                NewPhotoNotes = input$newPhotoNotes
            )
            NewPhotoFilenameValue <- as.character(newPhotoResponses$NewPhotoFilename)
            NewPhotoPhotographerValue <- as.character(newPhotoResponses$NewPhotoPhotographer)
            NewPhotoCameraValue <- as.character(newPhotoResponses$NewPhotoCamera)
            NewPhotoDateValue <- as.character(newPhotoResponses$NewPhotoDate)
            NewPhotoNotesValue <- as.character(newPhotoResponses$NewPhotoNotes)
            Photos <- dbReadTable(pool, 'photos')
            PhotoNumbers <- as.character(Photos$PhotoID)
            PhotoNumbers_int <- as.numeric(gsub("([[:alpha:]])", "", PhotoNumbers))
            HightestPH <- max(PhotoNumbers_int)
            NewPH <-  paste0("PH", HightestPH + 1)
            if (str_length(NewPH) < 7) {
                ExtraZeroesPH_quant <- 7 - str_length(NewPH)
                ExtraZeroesPH <- str_dup("0", ExtraZeroesPH_quant)
                FixedPrefixPH <- paste0("PH", ExtraZeroesPH)
                NewPH <- gsub("(^..)", FixedPrefixPH, NewPH)
            }
            
            LocusForNewPhoto <- unlist(Level3FilterResults[Level3IndexValues[1], 2])
            LocusTypeForNewPhoto <- unlist(Level3FilterResults[Level3IndexValues[1], 3])
            newPhotoInsert <<- glue::glue_sql("INSERT INTO `photos` (`Filename`, `Photographer`, `Camera`, `Date`, `Notes`, `ArtefactID`, `PhotoID`, `Locus`, `LocusType`) VALUES ({NewPhotoFilenameValue}, {NewPhotoPhotographerValue}, {NewPhotoCameraValue}, {NewPhotoDateValue}, {NewPhotoNotesValue}, {PhotosSelection_str}, {NewPH}, {LocusForNewPhoto}, {LocusTypeForNewPhoto})", .con = pool)
            dbExecute(pool, sqlInterpolate(ANSI(), newPhotoInsert))
            
            Photos <- dbReadTable(pool, 'photos')
            PhotosFilterResults <<- filter(Photos, ArtefactID==PhotosSelection_str)
            output$PhotosTable <<- DT::renderDataTable(
                datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row"), editable = TRUE))
            updateTextInput(session, "newPhotoFilename", value = "")
            Level3 <<- dbReadTable(pool, 'level3')
            Level3Photos <<- filter(Photos, ArtefactID==PhotosSelection)$PhotoID
            Level3Photos_str <<- as.character(paste(Level3Photos, collapse =  "; "))
            Level3PhotoUpdate <<- glue::glue_sql("UPDATE `level3` SET `Photos` = {Level3Photos_str} WHERE `ArtefactID` = {PhotosSelection}", .con = pool)
            dbExecute(pool, sqlInterpolate(ANSI(), Level3PhotoUpdate))
            
            # For some unknown reason, refreshing this table does not include PhotoIDs that do not contain non-number characters. For instance, if I add a photo with filename "1234", this will be added to the database in all its proper places, but this value will not be pulled down or displayed here. Better to use '1234.jpeg' or something of that sort (but not adding the '.jpeg' part automatically since it is not reasonable to assume that this is how files will be formatted (i.e. the distinction between '.jpeg' and '.jpg')).
            # The Photos column in the Level3 data table is too narrow to show more than one or two PhotoIDs.
            Level3FilterResults <<- filter(Level3, Locus %in% Level3Selection[,1] & Period %in% Level3Selection[,2] & Blank %in% Level3Selection[,3] & Modification %in% Level3Selection[,4])
            output$Level3Table <- DT::renderDataTable(
                datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,5)))))
            
        })
        
        observeEvent(input$newIllustration, once=F, {
            newIllustrationResponses <<- reactiveValues(
                NewIllustrationFilename = input$newIllustrationFilename,
                NewIllustrationIllustrator = input$newIllustrationIllustrator,
                NewIllustrationDate = input$newIllustrationDate,
                NewIllustrationYear = input$newIllustrationYear,
                NewIllustrationNotes = input$newIllustrationNotes
            )
            NewIllustrationFilenameValue <<- as.character(newIllustrationResponses$NewIllustrationFilename)
            NewIllustrationIllustratorValue <<- as.character(newIllustrationResponses$NewIllustrationIllustrator)
            NewIllustrationDateValue <<- as.character(newIllustrationResponses$NewIllustrationDate)
            NewIllustrationYearValue <<- as.character(newIllustrationResponses$NewIllustrationYear)
            NewIllustrationNotesValue <<- as.character(newIllustrationResponses$NewIllustrationNotes)
            Illustrations <- dbReadTable(pool, 'illustrations')
            IllustrationNumbers <<- as.character(Illustrations$IllustrationID)
            IllustrationNumbers_int <<- as.numeric(gsub("([[:alpha:]])", "", IllustrationNumbers))
            HightestDR <<- max(IllustrationNumbers_int)
            NewDR <<-  paste0("DR", HightestDR + 1)
            if (str_length(NewDR) < 7) {
                ExtraZeroesDR_quant <- 7 - str_length(NewDR)
                ExtraZeroesDR <- str_dup("0", ExtraZeroesDR_quant)
                FixedPrefixDR <- paste0("DR", ExtraZeroesDR)
                NewDR <- gsub("(^..)", FixedPrefixDR, NewDR)
            }
            
            LocusForNewIllustration <<- unlist(Level3FilterResults[Level3IndexValues[1], 2])
            LocusTypeForNewIllustration <<- unlist(Level3FilterResults[Level3IndexValues[1], 3])
            newIllustrationInsert <<- glue::glue_sql("INSERT INTO `illustrations` (`Filename`, `Illustrator`, `Date`, `Year`, `Notes`, `ArtefactID`, `IllustrationID`, `Locus`, `LocusType`) VALUES ({NewIllustrationFilenameValue}, {NewIllustrationIllustratorValue}, {NewIllustrationDateValue}, {NewIllustrationYearValue}, {NewIllustrationNotesValue}, {IllustrationsSelection_str}, {NewDR}, {LocusForNewIllustration}, {LocusTypeForNewIllustration})", .con = pool)
            dbExecute(pool, sqlInterpolate(ANSI(), newIllustrationInsert))
            
            Illustrations <- dbReadTable(pool, 'illustrations')
            IllustrationsFilterResults <<- filter(Illustrations, ArtefactID==IllustrationsSelection_str)
            output$IllustrationsTable <<- DT::renderDataTable(
                datatable(IllustrationsFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row"), editable = TRUE))
            updateTextInput(session, "newIllustrationFilename", value = "")
            Level3 <<- dbReadTable(pool, 'level3')
            Level3Illustrations <<- filter(Illustrations, ArtefactID==IllustrationsSelection)$IllustrationID
            Level3Illustrations_str <<- as.character(paste(Level3Illustrations, collapse =  "; "))
            Level3IllustrationsUpdate <<- glue::glue_sql("UPDATE `level3` SET `Illustrations` = {Level3Illustrations_str} WHERE `ArtefactID` = {IllustrationsSelection}", .con = pool)
            dbExecute(pool, sqlInterpolate(ANSI(), Level3IllustrationsUpdate))

                        
            Level3FilterResults <<- filter(Level3, Locus %in% Level3Selection[,1] & Period %in% Level3Selection[,2] & Blank %in% Level3Selection[,3] & Modification %in% Level3Selection[,4])
            output$Level3Table <- DT::renderDataTable(
                datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = list(target = "cell", disable = list(columns = c(0,1,2,3,4,5)))))
            
        })
        
        
        observeEvent(input$Level3Table_cell_edit, { # For some unknown reason input$Level3Table_cell_edit does not update after an initial update. So when I update a cell from A to B and then from B to C, it only recognizes an update from A to C as if the first update never occurred.
            CellsToEdit <<- input$Level3Table_cell_edit
            if (length(CellsToEdit)) {
                ARtoEdit <<- Level3FilterResults[CellsToEdit$row, 7]
                ColtoEdit <<- colnames(Level3FilterResults)[CellsToEdit$col+2]
                ValueToUpdate <<- Level3[Level3$ArtefactID == ARtoEdit, ColtoEdit]
                ValueToUpdate <<- ValueToUpdate[1] # For some unknown reason ValueToUpdate comprises 7 values, so this is necessary to truncate the list.
                UpdatedValue <<- CellsToEdit$value
                EditedCell <<- glue::glue_sql("UPDATE `level3` SET {`ColtoEdit`} = {UpdatedValue} WHERE `ArtefactID` = {ARtoEdit}", .con = pool)
                dbExecute(pool, sqlInterpolate(ANSI(), EditedCell))
                
                message4 <- paste0("Record for ",ARtoEdit," was updated. ",ColtoEdit," was changed from ",ValueToUpdate," to ",UpdatedValue,".")
                activitylog <- dbReadTable(pool, 'activitylog')
                activitylog <- data.frame(Log = message4,
                                          Timestamp = as.character(Sys.time()),
                                          stringsAsFactors = FALSE)
                writeActivity <- dbWriteTable(pool, 'activitylog', activitylog, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
                writeActivity
                activitylog <<- dbReadTable(pool, 'activitylog')
                activitylog
                
                output$SummaryInfo <- renderText({
                    HTML(paste0("Record for <b>",ARtoEdit,"</b> was updated. <b>",ColtoEdit,"</b> was changed from <b>",ValueToUpdate,"</b> to <b>",UpdatedValue,"</b>."))
                })
                
                Level3 <- dbReadTable(pool, 'level3')
                Level3FilterResults <<- filter(Level3, Locus %in% Level3Selection[,1] & Period %in% Level3Selection[,2] & Blank %in% Level3Selection[,3] & Modification %in% Level3Selection[,4])
                output$Level3Table <- DT::renderDataTable(
                    datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,5)))))
                
            }
        })
        #-----/TableFilters-----#
        
        
        #-----NewRecords-----#
        observeEvent(input$NewRecordButton, {
            showModal(modalDialog(title = paste0("Create New Records"),
                                  fluidRow(
                                      column(width = 2,
                                             selectInput("NewLocusType", "Locus Type", choices = c("Context","Transect","Grid","Grab"), multiple = FALSE, selected = NULL)),
                                      column(width = 2,
                                             selectizeInput("NewLocus", "Locus", choices = c("", allloci$Locus), multiple = FALSE, selected = "", options=list(create=TRUE))),
                                      column(width = 2,
                                             selectizeInput("NewPeriod", "Period", choices = c("", periods$Period), multiple = FALSE, selected = "")),
                                      column(width = 2,
                                             selectizeInput("NewBlank", "Blank", choices = c("", blanks$Blank), multiple = FALSE, selected = "", options = list(create = TRUE))),
                                      column(width = 2,
                                             selectizeInput("NewModification", "Modification", choices = c("", modifications), multiple = FALSE, selected = "", options = list(create = TRUE))),
                                      column(width = 1,
                                             numericInput("NewQuantity", "Quantity", "1"))
                                  ),
                                  fluidRow(
                                      column(width = 2,
                                             selectInput("NewRawMaterial","Raw Material", c(Choose = '', 'Type A', 'Type B', 'Type C', 'Type D', 'Type E', 'Type F', 'Indeterminate', 'Missing'), selectize = TRUE)),
                                      column(width = 2,
                                             selectInput("NewWeathering","Weathering", c(Choose = '', "1", "2", "3", "4", "5"), selectize = TRUE)),
                                      column(width = 2,
                                             selectInput("NewPatination","Patination", c(Choose = '', "Patinated"), selectize = TRUE)),
                                      column(width = 2,
                                             selectInput("NewBurned","Burned", c(Choose = '', "Burned"), selectize = TRUE))
                                  ),
                                  fluidRow(
                                      column(width = 3,
                                             actionButton("CreateNewRecord", "Create"),
                                             ##actionButton("toggleNewBlankMod", "New Blank or Modification"),
                                             actionButton("ClearNewInputs", "Clear Inputs")
                                      ),
                                      column(width = 7,
                                             wellPanel(htmlOutput("NewRecordMessages"))
                                      ),
                                      column(width = 2,
                                             uiOutput("ConfirmNewRecord")
                                      )
                                  ),
                                  hr(),
                                 DT::dataTableOutput("ShowNewRecords"),
                                  footer = modalButton("Dismiss"),
                                  size = "l",
                                  easyClose = TRUE,
                                  fade = FALSE
            )
            )
        })
        
        observeEvent(input$CreateNewRecord, {
            Level3 <- dbReadTable(pool, 'level3')
            EquivRecordFilter <- data.frame()
            EquivRecordFilter <<- reactive({
                select(filter(Level3, LocusType==input$NewLocusType & Locus==input$NewLocus & Period==input$NewPeriod & Blank==input$NewBlank & Modification==input$NewModification & RawMaterial==input$NewRawMaterial & Weathering==input$NewWeathering & Patination==input$NewPatination & Burned==input$NewBurned), LocusType, Locus, Period, Blank, Modification, RawMaterial, Weathering, Patination, Burned)
            })
            EquivRecordFilter_df <<- EquivRecordFilter()
            
            if (nrow(EquivRecordFilter_df) == 0) {
                values <- reactiveValues(singleResponse_df = data.frame(input$NewLocusType, input$NewLocus, input$NewPeriod, input$NewBlank, input$NewModification, input$NewRawMaterial, input$NewWeathering, input$NewPatination, input$NewBurned, input$NewQuantity))
                
                colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewLocusType'] <- 'LocusType'
                colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewLocus'] <- 'Locus'
                colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewPeriod'] <- 'Period'
                colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewBlank'] <- 'Blank'
                colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewModification'] <- 'Modification'
                colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewRawMaterial'] <- 'RawMaterial'
                colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewWeathering'] <- 'Weathering'
                colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewPatination'] <- 'Patination'
                colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewBurned'] <- 'Burned'
                colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewQuantity'] <- 'Quantity'
                
                NewRecord <<- as.data.frame(values$singleResponse_df, stringsAsFactors = FALSE)
                NewRecord$Locus <- as.character(NewRecord$Locus)
                NewRecord$LocusType <- as.character(NewRecord$LocusType)
                NewRecord$Period <- as.character(NewRecord$Period)
                NewRecord$Blank <- as.character(NewRecord$Blank)
                NewRecord$Modification <- as.character(NewRecord$Modification)
                NewRecord$RawMaterial <- as.character(NewRecord$RawMaterial)
                NewRecord$Weathering <- as.character(NewRecord$Weathering)
                NewRecord$Patination <- as.character(NewRecord$Patination)
                NewRecord$Burned <- as.character(NewRecord$Burned)
                NewRecord$Quantity <- as.numeric(NewRecord$Quantity)
                
                WriteNewLevel2Record <- glue::glue_sql("INSERT INTO `level2` (`Locus`, `LocusType`, `Period`, `Blank`, `Modification`, `Quantity`) VALUES ({NewRecord$Locus}, {NewRecord$LocusType}, {NewRecord$Period}, {NewRecord$Blank}, {NewRecord$Modification}, {NewRecord$Quantity})", .con = pool)
                WriteNewLevel2Record <<- as.character(WriteNewLevel2Record)
                WriteNewLevel2Record
                dbExecute(pool, sqlInterpolate(ANSI(), WriteNewLevel2Record))
                
                ValuesToExpand <<- reactiveValues(singleResponse_df = data.frame(input$NewLocusType, input$NewLocus, input$NewPeriod, input$NewBlank, input$NewModification, input$NewRawMaterial, input$NewWeathering, input$NewPatination, input$NewBurned, input$NewQuantity))
                toExpand <<- as.data.frame(ValuesToExpand$singleResponse_df)
                singleRow_expanded <<- expandRows(toExpand, count = 10, count.is.col = TRUE, drop = TRUE)
                singleRow_expanded <<- data.frame(lapply(singleRow_expanded, as.character), stringsAsFactors = FALSE)
                colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocus'] <- 'Locus'
                colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocusType'] <- 'LocusType'
                colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewPeriod'] <- 'Period'
                colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewBlank'] <- 'Blank'
                colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewModification'] <- 'Modification'
                colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewRawMaterial'] <- 'RawMaterial'
                colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewWeathering'] <- 'Weathering'
                colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewPatination'] <- 'Patination'
                colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewBurned'] <- 'Burned'
                
                singleRow_expanded <<- singleRow_expanded
                singleRow_expanded$Locus <- as.character(singleRow_expanded$Locus)
                singleRow_expanded$LocusType <- as.character(singleRow_expanded$LocusType)
                singleRow_expanded$Period <- as.character(singleRow_expanded$Period)
                singleRow_expanded$Blank <- as.character(singleRow_expanded$Blank)
                singleRow_expanded$Modification <- as.character(singleRow_expanded$Modification)
                singleRow_expanded$RawMaterial <- as.character(singleRow_expanded$RawMaterial)
                singleRow_expanded$Weathering <- as.character(singleRow_expanded$Weathering)
                singleRow_expanded$Patination <- as.character(singleRow_expanded$Patination)
                singleRow_expanded$Burned <- as.character(singleRow_expanded$Burned)
                
                for (j in (1:nrow(singleRow_expanded))) {
                    ArtefactIDQuantityDifferenceNewRecords <<- nrow(singleRow_expanded)
                    NewArtefactRecordsFromDifferenceNewRecords <<- data.frame(LocusType = as.character(), Locus = as.character(), Period = as.character(), Blank = as.character(), Modification = as.character(), RawMaterial = as.character(), Weathering = as.character(), Patination = as.character(), Burned = as.character(), ArtefactID = as.character(), stringsAsFactors = FALSE)
                    getnewARid <- glue::glue_sql("SELECT MAX(ArtefactID) FROM `level3`", .con = pool)
                    latestARid <- as.character(dbGetQuery(pool, getnewARid))
                    latestARidnum <- as.numeric((str_extract(latestARid, "[0-9]+")))
                    newARid <- latestARidnum + 1
                    #generate first one, then increment up from there to ArtefactIDQuantityDifference
                    ARidseq <- seq(newARid, length.out=ArtefactIDQuantityDifferenceNewRecords, by=1)
                    ARidstring <<- as.character(paste0("AR", str_pad(ARidseq, 6, pad="0"))) 
                    
                    l<-1
                    for (l in (1:ArtefactIDQuantityDifferenceNewRecords)) {
                        NewArtefactRecordsFromDifferenceNewRecords[l,] <- cbind(singleRow_expanded, data.frame(ArtefactID = ""), stringsAsFactors = F) #need to add a placeholder column for artefact ID
                        NewArtefactRecordsFromDifferenceNewRecords[l,]$ArtefactID <- ARidstring[l]
                    }
                }
                
                write_level3 <<- glue::glue_sql("INSERT INTO `level3` (`Locus`, `LocusType`, `Period`, `Blank`, `Modification`, `RawMaterial`, `Weathering`, `Patination`, `Burned`, `ArtefactID`) VALUES ({NewArtefactRecordsFromDifferenceNewRecords$Locus}, {NewArtefactRecordsFromDifferenceNewRecords$LocusType}, {NewArtefactRecordsFromDifferenceNewRecords$Period}, {NewArtefactRecordsFromDifferenceNewRecords$Blank}, {NewArtefactRecordsFromDifferenceNewRecords$Modification}, {NewArtefactRecordsFromDifferenceNewRecords$RawMaterial}, {NewArtefactRecordsFromDifferenceNewRecords$Weathering}, {NewArtefactRecordsFromDifferenceNewRecords$Patination}, {NewArtefactRecordsFromDifferenceNewRecords$Burned}, {NewArtefactRecordsFromDifferenceNewRecords$ArtefactID})", .con = pool)
                m <- 1
                for (m in (1:length(write_level3))) {
                    dbExecute(pool, write_level3[m])
                }
                
                Level3 <- dbReadTable(pool, 'level3')
                NewRecordsTableX <- Level3 %>%
                    select(ArtefactID, Locus, Period, Blank, Modification, RawMaterial, Weathering, Patination, Burned) %>%
                    filter(ArtefactID %in% ARidstring)
                output$ShowNewRecords <- DT::renderDataTable(datatable(NewRecordsTableX))
                
                # refresh the interface
                NewTermLocus <- if(!is.null(input$NewLocus)) {input$NewLocus} else {""}
                NewTermPeriod <- if(!is.null(input$NewPeriod)) {input$NewPeriod} else {""}
                NewTermBlank <- if(!is.null(input$NewBlank)) {input$NewBlank} else {""}
                NewTermModification <- if(!is.null(input$NewModification)) {input$NewModification} else {""}
                NewTermRawMaterial <- if(!is.null(input$NewRawMaterial)) {input$NewRawMaterial} else {""}
                NewTermWeathering <- if(!is.null(input$NewWeathering)) {input$NewWeathering} else {""}
                NewTermPatination <- if(!is.null(input$NewPatination)) {input$NewPatination} else {""}
                NewTermBurned <- if(!is.null(input$NewBurned)) {input$NewBurned} else {""}
                NewInputs <<- list(Locus = NewTermLocus, Period = NewTermPeriod, Blank = NewTermBlank, Modification = NewTermModification, RawMaterial = NewTermRawMaterial, Weathering = NewTermWeathering, Patination = NewTermPatination, Burned = NewTermBurned)
                
                CurrentResults <<- QueryResults(NewInputs)
                Level2 <- dbReadTable(pool, 'level2')
                EmptyDT <- filter(CurrentResults, Locus=="blah")
                
                CurrentResultsUpdated <<- QueryResults(NewInputs)
                Level2 <- dbReadTable(pool, 'level2')
                EmptyDT <- filter(CurrentResultsUpdated, Locus=="blah")
                
                if (nrow(CurrentResultsUpdated) == nrow(Level2)) {
                    output$Level2Table <- renderDataTable(datatable(EmptyDT))
                    output$SummaryInfo <- renderText({
                        HTML(paste0("Identical to the complete set of Level2 records"))
                    })
                }
                
                if (nrow(CurrentResultsUpdated) == 0) {
                    output$SummaryInfo <- renderPrint("Here I am, brain the size of a planet, and you ask me to count lithics.  Well, I can't find any that match your criteria.")
                    output$SummaryInfo <- renderText({
                        HTML(paste0("No lithics match criteria."))
                    })
                    output$Level2Table <- renderDataTable(datatable(CurrentResultsUpdated))
                }
                
                if (nrow(CurrentResultsUpdated) > 0 & nrow(CurrentResultsUpdated) != nrow(Level2)) {
                    SelectedRowEdit <<- input$Level2Table_rows_selected
                    output$Level2Table <- DT::renderDataTable(
                        datatable(CurrentResultsUpdated[,-1], escape = FALSE, rownames = FALSE, selection = list(mode = "multiple", target = "row"), editable = TRUE, options = list(autowidth = TRUE, searching = FALSE, columnDefs = list(list(targets=c(0,1,6,8), width='50'), list(targets=c(2,3,4,5), width='100')))))
                    to_index <<- CurrentResultsUpdated
                }
                
                message1Period <- ifelse(length(NewRecord$Period),NewRecord$Period,"NULL")
                message1Blank <- ifelse(length(NewRecord$Blank),NewRecord$Blank,"NULL")
                message1Modification <- ifelse(length(NewRecord$Modification),NewRecord$Modification,"NULL")
                message1RawMaterial <- ifelse(length(NewRecord$RawMaterial),NewRecord$RawMaterial,"NULL")
                message1Weathering <- ifelse(length(NewRecord$Weathering),NewRecord$Weathering,"NULL")
                message1Patination <- ifelse(length(NewRecord$Patination),"Yes","NULL")
                message1Burned <- ifelse(length(NewRecord$Burned),"Yes","NULL")
                message1 <- paste0("Created ",NewRecord$Quantity," records for ",NewRecord$LocusType," ",NewRecord$Locus," (Period: ",message1Period,", Blank: ",message1Blank,", Modification: ",message1Modification,", Raw Material: ",message1RawMaterial,", Weathering: ",message1Weathering,", Patination: ",message1Patination,", Burned: ",message1Burned,")")
                
                activitylog <- dbReadTable(pool, 'activitylog')
                activitylog <- data.frame(Log = message1,
                                          Timestamp = as.character(Sys.time()),
                                          stringsAsFactors = FALSE)
                writeActivity <- dbWriteTable(pool, 'activitylog', activitylog, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
                writeActivity
                activitylog <<- dbReadTable(pool, 'activitylog')
                activitylog
                output$NewRecordMessages <- renderText({
                    HTML((paste0("Created <b>",NewRecord$Quantity,"</b> records for ","<b>",NewRecord$LocusType," ",NewRecord$Locus,"</b>.</br>", "<b>Period:</b> ",message1Period,"</br>","<b>Blank:</b> ",message1Blank,"</br>", "<b> Modifiation:</b> ",message1Modification,"</br>","<b>Raw Material:</b> ",message1RawMaterial,"</br>","<b>Weathering:</b> ",message1Weathering,"</br>","<b>Patination:</b> ",message1Patination,"</br>","<b>Burned:</b> ",message1Burned)))
                })
            }
            
            if (nrow(EquivRecordFilter_df) > 0) {
                valuesx <<- reactiveValues(singleResponse_dfx = data.frame(input$NewLocus, input$NewLocusType, input$NewPeriod, input$NewBlank, input$NewModification, input$NewQuantity))
                
                colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewLocusType'] <<- 'LocusType'
                colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewLocus'] <<- 'Locus'
                colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewPeriod'] <<- 'Period'
                colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewBlank'] <<- 'Blank'
                colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewModification'] <<- 'Modification'
                colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewRawMaterial'] <<- 'RawMaterial'
                colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewWeathering'] <- 'Weathering'
                colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewPatination'] <<- 'Patination'
                colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewBurned'] <<- 'Burned'
                colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewQuantity'] <<- 'Quantity'
                
                toAdd <<- as.data.frame(valuesx$singleResponse_dfx, stringsAsFactors = FALSE)
                EquivRecordQuantityUpdated <<- nrow(EquivRecordFilter_df) + toAdd$Quantity
                EquivRecordQuantityUpdated_str <<- as.character(EquivRecordQuantityUpdated)
                EquivRecordFilter_df_str <<- as.character(nrow(EquivRecordFilter_df))
                toAddQuantity_str <<- as.character(toAdd$Quantity)
                
                output$NewRecordMessages <- renderText({
                    HTML((paste0("There are <b>",EquivRecordFilter_df_str,"</b> artefacts with the same set of characteristics from this locus already recorded in the database. Press 'Confirm' to add <b>",toAddQuantity_str,"</b> more, raising the total quantity to <b>",EquivRecordQuantityUpdated_str,"</b>.")))
                })
                
                output$ConfirmNewRecord <- renderUI({
                    actionButton("ConfirmNewRecordButton","Confirm")
                })
            }
        })
        
        observeEvent(input$ConfirmNewRecordButton, {
            UpdateExistingLevel2Record <<- glue::glue_sql("UPDATE `level2` SET `Quantity` = {EquivRecordQuantityUpdated} WHERE `Locus` = {EquivRecordFilter_df$Locus} AND `Period` = {EquivRecordFilter_df$Period} AND `Blank` = {EquivRecordFilter_df$Blank} AND `Modification` = {EquivRecordFilter_df$Modification}", .con = pool)
            UpdateExistingLevel2Record <<- as.character(UpdateExistingLevel2Record)
            UpdateExistingLevel2Record
            dbExecute(pool, sqlInterpolate(ANSI(), UpdateExistingLevel2Record))
            
            ValuesToExpand <<- reactiveValues(singleResponse_df = data.frame(input$NewLocusType, input$NewLocus, input$NewPeriod, input$NewBlank, input$NewModification, input$NewRawMaterial, input$NewWeathering, input$NewPatination, input$NewBurned, input$NewQuantity))
            toExpand <<- as.data.frame(ValuesToExpand$singleResponse_df)
            singleRow_expanded <<- expandRows(toExpand, count = 10, count.is.col = TRUE, drop = TRUE)
            
            singleRow_expanded <<- data.frame(lapply(singleRow_expanded, as.character), stringsAsFactors = FALSE)
            
            colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocus'] <- 'Locus'
            colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocusType'] <- 'LocusType'
            colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewPeriod'] <- 'Period'
            colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewBlank'] <- 'Blank'
            colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewModification'] <- 'Modification'
            colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewRawMaterial'] <- 'RawMaterial'
            colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewWeathering'] <- 'Weathering'
            colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewPatination'] <- 'Patination'
            colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewBurned'] <- 'Burned'
            
            singleRow_expanded <<- singleRow_expanded
            singleRow_expanded$Locus <- as.character(singleRow_expanded$Locus)
            singleRow_expanded$LocusType <- as.character(singleRow_expanded$LocusType)
            singleRow_expanded$Period <- as.character(singleRow_expanded$Period)
            singleRow_expanded$Blank <- as.character(singleRow_expanded$Blank)
            singleRow_expanded$Modification <- as.character(singleRow_expanded$Modification)
            singleRow_expanded$RawMaterial <- as.character(singleRow_expanded$RawMaterial)
            singleRow_expanded$Weathering <- as.character(singleRow_expanded$Weathering)
            singleRow_expanded$Patination <- as.character(singleRow_expanded$Patination)
            singleRow_expanded$Burned <- as.character(singleRow_expanded$Burned)
            
            for (j in (1:nrow(singleRow_expanded))) {
                ArtefactIDQuantityDifferenceNewRecordsExisting <<- toAdd$Quantity
                NewArtefactRecordsFromDifferenceNewRecordsExisting <<- data.frame(LocusType = as.character(), Locus = as.character(), Period = as.character(), Blank = as.character(), Modification = as.character(), RawMaterial = as.character(), Weathering = as.character(), Patination = as.character(), Burned = as.character(), ArtefactID = as.character(), stringsAsFactors = FALSE)
                getnewARid <- glue::glue_sql("SELECT MAX(ArtefactID) FROM `level3`", .con = pool)
                latestARid <- as.character(dbGetQuery(pool, getnewARid))
                latestARidnum <- as.numeric((str_extract(latestARid, "[0-9]+")))
                newARid <- latestARidnum + 1
                #generate first one, then increment up from there to ArtefactIDQuantityDifference
                ARidseq <- seq(newARid, length.out=ArtefactIDQuantityDifferenceNewRecordsExisting, by=1)
                ARidstring <<- as.character(paste0("AR", str_pad(ARidseq, 6, pad="0"))) 
                
                l<-1
                for (l in (1:ArtefactIDQuantityDifferenceNewRecordsExisting)) {
                    NewArtefactRecordsFromDifferenceNewRecordsExisting[l,] <- cbind(singleRow_expanded, data.frame(ArtefactID = ""), stringsAsFactors = F) #need to add a placeholder column for artefact ID
                    NewArtefactRecordsFromDifferenceNewRecordsExisting[l,]$ArtefactID <- ARidstring[l]
                    NewArtefactRecordsFromDifferenceNewRecordsExisting_df <<- NewArtefactRecordsFromDifferenceNewRecordsExisting
                }
            }
            
            write_level3_existing <<- glue::glue_sql("INSERT INTO `level3` (`Locus`, `LocusType`, `Period`, `Blank`, `Modification`, `RawMaterial`, `Weathering`, `Patination`, `Burned`, `ArtefactID`) VALUES ({NewArtefactRecordsFromDifferenceNewRecordsExisting$Locus}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$LocusType}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Period}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Blank}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Modification}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$RawMaterial}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Weathering}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Patination}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Burned}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$ArtefactID})", .con = pool)
            n <- 1
            for (n in (1:length(write_level3_existing))) {
                dbExecute(pool, write_level3_existing[n])
            }
            
            Level3 <- dbReadTable(pool, 'level3')
            NewRecordsTableY <- Level3 %>%
                select(ArtefactID, Locus, Period, Blank, Modification, RawMaterial, Weathering, Patination, Burned) %>%
                filter(Locus %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Locus & Period %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Period & Blank %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Blank & Modification %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Modification & RawMaterial %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$RawMaterial & Weathering %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Weathering & Patination %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Patination & Burned %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Burned)
            output$ShowNewRecords <- DT::renderDataTable(datatable(NewRecordsTableY))
            
            # refresh the interface
            NewTermLocus <- if(!is.null(input$NewLocus)) {input$NewLocus} else {""}
            NewTermPeriod <- if(!is.null(input$NewPeriod)) {input$NewPeriod} else {""}
            NewTermBlank <- if(!is.null(input$NewBlank)) {input$NewBlank} else {""}
            NewTermModification <- if(!is.null(input$NewModification)) {input$NewModification} else {""}
            NewTermRawMaterial <- if(!is.null(input$NewRawMaterial)) {input$NewRawMaterial} else {""}
            NewTermWeathering <- if(!is.null(input$NewWeathering)) {input$NewWeathering} else {""}
            NewTermPatination <- if(!is.null(input$NewPatination)) {input$NewPatination} else {""}
            NewTermBurned <- if(!is.null(input$NewBurned)) {input$NewBurned} else {""}
            NewInputs <<- list(Locus = NewTermLocus, Period = NewTermPeriod, Blank = NewTermBlank, Modification = NewTermModification, RawMaterial = NewTermRawMaterial, Weathering = NewTermWeathering, Patination = NewTermPatination, Burned = NewTermBurned)
            
            CurrentResults <<- QueryResults(NewInputs)
            Level2 <- dbReadTable(pool, 'level2')
            EmptyDT <- filter(CurrentResults, Locus=="blah")
            
            CurrentResultsUpdated <<- QueryResults(NewInputs)
            Level2 <- dbReadTable(pool, 'level2')
            EmptyDT <- filter(CurrentResultsUpdated, Locus=="blah")
            
            if (nrow(CurrentResultsUpdated) == nrow(Level2)) {
                output$Level2Table <- renderDataTable(datatable(EmptyDT))
                output$SummaryInfo <- renderText({
                    HTML(paste0("Identical to the complete set of Level2 records."))
                    })
            }
            
            if (nrow(CurrentResultsUpdated) == 0) {
                output$SummaryInfo <- renderText({
                    HTML(paste0("No lithics match criteria."))
                })
                output$Level2Table <- renderDataTable(datatable(CurrentResultsUpdated))
            }
            
            if (nrow(CurrentResultsUpdated) > 0 & nrow(CurrentResultsUpdated) != nrow(Level2)) {
                SelectedRowEdit <<- input$Level2Table_rows_selected
                output$Level2Table <- DT::renderDataTable(
                    datatable(CurrentResultsUpdated[,-1], escape = FALSE, rownames = FALSE, selection = list(mode = "multiple", target = "row"), editable = TRUE, options = list(autowidth = TRUE, searching = FALSE, columnDefs = list(list(targets=c(0,1,6,8), width='50'), list(targets=c(2,3,4,5), width='100')))))
                to_index <<- CurrentResultsUpdated
            }

            message2Period <- ifelse(EquivRecordFilter_df$Period!="",EquivRecordFilter_df$Period,"NULL")
            message2Blank <- ifelse(EquivRecordFilter_df$Blank!="",EquivRecordFilter_df$Blank,"NULL")
            message2Modification <- ifelse(EquivRecordFilter_df$Modification!="",EquivRecordFilter_df$Modification,"NULL")
            message2RawMaterial <- ifelse(EquivRecordFilter_df$RawMaterial!="",EquivRecordFilter_df$RawMaterial,"NULL")
            message2Weathering <- ifelse(EquivRecordFilter_df$Weathering!="",EquivRecordFilter_df$Weathering,"NULL")
            message2Patination <- ifelse(EquivRecordFilter_df$Patination!="","Yes","NULL")
            message2Burned <- ifelse(EquivRecordFilter_df$Burned!="","Yes","NULL")
            message2 <- paste0("Added ",toAddQuantity_str," lithics to existing batch of ",EquivRecordFilter_df_str," records in ",EquivRecordFilter_df$LocusType," ",EquivRecordFilter_df$Locus," with the following configuration: ","Period: ",message2Period,", Blank: ",message2Blank,", Modification: ",message2Modification,", Raw Material: ",message2RawMaterial,", Weathering: ",message2Weathering,", Patination: ",message2Patination,", Burned: ",message2Burned,". There are now ",EquivRecordQuantityUpdated_str," records matching that configuration in this locus.")
            
            activitylog <- dbReadTable(pool, 'activitylog')
            activitylog <- data.frame(Log = message2,
                                      Timestamp = as.character(Sys.time()),
                                      stringsAsFactors = FALSE)
            writeActivity <- dbWriteTable(pool, 'activitylog', activitylog, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
            writeActivity
            activitylog <<- dbReadTable(pool, 'activitylog')
            activitylog
            
            output$NewRecordMessages <- renderText({
                HTML((paste0("Added <b>",toAddQuantity_str,"</b> lithics to existing batch of ","<b>",EquivRecordFilter_df_str,"</b> records in <b>",EquivRecordFilter_df$LocusType," ",EquivRecordFilter_df$Locus,"</b> with the following configuration:</br>", "<b>Period:</b> ",message2Period,"</br>","<b>Blank:</b> ",message2Blank,"</br>", "<b> Modifiation:</b> ",message2Modification,"</br>","<b>Raw Material:</b> ",message2RawMaterial,"</br>","<b>Weathering:</b> ",message2Weathering,"</br>","<b>Patination:</b> ",message2Patination,"</br>","<b>Burned:</b> ",message2Burned,"</br>There are now <b>",EquivRecordQuantityUpdated_str,"</b> records matching that configuration in this locus.")))
            })
        })
        
        observeEvent(input$ClearNewInputs, {
            updateSelectInput(session, "NewLocusType", selected = "")
            updateSelectInput(session, "NewLocus", selected = "")
            updateSelectInput(session, "NewPeriod", selected = "")
            updateSelectInput(session, "NewBlank", selected = "")
            updateSelectInput(session, "NewModification", selected = "")
            updateSelectInput(session, "NewQuantity", selected = "1")
            updateSelectInput(session, "NewRawMaterial", selected = "")
            updateSelectInput(session, "NewWeathering", selected = "")
            updateSelectInput(session, "NewPatination", selected = "")
            updateSelectInput(session, "NewBurned", selected = "")
        })
        #-----/NewRecords-----#
        
        #-----EditRecords-----#
        observeEvent(input$EditButton, { # the selection has to be reset, currently have to de-select then select again
            sel <<- SelectedRowEdit
            
            if (length(sel)) {  #alternative that allows selection of multiple rows
                Level2IndexValues <- sel
                Level3SelectionEdit <<- to_index[Level2IndexValues, c(2,4,5,6,7,3)]
                Level3 <- dbReadTable(pool, 'level3')
                Level2FilterResultsEdit <<- filter(Level2, Locus %in% Level3SelectionEdit[,1] & Period %in% Level3SelectionEdit[,2] & Blank %in% Level3SelectionEdit[,3] & Modification %in% Level3SelectionEdit[,4])
                Level3FilterResultsEdit <<- filter(Level3, Locus %in% Level3SelectionEdit[,1] & Period %in% Level3SelectionEdit[,2] & Blank %in% Level3SelectionEdit[,3] & Modification %in% Level3SelectionEdit[,4])
                
                observe({
                    LocusTypeBefore <<- Level2FilterResultsEdit$LocusType
                    LocusBefore <<- Level2FilterResultsEdit$Locus
                    PeriodBefore <<- Level2FilterResultsEdit$Period
                    BlankBefore <<- Level2FilterResultsEdit$Blank
                    ModificationBefore <<- Level2FilterResultsEdit$Modification
                    QuantityBefore <<- Level2FilterResultsEdit$Quantity
                    
                    LocusTypeAfter <<- as.character(input$ModalLocusTypeSelect)
                    LocusAfter <<- as.character(input$ModalLocusSelect)
                    PeriodAfter <<- as.character(input$ModalPeriodSelect)
                    BlankAfter <<- as.character(input$ModalBlankSelect)
                    ModificationAfter <<- as.character(input$ModalModificationSelect)
                    QuantityAfter <<- input$ModalQuantitySelect
                    
                    LocusTypeSame <<- identical(LocusTypeBefore, LocusTypeAfter)
                    LocusSame <<- identical(LocusBefore, LocusAfter)
                    PeriodSame <<- identical(PeriodBefore, PeriodAfter)
                    BlankSame <<- identical(BlankBefore, BlankAfter)
                    ModificationSame <<- identical(ModificationBefore, ModificationAfter)
                    QuantitySame <<- identical(QuantityBefore, QuantityAfter)
                    
                    SelectedRawMaterial <<- as.character(input$ModalRawMaterialSelect)
                    SelectedWeathering <<- as.character(input$ModalWeatheringSelect)
                    SelectedPatination <<- as.character(input$ModalPatinationSelect)
                    SelectedBurned <<- as.character(input$ModalBurnedSelect)
                    SelectedQuantity <<- as.numeric(input$ModalQuantitySelect)
                    batchInputs <<- c(SelectedRawMaterial, SelectedWeathering, SelectedPatination, SelectedBurned)
                })
                
            }
            
            
            showModal(modalDialog(title = paste0("Update ghost records with level 3 info"),
                                  fluidRow(
                                      column(width = 2,
                                             selectInput("ModalRawMaterialSelect","Raw Material", c(Choose = '', 'Type A', 'Type B', 'Type C', 'Type D', 'Type E', 'Type F','Indeterminate','Missing'), selectize = TRUE)),
                                      column(width = 3,
                                             selectInput("ModalWeatheringSelect","Weathering", c(Choose = '', "1", "2", "3","4","5"), selectize = TRUE)), # Doesn't match recording system?
                                      column(width = 2,
                                             selectInput("ModalPatinationSelect","Patination", c(Choose = '', "Patinated"), selectize = TRUE)),
                                      column(width = 2,
                                             selectInput("ModalBurnedSelect","Burned", c(Choose = '', "Burned"), selectize = TRUE)),
                                      column(width = 2,
                                             numericInput("ModalQuantitySelect","Quantity", '', min=0))
                                  ),
                                  fluidRow(
                                      column(width = 2,
                                             actionButton('SaveBatch',"Apply update")),
                                      column(width = 2,
                                             actionButton('ModalClearInputs',"Clear inputs"))
                                  ),
                                  br(),
                                  fluidRow(
                                      column(width = 12,
                                             wellPanel(
                                                 textOutput("BatchErrors")))
                                  ),
                                  footer = (
                                      tagList(
                                          actionButton('ModalDismiss', "Dismiss")
                                      )
                                  ),
                                  easyClose = FALSE,
                                  size = "l",
                                  fade = FALSE
            ))
            
        })
        
        observeEvent(input$SaveBatch, {
            output$BatchErrors <- renderText(validate(
                need(input$ModalQuantitySelect, "Please input a quantity.")
            ))
            req(input$ModalQuantitySelect)
            if (input$ModalQuantitySelect == 0) {
                output$BatchErrors <- renderText("Please input a quantity.")
            }
            if (input$ModalQuantitySelect < 0) {
                output$BatchErrors <- renderText("Please input a positive quantity.")
            } 
            if (input$ModalQuantitySelect > 0) {
                
                Level3ToBatch <<- Level3FilterResultsEdit
                Level3ToBatch <<- Level3ToBatch %>% filter(WrittenOnArtefact == "No" | WrittenOnArtefact == "" | is.na(WrittenOnArtefact) == T | is.null(WrittenOnArtefact) == T)
                Level3ToBatch <<- Level3ToBatch %>% filter(RawMaterial == "" | is.na(RawMaterial) == T | is.null(RawMaterial))
                Level3ToBatch <<- Level3ToBatch %>% filter(Weathering == "" | is.na(Weathering) == T | is.null(Weathering))
                Level3ToBatch <<- Level3ToBatch %>% filter(Patination == "" | is.na(Patination) == T | is.null(Patination))
                Level3ToBatch <<- Level3ToBatch %>% filter(Burned == "" | is.na(Burned) == T | is.null(Burned))
                
                if (nrow(Level3ToBatch) >= SelectedQuantity) {
                    n <- SelectedQuantity
                    Level3TruncatedBatch <<- Level3ToBatch[1:n,]
                    if (SelectedRawMaterial != "" & is.na(SelectedRawMaterial) == F & is.null(SelectedRawMaterial) == F) {
                        BatchUpdateLevel3Query_RawMaterial <<- glue::glue_sql("UPDATE `level3` SET `RawMaterial` = {SelectedRawMaterial} WHERE `id` = {Level3TruncatedBatch$id}", .con = pool)
                        q <- 1
                        for (q in (1:length(BatchUpdateLevel3Query_RawMaterial))) {
                            dbExecute(pool, BatchUpdateLevel3Query_RawMaterial[q])
                        }
                    }
                    if (SelectedWeathering != "" & is.na(SelectedWeathering) == F & is.null(SelectedWeathering) == F) {
                        BatchUpdateLevel3Query_Weathering <<- glue::glue_sql("UPDATE `level3` SET `Weathering` = {SelectedWeathering} WHERE `id` = {Level3TruncatedBatch$id}", .con = pool)
                        w <- 1
                        for (w in (1:length(BatchUpdateLevel3Query_Weathering))) {
                            dbExecute(pool, BatchUpdateLevel3Query_Weathering[w])
                        }
                    }
                    if (SelectedPatination != "" & is.na(SelectedPatination) == F & is.null(SelectedPatination) == F) {
                        BatchUpdateLevel3Query_Patination <<- glue::glue_sql("UPDATE `level3` SET `Patination` = {SelectedPatination} WHERE `id` = {Level3TruncatedBatch$id}", .con = pool)
                        e <- 1
                        for (e in (1:length(BatchUpdateLevel3Query_Patination))) {
                            dbExecute(pool, BatchUpdateLevel3Query_Patination[e])
                        }
                    }
                    if (SelectedBurned != "" & is.na(SelectedBurned) == F & is.null(SelectedBurned) == F) {
                        BatchUpdateLevel3Query_Burned <<- glue::glue_sql("UPDATE `level3` SET `Burned` = {SelectedBurned} WHERE `id` = {Level3TruncatedBatch$id}", .con = pool)
                        r <- 1
                        for (r in (1:length(BatchUpdateLevel3Query_Burned))) {
                            dbExecute(pool, BatchUpdateLevel3Query_Burned[r])
                        }
                    }
                    
                    updateSelectInput(session, "ModalRawMaterialSelect", selected = "")
                    updateSelectInput(session, "ModalWeatheringSelect", selected = "")
                    updateSelectInput(session, "ModalPatinationSelect", selected = "")
                    updateSelectInput(session, "ModalBurnedSelect", selected = "")
                    updateSelectInput(session, "ModalQuantitySelect", selected = "")
                    
                    Level3TruncatedBatch <<- NULL
                    Level3ToBatch <<- Level3FilterResultsEdit
                    
                    sel <<- SelectedRowEdit
                    Level2IndexValues <- sel
                    
                    Level3Selection <<- to_index[Level2IndexValues, c(2,4,5,6,7,3)]
                    Level3SelectionEdit <<- to_index[Level2IndexValues, c(2,4,5,6,7,3)]
                    Level3 <- dbReadTable(pool, 'level3')
                    
                    Level3FilterResults <<- filter(Level3, Locus %in% Level3Selection[,1] & Period%in%Level3Selection[,2] & Blank%in%Level3Selection[,3] & Modification%in%Level3Selection[,4])
                    output$Level3Table <- DT::renderDataTable(
                        datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,5)))))
                    
                    output$BatchErrors <- renderText({
                        HTML(paste0("Update applied."))
                    })
                }
                
                if (nrow(Level3ToBatch) == 0) {
                    output$BatchErrors <- renderText({
                        HTML(paste0("All lithics have already been assigned raw materials / weathering / patination / burned values. To ensure you're modifying the correct records, please edit individual records by closing this modal and navigating to the 'Level 3' tab."))
                    })
                }
                
                if (nrow(Level3ToBatch) < SelectedQuantity & nrow(Level3ToBatch) > 0) {
                    BatchDifference <<- nrow(Level3ToBatch) - SelectedQuantity
                    BatchDifference_chr <<- as.character(BatchDifference)
                    output$BatchErrors <- renderText({
                        HTML(paste0("You have selected more artefacts than there are generic lithics available to be updated. Input a value less than or equal to the amount of editable lithics and/or edit individual records by closing this modal and navigating to the 'Level 3' tab."))
                    })
                }
            }
            
        })
        
        observeEvent(input$ModalDismiss, {
            removeModal()
        })
        
        observeEvent(input$ModalClearInputs, {
            updateSelectInput(session, "ModalRawMaterialSelect", selected = "")
            updateSelectInput(session, "ModalWeatheringSelect", selected = "")
            updateSelectInput(session, "ModalPatinationSelect", selected = "")
            updateSelectInput(session, "ModalBurnedSelect", selected = "")
            updateSelectInput(session, "ModalQuantitySelect", selected = "")
        })
        
        
        observeEvent(input$ClearInputs, {
            updateSelectizeInput(session, "LocusType", selected = "")
            updateSelectizeInput(session, "Locus", selected = "")
            updateSelectizeInput(session, "Period", selected = "")
            updateSelectizeInput(session, "Blank", selected = "")
            updateSelectizeInput(session, "Modification", selected = "")
            #updateSelectizeInput(session, "selectTrench", selected = "")
            
            output$SummaryInfo <- renderText({
                HTML(paste0("")) # clear summary and/or error messages when clearinputs is pressed
            })
            
            output$Level2Table <- DT::renderDataTable(
                datatable(CurrentResults[0,-1], escape = FALSE, rownames = FALSE, selection = list(mode = "multiple", target = "row"), editable = TRUE, options = list(autowidth = TRUE, searching = FALSE, columnDefs = list(list(targets=c(0,1,6,8), width='50'), list(targets=c(2,3,4,5), width='100')))))
            to_index <<- CurrentResults
        })
        
        #-----/EditRecords-----#
        
        #-----ActivityLog-----#
        activitylog <- dbReadTable(pool, 'activitylog')
        activitylog
        output$ActivityLogDisplay <- renderDataTable(datatable(activitylog[,-1], rownames = FALSE, selection=list(mode="single", target="row"), options=list(order=list(list(1, 'desc')), pageLength=20)))
        
        #-----/ActivityLog-----#
        
    }
)


shinyApp(ui, server)
