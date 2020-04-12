#FLiRT - Version current 4/2020

#load necessary packages
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


#get log-in info for database
source("/Users/zackbatist/Dropbox/FLIRT/keys.R")


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

#function to select trench and return associated contexts
chooseTrench <- function(trench) {
  trenches[trenches$Trench == trench,]$Context
}
transectcollectionpoints <- dbReadTable(pool, 'transectcollectionpoints')
transects <- as.character(sort(as.numeric(unique(transectcollectionpoints$Transect))))
transects <- str_pad(transects, 2, pad="0")
chooseTransect <- function(transect) {
  transectcollectionpoints[transectcollectionpoints$Transect == transect,]$CollectionPointID
}

##begin Shiny app

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
                                  selectizeInput("LocusType", "Locus Type", choices = c("Context","Transect","Grid","Grab", "XFind"), multiple = FALSE, selected = "")),
                           column(width = 1,
                                  uiOutput("LocusTypeSelected")),
                           column(width = 1,
                                  uiOutput("TrenchSelected")),
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
                           column(width = 8,
                                  wellPanel(htmlOutput("SummaryInfo"))
                           )
                         ),
                         hr(),
                         tabsetPanel(id = "myTabs", type = "tabs",
                                     tabPanel("Level 2 Selection",
                                              br(),
                                              DT::dataTableOutput("Level2Table"),
                                              actionButton("EditButton", "Edit selected rows")
                                     ),
                                     tabPanel("Level 3 Selection",
                                              br(),
                                              DT::dataTableOutput("Level3Table"),
                                              actionButton("Level3EditButton", "Submit cell edit")
                                     ),
                                     tabPanel("Photos",
                                              fluidRow(
                                                br(),
                                                column(width = 6,
                                                       textInput("newPhotoFilename", "Enter complete filename of new photo"))),
                                              fluidRow(
                                                column(width = 2,
                                                       actionButton("newPhoto", "Add new"))),
                                              hr(),
                                              br(),
                                              DT::dataTableOutput("PhotosTable")
                                     ),
                                     tabPanel("Illustrations",
                                              fluidRow(
                                                column(width = 6, align = "center", h5("Enter a drawing number (format DR####) or a file name or both. If no file name is entered, drawing number will become the file name."))
                                              ),
                                              fluidRow(
                                                br(),
                                                column(width = 3,
                                                       textInput("newIllustrationDrawingNumber", "Enter complete drawing number of new illustration")
                                                ),
                                                column(width = 3,
                                                       textInput("newIllustrationFilename", "Enter complete filename of new illustration")
                                                )
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
    #define the fields we want to save from the form
    fields <- c("Locus", "LocusType", "Period", "Blank", "Modification", "Quantity") #I think probably 'Quantity' should appear only on the data input tab, and not on the retrieval one?
    
    Level2 <- dbReadTable(pool, 'level2')
    output$NewLevel2Table <- DT::renderDataTable(
      datatable(Level2[,-1], rownames = FALSE))
    
    Level3 <- dbReadTable(pool, 'level3')
    output$NewLevel3Table <- DT::renderDataTable(
      datatable(Level3[,-1], rownames = FALSE))
    
    
    ###Selecting trenches / specific loci based on what LocusType is chosen 
    ###**FUNCTIONAL BUT selectTrench IS NOT DISPLAYING THE SELECTED OPTION (but it /is/ selected)
    observe({
      if (input$LocusType == "Context") {
        TrenchChoices <- unique(trenches$Trench)
        output$LocusTypeSelected <- renderUI({
          tagList(
            selectizeInput("selectTrench", "Trench", multiple = TRUE, choices = c("All",TrenchChoices), options=list(create=TRUE))
          )
        })
        req(input$selectTrench)
        if (input$selectTrench != "All") {  
          currentChoices <<- chooseTrench(input$selectTrench)
          output$TrenchSelected <- renderUI({
            tagList(
              selectizeInput("Locus", "Locus", selected = "", multiple = TRUE, choices = currentChoices, options=list(create=TRUE)))
          })
        }
        else {
          #query inputs Locus field should a) take new entries and b) re-populate Locus options following entry of new Loci
          currentChoices <<- allloci %>% select(Locus) %>% filter(allloci$LocusType == as.character(input$LocusType))
          output$TrenchSelected <- renderUI({
            tagList(
              selectizeInput("Locus", "Locus", choices = currentChoices, multiple = TRUE, selected = "", options=list(create=TRUE)))
          })
        }
      }
      
      if (input$LocusType == "Transect") {
        output$LocusTypeSelected <- renderUI({
          tagList(
            selectizeInput("selectTrench", "Select a transect", selected = "", multiple = T, choices = c("All",transects))
          )
        })
        req(input$selectTrench)
        if (input$selectTrench != "All"){
          currentChoices_transects <<- chooseTransect(input$selectTrench)
          output$TrenchSelected <- renderUI({
            tagList(
              selectizeInput("Locus", "Locus", choices = currentChoices_transects, multiple = TRUE, selected = "", options=list(create=TRUE))  
            )
          })  
        }
        else {
          currentChoices_transects <<- transectcollectionpoints %>% select(CollectionPointID) %>% filter(transectcollectionpoints$Transect == as.character(input$selectTrench))
          output$TrenchSelected <- renderUI({
            tagList(
              selectizeInput("Locus", "Locus", choices = currentChoices_transects, multiple = TRUE, selected = "", options=list(create=TRUE)) #filter transectcollectionpoints for all collection points associated with selected transect(s)
            )
          })
        }
      }
      
      
      if (input$LocusType == "Grid" | input$LocusType == "Grab" | input$LocusType == "XFind") {
        
        output$LocusTypeSelected <- renderUI({
          tagList(
            selectInput("selectTrench", "Trench", selected = "TRENCH NA", multiple = FALSE, choices = "TRENCH NA")
          )
        })
        output$TrenchSelected <- renderUI({
          tagList(
            selectizeInput("Locus", "Locus", choices = allloci %>% select(Locus) %>% filter(allloci$LocusType == as.character(input$LocusType)), multiple = TRUE, selected = "", options=list(create=TRUE))
          )
        })  
      }
      
      
      
      
    })
    
    
    
    
    
    #function to be used below to filter for response to query
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
        output$SummaryInfo <- renderText("Identical to the complete set of Level2 records")
      }
      
      if (nrow(CurrentResults) == 0) {
        output$SummaryInfo <- renderPrint("Here I am, brain the size of a planet, and you ask me to count lithics.  Well, I can't find any that match your criteria.") 
        output$SummaryInfo <- renderText(paste("No lithics match criteria."))
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
        output$SummaryInfo <- renderText(paste("Total artifacts:<b>" , sum(CurrentResults$Quantity), "</b>Total drawn:<b>", IllustrationSummary, "</b>Total photo'd:<b>", PhotoSummary,"</b>", sep=" "))
        
        
        observe({
          SelectedRowEdit <<- input$Level2Table_rows_selected
        })
        
        to_index <<- CurrentResults
      }
    })
    #-----/QueryLookup-----#
    
    #-----TableFilters-----#
    observe({
      sel <- input$Level2Table_rows_selected
      
      if (length(sel)) {  #alternative that allows selection of multiple rows
        Level2IndexValues <- sel
        Level3Selection <<- to_index[Level2IndexValues, c(2,4,5,6,7,3)]
        Level3 <- dbReadTable(pool, 'level3')
        
        #------------
        #select a cell to generate new table with rows/artifact which is to be filled in
        
        #when a Level 2 row is selected, generate Level3FilterResults; then check to see if nrow(Level3FilterResults) is equal to the quantity in the selected Level 2 row. If it is, no further action needed (Level 3 table generated anyway).  If the number of existing Level 3 records is less than the quantity in the selected Level 2 row, generate Level 3 records to make up the difference.  Compare sel$Quantity to nrow(Level3FilterResults). If nrow(Level3FilterResults) < sel$Quantity , calculate difference, and then get updated Level3 table and generate next n records, filling them with values from sel. For each selected row in sel, go through this process separately (in for loop?).
        # 
        
        #-------
        
        Level3FilterResults <<- filter(Level3, Locus %in% Level3Selection[,1] & Period %in% Level3Selection[,2] & Blank %in% Level3Selection[,3] & Modification %in% Level3Selection[,4])
        output$Level3Table <- DT::renderDataTable(
          datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = T))
        
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
            datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = T))
        }
        else 
          output$Level3Table <- DT::renderDataTable(
            datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = T))
        
        #if the 'Photo' or 'Illustration' cell is selected in the Level 3 table, that generates a query about any associated photos or illustrations.  That query returns a table of existing photos/illustrations, into which new photos/illustrations can be entered if necessary. If new ones are entered, check db to generate next photo/illustration ID and assign that to the one(s) entered.        
        ##need also to write Photo/Drawing ID to appropriate column of Level 3 record
        ##and why are these writing multiple records?
        observe({   #because other observes are inside this one, those actions seem to be doubling (or more) - happening when cells are selected rather than or in addition to when buttons are pressed
          SelectedCells <- input$Level3Table_cells_selected   #col 7 comes from selection of illustration, 8 from photo
          #first create empty tables 
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
              #    repeat for illustrations
              Illustrations <- dbReadTable(pool, 'illustrations')
              output$IllustrationsTable <- DT::renderDataTable(
                datatable(IllustrationsFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row")))
            }
            
            else {
              PhotosSelection <<- unlist(Level3FilterResults[Level3IndexValues[1], 7])
              Photos <- dbReadTable(pool, 'photos')
              PhotosFilterResults <<- filter(Photos, ArtefactID==PhotosSelection)
              output$PhotosTable <- DT::renderDataTable(
                datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row"), editable = TRUE))
              
              observeEvent(input$newPhoto, once=T, {
                newPhotoResponses <- reactiveValues(
                  NewPhoto = input$newPhotoFilename)  
                NewPhotoValue <- as.character(newPhotoResponses$NewPhoto)
                Photos <- dbReadTable(pool, 'photos') # re-grab in case of duplicates
                #increment photo ID - get current list, choose highest number, increment, and include in glue_sql statement (currently handled by mySQL, which auto-increments when row is inserted)
                ###**CURRENTLY MAKING TWO ENTRIES IN THE DATABASE (DOUBLE ENTERING)?
                contextUpdate <- Level3[Level3$ArtefactID == PhotosSelection,]$Locus
                newPhotoInsert <<- glue::glue_sql("INSERT INTO `photos` (`Filename`, `ArtefactID`,`Context`) VALUES ({NewPhotoValue}, {PhotosSelection},{contextUpdate})"
                                                  , .con = pool)
                dbExecute(pool, sqlInterpolate(ANSI(), newPhotoInsert))
                Photos <- dbReadTable(pool, 'photos')
                PhotosFilterResults <- filter(Photos, ArtefactID==PhotosSelection)
                output$PhotosTable <- DT::renderDataTable(
                  datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row"), editable = TRUE))
                
                # #once this new row is added, need to populate appropriate field of appropriate row in Level 3 table with the ID, using ArtefactID as index
                Level3 <- dbReadTable(pool, 'level3')
                # #need to get PhotoID from photo table, indexing by ArtefactID
                # Level3[Level3$ArtefactID==PhotosSelection,]$Photos <- filter(Photos, ArtefactID==PhotosSelection)$PhotoID #this is not strictly necessary, but has the benefit of updating the Level3 database that is in memory so it doesn't need to be refreshed
                # newPhoto <<- Level3[Level3$ArtefactID==PhotosSelection,]$Photos
                newPhoto <<- filter(Photos, ArtefactID==PhotosSelection)$PhotoID
                # #then update db
                Level3PhotoUpdate <<- glue::glue_sql("UPDATE `level3` SET `Photos` = CONCAT(IFNULL(`Photos`,''), ',' ,{newPhoto}) WHERE `ArtefactID` = {PhotosSelection}"
                                                     , .con = pool)
                k <- 1
                for (k in (1:length(newPhoto))) {
                  dbExecute(pool, sqlInterpolate(ANSI(), Level3PhotoUpdate[k]))
                }
                #  dbExecute(pool, sqlInterpolate(ANSI(), Level3PhotoUpdate))  ##but really this shouldn't be necessary; should be handled by a relate in MySQL, and here could just refresh the table displayed in FLIRT
                
                
                updateTextInput(session, "newPhotoFilename", value = "")
                
              })
              
              #   
              #repeat for Illustrations
              IllustrationsSelection <<- unlist(Level3FilterResults[Level3IndexValues[1], 7])
              Illustrations <- dbReadTable(pool, 'illustrations')
              IllustrationsFilterResults <- filter(Illustrations, ArtefactID==IllustrationsSelection)
              output$IllustrationsTable <- DT::renderDataTable(
                datatable(IllustrationsFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row"), editable = TRUE))
              
              
              
              observeEvent(input$newIllustration, once=T, {
                
                ##to avoid crash if you try to make a new entry with neither filename nor drawing ID: 
                if((is.null(input$newIllustrationFilename)) & (is.null(input$newIllustrationDrawingNumber))){
                  req(input$newIllustrationFilename & input$newIllustrationDrawingNumber)
                } 
                
                else {
                  newIllustrationResponses <- 
                    reactiveValues(
                      NewIllustration = input$newIllustrationFilename,
                      NewIllustrationDrawing = input$newIllustrationDrawingNumber
                    )
                  
                  
                  ##generate drawing number if one isn't entered:
                  getnewid <- glue::glue_sql("SELECT MAX(DrawingID) FROM `illustrations`", .con = pool)
                  latestid <- as.character(dbGetQuery(pool, getnewid))
                  latestidnum <- as.numeric((str_extract(latestid, "[0-9]+")))
                  newid <- latestidnum + 1
                  idstring <- as.character(paste0("DR", str_pad(newid, 4, pad="0")))
                  
                  
                  NewIllustrationDrawingValue <<- 
                    ifelse(
                      newIllustrationResponses$NewIllustrationDrawing == "",
                      as.character(idstring),
                      as.character(newIllustrationResponses$NewIllustrationDrawing)
                    )
                  
                  NewIllustrationValue <<- 
                    ifelse(
                      newIllustrationResponses$NewIllustration == "",
                      as.character(newIllustrationResponses$NewIllustrationDrawing),
                      as.character(newIllustrationResponses$NewIllustration)
                    )
                  
                  
                  Illustrations <- dbReadTable(pool, 'illustrations') # re-grab in case of duplicates
                  #increment illustration ID - get current list, choose highest number, increment, and include in glue_sql statement
                  contextUpdate <- Level3[Level3$ArtefactID == IllustrationsSelection, ]$Locus
                  newIllustrationInsert <<- glue::glue_sql("INSERT INTO `illustrations` (`Filename`, `ArtefactID`, `DrawingID`,`Locus`) VALUES ({NewIllustrationValue}, {IllustrationsSelection}, {NewIllustrationDrawingValue},{contextUpdate})"
                                                           , .con = pool)
                  dbExecute(pool, sqlInterpolate(ANSI(), newIllustrationInsert))
                  Illustrations <- dbReadTable(pool, 'illustrations')
                  IllustrationsFilterResults <<- filter(Illustrations, ArtefactID==IllustrationsSelection)
                  output$IllustrationsTable <- DT::renderDataTable(
                    datatable(IllustrationsFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row"), editable = TRUE))
                  
                  # #need to get IllustrationID from photo table, indexing by ArtefactID
                  # Level3[Level3$ArtefactID==IllustrationsSelection,]$Illustration <- filter(Illustrations, ArtefactID==IllustrationsSelection)$DrawingID #this is not strictly necessary, but has the benefit of updating the Level3 database that is in memory so it doesn't need to be refreshed
                  # newIllustration <<- Level3[Level3$ArtefactID==IllustrationsSelection,]$Illustration
                  newIllustration <<- filter(Illustrations, ArtefactID==IllustrationsSelection)$DrawingID
                  #then update db
                  Level3IllustrationUpdate <<- glue::glue_sql("UPDATE `level3` SET `Illustration` = CONCAT(IFNULL(`Illustration`,''),', ',{newIllustration}) WHERE `ArtefactID` = {IllustrationsSelection}"
                                                              , .con = pool)
                  k <- 1
                  for (k in (1:length(newIllustration))) {
                    dbExecute(pool, sqlInterpolate(ANSI(), Level3IllustrationUpdate[k]))
                  }
                  # dbExecute(pool, sqlInterpolate(ANSI(), Level3IllustrationUpdate))
                  
                  ##CLEAR INPUTS AT THE END
                  updateTextInput(session, "newIllustrationDrawingNumber", value = "")
                  updateTextInput(session, "newIllustrationFilename", value = "")
                  
                  
                }
              })
              
              
            }
          }
          else {
            Photos <- dbReadTable(pool, 'photos')
            PhotosFilterResults <- filter(Photos, ArtefactID=="None")
            output$PhotosTable <- DT::renderDataTable(
              datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row")))
            #repeat for Illustrations
            Illustrations <- dbReadTable(pool, 'illustrations')
            IllustrationsFilterResults <- filter(Illustrations, ArtefactID=="None")
            output$IllustrationsTable <- DT::renderDataTable(
              datatable(IllustrationsFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row")))
          }
        })
      }
      else {
        Level3 <- dbReadTable(pool, 'level3')
        Level3FilterResults <- filter(Level3, Blank=="None")
        output$Level3Table <- DT::renderDataTable(
          datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = T))
      }
    })
    
    observeEvent(input$Level3Table_cell_edit, {
      CellsToEdit <<- input$Level3Table_cell_edit
      ARtoEdit <<- Level3FilterResults[CellsToEdit$row, 7] #returns artifact (whichever column)
      ColtoEdit <<- colnames(Level3FilterResults)[CellsToEdit$col+2] #change
      #then use that AR and the column name of CellsToEdit[2] to identify cell to edit in Level3 table
      ValueToUpdate <<- Level3[Level3$ArtefactID == ARtoEdit, ColtoEdit]
      UpdatedValue <<- CellsToEdit$value #then get value from edited cell and write to db
    })
    
    observeEvent(input$Level3EditButton, {        #use 'Edit cell' button to save individual cell edits
      EditedCell <<- glue::glue_sql("UPDATE `level3` SET {`ColtoEdit`} = {UpdatedValue} WHERE `ArtefactID` = {ARtoEdit}", .con = pool)
      dbExecute(pool, sqlInterpolate(ANSI(), EditedCell))
    })
    
    
    
    #-----/TableFilters-----#
    
    
    #has this code below to check/generate IDs for photos/illustrations/etc been implemented?  Should it be written here as functions that can be called above?
    
    #-----New IDs-----#
    Level3 <- dbReadTable(pool, 'level3')
    ArtefactNumbers <- as.character(Level3$ArtefactID)
    ArtefactNumbers_int <- as.numeric(gsub("([[:alpha:]])", "", ArtefactNumbers))
    HightestAR <- max(ArtefactNumbers_int)
    NewAR <-  paste0("AR", HightestAR + 1)
    if (str_length(NewAR) < 8) {
      ExtraZeroesAR_quant <- 8 - str_length(NewAR)
      ExtraZeroesAR <- str_dup("0", ExtraZeroesAR_quant)
      FixedPrefixAR <- paste0("AR", ExtraZeroesAR)
      NewAR <- gsub("(^..)", FixedPrefixAR, NewAR)
    }
    
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
    
    Illustrations <- dbReadTable(pool, 'artefactillustrations') #the most up to date data on drawings has not yet been imported to the database
    IllustrationNumbers <- as.character(Illustrations$IllustrationNumber)
    IllustrationNumbers_int <- as.numeric(gsub("([[:alpha:]])", "", IllustrationNumbers))
    HightestDR <- max(IllustrationNumbers_int)
    NewDR <-  paste0("DR", HightestDR + 1)
    if (str_length(NewDR) < 6) {
      ExtraZeroesDR_quant <- 6 - str_length(NewDR)
      ExtraZeroesDR <- str_dup("0", ExtraZeroesDR_quant)
      FixedPrefixDR <- paste0("DR", ExtraZeroesDR)
      NewDR <- gsub("(^..)", FixedPrefixDR, NewDR)
    }
    
    #-----/New Ids-----#
    
    #-----NewRecords-----#
    observeEvent(input$NewRecordButton, {
      showModal(modalDialog(title = paste0("Create New Records"),
                            fluidRow(
                              column(width = 2,
                                     selectInput("NewLocusType", "Locus Type", choices = c("Context","Transect","Grid","Grab", "XFind"), multiple = FALSE, selected = NULL)),
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
        select(filter(Level3, LocusType==input$NewLocusType & Locus==input$NewLocus & Period==input$NewPeriod & Blank==input$NewBlank & Modification==input$NewModification & RawMaterial==input$NewRawMaterial & Weathering==input$NewWeathering & Patination==input$NewPatination), LocusType, Locus, Period, Blank, Modification, RawMaterial, Weathering, Patination)
      })
      EquivRecordFilter_df <<- EquivRecordFilter()
      
      if (nrow(EquivRecordFilter_df) == 0) {
        values <- reactiveValues(singleResponse_df = data.frame(input$NewLocusType, input$NewLocus, input$NewPeriod, input$NewBlank, input$NewModification, input$NewRawMaterial, input$NewWeathering, input$NewPatination, input$NewQuantity))
        
        colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewLocusType'] <- 'LocusType'
        colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewLocus'] <- 'Locus'
        colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewPeriod'] <- 'Period'
        colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewBlank'] <- 'Blank'
        colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewModification'] <- 'Modification'
        colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewRawMaterial'] <- 'RawMaterial'
        colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewWeathering'] <- 'Weathering'
        colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewPatination'] <- 'Patination'
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
        NewRecord$Quantity <- as.numeric(NewRecord$Quantity)
        
        WriteNewLevel2Record <- glue::glue_sql("INSERT INTO `level2` (`Locus`, `LocusType`, `Period`, `Blank`, `Modification`, `Quantity`) VALUES ({NewRecord$Locus}, {NewRecord$LocusType}, {NewRecord$Period}, {NewRecord$Blank}, {NewRecord$Modification}, {NewRecord$Quantity})", .con = pool)
        WriteNewLevel2Record <<- as.character(WriteNewLevel2Record)
        WriteNewLevel2Record
        dbExecute(pool, sqlInterpolate(ANSI(), WriteNewLevel2Record))
        
        ValuesToExpand <<- reactiveValues(singleResponse_df = data.frame(input$NewLocusType, input$NewLocus, input$NewPeriod, input$NewBlank, input$NewModification, input$NewRawMaterial, input$NewWeathering, input$NewPatination, input$NewQuantity))
        toExpand <<- as.data.frame(ValuesToExpand$singleResponse_df)
        singleRow_expanded <<- expandRows(toExpand, count = 9, count.is.col = TRUE, drop = TRUE)
        singleRow_expanded <<- data.frame(lapply(singleRow_expanded, as.character), stringsAsFactors = FALSE)
        colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocus'] <- 'Locus'
        colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocusType'] <- 'LocusType'
        colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewPeriod'] <- 'Period'
        colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewBlank'] <- 'Blank'
        colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewModification'] <- 'Modification'
        colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewRawMaterial'] <- 'RawMaterial'
        colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewWeathering'] <- 'Weathering'
        colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewPatination'] <- 'Patination'
        
        singleRow_expanded <<- singleRow_expanded
        singleRow_expanded$Locus <- as.character(singleRow_expanded$Locus)
        singleRow_expanded$LocusType <- as.character(singleRow_expanded$LocusType)
        singleRow_expanded$Period <- as.character(singleRow_expanded$Period)
        singleRow_expanded$Blank <- as.character(singleRow_expanded$Blank)
        singleRow_expanded$Modification <- as.character(singleRow_expanded$Modification)
        singleRow_expanded$RawMaterial <- as.character(singleRow_expanded$RawMaterial)
        singleRow_expanded$Weathering <- as.character(singleRow_expanded$Weathering)
        singleRow_expanded$Patination <- as.character(singleRow_expanded$Patination)
        
        for (j in (1:nrow(singleRow_expanded))) {
          ArtefactIDQuantityDifferenceNewRecords <<- nrow(singleRow_expanded)
          NewArtefactRecordsFromDifferenceNewRecords <<- data.frame(LocusType = as.character(), Locus = as.character(), Period = as.character(), Blank = as.character(), Modification = as.character(), RawMaterial = as.character(), Weathering = as.character(), Patination = as.character(), ArtefactID = as.character(), stringsAsFactors = FALSE)
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
        
        write_level3 <<- glue::glue_sql("INSERT INTO `level3` (`Locus`, `LocusType`, `Period`, `Blank`, `Modification`, `RawMaterial`, `Weathering`, `Patination`, `ArtefactID`) VALUES ({NewArtefactRecordsFromDifferenceNewRecords$Locus}, {NewArtefactRecordsFromDifferenceNewRecords$LocusType}, {NewArtefactRecordsFromDifferenceNewRecords$Period}, {NewArtefactRecordsFromDifferenceNewRecords$Blank}, {NewArtefactRecordsFromDifferenceNewRecords$Modification}, {NewArtefactRecordsFromDifferenceNewRecords$RawMaterial}, {NewArtefactRecordsFromDifferenceNewRecords$Weathering}, {NewArtefactRecordsFromDifferenceNewRecords$Patination}, {NewArtefactRecordsFromDifferenceNewRecords$ArtefactID})", .con = pool)
        m <- 1
        for (m in (1:length(write_level3))) {
          dbExecute(pool, write_level3[m])
        }
        
        output$ShowNewRecords <- renderUI({
          DT::renderDataTable(Level3 %>% filter(ArtefactID %in% ARidstring) %>% select(ArtefactID, Locus, Period, Blank, Modification, RawMaterial, Weathering, Patination))
        })
        
        # refresh the interface
        NewTermLocus <- if(!is.null(input$NewLocus)) {input$NewLocus} else {""}
        NewTermPeriod <- if(!is.null(input$NewPeriod)) {input$NewPeriod} else {""}
        NewTermBlank <- if(!is.null(input$NewBlank)) {input$NewBlank} else {""}
        NewTermModification <- if(!is.null(input$NewModification)) {input$NewModification} else {""}
        NewTermRawMaterial <- if(!is.null(input$NewRawMaterial)) {input$NewRawMaterial} else {""}
        NewTermWeathering <- if(!is.null(input$NewWeathering)) {input$NewWeathering} else {""}
        NewTermPatination <- if(!is.null(input$NewPatination)) {input$NewPatination} else {""}
        NewInputs <<- list(Locus = NewTermLocus, Period = NewTermPeriod, Blank = NewTermBlank, Modification = NewTermModification, RawMaterial = NewTermRawMaterial, Weathering = NewTermWeathering, Patination = NewTermPatination)
        
        CurrentResults <<- QueryResults(NewInputs)
        Level2 <- dbReadTable(pool, 'level2')
        EmptyDT <- filter(CurrentResults, Locus=="blah")
        
        CurrentResultsUpdated <<- QueryResults(NewInputs)
        Level2 <- dbReadTable(pool, 'level2')
        EmptyDT <- filter(CurrentResultsUpdated, Locus=="blah")
        
        if (nrow(CurrentResultsUpdated) == nrow(Level2)) {
          output$Level2Table <- renderDataTable(datatable(EmptyDT))
          output$SummaryInfo <- renderText("Identical to the complete set of Level2 records")
        }
        
        if (nrow(CurrentResultsUpdated) == 0) {
          output$SummaryInfo <- renderPrint("Here I am, brain the size of a planet, and you ask me to count lithics.  Well, I can't find any that match your criteria.")
          output$SummaryInfo <- renderText(paste("No lithics match criteria."))
          output$Level2Table <- renderDataTable(datatable(CurrentResultsUpdated))
        }
        
        if (nrow(CurrentResultsUpdated) > 0 & nrow(CurrentResultsUpdated) != nrow(Level2)) {
          SelectedRowEdit <<- input$Level2Table_rows_selected
          output$Level2Table <- DT::renderDataTable(
            datatable(CurrentResultsUpdated[,-1], escape = FALSE, rownames = FALSE, selection = list(mode = "multiple", target = "row"), editable = TRUE, options = list(autowidth = TRUE, searching = FALSE, columnDefs = list(list(targets=c(0,1,6,8), width='50'), list(targets=c(2,3,4,5), width='100')))))
          to_index <<- CurrentResultsUpdated
        }
        
        message1 <- paste0("Created ",NewRecord$Quantity," new records from ",NewRecord$LocusType," ",NewRecord$Locus," as: ",NewRecord$Period," / ",NewRecord$Blank," / ",NewRecord$Modification,".")
        activitylog <- dbReadTable(pool, 'activitylog')
        activitylog <- data.frame(Log = message1,
                                  Timestamp = as.character(Sys.time()),
                                  stringsAsFactors = FALSE)
        writeActivity <- dbWriteTable(pool, 'activitylog', activitylog, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
        writeActivity
        activitylog <<- dbReadTable(pool, 'activitylog')
        activitylog
        output$NewRecordMessages <- renderPrint(message1)
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
        colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewQuantity'] <<- 'Quantity'
        
        toAdd <<- as.data.frame(valuesx$singleResponse_dfx, stringsAsFactors = FALSE)
        EquivRecordQuantityUpdated <<- nrow(EquivRecordFilter_df) + toAdd$Quantity
        EquivRecordQuantityUpdated_str <<- as.character(EquivRecordQuantityUpdated)
        EquivRecordFilter_df_str <<- as.character(nrow(EquivRecordFilter_df))
        toAddQuantity_str <<- as.character(toAdd$Quantity)
        message3 <- paste0("",EquivRecordFilter_df_str," artefacts with the same set of characteristics have already been recorded. Press 'Confirm' to add ",toAddQuantity_str," more, raising the total quantity to ",EquivRecordQuantityUpdated_str,".")
        output$NewRecordMessages <- renderPrint(message3)
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
      
      ValuesToExpand <<- reactiveValues(singleResponse_df = data.frame(input$NewLocusType, input$NewLocus, input$NewPeriod, input$NewBlank, input$NewModification, input$NewRawMaterial, input$NewWeathering, input$NewPatination, input$NewQuantity))
      toExpand <<- as.data.frame(ValuesToExpand$singleResponse_df)
      singleRow_expanded <<- expandRows(toExpand, count = 9, count.is.col = TRUE, drop = TRUE)
      
      singleRow_expanded <<- data.frame(lapply(singleRow_expanded, as.character), stringsAsFactors = FALSE)
      
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocus'] <- 'Locus'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocusType'] <- 'LocusType'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewPeriod'] <- 'Period'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewBlank'] <- 'Blank'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewModification'] <- 'Modification'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewRawMaterial'] <- 'RawMaterial'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewWeathering'] <- 'Weathering'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewPatination'] <- 'Patination'
      
      singleRow_expanded <<- singleRow_expanded
      singleRow_expanded$Locus <- as.character(singleRow_expanded$Locus)
      singleRow_expanded$LocusType <- as.character(singleRow_expanded$LocusType)
      singleRow_expanded$Period <- as.character(singleRow_expanded$Period)
      singleRow_expanded$Blank <- as.character(singleRow_expanded$Blank)
      singleRow_expanded$Modification <- as.character(singleRow_expanded$Modification)
      singleRow_expanded$RawMaterial <- as.character(singleRow_expanded$RawMaterial)
      singleRow_expanded$Weathering <- as.character(singleRow_expanded$Weathering)
      singleRow_expanded$Patination <- as.character(singleRow_expanded$Patination)
      
      for (j in (1:nrow(singleRow_expanded))) {
        ArtefactIDQuantityDifferenceNewRecordsExisting <<- toAdd$Quantity
        NewArtefactRecordsFromDifferenceNewRecordsExisting <<- data.frame(LocusType = as.character(), Locus = as.character(), Period = as.character(), Blank = as.character(), Modification = as.character(), RawMaterial = as.character(), Weathering = as.character(), Patination = as.character(), ArtefactID = as.character(), stringsAsFactors = FALSE)
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
      
      write_level3_existing <<- glue::glue_sql("INSERT INTO `level3` (`Locus`, `LocusType`, `Period`, `Blank`, `Modification`, `RawMaterial`, `Weathering`, `Patination`, `ArtefactID`) VALUES ({NewArtefactRecordsFromDifferenceNewRecordsExisting$Locus}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$LocusType}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Period}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Blank}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Modification}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$RawMaterial}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Weathering}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$Patination}, {NewArtefactRecordsFromDifferenceNewRecordsExisting$ArtefactID})", .con = pool)
      n <- 1
      for (n in (1:length(write_level3_existing))) {
        dbExecute(pool, write_level3_existing[n])
      }
      
      # refresh the interface
      NewTermLocus <- if(!is.null(input$NewLocus)) {input$NewLocus} else {""}
      NewTermPeriod <- if(!is.null(input$NewPeriod)) {input$NewPeriod} else {""}
      NewTermBlank <- if(!is.null(input$NewBlank)) {input$NewBlank} else {""}
      NewTermModification <- if(!is.null(input$NewModification)) {input$NewModification} else {""}
      NewTermRawMaterial <- if(!is.null(input$NewRawMaterial)) {input$NewRawMaterial} else {""}
      NewTermWeathering <- if(!is.null(input$NewWeathering)) {input$NewWeathering} else {""}
      NewTermPatination <- if(!is.null(input$NewPatination)) {input$NewPatination} else {""}
      NewInputs <<- list(Locus = NewTermLocus, Period = NewTermPeriod, Blank = NewTermBlank, Modification = NewTermModification, RawMaterial = NewTermRawMaterial, Weathering = NewTermWeathering, Patination = NewTermPatination)
      
      CurrentResults <<- QueryResults(NewInputs)
      Level2 <- dbReadTable(pool, 'level2')
      EmptyDT <- filter(CurrentResults, Locus=="blah")
      
      CurrentResultsUpdated <<- QueryResults(NewInputs)
      Level2 <- dbReadTable(pool, 'level2')
      EmptyDT <- filter(CurrentResultsUpdated, Locus=="blah")
      
      if (nrow(CurrentResultsUpdated) == nrow(Level2)) {
        output$Level2Table <- renderDataTable(datatable(EmptyDT))
        output$SummaryInfo <- renderText("Identical to the complete set of Level2 records")
      }
      
      if (nrow(CurrentResultsUpdated) == 0) {
        output$SummaryInfo <- renderPrint("Here I am, brain the size of a planet, and you ask me to count lithics.  Well, I can't find any that match your criteria.")
        output$SummaryInfo <- renderText(paste("No lithics match criteria."))
        output$Level2Table <- renderDataTable(datatable(CurrentResultsUpdated))
      }
      
      if (nrow(CurrentResultsUpdated) > 0 & nrow(CurrentResultsUpdated) != nrow(Level2)) {
        SelectedRowEdit <<- input$Level2Table_rows_selected
        output$Level2Table <- DT::renderDataTable(
          datatable(CurrentResultsUpdated[,-1], escape = FALSE, rownames = FALSE, selection = list(mode = "multiple", target = "row"), editable = TRUE, options = list(autowidth = TRUE, searching = FALSE, columnDefs = list(list(targets=c(0,1,6,8), width='50'), list(targets=c(2,3,4,5), width='100')))))
        to_index <<- CurrentResultsUpdated
      }
      
      ShowNewRecordsTable <- Level3 %>% select(filter(Locus %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Locus & Period %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Period & Blank %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Blank & Modification %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Modification & RawMaterial %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$RawMaterial & Weathering %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Weathering & Patination %in% NewArtefactRecordsFromDifferenceNewRecordsExisting_df$Patination), ArtefactID, Locus, Period, Blank, Modification, RawMaterial, Weathering, Patination)
      
      output$ShowNewRecords <- renderUI({
        DT::renderDataTable(ShowNewRecordsTable)
      })
      
      
      
      message2 <- paste0("",toAddQuantity_str," lithics added to existing batch of ",EquivRecordFilter_df_str," records with configuration: [",EquivRecordFilter_df$LocusType,"/",EquivRecordFilter_df$Locus,"/",EquivRecordFilter_df$Period,"/",EquivRecordFilter_df$Blank,"/",EquivRecordFilter_df$Modification,"]. There are now ",EquivRecordQuantityUpdated_str," records of that configuration.")
      activitylog <- dbReadTable(pool, 'activitylog')
      activitylog <- data.frame(Log = message2,
                                Timestamp = as.character(Sys.time()),
                                stringsAsFactors = FALSE)
      writeActivity <- dbWriteTable(pool, 'activitylog', activitylog, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
      writeActivity
      activitylog <<- dbReadTable(pool, 'activitylog')
      activitylog
      output$NewRecordMessages <- renderPrint(message2)
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
      }
      
      
      showModal(modalDialog(title = paste0("Edit selected records"),
                            tabsetPanel(id = "modalTabs", type = "tabs",
                                        tabPanel("Level 3",
                                                 h4("Update multiple Level 3 records (Batch update)"),
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
                                                          actionButton("MaxQuantityEdit", "Select all"))
                                                 ),
                                                 br(),
                                                 fluidRow(
                                                   column(width = 12,
                                                          wellPanel(
                                                            textOutput("BatchErrors")))
                                                 ),
                                                 hr(),
                                                 fluidRow(
                                                   column(width = 5,
                                                          h4("Update individual Level 3 records")),
                                                   column(width = 2,
                                                          actionButton('SaveIndividual',"Apply changes"))
                                                 ),
                                                 DT::dataTableOutput('Level3TableModal')
                                        )#,
  
                            ),
                            footer = (
                              tagList(
                                actionButton('ModalDismiss', "Dismiss")
                              )
                            ),
                            easyClose = FALSE,
                            size = "l",
                            fade = FALSE
      )
      )
      
      output$Level3TableModal <- DT::renderDataTable(datatable(Level3FilterResultsEdit[,-c(1:6)], rownames = FALSE, selection=list(mode="single", target="cell"), editable = TRUE, escape = FALSE, options = list(scrollX=TRUE, searching =FALSE)))
      
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
        batchInputs <<- c(SelectedRawMaterial, SelectedWeathering, SelectedPatination)
      })
      
      #need to have 'Apply Changes' button write results from table inputs to output$SaveIndividual
      
      
    }) # close input$editButton
  
    
    #when 'Apply Update' button is clicked:
    observeEvent(input$SaveBatch, {
      output$BatchErrors <- renderText(validate(
        need(input$ModalQuantitySelect, "Please input a quantity.")
      ))
      req(input$ModalQuantitySelect)
      if (input$ModalQuantitySelect == 0) {
        output$BatchErrors <- renderText("Please input a quantity.")
      }
      if (input$ModalQuantitySelect < 0) {   ###NEGATIVE MODAL QUANTITY
        output$BatchErrors <- renderText("Please input a positive quantity.")
      } 
      if (input$ModalQuantitySelect > 0) {
        
        Level3ToBatch <<- Level3FilterResultsEdit
        Level3ToBatch <<- Level3ToBatch %>% filter(WrittenOnArtefact == "No" | WrittenOnArtefact == "" | is.na(WrittenOnArtefact) == T | is.null(WrittenOnArtefact) == T) #no artifacts that have been written on are allowed to be batch updated
        
        #then filter depending on which input has been selected
        #check inputs - for each column, if an input has been made, filter for that column; if not, move on to the next column 
        names(batchInputs) <- c("RawMaterial", "Weathering", "Patination")
        
        m <- 1
        for (m in 1:length(batchInputs)) {
          if (batchInputs[m] != "") {
            Level3ToBatch <<- Level3ToBatch %>% filter(!!(sym(names(batchInputs)[m])) == "" | is.na(!!(sym(names(batchInputs)[m])))==T | is.null(!!(sym(names(batchInputs)[m])))==T)
          }
        } 
        #this takes data inputs and filters for rows in which there's no data entered, allowing changes for those which are empty
        
        
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
  
          
          # reset
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
            datatable(Level3FilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = TRUE))
          
          Level3FilterResultsEdit <<- filter(Level3, Locus%in%Level3SelectionEdit[,1] & Period%in%Level3SelectionEdit[,2] & Blank==Level3SelectionEdit[,3] & Modification%in%Level3SelectionEdit[,4])
          output$Level3TableModal <- DT::renderDataTable(
            datatable(Level3FilterResultsEdit[,-c(1:6)], rownames = FALSE, selection=list(mode="single", target="cell"), editable = TRUE, escape = FALSE, options = list(scrollX=TRUE, searching =FALSE)))
          
          

          
          output$BatchErrors <- renderText("Update applied.")
        } # close input$SaveBatch, if no conflicts
        
        if (nrow(Level3ToBatch) == 0) {
          output$BatchErrors <- renderText(paste0("All lithics have already been assigned raw materials / weathering / patination / burned values. Please edit individual records below."))
        }
        
        if (nrow(Level3ToBatch) < SelectedQuantity & nrow(Level3ToBatch) > 0) {
          BatchDifference <<- SelectedQuantity - nrow(Level3ToBatch)
          BatchDifference_chr <<- as.character(BatchDifference)
          output$BatchErrors <- renderText(paste0("You have selected more artefacts than there are unassigned lithics. Input a value less than or equal to ",BatchDifference_chr,", and/or edit individual records below."))
        }
      }
      
    }) # close input$SaveBatch, all conditions
    
    observeEvent(input$Level3TableModal_cell_edit, {
      CellsToEdit_modal <<- input$Level3TableModal_cell_edit
      ARtoEdit_modal <<- Level3FilterResultsEdit[CellsToEdit_modal$row, 7] #returns artifact (whichever column)
      ColtoEdit_modal <<- colnames(Level3FilterResultsEdit[,-c(1:6)])[CellsToEdit_modal$col+1] #change 
      #then use that AR and the column name of CellsToEdit[2] to identify cell to edit in Level3 table
      ValueToUpdate_modal <<- Level3[Level3$ArtefactID == ARtoEdit_modal, ColtoEdit_modal]
      UpdatedValue_modal <<- CellsToEdit_modal$value #then get value from edited cell and write to db
    })
    
    observeEvent(input$SaveIndividual, {        #use 'Apply Change' button to save individual cell edits
      EditedCell_modal <- glue::glue_sql("UPDATE `level3` SET {`ColtoEdit_modal`} = {UpdatedValue_modal} WHERE `ArtefactID` = {ARtoEdit_modal}", .con = pool)
      dbExecute(pool, sqlInterpolate(ANSI(), EditedCell_modal))
    })
    
    observeEvent(input$ModalDismiss, {
      removeModal()
    })
    
    observeEvent(input$ClearInputs, {
      updateSelectInput(session, "LocusType", selected = "")
      updateSelectInput(session, "Locus", selected = "")
      updateSelectInput(session, "Period", selected = "")
      updateSelectInput(session, "Blank", selected = "")
      updateSelectInput(session, "Modification", selected = "")
      updateSelectizeInput(session, "selectTrench", selected = "")
      
      output$SummaryInfo <- renderText(paste("")) ##clear summary when clearinputs is pressed
      output$SummaryInfo <- renderText(paste("")) ##clear error message when clearinputs is pressed
      
      output$Level2Table <- DT::renderDataTable(
        datatable(CurrentResults[0,-1], escape = FALSE, rownames = FALSE, selection = list(mode = "multiple", target = "row"), editable = TRUE, options = list(autowidth = TRUE, searching = FALSE, columnDefs = list(list(targets=c(0,1,6,8), width='50'), list(targets=c(2,3,4,5), width='100')))))
      to_index <<- CurrentResults
    })
    
    observeEvent(input$MaxQuantityEdit, {
      AllRows <<- sum(Level2FilterResultsEdit$Quantity)
      updateSelectInput(session, "ModalQuantitySelect", selected = AllRows)
    })
    
    
    
    
    
    #-----/EditRecords-----#
    
    #-----ActivityLog-----#
    activitylog <- dbReadTable(pool, 'activitylog')
    #activitylog
    output$ActivityLogDisplay <- renderDataTable(datatable(activitylog[,-1], rownames = FALSE, selection=list(mode="single", target="row"), options=list(order=list(list(1, 'desc')), pageLength=20)))
    
    #-----/ActivityLog-----#
    
  }
)


shinyApp(ui, server)
