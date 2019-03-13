library(shiny)
library(xtable)
library(DT)
#library(plyr)
library(dplyr)
library(reshape2)
library(RMariaDB)
library(DBI)
library(splitstackshape)
library(devtools)
library(pool)
library(purrr)
library(shinyjs)
library(stringr)
library(shinyBS)
library(shinydashboard)
#devtools::install_github('rstudio/DT') #this was necessary in order to resolve an issue I had with the coerceValue command, which was throwing up errors when I wanted to coerce character values. More here: https://github.com/rstudio/DT/pull/480

#need to set working directory to where keys.R is
source("keys.R")


#define pool handler by pool on global level
pool <- pool::dbPool(drv = RMariaDB::MariaDB(),
                     dbname = dbnamex,
                     host = hostx,
                     port = portx,
                     user = userx,
                     password = passwordx)

onStop(function() {
  poolClose(pool)
}) # important!


# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}

#read unmodified versions of allloci, blanks and modifications tables specifically for the input selection
allloci <- dbReadTable(pool, 'allloci')
allloci
blanks <- dbReadTable(pool, 'blanks_excavation')
blanks
modifications <- dbReadTable(pool, 'modifications_excavation')
modifications
periods <- dbReadTable(pool, 'dating')
periods

shinyApp(
  ui <- fluidPage(
    tags$head(tags$style(
      HTML("input[type='search']:disabled {visibility:hidden}")
    )),
    tabsetPanel(id = "OverallTabs", type = "tabs",
                tabPanel("Query",
                         
                         
                         titlePanel("SNAP Lithics Processing"),
                         fluidRow(
                           column(width = 2,
                                  #these (below) only are submitted once 'query' button is clicked, but then update dynamically as filters are updated         
                                  selectizeInput("LocusType", "Locus Type", choices = c("Context","Transect","Grid","Grab", "XFind"), multiple = FALSE, selected = NULL)),  
                           column(width = 2,
                                  selectizeInput("Locus", "Locus", choices = allloci$Locus, multiple = FALSE, selected = NULL)),
                           column(width = 2,
                                  selectizeInput("Period", "Period", choices = c(periods$Period), multiple = TRUE, selected = NULL)),
                           column(width = 2,
                                  selectizeInput("Blank", "Blank", choices = c(blanks$Blank), multiple = TRUE, selected = NULL)),
                           column(width = 2,
                                  selectizeInput("Modification", "Modification", choices = c(modifications$Modification), multiple = TRUE, selected = NULL)),
                           column(width = 1,
                                  numericInput("Quantity", "Quantity", "1", min=0)),
                           column(3, verbatimTextOutput("x1")),
                           column(3, verbatimTextOutput("x2"))
                         ),
                         actionButton("query", "Query"),
                         hr(),
                         tabsetPanel(id = "myTabs", type = "tabs",
                                     tabPanel("All Level 2",
                                              DT::dataTableOutput("Level2Table"),
                                              uiOutput("popup")
                                     ),
                                     tabPanel("Level 3 Selection",
                                              DT::dataTableOutput("Level3Table")
                                     ),
                                     tabPanel("Photos",
                                              column(width = 2,
                                                     textInput("newPhotoFilename", "Enter complete filename of new photo")),
                                              column(width = 2,
                                                     actionButton("newPhoto", "Add new")),
                                              DT::dataTableOutput("PhotosTable")
                                     ),
                                     tabPanel("Illustrations",
                                              DT::dataTableOutput("IllustrationsTable")
                                     )
                         )
                ),
                
                tabPanel("New Records",
                         titlePanel("Create new records"),
                         fluidRow(
                           column(width = 2,
                                  selectInput("NewLocusType", "Locus Type", choices = c("Context","Transect","Grid","Grab", "XFind"), multiple = FALSE, selected = NULL)),
                           column(width = 2,
                                  selectInput("NewLocus", "Locus", choices = allloci$Locus, multiple = FALSE, selected = NULL)),
                           column(width = 2,
                                  selectInput("NewPeriod", "Period", choices = c(periods$Period), multiple = TRUE, selected = NULL)),
                           column(width = 2,
                                  selectInput("NewBlank", "Blank", choices = c(blanks$Blank), multiple = TRUE, selected = NULL)),
                           column(width = 2,
                                  selectInput("NewModification", "Modification", choices = c(modifications$Modification), multiple = TRUE, selected = NULL)),
                           column(width = 1,
                                  numericInput("NewQuantity", "Quantity", "1"))
                         ),
                         actionButton("submit", "Submit"),
                         actionButton("toggleNewBlankMod", "New Blank or Modification"),
                         uiOutput("newBlankMod"),
                         hr(),
                         tabsetPanel(id = "myTabs", type = "tabs",
                                     tabPanel("All Level 2",
                                              DT::dataTableOutput("Level2Table"),
                                              uiOutput("popup")
                                     ),
                                     tabPanel("Level 3 Selection",
                                              DT::dataTableOutput("Level3Table")
                                     )
                         )
                )
    )
  ),
  
  server <- function(input, output, session){
    #define the fields we want to save from the form
    fields <- c("Locus", "LocusType", "Period", "Blank", "Modification", "Quantity")
    
    # 
    # observe({
    #   Level2 <- dbReadTable(pool, 'level2')
    #   FilteredSelectionsRV <- reactive({
    #     filtered <- Level2
    #     if (!is.null(input$Locus)) {
    #       filtered <- filtered %>% filter(Locus == input$Locus)
    #     }
    #     if (!is.null(input$Blank)) {
    #       filtered <- filtered %>% filter(Blank == input$Blank)
    #     }
    #     if (!is.null(input$Modification)) {
    #       filtered <- filtered %>% filter(Modification == input$Modification)
    #     }
    #     if (!is.null(input$Period)) {
    #       filtered <- filtered %>% filter(Period == input$Period)
    #     }
    #     filtered
    #   })
    #   
    # this second observe() takes filtered results and uses them to populate options for filters
    #   observe({
    #   FilteredSelections <<- FilteredSelectionsRV()
    #   updateSelectInput(session, inputId = "Locus", choices=c(FilteredSelections$Locus))
    #   updateSelectInput(session, inputId = "Period", choices=c(FilteredSelections$Period))
    #   updateSelectInput(session, inputId = "Blank", choices=c(FilteredSelections$Blank))
    #   updateSelectInput(session, inputId = "Modification", choices=c(FilteredSelections$Modification))
    #   })
    # })
    # 
    
    #store content of the input fields
    responses <- data.frame()
    singleResponse <- reactive(
      data.frame(input$Locus, input$LocusType, input$Period, input$Blank, input$Modification, input$Quantity))
    output$x1 <- renderPrint(cat(input$Period))
    #these lines (above) exist only for testing purposes I think
    
    QueryResults <- eventReactive(input$query, {
      Level2 <- dbReadTable(pool, 'level2')
      filtered <- Level2
      if (!is.null(input$Blank)) {
        filtered <- filtered %>% filter(Blank %in% input$Blank)
      }
      if (!is.null(input$Modification)) {
        filtered <- filtered %>% filter(Modification %in% input$Modification)
      }
      if (!is.null(input$Period)) {
        filtered <- filtered %>% filter(Period %in% input$Period)
      }
      filtered
    })
    
    observe({
      Level2 <- dbReadTable(pool, 'level2')
      EmptyDT <- filter(QueryResults(), LocusType=="blah")
      if (identical(QueryResults(), Level2) == TRUE) {
        output$x2 <- renderPrint("identical to Level2")
        output$Level2Table <- renderDataTable(EmptyDT[,-1])
      }
      if (nrow(QueryResults()) == 0) {
        output$x2 <- renderPrint("Here I am, brain the size of a planet, and you ask me to count lithics.  Well, I can't find any that match your criteria.")
        output$Level2Table <- renderDataTable(QueryResults()[,-1])
      }
      if (nrow(QueryResults()) > 0 & nrow(QueryResults()) != nrow(Level2)) {
        QueryResultsxx <- reactive({
          withUpdateButton <- as.data.frame(cbind(Update = shinyInput(actionButton, nrow(QueryResults()), 'button_', label = "Update", onclick = 'Shiny.onInputChange(\"UpdateButton\", this.id)'), QueryResults()))
          withDeleteButton <- as.data.frame(cbind(Delete = shinyInput(actionButton, nrow(QueryResults()), 'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"DeleteButton\", this.id)'), withUpdateButton))
        })
        QueryResults <- QueryResultsxx()
        
        SelectedRow <- eventReactive(input$UpdateButton, {
          as.numeric(strsplit(input$UpdateButton, "_")[[1]][2])
        })
        SelectedRow <- eventReactive(input$DeleteButton, {
          as.numeric(strsplit(input$DeleteButton, "_")[[1]][2])
        })
        
        output$Level2Table <- DT::renderDataTable(
          datatable(QueryResults[,-3], escape = FALSE, rownames = FALSE, selection = list(mode = "single", target = "row")),
          options = list(
            autowidth = TRUE,
            columnDefs = list(list(width = '200px', targets = c(1,2)))
          )
        )#added [,-3] to QueryResults to exclude 'id' column, since including it can I think  only confuse things, still haven't figured out how to control column widths (it's actually a real struggle)
        
        to_index <<- QueryResults[,-c(1,2)]  #this excludes the columns that have been dedicated to buttons, since they will screw up the indexing that follows
        
      }
    })
    
    observe({
      sel <- input$Level2Table_rows_selected
      if (length(sel)) {
        Level2IndexValues <- sel
        Level3Selection <- unlist(to_index[Level2IndexValues, c(2,4,5,6)])
        Level3 <- dbReadTable(pool, 'level3')
        Level3FilterResults <- filter(Level3, Locus==Level3Selection[1] & Period==Level3Selection[2] & Blank==Level3Selection[3] & Modification==Level3Selection[4])
        output$Level3Table <- DT::renderDataTable(
          datatable(Level3FilterResults, selection=list(mode="single", target="cell"), editable = TRUE)) #should eliminate 'id' field from results returned
        
        observe({
          SelectedCells <- input$Level3Table_cells_selected
          if (length(SelectedCells)) {
            Level3 <- dbReadTable(pool, 'level3')
            Level3IndexValues <- ifelse(SelectedCells[1,2]==10, SelectedCells, c(0,0))
            if (Level3IndexValues[1] == 0){
              Photos <- dbReadTable(pool, 'photos')
              PhotosFilterResults <- filter(Photos, ArtefactID=="None")
              output$PhotosTable <- DT::renderDataTable(
                datatable(PhotosFilterResults, selection=list(mode="single", target="row")))
            }
            else {
              PhotosSelection <- unlist(Level3[Level3IndexValues[1], 7])
              Photos <- dbReadTable(pool, 'photos')
              PhotosFilterResults <- filter(Photos, ArtefactID==PhotosSelection)
              output$PhotosTable <- DT::renderDataTable(
                datatable(PhotosFilterResults, selection=list(mode="single", target="row"), editable = TRUE))
              observeEvent(input$newPhoto, {
                newPhotoResponses <- reactiveValues(
                  NewPhoto = input$newPhotoFilename)
                NewPhotoValue <- as.character(newPhotoResponses$NewPhoto)
                Photos <- dbReadTable(pool, 'photos') # re-grab in case of duplicates
                #increment photo ID - get current list, choose highest number, increment, and include in glue_sql statement
                newPhotoInsert <- glue::glue_sql("INSERT INTO `photos` (`Filename`, `ArtefactID`) VALUES ({NewPhotoValue}, {PhotosSelection})"
                                                 , .con = pool)
                dbExecute(pool, sqlInterpolate(ANSI(), newPhotoInsert))
                Photos <- dbReadTable(pool, 'photos')
                PhotosFilterResults <- filter(Photos, ArtefactID==PhotosSelection)
                output$PhotosTable <- DT::renderDataTable(
                  datatable(PhotosFilterResults, selection=list(mode="single", target="row"), editable = TRUE))
              })
            }
            
          }
          else {
            Photos <- dbReadTable(pool, 'photos')
            PhotosFilterResults <- filter(Photos, ArtefactID=="None")
            output$PhotosTable <- DT::renderDataTable(
              datatable(PhotosFilterResults, selection=list(mode="single", target="row")))
          }  
        })
      }
      else {
        Level3 <- dbReadTable(pool, 'level3')
        Level3FilterResults <- filter(Level3, Blank=="None")
        output$Level3Table <- DT::renderDataTable(
          datatable(Level3FilterResults, selection=list(mode="single", target="cell")))
      }
      
      
    })
    
    
    
    #tabIndex <- 1:6
    #observeEvent(input$Level2Table_rows_selected, {
    #  appendTab("myTabs", tabPanel(tabIndex[2]), select = TRUE)
    #})
    #observeEvent(input$removeTab, {
    #  removeTab("myTabs", target=input$myTabs)
    #})
    
    
    
    
    
    
    
    
    #compare proxy to determine whether changes have been made, and only allow changes to be saved or force/prompt changes to be saved if changes are identified
    
    
    
    
    # 
    # 
    # 
    # observeEvent(input$Level3Table_cell_edit, {
    #   info = input$Level3Table_cell_edit
    #   
    #   GrabbedEntitiesRV <- reactiveValues(
    #     i = info$row,
    #     j = info$col = info$col + 1,  # column index offset by 1
    #     v = info$value,
    #     l = QueryResults[info$row,2]
    #   )
    #   
    #   GrabbedEntities <- NULL
    #   Rowx <- GrabbedEntitiesRV$i
    #   Columnx <- GrabbedEntitiesRV$j
    #   Valuex <- GrabbedEntitiesRV$v
    #   Locusx <- GrabbedEntitiesRV$l
    #   GrabbedEntities$Row <- Rowx
    #   GrabbedEntities$Column <- Columnx
    #   GrabbedEntities$Value <- Valuex
    #   GrabbedEntities$Locus <- Locusx
    #   
    #   # if (all(is.na(GrabbedEntities))) {
    #   #   GrabbedEntities <- data.frame(info, stringsAsFactors = FALSE)
    #   # } else {
    #   #   GrabbedEntities <- dplyr::bind_rows(GrabbedEntities, data.frame(info, stringsAsFactors = FALSE))
    #   # }
    #   
    #   Level3Mirror <- reactive({output$Level3Table})
    #   #determine sameness
    # })
    # 
    # observeEvent(input$level3_save, {
    #   updateDB(pool = pool, tbl = "level3")
    #   
    #   # GrabbedLocus <- NULL
    #   # GrabbedColumn <- NULL
    #   # NewValue <- NULL
    #   # GrabbedEntities$Locus <- GrabbedLocus
    #   # GrabbedEntities$Column <- GrabbedColumn
    #   # GrabbedEntities$Value <- NewValue
    #   # #DataSame <- NULL
    #   
    #   Level3 <- dbReadTable(pool, 'level3')
    #   QueryResults <<- QueryResultsRV()
    #   output$Level2Table <- DT::renderDataTable(
    #     datatable(QueryResults, extensions = 'Buttons', filter="top", selection=list(mode="single", target="row")))
    # })
    
    
    
    
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
    
    observeEvent(input$submit, {
      if (input$NewLocusType == "XFind") {
        # parse info pertaining to the xfind's provenance based on its XFindID
        LocusValue <<- reactive({
          z <- toString(input$NewLocus)
          return(z)
        })
        XFindContext <- substr(LocusValue(), 1, 4)
        XFindNumber <- substr(LocusValue(), 5, 8)
        
        #filter for contexts from which the xfind derives
        XFindLevel2 <- dbReadTable(pool, 'level2')
        XFindFilter <- reactive({
          select(filter(XFindLevel2, Period==input$NewPeriod & Blank==input$NewBlank & Modification==input$NewModification & LocusType=="Context" & Locus==XFindContext), Locus, LocusType, Period, Blank, Modification, Quantity)
        })
        observe({
          XFindSubset <<- XFindFilter()
          if (nrow(XFindSubset) == 0) {
            #if no equivalent record exists:
            #write the context to the level2 table
            writeXFind <- dbWriteTable(pool, "level2", XFindSubset, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
            writeXFind
            
            #render data table
            
          }
          
          else {
            #if the equivalent record already exists, update the quantity field to reflect this new xfind
            QuantityPlusOne <- XFindSubset$Quantity + 1
            XFindUpdateQuery1 <- glue::glue_sql("UPDATE `level2` SET
                                                `Quantity` = {QuantityPlusOne}
                                                WHERE `Locus` = {XFindSubset$Locus}
                                                AND `Period` = {XFindSubset$Period}
                                                AND `Blank` = {XFindSubset$Blank}
                                                AND `Modification` = {XFindSubset$Modification}
                                                ", .con = pool)
            dbExecute(pool, sqlInterpolate(ANSI(), XFindUpdateQuery1))
            
            
            #render datatable
            
          }
        })
      }
      
      else {
        #filter for equivalent lociXblankXmodXperiod combinations
        EquivRecord <- dbReadTable(pool, 'level2')
        EquivRecordFilter <- reactive({
          select(filter(EquivRecord, Period==input$NewPeriod & Blank==input$NewBlank & Modification==input$NewModification & LocusType==input$NewLocusType & Locus==input$NewLocus), Locus, LocusType, Period, Blank, Modification, Quantity)
        })
        observe({
          EquivRecordSubset <<- EquivRecordFilter()
          if (nrow(EquivRecordSubset) == 0) {
            #store field values to a responses table after the submit button is clicked
            values <- reactiveValues(singleResponse_df = data.frame(input$NewLocus, input$NewLocusType, input$NewPeriod, input$NewBlank, input$NewModification, input$NewQuantity))
            
            #rename columns in singleResponse_df
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewLocus'] <- 'Locus'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewLocusType'] <- 'LocusType'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewPeriod'] <- 'Period'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewBlank'] <- 'Blank'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewModification'] <- 'Modification'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewQuantity'] <- 'Quantity'
            
            NewRecord <- as.data.frame(values$singleResponse_df, stringsAsFactors = FALSE)
            
            #write the field values to the database as a new record in the level2 table after the submit button is clicked
            #this step, as well as the following dbReadTable command, are very important since they allow the database to allocate an id to the row, which is crucial for updating modified cells
            write_level2 <- dbWriteTable(pool, "level2", NewRecord, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
            write_level2
            
            EquivRecord <- dbReadTable(pool, 'level2')
            EquivRecordFilter <- reactive({
              select(filter(EquivRecord, Period==input$NewPeriod & Blank==input$NewBlank & Modification==input$NewModification & LocusType==input$NewLocusType & Locus==input$NewLocus), Locus, LocusType, Period, Blank, Modification, Quantity)
            })
            EquivRecordSubset <<- EquivRecordFilter()
            
            
            #####!!!! It gets stuck in a loop here, fails to recognize that now there are equivalent records and that this branch of the if/else function should come to an end
            
            #render datatable
            
          }
          
          else {
            #if the equivalent record already exists, update the quantity field to reflect this new addition to the existing record
            #store field values to a responses table after the submit button is clicked
            valuesx <- reactiveValues(singleResponse_dfx = data.frame(input$NewLocus, input$NewLocusType, input$NewPeriod, input$NewBlank, input$NewModification, input$NewQuantity))
            
            #rename columns in singleResponse_df
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewLocus'] <- 'Locus'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewLocusType'] <- 'LocusType'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewPeriod'] <- 'Period'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewBlank'] <- 'Blank'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewModification'] <- 'Modification'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewQuantity'] <- 'Quantity'
            toAdd <- as.data.frame(valuesx$singleResponse_dfx)
            
            EquivRecordQuantityUpdated <- EquivRecordSubset$Quantity + toAdd$Quantity
            EquivRecordUpdateQuery1 <- glue::glue_sql("UPDATE `level2` SET
                                                      `Quantity` = {EquivRecordQuantityUpdated}
                                                      WHERE `Locus` = {EquivRecordSubset$Locus}
                                                      AND `Period` = {EquivRecordSubset$Period}
                                                      AND `Blank` = {EquivRecordSubset$Blank}
                                                      AND `Modification` = {EquivRecordSubset$Modification}
                                                      ", .con = pool)
            dbExecute(pool, sqlInterpolate(ANSI(), EquivRecordUpdateQuery1))
            
            #render table
            
          }
        })
      }
      
      #upon submitting field values, expand the response based on the value on the Quantity field
      #the expandRows command is part of the splitstackshape package
      ValuesToExpand <- reactiveValues(singleResponse_df = data.frame(input$NewLocus, input$NewLocusType, input$NewPeriod, input$NewBlank, input$NewModification, input$NewQuantity))
      toExpand <- as.data.frame(ValuesToExpand$singleResponse_df)
      singleRow_expanded <- expandRows(toExpand, count = 6, count.is.col = TRUE, drop = TRUE)
      #still needs a mechanism that adds or removes records/rows if the Quantity field is updated after editing the data table
      
      #add columns to the expanded table
      singleRow_expanded$ArtefactID <- ""
      singleRow_expanded$WrittenOnArtefact <- ""
      singleRow_expanded$Illustration <- ""
      singleRow_expanded$RawMaterial <- ""
      singleRow_expanded$WeatheringIndex <- ""
      singleRow_expanded$Patination <- ""
      singleRow_expanded$Notes <- ""
      singleRow_expanded <- singleRow_expanded[c(0:12)]
      singleRow_expanded <- data.frame(lapply(singleRow_expanded, as.character), stringsAsFactors = FALSE)
      
      #rename columns in the expanded table
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocus'] <- 'Locus'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocusType'] <- 'LocusType'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewPeriod'] <- 'Period'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewBlank'] <- 'Blank'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewModification'] <- 'Modification'
      
      #write the new expanded rows to the level3 table in the database
      write_level3 <- dbWriteTable(pool, "level3", singleRow_expanded, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
      write_level3
      
      #render table
      
    })
    
    #-----/NewRecords-----#
    
    
  }
)
shinyApp(ui, server)
