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
#current <- getwd()
#setwd("/Users/danielcontreras/Documents/SNAP/RShiny_DBinterface/")
source("keys2.R") 
# setwd(current) #when done


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

# js <- glue::glue(
#   "$('[id^=check]').on('click', function(){
#   var id = this.getAttribute('id');
#   var i = parseInt(/check(\\d+)/.exec(id)[1]);
#   var value = $(this).prop('checked');
#   var cell = table.cell(i-1, 2).data(value).draw();
#   })"
# )

# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}

######## shinyInputX <- function(FUN, len, id, ...) {inputs <- character(len)
# for (i in seq_len(len)) {
#   inputs[i] <- as.character(checkboxInput(paste0(id, i), label=NULL,
#                                           value = FALSE))
#                                           }
# inputs
# }

# shinyCheckboxes <- function(len, id, checked){
#   inputs <- character(len)
#   for (i in seq_len(len)) {
#     inputs[i] <- as.character(checkboxInput(paste0(id, i), label=NULL,
#                                             value = FALSE))
#   }
#   inputs
# }



#read unmodified versions of allloci, blanks and modifications tables specifically for the input selection
allloci <- dbReadTable(pool, 'allloci')
allloci
blanks <- dbReadTable(pool, 'blanks_excavation')
blanks
modifications <- dbReadTable(pool, 'modifications_excavation')
modifications
periods <- dbReadTable(pool, 'dating')
periods
activitylog <- dbReadTable(pool, 'activitylog')
activitylog

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
                                  selectizeInput("Locus", "Locus", choices = c("", allloci$Locus), multiple = TRUE, selected = "")),
                           column(width = 2,
                                  selectizeInput("Period", "Period", choices = c("", periods$Period), multiple = TRUE, selected = "")),
                           column(width = 2,
                                  selectizeInput("Blank", "Blank", choices = c("", blanks$Blank), multiple = TRUE, selected = "")),
                           column(width = 2,
                                  selectizeInput("Modification", "Modification", choices = c("", modifications$Modification), multiple = TRUE, selected = "")),
                           column(width = 1,
                                  numericInput("Quantity", "Quantity", "1", min=0)),
                           column(3, verbatimTextOutput("x1")),
                           column(3, verbatimTextOutput("x2"))
                         ),
                         actionButton("query", "Query"),
                         hr(),
                         tabsetPanel(id = "myTabs", type = "tabs",
                                     tabPanel("Level 2 Selection",
                                              DT::dataTableOutput("Level2Table")
                                     ),
                                     tabPanel("Level 3 Selection",
                                              DT::dataTableOutput("Level3Table")
                                     ),
                                     tabPanel("Photos",
                                              fluidRow(
                                                column(width = 6,
                                                       textInput("newPhotoFilename", "Enter complete filename of new photo"))),
                                              fluidRow(
                                                column(width = 2,
                                                       actionButton("newPhoto", "Add new"))),
                                              hr(),
                                              DT::dataTableOutput("PhotosTable")
                                     ),
                                     tabPanel("Illustrations",
                                              DT::dataTableOutput("IllustrationsTable")
                                     )
                         )
                ),
                
                tabPanel("Create Records",
                         titlePanel("Create New Records"),
                         fluidRow(
                           column(width = 2,
                                  selectInput("NewLocusType", "Locus Type", choices = c("Context","Transect","Grid","Grab", "XFind"), multiple = FALSE, selected = NULL)),
                           column(width = 2,
                                  selectizeInput("NewLocus", "Locus", choices = c("", allloci$Locus), multiple = FALSE, selected = "")),
                           column(width = 2,
                                  selectizeInput("NewPeriod", "Period", choices = c("", periods$Period), multiple = FALSE, selected = "")),
                           column(width = 2,
                                  selectizeInput("NewBlank", "Blank", choices = c("", blanks$Blank), multiple = FALSE, selected = "")),
                           column(width = 2,
                                  selectizeInput("NewModification", "Modification", choices = c("", modifications$Modification), multiple = FALSE, selected = "")),
                           column(width = 1,
                                  numericInput("NewQuantity", "Quantity", "1"))
                         ),
                         actionButton("submit", "Submit"),
                         actionButton("toggleNewBlankMod", "New Blank or Modification"),
                         uiOutput("newBlankMod"),
                         hr(),
                         h2("All Records"),
                         column(12, verbatimTextOutput("x3", placeholder = TRUE)),
                         tabsetPanel(id = "myTabsx", type = "tabs",
                                     tabPanel("All Level 2",
                                              DT::dataTableOutput("NewLevel2Table")
                                     ),
                                     tabPanel("All Level 3",
                                              DT::dataTableOutput("NewLevel3Table")
                                     )
                         )
                ),
                tabPanel("Activity Log",
                         titlePanel("Activity Log"),
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
    
    #-----QueryLookup-----#
    
    QueryResults <- function(QueryInputs) {
      Level2 <- dbReadTable(pool, 'level2')
      filtered <- Level2
      if (QueryInputs$Locus != "") {
        filtered <- filtered %>% filter(Locus %in% QueryInputs$Locus)
      }
      if (QueryInputs$Blank != "") {
        filtered <- filtered %>% filter(Blank %in% QueryInputs$Blank)
      }
      if (QueryInputs$Modification != "") {
        filtered <- filtered %>% filter(Modification %in% QueryInputs$Modification)
      }
      if (QueryInputs$Period != "") {
        filtered <- filtered %>% filter(Period %in% QueryInputs$Period)
      }
      filtered
    }
    # QueryResults <- function(QueryInputs) {
    #   Level2 <- dbReadTable(pool, 'level2')
    #   filtered <- Level2
    #   if (!is.null(QueryInputs$Locus)) {
    #     filtered <- filtered %>% filter(Locus %in% QueryInputs$Locus)
    #   }
    #   if (!is.null(QueryInputs$Blank)) {
    #     filtered <- filtered %>% filter(Blank %in% QueryInputs$Blank)
    #   }
    #   if (!is.null(QueryInputs$Modification)) {
    #     filtered <- filtered %>% filter(Modification %in% QueryInputs$Modification)
    #   }
    #   if (!is.null(QueryInputs$Period)) {
    #     filtered <- filtered %>% filter(Period %in% QueryInputs$Period)
    #   }
    #   filtered
    # }
    # 
    # QueryResults <- eventReactive(input$query, {
    #   Level2 <- dbReadTable(pool, 'level2')
    #   filtered <- Level2
    #   if (!is.null(input$Locus)) {
    #     filtered <- filtered %>% filter(Locus %in% input$Locus)
    #   }
    #   if (!is.null(input$Blank)) {
    #     filtered <- filtered %>% filter(Blank %in% input$Blank)
    #   }
    #   if (!is.null(input$Modification)) {
    #     filtered <- filtered %>% filter(Modification %in% input$Modification)
    #   }
    #   if (!is.null(input$Period)) {
    #     filtered <- filtered %>% filter(Period %in% input$Period)
    #   }
    #   filtered
    # })
    
    observeEvent(input$query, {
      #QueryInputs  <<- data.frame(Locus = NULL, Blank = NULL, Modification = NULL, Period = NULL)   
      QueryTermLocus <- ifelse(!is.null(input$Locus), input$Locus, "")
      QueryTermPeriod <- ifelse(!is.null(input$Period), input$Period, "")
      QueryTermBlank <- ifelse(!is.null(input$Blank), input$Blank, "")
      QueryTermModification <- ifelse(!is.null(input$Modification), input$Modification, "")
      QueryInputs <<- data.frame(Locus = QueryTermLocus, Blank = QueryTermBlank, Modification = QueryTermModification, Period = QueryTermPeriod)
      #QueryInputs  <<- data.frame(Locus = ifelse(!is.null(input$Locus), input$Locus, NULL), Blank = ifelse(!is.null(input$Blank), input$Blank, NULL), Modification = ifelse(!is.null(input$Modification), input$Modification, NULL), Period = ifelse(!is.null(input$Period), input$Period, NULL))
      CurrentResults <- QueryResults(QueryInputs)
      Level2 <- dbReadTable(pool, 'level2')
      EmptyDT <- filter(CurrentResults, Locus=="blah")
      
      if (nrow(CurrentResults) == nrow(Level2)) {
        output$Level2Table <- renderDataTable(datatable(EmptyDT[,-1]))
        output$x2 <- renderPrint("Identical to the complete set of Level2 records")
      }
      
      if (nrow(CurrentResults) == 0) {
        output$x2 <- renderPrint("Here I am, brain the size of a planet, and you ask me to count lithics.  Well, I can't find any that match your criteria.")
        output$Level2Table <- renderDataTable(datatable(CurrentResults[,-1]))
      }
      
      if (nrow(CurrentResults) > 0 & nrow(CurrentResults) != nrow(Level2)) {
        QueryResultsxx <- reactive({
          withEditButton <- as.data.frame(cbind(Edit = shinyInput(actionButton, nrow(CurrentResults), 'button_', label = "Edit", onclick = 'Shiny.onInputChange(\"EditButton\", this.id)'), CurrentResults))
        })
        
        QueryResultsXX <<- QueryResultsxx()
        
        SelectedRowEdit <<- eventReactive(input$EditButton, {
          as.numeric(strsplit(input$EditButton, "_")[[1]][2])
        })
        
        
        output$Level2Table <- DT::renderDataTable(
          datatable(QueryResultsXX[,-2], escape = FALSE, rownames = FALSE, selection = list(mode = "single", target = "row"), editable = TRUE, options = list(autowidth = TRUE,columnDefs = list(list(targets=c(0,1,6,8), width='50'), list(targets=c(2,3,4,5), width='100')))))
        to_index <<- QueryResultsXX[,-c(1)]
      }
    })
    
    
    
    #-----/QueryLookup-----#
    
    #-----TableFilters-----#
    observe({
      sel <- input$Level2Table_rows_selected
      if (length(sel)) {
        Level2IndexValues <- sel
        Level3Selection <- unlist(to_index[Level2IndexValues, c(2,4,5,6)])
        Level3 <- dbReadTable(pool, 'level3')
        Level3FilterResults <- filter(Level3, Locus==Level3Selection[1] & Period==Level3Selection[2] & Blank==Level3Selection[3] & Modification==Level3Selection[4])
        output$Level3Table <- DT::renderDataTable(
          datatable(Level3FilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="cell"), editable = TRUE)) #should eliminate 'id' field from results returned
        
        observe({
          SelectedCells <- input$Level3Table_cells_selected
          if (length(SelectedCells)) {
            Level3 <- dbReadTable(pool, 'level3')
            Level3IndexValues <- ifelse(SelectedCells[1,2]==10, SelectedCells, c(0,0))
            if (Level3IndexValues[1] == 0){
              Photos <- dbReadTable(pool, 'photos')
              PhotosFilterResults <- filter(Photos, ArtefactID=="None")
              output$PhotosTable <- DT::renderDataTable(
                datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row")))
            }
            else {
              PhotosSelection <- unlist(Level3[Level3IndexValues[1], 7])
              Photos <- dbReadTable(pool, 'photos')
              PhotosFilterResults <- filter(Photos, ArtefactID==PhotosSelection)
              output$PhotosTable <- DT::renderDataTable(
                datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row"), editable = TRUE))
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
                  datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row"), editable = TRUE))
              })
            }
          }
          else {
            Photos <- dbReadTable(pool, 'photos')
            PhotosFilterResults <- filter(Photos, ArtefactID=="None")
            output$PhotosTable <- DT::renderDataTable(
              datatable(PhotosFilterResults[,-1], rownames = FALSE, selection=list(mode="single", target="row")))
          }  
        })
      }
      else {
        Level3 <- dbReadTable(pool, 'level3')
        Level3FilterResults <- filter(Level3, Blank=="None")
        output$Level3Table <- DT::renderDataTable(
          datatable(Level3FilterResults[-1], rownames = FALSE, selection=list(mode="single", target="cell")))
      }
      
      
    })
    
    
    # tabIndex <- 1:6
    # observeEvent(input$Level3Table_cells_selected, {
    #   showTab(inputId = "MyTabs", target = "Photos")
    #   appendTab("myTabs", tabPanel(tabIndex[2]), select = TRUE)
    # })
    # observeEvent(input$removeTab, {
    #   removeTab("myTabs", target=input$myTabs)
    # })
    # 
    
    #-----/TableFilters-----#
    
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
      query <- reactive({
        if (input$NewLocusType != "XFind") {
          EquivRecord <- dbReadTable(pool, 'level2')
          EquivRecordFilter <- data.frame()
          EquivRecordFilter <- reactive({
            select(filter(EquivRecord, Period==input$NewPeriod & Blank==input$NewBlank & Modification==input$NewModification & LocusType==input$NewLocusType & Locus==input$NewLocus), Locus, LocusType, Period, Blank, Modification, Quantity)
          })
          EquivRecordFilter_df <<- EquivRecordFilter()
          
          if (nrow(EquivRecordFilter_df) == 0) {
            values <- reactiveValues(singleResponse_df = data.frame(input$NewLocus, input$NewLocusType, input$NewPeriod, input$NewBlank, input$NewModification, input$NewQuantity))
            
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewLocus'] <- 'Locus'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewLocusType'] <- 'LocusType'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewPeriod'] <- 'Period'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewBlank'] <- 'Blank'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewModification'] <- 'Modification'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.NewQuantity'] <- 'Quantity'
            
            NewRecord <<- as.data.frame(values$singleResponse_df, stringsAsFactors = FALSE)
            NewRecord$Locus <- as.character(NewRecord$Locus)
            NewRecord$LocusType <- as.character(NewRecord$LocusType)
            NewRecord$Period <- as.character(NewRecord$Period)
            NewRecord$Blank <- as.character(NewRecord$Blank)
            NewRecord$Modification <- as.character(NewRecord$Modification)
            NewRecord$Quantity <- as.numeric(NewRecord$Quantity)
            
            WriteNewLevel2Record <- glue::glue_sql("INSERT INTO `level2` (`Locus`, `LocusType`, `Period`, `Blank`, `Modification`, `Quantity`) VALUES ({NewRecord$Locus}, {NewRecord$LocusType}, {NewRecord$Period}, {NewRecord$Blank}, {NewRecord$Modification}, {NewRecord$Quantity})", .con = pool)
            
            WriteNewLevel2Record <<- as.character(WriteNewLevel2Record)
            WriteNewLevel2Record
            queryx <<- as.character(WriteNewLevel2Record)
            queryx
            UpdateExistingLevel2Record <<- NULL
            
            message1 <- paste0("New record added to Level2 table: ","[",NewRecord$LocusType,"/",NewRecord$Locus,"/",NewRecord$Period,"/",NewRecord$Blank,"/",NewRecord$Modification,"/",NewRecord$Quantity,"].")
            activitylog <- dbReadTable(pool, 'activitylog')
            activitylog <- data.frame(Log = message1,
                                      Timestamp = as.character(Sys.time()),
                                      stringsAsFactors = FALSE)
            writeActivity <- dbWriteTable(pool, 'activitylog', activitylog, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
            writeActivity
            activitylog <<- dbReadTable(pool, 'activitylog')
            activitylog
            output$x3 <- renderPrint(message1)
            
          }
          
          if (nrow(EquivRecordFilter_df) > 0) {
            valuesx <- reactiveValues(singleResponse_dfx = data.frame(input$NewLocus, input$NewLocusType, input$NewPeriod, input$NewBlank, input$NewModification, input$NewQuantity))
            
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewLocus'] <- 'Locus'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewLocusType'] <- 'LocusType'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewPeriod'] <- 'Period'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewBlank'] <- 'Blank'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewModification'] <- 'Modification'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.NewQuantity'] <- 'Quantity'
            
            toAdd <- as.data.frame(valuesx$singleResponse_dfx, stringsAsFactors = FALSE)
            
            EquivRecordQuantityUpdated <- EquivRecordFilter_df$Quantity + toAdd$Quantity
            UpdateExistingLevel2Record <- glue::glue_sql("UPDATE `level2` SET `Quantity` = {EquivRecordQuantityUpdated} WHERE `Locus` = {EquivRecordFilter_df$Locus} AND `Period` = {EquivRecordFilter_df$Period} AND `Blank` = {EquivRecordFilter_df$Blank} AND `Modification` = {EquivRecordFilter_df$Modification}", .con = pool)
            
            UpdateExistingLevel2Record <<- as.character(UpdateExistingLevel2Record)
            UpdateExistingLevel2Record
            queryx <<- as.character(UpdateExistingLevel2Record)
            queryx
            WriteNewLevel2Record <<- NULL
            
            message2 <- paste0(toAdd$Quantity," lithics added to existing batch of ",EquivRecordFilter_df$Quantity," records with configuration: [",EquivRecordFilter_df$LocusType,"/",EquivRecordFilter_df$Locus,"/",EquivRecordFilter_df$Period,"/",EquivRecordFilter_df$Blank,"/",EquivRecordFilter_df$Modification,"]. There are now ",EquivRecordQuantityUpdated," records of that configuration.")
            activitylog <- dbReadTable(pool, 'activitylog')
            activitylog <- data.frame(Log = message2,
                                      Timestamp = as.character(Sys.time()),
                                      stringsAsFactors = FALSE)
            writeActivity <- dbWriteTable(pool, 'activitylog', activitylog, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
            writeActivity
            activitylog <<- dbReadTable(pool, 'activitylog')
            activitylog
            output$x3 <- renderPrint(message2)
            
          }
        }
        else {
          #do the xfind stuff here
        }
      })
      
      query <<- query()
      dbExecute(pool, sqlInterpolate(ANSI(), queryx))
      
      
      ValuesToExpand <- reactiveValues(singleResponse_df = data.frame(input$NewLocus, input$NewLocusType, input$NewPeriod, input$NewBlank, input$NewModification, input$NewQuantity))
      toExpand <- as.data.frame(ValuesToExpand$singleResponse_df)
      singleRow_expanded <- expandRows(toExpand, count = 6, count.is.col = TRUE, drop = TRUE)
      
      singleRow_expanded$ArtefactID <- ""
      singleRow_expanded$WrittenOnArtefact <- ""
      singleRow_expanded$Illustration <- ""
      singleRow_expanded$RawMaterial <- ""
      singleRow_expanded$WeatheringIndex <- ""
      singleRow_expanded$Patination <- ""
      singleRow_expanded$Notes <- ""
      singleRow_expanded <- singleRow_expanded[c(0:12)]
      singleRow_expanded <- data.frame(lapply(singleRow_expanded, as.character), stringsAsFactors = FALSE)
      
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocus'] <- 'Locus'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewLocusType'] <- 'LocusType'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewPeriod'] <- 'Period'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewBlank'] <- 'Blank'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.NewModification'] <- 'Modification'
      
      write_level3 <- dbWriteTable(pool, "level3", singleRow_expanded, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
      write_level3
      
      Level2 <- dbReadTable(pool, 'level2')
      output$NewLevel2Table <- DT::renderDataTable(
        datatable(Level2[,-1], rownames = FALSE))
      
      Level3 <- dbReadTable(pool, 'level3')
      output$NewLevel3Table <- DT::renderDataTable(
        datatable(Level3[,-1], rownames = FALSE))
      
      
      
    })
    
    #-----/NewRecords-----#
    
    #-----EditRecords-----#
    
    Level2_rvs <- reactiveValues(
      data = NA,
      dbdata = NA,
      dataSame = TRUE,
      editedInfo = NA
    )
    Level2_mysource <- reactive({
      pool %>% tbl("level2") %>% collect()
    })
    observeEvent(Level2_mysource(), {
      Level2_data <- Level2_mysource() %>% arrange(id)
      Level2_rvs$data <- Level2_data
      Level2_rvs$dbdata <- Level2_data
    })
    proxy1 = dataTableProxy('QueryResultsXX')
    
    observeEvent(input$Level2Table_cell_edit, {
      info = input$Level2Table_cell_edit
      i = info$row
      j = info$col
      v = info$value
      info$id <- to_index$id
      str(info)
      
      Level2_rvs$data[i, j] <<- DT::coerceValue(v, dplyr::pull(Level2_rvs$data[i, j]))
      replaceData(proxy1, Level2_rvs$data, resetPaging = FALSE, rownames = FALSE)
      Level2_rvs$dataSame <- identical(Level2_rvs$data, Level2_rvs$dbdata)
      if (all(is.na(Level2_rvs$editedInfo))) {
        Level2_rvs$editedInfo <- data.frame(info, stringsAsFactors = FALSE)
      } else {
        Level2_rvs$editedInfo <- dplyr::bind_rows(Level2_rvs$editedInfo, data.frame(info, stringsAsFactors = FALSE))
      }
    })
    
    
    
    observeEvent(input$EditButton, {
      sel <- SelectedRowEdit()
      if (length(sel)) {
        Level2IndexValues <- sel
        Level3SelectionEdit <- unlist(to_index[Level2IndexValues, c(2,4,5,6)])
        Level3 <- dbReadTable(pool, 'level3')
        Level2FilterResultsEdit <<- filter(Level2, Locus==Level3SelectionEdit[1] & Period==Level3SelectionEdit[2] & Blank==Level3SelectionEdit[3] & Modification==Level3SelectionEdit[4])
        Level3FilterResultsEdit <<- filter(Level3, Locus==Level3SelectionEdit[1] & Period==Level3SelectionEdit[2] & Blank==Level3SelectionEdit[3] & Modification==Level3SelectionEdit[4])
      }
      
      # for (i in seq_len(nrow(Level3FilterResultsEdit))) {
      #   Level3FilterResultsEdit[i, ] = sprintf(
      #     '<input type = "radio" name = "%s" value = "%s" />',
      #     Level3FilterResultsEdit[i, ]
      #   )
      # }
      # Level3FilterResultsEdit
      
      showModal(modalDialog(title = paste0("Edit record for ",Level2FilterResultsEdit$LocusType," ",Level2FilterResultsEdit$Locus,""),
                            fluidRow(
                              column(width = 2,
                                     output$ModalLocusType <- renderText(Level2FilterResultsEdit$LocusType)),
                              column(width = 2,
                                     output$ModalLocus <- renderText(Level2FilterResultsEdit$Locus)),
                              column(width = 2,
                                     output$ModalPeriod <- renderText(Level2FilterResultsEdit$Period)),
                              column(width = 2,
                                     output$ModalBlank <- renderText(Level2FilterResultsEdit$Blank)),
                              column(width = 2,
                                     output$ModalModification <- renderText(Level2FilterResultsEdit$Modification)),
                              column(width = 2,
                                     output$ModalQuantity <- renderText(Level2FilterResultsEdit$Quantity))
                            ),
                            br(),
                            h3("Change Level 2 record to..."),
                            br(),
                            fluidRow(
                              column(width = 2,
                                     selectInput("ModalLocusTypeSelect", "Locus Type", choices = c("", "Context", "Transect", "Grid", "Grab", "XFind"), multiple = FALSE, selected = "")),
                              column(width = 2,
                                     selectInput("ModalLocusSelect", "Locus", choices = c("", allloci$Locus), multiple = FALSE, selected = "")),
                              column(width = 2,
                                     selectInput("ModalPeriodSelect", "Period", choices = c("", periods$Period), multiple = FALSE, selected = "")),
                              column(width = 2,
                                     selectInput("ModalBlankSelect", "Blank", choices = c("", blanks$Blank), multiple = FALSE, selected = "")),
                              column(width = 2,
                                     selectInput("ModalModificationSelect", "Modification", choices = c("", modifications$Modification), multiple = FALSE, selected = ""))
                              ),
                            hr(),
                            #Level3FilterResults$logicalColumn <- "", #Build hidden logical columns for conditional formatting
                            # background <- "Level3FilterResults$logicalColumn == 'NewRow' ? 'green' : Level3FilterResults$logicalColumn == 'DeletedRow' ? 'red' : Level3FilterResults$logicalColumn == 'ModifiedRow' ? 'blue' : ''",
                            # class(background) <- "JS_EVAL",
                            
                            #add checkbox for creation of a level3 subset
                            
                            h3("Batch update Level 3 records"),
                            fluidRow(
                              column(width = 2,
                                     selectInput("ModalRawMaterialSelect", "Raw Material", choices = c("", "Type A", "Type B", "Type C"," Type D", "Type E"), multiple = FALSE, selected = "")),
                              column(width = 3,
                                     selectInput("ModalWeatheringIndexSelect", "Weathering Index", choices = c("", "Not Weathered", "Weathered", "Very Weathered", "Extremely Weathered"), multiple = FALSE, selected = "")),
                              column(width = 2,
                                     selectInput("ModalPatinationSelect", "Patination", choices = c("", "1", "2", "3"), multiple = FALSE, selected = "")),
                              column(width = 2,
                                     numericInput("ModalQuantitySelect", "Quantity", "", min=0)),
                              column(width = 2,
                                     actionButton('SaveBatch',"Save Batch")),
                              column(12, verbatimTextOutput("BatchErrors", placeholder = TRUE)),
                              uiOutput("BatchErrorActions")
                            ),
                            
                            hr(),
                            DT::renderDataTable(datatable(Level3FilterResultsEdit[,-c(1:6)], rownames = FALSE, selection=list(mode="single", target="cell"), editable = TRUE, escape = FALSE
                              #                             , callback = JS(js)
                              #                             , options = list(
                              # dom = 't', paging = FALSE, ordering = FALSE,
                              # preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                              # drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                              # ), {
                              #   Level3FilterResultsEdit$value_check <- shinyInputX(checkboxInput, nrow(Level3FilterResultsEdit), "check")
                              #   Level3FilterResultsEdit
                              # }
                            
                            # "table.rows().every(function(i, tab, row) {
                            # var $this = $(this.node());
                            # $this.attr('id', this.data()[0]);
                            # $this.addClass('shiny-input-radiogroup');
                            # });
                            # Shiny.unbindAll(table.table().node());
                            # Shiny.bindAll(table.table().node());"
                            )
                            
                            ),
                            footer = tagList(
                              actionButton('CancelEdit',"Cancel"),
                              actionButton('ConfirmEdit',"Confirm")),
                            easyClose = FALSE,
                            size = "l",
                            fade = FALSE
      ))
      
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
    })
    
    
    # if (LocusTypeSame == 0 | LocusSame == 0 | PeriodSame == 0 | BlankSame == 0 | ModificationSame == 0 | QuantitySame == 0) {
    #   #identify rows that would be changed and highlight them by changing their background colour
    #use a helper column containing values that indicate whether or not the row contains changed values
    # background <- "value == '0001' ? 'orange' : value != 'else' ? 'blue' : ''",
    # class(background) <- "JS_EVAL",
    # 
    # datatable(Level3FilterResults) %>% formatStyle(
    #   'Locus',
    #   target = 'row',
    #   backgroundColor = background
    # ),
    #   },
    
    #subset the Level3FilterResults to include only records that have a checkbox selected
    
    observeEvent(input$SaveBatch, {
      
      Level3ToBatch <- Level3FilterResultsEdit
      Level3ToBatch <- Level3ToBatch %>% filter(RawMaterial %in% input$ModalRawMaterialSelect)
      Level3ToBatch <- Level3ToBatch %>% filter(WeatheringIndex %in% input$ModalWeatheringIndexSelect)
      Level3ToBatch <- Level3ToBatch %>% filter(Patination %in% input$ModalPatinationSelect)
      Level3ToBatch <- Level3ToBatch %>% filter(WrittenOnArtefact %in% "No" | is.null(WrittenOnArtefact))
      
      if (nrow(Level3ToBatch) >= input$ModalQuantitySelect) {
        Level3TruncatedBatch <- head(Level3ToBatch, n = input$ModalQuantitySelect)
        if (!is.null(input$ModalRawMaterialSelect)) {
        BatchUpdateLevel3Query_RawMaterial <- glue::glue_sql("UPDATE `level3` SET `RawMaterial` = {input$ModalRawMaterialSelect} WHERE id = {Level3TruncatedBatch$id}", .con = pool)
        dbExecute(pool, sqlInterpolate(ANSI(), BatchUpdateLevel3Query_RawMaterial))
        }
        if (!is.null(input$ModalWeatheringIndexSelect)) {
        BatchUpdateLevel3Query_WeatheringIndex <- glue::glue_sql("UPDATE `level3` SET `WeatheringIndex` = {input$ModalWeatheringIndexSelect} WHERE id = {Level3TruncatedBatch$id}", .con = pool)
        dbExecute(pool, sqlInterpolate(ANSI(), BatchUpdateLevel3Query_WeatheringIndex))
        }
        if (!is.null(input$ModalPatinationSelect)) {
        BatchUpdateLevel3Query_Patination <- glue::glue_sql("UPDATE `level3` SET `Patination` = {input$ModalPatinationSelect} WHERE id = {Level3TruncatedBatch$id}", .con = pool)
        dbExecute(pool, sqlInterpolate(ANSI(), BatchUpdateLevel3Query_Patination))
        }
      }
      else {
        BatchDifference <- nrow(input$ModalQuantitySelect) - Level3ToBatch
        UpdatedLevel2Quantity <- BatchDifference + Level2FilterResultsEdit$Quantity
        output$BatchErrors <- renderPrint("The quantity that you specified is more than the number of records that correspond with the selected Level 2 record [",Level2FilterResultsEdit$Quantity,"]. Click 'Add the difference' to create ",BatchDifference," additional Level 3 records and increase the Level 2 quantity to ",UpdatedLevel2Quantity,".")
        renderUI(
          fluidRow(
            column(width = 2,
                   actionButton('AddDifference','Add the difference')),
            column(width = 2,
                   actionButton('ClearBatchInputs','Clear Batch Inputs'))
          )
        )
      }
    })
    
    observeEvent(input$AddDifference, {
    #create level2 records and corresponding level3 records
      })
    
    observeEvent(input$ClearBatchInputs, {
      updateSelectizeInput(session, "ModalRawMaterialSelect", selected = "")
      updateSelectizeInput(session, "ModalWeatheringIndexSelect", selected = "")
      updateSelectizeInput(session, "ModalPatinationSelect", selected = "")
      updateSelectizeInput(session, "ModalQuantitySelect", selected = "")
    })
    
    observeEvent(input$ConfirmEdit, {
      if (nrow(dplyr::filter(Level3FilterResultsEdit, Level3FilterResultsEdit$Selection == "1")) > 0) {
        Level3Subset <<- dplyr::filter(Level3FilterResultsEdit, Level3FilterResultsEdit$Selection == "1")
        if (LocusTypeSame == FALSE) { #repeat this for all the fields
          UpdateLevel2Records_LocusType <- glue::glue_sql("UPDATE `level2` SET `LocusType` = {LocusTypeAfter} WHERE id = {Level2FilterResultsEdit$id}", .con = pool)
          #identify the `id` of each record in the Level3Subset and use that
          UpdateCorrespondingLevel3Records_LocusType <- glue::glue_sql("UPDATE `level3` SET `LocusType` = {LocusTypeAfter} WHERE id = {Level3Subset$id}", .con = pool)
          dbExecute(pool, sqlInterpolate(ANSI(), UpdateLevel2Records_LocusType))
          dbExecute(pool, sqlInterpolate(ANSI(), UpdateCorrespondingLevel3Records_LocusType))
          
          #index the changes, stage them for an update query upon pressing the confirm button
        }
        
        if (LocusSame == FALSE) { #repeat this for all the fields
          UpdateLevel2Records_Locus <- glue::glue_sql("UPDATE `level2` SET `Locus` = {LocusAfter} WHERE id = {Level2FilterResultsEdit$id}", .con = pool)
          #identify the `id` of each record in the Level3Subset and use that
          UpdateCorrespondingLevel3Records_Locus <- glue::glue_sql("UPDATE `level3` SET `Locus` = {LocusAfter} WHERE id = {Level3Subset$id}", .con = pool)
          dbExecute(pool, sqlInterpolate(ANSI(), UpdateLevel2Records_Locus))
          dbExecute(pool, sqlInterpolate(ANSI(), UpdateCorrespondingLevel3Records_Locus))
          
          #index the changes, stage them for an update query upon pressing the confirm button
        }
        
      }
      
      
      
      
      
      updateSelectizeInput(session, "LocusType", selected = LocusTypeAfter)
      updateSelectizeInput(session, "Locus", selected = LocusAfter)
      updateSelectizeInput(session, "Period", selected = PeriodAfter)
      updateSelectizeInput(session, "Blank", selected = BlankAfter)
      updateSelectizeInput(session, "Modification", selected = ModificationAfter)
      
      QueryTermLocusType <- ifelse(!is.null(input$LocusType), input$LocusType, "")
      QueryTermLocus <- ifelse(!is.null(input$Locus), input$Locus, "")
      QueryTermPeriod <- ifelse(!is.null(input$Period), input$Period, "")
      QueryTermBlank <- ifelse(!is.null(input$Blank), input$Blank, "")
      QueryTermModification <- ifelse(!is.null(input$Modification), input$Modification, "")
      QueryInputs <<- data.frame(LocusType = QueryTermLocusType, Locus = QueryTermLocus, Blank = QueryTermBlank, Modification = QueryTermModification, Period = QueryTermPeriod)
      CurrentResults <- QueryResults(QueryInputs)
      
      Level2 <- dbReadTable(pool, 'level2')
      EmptyDT <- filter(CurrentResults, LocusType=="blah")
      
      if (nrow(CurrentResults) == nrow(Level2)) {
        output$Level2Table <- renderDataTable(datatable(EmptyDT[,-1]))
        output$x2 <- renderPrint("Identical to the complete set of Level2 records")
      }
      
      if (nrow(CurrentResults) == 0) {
        output$x2 <- renderPrint("Here I am, brain the size of a planet, and you ask me to count lithics.  Well, I can't find any that match your criteria.")
        output$Level2Table <- renderDataTable(datatable(CurrentResults[,-1]))
      }
      
      if (nrow(CurrentResults) > 0 & nrow(CurrentResults) != nrow(Level2)) {
        QueryResultsxx <- reactive({
          withEditButton <- as.data.frame(cbind(Edit = shinyInput(actionButton, nrow(CurrentResults), 'button_', label = "Edit", onclick = 'Shiny.onInputChange(\"EditButton\", this.id)'), CurrentResults))
        })
        
        QueryResultsXX <<- QueryResultsxx()
        
        SelectedRowEdit <<- eventReactive(input$EditButton, {
          as.numeric(strsplit(input$EditButton, "_")[[1]][2])
        })
        
        
        output$Level2Table <- DT::renderDataTable(
          datatable(QueryResultsXX[,-2], escape = FALSE, rownames = FALSE, selection = list(mode = "single", target = "row"), editable = TRUE, options = list(autowidth = TRUE,columnDefs = list(list(targets=c(0,1,6,8), width='50'), list(targets=c(2,3,4,5), width='100')))))
        to_index <<- QueryResultsXX[,-c(1)]
      }
      
      #After becomes the new before, reset those values
    })
    
    
    observeEvent(input$CancelEdit, {
      #wipe some values that would have been created prior to pressing cancel
      removeModal()
    })
    
    # 
    # 
    # editedValue = Level2_rvs$editedInfo
    # editedValue <- editedValue %>%
    #   group_by(row, col) %>%
    #   filter(value == dplyr::last(value) | is.na(value)) %>%
    #   ungroup()
    # 
    # editedValueForSelectedRow <<- subset(editedValue, editedValue$row %in% SelectedRowEdit())
    # Level2RowBeforeChanged <<- subset(Level2_rvs$dbdata, id %in% editedValueForSelectedRow$id)
    # Level2RowAfterChanged <<- subset(Level2_rvs$data, id %in% editedValueForSelectedRow$id)
    # Level3 <- dbReadTable(pool, 'level3')
    # Level3Filtered <<- Level3 %>%
    #   filter(Locus == as.character(Level2RowBeforeChanged[,2])) %>%
    #   filter(LocusType == as.character(Level2RowBeforeChanged[,3])) %>%
    #   filter(Period == as.character(Level2RowBeforeChanged[,4])) %>%
    #   filter(Blank == as.character(Level2RowBeforeChanged[,5])) %>%
    #   filter(Modification == as.character(Level2RowBeforeChanged[,6]))
    # ExistingLevel3_Locus <- as.character(Level2RowBeforeChanged[,2])
    # ExistingLevel3_LocusType <- as.character(Level2RowBeforeChanged[,3])
    # ExistingLevel3_Period <- as.character(Level2RowBeforeChanged[,4])
    # ExistingLevel3_Blank <- as.character(Level2RowBeforeChanged[,5])
    # ExistingLevel3_Modification <- as.character(Level2RowBeforeChanged[,6])
    # 
    # QuantityWoAR <- as.numeric(nrow(filter(Level3Filtered, WrittenOnArtefact == "Yes")))
    # if (length(editedValueForSelectedRow) & QuantityWoAR == 0) {
    #   lapply(seq_len(nrow(editedValueForSelectedRow)), function(i){
    #     id = editedValueForSelectedRow$id[i]
    #     col = dbListFields(pool, 'level2')[editedValueForSelectedRow$col[i]]
    #     value = editedValueForSelectedRow$value[i]
    #     
    #     UpdateLevel2Records <<- glue::glue_sql("UPDATE `level2` SET {`col`} = {value} WHERE id = {id}", .con = pool)
    #     UpdateCorrespondingLevel3Records <<- glue::glue_sql("UPDATE `level3` SET {`col`} = {value} WHERE Locus = {ExistingLevel3_Locus} AND LocusType = {ExistingLevel3_LocusType} AND Period = {ExistingLevel3_Period} AND Blank = {ExistingLevel3_Blank} AND Modification = {ExistingLevel3_Modification}", .con = pool)
    #     dbExecute(pool, sqlInterpolate(ANSI(), UpdateLevel2Records))
    #     dbExecute(pool, sqlInterpolate(ANSI(), UpdateCorrespondingLevel3Records))
    #   })
    # }
    # if (!length(editedValueForSelectedRow)) {
    #   output$x2 <- renderPrint("This record has not been changed. There is nothing to update.")
    # }
    # if (length(editedValueForSelectedRow) & QuantityWoAR > 0) {
    #   OverwritenWoARModalDT <- filter(Level3Filtered, WrittenOnArtefact == "Yes")
    #   OverwritenWoARModalDT <- OverwritenWoARModalDT[,-1]
    #   OverwritenWoARModalDT <- OverwritenWoARModalDT[,-7]
    #   
    #   showModal(modalDialog(title = "Warning!",
    #                         paste0("These changes will also modify data pertaining to ",QuantityWoAR," artefacts whose ArtefactIDs have already been written on the phyical artefacts. Please ensure that these changes reflect the actual characteristics of these items, and move them to their new bag [",Level2RowAfterChanged$Locus,"/",Level2RowAfterChanged$Period,"/",Level2RowAfterChanged$Blank,"/",Level2RowAfterChanged$Modification,"] if you decide to proceed. To confirm these changes, press Confirm. To abort, press Cancel"),
    #                         DT::renderDataTable(datatable(OverwritenWoARModalDT, rownames = FALSE, editable = FALSE, options=list(scrollX=TRUE))),
    #                         footer = tagList(
    #                           actionButton('WoAR_OverWrite_Cancel',"Cancel"),
    #                           actionButton('WoAR_OverWrite_Confirm', "Confirm")),
    #                         easyClose = FALSE,
    #                         size = "l",
    #                         fade = FALSE
    #   ))
    #   
    #   observeEvent(input$WoAR_OverWrite_Cancel, {
    #     removeModal()
    #   })
    #   
    #   observeEvent(input$WoAR_OverWrite_Confirm, {
    #     lapply(seq_len(nrow(editedValueForSelectedRow)), function(i){
    #       id = editedValueForSelectedRow$id[i]
    #       col = dbListFields(pool, 'level2')[editedValueForSelectedRow$col[i]]
    #       value = editedValueForSelectedRow$value[i]
    #       UpdateLevel2Records <<- glue::glue_sql("UPDATE `level2` SET {`col`} = {value} WHERE id = {id}", .con = pool)
    #       UpdateCorrespondingLevel3Records <<- glue::glue_sql("UPDATE `level3` SET {`col`} = {value} WHERE Locus = {ExistingLevel3_Locus} AND LocusType = {ExistingLevel3_LocusType} AND Period = {ExistingLevel3_Period} AND Blank = {ExistingLevel3_Blank} AND Modification = {ExistingLevel3_Modification}", .con = pool)
    #       dbExecute(pool, sqlInterpolate(ANSI(), UpdateLevel2Records))
    #       dbExecute(pool, sqlInterpolate(ANSI(), UpdateCorrespondingLevel3Records))
    #     })
    #     removeModal()
    #   })
    # }
    # 
    #   Level2 <- dbReadTable(pool, 'level2')
    #   Level2RowAfterChanged <<- subset(Level2, id %in% editedValueForSelectedRow$id)
    #   
    #   NewLevel3_Locus <- as.character(Level2RowAfterChanged[,2])
    #   NewLevel3_LocusType <- as.character(Level2RowAfterChanged[,3])
    #   NewLevel3_Period <- as.character(Level2RowAfterChanged[,4])
    #   NewLevel3_Blank <- as.character(Level2RowAfterChanged[,5])
    #   NewLevel3_Modification <- as.character(Level2RowAfterChanged[,6])
    #   
    #   updateSelectizeInput(session, "Locus", selected = NewLevel3_Locus)
    #   updateSelectizeInput(session, "LocusType", selected = NewLevel3_LocusType)
    #   updateSelectizeInput(session, "Period", selected = NewLevel3_Period)
    #   updateSelectizeInput(session, "Blank", selected = NewLevel3_Blank)
    #   updateSelectizeInput(session, "Modification", selected = NewLevel3_Modification)
    #   
    #  # QueryInputs  <- data.frame(Locus = NULL, Blank = NULL, Modification = NULL, Period = NULL)   
    # #  QueryInputs  <<- data.frame(Locus = ifelse(!is.null(input$Locus), input$Locus, NULL), Blank = ifelse(!is.null(input$Blank), input$Blank, NULL), Modification = ifelse(!is.null(input$Modification), input$Modification, NULL), Period = ifelse(!is.null(input$Period), input$Period, NULL))
    #   QueryTermLocus <- ifelse(!is.null(input$Locus), input$Locus, "")
    #   QueryTermPeriod <- ifelse(!is.null(input$Period), input$Period, "")
    #   QueryTermBlank <- ifelse(!is.null(input$Blank), input$Blank, "")
    #   QueryTermModification <- ifelse(!is.null(input$Modification), input$Modification, "")
    #   QueryInputs <<- data.frame(Locus = QueryTermLocus, Blank = QueryTermBlank, Modification = QueryTermModification, Period = QueryTermPeriod)
    #   CurrentResults <- QueryResults(QueryInputs)
    #   
    #   Level2 <- dbReadTable(pool, 'level2')
    #   EmptyDT <- filter(CurrentResults, LocusType=="blah")
    #   
    #   if (nrow(CurrentResults) == nrow(Level2)) {
    #     output$Level2Table <- renderDataTable(datatable(EmptyDT[,-1]))
    #     output$x2 <- renderPrint("Identical to the complete set of Level2 records")
    #   }
    #   
    #   if (nrow(CurrentResults) == 0) {
    #     output$x2 <- renderPrint("Here I am, brain the size of a planet, and you ask me to count lithics.  Well, I can't find any that match your criteria.")
    #     output$Level2Table <- renderDataTable(datatable(CurrentResults[,-1]))
    #   }
    #   
    #   if (nrow(CurrentResults) > 0 & nrow(CurrentResults) != nrow(Level2)) {
    #     QueryResultsxx <- reactive({
    #       withEditButton <- as.data.frame(cbind(Edit = shinyInput(actionButton, nrow(CurrentResults), 'button_', label = "Edit", onclick = 'Shiny.onInputChange(\"EditButton\", this.id)'), CurrentResults))
    #     })
    #     
    #     QueryResultsXX <<- QueryResultsxx()
    #     
    #     SelectedRowEdit <<- eventReactive(input$EditButton, {
    #       as.numeric(strsplit(input$EditButton, "_")[[1]][2])
    #     })
    #     
    #     
    #     output$Level2Table <- DT::renderDataTable(
    #       datatable(QueryResultsXX[,-2], escape = FALSE, rownames = FALSE, selection = list(mode = "single", target = "row"), editable = TRUE, options = list(autowidth = TRUE,columnDefs = list(list(targets=c(0,1,6,8), width='50'), list(targets=c(2,3,4,5), width='100')))))
    #     to_index <<- QueryResultsXX[,-c(1)]
    #   }
    #   
    #   Level2_rvs$dbdata <- Level2_rvs$data
    #   Level2_rvs$dataSame <- TRUE
    #   
    #   
    
    
    
    
    
    #-----/EditRecords-----#
    
    #-----ActivityLog-----#
    activitylog <- dbReadTable(pool, 'activitylog')
    #activitylog
    output$ActivityLogDisplay <- renderDataTable(datatable(activitylog[,-1], rownames = FALSE, selection=list(mode="single", target="row"), options=list(order=list(list(1, 'desc')), pageLength=20)))
    
    #-----/ActivityLog-----#
    
  }
)
shinyApp(ui, server)
