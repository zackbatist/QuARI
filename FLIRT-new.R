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
    titlePanel("SNAP Lithics Processing"),
    fluidRow(
      column(width = 2,
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
             numericInput("Quantity", "Quantity", "1")),
      column(3, verbatimTextOutput("x1")),
      column(3, verbatimTextOutput("x2"))
    ),
    actionButton("query", "Query"),
    actionButton("toggleNewBlankMod", "New Blank or Modification"),
    actionButton("newTab", "New Tab"),
    actionButton("removeTab", "Remove Tab"),
    uiOutput("newBlankMod"),
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
    ),
    actionButton("level2_save", "Save Level 2 Changes"),
    actionButton("level3_save", "Save Level 3 Changes")
    
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
    
    
    #this operates without needing to press the query a second time, which I find troubling for some reason
    #also it should wipe the existing table that's displayed when a new query is submitted
    observeEvent(input$query, {
      Level2 <- dbReadTable(pool, 'level2')
      QueryResultsRV <- reactive({
        filtered <- Level2
        if (!is.null(input$Blank)) {
          filtered <- filtered %>% filter(Blank == input$Blank)
        }
        if (!is.null(input$Modification)) {
          filtered <- filtered %>% filter(Modification == input$Modification)
        }
        if (!is.null(input$Period)) {
          filtered <- filtered %>% filter(Period == input$Period)
        }
        filtered
      })
      #this filters the results if a matching value exists, but the results remain the same when no matching values are found in the database
      #we need to create an error message using renderPrint that notifies the user that no matching values were found
      
      observe({
        QueryResults <<- QueryResultsRV()
        
        #if there are no results, create the option to create a record with the values in the input fields
        #if (nrow(QueryResults) == 0) {
        #}
        
        if (nrow(QueryResults) > 0) {
          
          #-------
          QueryResultsxx <- reactive({
            withUpdateButton <- as.data.frame(cbind(Update = shinyInput(actionButton, nrow(QueryResults),'button_', label = "Update", onclick = 'Shiny.onInputChange(\"UpdateButton\",  this.id)' ), QueryResults))
            withDeleteButton <- as.data.frame(cbind(Delete = shinyInput(actionButton, nrow(QueryResults),'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"DeleteButton\",  this.id)' ), withUpdateButton))
          })
          QueryResults <- QueryResultsxx()
          
          # Here I created a reactive to save which row was clicked which can be stored for further analysis
          SelectedRow <- eventReactive(input$UpdateButton, {
            as.numeric(strsplit(input$UpdateButton, "_")[[1]][2])
          })
          SelectedRow <- eventReactive(input$DeleteButton, {
            as.numeric(strsplit(input$DeleteButton, "_")[[1]][2])
          })
          
          # This is needed so that the button is clicked once for modal to show, a bug reported here
          # https://github.com/ebailey78/shinyBS/issues/57
          observeEvent(input$UpdateButton, {
            toggleModal(session, "modalExample", "open")
          })
          observeEvent(input$DeleteButton, {
            toggleModal(session, "modalExample", "open")
          })
          
          DataRow <- eventReactive(input$UpdateButton, {
            QueryResults()[SelectedRow(),2:ncol(QueryResults())]
          })
          DataRow <- eventReactive(input$DeleteButton, {
            QueryResults()[SelectedRow(),2:ncol(QueryResults())]
          })
          
          output$popup <- renderUI( {
            bsModal("modalExample", paste0("Data for Row Number: ", SelectedRow()), "", size = "large",
                    column(12,                   
                           DT::renderDataTable(DataRow())
                    )
            )
          })
          #----
          
          output$Level2Table <- DT::renderDataTable(
            datatable(QueryResults[,-3], extensions = 'Buttons', filter="top", escape = FALSE,rownames= FALSE, selection=list(mode="single", target="row")), options=list(columns.width=c("30%","30%",NULL, NULL,NULL,NULL,NULL,"50%",NULL,"50%")))  #I'd like to control column width here (and elsewhere) - we're wasting lots of space on columns that don't need it.  Setting options=list(columns.width) is an attempt to do that, but I'm not convinced it's doing anything at all
          #added [,-1] to QueryResults to exclude 'id' column, which I think can only confuse things
          
          to_index <<- QueryResults
        }
        
        if (nrow(QueryResults) == 0) {
          output$x2 <- renderPrint("Here I am, brain the size of a planet, and you ask me to count lithics.  Well, I can't find any that match your criteria.")
        }
        
        
      })
    })
    
    observe({
      sel <- input$Level2Table_rows_selected
      if (length(sel)) {
        Level2IndexValues <- sel
        Level3Selection <- unlist(to_index[Level2IndexValues, c(2,4,5,6)])
        Level3 <- dbReadTable(pool, 'level3')
        Level3FilterResults <- filter(Level3, Locus==Level3Selection[1] & Period==Level3Selection[2] & Blank==Level3Selection[3] & Modification==Level3Selection[4])
        output$Level3Table <- DT::renderDataTable(
          datatable(Level3FilterResults, selection=list(mode="single", target="cell"), editable = TRUE))
        
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
    

    
    
    #-----
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



    
    
    
    
  }
)
shinyApp(ui, server)
