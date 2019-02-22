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


#read unmodified versions of allloci, blanks and modifications tables specifically for the input selection
allloci <- dbReadTable(pool, 'allloci')
allloci
blanks <- dbReadTable(pool, 'blanks_excavation')
blanks
modifications <- dbReadTable(pool, 'modifications_excavation')
modifications

shinyApp(
  ui <- fluidPage(
    tags$head(tags$style(
      HTML("input[type='search']:disabled {visibility:hidden}")
    )),
    titlePanel("SNAP Lithics Processing"),
    fluidRow(
      column(width = 2,
             selectInput("Locus", "Locus", choices = allloci$Locus, multiple = FALSE)),
      column(width = 2,
             selectInput("LocusType", "Locus Type", choices = c("Context","Transect","Grid","Grab", "XFind"), multiple = FALSE)),
      column(width = 2,
             selectInput("Period", "Period", choices = c("Lower Palaeolithic", "Lower / Middle Palaeolithic", "Middle Palaeolithic", "Middle / Upper Palaeolithic", "Upper Palaeolithic", "Upper Palaeolithic / Mesolithic", "Mesolithic", "Late Neolithic / Final Neolithic", "Final Neolithic / Early Bronze Age", "Non-Diagnostic"), multiple = FALSE)),
      column(width = 2,
             selectInput("Blank", "Blank", choices = blanks$Blank, multiple = FALSE)),
      column(width = 2,
             selectInput("Modification", "Modification", choices = modifications$Modification, multiple = FALSE)),
      column(width = 1,
             numericInput("Quantity", "Quantity", "")),
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
                         DT::dataTableOutput("Level2Table")
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
    
    
    #filter the list of loci for the Locus field
    allloci <- dbReadTable(pool, 'allloci')
    locusChoices <- reactive({
      filter(allloci, LocusType==input$LocusType) %>%
        select(Locus) %>%
        arrange(Locus)
    })
    
    observe({
      selectedpoint <- locusChoices()
      updateSelectInput(session, inputId = "Locus", choices=c(selectedpoint[[1]]))
    })
    
    #filter the list of blanks for the Blank field
    blanks <- dbReadTable(pool, 'blanks_excavation')
    blankChoices <- reactive({
      select(filter(blanks, Period==input$Period), Blank)
    })
    
    observe({
      selectedBlank <- blankChoices()
      updateSelectInput(session, inputId = "Blank", choices=c(selectedBlank[[1]]))
    })
    
    #filter the list of modifications for the Modification field
    modifications <- dbReadTable(pool, 'modifications_excavation')
    modificationChoices <- reactive({
      select(filter(modifications, Period==input$Period), Modification)
    })
    
    observe({
      selectedModification <- modificationChoices()
      updateSelectInput(session, inputId = "Modification", choices=c(selectedModification[[1]]))
    })
    
    #store content of the input fields
    responses <- data.frame()
    singleResponse <- reactive(
      data.frame(
        input$Locus, input$LocusType, input$Period, input$Blank, input$Modification, input$Quantity
      )
    )
    
    
    observeEvent(input$query, {
      Level2 <- dbReadTable(pool, 'level2')
      QueryResultsRV <- reactive({
        select(filter(Level2, Period==input$Period & Blank==input$Blank & Modification==input$Modification & LocusType==input$LocusType & Locus==input$Locus), Locus, LocusType, Period, Blank, Modification, Quantity)
      })
      
      #if input fields are blank, treat them as wild cards
      observe({
        QueryResults <<- QueryResultsRV()
        
        #if there are no results, create the option to create a record with the values in the input fields
        #if (nrow(QueryResults) == 0) {
        #}
        
        if (nrow(QueryResults) > 0) {
          output$Level2Table <- DT::renderDataTable(
            datatable(QueryResults, extensions = 'Buttons', filter="top", selection=list(mode="single", target="row")
            )
          )
          
          
          to_index <<- QueryResults
          
          
        }
      })
    })
    
    observe({
      sel <- input$Level2Table_rows_selected
      if (length(sel)) {
        Level2IndexValues <- sel
        Level3Selection <- unlist(to_index[Level2IndexValues, c(1,4,5)])
        Level3 <- dbReadTable(pool, 'level3')
        Level3FilterResults <- filter(Level3, Blank==Level3Selection[2] & Modification==Level3Selection[3] & Locus==Level3Selection[1])
        output$Level3Table <- DT::renderDataTable(
          datatable(Level3FilterResults, selection=list(mode="single", target="cell"), editable = TRUE
          )
        )
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
    
    
    
    
    
    
    
    
    
    
    
  }
)
shinyApp(ui, server)
