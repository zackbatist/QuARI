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
library(tools)

source("keys.R")

pool <- pool::dbPool(drv = RMariaDB::MariaDB(),
                     dbname = dbnamex,
                     host = hostx,
                     port = portx,
                     user = userx,
                     password = passwordx)

onStop(function() {
  poolClose(pool)
}) # important!

staff <- dbReadTable(pool, 'staff')
staff
cameras <- dbReadTable(pool, 'cameras')
cameras
contexts <- dbReadTable(pool, 'contexts')
contexts
trenches <- dbReadTable(pool, 'trenches')
trenches

shinyApp(
  ui <- fluidPage(
    tags$head(tags$style(
      HTML("input[type='search']:disabled {visibility:hidden}")
    )),
    titlePanel("SNAP Photos"),
    fluidRow(
      column(width = 2,
             selectInput("PhotoOf", "What is this photo of?", choices = c("Context", "Trench", "Sample", "XFind", "Artefact", "Work"), multiple = FALSE)),
      column(width = 2,
             selectInput("PhotoType", "Photo Type", choices = c("Lower Palaeolithic", "Lower / Middle Palaeolithic", "Middle Palaeolithic", "Middle / Upper Palaeolithic", "Upper Palaeolithic", "Upper Palaeolithic / Mesolithic", "Mesolithic", "Late Neolithic / Final Neolithic", "Final Neolithic / Early Bronze Age", "NonDiagnostic"), multiple = FALSE)),
      column(width = 1,
             selectInput("Photographer", "Photographer", choices = staff$Code, multiple = FALSE)),
      column(width = 2,
             selectInput("Camera", "Camera", choices = cameras$CameraCode, multiple = FALSE)),
      column(width = 2,
             dateInput("Date", "Date")),
      column(width = 2,
              selectInput("Direction", "Direction", choices = c("North", "South", "East", "West"), multiple = FALSE))
      ),
    fluidRow(
    column(width = 3,
           textAreaInput("Description", "Description")),
    column(width = 3,
           textAreaInput("Notes", "Notes")),
    column(width = 4,
           fileInput("myFile", "Choose a file", accept = c('image/png', 'image/jpeg')))
    ),
    uiOutput("PhotoOfVariants"), #toggles different submit buttons depending on what LocusType is selected, which write to different tables accordingly
    hr(),
    DT::dataTableOutput("photos"),
    actionButton("saveChanges", "Save Changes")
  ),
  
  server <- function(input, output, session){
    #render ui for adding new blanks and modifications
    output$PhotoOfVariants <- renderUI({
      div(
        if (input$PhotoOf == "Context") {
          span(
            fluidRow(
              column(width = 2,
                     textInput(inputId = "Context", label = "Context")),
              column(width = 2,
                     actionButton(inputId = "saveContext", label = "Save Photo of Context")),
              column(width = 6,
                     textOutput("message"))
            ),
            tags$style(type='text/css', "#saveContext {margin-top: 25px;}"),
            tags$style(type='text/css', "#message {margin-top: 25px;}")
          )
        },
        
        if (input$PhotoOf == "Trench") {
          span(
            fluidRow(
              column(width = 2,
                     textInput(inputId = "Trench", label = "Trench")),
              column(width = 2,
                     actionButton(inputId = "saveTrench", label = "Save Trench Photo")),
              column(width = 6,
                     textOutput("message"))
            ),
            tags$style(type='text/css', "#saveTrench {margin-top: 25px;}")
          )
        },
        
        if (input$PhotoOf == "Sample") {
          span(
            fluidRow(
              column(width = 2,
                     textInput(inputId = "Sample", label = "Sample")),
              column(width = 2,
                     actionButton(inputId = "saveSample", label = "Save Sample Photo")),
              column(width = 6,
                     textOutput("message"))
            ),
            tags$style(type='text/css', "#saveSample {margin-top: 25px;}")
          )
        },
        
        if (input$PhotoOf == "XFind") {
          span(
            fluidRow(
              column(width = 2,
                     textInput(inputId = "XFind", label = "XFind")),
              column(width = 2,
                     actionButton(inputId = "saveXFind", label = "Save XFind Photo")),
              column(width = 6,
                     textOutput("message"))
            ),
            tags$style(type='text/css', "#saveXFind {margin-top: 25px;}")
          )
        },
        
        if (input$PhotoOf == "Artefact") {
          span(
            fluidRow(
              column(width = 2,
                     textInput(inputId = "Artefact", label = "Artefact")),
              column(width = 2,
                     actionButton(inputId = "saveArtefact", label = "Save Artefact Photo")),
              column(width = 6,
                     textOutput("message"))
            ),
            tags$style(type='text/css', "#saveArtefact {margin-top: 25px;}")
          )
        },
        
        if (input$PhotoOf == "Work") {
          span(
            fluidRow(
              column(width = 2,
                     textInput(inputId = "Work", label = "Work")),
              column(width = 2,
                     actionButton(inputId = "saveWork", label = "Save Work Photo")),
              column(width = 6,
                     textOutput("message"))
            ),
            tags$style(type='text/css', "#saveWork {margin-top: 25px;}")
          )
        }
        
      )
    })
    
    #display existing data in data tables
    output$photos <- renderDataTable({
      datatable(pool %>% tbl("photos") %>% collect(),
                extensions = 'Buttons', filter="top", rownames = FALSE, selection = list(mode = "multiple", target = "row"), editable = TRUE)
    })
    
    observeEvent(input$saveContext, {
        #uploads file to specified path
        inFile <- input$myFile
        if (is.null(inFile))
          return()
        file.copy(inFile$datapath, file.path("~/contexts/", inFile$name))
        
        #collect reactive values pertaining to the entered values and coerce the data types
        newContext <- reactiveValues(Context = input$Context)
        newContextPhotoOf <- reactiveValues(newPhotoOf = input$PhotoOf)
        newContextPhotographer <- reactiveValues(newPhotographer = input$Photographer)
        newContextCamera <- reactiveValues(newCamera = input$Camera)
        newContextDate <- reactiveValues(newDate = input$Date)
        newContextDirection <- reactiveValues(newDirection = input$Direction)
        newContextDescription <- reactiveValues(newDescription = input$Description)
        newContextNotes <- reactiveValues(newNotes = input$Notes)
        newContextPhotoType <- reactiveValues(newPhotoType = input$PhotoType)
        newContext <- as.character(newContext$Context)
        newContextPhotoOf <- as.character(newContextPhotoOf$newPhotoOf)
        newContextPhotographer <- as.character(newContextPhotographer$newPhotographer)
        newContextCamera <- as.character(newContextCamera$newCamera)
        newContextDate <- as.character(newContextDate$newDate)
        newContextDirection <- as.character(newContextDirection$newDirection)
        newContextDescription <- as.character(newContextDescription$newDescription)
        newContextNotes <- as.character(newContextNotes$newNotes)
        newContextPhotoType <- as.character(newContextPhotoType$newPhotoType)
        newContextFilename <- reactiveValues(newFilename = inFile$name)
        newContextFilename <- as.character(newContextFilename$newFilename)
        newContextFileExtension <- file_ext(newContextFilename)
        newContextFileExtension <- as.character(newContextFileExtension)
        newContextFileSansPath <- file_path_sans_ext(newContextFilename)
        newContextFileSansPath <- as.character(newContextFileSansPath)
        
        if (any(contexts$Context == newContext) == TRUE) {
          #generate and execute SQL INSERT statement
          newContextInsert <- glue::glue_sql("INSERT INTO `photos` (`ReferenceType`, `Entity`, `Photographer`, `Camera`, `Date`, `Direction`, `Description`, `Notes`, `Class`, `Filename`, `Extension`) VALUES ({newContextPhotoOf}, {newContext}, {newContextPhotographer}, {newContextCamera}, {newContextDate}, {newContextDirection}, {newContextDescription}, {newContextNotes}, {newContextPhotoType}, {newContextFileSansPath}, {newContextFileExtension})"
                                             , .con = pool)
          dbExecute(pool, sqlInterpolate(ANSI(), newContextInsert))
          
          #generate confirmation message
          message1 <- paste0("Photo of Context ", newContext, " successfully added to the database!")
          output$message <- renderText({message1})
        
      } #else write an error and do nothing
      else { output$message <- renderText({print("That context does not exist!")})}
      })
    
    observeEvent(input$saveTrench, {
      #generate and execute SQL INSERT statement
      newTrenchInsert <- glue::glue_sql("INSERT INTO `photos` (`ReferenceType`, `Entity`, `Photographer`, `Camera`, `Date`, `Direction`, `Description`, `Notes`, `Class`) VALUES ({PhotoOf}, {Trench}, {Photographer}, {Camera}, {Date}, {Direction}, {Description}, {Notes}, {PhotoType})"
                                         , .con = pool)
      dbExecute(pool, sqlInterpolate(ANSI(), newTrenchInsert))
      message1 <- paste0("Photo successfully added to the database.")
      output$message <- renderText({message1})
    })
    
    observeEvent(input$saveSample, {
      #generate and execute SQL INSERT statement
      newSampleInsert <- glue::glue_sql("INSERT INTO `photos` (`ReferenceType`, `Entity`, `Photographer`, `Camera`, `Date`, `Direction`, `Description`, `Notes`, `Class`) VALUES ({PhotoOf}, {Sample}, {Photographer}, {Camera}, {Date}, {Direction}, {Description}, {Notes}, {PhotoType})"
                                         , .con = pool)
      dbExecute(pool, sqlInterpolate(ANSI(), newSampleInsert))
      message1 <- paste0("Photo successfully added to the database.")
      output$message <- renderText({message1})
    })
    
    observeEvent(input$saveXFind, {
      #generate and execute SQL INSERT statement
      newXFindInsert <- glue::glue_sql("INSERT INTO `photos` (`ReferenceType`, `Entity`, `Photographer`, `Camera`, `Date`, `Direction`, `Description`, `Notes`, `Class`) VALUES ({PhotoOf}, {XFind}, {Photographer}, {Camera}, {Date}, {Direction}, {Description}, {Notes}, {PhotoType})"
                                         , .con = pool)
      dbExecute(pool, sqlInterpolate(ANSI(), newXFindInsert))
      message1 <- paste0("Photo successfully added to the database.")
      output$message <- renderText({message1})
    })
    
    observeEvent(input$saveArtefact, {
      #generate and execute SQL INSERT statement
      newArtefactInsert <- glue::glue_sql("INSERT INTO `photos` (`ReferenceType`, `Entity`, `Photographer`, `Camera`, `Date`, `Direction`, `Description`, `Notes`, `Class`) VALUES ({PhotoOf}, {Artefact}, {Photographer}, {Camera}, {Date}, {Direction}, {Description}, {Notes}, {PhotoType})"
                                         , .con = pool)
      dbExecute(pool, sqlInterpolate(ANSI(), newArtefactInsert))
      message1 <- paste0("Photo successfully added to the database.")
      output$message <- renderText({message1})
    })
    })

shinyApp(ui, server)