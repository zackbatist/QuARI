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


#derived from https://github.com/MangoTheCat/dtdbshiny
updateDB <- function(editedValue, pool, tbl){
  # Keep only the last modification for a cell
  editedValue <- editedValue %>% 
    group_by(row, col) %>% 
    filter(value == dplyr::last(value)| is.na(value)) %>% 
    ungroup()
  
  conn <- poolCheckout(pool)
  
  lapply(seq_len(nrow(editedValue)), function(i){
    id = editedValue$row[i]
    col = dbListFields(pool, tbl)[editedValue$col[i]]
    value = editedValue$value[i]
    
    query <- glue::glue_sql("UPDATE {`tbl`} SET
                            {`col`} = {value}
                            WHERE id = {id}
                            ", .con = conn)
    
    dbExecute(conn, sqlInterpolate(ANSI(), query))
  })
  
  poolReturn(conn)
  print(editedValue)  
  return(invisible())
}

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
             selectInput("Period", "Period", choices = c("Lower Palaeolithic", "Lower / Middle Palaeolithic", "Middle Palaeolithic", "Middle / Upper Palaeolithic", "Upper Palaeolithic", "Upper Palaeolithic / Mesolithic", "Mesolithic", "Late Neolithic / Final Neolithic", "Final Neolithic / Early Bronze Age", "NonDiagnostic"), multiple = FALSE)),
      column(width = 2,
             selectInput("Blank", "Blank", choices = blanks$Blank, multiple = FALSE)),
      column(width = 2,
             selectInput("Modification", "Modification", choices = modifications$Modification, multiple = FALSE)),
      column(width = 1,
             numericInput("Quantity", "Quantity", ""))
    ),
    actionButton("submit", "Submit"),
    actionButton("browser", "browser"),
    actionButton("toggleNewBlankMod", "New Blank or Modification"),
    uiOutput("newBlankMod"),
    hr(),
    tabsetPanel(type = "tabs",
                tabPanel("All Level 2",
                         DT::dataTableOutput("level2_allDT")
                ),
                tabPanel("All Level 3",
                         DT::dataTableOutput("level3_allDT")
                )
    ),
    actionButton("level2_save", "Save Level 2 Changes"),
    actionButton("level3_save", "Save Level 3 Changes")
  ),
  
  
  server <- function(input, output, session){
    observeEvent(input$browser,{
      browser()
    })
    
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
    #we might want the list of loci/blanks/modifications to be editable (i.e. not a preset list of possible options)
    #it might therefore be better to have these fields as textInput rather than selectInput
    #or maybe it's possible to have editable selectInputs somehow by coercing the values as character type rather than as factors?
    
    
    #store content of the input fields
    responses <- data.frame()
    singleResponse <- reactive(
      data.frame(
        input$Locus, input$LocusType, input$Period, input$Blank, input$Modification, input$Quantity
      )
    )
    
    
    observeEvent(input$submit, {
      if (input$LocusType == "XFind") {
        # parse info pertaining to the xfind's provenance based on its XFindID
        LocusValue <<- reactive({
          z <- toString(input$Locus)
          return(z)
        })
        XFindContext <- substr(LocusValue(), 1, 4)
        XFindNumber <- substr(LocusValue(), 5, 8)
        
        #filter for contexts from which the xfind derives
        XFindLevel2 <- dbReadTable(pool, 'level2')
        XFindFilter <- reactive({
          select(filter(XFindLevel2, Period==input$Period & Blank==input$Blank & Modification==input$Modification & LocusType=="Context" & Locus==XFindContext), Locus, LocusType, Period, Blank, Modification, Quantity)
        })
        observe({
          XFindSubset <<- XFindFilter()
          if (nrow(XFindSubset) == 0) {
            #if no equivalent record exists:
            #write the context to the level2 table
            writeXFind <- dbWriteTable(pool, "level2", XFindSubset, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
            writeXFind
            
            #re-read the updated table from the database and render it in the data table
            output$level2_allDT <- DT::renderDataTable({
              datatable(pool %>% tbl("level2") %>% collect(),
                        extensions = 'Buttons', filter="top", rownames = FALSE, selection = "none", editable = TRUE)
            })
            
            #update the level2_mysource
            level2_mysource <<- reactive({
              pool %>% tbl("level2") %>% collect()
            })
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
            
            
            #re-read the updated table from the database and render it in the data table
            output$level2_allDT <- DT::renderDataTable({
              datatable(pool %>% tbl("level2") %>% collect(),
                        extensions = 'Buttons', filter="top", rownames = FALSE, selection = "none", editable = TRUE)
            })
            
            #update the level2_mysource
            level2_mysource <<- reactive({
              pool %>% tbl("level2") %>% collect()
            })
          }
        })
      }
      else {
        #filter for equivalent lociXblankXmodXperiod combinations
        EquivRecord <- dbReadTable(pool, 'level2')
        EquivRecordFilter <- reactive({
          select(filter(EquivRecord, Period==input$Period & Blank==input$Blank & Modification==input$Modification & LocusType==input$LocusType & Locus==input$Locus), Locus, LocusType, Period, Blank, Modification, Quantity)
        })
        observe({
          EquivRecordSubset <<- EquivRecordFilter()
          if (nrow(EquivRecordSubset) == 0) {
            #store field values to a responses table after the submit button is clicked
            values <- reactiveValues(singleResponse_df = data.frame(input$Locus, input$LocusType, input$Period, input$Blank, input$Modification, input$Quantity))
            responses <<- rbind(responses, values$singleResponse_df)
            #rename columns in singleResponse_df
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.Locus'] <- 'Locus'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.LocusType'] <- 'LocusType'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.Period'] <- 'Period'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.Blank'] <- 'Blank'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.Modification'] <- 'Modification'
            colnames(values$singleResponse_df)[colnames(values$singleResponse_df) == 'input.Quantity'] <- 'Quantity'
            
            #write the field values to the database as a new record in the level2 table after the submit button is clicked
            #this step, as well as the following dbReadTable command, are very important since they allow the database to allocate an id to the row, which is crucial for updating modified cells
            write_level2 <- dbWriteTable(pool, "level2", values$singleResponse_df, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
            write_level2
            
            #####!!!! It gets stuck in a loop here, fails to recognize that now there are equivalent records and that this branch of the if/else function should come to an end
            
            #re-read the updated table from the database and render it in the data table
            output$level2_allDT <- DT::renderDataTable({
              datatable(pool %>% tbl("level2") %>% collect(),
                        extensions = 'Buttons', filter="top", rownames = FALSE, selection = "none", editable = TRUE)
            })
            
            #update level2_mysource in the global environment
            level2_mysource <<- reactive({
              pool %>% tbl("level2") %>% collect()
            })
          }
          else {
            #if the equivalent record already exists, update the quantity field to reflect this new addition to the existing record
            #store field values to a responses table after the submit button is clicked
            valuesx <- reactiveValues(singleResponse_dfx = data.frame(input$Locus, input$LocusType, input$Period, input$Blank, input$Modification, input$Quantity))
            #rename columns in singleResponse_df
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.Locus'] <- 'Locus'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.LocusType'] <- 'LocusType'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.Period'] <- 'Period'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.Blank'] <- 'Blank'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.Modification'] <- 'Modification'
            colnames(valuesx$singleResponse_dfx)[colnames(valuesx$singleResponse_dfx) == 'input.Quantity'] <- 'Quantity'
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
            
            
            #re-read the updated table from the database and render it in the data table
            output$level2_allDT <- DT::renderDataTable({
              datatable(pool %>% tbl("level2") %>% collect(),
                        extensions = 'Buttons', filter="top", rownames = FALSE, selection = "none", editable = TRUE)
            })
            
            #update the level2_mysource
            level2_mysource <<- reactive({
              pool %>% tbl("level2") %>% collect()
            })
          }
        })
      }
      
      #upon submitting field values, expand the response based on the value on the Quantity field
      #the expandRows command is part of the splitstackshape package
      ValuesToExpand <- reactiveValues(singleResponse_df = data.frame(input$Locus, input$LocusType, input$Period, input$Blank, input$Modification, input$Quantity))
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
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.Locus'] <- 'Locus'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.LocusType'] <- 'LocusType'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.Period'] <- 'Period'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.Blank'] <- 'Blank'
      colnames(singleRow_expanded)[colnames(singleRow_expanded) == 'input.Modification'] <- 'Modification'
      
      #write the new expanded rows to the level3 table in the database
      write_level3 <- dbWriteTable(pool, "level3", singleRow_expanded, row.names = FALSE, append = TRUE, overwrite = FALSE, temporary = FALSE)
      write_level3
      
      #read the updated level3 table from the database
      output$level3_allDT <- DT::renderDataTable({
        datatable(pool %>% tbl("level3") %>% collect(),
                  extensions = 'Buttons', filter="top", rownames = FALSE, selection = "none", editable = TRUE)
      })
      
      #update level3_mysource in the global environment
      level3_mysource <<- reactive({
        pool %>% tbl("level3") %>% collect()
      })
    })
    
    
    #borrowed all of below from https://github.com/MangoTheCat/dtdbshiny
    #generate reactive values
    level2_rvs <- reactiveValues(
      data = NA,
      dbdata = NA,
      dataSame = TRUE,
      editedInfo = NA
    )
    
    #generate source via reactive expression
    level2_mysource <- reactive({
      pool %>% tbl("level2") %>% collect()
    })
    
    # Observe the source, update reactive values accordingly
    observeEvent(level2_mysource(), {
      # Lightly format data by arranging id
      # Not sure why disordered after sending UPDATE query in db    
      level2_data <- level2_mysource() %>% arrange(id)
      level2_rvs$data <- level2_data
      level2_rvs$dbdata <- level2_data
    })
    
    # Render DT table and edit cell
    # 
    # no curly bracket inside renderDataTable
    # selection better be none
    # editable must be TRUE
    output$level2_allDT <- DT::renderDataTable(
      level2_rvs$data, extensions = 'Buttons', filter="top", rownames = FALSE, selection = "none", editable = TRUE)
    
    proxy2 = dataTableProxy('level2_allDT')
    
    observeEvent(input$level2_allDT_cell_edit, {
      
      info = input$level2_allDT_cell_edit
      
      i = info$row
      j = info$col = info$col + 1  # column index offset by 1
      v = info$value
      
      level2_rvs$data[i, j] <<- DT::coerceValue(v, dplyr::pull(level2_rvs$data[i, j]))
      replaceData(proxy2, level2_rvs$data, resetPaging = FALSE, rownames = FALSE)
      
      level2_rvs$dataSame <- identical(level2_rvs$data, level2_rvs$dbdata)
      
      if (all(is.na(level2_rvs$editedInfo))) {
        level2_rvs$editedInfo <- data.frame(info, stringsAsFactors = FALSE)
      } else {
        level2_rvs$editedInfo <- dplyr::bind_rows(level2_rvs$editedInfo, data.frame(info, stringsAsFactors = FALSE))
      }
      
    })
    
    #update edited values in db once save is clicked
    observeEvent(input$level2_save, {
      updateDB(editedValue = level2_rvs$editedInfo, pool = pool, tbl = "level2")
      level2_rvs$dbdata <- level2_rvs$data
      level2_rvs$dataSame <- TRUE
      #re-read the updated table from the database and render it in the data table
      output$level2_allDT <- DT::renderDataTable({
        datatable(pool %>% tbl("level2") %>% collect(),
                  extensions = 'Buttons', filter="top", rownames = FALSE, selection = "none", editable = TRUE)
      })
    })
    
    #now for level 3
    #generate reactive values
    level3_rvs <- reactiveValues(
      data = NA,
      dbdata = NA,
      dataSame = TRUE,
      editedInfo = NA
    )
    
    #generate source via reactive expression
    level3_mysource <- reactive({
      pool %>% tbl("level3") %>% collect()
    })
    
    # Observe the source, update reactive values accordingly
    observeEvent(level3_mysource(), {
      
      # Lightly format data by arranging id
      # Not sure why disordered after sending UPDATE query in db    
      level3_data <- level3_mysource() %>% arrange(id)
      
      level3_rvs$data <- level3_data
      level3_rvs$dbdata <- level3_data
      
    })
    
    # Render DT table and edit cell
    # 
    # no curly bracket inside renderDataTable
    # selection better be none
    # editable must be TRUE
    output$level3_allDT <- DT::renderDataTable(
      level3_rvs$data, extensions = 'Buttons', filter="top", rownames = FALSE, selection = "none", editable = TRUE)
    
    proxy3 = dataTableProxy('level3_allDT')
    
    observeEvent(input$level3_allDT_cell_edit, {
      
      info = input$level3_allDT_cell_edit
      
      i = info$row
      j = info$col = info$col + 1  # column index offset by 1
      v = info$value
      
      level3_rvs$data[i, j] <<- DT::coerceValue(v, dplyr::pull(level3_rvs$data[i, j]))
      replaceData(proxy3, level3_rvs$data, resetPaging = FALSE, rownames = FALSE)
      
      level3_rvs$dataSame <- identical(level3_rvs$data, level3_rvs$dbdata)
      
      if (all(is.na(level3_rvs$editedInfo))) {
        level3_rvs$editedInfo <- data.frame(info, stringsAsFactors = FALSE)
      } else {
        level3_rvs$editedInfo <- dplyr::bind_rows(level3_rvs$editedInfo, data.frame(info, stringsAsFactors = FALSE))
      }
      
    })
    
    #update edited values in db once save is clicked
    observeEvent(input$level3_save, {
      updateDB(editedValue = level3_rvs$editedInfo, pool = pool, tbl = "level3")
      level3_rvs$dbdata <- level3_rvs$data
      level3_rvs$dataSame <- TRUE
      #re-read the updated table from the database and render it in the data table
      output$level3_allDT <- DT::renderDataTable({
        datatable(pool %>% tbl("level3") %>% collect(),
                  extensions = 'Buttons', filter="top", rownames = FALSE, selection = "none", editable = TRUE)
      })
    })
    
    
    #render ui for adding new blanks and modifications
    output$newBlankMod <- renderUI({
      div(
        if (input$toggleNewBlankMod %% 2 == 1) {
          span(
            fluidRow(
              column(width = 2,
                     textInput(inputId = "newBlank", label = "New Blank")),
              column(width = 2,
                     textInput(inputId = "newModification", label = "New Modification")),
              column(width = 3,
                     selectInput("newBlankModPeriod", "Period", choices = c("Lower Palaeolithic", "Lower / Middle Palaeolithic", "Middle Palaeolithic", "Middle / Upper Palaeolithic", "Upper Palaeolithic", "Upper Palaeolithic / Mesolithic", "Mesolithic", "Late Neolithic / Final Neolithic", "Final Neolithic / Early Bronze Age", "NonDiagnostic"), multiple = FALSE))
            ), #had to limit the selection to 1 period per entry, enabling multiple posed problems with generating a table to write to the database that I could not yet figure out
            fluidRow(
              column(width = 2,
                     actionButton("submitBlank", "Submit Blank")),
              column(width = 2,
                     actionButton("submitModification", "Submit Modification")),
              column(width = 6,
                     textOutput("message"))
            )
          )
        } else {
          span()
        }
      )
    })
    
    
    observeEvent(input$submitBlank, {
      #collect reactive values pertaining to the entered values and coerce the data types
      newBlankResponses <- reactiveValues(
        Blank = input$newBlank, Period = input$newBlankModPeriod)
      newBlankValue <- as.character(newBlankResponses$Blank)
      newBlankPeriodValue <- as.character(newBlankResponses$Period) #would be courced as a list for new looping method described below
      
      #collect and subset the list of existing blanks and limit action based on whether the blank already exists
      filteredBlanks <- data.frame(
        select(filter(blanks, Period == input$newBlankModPeriod), Blank))
      if (any(filteredBlanks$Blank == newBlankValue) == FALSE) {
        #generate and execute SQL INSERT statement
        newBlankInsert <- glue::glue_sql("INSERT INTO `blanks_excavation` (`Blank`, `Period`) VALUES ({newBlankValue}, {newBlankPeriodValue})"
                                         , .con = pool)
        dbExecute(pool, sqlInterpolate(ANSI(), newBlankInsert))
        blanks <<- dbReadTable(pool, 'blanks_excavation')
        blankChoices <- reactive({
          select(filter(blanks, Period==input$Period), Blank)
        })
        observe({
          selectedBlank <- blankChoices()
          updateSelectInput(session, inputId = "Blank", choices=c(selectedBlank[[1]]))
        })
        message1 <- paste0(newBlankValue, " added as Blank for ", newBlankPeriodValue, " Period.")
        output$message <- renderText({message1})
      }
      else { output$message <- renderText({print("That Blank already exists for this Period")})}
      
      #could potentially be used to loop through items in a list of periods, applying the same newBlankValue for each one, but some work is still needed to get this to work
      #  var1 <- newBlankValue
      #  var2 <- newBlankPeriodValue
      #  for(i in 1:length(var2)){
      #    req <- paste("INSERT INTO `blanks_excavation` (`Blank`, `Period`) VALUES   
      #                 (",var1[i],",",var2[i],")",sep="")
      
      
      
      observeEvent(input$submitModification, {
        #collect reactive values pertaining to the entered values and coerce the data types
        newModificationResponses <- reactiveValues(
          Modification = input$newModification, Period = input$newBlankModPeriod)
        newModificationValue <- as.character(newModificationResponses$Modification)
        newModificationPeriodValue <- as.character(newModificationResponses$Period) #would be courced as a list for new looping method described below
        
        #collect and subset the list of existing modifications and limit action based on whether the blank already exists
        filteredModifications <- data.frame(
          select(filter(modifications, Period == input$newBlankModPeriod), Modification))
        if (any(filteredModifications$Modification == newModificationValue) == FALSE) {
          #generate and execute SQL INSERT statement
          newModificationInsert <- glue::glue_sql("INSERT INTO `modifications_excavation` (`Modification`, `Period`) VALUES ({newModificationValue}, {newModificationPeriodValue})"
                                                  , .con = pool)
          dbExecute(pool, sqlInterpolate(ANSI(), newModificationInsert))
          modifications <<- dbReadTable(pool, 'modifications_excavation')
          modificationChoices <- reactive({
            select(filter(modifications, Period==input$Period), Modification)
          })
          observe({
            selectedModification <- modificationChoices()
            updateSelectInput(session, inputId = "Modification", choices=c(selectedModification[[1]]))
          })
          message2 <- paste0(newModificationValue, " added as Modification for ", newModificationPeriodValue, " Period.")
          output$message <- renderText({message2})
        }
        else { output$message <- renderText({print("That Modification already exists for this Period")})}
        
        #could potentially be used to loop through items in a list of periods, applying the same newModificationValue for each one, but some work is still needed to get this to work
        #  var1 <- newModificationValue
        #  var2 <- newModificationPeriodValue
        #  for(i in 1:length(var2)){
        #    req <- paste("INSERT INTO `modifications_excavation` (`Modification`, `Period`) VALUES   
        #                 (",var1[i],",",var2[i],")",sep="")
        
      })
      
      
    })
    
    
  })
shinyApp(ui, server)

