library(shiny)
library(xtable)
library(DT)

allSurvey.transects.grids.grabs <- read.csv("allSurvey.all.csv")

#### server
server <- function(input, output, session) {
  output$Level2_Table <- DT::renderDataTable(
    datatable(allSurvey.transects.grids.grabs, extensions = 'Buttons', filter="top", options = list(dom='Bfrtip', buttons= I('colvis'), columnDefs = list(list(searchable = F, targets = c(5:53)))), selection=list(mode="single",target="cell"))) 
  ## could also fix lefthand columns, but that seems not to get along with a colVis button
  
  #select a cell to generate new table with rows/artifact which is to be filled in
  BlankMod_index <- reactive({input$Level2_Table_cells_selected})
  
  
  BlankMod_table <- reactive({ 
    BlankMod_indexed <- BlankMod_index()
    BlankMod_table_a <- matrix(nrow=allSurvey.transects.grids.grabs[BlankMod_indexed[1],BlankMod_indexed[2]], ncol=10) #create object of nrows determined by value of selected cell and ncol determined by our Artifact ID table needs  ##probably should precede this by a search for any ArtifactIDs attached to artifacts from the selected cell
    if (nrow(BlankMod_table_a) > 0) {
      artifactIDs <- paste0("AR",seq(1,nrow(BlankMod_table_a),by=1)) #get artifact IDs (placeholder system for now; last ArtifactID will have to get retrieved from table of artifact IDs and these increment on from there, e.g. last(ArtifactIDTable$ID))
      BlankMod_table_a[,1] <- artifactIDs  
      knownvals <- cbind(allSurvey.transects.grids.grabs[BlankMod_indexed[1],c(3,1,2,4)], colnames(allSurvey.transects.grids.grabs)[BlankMod_indexed[2]]) #retrieve available values from allSurvey.transects.grids.grabs
      BlankMod_table_a <- data.frame(BlankMod_table_a) 
      colnames(BlankMod_table_a) <- c("ArtifactID", "Specifically_Assigned","Source","CollectionPointID","Period","Blank","Modification","Drawing","Raw_Material","Weathering")
      BlankMod_table_a[,3:7] <- knownvals
    }
    return(BlankMod_table_a)
  })
  
  observeEvent(input$Level2_Table_cells_selected, {
    output$Level3_artifacttable <- DT::renderDataTable(datatable(BlankMod_table(), editable=T))
  })
}

#### user interface
ui <- fluidPage(
  
  titlePanel("Querying SNAP Survey Level 2 Data"),
  
  
  tabsetPanel(type="tabs",
              tabPanel("All Survey Data",
                       DT::dataTableOutput("Level2_Table")
              ),
              tabPanel("Artifact ID Table", 
                       #verbatimTextOutput('x4')  #for checking what index is returned
                       DT::dataTableOutput("Level3_artifacttable")
              )
  )
)



shinyApp(ui = ui, server = server)



