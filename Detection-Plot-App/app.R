library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Shorebird Detections"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Options for species
      selectInput(inputId = "speciesId",
      label = "Species",
      choices = (tagSummary$speciesEN %>% unique()),
      selected = "Pied Stilt"),
      
      # Input: Options for tag IDs
      selectInput(inputId = "tagID",
      label = "Motus Tag ID",
      choices = tagSummary$motusTagID,
      selected = 60471),
      
      # Input: Start and end Time
      dateRangeInput(inputId = "dateRange",
                  label = "Date Range"),

      # Input: Tide curve and sunrise/set
      checkboxInput(inputId = "tidalCurve",
                    label = "Tidal Curve",
                    value = TRUE),

      #checkboxInput(inputId = "sunRiseSet",
      #              label = "Sunrise and Sunset",
      #              value = TRUE),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "detectionPlot"),
      imageOutput("speciesImage")
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  ## Change min/max date ranges based on tag input
  observe({
    updateDateRangeInput(session, "dateRange",
                         min = (df.alltags %>% filter(motusTagID == input$tagID))$dateAus %>% min(),
                         max = (df.alltags %>% filter(motusTagID == input$tagID))$dateAus %>% max(),
                         start = ((df.alltags %>% filter(motusTagID == input$tagID))$dateAus %>% max()) - days(3),
                         end = (df.alltags %>% filter(motusTagID == input$tagID))$dateAus %>% max())
  })
  
  observe({
    updateSelectInput(session, "tagID",
                      choices = ((tagSummary %>% filter(speciesEN == input$speciesId))$motusTagID),
                      selected = (tagSummary %>% filter(speciesEN == input$speciesId))$motusTagID[1])
  })
  
  output$detectionPlot <- renderPlot({
    tmp.detections <- filter(df.alltags, motusTagID == input$tagID, dateAus >= input$dateRange[1], dateAus <= input$dateRange[2])
    
    p <- ggplot(tmp.detections, aes(x = timeAus, y = sigPositive, colour = recvDeployName)) + geom_point()
    
    # Add tidal curve
    if (input$tidalCurve) {
      p <- plot.addTidalCurveShiny(p, tmp.detections)
    }
    
    # Add sunset and sunrise
    #if (input$sunRiseSet) {
    #  p <- plot.addSunriseSet(p)
    #}
    
    p
    
  })
  
  output$speciesImage <- renderImage({
    filename <- normalizePath(file.path('./Images', paste(input$speciesId,'.jpeg',sep = '')),winslash = "/")
    list(src = filename, width = 200)
  }, deleteFile = FALSE)

  
}

shinyApp(ui = ui, server = server)