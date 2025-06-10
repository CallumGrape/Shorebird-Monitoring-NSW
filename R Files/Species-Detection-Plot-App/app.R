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
      selectInput(inputId = "speciesEN",
                  label = "Species",
                  choices = (tagSummary$speciesEN %>% unique()),
                  selected = "Pied Stilt"),
      
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
  
  ## Change min/max date ranges based on species input
  observe({
    updateDateRangeInput(session, "dateRange",
                         min = (df.alltags %>% filter(speciesEN == input$speciesEN))$dateAus %>% min(),
                         max = (df.alltags %>% filter(speciesEN == input$speciesEN))$dateAus %>% max(),
                         start = ((df.alltags %>% filter(speciesEN == input$speciesEN))$dateAus %>% max()) - days(3),
                         end = (df.alltags %>% filter(speciesEN == input$speciesEN))$dateAus %>% max())
  })
  
  output$detectionPlot <- renderPlot({
    tmp.detections <- filter(df.alltags, speciesEN == input$speciesEN, dateAus >= input$dateRange[1], dateAus <= input$dateRange[2])
    
    p <- tmp.detections %>% ggplot(aes(x = timeAus, y = sigPositive)) + # x-axis = signal strength, y-axis = time
      
      # Plot points
      geom_point(aes(colour = as.character(motusTagID)),
                 shape = pointShape,
                 size = pointSize) + # 
      
      # Grid by receiver
      facet_grid(vars(recvDeployName), switch = "both") +
      #facet_wrap(~factor(recvDeployName, levels = c("Tomago","Fullerton Cove","Curlew Point")), dir = "v", strip.position = "right") +
      
      # Legend
      labs(colour = "Motus Tag ID", ) + 
      guides(colour = guide_legend(order = 1)) + # Make this legend appear first
      
      
      # Axis titles
      ylab("Signal Strength") + 
      xlab("Date-Time") +
      
      # Font Size
      theme_gray(base_size = 14) +
      
      ## Automatic title 
      #ggtitle(paste("Detections for",currentSpecies)) + theme(plot.title = element_text(hjust = 0.5), legend.justification = c("right","top"))+  # Plot title
      
      ## Gridlines and background
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(panel.background = element_blank()) +
      ## Custom title
      # ggtitle("Pied Stilt Detections") + theme(plot.title = element_text(hjust = 0.5)) # Plot title
      
      scale_x_datetime(breaks = "12 hours",expand = c(0,0), date_labels = "%e %b\n%H:%M")
    
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