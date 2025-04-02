plot.detections <- function(method, name, timeStart, timeEnd){
  
  if (method == "tag"){
    currentTagID <- name
    
    plotTidalCurve <- 1
    plotSun <- 1
    plotTide <- 0
    pointSize <- 1.5
    
    timeStart <- timeStart %>% ymd_hms(tz = "Australia/Sydney")
    timeEnd <- timeEnd %>% ymd_hms(tz = "Australia/Sydney")
    
    # Filter by tag and time interval
    tmp <- filter(df.alltags, motusTagID == currentTagID, timeAus >= timeStart, timeAus <= timeEnd)
    
    # Create plot
    p <- tmp %>% 
      ggplot(aes(timeAus, sigPositive)) + # x-axis = signal strength, y-axis = time
      
      # Plot points
      geom_point(aes(colour = recvDeployName), 
                 size = pointSize, 
                 shape = pointShape) + 
      
      ## Automatic legend
      labs(colour = "Receiver") + # Legend title
      guides(colour = guide_legend(order = 1)) + # Make this legend appear first
      
      # Axis titles
      ylab("Signal Strength") + 
      xlab("Date-Time") + 
      
      ## Automatic plot title
      ggtitle(paste("Detections for Tag",currentTagID,"(",tagInfo %>% filter(motusTagID==currentTagID) %>% select(species),")")) +
      theme_gray(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add sunset and sunrise
    if (plotSun) {
      p <- plot.addSunriseSet(p)
    }
    
    # Add high and low tides
    if (plotTide) {
      p <- plot.addTideHighLow(p)
    }
    
    # Add tidal curve
    if (plotTidalCurve) {
      p <- plot.addTidalCurve(p, tmp, timeStart, timeEnd)
    }
    
    
  } else if (method == "species") {
    
  } else if (method == "receiver") {
    
  }
  
  return(p)
}