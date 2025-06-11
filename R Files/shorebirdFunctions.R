### This file defines functions to be used by the other shorebird scripts
## Useful functions for quickly getting information out of data frame ----
tag.species <- function(tagID) {
  # Get species common name from tag ID
  return(unique(filter(df.alltags,motusTagID==tagID)$speciesEN))
}

tag.burst <- function(tagID) {
  # Get burst interval in seconds from tag ID
  return(unique(filter(df.alltags,motusTagID==tagID)$tagBI))
}

## Functions to generate and display summaries ----
generateReceiverSummary <- function(){
  print("Generating detection summary for each receiver")
  
  # Get list of receivers from the data frame. 
  # NOTE: This means that receivers that haven't detected tags will not be included.
  receiverList <- df.alltags$recvDeployName %>% unique()
  receiverCount <- receiverList %>% length()
  
  # Create empty data frame to use for the summary
  receiverSummary <- as.data.frame(receiverList)
  colnames(receiverSummary) <- c("recvDeployName")
  
  for (i in 1:receiverCount) {
    # Get the first and most recent detections for each station.
    # Note: suppressWarnings is used for warning message regarding NA values (station with no detections)
    receiverSummary$recentDetection[i] <- df.alltags %>% filter(recvDeployName == receiverList[i]) %>% dplyr::select(ts) %>% max() %>% suppressWarnings()
    receiverSummary$firstDetection[i] <- df.alltags %>% filter(recvDeployName == receiverList[i]) %>% dplyr::select(ts) %>% min() %>% suppressWarnings()
  }
  
  # Convert detection times to Australian (Sydney) timezone
  receiverSummary <- mutate(receiverSummary, 
                         recentDetectionAus = recentDetection %>% as_datetime(tz = "Australia/Sydney"),
                         firstDetectionAus = firstDetection %>% as_datetime(tz = "Australia/Sydney"))
  
  # Remove columns with original integer time value
  receiverSummary <- receiverSummary %>% dplyr::select(recvDeployName, firstDetectionAus, recentDetectionAus)
  
  # Sort by most recent detection
  receiverSummary <- receiverSummary %>% arrange(desc(recentDetectionAus))
  
  # Count the number of unique tags detected by each station, and add to the data frame.
  receiverSummary <- left_join(receiverSummary, df.alltags %>% 
                              count(recvDeployName,motusTagID) %>% 
                              count(recvDeployName) %>% 
                              rename("uniqueTags"="n"), by = "recvDeployName")
  
  # Add list of species that the receiver has detected
  receiverSummary <- cbind(receiverSummary, species = NA)
  for (i in 1:nrow(receiverSummary)){
    receiverSummary$species[i] <- df.alltags %>% filter(recvDeployName == receiverSummary$recvDeployName[i]) %>% dplyr::select(speciesEN) %>% unique() %>% list()
  }
  
  # Add list of tags that the receiver has detected
  receiverSummary <- cbind(receiverSummary, motusTagID = NA)
  for (i in 1:nrow(receiverSummary)){
    receiverSummary$motusTagID[i] <- df.alltags %>% filter(recvDeployName == receiverSummary$recvDeployName[i]) %>% dplyr::select(motusTagID) %>% unique() %>% list()
  }
  
  # Adding lat and long
  for (i in 1:receiverCount) {
    receiverSummary$lat[i] <- df.alltags %>% filter(recvDeployName == receiverSummary$recvDeployName[i]) %>% dplyr::select(recvDeployLat) %>% unique()
    receiverSummary$lon[i] <- df.alltags %>% filter(recvDeployName == receiverSummary$recvDeployName[i]) %>% dplyr::select(recvDeployLon) %>% unique() 
  }
  
  # Add list of serno's and motus device IDs that have been deployed for the station name (recvDeployName)
  for (i in 1:receiverCount){
    receiverSummary$sernos[i] <- df.recvDeps %>% 
      filter(stationName == receiverSummary$recvDeployName[i]) %>% 
      select(serno) %>% 
      unique()
    
    receiverSummary$deviceIDs[i] <- df.recvDeps %>% 
      filter(stationName == receiverSummary$recvDeployName[i]) %>% 
      select(deviceID) %>% 
      unique()
    
    receiverSummary$deployIDs[i] <- df.recvDeps %>% 
      filter(stationName == receiverSummary$recvDeployName[i]) %>% 
      select(deployID) %>% 
      unique()
  }
  
  return(receiverSummary)
}

generateTagSummary <- function(){
  print("Generating summary of detections for each tag")
  
  #df.alltags$motusTagID <- df.alltags$motusTagID %>% as.character() %>% as.numeric()
  #tagIDs <- tagIDs %>% as.factor()
  
  tagSummary <- as.data.frame(tagIDs)
  colnames(tagSummary) <- c("motusTagID")
  
  for (i in 1:length(tagIDs)) {
    # Find the first and most recent detection for each tag.
    # Note: suppresswarnings() used due to not all tags having detections (they are filtered out later)
    tagSummary$firstDetection[i] <- df.alltags %>% filter(motusTagID == tagIDs[i]) %>% dplyr::select(ts) %>% min() %>% suppressWarnings()
    tagSummary$recentDetection[i] <- df.alltags %>% filter(motusTagID == tagIDs[i]) %>% dplyr::select(ts) %>% max() %>% suppressWarnings()
  }
  
  #df.alltags$motusTagID <- df.alltags$motusTagID %>% as.factor()
  #tagSummary$motusTagID <- tagSummary$motusTagID %>% as.factor()
  
  tagSummary <- mutate(tagSummary, firstDetectionAus = firstDetection %>% as_datetime(tz = "Australia/Sydney"), recentDetectionAus = recentDetection %>% as_datetime(tz = "Australia/Sydney"))
  tagSummary <- tagSummary %>% select(motusTagID, recentDetectionAus, firstDetectionAus)
  tagSummary <- tagSummary %>% arrange(desc(recentDetectionAus))
  
  # Filter out tags with no detections
  tagSummary <- filter(tagSummary, is.finite(firstDetectionAus))
  
  # Number of days between first and last detection
  tagSummary <- mutate(tagSummary, numDays = as.integer(date(recentDetectionAus) - date(firstDetectionAus) + 1))
  
  # Add species
  tagSummary <- cbind(tagSummary, speciesEN = NA)
  for (i in 1:nrow(tagSummary)){
    tagSummary$speciesEN[i] <- tag.species(tagSummary$motusTagID[i])
  }
 
  # Coerce tag ID to factor
  tagSummary$motusTagID <- tagSummary$motusTagID %>% as.factor()
   
  # Number of days each individual was detected
  tagSummary <- full_join(tagSummary, df.alltags %>% count(dateAus,motusTagID) %>% count(motusTagID) %>% rename("detectionDays"="n"), by = "motusTagID")
  
  ## What proportion of days (between first and last detection) was each tag detected?
  tagSummary <- mutate(tagSummary, detectionProportion = detectionDays/numDays)
  
  # Add burst interval (data from motus)
  #project294.motus <- dbConnect(SQLite(), "project-294.motus")
  #tbl.tags <- tbl(project294.motus, "tags") %>% collect() %>% as.data.frame()
  #rm(project294.motus)
  #tbl.tags$tagID <- tbl.tags$tagID %>% as.factor()
  #tagSummary <- left_join(tagSummary, tbl.tags, by = join_by(motusTagID == tagID))
  
  # Add list of receivers that tag has been detected at
  tagSummary <- cbind(tagSummary, receivers = NA)
  for (i in 1:nrow(tagSummary)){
    tagSummary$receivers[i] <- df.alltags %>% filter(motusTagID == tagSummary$motusTagID[i]) %>% dplyr::select(recvDeployName) %>% unique() %>% list()
  }
  
  return(tagSummary)
}

generateSpeciesSummary <- function(){
  speciesSummary <- df.alltags %>% 
    select(speciesEN) %>% 
    distinct()
  
  unique_by_species <- df.alltags %>% 
    group_by(speciesEN) %>% 
    summarise(uniqueTags = n_distinct(motusTagID), .groups = "drop")
  
  speciesSummary <- speciesSummary %>% 
    left_join(unique_by_species, by = "speciesEN")
  
  speciesSummary <- speciesSummary %>% 
    mutate(
      motusTagIDs = purrr::map(.x = speciesEN, .f = function(species){
      df.alltags %>% 
        filter(speciesEN == species) %>% 
        distinct(motusTagID) %>% 
        pull(motusTagID) 
    }),
      receivers = purrr::map(.x = speciesEN, .f = function(species){
        df.alltags %>% 
          filter(speciesEN == species) %>% 
          distinct(recvDeployName) %>% 
          pull(recvDeployName)
    }))
  
  speciesSummary <- speciesSummary %>% 
    as_tibble()
  
  return(speciesSummary)
}
## Functions to help with plotting ----
plot.addSunriseSet <- function(p){
  p <- p + ggnewscale::new_scale_colour() +
    geom_vline(data = as_data_frame(plot.sunrise), aes(xintercept = value, colour="sunriseColour"),alpha = sunLineAlpha,linewidth = sunLineWidth, linetype = sunLineType) +
    geom_vline(data = as_data_frame(plot.sunset), aes(xintercept=value,colour="sunsetColour"), alpha = sunLineAlpha,linewidth = sunLineWidth, linetype = sunLineType) +
    scale_color_manual(name = "Sun",
                       labels = c("Sunrise","Sunset"),
                       values = c(sunriseColour,sunsetColour),
                       guide = guide_legend(order = 2))
  return(p)
}

plot.addTideHighLow <- function(p){
  p <- p + ggnewscale::new_scale_colour() +
    geom_vline(data = filter(tideData, high_low == "High"), aes(xintercept = tideDateTimeAus, colour = high_low),
               alpha = 0.3,
               linewidth = 1) +
    geom_vline(data = filter(tideData, high_low == "Low"), aes(xintercept = tideDateTimeAus, colour = high_low),
               alpha = 0.3, linewidth = 1) +
    scale_color_manual(name = "Tides",
                       labels = c("High Tide","Low Tide"),
                       values = c(highTideColour,lowTideColour),
                       guide = guide_legend(order = 3))
  return(p)
}

plot.addTidalCurve <- function(p){
  # Filter tidal curve by time range
  tmpTide <- filter(tidalCurve, time >= timeStart, time <= timeEnd)
  
  # Scaling factor to plot tide on the same axes as signal strength
  scaleCoeff <- (max(tmpTide$height)-min(tmpTide$height))/(max(tmp$sigPositive)-min(tmp$sigPositive))
  
  # Vertical shift amount to centre tidal curve on plot
  verticalShift <- ((max(tmpTide$height/scaleCoeff)+min(tmpTide$height/scaleCoeff))/2) - ((max(tmp$sigPositive)+min(tmp$sigPositive))/2)
  
  # Plot tidal curve
  p <- p + geom_line(data = tmpTide, aes(x = time, y = height/scaleCoeff-verticalShift) ,colour = tideColour, linewidth = tideLineWidth, alpha = tideAlpha)  +
    theme(
      axis.title.y.right = element_text(size = fontSizeAxisTitle, margin = margin(l = paddingAxisTitle)),
      axis.text.x = element_text(size = fontSizeAxisTicks)
    ) 
  
  # Add second axis for tide height
  p <- p +  scale_y_continuous(
    sec.axis = sec_axis(~.*scaleCoeff+verticalShift*scaleCoeff, name = "Tide Height (m)")
  )
  return(p)
}

plot.addTidalCurveShiny <- function(p, tmp.detections){
  # Filter tidal curve by time range
  tmpTide <- filter(tidalCurve, time >= layer_scales(p)$x$range$range[1], time <= layer_scales(p)$x$range$range[2])
  
  # Scaling factor to plot tide on the same axes as signal strength
  scaleCoeff <- (max(tmpTide$height)-min(tmpTide$height))/(max(tmp.detections$sigPositive)-min(tmp.detections$sigPositive))
  
  # Vertical shift amount to centre tidal curve on plot
  verticalShift <- ((max(tmpTide$height/scaleCoeff)+min(tmpTide$height/scaleCoeff))/2) - ((max(tmp.detections$sigPositive)+min(tmp.detections$sigPositive))/2)
  
  # Plot tidal curve
  p <- p + geom_line(data = tmpTide, aes(x = time, y = height/scaleCoeff-verticalShift) ,colour = tideColour, linewidth = 3, alpha = 0.3)  +
    theme(
      axis.title.y.right = element_text(size = 16)
    ) 
  
  # Add second axis for tide height
  p <- p +  scale_y_continuous(
    sec.axis = sec_axis(~.*scaleCoeff+verticalShift*scaleCoeff, name = "Tide Height (m)")
  )
  return(p)
}

save.plot <- function(title = paste(p$labels$title," ",format(currentTime,"%Y-%m-%d %H-%M-%S"),sep = ""), format = "png", dir_path = "Plots") {
  currentTime <- with_tz(Sys.time(),"Australia/Sydney")
  if (title != paste(p$labels$title," ",format(currentTime,"%Y-%m-%d %H-%M-%S"),".",format,sep = "")){
    title <- paste(title,".",format,sep = "")
  }
  ggsave(title,plot = p, path = dir_path)
  return()
}


## Functions to quickly produce complete plots with default settings ----
plot.detectionsVsTideHeight <- function(species.tmp) {
  
  # Count number of detections at each tide height (for each species and site)
  df.tideHeight <- df.alltags %>% 
    mutate(tideHeight = round(tideHeight,2)) %>% # Round to 2 decimal places
    count(tideHeight,speciesEN,recvDeployName) %>% 
    rename("detections" = "n")
  
  # Filter by species and site of interest
  df.tideHeight.tmp <- df.tideHeight %>% filter(speciesEN == species.tmp)
  
  # Scatter plot
  p <- ggplot(df.tideHeight.tmp, aes(x=tideHeight, y = detections)) + geom_point() + 
    ggtitle(paste("Tide Height and Detection Count for",species.tmp)) +
    theme_gray(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~recvDeployName, scales = "free") +
    #facet_grid(vars(recvDeployName))
    
    # Axis labels
    xlab("Tide Height (m)") + ylab("Detection Count")
  
  return(p)
}
## Tide category analysis (old / not tested recently) ----
get.tideCategory <- function(timeVal){
  # Determine which tidal category (high/falling/low/rising) each detection is in, by finding the closest temporal value. 
  tideCategory <- tideCategoryData$tideCategory[which.min(abs(tideCategoryData$dateTimeAus-timeVal))]
}

get.tideCategoryHeight <- function(timeVal){
  # Determine the height of the tide category (high/falling/low/rising) each detection is in, by finding the closest temporal value. 
  # Height for falling/rising not used (just for analysis of high and low tide amplitude) 
  tideCategoryHeight <- tideCategoryData$tideCategoryHeight[which.min(abs(tideCategoryData$dateTimeAus-timeVal))]
}

get.tideCategoryTime <- function(timeVal){
  # Determine which tidal category (high/falling/low/rising) each detection is in, by finding the closest temporal value. 
  tideCategory <- tideCategoryData$dateTimeAus[which.min(abs(tideCategoryData$dateTimeAus-timeVal))]
}

get.tideHeight <- function(timeVal){
  # Determine the tide height of each detection, based on the spline interpolated tidal curve.
  tideHeight <- tidalCurve$height[which.min(abs(tidalCurve$time-timeVal))]
}

niceTagSummary <- function(){
  tagSummary %>% dplyr::select(motusTagID, speciesEN, recentDetectionAus, firstDetectionAus, detectionDays, detectionProportion) %>% rename("First_Detection" = "firstDetectionAus", "Last_Detection" = "recentDetectionAus", "Days_Detected" = "detectionDays", "Rate" = "detectionProportion") %>% mutate(First_Detection = as.Date(First_Detection), Last_Detection = as.Date(Last_Detection), Rate = percent(Rate)) %>% relocate(First_Detection, .before = Last_Detection) %>% arrange(desc(Last_Detection)) %>% 
  kable(align = "l") %>% kableExtra::kable_styling()
}

tag.firstDetection <- function(tagID){
  firstDetection <- (tagSummary %>% filter(motusTagID == tagID))$firstDetectionAus
  return(firstDetection)
}

tag.lastDetection <- function(tagID){
  lastDetection <- (tagSummary %>% filter(motusTagID == tagID))$recentDetectionAus
  return(lastDetection)
}

deviceID_from_name <- function(recvDeployName){
  
  df.recvDeps <- readRDS("Data/df.recvDeps.rds")
  
  deviceIDs <- df.recvDeps %>%
    filter(stationName == recvDeployName) %>% 
    select(deviceID) %>% 
    distinct()
  
  return(deviceIDs$deviceID)
}

serno_from_name <- function(recvDeployName){
  
  df.recvDeps <- readRDS("Data/df.recvDeps.rds")
  
  deviceIDs <- df.recvDeps %>%
    filter(stationName == recvDeployName) %>% 
    select(serno) %>% 
    distinct()
  
  return(deviceIDs$serno)
}

deployID_from_name <- function(recvDeployName){
  
  deployIDs <- df.recvDeps %>%
    filter(stationName == recvDeployName) %>% 
    select(deployID) %>% 
    distinct()
  
  return(deployIDs$deployID)
}

name_from_ID <- function(stationID){
  
  if(is.character(stationID)){
   name <- receiverSummary %>% 
     filter(map_lgl(sernos, ~stationID %in% .x))
  } else{
    name <- receiverSummary %>%
      filter(map_lgl(deviceIDs, ~stationID %in% .x))
  }
  return(name$recvDeployName)
}

activity_by_name <- function(target_station_name){
  
  # Get deployments
  station_deployments <- df.recvDeps %>%
    filter(stationName == target_station_name) %>%
    select(deviceID, tsStart, tsEnd)
  
  station_deployments %>% datatable(caption = paste0("Deployments for Station '",target_station_name,"'."))
  
  # Set arbitrary max end date for current deployment
  max_ts_future <- as.numeric(as.POSIXct("2100-01-01", tz = "UTC")) # Define a date far in the future
  station_deployments <- station_deployments %>%
    mutate(tsEnd = if_else(is.na(tsEnd), max_ts_future, tsEnd))
  
  # Filter the activity dataframe to our station of interest.
  ## semi_join necessary here to filter by multiple conditions
  ## due to the complications of having several receiver deployments at the same
  ## site / under the same name, and having the same receiver deployed at different
  ## sites / under different names.
  df.activity.station <- df.activity %>%
    semi_join(
      station_deployments,       # The lookup table defining valid deployments
      by = join_by(
        motusDeviceID == deviceID,  # Condition 1: motusDeviceID from left == deviceID from right
        hourBinStart_ts >= tsStart,         # Condition 2: hourBin from left >= tsStart from right
        hourBinEnd_ts <= tsEnd            # Condition 3: hourBin from left <= tsEnd from right
      )
    )
  
  # Step 7: Print the result
  df.activity.station %>% datatable()
  
  # Display first and last times
  cat("The beginning of the first hour bin for",target_station_name,"is",format(min(df.activity.station$hourBinStart)))
  cat("\nThe end of the last hour bin for",target_station_name,"is",format(max(df.activity.station$hourBinEnd)))
  
  return(df.activity.station)
}