# ---- Load Packages  ----
library(dplyr)
library(lubridate)
library(bioRad) # Sunrise/set from coordinates

# ---- Load Shorebird Data ----


# Read high/low tide data from file
tideData <- read.csv("Data/Tide/TideDataNewcastle.csv")
#beep again
# Format date and datetime columns
tideData$tideDateTimeAus <- ymd_hms(tideData$tideDateTime, tz = "Australia/Sydney")
tideData <- tideData %>% mutate(
  date = as.POSIXct(date, format = "%d/%m/%Y", tz = "Australia/Sydney")
)
# Generate tidal curve
# NOTE: Spline fits tide data quite well, EXCEPT for the second and second last values
print("Interpolating tidal curve")
tidalCurve <- as.data.frame(spline(tideData$tideDateTimeAus, tideData$tideHeight, n = 1000*length(tideData$tideDateTimeAus), method = "natural"))
tidalCurve$x <- as_datetime(tidalCurve$x, tz = "Australia/Sydney")
tidalCurveFunc <- splinefun(tideData$tideDateTimeAus, tideData$tideHeight, method = "natural")

colnames(tidalCurve) <- c("time","height")

# ==== Classify tides as diurnal or nocturnal====
# Nocturnal = before sunrise or after sunset.
# Diurnal = after sunrise and before sunset.

# Add Newcastle sunrise and sunset times to each tide point
## Lat = 32° 55’ S, Lon = 151° 47’ E from BoM Tide Data
## Converted to decimal degrees, Lat = -32.9167, Lon = 151.7833)
tideData <- tideData %>% mutate(
  sunriseNewc = sunrise(date, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
  sunsetNewc = sunset(date, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
  sunriseNewcTime = strftime(sunriseNewc, format = "%H:%M:%S", tz = "Australia/Sydney"),
  sunsetNewcTime = strftime(sunsetNewc, format = "%H:%M:%S", tz = "Australia/Sydney")
)

# Define each tide point as either diurnal or nocturnal. 
tideData <- tideData %>% mutate(
  day_night = case_when(
    tideDateTimeAus < sunriseNewc ~ "Nocturnal",
    tideDateTimeAus > sunriseNewc & tideDateTimeAus < sunsetNewc ~ "Diurnal",
    tideDateTimeAus > sunsetNewc ~ "Nocturnal"
  )
)

# ==== Categorise each tide by tidal/diel period and give numeric ID ====
# Define each tide point by day / night and high / low
tideData <- tideData %>% mutate(  
    tideCategory = case_when(
      high_low == "Low" & day_night == "Diurnal" ~ "Diurnal_Low",
      high_low == "Low" & day_night == "Nocturnal" ~ "Nocturnal_Low",
      high_low == "High" & day_night == "Diurnal" ~ "Diurnal_High",
      high_low == "High" & day_night == "Nocturnal" ~ "Nocturnal_High"
    ) %>% as_factor()
)

# Add numeric ID to each category, allowing for unique tide bins
tideData <- tideData %>%
  group_by(tideCategory) %>% 
  mutate(tideID = paste0(tideCategory, "_", row_number())) %>% 
  ungroup()

# ==== Save Tide Data to File ====

saveRDS(tideData, "Data/tideData.rds")
saveRDS(tidalCurve, "Data/tidalCurve.rds")
saveRDS(tidalCurveFunc, "Data/tidalCurveFunc.rds")

### OLD
## Categorise detections into tidal category ----

# Data generated from original TideData spreadsheet, categorising times into 
# either "High", "Falling", "Low", or "Rising" tides. Rising and falling values 
# were calculated as the midpoint between the nearest high and low tide. 
# The function get.tideCategory determines which tidal category a data point falls into by finding the nearest value. 

## Import tide category data
#tideCategoryData <- read.csv("Tide/TideCategoriesNewcastle2023.csv")

# Convert dates to POSIXct format
#tideCategoryData$dateTimeAus <- ymd_hms(tideCategoryData$dateTimeAus, tz = "Australia/Sydney")

## Commented out as this bit takes a super long time
## Assign tide category and calculate time since nearest category value (e.g. nearest high / low / falling / rising)
#print("Assigning tidal category to each detection")
#df.alltags <- df.alltags  %>% rowwise() %>% mutate(tideCategory = get.tideCategory(timeAus), tideCategoryHeight = get.tideCategoryHeight(timeAus), tideCategoryTime = get.tideCategoryTime(timeAus)) %>% ungroup()
#df.alltags <- df.alltags %>% mutate(timeSinceTideCategory = (difftime(timeAus, tideCategoryTime, units = "hours") %>% as.numeric))






