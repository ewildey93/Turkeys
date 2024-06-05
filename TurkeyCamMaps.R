library(readxl)
library(sf)
library(basemaps)
library(ggplot2)
#########################
######functions##########
#########################
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, skip = 1))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
#########################
##spatial map for cams###
#########################
TurkeyHist <- read_excel_allsheets("//central/OAS/Sandhill/Turkey Project/Datasheets/Turkey Histories.xlsx")
TurkeyCamMaps <- list()
for(i in 1:length(TurkeyHist)) {
  df <- TurkeyHist[[i]]
  CamEvents <- df[df$Event == "Camera",]
  CamEvents$DateTime <- paste(CamEvents$Date, CamEvents$Time)
  #time column looks weird being pulled in from excel
  CamEvents$DateTime <- as.POSIXct(CamEvents$DateTime, "%Y-%m-%d %H:%M")
  CamEventsSF <- st_as_sf(CamEvents, coords=c("Latitude", "Longitude"), crs= 4326)
  CamLines <- CamEventsSF %>% 
    group_by(Camera) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
  map <- ggplot() +
  basemap_gglayer(ext, map_service = "mapbox", map_type = "satellite")
  
  TurkeyCamMaps <- append(TurkeyCamMaps, list(CamMap))

}