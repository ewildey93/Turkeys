library(readxl)
library(sf)
library(basemaps)
library(ggplot2)
library(dplyr)
library(ggmap)
library(leaflet)
library(leaflet.extras2)
library(leaflet.minicharts)
library(osmdata)
library(plotly)
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
register_google("AIzaSyC942RKkTJtniApeagdf7VmJuCWWHdOMQo", write=TRUE)
TurkeyCamMaps <- list()
for(i in 1:length(TurkeyHist)) {
  if(nrow(TurkeyHist[[i]] >1)){
  df <- TurkeyHist[[i]]
  CamEvents <- subset(df, df$Event == "Camera")
  CamEvents$Time <- format(CamEvents$Time, "%H:%M")
  CamEvents$Date2 <- format(CamEvents$Date, "%m-%d-%y")
  #time column looks weird being pulled in from excel
  CamEventsSF <- st_as_sf(CamEvents, coords=c("Longitude", "Latitude"), crs= 4326)
  CamEventsSF$Camera <- as.factor(CamEventsSF$Camera)
  CamLines <- CamEventsSF %>% 
    group_by(Camera) %>% filter(n() >1) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")

  pal <- colorFactor("plasma", levels = levels(CamEventsSF$Camera))
  
  map <- get_map(location = c(mean(CamEvents$Longitude), mean(CamEvents$Latitude)),maptype="hybrid", source="google", zoom=16)
  p <- ggmap(map) +
    geom_sf(data=CamEventsSF,inherit.aes=FALSE, aes(color=Camera), show.legend = "point") +
    geom_sf_text(data=CamEventsSF,inherit.aes = FALSE, aes(label=Date2), show.legend=FALSE, color="white", size=3) +
    geom_sf(data=CamLines, inherit.aes= FALSE, arrow=arrow(), aes(color=Camera), show.legend = FALSE)
  
  #CamMap <- ggplotly(p)
  CamMap <- p
  
  
  TurkeyCamMaps <- append(TurkeyCamMaps, list(CamMap))
  }
}


##########scrap paper###############
plot(st_geometry(CamLines[c(1,2,5),]))
st_geometry(CamLines)

pal <- colorFactor("plasma", levels = levels(CamEventsSF$Camera))

map <- get_map(location = c(mean(CamEvents$Longitude), mean(CamEvents$Latitude)),maptype="hybrid", source="google", zoom=16)
ggmap(map) +
  geom_sf(data=CamEventsSF,inherit.aes=FALSE, aes(color=Camera), show.legend = "point") +
  geom_sf_text(data=CamEventsSF,inherit.aes = FALSE, aes(label=Date2), show.legend=FALSE, color="white", size=2) +
  geom_sf(data=CamLines, inherit.aes= FALSE, arrow=arrow(), aes(color=Camera), show.legend = FALSE)

ggplotly(p)

ggplotly(p) %>% theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#use this palette for both lines and circles, while entering different layer names in color=pal(...) and it'll match colors
pal <- colorFactor("plasma", levels = levels(CamEventsSF$Camera))

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  setView(lng = mean(CamEvents$Longitude), lat = mean(CamEvents$Latitude), zoom = 16) %>%
  addPolylines(data=CamLines, color=pal(CamLines$Camera)) %>%
  addCircleMarkers(data=CamEventsSF, label=CamEventsSF$Date, labelOptions = labelOptions(noHide = FALSE), 
                   fillOpacity = 1, radius=6, color=pal(CamEventsSF$Camera), opacity=1) %>%
  addLegend("bottomright", 
            pal=pal,
            #data=CamEventsSF,
            values = CamEventsSF$Camera,
            title = "Cameras",
            opacity = 1)
data_map <- read_sf("https://raw.githubusercontent.com/R-CoderDotCom/data/main/sample_geojson.geojson")

leaflet() %>% addTiles() %>%
  addFlows(0, 0, 1, 1, flow = 20)
data("eco2mixBalance")
bal <- eco2mixBalance
leaflet() %>% addTiles() %>%
  addFlows(
    bal$lng0, bal$lat0, bal$lng1, bal$lat1,
    flow = bal$balance,
    time = bal$month,
    popupOptions = list(closeOnClick = FALSE, autoClose = FALSE)
  )
ext <- st_bbox(CamEventsSF)
names(ext) <- c("left", "bottom", "right", "top")
ext2 <- matrix(data=ext,2,2,dimnames=list(row=c("x","y"), column=c("min", "max")))
bbox <- c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652)