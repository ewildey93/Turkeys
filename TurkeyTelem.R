library(readxl)
library(stringr)
library(openxlsx)
library(amt)
library(sf)

TurkeyLocFiles <- list.files(path = "Z:/Sandhill/Turkey Project/Transmitter Files/Deployed Transmitter GPS Data Downloads", 
                             pattern = "all_locations", recursive = TRUE)
path <- "Z:/Sandhill/Turkey Project/Transmitter Files/Deployed Transmitter GPS Data Downloads/"


TurkeyLocsList <- lapply(TurkeyLocFiles, function (x) read_excel(paste0(path,x)))
names(TurkeyLocsList) <- str_extract(string = TurkeyLocFiles, pattern = "([A-Za-z]+\\d{3})_.*_.*$", group = 1)

TurkeyLocsList <- lapply(TurkeyLocsList, function (x) {x$Time <- format(x$Time, format= "%H:%M:%S")
                                          x$DateTime <- as.POSIXct(paste(x$Date, x$Time), tz = "America/Chicago", format= "%Y-%m-%d %H:%M:%S")
                                          x <- x[,!(colnames(x) %in% c("Date & Time [GMT]","Date & Time [Local]"))]
                                          x[grep(x = x$`Fix Status`, pattern = "Valid"),]
})


TurkeySFList <- lapply(TurkeyLocsList, function (x) {x <- st_as_sf(x, coords=c("Longitude", "Latitude"), crs=4326)
                                                x <- st_transform(x, 3071)
                                                x <- cbind(x, st_coordinates(x))
                                                colnames(x)[15:16] <- c("Longitude", "Latitude")
                                                x})
TurkeyTrackList <- map(TurkeySFList, ~make_track(.x, Longitude,Latitude,.t=DateTime, id=Device.Name, crs = 3071))
TurkeyHRList <- map(TurkeyTrackList[1:5], ~hr_kde(.x))

Fit <- fit_ctmm(track2, "auto")
akde <- hr_akde(track2, model = Fit)

library(leaflet)
leaflet() %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addTiles() %>%
  addPolygons(data = st_transform(hr_isopleths(TurkeyHRList[["Orange015"]], levels=0.95), 4326))# %>%
  addCircleMarkers(data=Collar4326, fillColor = "black", fillOpacity = 1, stroke=F, radius=3)