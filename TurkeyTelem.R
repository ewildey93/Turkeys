library(readxl)
library(stringr)
library(openxlsx)
library(amt)
library(sf)
library(ctmm)
library(dplyr)

TurkeyLocFiles <- list.files(path = "Z:/Sandhill/Turkey Project/Transmitter Files/Deployed Transmitter GPS Data Downloads", 
                             pattern = "all_locations", recursive = TRUE)[1:93]
path <- "Z:/Sandhill/Turkey Project/Transmitter Files/Deployed Transmitter GPS Data Downloads/"


TurkeyLocsList <- lapply(TurkeyLocFiles, function (x) read_excel(paste0(path,x)))
names(TurkeyLocsList) <- str_extract(string = TurkeyLocFiles, pattern = "([A-Za-z]+\\d{3})_.*_.*$", group = 1)

TurkeyLocsList <- lapply(TurkeyLocsList, function (x) {x$Time <- format(x$Time, format= "%H:%M:%S")
                                          x$DateTime <- as.POSIXct(paste(x$Date, x$Time), tz = "America/Chicago", format= "%Y-%m-%d %H:%M:%S")
                                          x <- x[,!(colnames(x) %in% c("Date & Time [GMT]","Date & Time [Local]"))]
                                          colnames(x)[5:6] <- c("Y", "X")
                                          x[grep(x = x$`Fix Status`, pattern = "Valid"),]
})
TurkeyLocsList <- TurkeyLocsList[sapply(TurkeyLocsList, function (x) nrow(x) > 40)]

TurkeySFList <- lapply(TurkeyLocsList, function (x) {x <- st_as_sf(x, coords=c("X", "Y"), crs=4326)
                                                x <- st_transform(x, 3071)
                                                x <- cbind(x, st_coordinates(x))
                                                colnames(x)[15:16] <- c("Longitude", "Latitude")
                                                x})
TurkeySFlines <- lapply(TurkeyLocsList, function (x) { x <- cbind(x, data.frame("X2"=lead(x$X),"Y2"=lead(x$Y)))
x <- x[!is.na(x$X2),]
x$geom <- sprintf("LINESTRING(%s %s, %s %s)", x$X, x$Y, x$X2, x$Y2)
x <- st_as_sf(x, wkt="geom", crs=4326)
x <- st_transform(x, 3071)
x})

TurkeyTrackList <- map(TurkeySFList, ~make_track(.x, Longitude,Latitude,.t=DateTime, id=Device.Name, crs = 3071))
TurkeyTelemList <- map(TurkeyTrackList, ~as_telemetry(.x))
TurkeyVGList <- map(TurkeyTelemList, ~variogram(.x))
TurkeyHRList <- map(TurkeyTrackList, ~hr_akde(.x, model = fit_ctmm(.x, "auto")))
TurkeyHRList2 <- map(TurkeyHRList, ~append(.x,list(ctmm:::name.ctmm(.x[["model"]]))))
table(sapply(TurkeyHRList2, function(x) x[[9]])) #all show home range behavior?


Fitamt <- fit_ctmm(TurkeyTrackList[["Orange015"]], "auto")
akde <- hr_akde(track2, model = Fit)

library(leaflet)
library(leaflet.extras2)



leaflet() %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addTiles() %>%
  addPolygons(data = st_transform(hr_isopleths(TurkeyHRList[["Orange015"]], levels=0.95), 4326)) %>%
  #addPolylines(data=st_transform(TurkeySFlines[["Orange015"]], 4326), color="black") #%>%
  #addCircleMarkers(data=st_transform(TurkeySFList[["Orange015"]], 4326), fillColor = "black", fillOpacity = 1, stroke=F, radius=3, group="points")%>%
  addTimeline(zgeo, width="95%", sliderOpts= sliderOptions(steps=2)) 
  
  
  ############################### scrap  ################################################################
  GUESS <- ctmm.guess(TurkeyTelemList[["Orange015"]],interactive=FALSE)
  Fits <- ctmm.select(TurkeyTelemList[["Orange015"]],CTMM = GUESS,verbose=TRUE,cores=2, trace=1)
  akde <- hr_akde(TurkeyTelemList[["Orange015"]], model = Fit[[1]])
  
  z <- st_transform(TurkeySFlines[["Orange015"]], 4326)
  z$start <- z$DateTime
  z$end <- lead(z$DateTime)
  z$end[is.na(z$end)] <- z$start[nrow(z)] + 60*60
  zgeo <- geojsonio::geojson_json(z,lat="X",lon="Y")
  v <- st_transform(TurkeySFList[["Orange015"]], 4326)
  v$start <- v$DateTime
  v$end <- lead(v$DateTime)
  v$end[is.na(v$end)] <- v$start[nrow(v)] + 60*60
  vgeo <- geojsonio::geojson_json(v,lat="X",lon="Y")
  
  st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, 
                TurkeySFList[["Orange015"]]$geometry, lead(TurkeySFList[["Orange015"]]$geometry), SIMPLIFY=FALSE))
  
  

  library(leaflet)
  library(leaftime)
  library(htmltools)
  
  #Build data.frame with 10 obs + 3 cols
  power <- data.frame(
    "Latitude" = c(
      33.515556, 38.060556, 47.903056, 49.71, 49.041667, 31.934167,
      54.140586, 54.140586, 48.494444, 48.494444
    ),
    "Longitude" = c(
      129.837222, -77.789444, 7.563056, 8.415278, 9.175, -82.343889,
      13.664422, 13.664422, 17.681944, 17.681944
    ),
    "start" = seq.Date(as.Date("2015-01-01"), by = "day", length.out = 10),
    "end" = seq.Date(as.Date("2015-01-01"), by = "day", length.out = 10) + 1
  )
  
  # use geojsonio to convert our data.frame
  #  to GeoJSON which timeline expects
  power_geo <- geojsonio::geojson_json(power,lat="Latitude",lon="Longitude")
  
  # we can add data in addTimeline
  leaflet() %>%
    addTiles() %>%
    setView(44.0665,23.74667,2) %>%
    addTimeline(data = power_geo)
  
  # or we can add data in leaflet()
  leaflet(power_geo) %>%
    addTiles() %>%
    setView(44.0665,23.74667,2) %>%
    addTimeline()
  
  # we can control the slider controls through sliderOptions
  leaflet(power_geo) %>%
    addTiles() %>%
    setView(44.0665,23.74667,2) %>%
    addTimeline(
      sliderOpts = sliderOptions(
        formatOutput = htmlwidgets::JS(
          "function(date) {return new Date(date).toDateString()}
      "),
        position = "bottomright",
        step = 10,
        duration = 3000,
        showTicks = FALSE
      )
    ) 
  
  # we can control the timeline through timelineOptions
  #  wondering what should be the default
  #  currently timeline uses marker
  leaflet(power_geo) %>%
    addTiles() %>%
    setView(44.0665,23.74667,2) %>%
    addTimeline(
      timelineOpts = timelineOptions(
        pointToLayer = htmlwidgets::JS(
          "
function(data, latlng) {
  return L.circleMarker(latlng, {
    radius: 3
  })
}
"
        ),
        style = NULL
      )
    )
  
  # change styling manually
  leaflet(power_geo) %>%
    addTiles() %>%
    setView(44.0665,23.74667,2) %>%
    addTimeline(
      timelineOpts = timelineOptions(
        pointToLayer = htmlwidgets::JS(
          "
function(data, latlng) {
  return L.circleMarker(latlng, {
    radius: 10,
    color: 'black',
    fillColor: 'pink',
    fillOpacity: 1
  })
}
"
        ),
        styleOptions = NULL
      )
    )
  
  # change style with styleOptions helper function
  #   this will change style for all points
  leaflet(power_geo) %>%
    addTiles() %>%
    setView(44.0665,23.74667,2) %>%
    addTimeline(
      timelineOpts = timelineOptions(
        styleOptions = styleOptions(
          radius = 10,
          color = "black",
          fillColor = "pink",
          fillOpacity = 1
        )
      )
    )
  
  # to style each point differently based on the data
  power_styled <- power
  # IE does not like alpha so strip colors of alpha hex
  power_styled$color <- substr(topo.colors(6)[ceiling(runif(nrow(power),0,6))],1,7)
  power_styled$radius <- seq_len(nrow(power_styled)) # ceiling(runif(nrow(power),3,10))
  
  leaflet(geojsonio::geojson_json(power_styled)) %>%
    addTiles() %>%
    setView(44.0665,23.74667,2) %>%
    # addCircleMarkers(
    #   data = power_styled, lat = ~Latitude, lng = ~Longitude, radius = 11
    # ) %>%
    addTimeline(
      timelineOpts = timelineOptions(
        styleOptions = NULL, # make sure default style does not override
        pointToLayer = htmlwidgets::JS(
          "
function(data, latlng) {
  return L.circleMarker(
    latlng,
    {
      radius: +data.properties.radius,
      color: data.properties.color,
      fillColor: data.properties.color,
      fillOpacity: 1
    }
  );
}
"
        )
      )
    )
  
  
  
  # we can use onchange to handle timeline change event
  leaflet(power_geo) %>%
    addTiles() %>%
    setView(44.0665,23.74667,2) %>%
    addTimeline(
      onchange = htmlwidgets::JS("function(e) {console.log(e, arguments)}")
    )
  
  
  leaflet(power_geo, elementId = "leaflet-wide-timeline") %>%
    addTiles() %>%
    setView(44.0665,23.74667,2) %>%
    addTimeline(
      width = "96%"
    )
  