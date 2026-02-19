library(knitr)
library(rmarkdown)
library(stringr)
library(sf)
library(sswids)
library(readxl)

TurkeyLocFiles <- list.files(path = "Z:/Sandhill/Turkey Project/Transmitter Files/Deployed Transmitter GPS Data Downloads", 
                             pattern = "all_locations", recursive = TRUE)[1:93] #exclude marked birds without transmitter locations
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
BroodingBirds <- read.csv("Z:/Sandhill/Turkey Project/Datasheets/BroodingBirds.csv")%>%mutate(across(matches("Brood"), ~as.Date(., format="%m/%d/%Y")))

TurkeyLocsList2 <- TurkeyLocsList[gsub(x = names(TurkeyLocsList), pattern = "[a-z]+", replacement = "") %in% BroodingBirds$BirdID]


lapply(seq_along(TurkeyLocsList2),
       function(x, n, i) {
         render("TurkeyTelem.Rmd",
                output_file = paste0("MovementRMDs/", n[[i]], "Movement.html"),
                params = list(Locs = x[[i]]))
       }, x=TurkeyLocsList2 , n=names(TurkeyLocsList2))



###################################################scrap paper/ testing (IGNORE)##########################################################
# for (i in 1:length(TrackFiles)){
#   for (j in 1:length(TrackFiles[[i]])){
#     render("Rmarkdown_CameraBatchPhotoDetails.Rmd",
#            output_file = paste0("reports/CameraBatchPhotoDetails-", x, ".html"),
#            params = list(camera_seq_no = as.character(x)))
#   }
#     
# }
t <- lapply(TrackFilesYear, function(x) params = list(filename = x))


# V3 cameras in testing.
camera_seq_no <- c(56147,
                   64466)

lapply(camera_seq_no,
       function(x) {
         render("Rmarkdown_CameraBatchPhotoDetails.Rmd",
                output_file = paste0("reports/CameraBatchPhotoDetails-", x, ".html"),
                params = list(camera_seq_no = as.character(x)))
       })
