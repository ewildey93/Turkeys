library(dplyr)
library(tidyr)
library(stringr)
library(hms)
library(readxl)


#read in Deployment Data csv
DeploymentData <- read.csv("Z:/Sandhill/Turkey Project/Photos/TrailCams2025/CsvExportedFiles/ExportedDataDDBDate10-22/Deployment.csv")

#read in Image Data csv
ImageData <- read.csv("Z:/Sandhill/Turkey Project/Photos/TrailCams2025/CsvExportedFiles/ExportedDataDDBDate10-22/ImageData.csv")
#filter out image data with no tagged turkeys
ImageData2 <- ImageData%>%filter(TagID != "")

#get path for telemetry files
path <- "Z:/Sandhill/Turkey Project/Transmitter Files/Deployed Transmitter GPS Data Downloads"
#list all_location.xlsx filenames for each bird pulled from subfolders
TurkeyLocFiles <- list.files(path = path, 
                             pattern = "all_locations", recursive = TRUE)
#make data frame of all_location.xlsx filesnames, and turkey ID extracted from filenames, then arrange alphabeticcaly by ID
TelemTurkeys <- data.frame("fp"=TurkeyLocFiles, "ID"=str_extract(string = TurkeyLocFiles, pattern = "([A-Za-z]+\\d{3})_.*_.*$", group = 1))%>%
  arrange(ID)
#Shorten turkey ID removing lower case letters to get B123 format
TelemTurkeys$ID <- gsub(pattern = "[a-z]*", replacement = "", x = TelemTurkeys$ID)
#add column extracting folders contain telemetry all_location.xlsx from file path, this will be important for writing .csvs of camera locations
TelemTurkeys$dir <- paste(path,paste0(dirname(TelemTurkeys$fp), "/"), sep="/")


#left_join keeps all the records in X, add deployment data(lat/lon) to photo data
Image.DeploymentData <- left_join(x = ImageData2, y = DeploymentData, by = "Deployment")

#separate rows (photos) with multiple tagged birds into separate lines
Image.DeploymentData2 <-separate_longer_delim(Image.DeploymentData, TagID, delim = ", ") #14725
#filter out uncertain turkey tags
Image.DeploymentData2 <- Image.DeploymentData2[grep(x = Image.DeploymentData2$TagID, pattern = "^[A-Z]{1}\\d{3}$"),]
#make a vector of what columns to keep
colstokeep <- c("DateTime", "TagID", "Latitude", "Longitude")
#shorten data frame to just datetime, tagid, and lat/lon
Image.DeploymentData3 <- Image.DeploymentData2[,colstokeep]
Image.DeploymentData3$Date <- str_extract(string = Image.DeploymentData3$DateTime, pattern = "\\d{4}-\\d{2}-\\d{2}")
Image.DeploymentData3$Time <- str_extract(string = Image.DeploymentData3$DateTime, pattern = "\\d{2}:\\d{2}:\\d{2}")
Image.DeploymentData3 <- Image.DeploymentData3[,-grep(pattern = "DateTime", x = colnames(Image.DeploymentData3))]
#ass column to telem turkeys data frame to see which turkeys with telemetry have been tagged in photos
TelemTurkeys$HasTelemCSV <- TelemTurkeys$ID %in% unique(Image.DeploymentData3$TagID)


#split dataframe into different IDs
Image.DeploymentDataList <- split(Image.DeploymentData3, f = Image.DeploymentData2$TagID)
#shorten list of tagged photos to just those Turkey IDs that have a telemetry spreadsheet
Image.DeploymentDataList2 <- Image.DeploymentDataList[names(Image.DeploymentDataList) %in% TelemTurkeys$ID]
not <- Image.DeploymentDataList[!(names(Image.DeploymentDataList) %in% TelemTurkeys$ID)]
#get file path for all_locations.xlsx and match to photos of tag birds for export
filepath <- TelemTurkeys$dir[which(TelemTurkeys$ID %in% names(Image.DeploymentDataList2))]
#make filenames (including full path) for .csvs we will export for each tagged birds photo locations
filenames <- paste0(filepath,names(Image.DeploymentDataList2), "CameraLocs.csv")
#export .csvs of each tagged birds photo locations
mapply(function (x,y) write.csv(x,y),x=Image.DeploymentDataList2, y=filenames)




aftercomma <- as.data.frame(str_extract_all(string = Image.DeploymentData$TagID, pattern = ", [A-Z]{1}\\d{3}", simplify=TRUE))
aftercomma2 <- aftercomma[aftercomma$V1 != "",]
gsub(x = unique(c(aftercomma2$V1, aftercomma2$V2)), pattern = ", ", replacement = "")


unique(Image.DeploymentData$TagID[grep(x = Image.DeploymentData$TagID, pattern = ",")])

O016 <- rbind(Image.DeploymentData[grep(x = Image.DeploymentData$TagID, pattern = "O016"),],
              Image.DeploymentData[grep(x = Image.DeploymentData$Notes, pattern = "O016"),])
O016Notes <- Image.DeploymentData[grep(x = Image.DeploymentData$Notes, pattern = "O016"),]
