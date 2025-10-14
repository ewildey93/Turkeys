library(dplyr)
library(tidyr)

DeploymentData <- read.csv("Z:/Sandhill/Turkey Project/Photos/TrailCams2025/CsvExportedFiles/ExportedDataDDBDate10-14/Deployment.csv")
ImageData <- read.csv("Z:/Sandhill/Turkey Project/Photos/TrailCams2025/CsvExportedFiles/ExportedDataDDBDate10-14/ImageData.csv")
ImageData2 <- ImageData%>%filter(TagID != "")
ImageData2[grep(x = ImageData2, pattern = "^[A-Z]{1}\\d{3}$")]

TurkeyLocFiles <- list.files(path = "Z:/Sandhill/Turkey Project/Transmitter Files/Deployed Transmitter GPS Data Downloads", 
                             pattern = "all_locations", recursive = TRUE)
TelemTurkeys <- str_extract(string = TurkeyLocFiles, pattern = "([A-Za-z]+\\d{3})_.*_.*$", group = 1)
TelemTurkeys <- gsub(pattern = "[a-z]*", replacement = "", x = TelemTurkeys)

#left_join keeps all the records in X
Image.DeploymentData <- left_join(x = ImageData2, y = DeploymentData, by = "Deployment")

#separate rows with multiple tagged birds into separate lines
Image.DeploymentData2 <-separate_longer_delim(Image.DeploymentData, TagID, delim = ",") #14725
Image.DeploymentData2 <- Image.DeploymentData2[grep(x = Image.DeploymentData2$TagID, pattern = "^[A-Z]{1}\\d{3}$"),]
colstokeep <- c("DateTime", "TagID", "Latitude", "Longitude")
Image.DeploymentData3 <- Image.DeploymentData2[,colstokeep]
CompareTurkeyIDs <- data.frame("ID"=unique(Image.DeploymentData3$TagID), "HasTelemCSV"=unique(Image.DeploymentData3$TagID) %in% TelemTurkeys)

#split dataframe into different IDs
Image.DeploymentDataList <- split(Image.DeploymentData3, f = Image.DeploymentData2$TagID)
filepath <- "Z:/Sandhill/Turkey Project/Transmitter Files/Deployed Transmitter GPS Data Downloads/"
filenames <- paste0(filepath,names(Image.DeploymentDataList), "CameraLocs.csv")
mapply(function (x,y) write.csv(x,y),x=Image.DeploymentDataList, y=filenames)
