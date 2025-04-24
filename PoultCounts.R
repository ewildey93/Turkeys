library(readxl)
library(data.table)
library(readr)
library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)

path <- "//central/OAS/Sandhill/Turkey Project/Datasheets/2024 Files/Turkey Histories_2024.xlsx"
sheetnames <- excel_sheets(path)
mylist <- lapply(sheetnames, read_excel, path = path, skip=1)
names(mylist) <- sheetnames

photopath <- "//central/OAS/Sandhill/Turkey Project/Photos/TrailCams2024/FinalClassificationFiles"
photofiles <- list.files(path = photopath, full.names = TRUE)
photolist <- lapply(photofiles, read_csv)
names(photolist) <- basename(photofiles)
photodf <- rbindlist(photolist, idcol=TRUE)
taggedbirds <- photodf[!is.na(photodf$IndividualIDs),]
taggedbirds2 <- taggedbirds[,c(10,4,5,6:9,11,2,3)]

TurkeyHistories <- rbindlist(mylist, idcol=TRUE)
TurkeyHistories$.id <- gsub(pattern = "[a-z]*_", replacement = "", x = TurkeyHistories$.id)
TurkeyHistories$Time <- strftime(TurkeyHistories$Time, format="%H:%M:%S")
FlushCounts <- TurkeyHistories[TurkeyHistories$Event == "Flush count",]
FlushCounts$NumPoults <- str_extract(string = FlushCounts$Action, pattern = "\\w{1,2}(?= poult)")
FlushCounts$NumHens <- ifelse(grepl(x = FlushCounts$Action, pattern = "hens") | grepl(x = FlushCounts$Action, pattern = "hen.*hen"),2,1)
FlushCounts$NumPoults[grep(pattern = "No", x = FlushCounts$NumPoults, ignore.case = TRUE)] <- 0
FlushCounts2 <- FlushCounts[, c(1:4,14,15,13,10,11)]


#for sightings on Snapshot or landowner cameras
Photos <- TurkeyHistories[TurkeyHistories$Event == "Photo",]
Snapshot <- Photos[!grep(pattern = "supplemental", x = Photos$`Additional Notes`, ignore.case = TRUE)]
#no details in comments on number or sex/age of birds 
Snapshot$NumPoults <- 0
Snapshot$NumHens <- 1
Snapshot2 <- Snapshot[, c(1:4,14,15,13,10,11)]
Snapshot2$NumMales <- 0
Snapshot2$NumUnknown <- 0
colnames(Snapshot2)[c(1, 7, 8, 9)] <- c("IndividualIDs", "Notes", "FileName", "Directory")
Snapshot2$Date <- as.Date(Snapshot2$Date, tz="America/Chicago")

#Tagged Turkeys from VM
sswids::connect_to_sswidb(db_version = 'PROD')
VMTurkey <- read.csv("./TaggedTurkeysSandhill(Sheet1).csv")
VMdetects <- sswidb::sswidb_detections_for_triggers(conn, VMTurkey$Trigger)

Q.VMdetects <- DBI::dbGetQuery(conn,"SELECT 
G83100.sswi_photo.TRIGGER_SEQ_NO,
G83100.sswi_photo.FINAL_DATE_TIME,
G83100.sswi_classification.TRIGGER_SEQ_NO,
G83100.sswi_classification.METADATA_NAME,
G83100.sswi_classification.CLASSIFICATION_AMT
  FROM
g83100.sswi_photo
INNER JOIN G83100.sswi_classification
ON G83100.sswi_photo.TRIGGER_SEQ_NO = G83100.sswi_classification.TRIGGER_SEQ_NO
WHERE G83100.sswi_photo.TRIGGER_SEQ_NO IN(35253046,
                        37584983,
                        35823997,
                        35252830,
                        35253048,
                        35252544,
                        35252680,
                        35252535,
                        37748886,
                        37748885,
                        34894383);")
Q.VMdetects <- Q.VMdetects[Q.VMdetects$METADATA_NAME != "UNKNOWN_AMT",]%>%select(-3)
VMTurkey2 <- left_join(VMTurkey, Q.VMdetects, by=join_by("Trigger" == "TRIGGER_SEQ_NO"))%>%distinct()
colnames(VMTurkey2)[c(1,4,7)] <- c("Notes", "IndividualIDs", "NumHens")
VMTurkey2$Date <- as.Date(VMTurkey2$FINAL_DATE_TIME, tz="America/Chicago")
VMTurkey2$Time <- strftime(VMTurkey2$FINAL_DATE_TIME, format="%H:%M:%S") 
VMTurkey2$Event <- "Photo"
VMTurkey2$NumPoults <- 0
VMTurkey2$NumMales <- 0
VMTurkey2$NumUnknown <- 0
VMTurkey2$FileName <- NA
VMTurkey2$Directory <- NA
VMTurkey2$Notes <- paste0("Trigger:",VMTurkey2$Notes)
VMTurkey2$Notes[VMTurkey2$IndividualIDs == "O"] <- paste(VMTurkey2$Notes[VMTurkey2$IndividualIDs == "O"], "can't tell number on tag")

VMTurkey3 <- VMTurkey2[, -c(2,3,5,6)]

colnames(taggedbirds2)
colnames(FlushCounts2)
taggedbirds2$Event <- "Photo"
taggedbirds2 <- taggedbirds2%>%mutate(across(starts_with("Num"), ~replace_na(.,replace = 0)))
taggedbirds2$Notes <- gsub(pattern = "unqiue", replacement = "unique", x = taggedbirds2$Notes)
taggedbirds2 <- taggedbirds2%>%mutate(NumHens=ifelse(grepl(pattern = "hens", x = Notes), 
                                                     str_extract(string = Notes, pattern = "\\d{1,2}(?= unique hens)|\\d{1,2}(?= hens)"),
                                                     NumHens),
                                      NumPoults=ifelse(grepl(pattern = "poults", x = Notes), 
                                                       str_extract(string = Notes, pattern = "\\d{1,2}(?= unique poults)"),
                                                       NumPoults))
taggedbirds2$Time <- as.character(taggedbirds2$Time)
taggedbirds2$Date <- as.Date(taggedbirds2$Date, "%m/%d/%Y", tz="America/Chicago")

FlushCounts2$NumMales <- 0
FlushCounts2$NumUnknown <- 0
colnames(FlushCounts2)[c(1, 7, 8, 9)] <- c("IndividualIDs", "Notes", "FileName", "Directory")
FlushCounts2$Date <- as.Date(FlushCounts2$Date, tz="America/Chicago")

sapply(list(FlushCounts2, taggedbirds2, Snapshot2, VMTurkey3), function(x) unique(x$IndividualIDs))
PoultCounts <- rbind(FlushCounts2, taggedbirds2, Snapshot2, VMTurkey3)
PoultCounts$DateTime <- as.POSIXct(paste(PoultCounts$Date, PoultCounts$Time), format= c("%Y-%m-%d %H:%M:%S"))
NADateTime <- PoultCounts[is.na(PoultCounts$DateTime),]
PoultCounts2 <- PoultCounts[,c(1,12,10,11,4:9)]%>%
  separate_longer_delim(., IndividualIDs, delim = ", ")%>%
  arrange(IndividualIDs, DateTime)

write.csv(PoultCounts2, "./PoultCounts2024.csv")
