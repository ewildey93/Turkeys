library(readr)
library(lubridate)
library(data.table)
library(stringr)
library(dplyr)



#"//central/OAS/Sandhill/Turkey Project/Photos/TrailCams/FinalTaggedTurkeyPhotos"
path <- "//central/OAS/Sandhill/Turkey Project/Photos/TrailCams2024/FinalTaggedTurkeyPhotos/C138"
C138dirs <- list.dirs(path = path)
filterdirs <- data.frame(dir=C138dirs[2],
                         Camera=str_extract(string = C138dirs[2], pattern = "C\\d{3}"),
                         SDcard=str_extract(string = C138dirs[2], pattern = "(?<=_).*$"),
                         PickupDate=str_extract(string = C138dirs[2], pattern = "\\d{4}-\\d{2}-\\d{2}"))
files <- list.files(path, recursive = )



classfilespath <- "//central/OAS/Sandhill/Turkey Project/Photos/TrailCams2024/FinalClassificationFiles"
files <- list.files("//central/OAS/Sandhill/Turkey Project/Photos/TrailCams2024/FinalClassificationFiles")
ClassFiles <- lapply(files, function (x) read_csv(paste(classfilespath, x,sep="/")))
names(ClassFiles) <- files

ClassFilesDF <- rbindlist(ClassFiles, idcol = "file")



strsplit(ClassFilesDF$file, "_")
ClassFilesDF$SDcard <- sapply(strsplit(ClassFilesDF$file, "_"), function(x) x[3])
strsplit(ClassFilesDF$file, "_")[1]
ClassFilesDF$Camera <- sapply(strsplit(ClassFilesDF$file, "_"), function(x) gsub(pattern = ".csv",replacement = "",x[6]))
ClassFilesDF$PickupDate <- sapply(strsplit(ClassFilesDF$file, "_"), function(x) x[2])
C138ClassFiles <- left_join(filterdirs, ClassFilesDF, by=c("Camera", "SDcard", "PickupDate"))
NoTaggedTurkeys <- C138ClassFiles[is.na(C138ClassFiles$IndividualIDs),]
NoTaggedTurkeys$FullFileName <- paste(NoTaggedTurkeys$dir,NoTaggedTurkeys$FileName, sep="/")
saemfilename <- NoTaggedTurkeys%>%group_by(FileName)%>%filter(n() > 1)
hastags <- C138ClassFiles%>%filter(!is.na(IndividualIDs))
Allfilesfilterdirs <- list.files(filterdirs$dir, full.names = TRUE)
deletethese <- Allfilesfilterdirs[!(Allfilesfilterdirs %in% paste(hastags$dir, hastags$FileName, sep="/"))]
file.remove(NoTaggedTurkeys$FullFileName)


NewDir <- "//central/OAS/Sandhill/Turkey Project/Photos/TrailCams2024/FinalTaggedTurkeyPhotos/C138/NotTagged"
for (i in 1:nrow(NoTaggedTurkeys)){
  if(!dir.exists(NewDir)){
    dir.create(NewDir, recursive=TRUE)}
  file.rename(NoTaggedTurkeys$FullFileName, paste(NewDir,paste0(NoTaggedTurkeys$SDcard, NoTaggedTurkeys$FileName), sep="/"))
}

warnings()

TaggedTurkeys <- ClassFilesDF[!is.na(ClassFilesDF$IndividualIDs),]


TaggedTurkeys$FullPath <- paste(gsub(pattern = "\\\\","/",TaggedTurkeys$Directory), TaggedTurkeys$FileName, sep="/")
TaggedTurkeys$FullPath <- paste0("/", TaggedTurkeys$FullPath)
TaggedTurkeys$NewFilePath <- paste(gsub(pattern = "\\\\","/",TaggedTurkeys$Directory), TaggedTurkeys$FileName, sep="/")
TaggedTurkeys$NewFilepath <- paste0("/", TaggedTurkeyFiles)

VTurkeys <- TaggedTurkeys[grepl(pattern = "^V:/",x = TaggedTurkeys$Directory),]
VTurkeys$Directory <- gsub(pattern = "^V:", replacement = "//central/OAS", x = VTurkeys$Directory)
VTurkeys$FullPath <- paste(VTurkeys$Directory, VTurkeys$FileName, sep="/")

for (i in 1:nrow(VTurkeys)){
newsub <- paste(NewDir, VTurkeys$Camera[i], basename(VTurkeys$Directory)[i], sep="/")
if(!dir.exists(newsub)){
dir.create(newsub, recursive=TRUE)}
file.copy(VTurkeys$FullPath[i], newsub)
}



dirname(TaggedTurkeyFiles[1])
basename(dirname(TaggedTurkeyFiles[1]))






NewDir <- "//central/OAS/Sandhill/Turkey Project/Photos/TrailCams/FinalTaggedTurkeyPhotos"



TaggedTurkeys <- ClassFilesDF[!is.na(ClassFilesDF$IndividualIDs),]
TaggedTurkeys$FullPath <- paste(gsub(pattern = "\\\\","/",TaggedTurkeys$Directory), TaggedTurkeys$FileName, sep="/")
TaggedTurkeys$FullPath <- paste0("/", TaggedTurkeys$FullPath)
paste(NewDir, TaggedTurkeys$Camera[1], basename(TaggedTurkeys$Directory)[1], sep="/")


for (i in 1:nrow(TaggedTurkeys)){
newsub <- paste(NewDir, TaggedTurkeys$Camera[i], basename(TaggedTurkeys$Directory)[i], sep="/")
if(!dir.exists(NewDir)){
dir.create(NewDir, recursive=TRUE)}
file.copy(TaggedTurkeys$FullPath[i], newsub)
}
VTurkeys <- TaggedTurkeys[grepl(pattern = "^V:/",x = TaggedTurkeys$Directory),]
VTurkeys$Directory <- gsub(pattern = "^V:", replacement = "//central/OAS", x = VTurkeys$Directory)
VTurkeys$FullPath <- paste(VTurkeys$Directory, VTurkeys$FileName, sep="/")

for (i in 1:nrow(VTurkeys)){
newsub <- paste(NewDir, VTurkeys$Camera[i], basename(VTurkeys$Directory)[i], sep="/")
if(!dir.exists(newsub)){
dir.create(newsub, recursive=TRUE)}
file.copy(VTurkeys$FullPath[i], newsub)
}

file.rename


##########scrap#############################################
TaggedTurkeyFiles <- paste(gsub(pattern = "\\\\","/",TaggedTurkeys$Directory), TaggedTurkeys$FileName, sep="/")
TaggedTurkeyFiles <- paste0("/", TaggedTurkeyFiles)