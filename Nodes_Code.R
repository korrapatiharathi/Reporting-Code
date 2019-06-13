# load libraries
library(dplyr)
library("anytime")
library("lubridate")
library(psych)
library(reshape)
library(ggplot2)
library(tidyr)
library(openxlsx)
library(gsubfn)

# Set file path
(WD <- getwd())
if (!is.null(WD)) setwd(WD)
filepath <- WD

# Get list of all subfolders 
folders <- list.dirs(path=filepath, full.names=TRUE, recursive=TRUE)
# Remove the parent folder from list
folders <- folders[-1]
# Remove output folders if they exist
inputFolders <- Filter(function(x) !any(grepl("output", x,ignore.case=TRUE)), folders)

# Loop through every folder to extract the csv files
for (i in 1:length(inputFolders)) {
  setwd(inputFolders[i])
  files <- list.files(path=inputFolders[i], pattern="*.csv", full.names=TRUE, recursive=FALSE)
  # Loop through the files to perform appropriate analysis
   for (i in 1:length(files)) {
    # # Input the csv file in every iteration
    df2 <- read.csv(files[i], sep = ",", header = TRUE)
    # Convert time to human readable format
    df2$newTime <- as.POSIXct(df2$time/1000000000, origin="1970-01-01")
    df2$month <- (format(as.Date(df2$newTime, tz = "CST6CDT"), "%Y-%m"))
    df2$date <- (format(as.Date(df2$newTime, tz = "CST6CDT"), "%Y-%m-%d"))
    df2$host <- toupper(df2$host)
    # Extract filename for naming the outputs
    fileTitle <- sub('\\.csv$', '', basename(files[i]))
    filename <- strsplit(fileTitle, "_")[[1]][2]
    filetypeName <- strsplit(fileTitle, "_")[[1]][1]
    
    df3 <- df2 
    # Check if the file type is disk or not; if yes, calculate maximum disk used percent per day
    if("disk" == tolower(filetypeName)) {
    maxDisk <- as.data.frame(df3 %>% group_by(date, host) %>%
                                 summarise(max = max(used_percent)))
    maxDisk <- (as.data.frame(spread(maxDisk, key = host, value = max)))
    # Create a new folder for outputs
    diskFolder <- paste0(WD, "/OUTPUTS/disk_Output/")
    dir.create(diskFolder, showWarnings = FALSE, recursive = TRUE) 
    # Write the outputs to excel file
    write.xlsx(maxDisk, file = paste0(diskFolder, filename, "", "_Disk_Output",".xlsx",sep = ''))
    }
    # Check if the file type is Memoery or not; if yes, calculate maximum memory used percent per day
    else if ("mem" == tolower(filetypeName)) {
    maxMem <- as.data.frame(df3 %>% group_by(date, host) %>%
                                 summarise(max = max(used_percent)))
    
    maxMem <- (as.data.frame(spread(maxMem, key = host, value = max)))
    # Create a new folder for outputs
    memFolder <- paste0(WD, "/OUTPUTS/Memory_Output/")
    dir.create(memFolder, showWarnings = FALSE, recursive = TRUE) 
    # Write the outputs to excel file
    write.xlsx(maxMem, file = paste0(memFolder, filename, "_Memory_Output",".xlsx",sep = ''))  
    }
    # Check if the file type is cpu or not; if yes, calculate maximum cpu load per day
    else if ("cpu" == tolower(filetypeName)) {
      maxCpu <- as.data.frame(df3 %>% group_by(date, host) %>%
                                summarise(max = max(load1)))
      maxCpu <- (as.data.frame(spread(maxCpu, key = host, value = max)))
      # Create a new folder for outputs
      cpuFolder <- paste0(WD, "/OUTPUTS/Cpu_Output/")
      dir.create(cpuFolder, showWarnings = FALSE, recursive = TRUE) 
      list_of_datasets <- list("maximum Disk" = maxCpu)
      # Write the outputs to excel file
      write.xlsx(maxCpu, file = paste0(cpuFolder, filename, "_CPU_Output",".xlsx",sep = ''))  
      
    }

    else {
      print("Error-No file exists")
    }
           
  }
  
}








#options(repos="https://CRAN.R-project.org")