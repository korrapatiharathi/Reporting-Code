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
# Extract csv files for analysis
file <- list.files(path=filepath, pattern="*.csv", full.names=TRUE, recursive=FALSE)

# Read new file
df2 <- read.csv(file[1], sep = ",", header = TRUE)

mylist <- split(df2, df2$fileset)

names(mylist)
list <- c("datasets","dedicated","home","project","scratch","software")

names(mylist[list[1]]$datasets)

names(mylist[[list[1]]])

mylist[[list[1]]]


as.POSIXct(mylist[[list[1]]]$time/1000000000, origin="1970-01-01")

# Loop through each file
for (i in 1:length(list)) {
  # Convert time to human readable format
  df3 <- mylist[[list[i]]]
  df3$newTime <- as.POSIXct(df3$time/1000000000, origin="1970-01-01")
  df3$month <- (format(as.Date(df3$newTime, tz = "CST6CDT"), "%Y-%m"))
  df3$date <- (format(as.Date(df3$newTime, tz = "CST6CDT"), "%Y-%m-%d"))
  df3$blockused <- df3$blockused/1000000000
  
    # Calculate Maximum disk used percent daily
    maxDisk <- as.data.frame(df3 %>% group_by(date) %>%
                      summarise(max = max(blockused)))
    # Calculate Mean disk used percent daily
    meanDisk <- as.data.frame(df3 %>% group_by(date) %>%
                                summarise(mean = mean(blockused)))
    
    # Name the columns of the new output datasets
    colnames(maxDisk) = c("Date", paste0("Daily Maximum Disk Used Percent (TB)",sep = ': ',list[i]))
    colnames(meanDisk) = c("Date", paste0("Daily Mean Disk Used (TB)",sep = ': ',list[i]))
    
    # Name each sheet of the excel for identification
    list_of_datasets <- list("maximum Disk" = maxDisk, "Mean Disk" = meanDisk)
    # Write the outputs to excel  (2 sheets in one workbook)
    write.xlsx(list_of_datasets, file = paste0(list[i], "_Output",".xlsx",sep = ''))
}





options(repos="https://CRAN.R-project.org")
#write.csv(maxDISK, paste(paste('Max_DISK_Used_percent', userlist[i],sep = '_'), 'csv', sep = '.'), row.names=FALSE)