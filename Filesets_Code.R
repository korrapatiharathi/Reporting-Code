# load library
library(dplyr)
library("anytime")
library("lubridate")
library(psych)
library(reshape)
library(ggplot2)
library(tidyr)
library(openxlsx)
library(gsubfn)

setwd("E:/Harathi data/HarathiWork_Automation/automation/Nodes")

filepath <- setwd("E:/Harathi data/HarathiWork_Automation/automation/Nodes")
folders <- list.dirs(path=filepath, full.names=TRUE, recursive=TRUE)
folders <- folders[-1]

for (i in 1:length(folders)) {
  setwd(folders[i])
  files <- list.files(path=folders[i], pattern="*.csv", full.names=TRUE, recursive=FALSE)
  for (i in 1:length(files)) {
    df2$newTime <- as.POSIXct(df2$time/1000000000, origin="1970-01-01")
    df2$month <- (format(as.Date(df2$newTime, tz = "CST6CDT"), "%Y-%m"))
    df2$date <- (format(as.Date(df2$newTime, tz = "CST6CDT"), "%Y-%m-%d"))
    
    filename <- sub('\\.csv$', '', basename(files[i]))
    filename <- strsplit(filename, "_")[[1]][2]
    
    df3 <- df2 
    
    if(grep("disk",  tolower(files))==TRUE) {
    maxDisk <- as.data.frame(df3 %>% group_by(date) %>%
                                 summarise(max = max(used_percent)))
    colnames(maxDisk) = c("Date", paste0("Daily Maximum Disk Used Percent (%)",sep = ': ',filename))

    write.xlsx(maxDisk, file = paste0(filename, "_Disk_Output",".xlsx",sep = ''))
    }
    
    else if (grep("mem",  tolower(files))==TRUE) {
    maxMem <- as.data.frame(df3 %>% group_by(date) %>%
                                 summarise(max = max(used_percent)))
    colnames(maxMem) = c("Date", paste0("Daily Maximum Memory Used Percent (%)",sep = ': ',filename))
    
    write.xlsx(maxMem, file = paste0(filename, "_Memory_Output",".xlsx",sep = ''))  
    
    }
    
    else (grep("cpu",  tolower(files))==TRUE) {
    maxCpu <- as.data.frame(df3 %>% group_by(date) %>%
                                summarise(max = max(load1)))
    colnames(maxCpu) = c("Date", paste0("Daily Maximum CPU load",sep = ': ',filename))
    
    write.xlsx(maxCpu, file = paste0(filename,"_CPU_Output",".xlsx",sep = ''))    
    }
           
  }
  
}











#setwd("E:/Harathi data/HarathiWork/automation/filesets_disk/")


  #names(df2) <- c("name", "time","host", "available_percent", "used_percent")
  df2$newTime <- as.POSIXct(df2$time/1000000000, origin="1970-01-01")
  df2$month <- (format(as.Date(df2$newTime, tz = "CST6CDT"), "%Y-%m"))
  df2$date <- (format(as.Date(df2$newTime, tz = "CST6CDT"), "%Y-%m-%d"))
  df2$inodes_used_percent <- (df2$inodes_used/df2$inodes_total)*100
  
  filename <- sub('\\.csv$', '', basename(files[i]))
  filename <- strsplit(filename, "_")[[1]][2]
  
  df3 <- df2 
  maxDisk <- as.data.frame(df3 %>% group_by(date) %>%
                             summarise(max = max(used_percent)))
  meanDisk <- as.data.frame(df3 %>% group_by(date) %>%
                              summarise(mean = mean(used/1099511627776)))
  maxInodes <- as.data.frame(df3 %>% group_by(date) %>%
                               summarise(maxInode = max(inodes_used_percent)))
  
  colnames(maxDisk) = c("Date", paste0("Daily Maximum Disk Used Percent (%)",sep = ': ',filename))
  colnames(meanDisk) = c("Date", paste0("Daily Mean Disk Used (TB)",sep = ': ',filename))
  colnames(maxInodes) = c("Date", paste0("Daily Maximum Inodes Used Percent (%)",sep = ': ',filename))
  
  list_of_datasets <- list("maximum Disk" = maxDisk, "Mean Disk" = meanDisk, "Maximum Inodes" = maxInodes)
  write.xlsx(list_of_datasets, file = paste0(filename, "_Output",".xlsx",sep = ''))


options(repos="https://CRAN.R-project.org")
#write.csv(maxDISK, paste(paste('Max_DISK_Used_percent', userlist[i],sep = '_'), 'csv', sep = '.'), row.names=FALSE)