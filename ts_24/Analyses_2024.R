library(dplyr)
library(vroom)
library(readr)
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)
library(tidyr)
library(readxl)
library(gridExtra)

# Read all the files and create a FileName column to store filenames

list_of_files <- list.files(path = "ts_24/Data_2024/Fichiers texte renamed/", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)

df1 <- vroom(list_of_files, skip = 8, col_names = NULL)
df1 <- as.data.frame(df1)

df1$Datetime <- as.POSIXct(df1$X1,format="%d%m%y%H%M%S")

colnames(df1) <- c("del", "Pression", "Temperature", "Salinite", "Oxygene", "pH", "Fluorescence", "Tension","del","Datetime")
df <- df1[, -c(1,9)]
