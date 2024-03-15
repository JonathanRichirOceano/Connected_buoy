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

df$date <- as.Date(df$Datetime)
df$mois <- month(df$date, label = TRUE)

class(df$Datetime)
df <- df %>% arrange(Datetime)
table(duplicated(df$Datetime))

# àpad de mars 2023 qd la sonde est revenue d'entretien: "les paramètres Chloro-A sont calibrés en utilisant des ppb de Rhodamine WT.400ppb de rhodamine WT équivalent à 40µg/l de chlorophylle." => facteur 10 de correction.
df$Fluorescence <- df$Fluorescence/10

saveRDS(df, "ts_24/Data_2024/raw.data_2024.rds")

go1 <- melt(df[, c(2:6,8)], id.vars= "Datetime")
go1$value <- as.numeric(go1$value)

ggplot(go1, aes(Datetime, value)) +
  geom_point(size = 0.4) +
  scale_x_datetime(date_labels = "%d-%m-%y", date_breaks = "1 month") +
  xlab("") +
  ylab("") +
  facet_grid(variable~., scales = "free",  
             labeller = as_labeller(c(Temperature='Température\n (°C)',
                                      Salinite='Salinité ',
                                      Oxygene= bquote('Oxygène dissous \n (mgl)'),
                                      pH='pH ',
                                      Fluorescence= "Fluorescence \n (µgl)"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

#ggsave("ts_24/raw.data_2024.png")

#unique(df$date)
#as.integer(unique(df$date))
#for(i in 1:(length(unique(df$date))-1)) { 
#show(as.integer(unique(df$date))[i+1]-as.integer(unique(df$date))[i])
#}

df$difftime.min <- NA
for(i in 1:(length(unique(df$Datetime))-1)) { 
  df$difftime.min[i+1] <- (difftime(df$Datetime[i+1], df$Datetime[i], units="mins"))
}
table(df$difftime.min)
filter(df, difftime.min != 15)
z <- which(df$difftime.min != 15)
ind <- unique(unlist(lapply(z, function(x){
  g <- pmax(x - 1, 1)
})))
length(ind)
show(ind)
show(z)
df[c(sort(unique(c(z,ind)))),] 

# Analyse data via ploting when difftime != 15 min

buoy.plot <- function(var., ylab.) {
  df.plot <- df[, c("Datetime", var.)]
  df.point <- df[c(sort(unique(c(z,ind)))),c("Datetime", var.)]
  colnames(df.plot)[2] <- "plot.var."
  colnames(df.point)[2] <- "plot.var."
  p. <<- ggplot(df.plot, aes(Datetime, plot.var.)) +
    geom_point(size = 0.4) +
    scale_x_datetime(date_labels = "%d-%m-%y", date_breaks = "1 month") +
    xlab("Date") +
    ylab(ylab.) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) 
  p. <<- p. + geom_point(data=df.point,colour="red")
}

buoy.plot(var. = "Temperature", ylab. = "Température (°C)")
(p.temp.raw <- p.)
rm(p.)

buoy.plot(var. = "Salinite", ylab. = "Salinité")
(p.sal.raw <- p.)
rm(p.)

buoy.plot(var. = "Oxygene", ylab. = "Oxygène dissous (mg/L)")
(p.oxy.raw <- p.)
rm(p.)

buoy.plot(var. = "pH", ylab. = "pH")
(p.pH.raw <- p.)
rm(p.)

buoy.plot(var. = "Fluorescence", ylab. = "Chla (µg/L)")
(p.fluo.raw <- p.)
rm(p.)

# Apply on 2024 raw data some corrections following a process similar to that of 2022-2023 data.
# This section will have to be updated according to data sent by Kevin
# either we remove a date, i.e. all parameter data, or we just replace by NA incoherent values for some parameters 

bluelines <- data.frame(Datetime = NA)

df.raw <- df
df.clean <- df

filter(df.clean, Salinite >= 20) -> df.clean
bluelines <- bind_rows(bluelines, unique(filter(df.raw, Salinite < 20)["Datetime"]))

#filter(df.clean, pH >= 8.20) -> df.clean
#bluelines <- bind_rows(bluelines, unique(filter(df.raw, pH < 8.20)["Datetime"]))
#filter(df.clean, pH <= 8.35) -> df.clean
#bluelines <- bind_rows(bluelines, unique(filter(df.raw, pH > 8.35)["Datetime"]))
df.clean$pH <- ifelse(df.clean$pH < 8.20, NA, df.clean$pH)
df.clean$pH <- ifelse(df.clean$pH > 8.35, NA, df.clean$pH)

filter(df.clean, Fluorescence <= 40) -> df.clean
bluelines <- bind_rows(bluelines, unique(filter(df.raw, Fluorescence > 40)["Datetime"]))

# ...
# add here above any suppl. filter

(bluelines <- unique(na.omit(bluelines)))


filter(df, difftime.min > 60)
z <- which(df$difftime.min > 60)
ind <- unique(unlist(lapply(z, function(x){
  g <- pmax(x - 4, 1)
  g : (x + 3)
})))
length(ind)
show(ind)
show(z)
(df.del <- df[c(sort(unique(c(z,ind)))),]) 

library(dplyr)
df.clean <- setdiff(df.clean, df.del)
#rm(df.del)

setdiff(df.raw, df.clean)
ind <- unique(unlist(lapply(z, function(x){
  g <- pmax(x - 1, 1)
})))
df[c(sort(unique(c(z,ind)))),]$date 
bluelines <- bind_rows(bluelines, df[c(sort(unique(c(z,ind)))),]["Datetime"]) 

ts_22to23.raw <- readRDS("ts_22_23/full.raw.data.ts_Lovina-Jojo.rds")
ts_22to23.clean <- readRDS("ts_22_23/full.clean.data.ts_Lovina-Jojo.rds")

ts_full.raw <- rbind(ts_22to23.raw, df.raw[,1:10])
ts_full.clean <- rbind(ts_22to23.clean, df.clean[,1:10])

#rm(ts_22to23.raw, ts_22to23.clean)

## do all the plots now

# plot raw data

go1 <- melt(ts_full.raw[, c(2:6,8)], id.vars= "Datetime")
go1$value <- as.numeric(go1$value)

ggplot(go1, aes(Datetime, value)) +
  geom_point(size = 0.4) +
  scale_x_datetime(date_labels = "%d-%m-%y", date_breaks = "1 month") +
  xlab("") +
  ylab("") +
  facet_grid(variable~., scales = "free",  
             labeller = as_labeller(c(Temperature='Température \n (°C)',
                                      Salinite='Salinité',
                                      Oxygene= bquote('Oxygène dissous \n (mg/L)'),
                                      pH='pH',
                                      Fluorescence= "Chla \n (µg/L)"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggsave("ts_24/Figs/synthesis/ts_raw.png", width = 5, height = 7.5, dpi = 300)

# plot individual parameter raw data

buoy.plot <- function(df., var., ylab.) {
  df.plot <- df.[, c("Datetime", var.)]
  colnames(df.plot)[2] <- "plot.var."
  p. <<- ggplot(df.plot, aes(Datetime, plot.var.)) +
    geom_point(size = 0.4) +
    scale_x_datetime(date_labels = "%d-%m-%y", date_breaks = "1 month") +
    xlab("Date") +
    ylab(ylab.) +
    # Lovina 2022 script
    geom_vline(aes(xintercept = as.POSIXct("2022-01-05 19:12:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-01-13 09:33:41")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-04-14 09:33:41")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-01-20 09:33:41")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-01-28 19:33:41")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-02-17 09:23:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-04-04 16:23:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-05-20 09:23:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-07-21 10:23:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-08-12 09:33:41")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-08-29 09:23:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-10-25 10:23:00")), col = "blue", alpha = 0.4) +
    # Kevin 2022-2023 mail mer. 15-11-23 10:48  
    geom_vline(aes(xintercept = as.POSIXct("2023-03-17 11:10:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2023-06-06 12:00:00")), col = "blue", alpha = 0.4) + # hh:mm:ss inconnu
    geom_vline(aes(xintercept = as.POSIXct("2023-07-13 15:40:00")), col = "blue", alpha = 0.4) +  
    geom_vline(aes(xintercept = as.POSIXct("2023-09-01 16:24:00")), col = "blue", alpha = 0.4) + 
    geom_vline(aes(xintercept = as.POSIXct("2023-09-04 16:35:00")), col = "blue", alpha = 0.4) + 
    # Jonathan 2022-2023 deduction when missing   
    geom_vline(aes(xintercept = as.POSIXct("2022-12-13 12:00:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-12-14 12:00:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2023-01-26 12:00:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2023-05-04 12:00:00")), col = "blue", alpha = 0.4) + 
    geom_vline(aes(xintercept = as.POSIXct("2023-08-18 12:00:00")), col = "blue", alpha = 0.4) + 
    geom_vline(aes(xintercept = as.POSIXct("2023-09-05 12:00:00")), col = "blue", alpha = 0.4) + 
    geom_vline(aes(xintercept = as.POSIXct("2023-11-24 12:00:00")), col = "blue", alpha = 0.4) +
    # 2024 and more
    geom_vline(aes(xintercept = last(ts_22to23.raw$Datetime)), col = "blue", alpha = 0.4) + 
    geom_vline(aes(xintercept = first(df.raw$Datetime)), col = "blue", alpha = 0.4) + 
    geom_vline(data = bluelines, aes(xintercept = Datetime), col = "blue", alpha = 0.4) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
}

# first, year currently analysed afeter filtering for zoom purpose

buoy.plot(df. = df.raw, var. = "Temperature", ylab. = "Température (°C)")
(p.temp.raw <- p.)
rm(p.)

buoy.plot(df. = df.raw, var. = "Salinite", ylab. = "Salinité")
(p.sal.raw <- p.)
rm(p.)

buoy.plot(df. = df.raw, var. = "Oxygene", ylab. = "Oxygène dissous (mg/L)")
(p.oxy.raw <- p.)
rm(p.)

buoy.plot(df. = df.raw, var. = "pH", ylab. = "pH")
(p.pH.raw <- p.)
rm(p.)

buoy.plot(df. = df.raw, var. = "Fluorescence", ylab. = "Chla (µg/L)")
(p.fluo.raw <- p.)
rm(p.)

# second, full ts

buoy.plot(df. = ts_full.raw, var. = "Temperature", ylab. = "Température (°C)")
(p.temp.raw <- p.)
rm(p.)
ggsave("ts_24/Figs./raw/p.temp.raw.png", width = 5, height = 2.5, dpi = 300)

buoy.plot(df. = ts_full.raw, var. = "Salinite", ylab. = "Salinité")
(p.sal.raw <- p.)
rm(p.)
ggsave("ts_24/Figs./raw/p.sal.raw.png", width = 5, height = 2.5, dpi = 300)

buoy.plot(df. = ts_full.raw, var. = "Oxygene", ylab. = "Oxygène dissous (mg/L)")
(p.oxy.raw <- p.)
rm(p.)
ggsave("ts_24/Figs./raw/p.oxy.raw.png", width = 5, height = 2.5, dpi = 300)

buoy.plot(df. = ts_full.raw, var. = "pH", ylab. = "pH")
(p.pH.raw <- p.)
rm(p.)
ggsave("ts_24/Figs./raw/p.pH.raw.png", width = 5, height = 2.5, dpi = 300)

buoy.plot(df. = ts_full.raw, var. = "Fluorescence", ylab. = "Chla (µg/L)")
(p.fluo.raw <- p.)
rm(p.)
ggsave("ts_24/Figs./raw/p.fluo.raw.png", width = 5, height = 2.5, dpi = 300)

# plot clean data

go1 <- melt(ts_full.clean[, c(2:6,8)], id.vars= "Datetime")
go1$value <- as.numeric(go1$value)

ggplot(go1, aes(Datetime, value)) +
  geom_point(size = 0.4) +
  scale_x_datetime(date_labels = "%d-%m-%y", date_breaks = "1 month") +
  xlab("") +
  ylab("") +
  facet_grid(variable~., scales = "free",  
             labeller = as_labeller(c(Temperature='Température \n (°C)',
                                      Salinite='Salinité',
                                      Oxygene= bquote('Oxygène dissous \n (mg/L)'),
                                      pH='pH',
                                      Fluorescence= "Chla \n (µg/L)"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggsave("ts_24/Figs/synthesis/ts_clean.png", width = 5, height = 7.5, dpi = 300)

# plot individual parameter clean data

buoy.plot.clean <- function(df., var., ylab.) {
  df.plot <- df.[, c("Datetime", var.)]
  colnames(df.plot)[2] <- "plot.var."
  p. <<- ggplot(df.plot, aes(Datetime, plot.var.)) +
    geom_point(size = 0.4) +
    scale_x_datetime(date_labels = "%d-%m-%y", date_breaks = "1 month") +
    xlab("Date") +
    ylab(ylab.) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
}

buoy.plot.clean(df. = ts_full.clean, var. = "Temperature", ylab. = "Température (°C)")
(p.temp.clean <- p.)
rm(p.)
ggsave("ts_24/Figs/clean/p.temp.clean.png", width = 5, height = 2.5, dpi = 300)

buoy.plot.clean(df. = ts_full.clean, var. = "Salinite", ylab. = "Salinité")
(p.sal.clean <- p.)
rm(p.)
ggsave("ts_24/Figs/clean/p.sal.clean.png", width = 5, height = 2.5, dpi = 300)

buoy.plot.clean(df. = ts_full.clean, var. = "Oxygene", ylab. = "Oxygène dissous (mg/L)")
(p.oxy.clean <- p.)
rm(p.)
ggsave("ts_24/Figs/clean/p.oxy.clean.png", width = 5, height = 2.5, dpi = 300)

buoy.plot.clean(df. = ts_full.clean, var. = "pH", ylab. = "pH")
(p.pH.clean <- p.)
rm(p.)
ggsave("ts_24/Figs/clean/p.pH.clean.png", width = 5, height = 2.5, dpi = 300)

buoy.plot.clean(df. = ts_full.clean, var. = "Fluorescence", ylab. = "Chla (µg/L)")
(p.fluo.clean <- p.)
rm(p.)
ggsave("ts_24/Figs/clean/p.fluo.clean.png", width = 5, height = 2.5, dpi = 300)

# plot des raw data et des data removed

diff.ts <- setdiff(ts_full.raw,ts_full.clean)
diff.temp.ts <- setdiff(ts_full.raw[,c(2,8:10)],ts_full.clean[,c(2,8:10)])
diff.sal.ts <- setdiff(ts_full.raw[,c(3,8:10)],ts_full.clean[,c(3,8:10)])
diff.O2.ts <- setdiff(ts_full.raw[,c(4,8:10)],ts_full.clean[,c(4,8:10)])
diff.pH.ts <- setdiff(ts_full.raw[,c(5,8:10)],ts_full.clean[,c(5,8:10)])
diff.fluo.ts <- setdiff(ts_full.raw[,c(6,8:10)],ts_full.clean[,c(6,8:10)])

p <- ggplot(ts_full.raw, aes(x=Datetime, y=Temperature)) + 
  geom_point() +
  xlab("") +
  ylab("Température (°C)")
p.temp <- p + geom_point(data=diff.temp.ts,colour="red")

p <- ggplot(ts_full.raw, aes(x=Datetime, y=Salinite)) + 
  geom_point() +
  xlab("") +
  ylab("Salinité")
p.sal <- p + geom_point(data=diff.sal.ts,colour="red")

p <- ggplot(ts_full.raw, aes(x=Datetime, y=Oxygene)) + 
  geom_point() +
  xlab("") +
  ylab("Oxygène dissous (mg/L)")
p.O2 <- p + geom_point(data=diff.O2.ts,colour="red")

p <- ggplot(ts_full.raw, aes(x=Datetime, y=pH)) + 
  geom_point() +
  xlab("") +
  ylab("pH")
p.pH <- p + geom_point(data=diff.pH.ts,colour="red")

p <- ggplot(ts_full.raw, aes(x=Datetime, y=Fluorescence)) + 
  geom_point() +
  xlab("") +
  ylab("Chla (µg/L)")
p.fluo <- p + geom_point(data=diff.fluo.ts,colour="red")

rm(p)

library(gridExtra)
grid.arrange(p.temp, p.sal, p.O2, p.pH, p.fluo, ncol = 1) #arranges plots within grid
p.multiple.ts <- arrangeGrob(p.temp, p.sal, p.O2, p.pH, p.fluo, ncol = 1) #generates plot
ggsave("ts_24/Figs/synthesis/ts_raw.vs.ts_clean.png", p.multiple.ts, width = 5, height = 7.5, dpi = 300)

# plot individual parameter clean data + removed data

buoy.plot.del. <- function(df., var., ylab., ylim.) {
  df.plot <- df.[, c("Datetime", var.)]
  colnames(df.plot)[2] <- "plot.var."
  p. <<- ggplot(df.plot, aes(Datetime, plot.var.)) +
    geom_point(size = 0.4) +
    scale_x_datetime(date_labels = "%d-%m-%y", date_breaks = "1 month") +
    xlab("Date") +
    ylab(ylab.) +
    ylim(ylim.) +
    # Lovina script
    geom_vline(aes(xintercept = as.POSIXct("2022-01-05 19:12:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-01-13 09:33:41")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-04-14 09:33:41")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-01-20 09:33:41")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-01-28 19:33:41")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-02-17 09:23:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-04-04 16:23:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-05-20 09:23:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-07-21 10:23:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-08-12 09:33:41")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-08-29 09:23:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-10-25 10:23:00")), col = "blue", alpha = 0.4) +
    # Kevin mail mer. 15-11-23 10:48  
    geom_vline(aes(xintercept = as.POSIXct("2023-03-17 11:10:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2023-06-06 12:00:00")), col = "blue", alpha = 0.4) + # hh:mm:ss inconnu
    geom_vline(aes(xintercept = as.POSIXct("2023-07-13 15:40:00")), col = "blue", alpha = 0.4) +  
    geom_vline(aes(xintercept = as.POSIXct("2023-09-01 16:24:00")), col = "blue", alpha = 0.4) + 
    geom_vline(aes(xintercept = as.POSIXct("2023-09-04 16:35:00")), col = "blue", alpha = 0.4) + 
    # Jonathan deduction when missing   
    geom_vline(aes(xintercept = as.POSIXct("2022-12-13 12:00:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2022-12-14 12:00:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2023-01-26 12:00:00")), col = "blue", alpha = 0.4) +
    geom_vline(aes(xintercept = as.POSIXct("2023-05-04 12:00:00")), col = "blue", alpha = 0.4) + 
    geom_vline(aes(xintercept = as.POSIXct("2023-08-18 12:00:00")), col = "blue", alpha = 0.4) + 
    geom_vline(aes(xintercept = as.POSIXct("2023-09-05 12:00:00")), col = "blue", alpha = 0.4) + 
    geom_vline(aes(xintercept = as.POSIXct("2023-11-24 12:00:00")), col = "blue", alpha = 0.4) +
    # 2024 and more
    geom_vline(aes(xintercept = last(ts_22to23.raw$Datetime)), col = "blue", alpha = 0.4) + 
    geom_vline(aes(xintercept = first(df.raw$Datetime)), col = "blue", alpha = 0.4) + 
    geom_vline(data = bluelines, aes(xintercept = Datetime), col = "blue", alpha = 0.4) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
}


y. <- c(min(ts_full.raw$Temperature, na.rm=T), max(ts_full.raw$Temperature, na.rm=T))
buoy.plot.del.(df. = ts_full.raw, var. = "Temperature", ylab. = "Température (°C)", ylim. = y.)
names(diff.temp.ts)[names(diff.temp.ts) == 'Temperature'] <- "plot.var."
(p.temp.del. <- p. + geom_point(data=diff.temp.ts,colour="red",size = 0.8))
rm(p.,y.)
ggsave("ts_24/Figs/deleted/p.temp.del.png", width = 5, height = 2.5, dpi = 300)

y. <- c(25, max(ts_full.raw$Salinite, na.rm=T))
buoy.plot.del.(df. = ts_full.raw, var. = "Salinite", ylab. = "Salinite", ylim. = y.)
names(diff.sal.ts)[names(diff.sal.ts) == 'Salinite'] <- "plot.var."
(p.sal.del. <- p. + geom_point(data=diff.sal.ts,colour="red",size = 0.8))
rm(p.,y.)
ggsave("ts_24/Figs/deleted/p.sal.del.png", width = 5, height = 2.5, dpi = 300)

y. <- c(min(ts_full.raw$Oxygene, na.rm=T), max(ts_full.raw$Oxygene, na.rm=T))
buoy.plot.del.(df. = ts_full.raw, var. = "Oxygene", ylab. = "Oxygène dissous (mg/L)", ylim. = y.)
names(diff.O2.ts)[names(diff.O2.ts) == 'Oxygene'] <- "plot.var."
(p.oxy.del. <- p. + geom_point(data=diff.O2.ts,colour="red",size = 0.8))
rm(p.,y.)
ggsave("ts_24/Figs/deleted/p.oxy.del.png", width = 5, height = 2.5, dpi = 300)

y. <- c(5,9)
buoy.plot.del.(df. = ts_full.raw, var. = "pH", ylab. = "pH", ylim. = y.)
names(diff.pH.ts)[names(diff.pH.ts) == 'pH'] <- "plot.var."
(p.pH.del. <- p. + geom_point(data=diff.pH.ts,colour="red",size = 0.8))
rm(p.,y.)
ggsave("ts_24/Figs/deleted/p.pH.del.png", width = 5, height = 2.5, dpi = 300)

y. <- c(min(ts_full.raw$Fluorescence, na.rm=T), 10)
buoy.plot.del.(df. = ts_full.raw, var. = "Fluorescence", ylab. = "Chla (µg/L)", ylim. = y.)
names(diff.fluo.ts)[names(diff.fluo.ts) == 'Fluorescence'] <- "plot.var."
(p.fluo.del. <- p. + geom_point(data=diff.fluo.ts,colour="red",size = 0.8))
rm(p.,y.)
ggsave("ts_24/Figs/deleted/p.fluo.del.png", width = 5, height = 2.5, dpi = 300)

# boxplot

m <- melt(ts_full.clean[, c(2:6,10)], id.vars= "mois")
m <- m %>% drop_na(mois)
m$mois <- as.factor(m$mois)
m$value <- as.numeric(m$value)

unique(m$mois)
unique(m$variable)
#m$value <- ifelse(m$mois == "déc" & m$variable == "Fluorescence", NA, m$value) #no need to remove data now that we have corrected => 202303 fluo data (factor 10 cfr calib).

ggplot(m, aes(mois, value)) +
  geom_boxplot()+
  xlab("\nDate") +
  ylab("")+
  facet_grid(variable~., scales = "free",  
             labeller = as_labeller(c(Temperature='Température \n (°C)',
                                      Salinite='Salinité',
                                      Oxygene= bquote('Oxygène dissous \n (mg/L)'),
                                      pH='pH',
                                      Fluorescence= "Chla \n (µg/L)")))+
  theme_bw()


ggsave("ts_24/Figs/synthesis/boxplot_clean.data.png", width = 5, height = 7, dpi = 300)
