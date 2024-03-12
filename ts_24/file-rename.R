
list.txt <- list.files(path = "ts_24/Data_2024/Fichiers texte/", recursive = TRUE,
           pattern = "\\.txt$", 
           full.names = TRUE)
list.txt

for(i in c(1:length(list.txt))) {

#i=1
txt. <- list.txt[i]
  
read.csv(txt., header = F, sep = " ")[1:9,1]
ini <- read.csv2(txt., header = F, sep = " ")[9,1]
ini

end.date <- nrow(read.csv(txt., header = F, sep = " "))
end <- read.csv2(txt., header = F, sep = " ")[end.date,1][1]
end

ini
paste0("20",substr(ini,5,6),substr(ini,3,4),substr(ini,1,2))
ini.name <- paste0("20",substr(ini,5,6),substr(ini,3,4),substr(ini,1,2))
end
paste0("20",substr(end,5,6),substr(end,3,4),substr(end,1,2))
end.name <- paste0("20",substr(end,5,6),substr(end,3,4),substr(end,1,2))

new.txt. <- paste0("ts_24/Data_2024/Fichiers texte renamed/",ini.name,"_",end.name,"_",substr(txt.,32,nchar(txt.)))

file.copy(
  from=txt., 
  to=paste0("ts_24/Data_2024/Fichiers texte renamed/",ini.name,"_",end.name,"_",substr(txt.,32,nchar(txt.)))
  )

rm(i,txt.,ini,ini.name,end.date,end,end.name,new.txt.)

}


