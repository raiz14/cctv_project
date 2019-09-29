# ЧИСТИМ ДАТАСЕТ

library(tidyr)
library(stringr)
library(dplyr)
library(data.table)
library(geojsonsf)
library(jsonlite)
library(httr)


url <- "https://apidata.mos.ru/v1/datasets/2386/features?api_key=fdefc2d7a8433ce30de0ce3e48d6a634"
cctv <- geojson_sf(url)
cctv$Attributes <- iconv(cctv$Attributes, from = "utf-8", to = "cp1251")

cctv$Attributes[1]

a <- cctv
a <- separate(a, Attributes, sep= ":", into = c("NotUse1", "Address", "AdmArea", "global_id", "District", "ID", "OVDAddress", "NotUse", "PhoneOVD", "NotUse2"))

# Удаляем колонки
 
a <- subset(a, select = -c(DatasetId, VersionNumber, ReleaseNumber, RowId, NotUse1, NotUse, NotUse2))

# Правим все

# Адрес
a$Address <- gsub('.{11}$', '',a$Address)
a$Address <- sub('.', '', a$Address)

# Остальные
a$AdmArea <- gsub('.{13}$', '',a$AdmArea)
a$AdmArea <- sub('.', '', a$AdmArea)


a$global_id <- gsub('.{11}$', '',a$global_id)

a$District <- gsub('.{6}$', '',a$District)
a$District <- sub('.', '', a$District)

a$ID <- gsub('.{14}$', '',a$ID)
a$ID <- sub('.', '', a$ID)

a$OVDAddress <- gsub('.{12}$', '',a$OVDAddress)
a$OVDAddress <- sub('.', '', a$OVDAddress)

a$PhoneOVD <- gsub('.{14}$', '',a$PhoneOVD)
a$PhoneOVD <- sub('.', '', a$PhoneOVD)

cctv <- a

# Сохраним файл
write_sf(cctv, "cctv_2019-06-27.geojson")

# Проверим
cctv <- st_read("cctv_2019-06-27.geojson")
