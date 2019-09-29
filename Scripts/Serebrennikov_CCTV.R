---
title: "Анализ пространственного размещения камер наблюдения в публичном пространстве г. Москвы"
author: "Серебренников Дмитрий"
institute: "ИПП ЕУ СПб"
date: "26.09.2019"
---

# Список раюочих задач:
# 1. Загрузить данные и отобразить их в R 
# 3. Т.к. подсчет на всем объеме данных занимает много времени - создать выборку.
# 4. Научиться забирать из данных точки интереса в k метрах (где k=150 м.) вокруг каждой камеры.
# 5. Научиться представлять полученные данные в виде длинной плоской таблицы формата «идентификатор камеры-идентификатор точки интереса в OpenStreetMap-тип точки интереса-иные поля точки интереса-расстояние от точки интереса до камеры».

---

# При работе я больше всего ориентировался на следующие материалы:
# https://tsamsonov.github.io/r-geo-course/vector-analysis.html
# https://geocompr.robinlovelace.net/geometric-operations.html


---

setwd("C:/Literature/Police studies & Law Enforcement/CCTV-Moscow_IRL_Project/data/Off/data")
getwd()

library(sf)
library(dplyr)
library(jsonlite)
library(geojsonsf)
library(geojsonio)


# 1. Выгрузка данных
 
# Загрузим самые необходимые в работе данные 
roads <-  st_read("highway-line.shp") 
poi <-  st_read("poi-point.shp") 
rayons <-  st_read("boundary-polygon-lvl8.shp") 
stations <-  st_read("railway-station-point.shp") 
water <-  st_read("water-line.shp")
cctv <- st_read("cctv_2019-06-27.geojson")


rayons = st_transform(rayons, 3857)
poi = st_transform(poi, 3857)
roads = st_transform(roads, 3857)
stations = st_transform(stations, 3857)
water = st_transform(water,3857)
cctv = st_transform(cctv, 3857)



# Способ №1
# Через API сайта Открытых данных ПМ 

url <- "https://apidata.mos.ru/v1/datasets/2386/features?api_key=fdefc2d7a8433ce30de0ce3e48d6a634"
cctv <- geojson_sf(url)
# Перекодируем колонку, чтобы она была читаема
cctv$Attributes <- iconv(cctv$Attributes, from = "utf-8", to = "cp1251")
# Все переменные находятся в Attributes и я не понимаю, как их оттуда вытащить



---

# 2. Выборка
# https://www.r-bloggers.com/clipping-spatial-data-in-r/
# https://www.rdocumentation.org/packages/sp/versions/1.3-1/topics/SpatialPoints

# Изначально, я пытался сделать выборку через bounding box:

frame <-  st_as_sfc(st_bbox(rayons$NAME"Басманный район")) # Ограничивающий прямоугольник

plot(frame, 
     border = 'orangered')
plot(rayons %>% st_geometry(), 
     col = 'lightgrey',
     add = TRUE)
  


# В результате долгих попыток ничего не вышло.
# Тогда я решил сделать выборку по району (Бассманному):

rayons <- rayons %>% filter(OSM_ID == "2162195") %>% st_geometry() 
# Сделаем выборку всех остальных слоев по Бассманному району
poi <- poi[rayons, ]
roads <- roads[rayons, ]
stations <- stations[rayons,]
water <- water[rayons, ]
cctv <- cctv[rayons, ]

# Строим буферную зону, используя функцию st_buffer() из пакета sf. Т.к. есть проблемы с камерами, будем строить зоны вокруг станций

zone = st_buffer(cctv, dist = 150) 



#Затем преобразуйте свою координату в ирландскую сетку :
  
  data = st_transform(data, 29902)
#Создайте свой буфер в метрах вокруг этой точки:
    dub_buffer = st_buffer(data, 150)



# Сделаем отбор точек интереса по буферным зонам
selected.poi = poi[zone, ]
View(selected.poi)

# (!)
# И тут вторая большая проблема. Почему то точки интереса не отбираются. При этом на других данных из которых я взял основу выполнения этой задачи (https://tsamsonov.github.io/r-geo-course/vector-analysis.html), эта команда работала и собирала данные.
# Проблема №3 состоит в том, что когда она их собирала, то не делала сцепки между конкретной станцией и точками интереса, а выдавала просто таблицу с точками интереса, попавшими в буффер.


# На всякий случай визуализируем то, что получилось:

# Убираем большие поля на карте
par(mar = c(0,0,0,0))

# Делаем основу карты в виде функции
plotBasemap = function(){
  plot(rayons)
  plot(water %>% st_geometry(), 
       col = "lightskyblue1",
       border = "lightskyblue3",
       add = TRUE)
  plot(roads %>% st_geometry(),
       col = "gray70",
       add = TRUE)
  plot(poi %>% st_geometry(), 
       col = "deepskyblue4", 
       pch = 20, 
       cex = 0.3, 
       add = TRUE)
  plot(stations %>% st_geometry(), 
       col = "slategray4", 
       pch = 20, 
       cex = 2, 
       add = TRUE)
}


# Визуализируем то, что получилось с буфферными зонами вокруг станций
poi1 <- st_equals(poi, selected.poi)


plotBasemap()
plot(cctv %>% st_geometry(), 
     col = "red", 
     pch = 20, 
     cex = 1, 
     add = TRUE)
plot(zone %>% st_geometry(),
     col = adjustcolor("sienna3", alpha.f = 0.5),
     border = "sienna3",
     add = TRUE)


# Визуальная проверка
a <- function(){
plot(selected.poi$geometry)
plot(cctv %>% st_geometry(), 
     col = "red", 
     pch = 20, 
     cex = 1, 
     add = TRUE)
plot(zone %>% st_geometry(),
     col = adjustcolor("sienna3", alpha.f = 0.5),
     border = "sienna3",
     add = TRUE)
}



a()


library(mapview)

mapview(selected.poi, label = selected.poi$NAME, map.types = c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) + mapview(cctv, col.regions = 'red') +  mapview(zone, col.regions = "sienna3") + mapview(poi, cex = 0.1, col.regions = "deepskyblue4") + mapview(stations, col.regions = "black") + mapview(water, col.regions = "blue", border = "blue")

---

# Итого три проблемы:
# 1. Чтение набора данных с камерами
# 2. Работа функции st_buffer на используемых данных
# 3. Проблема вывода итоговой таблицы точек интереса