###
"Тестируем код по сбору точек интереса вокруг объекта на данных центра Москвы"
# Тренировочная задача - сделать датасет с точками интереса (магазины и проч.) вокруг 300 м. от станций метро в центре Мск. Затем сделаем из этого датасет
###

setwd("C:/Literature/Police studies & Law Enforcement/CCTV-Moscow_IRL_Project/data/Off/data")

library(sf)
library(dplyr)

# Данные
roads = st_read("highway-line.shp") # Дороги
poi = st_read("poi-point.shp") # Точки интереса
rayons = st_read("boundary-polygon-lvl8.shp") # Границы районов
stations = st_read("railway-station-point.shp") # Станции метро
water = st_read("water-line.shp") # Водичка!

# Долой поля на карте!
par(mar = c(0,0,0,0))

# Данных слишком много, как анализировать часть из них? Попробуем очертить границу участка
frame = roads %>% st_bbox(c(xmin = 55.765290, xmax = 55.782734, ymax = 37.687516, ymin = 37.640145), crs = st_crs(4326)) %>% st_as_sfc() %>% st_geometry()


# Попробуем сделать выборку по району
par(mar = c(0,0,0,0))

rayons <- rayons %>% filter(OSM_ID == "2162195") %>% st_geometry() 

poi <- poi[rayons, ]
roads <- roads[rayons, ]
stations <- stations[rayons,]
water <- water[rayons, ]


plot(rayons)
plot(poi, add = T)


# Сделаем отбор точек интереса по буферным зонам
selected.poi = poi[zone, ]

  
  
  
# Замутим функцию по созданию основы карты

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

# Выберем станцию метро и построим буферную зону в 300 м.
 
kur = stations %>% filter(NAME == "Курская")
zone = st_buffer(kur, dist = 300) 

zone = st_buffer(kur, dist = 1000)

# Сделаем отбор точек интереса по буферным зонам
selected.poi = poi[zone, ]

# Нарисуем основу
plotBasemap()

# Визуализируем все

plot(stations %>% st_geometry(), 
     col = "red", 
     pch = 20, 
     cex = 4, 
     add = TRUE)

plot(zone %>% st_geometry(),
     col = adjustcolor("sienna3", alpha.f = 0.5),
     border = "sienna3",
     add = TRUE)

plot(selected.poi %>% st_geometry(), 
     col = "sienna4", 
     pch = 20, 
     cex = 0.5, 
     add = TRUE)

# Ну красота жи!
# Вот только есть одно НО - датасет. Глянем его:
 
View(selected.poi)

# Негодный R создал датасет просто из набора точек в буфферных зонах, но без привязки этих точек, к станциям метро. Толку то что мы его собрали, когда мы не можем понять, около какой станции больше магазинов и ларьков, а около какой меньше. 
 
# Отсюда задача (которую я не могу понять как решить): как сделать сбор так, чтобы в датасете также еще учитывалась станция Приходит в голову два варианта (оба из которых я не знаю как делать): 1. Сделать датсет со станциями, где будет доп-колонка, куда будут падать объекты интереса; 2. Сделать в существующем датасете колонку, в которой будет значится станция около точки интереса (но здесь есть одна проблема - что делать, когда буферы двух станций пересекаются? там будет два значения или как). Есть третий (самый страшный) путь - написать функцю, но это чет вообще...






