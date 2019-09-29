###
"tsamsonov.github"
"https://tsamsonov.github.io/r-geo-course"
###


library("sf")

dir()
boundary <- st_read("boundary-polygon-lvl8.shp")
airlways_p <- st_read("aerialway-point.shp")
bulding_pnt <- st_read("building-point.shp")
bulding_poly <- st_read("building-polygon.shp")
highway <- st_read("highway-line.shp")
land <- st_read("land.shp")
landuse <- st_read("landuse-polygon.shp")
poi_point <- st_read("poi-point.shp")
power_l <- st_read("power-line.shp")
railway_l <- st_read("railway-line.shp")
settlement_poly <- st_read("settlement-polygon.shp")
vegetation_poly <- st_read("vegetation-polygon.shp")
water_l <- st_read("water-line.shp")
public_transport-l <- st_read("public-transport-line.shp")
surface_poly <- st_read("surface-polygon.shp")



###

# Из файла

boundary <- st_read("boundary-polygon-lvl8.shp")



#Видно, что геометрия пространственных объектов хранится в заключительном столбце с названием geometry. Данный столбец можно быстро извлечь, применив функцию st_geometry(). Полученный объект будет иметь тип sfc (Simple Feature Geometry Column)
geom_bound = st_geometry(boundary)
class(geom_bound)
## [1] "sfc_MULTIPOLYGON" "sfc"
head(geom_bound)
class(geom_bound[[3]])
## [1] "XY"           "MULTIPOLYGON" "sfg"
# Исходя из полученной информации можно сделать вывод, что геометрия 3-го объекта таблицы countries имеет класс sfg, реализованный в виде мультиполигонов (MULTIPOLYGON) с плоскими координатами (XY)

#Рисуем карты
#Если задача стоит нарисовать границы объектов, то нужно отображать объект sfc:
plot(geom_bound, col = 'grey') 

# Для быстрого построения тематических карт по выбранному показателю необходимо при вызове функции plot() указать соответствующий атрибут фрейма данных:
# Строим по административным округам
# Узнаем какая колонка за это отвечает
head(boundary)
# Узнали - ADMIN_L5. Теперь рисуем
plot(boundary['ADMIN_L5'], key.pos = NULL) # Здесь без легенды

# Для отображения координатной сетки надо указать параметр graticule = TRUE, а подписей координат — axes = TRUE:
plot(boundary['ADMIN_L5'], graticule = TRUE, axes = TRUE)
# КРАСОТА!


###
# Совмещение слоев!

# Для совмещения нескольких слоев на одной карте необходимо при втором и последующих вызовах функции plot() указать параметр add = TRUE:
  
library("sf")
dir()
boundary <- st_read("boundary-polygon-lvl8.shp")
airlways <- st_read("aerialway-point.shp")
bulding_pnt <- st_read("building-point.shp")
bulding_poly <- st_read("building-polygon.shp")
highway <- st_read("highway-line.shp")

# Строим графики

plot(boundary %>% st_geometry, lwd = 0.5, border = 'gray')
plot(airlways %>% st_geometry, col = 'steelblue1', border = 'steelblue', add = TRUE)
plot(bulding_pnt %>% st_geometry, col = 'blue', border = 'blue', add = TRUE)
plot(bulding_poly %>% st_geometry, col = 'green', border = 'green', add = TRUE)

plot(cctv %>% st_geometry, col = 'steelblue1', border = 'steelblue', add = TRUE)


# Разбираемся с функцией плот в сф
nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), quiet = TRUE)
plot(nc["SID74"])
# plot multiple:
plot(nc[c("SID74", "SID79")]) # better use ggplot2::geom_sf to facet and get a single legend!
# adding to a plot of an sf object only works when using reset=FALSE in the first plot:
plot(nc["SID74"], reset = FALSE)
plot(st_centroid(st_geometry(nc)), add = TRUE)
# log10 z-scale:
plot(nc["SID74"], logz = TRUE, breaks = c(0,.5,1,1.5,2), at = c(0,.5,1,1.5,2))
# and we need to reset the plotting device after that, e.g. by
layout(1)
# when plotting only geometries, the reset=FALSE is not needed:
plot(st_geometry(nc))
plot(st_geometry(nc)[1], col = 'red', add = TRUE)
# add a custom legend to an arbitray plot:
layout(matrix(1:2, ncol = 2), widths = c(1, lcm(2)), add = TRUE)
###


# Системы координат и проекции
# Узнаем о координатной системе того или иного слоя
st_crs(cctv)
st_crs(boundary)
st_crs(airlways)
# Все одинаково - что хорошо)
# Эта же функция позволяет создать новую координатную систему, путем передачи ей кода EPSG или строки PROJ.4:
st_crs(3857) # Проекция Меркатора для карт мира
st_crs(54030) # Проекция Робинсона для карт мира
# Проекция UTM, зона 37.
st_crs('+proj=utm +zone=37 +datum=WGS84 +units=m')
#

# А если координатные системы не одинаковые?
#Замена координатной системы требуется в тех случаях, когда слой не имеет пространственной привязки, или же она задана некорректно. В этом случае необходимо вызвать для слоя функцию st_crs() и перезаписать результат.
st_crs(countries) = NA
## Меняем
st_crs(countries) = st_crs(4326)
st_crs(countries)
# НО
#  замена координатной системы не осуществляет перепроецирования данных и не меняет координаты точек. Она лишь влияет на то, как эти координаты будут интерпретироваться

# А если нужна проекция?
# Для трансформирования данных в другую проекцию следует использовать функцию st_tranform(x, crs)
countries.merc = st_transform(countries, 3857)
plot(st_geometry(countries.merc), 
     col = 'lightgray',
     lwd = 0.5,
     graticule = TRUE, 
     axes = TRUE)
# Или...
# Проекция Робинсона (используем dplyr)
countries.rob = countries %>% st_transform(54030)
plot(st_geometry(countries.rob), 
     col = 'lightgray',
     lwd = 0.5,
     graticule = TRUE, 
     axes = TRUE)
#
# Или...
# Зарубежная Европа в Конической равнопромежуточной проекции. 
# Задаем только необходимые параметры проекции
europe.conic = countries %>% 
  dplyr::filter(continent == 'Europe' & sovereignt != 'Russia') %>% 
  st_transform('+proj=eqdc +lon_0=10 +lat_1=30 +lat_2=60 +datum=WGS84 +units=m')
plot(st_geometry(europe.conic), 
     col = 'lightgray',
     lwd = 0.5,
     graticule = TRUE, 
     axes = TRUE)
#

##################

# АТРИБУТИВНЫЕ ОПЕРАЦИИ

library(dplyr)
library(sf)
ex1 = boundary %>% filter(ADMIN_L5 == 'Новомосковский административный округ')
plot(st_geometry(ex1))

# Следующий пример иллюстрирует как выбрать населенные пункты больше 5000 человек в подмосковье и Мск. Функции dplyr определены для объектов sf таким образом, чтобы всегда сохранять геометрический столбец (!):
more5k <-  settlement_poly %>% select(POPULATION) %>% filter(POPULATION > 5000)
plot(boundary %>% st_geometry, lwd = 0.5, border = 'gray')
plot(more5k,  col = 'green', border = 'green', add = TRUE)
# ???

# Агрегирование объектов по атрибутам. Мы берем и объединям по административным округам и раскрашиваем по среднему коду ОСМ (просто потому что больше не по чему красить)
AO = boundary %>% 
  group_by(ADMIN_L5) %>% 
  summarise(osm_id = sum(OSM_ID))
plot(AO['osm_id'])
# Красота!




#######################

# ПРОСТРАНСТВЕННЫЕ ОПЕРАЦИИ в SF

st_intersects(x, y)	#x имеет общие точки с y
st_disjoint(x, y)	#x не имеет общих точек с y
st_touches(x, y)	#x касается y (граница x имеет общие точки с границей y И внутренняя область x не имеет имеет общих точек с внутренней областью y)
st_crosses(x, y)	#x пересекает y (граница x имеет общие точки с границей y, при этом размерность их пересечения меньше размерности хотя бы одного из исходных объектов)
st_within(x, y)	#x внутри y (все точки x содержатся в y И внутренняя область x имеет общие точки с внутренней областью y)
st_contains(x, y)	#x содержит y (все точки y содержатся в x И внутренняя область y имеет общие точки с внутренней областью x)
st_contains_properly(x, y)	#x содержит y полностью (все точки y содержатся в x И граница x не имеет общих точек с границей y)
st_overlaps(x, y)	#x перекрывает y (внутренняя область x имеет как общие, так и не общие точки с внутренней областью y)
st_equals(x, y)	#x совпадает y (множества точек x и y совпадают)
st_covers(x, y)	#x покрывает y (все точки y содержатся в x)
st_covered_by(x, y)	#x покрыт y (все точки x содержатся в y)
st_equals_exact(x, y)	#x совпадает y точно (упорядоченные множества точек x и y совпадают)

# ВАЖНО!
# Между covered_by и within, а также covers и contains нет разницы в случае, когда оба объекта являются площадными. Эта разница будет сказываться если хотя бы один из объектов является линией либо точкой. В этом случае within, contains и contains_properly будут давать ложный результат (FALSE), поскольку ни у линий, ни у точек нет внутренней области.

# Наиболее простой способ выбрать объекты по пространственному местоположению — это использовать один слой в качестве фильтра для другого слоя. В этом случае будет по умолчанию использовано отношение st_intersects() (пересекает). Никаких отличий от работы с обычными таблицами нет.

 
"Например, вот так можно выбрать точки, находящиеся внутри ранее отобранных стран с максимальным ВВП:"
  ...
  
  
boundary <- st_read("boundary-polygon-lvl8.shp")
airlways_p <- st_read("aerialway-point.shp")
airlports <- st_read("airport-polygon.shp")
settl_poly <- st_read("settlement-point.shp")



plot(airlways_l)

airlways <-  st_geometry(airlways_p) #?
# Наносим исходную конфигурацию
plot(boundary["ADMIN_L1"], lwd = 0.5)
plot(airlways_p["AERIALWAY"], col = 'red', pch = 20, cin = 5905.51, add = TRUE) ### cin = 5905.51 - ЭТО 150 МЕТРОВ ВОКРУГ
# Отбираем точки внутри стран с максимальным ВВП
sel = cities[largest, ]
# Смотрим что получилось
plot(outlines, lwd = 0.5)
plot(largest, col = 'gray', add = TRUE)
plot(sel, pch = 20, col = 'black', add = TRUE)

dir()

head(settlement_poly)

summary(settlement_poly$PLACE)



plot(airlways_p["AERIALWAY"], col = 'red', pch = 16, cex = 5905.51) 
### cin = 5905.51 - ЭТО 150 .... но не работает... мб через par... Но вообще ниже - ответ.



# Для интерактивных данных библиотека mapview
library(mapview)
mapview(boundary, zcol = "NAME")
mapview(boundary_poly, zcol = "NAME") + mapview(cctv,  cin = 5905.51, col.regions = 'red')
# ВАУ!!!





###
"Анализ абсолютных зон окружения"
###

#Задача данного раздела модуля звучит следующим образом: определить, какие пункты питания находятся в радиусе 300 метров от метро “Кропоткинская”. Контекстом анализа в данном случае служит 300-метровая зона окружения станции метро. Поставленную задачу можно решить так - Построить буферную зону радиусом 300 метров и выбрать ею точки, используя топологическое отношение пересечения
  
# Строим буферную зону, используя функцию st_buffer() из пакета sf.
# Выбрать полученной зоной точки пунктов питания, используя стандартный оператор [].
# Визуализировать на карте полученные точки и буферную зону.
# Определим функцию plotBasemap(), которая будет рисовать объекты картографической основы, ее мы будем использовать далее неоднократно.


# Подготовим пространство - сделаем баундин бокс - ???
# xlim = c(-55.70467655, 37.40615845), ylim = c(55.77734539, 37.55012513)


###

# Рисуем ??

# Уберем поля, чтобы карта занимала весь экран
par(mar = c(0,0,0,0))

sample1 <- boundary_poly %>% filter(oktmo == "45375000")
frame <-  sample1 %>% st_bbox() %>% st_as_sfc() %>% st_geometry()


# Визуализируем входные данные
plot(frame)





library(dplyr)
library(sf)
# Функция отвечает за рисование базовой карты
plotBasemap = function(){
  
  plot(sample1)
 
  plot(boundary_poly %>% st_geometry(), 
       col = "white",
       border = "black",
       add = TRUE)
  
  plot(cctv %>% st_geometry(),
       col = sf.colors(10),
       add = TRUE)
  
  plot(highway_l %>% st_geometry(), 
       col = "grey",
       border = "grey",
       add = TRUE)
  
  plot(poi_point %>% st_geometry(), 
       col = "deepskyblue4", 
       pch = 20, 
       cex = 0.3, 
       add = TRUE)

  plot(water_l %>% st_geometry(), 
       col = "lightskyblue1",
       border = "lightskyblue3",
       add = TRUE)  
  
  plot(railway_station %>% st_geometry(), 
       col = "slategray4", 
       pch = 20, 
       cex = 2, 
       add = TRUE)
  
  plot(building_p %>% st_geometry(), 
       col = "red", 
       pch = 20, 
       cex = 2, 
       add = TRUE)

  #text(stations %>% st_centroid() %>% st_coordinates(),
       #labels = "M",
       #col = "white",
       #cex = 0.4)
}

# УРЯЯЯЯЯЯЯЯЯЯЯЯЯ




# Выберем станцию метро и построим буферную зону
krop = stations %>% filter(Datasetid == "2386")
zone = st_buffer(krop, dist = 300)

# Применим разработанную функцию для отбора точек
selected.poi = poi.food[zone, ]


# Выберем камеры и построим буферную зону
basman_cctv <-  cctv[sample1, ]


obj1 = airlways_p %>% filter(OSM_ID == "1045233835")
zone_cam <-  st_buffer(obj1, dist = 500)
selected.poi = poi_point[zone_cam, ]
plot(boundary_poly["ADMIN_L5"])
plot(selected.poi %>% st_geometry(), 
     col = "firebrick1", 
     pch = 20, 
     cex = 0.5, 
     add = TRUE)





zone_cam <-  st_buffer(airlways_p, dist = 0.00015)
plot(zone_cam)

# Применим разработанную функцию для отбора точек
poi_near_cctv <-  zone_cam[poi_point, ]

# Применим разработанную функцию для рисования картографической основы
plotBasemap()

# Визуализируем результаты анализа

 

plot(selected.poi %>% st_geometry(), 
     col = "sienna4", 
     pch = 20, 
     cex = 0.5, 
     add = TRUE)


# Найденные объекты в табличном представлении:
View(selected.poi)













