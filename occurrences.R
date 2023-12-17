# получение точек из GBIF и их фильтрация
# 2023-12-12 
# этап 5 - поиск и подготовка точек находок для моделирования

library(rgbif)
library(sf)
library(raster)
library(ggplot2)
library(tidyverse)
library(dplyr)

setwd('/media/carabus/Enterprise/EW_SDM/')

# виды 
# Aporrectodea caliginosa     2307759 
# Lumbricus terrestris        4410657
# Lumbricus rubellus          4410669
# Dendrobaena octaerda        7772922
# Octolasion lacteum          4410647

# территория интереса
aoi = read_sf('navigation/Russia_European_albers.shp')

ggplot() +
  geom_sf(data = aoi)

# преобразуем в систему координат WGS84
aoi_dd = st_transform(aoi, st_crs('EPSG:4326'))

extent(aoi_dd) # крайние координаты области интереса

# прямоугольных полигон по крайним точкам территории интереса 
aoiExt = "POLYGON((66.25253 69.95413, 27.3196 69.95413, 27.3196 44.75997, 66.25253 44.75997, 66.25253 69.95413))"

# account = occ_count(taxonKey = 2307759, country = 'RU') # 3011
# account = occ_count(taxonKey = 2307759, geometry = aoiExt) # 2902
account = occ_count(taxonKey = 2307759, geometry = aoiExt, country = 'RU') # 2668
ltcount = occ_count(taxonKey = 4410657, geometry = aoiExt, country = 'RU') # 253
lrcount = occ_count(taxonKey = 4410669, geometry = aoiExt, country = 'RU') # 667
docount = occ_count(taxonKey = 7772922, geometry = aoiExt, country = 'RU') # 1106
olcount = occ_count(taxonKey = 4410647, geometry = aoiExt, country = 'RU') # 601

terms = c('key','datasetKey','datasetName','occurrenceStatus','eventDate','year',
          'month','decimalLatitude','decimalLongitude','coordinateUncertaintyInMeters',
          'countryCode','scientificName')

ac = occ_search(taxonKey = 2307759, geometry = aoiExt, country = 'RU', 
                fields = terms, limit = account)$data
write.csv(ac,'occurrences/acraw.csv')

lt = occ_search(taxonKey = 4410657, geometry = aoiExt, country = 'RU', 
                fields = terms, limit = account)$data
write.csv(lt,'occurrences/ltraw.csv')

lr = occ_search(taxonKey = 4410669, geometry = aoiExt, country = 'RU', 
           fields = terms, limit = account)$data
write.csv(lr,'occurrences/lrraw.csv')

do = occ_search(taxonKey = 7772922, geometry = aoiExt, country = 'RU', 
           fields = terms, limit = account)$data
write.csv(do,'occurrences/doraw.csv')

ol = occ_search(taxonKey = 4410647, geometry = aoiExt, country = 'RU', 
           fields = terms, limit = account)$data
write.csv(ol,'occurrences/olraw.csv')

table(ac$datasetKey)
# datasets
# 9ceef4b3-ecac-4f8a-9cca-b4a7953640ba literature
# f6822eb1-b570-4566-98b0-894d4213510e Zaseki
# 50c9509d-22c7-4a22-a47d-8c48425ef4a7 iNaturalist


# загружаем сохраненные таблицы - можно начинать отсюда
ac = read.csv('occurrences/acraw.csv')
lt = read.csv('occurrences/ltraw.csv')
lr = read.csv('occurrences/lrraw.csv')
do = read.csv('occurrences/doraw.csv')
ol = read.csv('occurrences/olraw.csv')

# либо используем заранее подготовленные данные
# заранее подготовленная таблица
ewOccurs = read.csv('occurrences/pointsForModelling.csv')
ac = ewOccurs[ewOccurs$scientificName == 'Aporrectodea caliginosa',]
do = ewOccurs[ewOccurs$scientificName == 'Dendrobaena octaedra',]
lr = ewOccurs[ewOccurs$scientificName == 'Lumbricus rubellus',]
lt = ewOccurs[ewOccurs$scientificName == 'Lumbricus terrestris',] 
ol = ewOccurs[ewOccurs$scientificName == 'Octolasion lacteum',]


# процедура для фильтрации точек находок
occurClean <- function(occurs, torerance = 32000) {
  
  sn = unique(occurs$scientificName)[1]
  cat(paste0('Число записей ',nrow(occurs),' для вида - ',sn,'\n'))
  
  # удаляем записи 
  # без координат
  occurs  = occurs[!is.na(occurs$decimalLatitude), ]
  occurs  = occurs[!is.na(occurs$decimalLongitude),]

  # с повторяющимися координатами - округляем до 4 знака
  occurs$decimalLatitude = round(occurs$decimalLatitude,4)
  occurs$decimalLongitude = round(occurs$decimalLongitude,4)

  # length(unique(paste0(occurs$decimalLatitude,' ',occurs$decimalLongitude)))

  uniqueCoords = !duplicated(occurs[,c('decimalLatitude','decimalLongitude')])
  occurs = occurs[uniqueCoords,]
  # nrow(occurs)
  # plot(occurs$decimalLongitude, occurs$decimalLatitude, pch = 16, cex = 0.6)

  # в пространственный слой
  occursLdd = st_as_sf(occurs, coords = c('decimalLongitude','decimalLatitude'))
  st_crs(occursLdd) = st_crs('EPSG:4326')
  
  occursLdd = st_filter(occursLdd, aoi_dd) # точки внутри полигона территории интереса
  # nrow(occursLdd) # 212
  
  aoi_albers = st_read('navigation/Russia_European_albers.shp')
  occursLalbers = st_transform(occursLdd, st_crs(aoi_albers))
 
  tempGrid = st_make_grid(aoi_albers, cellsize = torerance, square = F)
  selectGrid = st_sf(tempGrid, 'ID' = seq(length(tempGrid)))
  # если square = F будут гексагоны

  # выбираем точки по полигонам
  occursSelection = st_intersection(occursLalbers, selectGrid)

  occursInput <- occursSelection %>% group_by(ID) %>% sample_n(1)

  # переводим обратно в десятичные градусы
  occursInputDD = st_transform(occursInput, st_crs('EPSG:4326'))
  occursInputDD$latitude = round(st_coordinates(occursInputDD)[,1],4)
  occursInputDD$longitude = round(st_coordinates(occursInputDD)[,2],4)
  occursInputDD$geometry <- NULL
  occursInputDD$ID <- NULL
  occursInputDD$X <- NULL
  return(occursInputDD)
}

# st_write(occursInputDD, 'occurrences/acPoints.csv', row.names = F)

acPoints = occurClean(ac)
write.csv(acPoints, 'occurrences/acPoints.csv', row.names = F)

doPoints = occurClean(do)
write.csv(doPoints, 'occurrences/doPoints.csv', row.names = F)

ltPoints = occurClean(lt)
write.csv(ltPoints, 'occurrences/ltPoints.csv', row.names = F)

lrPoints = occurClean(lr)
write.csv(lrPoints, 'occurrences/lrPoints.csv', row.names = F)

olPoints = occurClean(ol)
write.csv(olPoints, 'occurrences/olPoints.csv', row.names = F)

# все точки из заранее подготовленного файла
nrow(ewOccurs)
wormPoints = occurClean(ewOccurs)
nrow(wormPoints)
write.csv(wormPoints, 'occurrences/wormPoints.csv', row.names = F)