# подготовка предикторов
# 2023-12-12
# этап 6 работа с предикторами

library(geodata)
library(raster)
# install.packages('corrplot')
library(corrplot)
# install.packages('lwgeom')
library(lwgeom)
library(sf)
library(ggplot2)


# рабочая директория
setwd('/media/carabus/Enterprise/EW_SDM/')

# точки по всем видам подготовленные для моделирования
occurs = read.csv('occurrences/pointsForModelling.csv')

# worldClim разрешение 10 (18.5 км), 5 (9.25 км), 2.5 (4.625 км), 0.5 (0.925 км)

# скачиваем слои WorldClim
wClim = worldclim_country(var = 'bio', country = 'RU', res = 2.5, 
        path = '/media/carabus/Enterprise/EW_SDM/predictors/meteo/rasters/wolrdClim/source/')

# если по какой-то причине загрузка данных не работает - надо загрузить 
# биоклиматические слои разрешением 2.5 минуты со страницы https://www.worldclim.org/data/worldclim21.html 
# и распаковать архив в директорию predictors/meteo/rasters/wolrdClim/source/

# территория интереса
aoi = read_sf('navigation/Russia_European_albers.shp')
aoi_dd = st_transform(aoi, st_crs('EPSG:4326'))

# визуализация средствами базовой графики
plot(st_geometry(aoi_dd))

# визуализация средствами продвинутой графики qqplot2
ggplot() +
  geom_sf(data = aoi_dd)

# готовим набор слоев на территорию интереса
# worldClim

# Elevation - цветовая палитра
colfunelev <- colorRampPalette(c('chartreuse4','chartreuse3','darkolivegreen1','gold','gold2','orange','orange3','orange4'))

# слой с высотой над уровнем моря
elev = raster('rasters/worldClim/wc2.1_2.5m_elev.tif')
plot(elev, col = colfunelev(32))

elev_aoi = mask(crop(elev, aoi_dd), aoi_dd) # диапазон высот -31 .. 1412
plot(elev_aoi, col = colfunelev(32))
writeRaster(elev_aoi, 'rasters/worldClim/wc_2e5m_elev_aoi.tif', options = 'COMPRESS=LZW')

# все остальные биоклиматические слои BIO01..19
# описание слоёв https://www.worldclim.org/data/bioclim.html
for (i in 1:19) {
  
  if (i < 10) i = paste0('0',i)
  rst = raster(paste0('rasters/worldClim/wc2.1_2.5m_bio_',i,'.tif'))
  rst_aoi = mask(crop(rst, aoi_dd), aoi_dd)
  writeRaster(rst_aoi, paste0('rasters/worldClim/wc_2e5m_bio_',i,'_aoi.tif'), options = 'COMPRESS=LZW')
  
}


# при повторном запуске можно начинать отсюда 
# слои на территорию интереса в архиве worldClim_aoi.zip
wclayers_aoi = list.files('rasters/worldClim', full.names = T)
wclayers_aoi_stack = stack(wclayers_aoi)
# plot(wclayers_aoi_stack)
names(wclayers_aoi_stack) = c ('BIO_01','BIO_02','BIO_03','BIO_04','BIO_05','BIO_06',
                               'BIO_07','BIO_08','BIO_09','BIO_10','BIO_11','BIO_12',
                               'BIO_13','BIO_14','BIO_15','BIO_16','BIO_17','BIO_18',
                               'BIO_19','elevation')

occurs = read.csv('../../occurrences/acinput.csv')

# находки по всем видам
occurs = read.csv('../../occurrences/pointsForModelling.csv')

names(occurs)
  
input = st_as_sf(occurs, coords = c('decimalLongitude','decimalLatitude'))
st_crs(input) = st_crs('EPSG:4326')

ggplot() +
  geom_sf(data = aoi_dd) +
  geom_sf(data = input) 

  
# одна точка в NA попала
# input = input[-6,] 

predictors = as.data.frame(extract(wclayers_aoi_stack,input))
class(input)

# записываем CSV файл для проверки
predictors = round(predictors, 2)
inputPr = cbind(input,predictors)
inputPr$uncertainty = inputPr$coordinateUncertaintyInMeters
inputPr$coordinateUncertaintyInMeters = NULL

# удаляем значения NA
nrow(inputPr[is.na(inputPr$BIO_01),]) # 14 точек за пределами растров 
# ? территорию интереса расширить на несколько км

# убираем такие точки
inputPrc = inputPr[!is.na(inputPr$BIO_01),]

nrow(inputPr)
nrow(inputPrc)

write.table(inputPrc, '../../occurrences/input4check.csv', row.names = F, sep = '\t')


# убираем колинеарность
predictors$BIO_02 = NULL
predictors$BIO_03 = NULL
predictors$BIO_06 = NULL
predictors$BIO_07 = NULL
predictors$BIO_08 = NULL
# оставим не смотря на корреляцию 0.721 с BIO_01
predictors$BIO_10 = NULL
predictors$BIO_11 = NULL
# для осадков убираем всё, кроме 
# BIO_12 - годовая сумма осадков и BIO_15 - сезонность
# это два, BIO_5 - максильманая t теплого месяца и elevation не коррелируют ни с чем
predictors$BIO_13 = NULL
predictors$BIO_14 = NULL
predictors$BIO_16 = NULL
predictors$BIO_17 = NULL
predictors$BIO_18 = NULL
predictors$BIO_19 = NULL

corrMarrix = round(cor(predictors),3)
write.csv(corrMarrix,'../../occurrences/corrMatrix.csv')


corrplot(corrMarrix, type = 'upper')
# BIO_02 - среднемесячный суточный диапазон          BIO_04, BIO_07
# BIO_03 - изотермальность                           BIO_03
# BIO_06 - средняя температура холодного месяца      BIO_01, BIO_03
# BIO_07 - годовой диапазон температур               BIO_02, BIO_04
# BIO_08 - средняя температура влажного квартала     BIO_01, BIO_05
# BIO_10 - средняя температура теплого квартала      BIO_01, BIO_05
# BIO_11 - средняя температура холодного квартала    BIO_01, BIO_03, BIO_06

# в результате остаётся: 
# среднегодовая температура, сезонность температуры, маx t теплого месяца,
# средняя t сухого квартала, годовая сумма осадков, сезонность осадков, ВНУМ

# background points
set.seed(256)
# bgpoints = st_sample(st_as_sfc(st_bbox(aoi_dd)), size = 200)
bgpoints = st_as_sf(st_sample(aoi_dd, size = 500))
# length(bgpoints)
# bgpnt  = st_sf(bgpoints, ID = seq(length(bgpoints)))

plot(st_geometry(aoi_dd))
plot(bgpoints, add = T, pch = 16, cex = 0.5, col = 'grey')


bgpredictors = as.data.frame(extract(wclayers_aoi_stack,  bgpoints))
names(bgpredictors)
predictors2 = bgpredictors[,c(1,4,5,9,12,15,20)]
names(predictors2)
names(predictors)

predictors$status = 1
predictors2$status = 0

inputData = rbind(predictors, predictors2)

m1 = glm(status ~ BIO_01 + BIO_04 + BIO_05 + BIO_09 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m1)
pt1 = predict(wclayers_aoi_stack, m1, type = 'response')
plot(pt1)
ev1 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m1)
plot(ev1, 'ROC')

m2 = glm(status ~ BIO_01 + BIO_04 + BIO_05 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m2)
pt2 = predict(wclayers_aoi_stack, m2, type = 'response')
plot(pt2)
ev2 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m2)
plot(ev2, 'ROC')


m3 = glm(status ~ BIO_01 + BIO_04 + BIO_05 + BIO_12 + elevation,
              data = inputData, family = 'binomial')
summary(m3)
pt3 = predict(wclayers_aoi_stack, m3, type = 'response')
plot(pt3)
ev3 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m3)
plot(ev3, 'ROC')

m4 = glm(status ~ BIO_01 + BIO_04 + BIO_05 + BIO_12,
         data = inputData, family = 'binomial')
summary(m4)
pt4 = predict(wclayers_aoi_stack, m4, type = 'response')
plot(pt4)
ev4 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m4)
plot(ev4, 'ROC')

m5 = glm(status ~ BIO_01 + BIO_05 + BIO_12,
         data = inputData, family = 'binomial')
summary(m5)
pt5 = predict(wclayers_aoi_stack, m5, type = 'response')
plot(pt5)
ev5 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m5)
plot(ev5, 'ROC')

m6 = glm(status ~ BIO_01 + BIO_12,
         data = inputData, family = 'binomial')
summary(m6)
pt6 = predict(wclayers_aoi_stack, m6, type = 'response')
plot(pt6)
ev6 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m6)
plot(ev6, 'ROC')

plot(st_geometry(input), add = T, pch = 16, cex = 0.7)