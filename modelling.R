# собственно моделирование 
# 2023-12-13
# этап 7

# рабочая директория
setwd('/media/carabus/Enterprise/EW_SDM/')

# install.packages('dismo')
library(dismo)
library(raster)
library(sf)
library(ggplot2)
library(corrplot)

ac = read.csv('occurrences/acPoints.csv')
do = read.csv('occurrences/doPoints.csv')
lr = read.csv('occurrences/lrPoints.csv')
lt = read.csv('occurrences/ltPoints.csv')
lt = read.csv('occurrences/ltPoints2.csv')
ol = read.csv('occurrences/olPoints.csv')

ourDataset = read.csv('occurrences/pointsForModelling.csv')
wormPoints = read.csv('occurrences/wormPoints.csv')

occurs = ac
occurs = do
occurs = lr
occurs = lt
occurs = ol
occurs = ourDataset
occurs = wormPoints

# предикторы WorldClim
wclayers_aoi = list.files('predictors/meteo/rasters/wolrdClim/', full.names = T)
wclayers_aoi_stack = stack(wclayers_aoi)
plot(wclayers_aoi_stack$wc_2e5m_bio_01_aoi)

names(wclayers_aoi_stack) = c ('BIO_01','BIO_02','BIO_03','BIO_04','BIO_05','BIO_06',
                               'BIO_07','BIO_08','BIO_09','BIO_10','BIO_11','BIO_12',
                               'BIO_13','BIO_14','BIO_15','BIO_16','BIO_17','BIO_18',
                               'BIO_19','elevation')

names(occurs)
inputOccurs = st_as_sf(occurs, coords = c('latitude','longitude'))
st_crs(inputOccurs) = st_crs('EPSG:4326')

res(wclayers_aoi_stack)
wcStack05 = resample(wclayers_aoi_stack, 0.05)

addLayer(wclayers_aoi_stack, newStack)

ggplot() + 
  geom_sf(data = aoi) + 
  geom_sf(data = inputOccurs)

inputValues = as.data.frame(raster::extract(wclayers_aoi_stack,inputOccurs))

# inputValues = inputValues[-29,] # Do

nrow(inputValues)
inputValues = inputValues[!is.na(inputValues$BIO_01),]
nrow(inputValues)

corrMarrix = round(cor(inputValues),3)
write.csv(corrMarrix,'occurrences/corrMatrix_lt2.csv')
corrplot(corrMarrix, type = 'upper')

# для террестриса - 12,15, elev, 9,

names(inputValues)

# bg points 
set.seed(256)

bgpoints = st_as_sf(st_sample(aoi_dd, size = 512))
plot(st_geometry(aoi_dd))
plot(bgpoints, add = T, pch = 16, cex = 0.5, col = 'grey')
plot(inputOccurs, add = T, pch = 16, cex = 0.7, col = 'red')

inputValuesBG = as.data.frame(raster::extract(wclayers_aoi_stack, bgpoints))
# inputValuesBG = inputValuesBG[, c(1,3,4,5,9,12,15,20)]

inputValues$status = 1
inputValuesBG$status = 0

inputData = rbind(inputValues, inputValuesBG)

# Aporrectodea caliginosa
m1 = glm(status ~ BIO_01 + BIO_03 + BIO_05 + BIO_09 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m1)
pt1 = predict(wclayers_aoi_stack, m1, type = 'response')
plot(pt1)
ev1 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m1)
plot(ev1, 'ROC')

# без BIO1
m11 = glm(status ~ BIO_03 + BIO_04 + BIO_05 + BIO_06 + BIO_09 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m11)
pt11 = predict(wclayers_aoi_stack, m11, type = 'response')
plot(pt11)
ev11 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m11)
plot(ev11, 'ROC')


# Dendrobaeba octaedra
m2 = glm(status ~ BIO_01 + BIO_02 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m2)
pt2 = predict(wclayers_aoi_stack, m2, type = 'response')
plot(pt2)
ev2 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m2)
plot(ev1, 'ROC')

# Lumbricus rubellus
m3 = glm(status ~ BIO_01 + BIO_02 + BIO_05 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m3)
pt3 = predict(wclayers_aoi_stack, m3, type = 'response')
plot(pt3)
ev3 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m3)
plot(ev3, 'ROC')

# Lumbricus terrestris - с заполярной точкой
m4 = glm(status ~ BIO_01 + BIO_02 + BIO_09 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m4)
pt4 = predict(wclayers_aoi_stack, m4, type = 'response')
plot(pt4)
ev4 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m4)
plot(ev4, 'ROC')

# Lumbricus terrestris - без заполярной точки
m41 = glm(status ~ BIO_01 + BIO_02 + BIO_09 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m41)
pt41 = predict(wclayers_aoi_stack, m41, type = 'response')
plot(pt41)
ev41 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m41)
plot(ev41, 'ROC')


# Octolasiun lacteum 
m5 = glm(status ~ BIO_01 + BIO_02 + BIO_04 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m5)
pt5 = predict(wclayers_aoi_stack, m5, type = 'response')
plot(pt5)
ev5 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m5)
plot(ev5, 'ROC')


# все точки непрореженные
m6 = glm(status ~ BIO_01 + BIO_02 + BIO_05 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m6)
pt6 = predict(wclayers_aoi_stack, m6, type = 'response')
plot(pt6)
ev6 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m6)
plot(ev6, 'ROC')


# все точки по червям прореженные
m7 = glm(status ~ BIO_01 + BIO_02 + BIO_12 + BIO_15 + elevation,
         data = inputData, family = 'binomial')
summary(m7)
pt7 = predict(wclayers_aoi_stack, m7, type = 'response')
plot(pt7)
ev7 = evaluate(inputData[inputData$status == 1, ], inputData[inputData$status == 0, ], m7)
plot(ev7, 'ROC')