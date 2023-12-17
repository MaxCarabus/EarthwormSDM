# моделирование MaxEnt
# этап 8

# install.packages('dismo')
library(dismo)

# задаём рабочую директорию !
setwd('/media/carabus/Enterprise/EW_SDM/')

# точки находок
ac = read.csv('occurrences/acPoints.csv')
do = read.csv('occurrences/doPoints.csv')
lr = read.csv('occurrences/lrPoints.csv')
lt = read.csv('occurrences/ltPoints.csv')
lt = read.csv('occurrences/ltPoints2.csv')
ol = read.csv('occurrences/olPoints.csv')

# install.packages('rJava')
library(rJava)

# предикторы WorldClim
wclayers_aoi = list.files('predictors/meteo/rasters/wolrdClim/', full.names = T)
wclayers_aoi_stack = stack(wclayers_aoi)
plot(wclayers_aoi_stack$wc_2e5m_bio_01_aoi)

names(wclayers_aoi_stack) = c ('BIO_01','BIO_02','BIO_03','BIO_04','BIO_05','BIO_06',
                               'BIO_07','BIO_08','BIO_09','BIO_10','BIO_11','BIO_12',
                               'BIO_13','BIO_14','BIO_15','BIO_16','BIO_17','BIO_18',
                               'BIO_19','elevation')

# начинать отсюда
occurs = ac
occurs = do
occurs = lt
occurs = lr
occurs = ol

# seLayers = dropLayer(wclayers_aoi_stack, c(1,2,4,5))
# names(seLayers)
# names(wclayers_aoi_stack)

occurs = occurs[,c(6,7)]
fold = kfold(occurs, k = 5) # делим на 80% 20%
occurTrain = occurs[fold != 1,] # точки для обучения модели
occurTest  = occurs[fold == 1,] # точки для теста

acMe = maxent(wclayers_aoi_stack, occurTrain)
# acMe = maxent(seLayers, occurTrain)
par(mar = c(2,2,2,2))
response(acMe)
plot(acMe)

acP = predict(acMe, wclayers_aoi_stack)
plot(acP)

bg = randomPoints(wclayers_aoi_stack, 256)
e1 = evaluate(acMe, p = occurTest, a = bg, x = wclayers_aoi_stack)
plot(e1, 'ROC')

pvtest = data.frame(raster::extract(wclayers_aoi_stack, occurTest))
avtest = data.frame(raster::extract(wclayers_aoi_stack, bg))

e2 = evaluate(acMe, p = pvtest, a = avtest)
plot(e2, 'ROC')