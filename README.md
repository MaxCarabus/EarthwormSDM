## (under updating.....% 2023-12-14)

### Моделирование распространения массовых видов дождевых червей на равнинной территории Еврочейской части России

Исследование поддержано грантом Российского Научного Фонда № [23-24-00112](https://rscf.ru/en/project/23-24-00112/)

Набор данных по литературным данным [Earthworm occurrences from Russian-language literature](https://www.gbif.org/dataset/9ceef4b3-ecac-4f8a-9cca-b4a7953640ba)

Целевые виды: 
1. Aporrectodea caliginosa (Savigny, 1826)

2. Dendrobaena octaedra (Savigny, 1826)

3. Lumbricus terrestris Linnaeus, 1758

4. Lumbricus rubellus Hoffmeister, 1843
   
5. Octolasion lacteum (Örley, 1881)

скрипты:<br>

1. ***occurrences.R*** - поиск, импорт и подготовка точек находок в качестве входного набора данных для моделирования. Ресурс: Глобальная Информационная Система о Биоразннобразии (GBIF)<br
                                                                                                                                                                                            >
2. ***predictors.R*** - извлечение данных из слоёв предикторов

3. ***modelling.R*** - моделирование потенциальных ареалов методом логистической регрессии (GLM)<br>
***pointsForModelling.csv*** - заранее отобранные находки целевых видов
