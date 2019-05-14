19.01.2019 Ира Королева Стрим по картам

library(stringr)
library(rvest)
library(magrittr)
library(rgdal) 
library(ggmap) 
library(readr) 
library(dplyr) 
library(ggplot2) 
library(ggfortify) 
library(sp) 
library(rgeos)
library(maps)
library(geosphere)
options(scipen = 999)

# загрузка шейпфайла
setwd("~/Downloads/la_LATIMEScounty.shp")
nbh <- readOGR("./", "la_county") # шейпфайл состоит из 5-6 файлов разных расширений, они все должны храниться в одной папке. Мы обращаемся к ним по общему имени, не указывая расширения
plot(nbh)

# шейпфайлы для разных стран и регионов можно поискать здесь
https://gadm.org/download_country_v3.html
https://wiki.openstreetmap.org/wiki/Shapefiles 

# для всего остального есть ggplot2
nbh_df <- fortify(nbh) #  превращаем многоуровневый шейпфайл в табличку. Обычно это не требуется, дальше можно обращаться к самому шейпфайлу 
ggplot() + geom_polygon(data = nbh, aes(x = long, y = lat, group = group)) # аргумент group обязателен для полигонов и линий, потому что именно он говорит какие точки относятся к одному объекту 
  # coord_map() # чтобы картинка не размазывалась, указываем проекцию, по умолчанию - меркатор

# добавим жизненности и наглядности картой-подложкой из google
register_google(key='')
la8 = ggmap(get_googlemap("los angeles", # для неочевидных локаций понадобится уточнить регион
                          maptype = "terrain", # варианты "terrain", "satellite", "roadmap", "hybrid"
                          style = c(feature = "all", element = "labels", visibility = "off"),
                          color = "bw", zoom = 8 # приближение 3 (continent) to 21 (building)
))
plot(la8)
# используем подложку вместо базового слоя ggplot
la8 + geom_path(data = nbh_df, aes(x = long, y = lat, group = group)) 

# геокодирование
geocode(c('hollywood fame alley', '12001 Washington bvld', 'universal pictures'))

LA_RESTAURANT_AND_MARKET_VIOLATIONS <- read_csv("~/Downloads/LA_RESTAURANT_AND_MARKET_VIOLATIONS.csv")
insp <- read_csv("~/Downloads/LA_RESTAURANT_AND_MARKET_INSPECTIONS.csv")
insp = left_join(insp, LA_RESTAURANT_AND_MARKET_VIOLATIONS, by = 'SERIAL NUMBER')
addr = insp[,c(4,11, 12)]
addr = unique(addr)
# к каждому адресу дописываем город и штат, чтоб наверняка
addr$`FACILITY ADDRESS` = paste0(addr$`FACILITY ADDRESS`, ", ", addr$`FACILITY CITY`, ', CA')
x = data.frame() # пустой датафрейм
for (i in length(addr$`FACILITY ID`)){ 
  y = ggmap::geocode(addr$`FACILITY ADDRESS`[i], source = "google") 
  x = rbind(x, y) 
}
# прилепить результат к таблице с адресами
addr = cbind(addr, x)

rest = left_join(insp, addr, by = 'FACILITY ID') # совместим с остальными данными
write.csv(rest, 'restaurants_LA_coord.csv') # и немедленно СОХРАНИМ В ФАЙЛ

# ТОЧКИ В ПОЛИГОНАХ
# https://pp.userapi.com/c850324/v850324009/ac691/jGRnw4xL_4w.jpg
sp_rest = SpatialPoints(na.omit(addr[,c(4,5)])) # переводим координаты в spatial формат
proj4string(sp_rest) = proj4string(nbh) # совмещаем проекции и прочую пространственную инфу

pip = sp::over(nbh, sp_rest, returnList = TRUE ) # считаем точки в полигонах = рестораны в районах
pip_len = lengths(pip) #  длина каждого листа в листе получается sum(pip_len) 44542 значит все ок

# теперь скучные перестановочные штуки
nbh_names = nbh@data$Name # вытаскиваем названия районов из шейпфайла
nbh_names = as.data.frame(nbh_names)
nbh_df$id = as.numeric(nbh_df$id) + 1 # нумерация в nbh_manes и nbh_id не совпадала
nbh_names$id = as.numeric(rownames(nbh_names))
nbh_df = full_join(nbh_df, nbh_names, by = 'id')
# colnames(nbh_df)[8] = 'Neighborhood' # название столбика
pip = cbind(pip_len, nbh_names) # совмещаем названия районов и количество заведений
nbh_pip =  left_join(nbh_df, pip, by = 'nbh_names')

# все подписи на карту не влезут, значит придётся фильтровать, для этого нужны отдельные данные
agg.data <- aggregate(cbind(long,lat) ~ nbh_names, data = nbh_pip, mean)
agg.data = left_join(agg.data, pip, by = 'nbh_names')

ggplot() + geom_polygon(data = nbh_pip, aes(x = long, y = lat, group = group, alpha = 0.6, fill = as.numeric(pip_len))) + # красим полигоны по количеству ресторанов в них
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + 
  scale_fill_gradientn(guide="colourbar", colours=rev(colorspace::heat_hcl(300)), na.value = 'gray88') + 
  geom_text(data=subset(agg.data, pip_len > 500 | pip_len < 10), aes(fontface=2, long, lat , label = nbh_names), 
            color= "black",size=3,check_overlap = T) + # оставляем только подписи для районов где очень много и где очень мало ресторанов
  coord_map() + # последними идут ограничительные параметры, только так они распространятся на все слои
  coord_fixed(xlim = c(-118.7, -118),  ylim = c(33.9, 34.5)) # обрезаем картинку 

# GREAT CIRCLES 
# построение длинных линий\расчет больших дистанций требует учитывать изгибы земной поверхности
# https://www.r-graph-gallery.com/how-to-draw-connecting-routes-on-map-with-r-and-great-circles/ 
map('world',col="gray27", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80) )
spb = c(33, 56)
la = c(-118, 34)
points(x=c(33, -118), y=c(56, 34), col="slateblue", cex=3, pch=20)
dist <- gcIntermediate(spb,  la, n=50, addStartEnd=TRUE, breakAtDateLine=F)      
# dist = greatCircle(spb, la, n=360, sp=FALSE)
lines(dist, col="slateblue", lwd=2)

# РАССТОЯНИЕ МЕЖДУ ТОЧКАМИ
rest_unique = unique(addr)
raster::pointDistance(rest_unique[1,4:5], rest_unique[2,4:5], lonlat = TRUE)
distm(rest_unique[1,4:5], rest_unique[2,4:5], fun=distGeo)
# чтобы учесть действительность в виде дорог и заборов, придется обратиться к гугл апи
mapdist('21190 GOLDEN SPRINGS DR, DIAMOND BAR CA', '24134 LYONS AVE, NEWHALL, CA', mode = "driving",
        output = "all", language = "en-EN") 
# "93.5 km", "58 mins"





