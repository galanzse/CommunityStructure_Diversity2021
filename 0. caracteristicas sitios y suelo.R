


library(tidyverse)
library(ggplot2)


# suelo ####

library(readxl)

suelo <- read_excel("data/suelo.xlsx")
str(suelo)


# correlacion entre nutrientes

# X11()
pairs(suelo[,3:7])


# ratio C/N es directamente %C dividido por %N, es el parametro que menos varia, y,
# segun wikipedia un suelo se considera fértil si
# el valor numérico de esta relación se encuentra en torno a 10



# genero una tabla para el articulo

colnames(suelo)

means_soil <- suelo %>%
  group_by(grassland) %>% 
  summarize(C = mean(C, na.rm=T),
            OM = mean(OM, na.rm=T),
            N = mean(N, na.rm=T),
            P = mean(P, na.rm=T),
            `C:N` = mean(`C:N`, na.rm=T)
  )
means_soil <- as.data.frame(means_soil)


# voy a sacar SE de los suelos para que se vea lo variables que son

for (st in unique(suelo$grassland)) {
  
  aaa <- subset(suelo, grassland == st)
  
  means_soil$N_SE[means_soil$grassland == st] <- sd(aaa$N)/sqrt(5)
    
  means_soil$P_SE[means_soil$grassland == st] <- sd(aaa$P)/sqrt(5)
    
  means_soil$OM_SE[means_soil$grassland == st] <- sd(aaa$OM)/sqrt(5)
  
  means_soil$CN_SE[means_soil$grassland == st] <- sd(aaa$`C:N`)/sqrt(5)
  
  
  rm(st, aaa)
    
}



# ordenamos los sitios por regiones para verlo mejor en boxplot

suelo <- suelo %>% mutate(grassland = factor(grassland, levels = c("Santa Cruz","La Honda",
                                                         "Merced","San Joaquin",
                                                         "Montes","Puerto",
                                                         "Navalagrulla","Ventas")))

par(mfrow=c(2,2))
boxplot(suelo$OM ~ suelo$grassland)
boxplot(suelo$P ~ suelo$grassland)
boxplot(suelo$`C:N` ~ suelo$grassland)




# clima ####

library(raster)
library(maps)
library(sp)

# genero una tabla de sitios y coordenadas
coordenadas <- matrix(ncol=3, nrow=8) %>% as.data.frame()
colnames(coordenadas) <- c('grassland', 'latitude', 'longitude')
coordenadas$grassland <- unique(means_soil$grassland)
coordenadas$MAR <- NA
coordenadas$MAT <- NA
coordenadas$MCM <- NA
coordenadas$TAR <- NA


      # incluyo las coordenadas
      coordenadas$latitude[coordenadas$grassland == 'Santa Cruz'] <- 36.989260
      coordenadas$longitude[coordenadas$grassland == 'Santa Cruz'] <- -122.057895
        
      coordenadas$latitude[coordenadas$grassland == 'San Joaquin'] <- 37.101172
      coordenadas$longitude[coordenadas$grassland == 'San Joaquin'] <- -119.736823
        
      coordenadas$latitude[coordenadas$grassland == 'La Honda'] <- 37.08416667
      coordenadas$longitude[coordenadas$grassland == 'La Honda'] <- -122.28388889
      
      coordenadas$latitude[coordenadas$grassland == 'Merced'] <-  37.37750000
      coordenadas$longitude[coordenadas$grassland == 'Merced'] <- -120.40555556
      
      coordenadas$latitude[coordenadas$grassland == 'Ventas'] <-  37.75916667
      coordenadas$longitude[coordenadas$grassland == 'Ventas'] <- -5.75666667
      
      coordenadas$latitude[coordenadas$grassland == 'Navalagrulla'] <-  37.75916667
      coordenadas$longitude[coordenadas$grassland == 'Navalagrulla'] <- -5.93916667
      
      coordenadas$latitude[coordenadas$grassland == 'Montes'] <-  36.60444444
      coordenadas$longitude[coordenadas$grassland == 'Montes'] <- -5.58444444
      
      coordenadas$latitude[coordenadas$grassland == 'Puerto'] <-  36.61166667
      coordenadas$longitude[coordenadas$grassland == 'Puerto'] <- -5.52722222

        
      # lo convierto en un archivo de puntos
      points <- coordenadas
      #decimos a R que columnas se corresponden con coordenadas
      coordinates(points) <- ~ longitude + latitude
      # establecemos nuestra proyeccion
      crs(points) <- CRS("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")  

      

# importo el mapa de MAR
  world_MAR <- raster("C:/Users/Javier/Desktop/GIS & Model/wc2.0_30s_bio/wc2-5/bio12.bil")
  crs(world_MAR) <- CRS("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")
  world_MAT <- raster("C:/Users/Javier/Desktop/GIS & Model/wc2.0_30s_bio/wc2-5/bio1.bil")
  crs(world_MAT) <- CRS("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")
  world_MCM <- raster("C:/Users/Javier/Desktop/GIS & Model/wc2.0_30s_bio/wc2-5/bio6.bil")
  crs(world_MCM) <- CRS("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")
  world_TAR <- raster("C:/Users/Javier/Desktop/GIS & Model/wc2.0_30s_bio/wc2-5/bio7.bil")
  crs(world_TAR) <- CRS("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")
  
  
 
  
  
# compruebo que los puntos estan donde tienen que estar
X11()
plot(world_MAR)
points(points, pch=16)
 

# extraigo la info
coordenadas$MAR <- raster::extract(world_MAR, points, method = "simple")
coordenadas$MAT <- raster::extract(world_MAT, points, method = "simple")/10
coordenadas$MCM <- raster::extract(world_MCM, points, method = "simple")/10
coordenadas$TAR <- raster::extract(world_TAR, points, method = "simple")/10


rm(world_MAR, world_MAT, world_MCM, points)



# juntamos las tablas ####


coordenadas <- left_join(means_soil, coordenadas, by='grassland')

write.table(coordenadas, 'results/caracterizacion_sitios.txt')

# analisis ####

# no tenemos tamaño muestral como para hacer estadistica
# asi que voy a descomponer la varianza para ensenar que la variabilidad no depende de la region

library(lmerTest)


modCM <- lmerTest::lmer(`C:N` ~ 1 + region + (1|grassland), data=suelo)
summary(modCM)
# grassland var 0.03928324
# region var 0
# residual var 1.95916
# total 1.998443

modP <- lmerTest::lmer(P ~ 1 + region + (1|grassland), data=suelo)
summary(modP)
# grassland var 16.7046
# region var 3.682062e-07
# residual var 31.29607
# total 48.00067


modOM <- lmerTest::lmer(OM ~ 1 + region + (1|grassland), data=suelo)
summary(modOM)



modN <- lmerTest::lmer(N ~ 1 + region + (1|grassland), data=suelo)
summary(modN)

# los resultados nos dicenq ue no hay diferencia en nutrientes de suelo entre regiones


rm(modCM, modP, modOM)















