

source('C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/scripts/1. tablas de abundancias bruta y relativa.R')
source('C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/scripts/5. especies comunes.R')

rm(lahonda, merced, montes1, montes2, navalagrulla, sanjoaquin, ventas, ucsc, a, c,
   california_species, spain_species, california, spain)


# tabla resumen con los medias y SD de cada especie comun en cada region para plots ####

# creamos una matriz de resultados
commons_cover <- matrix(nrow = length(commons), ncol = 12)
rownames(commons_cover) <- commons
colnames(commons_cover) <- c("mean_cover_spain",
                             "mean_dominance_spain",
                             "occurrence_spain",
                             "rel_occurrence_spain",
                             "mean_cover_california",
                             "mean_dominance_california",
                             "occurrence_california",
                             "rel_occurrence_california",
                             "SE_cover_spain", "SE_cover_california",
                             "SE_dominance_spain", "SE_dominance_california")

# rellenamos la matriz de resultados

spain_relcov <- subset(relative_cover, region=='Spain')
spain_relcov[spain_relcov==0] <- NA

california_relcov <- subset(relative_cover, region=='California')
california_relcov[california_relcov==0] <- NA



for (a in rownames(commons_cover)) {
  
  b <- spain_relcov[,a]
  commons_cover[a,"mean_cover_spain"] <- mean(b[b!=0], na.rm = T)
  commons_cover[a,"SE_cover_spain"] <- sd(b[b!=0], na.rm = T)/sqrt(length(which(b>0)))
  commons_cover[a,"occurrence_spain"] <- summary(is.na(b))[2]
  
  b <- california_relcov[,a]
  commons_cover[a,"mean_cover_california"] <- mean(b[b!=0], na.rm = T)
  commons_cover[a,"SE_cover_california"] <- sd(b[b!=0], na.rm = T)/sqrt(length(which(b>0)))
  commons_cover[a,"occurrence_california"] <- summary(is.na(b))[2]
}


class(commons_cover)
str(commons_cover)
# transformamos commons_cover en un data frame
commons_cover <- as.data.frame(commons_cover)

# para convertir las variables en numerico sin perder decimales indicamos

commons_cover$mean_cover_spain <- as.numeric(as.character(commons_cover$mean_cover_spain))
commons_cover$mean_cover_california <- as.numeric(as.character(commons_cover$mean_cover_california))
commons_cover$SE_cover_spain <- as.numeric(as.character(commons_cover$SE_cover_spain))
commons_cover$SE_cover_california <- as.numeric(as.character(commons_cover$SE_cover_california))
commons_cover$occurrence_spain <- as.numeric(as.character(commons_cover$occurrence_spain))
commons_cover$occurrence_california <- as.numeric(as.character(commons_cover$occurrence_california))


# calculamos el porcentaje de plots ocupados
commons_cover[,'rel_occurrence_spain'] <- commons_cover$occurrence_spain/60
commons_cover[,'rel_occurrence_california'] <- commons_cover$occurrence_california/60






# y anadimos la info de traits
# importamos la base de datos de traits
library(readxl)
plot_traits <- read_excel("allplots.xlsx", sheet = "plot_traits", col_names = T)
plot_traits <- as.data.frame(plot_traits)
plot_traits <- unique(plot_traits)
plot_traits <- subset(plot_traits, species %in% commons) 

# creamos una columna de especies en nuestra matriz de coberturas
commons_cover <- as.data.frame(commons_cover)
commons_cover$species <- rownames(commons_cover)

# juntamos las tablas
commons_cover <- left_join(plot_traits, commons_cover, by="species")
# y eliminamos traits
rm(plot_traits)








# calculamos W promedio + SD, que es la medida de localized dominance de la que habla Pearson ####



# el primer componente de la ecuacion ya le tenemos que es Wj
# wj = la abundancia relativa de una especie en un plot
  relative_cover
  str(relative_cover)

  
# el segundo componente es la abundancia promedio de todas las especies del plot
  
  # creo un vector de abundancia promedio por plot
  mean_cover_plot <- relative_cover
  mean_cover_plot$site <- NULL
  mean_cover_plot$region <-NULL
  mean_cover_plot[mean_cover_plot==0] <- NA
  
  mean_cover_plot <- rowMeans(mean_cover_plot, na.rm=T)
  
  
  # por ultimo divido cada columna de relative_cover por mean_cover_plot 
  
  W_dominance_mat <- relative_cover
  W_dominance_mat$site <- NULL
  W_dominance_mat$region <-NULL
  
  
  for (c in colnames(W_dominance_mat)) {
    
    W_dominance_mat[,c] <- W_dominance_mat[,c] / mean_cover_plot
    
  }  
  
  class(W_dominance_mat)
  
  
  # ahora completo la matriz de resultados para las graficas
        
        commons_cover
        
        # preparamos la tabla de dominancia Wj
        W_dominance_mat[W_dominance_mat==0] <- NA
        W_dominance_mat_spain <- W_dominance_mat[61:120,]
        W_dominance_mat_california <- W_dominance_mat[1:60,]
        
        rownames(commons_cover) <- commons_cover$species
        
        # ponemos las variables que vamos a rellenar en el formato adecuado
        str(commons_cover)
        commons_cover$mean_dominance_spain <- as.numeric(as.character(commons_cover$mean_dominance_spain))
        commons_cover$mean_dominance_california <- as.numeric(as.character(commons_cover$mean_dominance_california))
        commons_cover$SE_dominance_spain <- as.numeric(as.character(commons_cover$SE_dominance_spain))
        commons_cover$SE_dominance_california <- as.numeric(as.character(commons_cover$SE_dominance_california))
        
        
  for (a in rownames(commons_cover)) {
    
    b <- W_dominance_mat_spain[,a]
    commons_cover[a,"mean_dominance_spain"] <- mean(b[b!=0], na.rm = T)
    commons_cover[a,"SE_dominance_spain"] <- sd(b[b!=0], na.rm = T)/sqrt(length(which(b>0)))

    b <- W_dominance_mat_california[,a]
    commons_cover[a,"mean_dominance_california"] <- mean(b[b!=0], na.rm = T)
    commons_cover[a,"SE_dominance_california"] <- sd(b[b!=0], na.rm = T)/sqrt(length(which(b>0)))

  }
  
  
  rm(W_dominance_mat_california, W_dominance_mat_spain, a, b, c)
  
  
  
# vamos a echar un ojo a las correlaciones entre variables 
  # X11()
  # pairs(commons_cover[,9:16])
  # View(cor(commons_cover[,9:16]))
  # hay una gran correlacion a nivel regional
  

# a nivel interregional al correlacion sera mas robusta si eliminamos especies con menos de 3 observaciones
  # en alguno de los rangos

  
  commons_cover <- commons_cover[order(rownames(commons_cover)),]
  
  commons_cover$ID <- c('AVBA', 'BRDI', 'BRMX', 'BRHO', 'BRMA', 'BRTE',
                        'CEGL', 'CRCA', 'ERBO', 'ERMO', 'FEPE', 'GEDI',
                        'HERH', 'HYGL', 'HYRA', 'LIBI', 'LOGA', 'LYAR',
                        'MEPO', 'SEVU', 'SHAR', 'SIGA', 'SIMA', 'TRAN',
                        'TRCA', 'TRSU')
  
  commons_cover2 <- commons_cover[-c(which(commons_cover$occurrence_spain < 3 | commons_cover$occurrence_california < 3)),]

  
  
  # X11()
  # pairs(commons_cover2[,9:16])
  # View(cor(commons_cover2[,9:16]))
  
  
  
  
# nota sobre las abundancias promedio en cada region #### 
  
  dotchart(mean_cover_plot)
  boxplot(mean_cover_plot[1:60], mean_cover_plot[61:120])
  # los datos tienen una clara estructura, la abundancia promedio por plot es mayor en
  # california que en espana, seguramente porque hay menos especies
  
  # voy a plotear los datos brutos, no deberia verse esta estructura ya que no estan estandarizados
  data <- plots
  data[data==0] <- NA
  data$site <- NULL
  data$region <-NULL
  data <- rowMeans(data, na.rm=T)
  dotchart(data)
  boxplot(data[1:60], data[61:120])
  rm(data)
  
  # efectivamente, los datos tienen una estructura, pero al usar los datos brutos esta es mas diluida
  # lo que ocurre es que en california hya menos especies, y las que hay son mas dominantes
  
  
# correlacion entre dominancia y cobertura media ####
  
  # juntamos todos los datos para tener mayor N
  # hacemos un vector con la cobertura media de espana y california
  cover <- c(commons_cover$mean_cover_spain, commons_cover$mean_cover_california)
  
  # y otro con la dominancia de espana y california
  dominance <- c(commons_cover$mean_dominance_spain, commons_cover$mean_dominance_california)
  
  # corremos el test
  plot(cover, dominance)
  cor.test(x=cover, y=dominance, method = "pearson")  
  
  rm(cover, dominance)  
  
  


# datos para modelos #### 
  
  # 3 cosas a tener en cuenta
  
  
  # 1/ establecemos en 3 el minimo numero de observaciones por region para los analisis 
  species <- commons_cover2$species
  
  
  # 2/ usariamos todos los datos de coberturas
  commons_plots <- relative_cover[,colnames(relative_cover) %in% species]
  
  rm(species)
  
  
  # 3/ transformamos las variables arc.sin firn, porque quedan mejor los residuos
  commons_plots <- commons_plots/100
  commons_plots$region <- relative_cover$region
  commons_plots$site <- relative_cover$site
  commons_plots$plot <- rownames(commons_plots)
  
  
  # 4/ hacemos LMM con la estructura random species (species/site/plot) Firn
  # para ello hay que preparar los datos en un tabla que tenga las variables: plot, site, region, species, invasiveness, cover, commonnes
  
  # creamos una variable 'species' y otra 'cover'
  log_table <- commons_plots %>% gather("species", "cover", 1:19)
  
  # preparamos la trabal de traits que vamos a pegarle
  
  species_traits <- read_excel("allplots.xlsx", sheet = "plot_traits")
  
  my_traits <- species_traits[,c(1,3,4,7,8)] %>%
    subset(common == 'common') %>%
    distinct()
  
  # juntamos las tablas
  my_traits <- merge(my_traits, log_table, by = 'species')
  
  # eliminamos filas cuya cobertura sea 0, porque no las usamos para calcular mean_cover
  log_table <- subset(my_traits, cover != 0)
  
  # anadimos una columnas con cover log trans, y otra arcsintrans
  log_table$arcsin_cover <- asin(sqrt(log_table$cover))
  log_table$ln_cover <- log(log_table$cover)
  
  
  rm(my_traits, commons_plots)
  
  
  
  
  
  

  
# tabla de occurrence por sitio para analisis ####
  # hay que preparar la tabla en formato longitudinal
  
  species <- unique(commons_cover2$species)
  site <- unique(log_table$site)
  
  occurrence_data <- expand.grid(species, site)
  
  rm(species, site)
  
  colnames(occurrence_data) <- c('species', 'site')
  
  occurrence_data$occurrence <- NA
  
  
  
  for (sp in unique(commons_cover2$species)) {
    
    ccc <-  subset(relative_cover, select=c("site", sp))
    
    for (st in unique(ccc$site)) {
      
      aaa <- subset(ccc, site == st)
      
      occurrence_data$occurrence[which(occurrence_data$species == sp & occurrence_data$site == st)] <- length(which(aaa[,sp] != 0))
      
    }
    
  }
  
  rm(sp, st, ccc, aaa)
  
  
  
  # anadimos la informacion de region
  
  occurrence_data$region[1:76] <- 'California'
  occurrence_data$region[77:152] <- 'Spain'
  
  # eliminamos filas cuya occurrencia sea 0, porque no se puedent transformar logaritmicamente
  occurrence_data <- occurrence_data[-which(occurrence_data$occurrence == 0),]
  
  # transformamos la variable logaritmicamente
  occurrence_data$ln_occurrence <- log(occurrence_data$occurrence)
  
  # anadimos info de invasiveness
  my_traits <- read_excel("allplots.xlsx", sheet = "plot_traits")
  occurrence_data <- merge(occurrence_data, distinct(my_traits[,c(1,7)]), by = 'species')
  
  # anadimos la variable region_invasiveness
  occurrence_data$region_invasiveness <- paste(occurrence_data$region, occurrence_data$invasiveness)
  
  