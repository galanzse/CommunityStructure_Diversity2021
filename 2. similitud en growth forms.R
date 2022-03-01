

# recuperamos las matrices de ocurrencia de especies
source('C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/scripts/1. tablas de abundancias bruta y relativa.R')


# tablas ####

# recuperamos la informacion de life forms y invasividad
plot_traits <- read_excel("allplots.xlsx", sheet = "plot_traits") %>%
  as.data.frame()
# codificamos correstamente los NAs
plot_traits$`growth form`[plot_traits$`growth form`=='NA'] <- NA




# creamos un vector con los nombres de las especies de Espana
spain <- subset(plots, region=="Spain")
spain$region <- NULL
spain$site <- NULL
spain_species <- as.data.frame(colSums(spain))
spain_species <- as.data.frame(which(colSums(spain)>0))
spain_species <- rownames(spain_species)
# creamos un vector con los nombres de las especies de California
california <- subset(plots, region=="California")
california$region <- NULL
california$site <- NULL
california_species <- as.data.frame(colSums(california))
california_species <- as.data.frame(which(colSums(california)>0))
california_species <- rownames(california_species)

rm(spain, california)




# a partir de este vector filtramos la tabla de traits y anadimos la variable region
spain_species_traits <- subset(plot_traits, plot_traits$species %in% spain_species)
spain_species_traits$region <- 'Spain'
california_species_traits <- subset(plot_traits, plot_traits$species %in% california_species)
california_species_traits$region <- 'California'
# y las pegamos en una tabla unica
species_traits <- rbind(spain_species_traits, california_species_traits)
# quitamos NAs de growth form
species_traits <- species_traits[-which(is.na(species_traits$`growth form`)),]




# chi-sq para comparacion biogeografica ####

  # growth form
      chisq.test(table(species_traits$region, species_traits$`growth form`))
  # longevity
      chisq.test(table(species_traits$region, species_traits$longevity))
  # life form
      chisq.test(table(species_traits$region, species_traits$`life form`))
  # origin
      chisq.test(table(species_traits$region, species_traits$origin))


# el test de fisher nos dice que la composicion de los dos pooles regionales es similar
      
      

      
# chi-sq dentro de cada region con origen y growth form ####

      chisq.test(table(species_traits$origin[species_traits$region=='Spain'],
                       species_traits$`growth form`[species_traits$region=='Spain']))
      
      
      chisq.test(table(species_traits$origin[species_traits$region=='California'],
                       species_traits$`growth form`[species_traits$region=='California']))
      
      
      
#diagrama de quesitos #### 

cheese <- matrix(ncol=2, nrow=3)
colnames(cheese) <- c('Spain', 'California')
rownames(cheese) <- c('exotic', 'invasive', 'native')
      
cheese[,'Spain'] <- table(spain_species_traits$invasiveness)
cheese[,'California'] <- table(california_species_traits$invasiveness)



table(species_traits$`growth form`[species_traits$common == 'common'])/2

      

rm(spain_species_traits, california_species_traits, cheese)
      



