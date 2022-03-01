

source('C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/scripts/6. comparacion biogeografica comunes tablas.R')


# los barplots los hacemos con los resultados de los modelos!

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(ggpubr)


#####

tam_puntos <- 4

tam_letra_ejes <- 14


plot1 <- ggplot(data = commons_cover2, aes(x = mean_cover_spain, y = mean_cover_california)) +
  theme_bw() +
  
  geom_point(aes(color = invasiveness), size = tam_puntos, pch = 17) +
  
  scale_color_manual(values=c("grey","red")) +
  
  geom_errorbar(aes(ymin = mean_cover_california-SE_cover_california, ymax = mean_cover_california+SE_cover_california)) + 
  geom_errorbarh(aes(xmin = mean_cover_spain-SE_cover_spain, xmax = mean_cover_spain+SE_cover_spain)) +
  
  geom_text_repel(data=commons_cover2, aes(label=ID), size=tam_puntos*0.90) +
  
  coord_cartesian(xlim = c(0,17.5), ylim = c(0,17.5)) +
  
  labs(x = "mean % cover Spain", y = "mean % cover California") +
  
  geom_abline(intercept = 0, slope = 1, size = 1) +
  
  theme(legend.position = "none",
        text = element_text(color = 'black', size = tam_letra_ejes),
        axis.text.x = element_text(color = 'black', size = tam_letra_ejes),
        axis.text.y = element_text(color = 'black', size = tam_letra_ejes),
        # axis.line = element_line(colour = 'black', size = 2),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))




# preparamos una tabla nueva para hacer barplots
tb2 <- data.frame(matrix(ncol = 4, nrow = 4))
colnames(tb2) <- c('region', 'invasiveness', 'mean_cover', 'SE')
tb2$region <- c('California','California','Spain','Spain')
tb2$invasiveness <- c('Naturalised','Invasive','Naturalised','Invasive')

# la rellenamos con los parametros del modelo
cov_reg_inv <- read.csv("C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/results/ln_cover_regionxinvasiveness.txt", sep="")

# MEDIAS: las interceptas se suman a la primera de la lista
cov_reg_inv$Estimate[2:4] <- cov_reg_inv$Estimate[2:4] + cov_reg_inv$Estimate[1]
tb2$mean_cover <- exp(1) ^ cov_reg_inv$Estimate * 100

# ERRORES
tb2$SE <- exp(1) ^ cov_reg_inv$Std..Error

plot2 <- ggplot(data = tb2, aes(x = region, y = mean_cover, group=invasiveness, fill=invasiveness)) +
  scale_fill_manual(values=c("red","grey")) +
  geom_errorbar(aes(ymin=mean_cover-SE, ymax=mean_cover+SE), width=.2, position=position_dodge(0.5)) +
  geom_bar(stat="identity", position=position_dodge(width=0.5), width = 0.5) +
  labs(y = "mean % cover") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(color = 'black', size = tam_letra_ejes),
        axis.text.x = element_text(color = 'black', size = tam_letra_ejes),
        axis.text.y = element_text(color = 'black', size = tam_letra_ejes),
        # axis.line = element_line(colour = 'black', size = 2),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))



ggarrange(plot1, plot2, labels=c("(a)","(b)"), widths = c(4,2))



# OCCURRENCES

plot3 <- ggplot(data = commons_cover2, aes(x = rel_occurrence_spain*100, y = rel_occurrence_california*100)) +
  theme_bw() +
  geom_point(aes(color = invasiveness), size = tam_puntos, pch=17) +
  
  scale_color_manual(values=c("grey","red")) +

  coord_cartesian(xlim = c(0,85), ylim = c(0,85)) +
  labs(x = "% occupied plots Spain", y = "% occupied plots California") +
  geom_text_repel(data=commons_cover2, aes(label=ID), size=tam_puntos*0.90) +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  
  theme(legend.position = "none",
        text = element_text(color = 'black', size = tam_letra_ejes),
        axis.text.x = element_text(color = 'black', size = tam_letra_ejes),
        axis.text.y = element_text(color = 'black', size = tam_letra_ejes),
        # axis.line = element_line(colour = 'black', size = 2),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))



# creamos una tabla y incluimos SE para anadirla al grafico
tb4 <- data.frame(matrix(ncol = 4, nrow = 4))
colnames(tb4) <- c('region', 'invasiveness', 'occurrence', 'SE')
tb4$region <- c('California','California','Spain','Spain')
tb4$invasiveness <- c('Naturalised','Invasive','Naturalised','Invasive')

# la rellenamos con los parametros del modelo
cooc_reg_inv <- read.csv("C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/results/ln_occurrence_regionxinvasiveness.txt", sep="")

# MEDIAS: las interceptas se suman a la primera de la lista
cooc_reg_inv$Estimate[2:4] <- cooc_reg_inv$Estimate[2:4] + cooc_reg_inv$Estimate[1]
tb4$occurrence <- exp(1) ^ cooc_reg_inv$Estimate * 10

# ERRORES
tb4$SE <- exp(1) ^ cooc_reg_inv$Std..Error * 10

plot4 <- ggplot(data = tb4, aes(x = region, y = occurrence, group=invasiveness, fill=invasiveness)) +
  scale_fill_manual(values=c("red","grey")) +
  geom_bar(stat="identity", position=position_dodge(width=0.5), width = 0.4) +
  geom_errorbar(aes(ymin=occurrence-SE, ymax=occurrence+SE), width=.1, position=position_dodge(0.5)) +
  labs(y = "% plots occupied") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(color = 'black', size = tam_letra_ejes),
        axis.text.x = element_text(color = 'black', size = tam_letra_ejes),
        axis.text.y = element_text(color = 'black', size = tam_letra_ejes),
        # axis.line = element_line(colour = 'black', size = 2),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))






# rm(plot1, plot2, plot3, plot4, tabla_barplot, tb2, tb4, r)











# grafica de dominancia en cada region
ggplot(data = commons_cover2, aes(x = mean_dominance_spain, y = mean_dominance_california)) +
  geom_point(aes(color = invasiveness), size = 2) +
  labs(x = "dominance Spain", y = "dominance California") +
  geom_text_repel(data=commons_cover2, aes(label=ID)) +
  geom_abline(intercept = 0, slope = 1)




# los barplots que incluyo en el paper estan calculados a partir del promedio de cada especie en cada region,
# abajo los calculo a partir de los datos originales brutos (haciendo la media final directamente, sin hacer antes la emdia de cada especie)
# el resultado es muy parecido, pero los SE son mayores al hacerlo con la media de cada especie, asi que lo dejo como estaba

tb2B <- data.frame(matrix(ncol = 4, nrow = 4))
colnames(tb2B) <- c('region', 'invasiveness', 'mean_cover', 'SE')
tb2B$region <- c('California','California','Spain','Spain')
tb2B$invasiveness <- c('invasive','exotic','invasive','exotic')

for (r in unique(log_table$region)) {
  
  ccc <- subset(log_table, region == r)
  
  for (i in unique(log_table$invasiveness)) {
    
    aaa <- subset(ccc, invasiveness == i)
    tb2B$mean_cover[tb2B$region == r & tb2B$invasiveness == i] <- mean(aaa$cover)
    tb2B$SE[tb2B$region == r & tb2B$invasiveness == i] <- sd(aaa$cover, na.rm = T)/sqrt(length(aaa$cover))
    
  }
}

rm(ccc, aaa)

tb2B$invasiveness[tb2B$invasiveness=="exotic"] <- "Naturalised"


ggplot(data = tb2B, aes(x = invasiveness, y = mean_cover, group=region, fill=region)) +
  geom_bar(stat="identity", position=position_dodge(width=0.5), width = 0.4) +
  geom_errorbar(aes(ymin=mean_cover-SE, ymax=mean_cover+SE), width=.1, position=position_dodge(0.5)) +
  labs(x = "invasiveness", y = "mean % cover") +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = c(0.7,0.8),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))

rm(tb2B)


save.image(file='plotsFig6paper.Rdata')





