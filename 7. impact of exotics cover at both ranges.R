

library(tidyverse)
library(lattice)


# cargo los datos de riqueza, shannon y coberturas por plots
community_metrics <- read.csv("C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/results/community_metrics.txt", sep="")

# ordeno los niveles del factor site por region y clima
community_metrics <- community_metrics %>% mutate(site = factor(site,
                                              levels = c("Santa Cruz", "La Honda", "Merced", "San Joaquin",  "Montes1",  "Montes2",  "Navalagrulla", "Ventas")))

# creo una variable que diferencia entre especies nativas y el resto del pool (incluyendo posibles especies invasoras)
community_metrics$notcommon <- community_metrics$richness-community_metrics$cooc_pool

# pasamos la cobertura de 0-100 a 0-1
community_metrics$cover_natives <- community_metrics$cover_natives/100
community_metrics$cover_exotics <- community_metrics$cover_exotics/100


# grafica lattice ####

# xyplot(shannon ~ cooc_pool | site, data = community_metrics, cex = 1)

# xyplot(cover_natives ~ cooc_pool | site, data = community_metrics, cex = 1)

colnames(community_metrics)

xyplot(richness_natives ~ richness_exotics | site,
       
       pch=16, col='black',
       
       strip=strip.custom(factor.levels=c("Montes 1",  "Montes 2",  "Navalagrulla", "Ventas",
                                          "Santa Cruz", "La Honda", "Merced", "San Joaquin")),
       
       xlab="Exotics richness", ylab="Native species richness",
       
       data = community_metrics, cex = 1,
       
       type = c("p", "r"))


# modelo general ####

  mod1 <- lmer(richness_natives ~ 1 + richness_exotics + (1|site), data = community_metrics, REML = T)
  summary(mod1)
  anova(mod1)
  r.squaredGLMM(mod1)
  
  plot(mod1)
  plot(mod1, site ~ resid(.))
  par(mfrow = c(1, 2))
  qqnorm(resid(mod1))
  qqline(resid(mod1))
  qqnorm(unlist(ranef(mod1)))
  qqline(unlist(ranef(mod1)))
  
  rm(mod1)
  

# test de correlacion como en la figura 3 de Martin Fores 2017 ####

  # creamos una tabla de resultados con todos los pares de combinaciones entre variables y sitios 
  stat_natives <- c("shannon_natives", "richness_natives", "cover_natives")
  stat_exotics <- c("shannon_exotics", "richness_exotics", "cover_exotics")
  
  cor_results <- expand.grid(stat_natives, stat_exotics, unique(community_metrics$site)) %>%
    as.data.frame()
  
  colnames(cor_results) <- c('natives', 'exotics', 'site')
  
  # anado las variables donde almacenaremos los resultados, df=13 siempre
  cor_results$p.value <- NA
  cor_results$t <- NA
  cor_results$cor <- NA
  

  # generamos un loop para llenar la tabla
  
  for (rw in rownames(cor_results)) {

    a <- subset(community_metrics, site == cor_results[rw,'site'])
    
    nat <- as.character(cor_results[rw,'natives'])
    exo <- as.character(cor_results[rw,'exotics'])
    
    a <- cor.test(x=a[,nat],  y=a[,exo], method = 'pearson')
    
    cor_results[rw,'p.value'] <- a$p.value
    cor_results[rw,'t'] <- a$statistic
    cor_results[rw,'cor'] <- a$estimate
    
    # rm(a, nat, exo, rw)
    
  }  
  
  # dejo 3 decimales del p.value para que sea mas facil entenderlo
  cor_results$p.value <- round(cor_results$p.value, 3)



              
              