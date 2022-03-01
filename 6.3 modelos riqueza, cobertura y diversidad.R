


library(vegan)


# hacemos una tabla donde aparezca la cobertura, riqueza y shannon del pool comun por plot

relative_cover
commons


shared_metrics <- matrix(ncol = 5, nrow = 120)

    colnames(shared_metrics) <- c('region', 'site', 'shannon', 'richness', 'cover')
    
    rownames(shared_metrics) <- rownames(relative_cover)
    shared_metrics[,'region'] <- as.character(relative_cover[,'region'])
    shared_metrics[,'site'] <- as.character(relative_cover[,'site'])


shared_metrics <- as.data.frame(shared_metrics)

str(shared_metrics)
shared_metrics$shannon <- as.numeric(shared_metrics$shannon)
shared_metrics$richness <- as.numeric(shared_metrics$richness)
shared_metrics$cover <- as.numeric(shared_metrics$cover)
    
    

for (P in rownames(relative_cover)) {
  
  Px <- relative_cover[P,commons]
  
  shared_metrics[P,'richness'] <- length(which(Px>0))
  shared_metrics[P,'cover'] <- sum(Px)
  shared_metrics[P,'shannon'] <- diversity(Px, index = "shannon")
  
}



str(shared_metrics)

shared_metrics$region <- as.factor(as.character(shared_metrics$region))
shared_metrics$site <- as.factor(as.character(shared_metrics$site))

shared_metrics$shannon <- as.numeric(as.character(shared_metrics$shannon))
shared_metrics$richness <- as.numeric(as.character(shared_metrics$richness))
shared_metrics$cover <- as.numeric(as.character(shared_metrics$cover))



write.table(shared_metrics, 'results/shared_metrics.txt')




# modelo riqueza ####

plot(x=shared_metrics$site, y=shared_metrics$shannon, las=2)


mod <- glmer(richness ~ 1 + region + (1|site), data = shared_metrics, family='poisson')


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))


mean(shared_metrics$richness[shared_metrics$region == 'Spain'])
sd(shared_metrics$richness[shared_metrics$region == 'Spain'])
mean(shared_metrics$richness[shared_metrics$region == 'California'])
sd(shared_metrics$richness[shared_metrics$region == 'California'])




# modelo cobertura ####

plot(x=shared_metrics$site, y=shared_metrics$cover, las=2)


mod <- lmer(log(cover) ~ 1 + region + (1|site), data = shared_metrics)


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))


mean(shared_metrics$cover[shared_metrics$region == 'Spain'])
sd(shared_metrics$cover[shared_metrics$region == 'Spain'])
mean(shared_metrics$cover[shared_metrics$region == 'California'])
sd(shared_metrics$cover[shared_metrics$region == 'California'])




# modelo diversidad ####

plot(x=shared_metrics$site, y=shared_metrics$shannon)


mod <- lmer(shannon ~ 1 + region + (1|site), data = shared_metrics)


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))


mean(shared_metrics$shannon[shared_metrics$region == 'Spain'])
sd(shared_metrics$shannon[shared_metrics$region == 'Spain'])
mean(shared_metrics$shannon[shared_metrics$region == 'California'])
sd(shared_metrics$shannon[shared_metrics$region == 'California'])



    
    
    