## ---- ggplot theme ----

require(scales)
require(ggplot2)
require(grid)
require(gridExtra)

annotate_plot <- function(label) { annotation_custom(grobTree(textGrob(label, x = 0.1,  y = 0.95, hjust = 0, vjust = 1,
                                                                       gp = gpar(col = "black", fontsize = 11)))) }

annotate_plot <- function(label) { annotation_custom(grobTree(textGrob(label, x = 0.1,  y = 0.95, hjust = 0, vjust = 1,
                                                                       gp = gpar(col = "black", fontsize = 9)))) }

cubroot_trans = function() trans_new('cubroot', transform= function(x) x^(1/3),
                                     inverse = function(x) x^3 )

theme_oeco <- theme_classic() +
  theme(axis.title = element_text(size = 10), axis.text = element_text(size = 10), 
        axis.line.x = element_line(size = 0.35, colour = 'grey50'), axis.line.y = element_line(size = 0.35, colour = 'grey50'),
        axis.ticks = element_line(size = 0.25, colour = 'grey50'), 
        legend.justification = c(1, 0.75), legend.position = c(1, 0.9), legend.key.size = unit(0.35, 'cm'),
        legend.title = element_blank(), legend.text = element_text(size = 9),
        legend.text.align = 0, legend.background = element_blank(),
        plot.subtitle = element_text(size = 10, vjust = 0), #plot.margin = unit(c(0.35, 0, 0.25, 0), 'cm'),
        strip.background = element_blank(), strip.text = element_text(hjust = 0.5, size = 10 ,vjust = 0), 
        strip.placement = 'outside', panel.spacing.x = unit(0, 'cm'))
  theme_set(theme_oeco)

  
update_geom_defaults('boxplot', list(size = 0.5))
update_geom_defaults('pointrange', list(size = 0.35))
update_geom_defaults('smooth', list(span = 1, size = 0.5)) 
update_geom_defaults('point', list(size = 2))
update_geom_defaults('errorbar', list(size = 0.35))
update_geom_defaults('hline', list(size = 0.35, colour = 'grey50'))


## ---- Functions ----
require(tidyverse)
resids.fig <- function(mod, df) {
  residdf <- dplyr::mutate(df, resids = residuals(mod, type = 'normalized'),
                           fits = fitted(mod))
  
  fig1 <- ggplot(residdf, aes(x = nutrients, y = resids)) + geom_boxplot() + 
    geom_boxplot(aes(x = enemies), colour = 'red') +
    geom_boxplot(aes(x = provenance), colour = 'blue') + 
    labs(y = 'Residuals\n', x = '\nTreatment levels')
  
  fig2 <- ggplot(residdf, aes(x = fits, y = resids)) + geom_point() +
    labs(x = '\nFitted values', y = '\n\n')
  
  fig3 <- ggplot(residdf) + stat_qq(aes(sample = resids)) +
    labs(x = '\nTheoretical Quantiles', y = 'Sample Quantiles\n')
  
  fig4 <- ggplot(residdf, aes(x = resids)) + geom_histogram(aes(y=..density..), colour = 'grey50') +
    labs(x = '\nResiduals', y = 'Frequency\n') + scale_y_continuous(expand = c(0, 0)) +
    stat_function(fun = dnorm, color = "red", args = list(mean = mean(residdf$resids), 
                                                          sd = sd(residdf$resids)))
  
  grid.draw(rbind(cbind(ggplotGrob(fig1), ggplotGrob(fig2), size = 'first'),
                  cbind(ggplotGrob(fig3), ggplotGrob(fig4), size = 'first'), size = 'first'))
  
  return(shapiro.test(residdf$resids))
}


resids.lmerfig <- function(mod, df) {
  
  residdf <- dplyr::mutate(df, resids = residuals(mod, type = 'pearson'),
                           fits = predict(mod))
  
  fig1 <- ggplot(residdf, aes(x = fits, y = resids)) + geom_point()
  
  fig2 <- ggplot(residdf, aes(x = nutrients, y = resids)) + 
    geom_boxplot(colour = 'red') + 
    geom_boxplot(aes(x = enemies), colour = 'blue') 
  
  fig3 <- ggplot(residdf) + stat_qq(aes(sample = resids)) +
    labs(x = '\nTheoretical Quantiles', y = 'Sample Quantiles\n')
  
  
  fig4 <- ggplot(residdf, aes(x = resids)) + geom_histogram(aes(y=..density..), colour = 'grey50') +
    labs(x = '\nResiduals', y = 'Frequency\n') + scale_y_continuous(expand = c(0, 0)) +
    stat_function(fun = dnorm, color = "red", args = list(mean = mean(residdf$resids), 
                                                          sd = sd(residdf$resids)))
  
  grid::grid.draw(rbind(cbind(ggplotGrob(fig1), ggplotGrob(fig2), size = 'first'),
                        cbind(ggplotGrob(fig3), ggplotGrob(fig4), size = 'first'), size = 'first'))
  
  return(shapiro.test(residdf$resids))
  
}


resids.lme1fig <- function(mod, df) {
  require(dplyr)
  residdf <- dplyr::mutate(df, .resid = residuals(mod, type = 'normalized'), 
                           .fitted = fitted(mod),
                           var1 = df[, names(mod$contrasts)[1]])
  
  fig1 <- ggplot(residdf, aes(x = var1, y = .resid)) + geom_boxplot() + 
    labs(y = 'Residuals\n', x = '\nTreatment levels')
  
  fig2 <- ggplot(residdf, aes(x = .fitted, y = .resid)) + geom_point() +
    labs(x = '\nFitted values', y = '\n\n')
  
  fig3 <- ggplot(residdf) + stat_qq(aes(sample = .resid)) +
    labs(x = '\nTheoretical Quantiles', y = 'Sample Quantiles\n')
  
  fig4 <- ggplot(residdf, aes(x = .resid)) + geom_histogram(aes(y=..density..), colour = 'grey50') +
    labs(x = '\nResiduals', y = 'Frequency\n') + scale_y_continuous(expand = c(0, 0)) +
    stat_function(fun = dnorm, color = "red", args = list(mean = mean(residdf$.resid), 
                                                          sd = sd(residdf$.resid)))
  
  grid.draw(rbind(cbind(ggplotGrob(fig1), ggplotGrob(fig2), size = 'first'),
                  cbind(ggplotGrob(fig3), ggplotGrob(fig4), size = 'first'), size = 'first'))
  
  return(shapiro.test(residdf$.resid))
}


resids.lme2fig <- function(mod, df) {
  require(dplyr)
  residdf <- dplyr::mutate(df, .resid = residuals(mod, type = 'normalized'), 
                           .fitted = fitted(mod),
                           var1 = df[, names(mod$contrasts)[1]],
                           var2 = df[, names(mod$contrasts)[2]])
  
  fig1 <- ggplot(residdf, aes(x = var1, y = .resid)) + geom_boxplot() + 
    geom_boxplot(aes(x = var2), colour = 'red') +
    labs(y = 'Residuals\n', x = '\nTreatment levels')
  
  fig2 <- ggplot(residdf, aes(x = .fitted, y = .resid)) + geom_point() +
    labs(x = '\nFitted values', y = '\n\n')
  
  fig3 <- ggplot(residdf) + stat_qq(aes(sample = .resid)) +
    labs(x = '\nTheoretical Quantiles', y = 'Sample Quantiles\n')
  
  fig4 <- ggplot(residdf, aes(x = .resid)) + geom_histogram(aes(y=..density..), colour = 'grey50') +
    labs(x = '\nResiduals', y = 'Frequency\n') + scale_y_continuous(expand = c(0, 0)) +
    stat_function(fun = dnorm, color = "red", args = list(mean = mean(residdf$.resid), 
                                                          sd = sd(residdf$.resid)))
  
  grid.draw(rbind(cbind(ggplotGrob(fig1), ggplotGrob(fig2), size = 'first'),
                  cbind(ggplotGrob(fig3), ggplotGrob(fig4), size = 'first'), size = 'first'))
  
  return(shapiro.test(residdf$.resid))
}


resids.lme3fig <- function(mod, df) {
  require(dplyr)
  residdf <- dplyr::mutate(df, .resid = residuals(mod, type = 'normalized'), 
                           .fitted = fitted(mod),
                           var1 = df[, names(mod$contrasts)[1]],
                           var2 = df[, names(mod$contrasts)[2]],
                           var3 = df[, names(mod$contrasts)[3]])
  
  fig1 <- ggplot(residdf, aes(x = var1, y = .resid)) + geom_boxplot() + 
    geom_boxplot(aes(x = var2), colour = 'red') +
    geom_boxplot(aes(x = var3), colour = 'blue') + 
    labs(y = 'Residuals\n', x = '\nTreatment levels')
  
  fig2 <- ggplot(residdf, aes(x = .fitted, y = .resid)) + geom_point() +
    labs(x = '\nFitted values', y = '\n\n')
  
  fig3 <- ggplot(residdf) + stat_qq(aes(sample = .resid)) +
    labs(x = '\nTheoretical Quantiles', y = 'Sample Quantiles\n')
  
  fig4 <- ggplot(residdf, aes(x = .resid)) + geom_histogram(aes(y=..density..), colour = 'grey50') +
    labs(x = '\nResiduals', y = 'Frequency\n') + scale_y_continuous(expand = c(0, 0)) +
    stat_function(fun = dnorm, color = "red", args = list(mean = mean(residdf$.resid), 
                                                          sd = sd(residdf$.resid)))
  
  grid.draw(rbind(cbind(ggplotGrob(fig1), ggplotGrob(fig2), size = 'first'),
                  cbind(ggplotGrob(fig3), ggplotGrob(fig4), size = 'first'), size = 'first'))
  
  return(shapiro.test(residdf$.resid))
}