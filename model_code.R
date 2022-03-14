##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##          Run models with the plant pop net data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)

## load function for the bins (Thank you Aldo!)
df_binned_prop <- function(df, n_bins, siz_var, rsp_var)
{
  
  size_var <- deparse( substitute(siz_var) )
  resp_var <- deparse( substitute(rsp_var) )
  
  min_size <- min(log(df[,size_var]), na.rm = T) - 0.000001
  max_size <- max(log(df[,size_var]), na.rm = T) + 0.000001
  
  # binned survival probabilities
  h    <- (max_size - min_size) / n_bins
  lwr  <- min_size + (h*c(0:(n_bins-1)))
  upr  <- lwr + h
  mid  <- lwr + (1/2*h)
  
  binned_prop <- function(lwr_x, upr_x, response){
    
    id  <- which(log(df[,size_var]) > lwr_x & log(df[,size_var]) < upr_x)
    tmp <- df[id,]
    
    if( response == 'prob' ){   return( sum(tmp[,resp_var],na.rm=T) / nrow(tmp) ) }
    if( response == 'n_size' ){ return( nrow(tmp) ) }
    
  }
  
  y_binned <- Map(binned_prop, lwr, upr, 'prob') %>% unlist
  x_binned <- mid
  y_n_size <- Map(binned_prop, lwr, upr, 'n_size') %>% unlist
  
  data.frame( x = x_binned,
              y = y_binned )
}

## load data
dat <- read.csv("https://raw.githubusercontent.com/Martin19910130/Extrem_weather_plapop/main/Data/demo_dat.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    survival model
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site <- unique(dat$site)
dat_list <- c()

## easier for me to understand if we split everything into the two different sites
for(i in 1:length(site))
{
  dat_list[i] <- list(subset(dat, site == site[i]))
  
  names(dat_list)[i] <- site[i]
}

### Models 
surv_mod <- c()
size_mod <- c()
flow_mod <- c()
nrfl_mod <- c()

for(i in 1:length(dat_list))
{
  ## survival
  surv_mod[i] <- list(glm(surv ~ log(size_t0) * transition, data = dat_list[[i]], family = "binomial"))
  names(surv_mod)[i] <- names(dat_list)[i]
  
  ## growth
  size_mod[i] <- list(lm(log(size_t1) ~ log(size_t0) * transition, data = dat_list[[i]]))
  names(size_mod)[i] <- names(dat_list)[i]
  
  ## flower probability
  flow_mod[i] <- list(glm(pr_fl ~ log(size_t0) * transition, data = dat_list[[i]], family = "binomial"))
  names(flow_mod)[i] <- names(dat_list)[i]
  
  ## number of flowers
  nrfl_mod[i] <- list(subset(dat_list[[i]], pr_fl > 0) %>% glm(no_fl ~ log(size_t0), data = ., family = "poisson"))
  names(flow_mod)[i] <- names(dat_list)[i]
}

### produce ggplots

ggplot(dat, aes(x = log(size_t0), y = surv)) + geom_point() + facet_wrap(~ site + transition) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

ggplot(dat, aes(x = log(size_t0), y = log(size_t1))) + geom_point() + facet_wrap(~ site + transition) + geom_smooth(method = "lm") + 
  theme_bw()

ggplot(dat, aes(x = log(size_t0), y = pr_fl)) + geom_point() + facet_wrap(~ site + transition) + theme_bw() + 
  geom_smooth(method = glm, method.args = list(family = "binomial"), se = F)  

ggplot(subset(dat, pr_fl > 0), aes(x = log(size_t0), y = no_fl)) + geom_point() + theme_bw() + facet_wrap(~ site + transition) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = F)
