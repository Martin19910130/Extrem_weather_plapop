library(tidyverse)
library(ggthemes)
source('plot_binned_prop.R')

# HUFZ/BL Demographic data -----------------------------------------------------

# format UFZ/BL data
ufz   <- read.csv('demo_dat.csv') %>% 
           select( -transition ) %>% 
           rename( transition = years ) %>% 
           mutate( year = gsub(' - [0-9]{4}','',transition) ) %>% 
           mutate( year = as.numeric(year) + 1 ) %>% 
           select( plant_id, size_t0, size_t1, surv, 
                   site, year, transition ) %>% 
           rename( survival_t1 = surv ) %>%  
           mutate( duration    = (year - min(year)) - 1,
                   log_size_t0 = log(size_t0),
                   log_size_t1 = log(size_t1) ) %>% 
           mutate( site = replace(site, site == 'Halle', 'HUFZ') ) %>% 
           mutate( site = replace(site, site == "Bad Lauchst?dt", 'BL') )

write.csv(ufz, 'results/clean_data/demo_HUFZ_BL.csv', row.names=F)


# Collate abundance data
n_t00 <- subset( ufz, !is.na(size_t0) ) %>% 
          count( year, site ) %>% 
          arrange( site, year )
n_t01 <- subset( ufz, !is.na(size_t1) ) %>% 
          count( year, site ) %>% 
          arrange( site, year ) %>% 
          subset( year == 2021 ) %>% 
          mutate( year = year + 1 )

# use abundance data to create "transitions"
n_t0  <- bind_rows( n_t00, n_t01 ) %>% rename( n_t0 = n ) %>% arrange( site, year ) 
n_t1  <- bind_rows( n_t00, n_t01 ) %>% 
          rename( n_t1 = n ) %>% 
          mutate( year = year - 1 ) %>% 
          arrange( site, year ) 
n_df  <- full_join( n_t0, n_t1 ) %>% 
           arrange( site, year ) %>% 
           mutate( gr       = n_t1 / n_t0 )  %>%  
           mutate( duration   = (year - min(year)) - 1,
                   transition = paste0(year-1,' - ',year) ) 

write.csv(n_df, 'results/clean_data/n_HUFZ_BL.csv', row.names=F)

# growth rate plot
ggplot(n_df) +
  geom_line( aes(year,gr,
                 group = site,
                 color = site) ) +
  theme_minimal() +
  scale_color_colorblind()

# total abundance plot
ggplot(n_df) +
  geom_line( aes(year,n_t1,
                 group = site,
                 color = site) ) +
  theme_minimal() +
  scale_color_colorblind()
