library(tidyverse)
library(ggthemes)
library(bbmle)
library(lme4)

# quote a series of bare names
quote_bare <- function( ... ){
  substitute( alist(...) ) %>% 
    eval( ) %>% 
    sapply( deparse )
}

# SPEI data
spei_df <- read.csv('spei_clim_analyses.csv') %>% 
            subset( pop %in% c('LK1','TX','HUFZ','BL','TO') ) %>%
            mutate( pop = replace(pop, pop == 'TX', 'RUSC') ) %>%
            subset( month %in% c(4,6) ) %>%
            subset( !(month %in% 4 & pop %in% c('LK1','HUFZ','BL','TO')) ) %>%
            subset( !(month %in% 6 & pop %in% 'RUSC') ) %>%
            rename( site = pop ) %>% 
            select( site, year, spei06, spei12 )

spei_lag <- spei_df %>% 
              mutate( year = year + 1 ) %>% 
              rename( spei12_lag = spei12,
                      spei06_lag = spei06 )

# Demographic data -------------------------------------------------------------
col_v  <- quote_bare( site, year, transition, duration, plant_id, 
                      size_t0, size_t1, log_size_t0, log_size_t1, 
                      survival_t1 )
ufz_bl  <- read.csv('results/clean_data/demo_HUFZ_BL.csv') %>% select( all_of(col_v) )
lk1     <- read.csv('results/clean_data/demo_LK1.csv') %>% select( all_of(col_v) )
rusc    <- read.csv('results/clean_data/demo_RUSC.csv') %>% select( all_of(col_v) )
to      <- read.csv('results/clean_data/demo_TO.csv') %>% select( all_of(col_v) )
demo_df <- list(ufz_bl, lk1, rusc, to ) %>%
              bind_rows %>% 
              left_join( spei_df ) %>% 
              left_join( spei_lag )
              

# Abundance data ---------------------------------------------------------------
ufz_bl  <- read.csv('results/clean_data/n_HUFZ_BL.csv') 
lk1     <- read.csv('results/clean_data/n_LK1.csv') 
rusc    <- read.csv('results/clean_data/n_RUSC.csv') 
to      <- read.csv('results/clean_data/n_TO.csv') 
n_df    <- list(ufz_bl, lk1, rusc, to ) %>%
             bind_rows %>% 
             arrange( site, year ) %>% 
             left_join( spei_df ) %>% 
             left_join( spei_lag ) %>% 
             mutate( log_gr = log(gr) )

# Growth rate models -----------------------------------------------------------

# simple linear models
mod0  <- lm( log_gr ~ 1, data = n_df)
mod1  <- lm( log_gr ~ duration, data = n_df)
mod2  <- lm( log_gr ~ spei12, data = n_df)
mod3  <- lm( log_gr ~ duration + spei12, data = n_df)
mod_l <- list( mod0, mod1, mod2, mod3 ) %>% 
            setNames( c('NULL','duration',
                        'spei12',
                        'durationAndSpei12') ) 

write.csv( AICtab(mod_l , weights = T),
           'results/loggrowthrate_mod_sel.csv' )

# graphs for growth rate
n_df %>% 
  ggplot( ) +
  geom_line( aes(year, log_gr,
                 group = site,
                 color = site),
             lwd = 2 ) +
  theme_minimal() +
  scale_color_colorblind()

# duration
p_gr_dur <- n_df %>% 
  ggplot( ) +
  geom_line( aes(duration, log_gr,
                 group = site,
                 color = site),
             lwd = 2 ) +
  theme_minimal() +
  scale_color_colorblind() +
  labs( x = 'Census duration (years)',
        y = 'log(growth rate)' ) +
  theme( axis.text  = element_text( size = 10),
         axis.title = element_text( size = 15) )

# SPEI12
p_gr_spei <- n_df %>% 
  ggplot( ) +
  geom_point( aes(spei12, log_gr,
                 group = site,
                 color = site),
             lwd = 2 ) +
  theme_minimal() +
  scale_color_colorblind() +
  labs( x = 'SPEI (12 months)',
        y = 'log(growth rate)' ) +
  theme( axis.text  = element_text( size = 10),
         axis.title = element_text( size = 15) )

out_p <- gridExtra::grid.arrange(p_gr_dur, 
                                 p_gr_spei)
                        
ggsave( 'results/gr_vs_spei.tiff',
        out_p, 
        width = 5, height = 6.3,
        compression = 'lzw')

# growth models ------------------------------------------

# mixed effect models
mod0    <- lmer( log_size_t1 ~ (log_size_t0 | site) , data = demo_df)
mod1    <- lmer( log_size_t1 ~ (log_size_t0 | site) + duration, data = demo_df)
mod2    <- lmer( log_size_t1 ~ (log_size_t0 | site) + spei12, data = demo_df)
mod3    <- lmer( log_size_t1 ~ (log_size_t0 | site) + duration + spei12, data = demo_df)
mod_l   <- list( mod0, mod1, mod2, mod3 ) %>% 
              setNames( c('NULL','duration',
                          'spei12',
                          'durationAndSpei12') ) 
  
write.csv( AICtab(mod_l , weights = T),
           'results/growth_mod_sel.csv' )
  
# growth plot
p_growth <- ggplot(demo_df) +
              geom_point( aes(log_size_t0,
                              log_size_t1,
                              color = spei12),
                          size = 3,
                          alpha = 0.8 ) +
              geom_abline( aes(intercept = 0,
                               slope     = 1), 
                           lwd = 1, lty = 2 ) +
              theme_minimal() +
              scale_color_viridis_c() +
              theme( axis.text    = element_text( size = 10),
                     axis.title   = element_text( size = 15),
                     legend.text  = element_text( size = 15),
                     legend.title = element_text( size = 15) )
                          
ggsave( 'results/growth_by_spei12.tiff', p_growth, 
        width = 6.3, height = 6.3, compression = 'lzw' )


# Survival models ------------------------------------------

# mixed effect models
mod0 <- glmer( survival_t1 ~ (1 | site), family='binomial', data = demo_df)
mod1 <- glmer( survival_t1 ~ (1 | site) + duration, family='binomial', data = demo_df)
mod2 <- glmer( survival_t1 ~ (1 | site) + spei12, family='binomial', data = demo_df)
mod3 <- glmer( survival_t1 ~ (1 | site) + duration + spei12, family='binomial', data = demo_df)
mod_l   <- list( mod0, mod1, mod2, mod3 ) %>% 
            setNames( c('NULL','duration',
                        'spei12',
                        'durationAndSpei12') ) 

write.csv( AICtab(mod_l , weights = T),
           'results/survival_mod_sel.csv' )

# survival plot
p_surv <- demo_df %>% 
            subset( !is.na(log_size_t0) ) %>% 
            select( site, year, duration, spei12, survival_t1 ) %>% 
            drop_na %>% 
            group_by( site, year, duration, spei12 ) %>% 
            summarise( surv = sum(survival_t1) / n() ) %>% 
            ungroup %>% 
            ggplot() +
            geom_point( aes(spei12,surv),
                        size = 3 ) +
            ylim( 0, 1 ) +
            theme_minimal() +
            labs( x = 'SPEI (12 months)',
                  y = 'Survival rate' ) +
            theme( axis.text    = element_text( size = 10),
                   axis.title   = element_text( size = 15) )

ggsave( 'results/survival_by_spei12.tiff', p_surv, 
        width = 6.3, height = 6.3, compression = 'lzw' )
