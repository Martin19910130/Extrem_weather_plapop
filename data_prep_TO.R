library(tidyverse)
library(ggthemes)
source('plot_binned_prop.R')

# TO ---------------------------------------------------------------

dir   <- 'C:/Users/ac22qawo/Dropbox/PlantPopNet_extreme_weather/TO/'
to_v  <- list.files( dir ) %>% grep('.csv',.,value=T)
to_yr <- regmatches(to_v, 
                    gregexpr("[[:digit:]]{4}", 
                    to_v) ) %>% 
             unlist %>% 
             as.numeric

# format the TO data
format_to <- function( x, yr, time_step ){
  
  print( yr )
  
  read_mat <-  paste0(dir, x) %>% read.csv 
  
  name_v   <- names(read_mat)
  
  demo_df  <- read_mat %>% 
                # change names
                rename( size           = no_leaves,
                        leaf_length    = leaf_length,
                        leaf_width     = leaf_width,
                        no_fl          = no_fl_stems,
                        fl_infl_height = inflor_length,
                        SL             = number_seedlings ) %>% 
                mutate( survival       = replace(survival, 
                                                 survival == 'dead',
                                                 0) ) %>% 
                mutate( survival       = replace(survival, 
                                                 survival == 'survived',
                                                 1) ) %>% 
                subset( !grepl("/", plant_id ) ) %>% 
                mutate( plant_id = as.numeric( plant_id) )
  
  rm_id    <- which( names(demo_df) %in% c("transect",
                                           "transect_Lat_start", 
                                           "transect_Lon_start", 
                                           "transect_Lat_stop", 
                                           "transect_Lon_stop",
                                           "suspected_clone", 
                                           "no_rosettes", "rosette_number",
                                           "note",
                                           "powdery_mildew", "other_disease_or_herbivory",
                                           "disease (yes/no)", "disease_comments",
                                           "herbivory (yes/no)", "herbivory_comments",
                                           "General observations",
                                           "not present in last year's data sheet") )
  
  if( sum(rm_id) == 0 ) rm_id=NULL
                
  transition_vars <- c( "SL", "survival", "size", 
                        "leaf_length", "leaf_width", "no_fl",
                        "fl_stem_height", "fl_infl_height", "inflor_phenology",
                        "powdery_mildew", "other_disease_or_herbivory" )
    
  # if data frame refers to time t0
  if( time_step == 't0' ){  
      
    add_v <- rep('', ncol(demo_df) )
    add_v <- replace( add_v, names(demo_df) %in% transition_vars, '_t0' )
    
    demo_df <- demo_df %>% 
                 mutate_at( transition_vars, as.numeric ) %>% 
                 setNames( paste0( names(demo_df), add_v) ) %>% 
                 mutate( year = yr + 1 ) 
    
  }

  # if data frame refers to time t0
  if( time_step == 't1' ){  
    
    add_v <- rep('', ncol(demo_df) )
    add_v <- replace( add_v, names(demo_df) %in% transition_vars, '_t1' )
    
    demo_df <- demo_df %>% 
      mutate_at( transition_vars, as.numeric ) %>% 
      setNames( paste0( names(demo_df), add_v) ) %>% 
      mutate( year = yr ) 
    
  }

  if( sum(rm_id) > 0 ){
    
    demo_df <- demo_df[,-rm_id]
    
  }
  
  return( demo_df )

}
    
# two separate transition files
to_t0   <- Map( format_to, to_v, to_yr, 't0' ) %>% bind_rows %>% select(-X)
to_t1   <- Map( format_to, to_v, to_yr, 't1' ) %>% bind_rows %>% select(-X)
full_df <- full_join( to_t0,
                      to_t1 ) %>% 
             # log the sizes
             mutate( log_size_t0 = log(size_t0),
                     log_size_t1 = log(size_t1) ) %>% 
             # remove individuals that area already dead
             mutate( surv_t0_t1  = survival_t0 + survival_t1 ) %>% 
             subset( !(surv_t0_t1 %in% 0) ) %>% 
             mutate( transition = paste(year-1, '-', year),
                     site       = 'TO' ) %>% 
             mutate( duration   = (year - min(year)) - 1 )

write.csv(full_df, 'results/clean_data/demo_TO.csv', row.names=F)

# growth
full_df %>% 
  subset( !(year %in% c(2015,2019)) ) %>% 
  mutate( year = as.factor(year) ) %>% 
  ggplot( ) +
  geom_jitter( aes(log_size_t0,
                   log_size_t1,
                   group = year,
                   color = year,
                   height = 1,
                   width = 1) ) +
  theme_minimal() +
  scale_color_colorblind()

# survival
list( subset(full_df, year == 2016) %>% 
        plot_binned_prop( 10, log_size_t0, survival_t1 ) %>% mutate( year = 2016),
      subset(full_df, year == 2017) %>% 
        plot_binned_prop( 10, log_size_t0, survival_t1 ) %>% mutate( year = 2017),
      subset(full_df, year == 2018) %>% 
        plot_binned_prop( 10, log_size_t0, survival_t1 ) %>% mutate( year = 2018)
      ) %>% 
  bind_rows %>% 
  mutate( year = as.factor(year) ) %>% 
  ggplot( ) +
  geom_point( aes(x,y,size=n_size,
                  group = year,
                  color = year ) ) +
  ylim(0,1) +
  theme_minimal() +
  scale_color_colorblind()

# pop growth rate
n_t0 <- full_df %>% 
          select( plot, year, plant_id, 
                  x_coord, y_coord, size_t0 ) %>% 
          subset( !is.na(size_t0) ) %>% 
          count( year ) %>% 
          rename( n_t0 = n ) 

n_t1 <- n_t0 %>% 
          mutate( year = year - 1 ) %>% 
          rename( n_t1 = n_t0 )
          
n_df <- full_join( n_t0, n_t1 ) %>% 
          mutate( gr = n_t1 / n_t0 ) %>% 
          arrange( year ) %>% 
          mutate( transition = paste(year-1, '-', year),
                  site       = 'TO' ) %>% 
          mutate( duration   = (year - min(year)) - 1 )
  
write.csv(n_df, 'results/clean_data/n_TO.csv', row.names=F)

# growth rate plot
ggplot(n_df) +
  geom_line( aes(year,gr) ) +
  theme_minimal()

# total abundance plot
ggplot(n_df) +
  geom_line( aes(year,n_t1) ) +
  theme_minimal()
