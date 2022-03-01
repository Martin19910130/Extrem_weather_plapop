library(tidyverse)
library(ggthemes)
library(readxl)
source('plot_binned_prop.R')

# LK1 ---------------------------------------------------------------

read_xlsx( paste0(dir,'individual_plant_census_2021.xlsx') )

dir   <- 'C:/Users/ac22qawo/Dropbox/PlantPopNet_extreme_weather/LK1/'
lk1   <- read_xlsx( paste0(dir,'individual_plant_census_2021.xlsx') )
year1 <- c(1:12,13:27)
year2 <- c(1:12,28:41)
year3 <- c(1:12,42:56)
year4 <- c(1:12,57:71)
year5 <- c(1:12,72:86)
year6 <- c(1:12,87:101)
yr_l   <- list( year1, year2, year3,
                year4, year5, year6 )
lk1_yr <- c(2016,2017,2018,2019,2020,2021)


# format the TO data
format_lk1 <- function( yr_cols, yr, time_step ){
  
  print( yr )
  
  nam_v    <- lk1[,yr_cols] %>% names %>% gsub('...[0-9]{1,3}','',.)
  
  demo_df  <- lk1[,yr_cols] %>% 
                # change names
                setNames( nam_v ) %>% 
                rename( size           = no_leaves,
                        leaf_length    = leaf_length,
                        leaf_width     = leaf_width,
                        no_fl          = no_fl_stems,
                        fl_infl_height = inflor_length,
                        SL             = number_seedlings ) %>% 
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
                                           "General Observations",
                                           "not present in last year's data sheet") )
  
  if( sum(rm_id) == 0 ) rm_id=NULL
                
  transition_vars <- c( "SL", "survival", "size", 
                        "leaf_length", "leaf_width", "no_fl",
                        "fl_stem_height", "fl_infl_height", "inflor_phenology" )
    
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
to_t0   <- Map( format_lk1, yr_l, lk1_yr, 't0' ) %>% bind_rows 
to_t1   <- Map( format_lk1, yr_l, lk1_yr, 't1' ) %>% bind_rows 
full_df <- full_join( to_t0,
                      to_t1 ) %>% 
             # log the sizes
             mutate( log_size_t0 = log(size_t0),
                     log_size_t1 = log(size_t1) ) %>% 
             # remove individuals that area already dead
             mutate( transition = paste(year-1, '-', year),
                     site       = 'LK1' ) %>% 
             mutate( duration   = (year - min(year)) - 1 )
             
write.csv(full_df, 'results/clean_data/demo_LK1.csv', row.names=F)

# growth
full_df %>% 
  subset( !(year %in% c(2016,2022)) ) %>%
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

# # survival
# list( subset(full_df, year == 2016) %>% 
#         subset( log_size_t0 > 0.5 & log_size_t0 < 2.5 ) %>% 
#         plot_binned_prop( 4, log_size_t0, survival_t1 ) %>% mutate( year = 2016),
#       subset(full_df, year == 2017) %>% 
#         plot_binned_prop( 10, log_size_t0, survival_t1 ) %>% mutate( year = 2017),
#       subset(full_df, year == 2018) %>% 
#         plot_binned_prop( 10, log_size_t0, survival_t1 ) %>% mutate( year = 2018)
#       ) %>% 
#   bind_rows %>% 
#   mutate( year = as.factor(year) ) %>% 
#   ggplot( ) +
#   geom_point( aes(x,y,size=n_size,
#                   group = year,
#                   color = year ) ) +
#   ylim(0,1) +
#   theme_minimal() +
#   scale_color_colorblind()

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
                  site       = 'LK1' ) %>% 
          mutate( duration   = (year - min(year)) - 1 )
  
write.csv(n_df, 'results/clean_data/n_LK1.csv', row.names=F)

# growth rate plot
ggplot(n_df) +
  geom_line( aes(year,gr) ) +
  theme_minimal()

# total abundance plot
ggplot(n_df) +
  geom_line( aes(year,n_t1) ) +
  theme_minimal()
