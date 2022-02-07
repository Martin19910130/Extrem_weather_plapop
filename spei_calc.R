###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###             SPEI calculations                        ###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
rm(list = ls())
gc()

##### spi plus arrange data new
library(dplyr)
library(SPEI)


#setwd("C://Users/ma22buky/Documents/01_PhD/plantpopnet/Aldo_project/")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##              reorder data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### read data from sanne
dat <- read.csv("https://raw.githubusercontent.com/Martin19910130/Extrem_weather_plapop/main/CHELSA_clim_all_populations.csv")

## quick and dirty subset each for each climate variable
dat_pr <- subset(dat, variable == "pr" )[,c("Latitude", "Longitude", "pop", 
                                            "year", "month", "value")] %>% 
  rename(pr = value)

dat_tas <- subset(dat, variable == "tas" )[,c("Latitude", "Longitude", "pop", 
                                              "year", "month", "value")] %>% 
  rename(tas = value)

dat_tasmin <- subset(dat, variable == "tasmin" )[,c("Latitude", "Longitude", "pop", 
                                                    "year", "month", "value")] %>% 
  rename(tasmin = value)

subset(dat, variable != "pr")$value %>% boxplot()

dat_tasmax <- subset(dat, variable == "tasmax" )[,c("Latitude", "Longitude", "pop", 
                                                    "year", "month", "value")] %>% 
  rename(tasmax = value)

## combine the temperature data sets
## precipitation needs special care as it is 
## a few rows short (2019 Jul - Dec missing)
dat_all <- cbind(dat_tas, dat_tasmin$tasmin, dat_tasmax$tasmax) %>% 
  rename(tasmin = 'dat_tasmin$tasmin', tasmax = 'dat_tasmax$tasmax')


## join the precipitation data set
dat_all <- full_join(dat_all, dat_pr, by = c("Longitude", "Latitude", "pop", 
                                             "year", "month"))

## divide the climate data by 100 
dat_all[,c("tasmin", "tasmax", "tas", "pr")] <- dat_all[,c("tasmin", "tasmax", "tas", "pr")]/10 - 273


## kick the na sites frome the dataset
dat_all <- subset(dat_all, !is.na(Longitude))

## write the data frame
#write.csv(dat_all, 
 #         "C://Users/ma22buky/Documents/01_PhD/plantpopnet/Aldo_project/Data/climate_reorganized.csv")

## order the data after site, year and month (month is automatically ordered)
dat_all <- dat_all[order(dat_all$pop, dat_all$year),]

## include box plot to check temperature distribution
boxplot(dat_all$tas)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##            SPEI calculations
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## subset every site into a list entry and calculate for each site individually the SPEI
pop <- unique(dat_all$pop)
list_dat <- c()

for(i in 1:length(pop))
{
  
  ## save current site as suber and use it in the subset comment
  suber <- pop[i]
  
  ## subset
  list_dat[i] <- list(subset(dat_all, pop == suber))
  
  ## name the list entry like the site
  names(list_dat)[i] <- pop[i]
}

## for loop to calculuate: PET, BAL and spei (12 month and 6 month scale)
for(i in 1:length(list_dat))
{
  ## calculate the PET using thornthwaite
  list_dat[[i]]$PET <- thornthwaite(list_dat[[i]]$tas, unique(list_dat[[i]]$Latitude))
  ## get the balance
  list_dat[[i]]$BAL <- list_dat[[i]]$pr - list_dat[[i]]$PET
  
  ## calculate the SPEI for 12 month scale and 6 month scale
  list_dat[[i]]$spei12 <- spei(list_dat[[i]]$BAL, 12, na.rm = T)$fitted
  list_dat[[i]]$spei06 <- spei(list_dat[[i]]$BAL, 6, na.rm = T)$fitted
}

## unlist the data and subset for the two years we want. 
dat_spei <- bind_rows(list_dat)
plot_dat <- subset(dat_spei, year >= 2018)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Get the information about which site is at which continent and as another option for country
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  #if(scale == "continents")
  indices$REGION   # returns the continent (7 continent model)
  #if(scale == "country")
  #indices$ADMIN  #returns country name
}
lat_lon <- unique(dat_all[,c("Latitude", "Longitude")])
lat_lon$continents <- as.character(coords2continent(lat_lon[,c(2,1)]))
lat_lon$countries <- as.character(coords2continent(lat_lon[,c(2,1)]))

## 4 turned out as NA so I checked and add them by hand
lat_lon[which(lat_lon$Longitude == 10.295065),3] <- "Europe"
lat_lon[which(lat_lon$Longitude == -121.073780), 3] <- "North America"
lat_lon[which(lat_lon$Longitude == 21.959783), 3] <- "Europe"
lat_lon[which(lat_lon$Longitude == -7.617440), 3] <- "Europe"

for(i in 1:nrow(plot_dat))
  for(j in 1:nrow(lat_lon))
  {
    if(lat_lon[j, "Latitude"] == plot_dat[i, "Latitude"] &
       lat_lon[j, "Longitude"] == plot_dat[i, "Longitude"])
    {
      plot_dat[i, "continent"] <- lat_lon[j, "continents"]
      #plot_dat[i, "country"] <- lat_lon[j, "countries"]
    }
  }

#write.csv(x = plot_dat, file = "Data/Spei_dat.csv")

spei_12 <- ggplot(subset(plot_dat, !(pop %in% c("HUFZ", "BL"))), aes(y = spei12, x = month, color = continent, group = pop)) + 
  geom_line(alpha = 0.5) +  
  geom_line(data = subset(plot_dat, pop == "HUFZ"), mapping = aes(size = 1, color = pop), color = "blue")  + 
  geom_line(data = subset(plot_dat, pop == "BL"), mapping = aes(size = 1, color = pop), color = "orange")  + theme_bw() + 
  facet_wrap(~year) + scale_x_continuous(breaks = 1:12, labels = c(month.abb[1:12])) + 
  theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank()) + ylab("SPEI (scale = 12 month)") +
  scale_size_continuous(guide = "none") + ggthemes::scale_color_colorblind()


spei_6 <- ggplot(subset(plot_dat, !(pop %in% c("HUFZ", "BL"))), aes(y = spei06, x = month, color = continent, group = pop)) + 
  geom_line(alpha = 0.5) +  
  geom_line(data = subset(plot_dat, pop == "HUFZ"), mapping = aes(size = 1, color = pop), color = "blue")  + 
  geom_line(data = subset(plot_dat, pop == "BL"), mapping = aes(size = 1, color = pop), color = "orange") +
  theme_bw() + 
  facet_wrap(~year) + scale_x_continuous(breaks = 1:12, labels = c(month.abb[1:12])) + 
  theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank()) + ylab("SPEI (scale = 6 month)") +
  scale_size_continuous(guide = "none") + ggthemes::scale_color_colorblind()

spei_all <- ggpubr::ggarrange(spei_6, spei_12, common.legend = T, nrow = 2)

ggsave("Spei_all.jpeg", 
       device = "jpeg")
getwd()
