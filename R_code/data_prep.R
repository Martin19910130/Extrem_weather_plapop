#### Plantpopnet data
rm(list = ls())
gc()

library(openxlsx)
library(dplyr)
library(ggplot2)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Bad Lauchstädt
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## read data and transpose the data
badL18 <- read.xlsx("C://Users/ma22buky/Documents/01_PhD/plantpopnet/Data_BL_GCEF/Individual_plant_census_BL_GCEF.xlsx", rowNames = T, colNames = F) %>% 
  t() %>% as.data.frame()
badL19 <- read.xlsx("C://Users/ma22buky/Documents/01_PhD/plantpopnet/Data_2020/Individual_plant_census_BL_GCEF_19.xlsx",
                    rowNames = F, colNames = T) %>% as.data.frame()
badL20 <- read.xlsx("C://Users/ma22buky/Documents/01_PhD/plantpopnet/Data_2020/Sampled_data_2020/individual_plant_census_2020.xlsx",
                    rowNames = T, colNames = F, sheet = 1) %>% t() %>% as.data.frame() %>% .[,-2] %>% subset(., !is.na(plant_id))
badL21 <- read.xlsx("C://Users/ma22buky/Documents/01_PhD/plantpopnet/ind_cens_BLGCEF21.xlsx", rowNames = T, colNames = F) %>% 
  t() %>% as.data.frame() %>% .[,-2]

## replace the plant id for one individual which didn't get one by 0 as place holder
badL21$plant_id[is.na(badL21$plant_id)] <- 0

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##      First transition
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
trans1 <- data.frame()

for(i in 1:nrow(badL18))
  for(j in 1:nrow(badL19))
  {
    ## check for the same plant id to add data together
    if(badL18[i,"plant_id"] == badL19[j, "plant_id"])
    {
      ## add the needed data to the data frame
      ## plant id and transition
      trans1[i, "plant_id"] <- badL18[i, "plant_id"]
      trans1[i, "transition"] <- 1
      
      ## sizes
      trans1[i, "size_t0"] <- as.numeric(badL18[i, "no_leaves"])
      trans1[i, "size_t1"] <- as.numeric(badL19[j, "no_leaves"])
      trans1[i, "leaf_length_t0"] <- as.numeric(badL18[i, "leaf_length"])
      trans1[i, "leaf_length_t1"] <- as.numeric(badL19[j, "leaf_length"])
      trans1[i, "leaf_width_t0"] <- as.numeric(badL18[i, "leaf_width"])
      trans1[i, "leaf_width_t1"] <- as.numeric(badL19[j, "leaf_width"])
      
      ## reproduction
      trans1[i, "no_fl"] <- as.numeric(badL18[i, "no_fl_stems"])
      trans1[i, "fl_stem_height"] <- as.numeric(badL18[i, "fl_stem_height"])
      trans1[i, "fl_infl_height"] <- as.numeric(badL18[i, "inflor_height"])
      trans1[i, "SL"] <- as.numeric(badL18[i, "number_seedlings"])
    }
    
  }

## add new individuals to the data frame as they are all the new plant ids in 2019
new_ind1 <- subset(badL19, !(plant_id %in% unique(badL18$plant_id))) %>% 
  .[,c("plant_id", "no_leaves", "leaf_length", "leaf_width", "no_fl_stems", "number_seedlings")] %>%
  data.frame(.,transition = 1, size_t0 = NA, leaf_length_t0 = NA, leaf_width_t0 = NA, fl_stem_height = NA, fl_infl_height = NA) %>% 
  rename(size_t1 = no_leaves,no_fl = no_fl_stems, SL = number_seedlings, leaf_width_t1 = leaf_width, leaf_length_t1 = leaf_length)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##            Second transition
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tran2 <- data.frame()
for(i in 1:nrow(badL19))
  for(j in 1:nrow(badL20))
  {
    if(badL19[i, "plant_id"] == badL20[j, "plant_id"])
    {
      ## add the needed data to the data frame
      ## plant id and transition
      tran2[i, "plant_id"] <- badL19[i, "plant_id"]
      tran2[i, "transition"] <- 2
      
      ## sizes
      tran2[i, "size_t0"] <- as.numeric(badL19[i, "no_leaves"])
      tran2[i, "size_t1"] <- as.numeric(badL20[j, "no_leaves"])
      tran2[i, "leaf_length_t0"] <- as.numeric(badL19[i, "leaf_length"])
      tran2[i, "leaf_length_t1"] <- as.numeric(badL20[j, "leaf_length"])
      tran2[i, "leaf_width_t0"] <- as.numeric(badL19[i, "leaf_width"])
      tran2[i, "leaf_width_t1"] <- as.numeric(badL20[j, "leaf_width"])
      
      ## reproduction
      tran2[i, "no_fl"] <- as.numeric(badL19[i, "no_fl_stems"])
      tran2[i, "fl_stem_height"] <- as.numeric(badL19[i, "fl_stem_height"])
      tran2[i, "fl_infl_height"] <- as.numeric(badL19[i, "inflor_height"])
      tran2[i, "SL"] <- as.numeric(badL19[i, "number_seedlings"])
    }
  }

## add new individuals to the data frame as they are all the new plant ids in 2020
new_ind2 <- subset(badL20, !(plant_id %in% unique(badL19$plant_id))) %>% 
  .[,c("plant_id", "no_leaves", "leaf_length", "leaf_width", "no_fl_stems", "number_seedlings")] %>%
  data.frame(.,transition = 1, size_t0 = NA, leaf_length_t0 = NA, leaf_width_t0 = NA, fl_stem_height = NA, fl_infl_height = NA) %>% 
  rename(size_t1 = no_leaves,no_fl = no_fl_stems, SL = number_seedlings, leaf_width_t1 = leaf_width, leaf_length_t1 = leaf_length)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##          Third transition
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tran3 <- data.frame()

for(i in 1:nrow(badL20))
  for(j in 1:nrow(badL21))
  {
    if(badL20[i, "plant_id"] == badL21[j, "plant_id"])
    {
      ## add the needed data to the data frame
      ## plant id and transition
      tran3[i, "plant_id"] <- badL20[i, "plant_id"]
      tran3[i, "transition"] <- 3
      
      ## sizes
      tran3[i, "size_t0"] <- as.numeric(badL20[i, "no_leaves"])
      tran3[i, "size_t1"] <- as.numeric(badL21[j, "no_leaves"])
      tran3[i, "leaf_length_t0"] <- as.numeric(badL20[i, "leaf_length"])
      tran3[i, "leaf_length_t1"] <- as.numeric(badL21[j, "leaf_length"])
      tran3[i, "leaf_width_t0"] <- as.numeric(badL20[i, "leaf_width"])
      tran3[i, "leaf_width_t1"] <- as.numeric(badL21[j, "leaf_width"])
      
      ## reproduction
      tran3[i, "no_fl"] <- as.numeric(badL20[i, "no_fl_stems"])
      tran3[i, "fl_stem_height"] <- as.numeric(badL20[i, "fl_stem_height"])
      tran3[i, "fl_infl_height"] <- as.numeric(badL20[i, "inflor_height"])
      tran3[i, "SL"] <- badL20[i, "number_seedlings"]
    }
  }

## add new individuals to the data frame as they are all the new plant ids in 2021
new_ind3 <- subset(badL21, !(plant_id %in% unique(badL19$plant_id))) %>% 
  .[,c("plant_id", "no_leaves", "leaf_length", "leaf_width", "no_fl_stems", "Seedling")] %>%
  data.frame(.,transition = 1, size_t0 = NA, leaf_length_t0 = NA, leaf_width_t0 = NA, fl_stem_height = NA, fl_infl_height = NA) %>% 
  rename(size_t1 = no_leaves,no_fl = no_fl_stems, SL = Seedling, leaf_width_t1 = leaf_width, leaf_length_t1 = leaf_length)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## combine data sets and add information
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat <- rbind(trans1, new_ind1, tran2, new_ind2, tran3, new_ind3)

## change the columns to numeric, as some are characater
dat <- sapply(dat, as.numeric) %>% as.data.frame()

## add survival
dat$surv <- ifelse(!is.na(dat$size_t0) & !is.na(dat$size_t1), 1, 0)

## change na's in number of flowers to 0. Keep in mind that that also translates the new individuals in 0 flowers
## It shouldn't matter as th esize t0 will be NA for those cases
dat$no_fl[is.na(dat$no_fl)] <- 0
dat$pr_fl <- ifelse(dat$no_fl > 0 , 1, 0)

## delete the ones where plant_id is missing as those are empty rows and columns in this case
dat <- subset(dat, !is.na(plant_id))

## add the years of the transition
dat$years <- ifelse(dat$transition == 1, "2018 - 2019", 
                    ifelse(dat$transition == 2, "2019 - 2020", "2020 - 2021"))

## add a column which tells us the site
dat$site <- "Bad Lauchstädt"

### how many survivors did we have in each transition 
bad_surv <- ggplot(dat, aes(x = log(size_t0), y = surv)) + geom_point() + facet_wrap(~ years) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) + theme_bw() + 
  labs(title = "Bad Lauchstädt", x = "Log size t0", y = "survival")

bad_grow <- ggplot(dat, aes(x = log(size_t0), y = log(size_t1))) + geom_point() + facet_wrap(~ years) + theme_bw() + geom_smooth(method = "lm", se = F) + 
  labs(title = "Bad Lauchstädt", x = "Log size t0", y = "Log size t1")

bad_flow <- ggplot(dat, aes(x = log(size_t0), y = pr_fl)) + geom_point() + theme_bw() + facet_wrap(~ years) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) + 
  labs(title = "Bad Lauchstädt", x = "Log size t0", y = "Probability of flowering")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##      Halle UFZ 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## read data
Hufz18 <- read.xlsx("C://Users/ma22buky/Documents/01_PhD/plantpopnet/Data_HUFZ/Individual_plant_census_HUFZ.xlsx", colNames = F, rowNames = T) %>%
  t() %>% data.frame()
Hufz19 <- read.xlsx("C://users/ma22buky/Documents/01_PhD/plantpopnet/Data_2020/Individual_plant_census_HUFZ_19.xlsx", colNames = T) %>% data.frame()
Hufz20 <- read.xlsx("C://Users/ma22buky/Documents/01_PhD/plantpopnet/Data_2020/Sampled_data_2020/individual_plant_census_2020.xlsx", sheet = 2, 
                    rowNames = T, colNames = F) %>% t() %>% data.frame()
Hufz21 <- read.xlsx("C://Users/ma22buky/Documents/01_PhD/plantpopnet/ind_cens_HUFZ2021.xlsx", colNames = F, rowNames = T) %>% 
  t() %>% data.frame()

## replace the plant id for one individual which didn't get one by 0 as place holder
Hufz21$plant_id[is.na(Hufz21$plant_id)] <- 0

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##      Transition 1
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Hufz_tran1 <- data.frame()

for(i in 1:nrow(Hufz18))
  for(j in 1:nrow(Hufz19))
  {
    if(as.numeric(Hufz18[i, "plant_id"]) == Hufz19[j, "plant_id"])
    {
      ## plant id and transition
      Hufz_tran1[i, "plant_id"] <- as.numeric(Hufz18[i, "plant_id"]) 
      Hufz_tran1[i, "transition"] <- 1
      
      ## sizes
      Hufz_tran1[i, "size_t0"] <- as.numeric(Hufz18[i, "no_leaves"])
      Hufz_tran1[i, "size_t1"] <- as.numeric(Hufz19[j, "no_leaves"])
      Hufz_tran1[i, "leaf_length_t0"] <- as.numeric(Hufz18[i, "leaf_length"])
      Hufz_tran1[i, "leaf_length_t1"] <- as.numeric(Hufz19[j, "leaf_length"])
      Hufz_tran1[i, "leaf_width_t0"] <- as.numeric(Hufz18[i, "leaf_width"])
      Hufz_tran1[i, "leaf_width_t1"] <- as.numeric(Hufz19[j, "leaf_width"])
      
      ## reproduction
      Hufz_tran1[i, "no_fl"] <- as.numeric(Hufz18[i, "no_fl_stems"])
      Hufz_tran1[i, "fl_stem_height"] <- as.numeric(Hufz18[i, "fl_stem_height"])
      Hufz_tran1[i, "fl_infl_height"] <- as.numeric(Hufz18[i, "inflor_length"])
      Hufz_tran1[i, "SL"] <- as.numeric(Hufz18[i, "number_seedlings"])
    }
  }

## add new individuals to the data frame as they are all the new plant ids in 2019
Halle_new_ind1 <- subset(Hufz19, !(plant_id %in% unique(badL19$plant_id))) %>% 
  .[,c("plant_id", "no_leaves", "leaf_length", "leaf_width", "no_fl_stems", "number_seedlings")] %>%
  data.frame(.,transition = 1, size_t0 = NA, leaf_length_t0 = NA, leaf_width_t0 = NA, fl_stem_height = NA, fl_infl_height = NA) %>% 
  rename(size_t1 = no_leaves,no_fl = no_fl_stems, SL = number_seedlings, leaf_width_t1 = leaf_width, leaf_length_t1 = leaf_length)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##          Transition 2
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Hufz_tran2 <- data.frame()
for(i in 1:nrow(Hufz19))
  for(j in 1:nrow(Hufz20))
  {
    if(Hufz19[i, "plant_id"] == Hufz20[j, "plant_id"])
    {
      ## transition and plant id
      Hufz_tran2[i, "plant_id"] <- Hufz19[i, "plant_id"] 
      Hufz_tran2[i, "transition"] <- 2
      
      ## sizes
      Hufz_tran2[i, "size_t0"] <- as.numeric(Hufz19[i, "no_leaves"])
      Hufz_tran2[i, "size_t1"] <- as.numeric(Hufz20[j, "no_leaves"])
      Hufz_tran2[i, "leaf_length_t0"] <- as.numeric(Hufz19[i, "leaf_length"])
      Hufz_tran2[i, "leaf_length_t1"] <- as.numeric(Hufz20[j, "leaf_length"])
      Hufz_tran2[i, "leaf_width_t0"] <- as.numeric(Hufz19[i, "leaf_width"])
      Hufz_tran2[i, "leaf_width_t1"] <- as.numeric(Hufz20[j, "leaf_width"])
      
      ## reproduction
      Hufz_tran2[i, "no_fl"] <- as.numeric(Hufz19[i, "no_fl_stems"])
      Hufz_tran2[i, "fl_stem_height"] <- as.numeric(Hufz19[i, "fl_stem_height"])
      Hufz_tran2[i, "fl_infl_height"] <- as.numeric(Hufz19[i, "inflor_height"])
      Hufz_tran2[i, "SL"] <- as.numeric(Hufz19[i, "number_seedlings"])
    }
  }

## add new individuals to the data frame as they are all the new plant ids in 2020
Halle_new_ind2 <- subset(Hufz20, !(plant_id %in% unique(badL19$plant_id))) %>% 
  .[,c("plant_id", "no_leaves", "leaf_length", "leaf_width", "no_fl_stems", "number_seedlings")] %>%
  data.frame(.,transition = 1, size_t0 = NA, leaf_length_t0 = NA, leaf_width_t0 = NA, fl_stem_height = NA, fl_infl_height = NA) %>% 
  rename(size_t1 = no_leaves,no_fl = no_fl_stems, SL = number_seedlings, leaf_width_t1 = leaf_width, leaf_length_t1 = leaf_length)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Transition 3
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Hufz_tran3 <- data.frame()
for(i in 1:nrow(Hufz20))
  for(j in 1:nrow(Hufz21))
  {
    if(as.numeric(Hufz20[i, "plant_id"]) == as.numeric(Hufz21[j, "plant_id"]))
    {
      ## plant id and transition
      Hufz_tran3[i, "plant_id"] <- as.numeric(Hufz20[i, "plant_id"]) 
      Hufz_tran3[i, "transition"] <- 3
      
      ## sizes
      Hufz_tran3[i, "size_t0"] <- as.numeric(Hufz20[i, "no_leaves"])
      Hufz_tran3[i, "size_t1"] <- as.numeric(Hufz21[j, "no_leaves"])
      Hufz_tran3[i, "leaf_length_t0"] <- as.numeric(Hufz20[i, "leaf_length"])
      Hufz_tran3[i, "leaf_length_t1"] <- as.numeric(Hufz21[j, "leaf_length"])
      Hufz_tran3[i, "leaf_width_t0"] <- as.numeric(Hufz20[i, "leaf_width"])
      Hufz_tran3[i, "leaf_width_t1"] <- as.numeric(Hufz21[j, "leaf_width"])
      
      ## reproduction
      Hufz_tran3[i, "no_fl"] <- as.numeric(Hufz20[i, "no_fl_stems"])
      Hufz_tran3[i, "fl_stem_height"] <- as.numeric(Hufz20[i, "fl_stem_height"])
      Hufz_tran3[i, "fl_infl_height"] <- as.numeric(Hufz20[i, "inflor_height"])
      Hufz_tran3[i, "SL"] <- as.numeric(Hufz20[i, "number_seedlings"])
    }
  }

## add new individuals to the data frame as they are all the new plant ids in 2020
Halle_new_ind3 <- subset(Hufz21, !(plant_id %in% unique(badL19$plant_id))) %>% 
  .[,c("plant_id", "no_leaves", "leaf_length", "leaf_width", "no_fl_stems", "number_seedlings")] %>%
  data.frame(.,transition = 1, size_t0 = NA, leaf_length_t0 = NA, leaf_width_t0 = NA, fl_stem_height = NA, fl_infl_height = NA) %>% 
  rename(size_t1 = no_leaves,no_fl = no_fl_stems, SL = number_seedlings, leaf_width_t1 = leaf_width, leaf_length_t1 = leaf_length)

## combine data frames
dat_halle <- rbind(Hufz_tran1, Halle_new_ind1, Hufz_tran2, Halle_new_ind2, Hufz_tran3, Halle_new_ind3) %>% subset(., !is.na(plant_id))

## make the columns numeric
dat_halle <- sapply(dat_halle, as.numeric) %>% as.data.frame()

## add survival 
dat_halle$surv <- ifelse(!is.na(dat_halle$size_t0) & !is.na(dat_halle$size_t1), 1, 0)

## add info on what transition actually means
dat_halle$years <- ifelse(dat_halle$transition == 1, "2018 - 2019", 
                          ifelse(dat_halle$transition == 2, "2019 - 2020", "2020 - 2021"))

## add column with site 
dat_halle$site <- "Halle"

## flower probability, replace nas in flowering column with 0. Same restriction as for Bad Lauchstädt
dat_halle$no_fl[is.na(dat_halle$no_fl)] <- 0
dat_halle$pr_fl <- ifelse(dat_halle$no_fl > 0, 1, 0)


## plots for halle
hal_surv <- ggplot(dat_halle, aes(x = log(size_t0), y = surv)) + geom_point() + theme_bw() + facet_wrap(~ years) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F) + labs(title = "Halle", y = "survival", 
                                                                                      x = "Log size t0")
hal_grow <- ggplot(dat_halle, aes(x = log(size_t0), y = log(size_t1))) + geom_point() + facet_wrap( ~ years) + theme_bw() +
  geom_smooth(method = "lm", se = F) + 
  labs(title = "Halle", y = "Log size t1", x = "Log size t0")

ggplot(dat_halle, aes)

surv_plot <- ggpubr::ggarrange(bad_surv, hal_surv, nrow = 2)
grow_plot <- ggpubr::ggarrange(bad_grow, hal_grow, nrow = 2)

ggsave("C://Users/ma22buky/Documents/01_PhD/plantpopnet/survival_plots.jpeg", 
       device = "jpeg", 
       plot = surv_plot)
ggsave("C://Users/ma22buky/Documents/01_PhD/plantpopnet/growth_plots.jpeg",
       device = "jpeg", 
       plot = grow_plot)

## combine both data frames for git hub and analysis
data_combined <- rbind(dat_halle, dat)

### save data frames (one csv for github one xlsx for pc)
write.xlsx(data_combined, "C://Users/ma22buky/Documents/01_PhD/plantpopnet/Aldo_project/Data/demo_dat.xlsx", overwrite = T)
write.csv(data_combined, "C://Users/ma22buky/Documents/01_PhD/plantpopnet/Aldo_project/Data/demo_dat.csv")