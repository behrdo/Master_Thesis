library(tidyverse)
library(readxl)
library(chillR)

# 1. Loading the table and making it easier to work with ####
Nmin <- read_excel("Daten_CeFiT_A_B_final.xlsx", sheet = "Nmin")

Nmin[14] <- NULL
Nmin[13] <- NULL
Nmin[10] <- NULL
Nmin[9] <- NULL
Nmin[8] <- NULL
Nmin[3] <- NULL
names(Nmin)[8]  <- "Nmin"
names(Nmin)[7]  <- "depth"

Nmin <- separate(Nmin, sampling_date, sep = "-", into =c("Year", "Month", "Day"))
Nmin <- make_JDay(Nmin)

# filtering the core treatments
Nmin <- filter(Nmin, precrop_duration <= 2)
Nmin <- Nmin[!(Nmin$precrop == "lucerne" & Nmin$precrop_duration == 1),]
Nmin <- Nmin[!(Nmin$precrop == "chicory" & Nmin$precrop_duration == 1),]
Nmin <- Nmin[!(Nmin$trial == "trial_C"),]

# Some measurement dates have 3, other 4 measured depths, removing the ones with 3 for now
Nmin <- Nmin[!(Nmin$Year == "2010" & Nmin$JDay == 61),] 
Nmin <- Nmin[!(Nmin$Year == "2011" & Nmin$JDay == 52),] 
Nmin <- Nmin[!(Nmin$Year == "2013" & Nmin$JDay == 53),] 

Nmin <- drop_na(Nmin, Nmin)

# making separat tables for trial a and b
Nmin_A <- filter(Nmin, trial == "trial_A")
Nmin_B <- filter(Nmin, trial == "trial_B")

# adding main crops to the tables
Nmin_A <- Nmin_A %>% mutate(main_crop = case_when(treatment == "4" & Year == 2010  ~ "Mallow",
                                              treatment == "4" & Year == 2011  ~ "W-Barley",
                                              treatment == "4" & Year == 2012  ~ "W-Rye",
                                              treatment == "5" & Year == 2010  ~ "S-Wheat",
                                              treatment == "5" & Year == 2011  ~ "W-Oilseed",
                                              treatment == "5" & Year == 2012  ~ "W-Rye",
                                              treatment == "6" & Year == 2010  ~ "S-Wheat",
                                              treatment == "6" & Year == 2011  ~ "W-Barley",
                                              treatment == "6" & Year == 2012  ~ "W-Oilseed",
                                              treatment == "14" & Year == 2010  ~ "S-Wheat",
                                              treatment == "14" & Year == 2011  ~ "W-Oilseed",
                                              treatment == "14" & Year == 2011  ~ "W-Rye",
                                              treatment == "15" & Year == 2010  ~ "S-Wheat",
                                              treatment == "15" & Year == 2011  ~ "W-Barley",
                                              treatment == "15" & Year == 2012  ~ "W-Oilseed",
                                              treatment == "21" & Year == 2010  ~ "S-Wheat",
                                              treatment == "21" & Year == 2011  ~ "W-Barley",
                                              treatment == "21" & Year == 2012  ~ "W-Oilseed",
                                              treatment == "24" & Year == 2010  ~ "S-Wheat",
                                              treatment == "24" & Year == 2011  ~ "W-Barley", 
                                              treatment == "24" & Year == 2012  ~ "W-Oilseed",))

Nmin_B <- Nmin_B %>% mutate(main_crop = case_when(treatment == "4" & Year == 2011  ~ "Precrops",
                                                  treatment == "4" & Year == 2012  ~ "Mallow",
                                                  treatment == "4" & Year == 2013  ~ "W-Barley",
                                                  treatment == "4" & Year == 2014  ~ "W-Rye",
                                                  treatment == "4" & Year == 2015  ~ "Oats",
                                                  treatment == "4" & Year == 2016  ~ "W-Wheat",
                                                  treatment == "4" & Year == 2017  ~ "W-Barley",
                                                  treatment == "5" & Year == 2011  ~ "Precrops",
                                                  treatment == "5" & Year == 2012  ~ "S-Wheat",
                                                  treatment == "5" & Year == 2013  ~ "W-Oilseed",
                                                  treatment == "5" & Year == 2014  ~ "W-Rye",
                                                  treatment == "5" & Year == 2015  ~ "Oats",
                                                  treatment == "5" & Year == 2016  ~ "W-Wheat",
                                                  treatment == "5" & Year == 2017  ~ "W-Barley",
                                                  treatment == "6" & Year == 2011  ~ "Precrops",
                                                  treatment == "6" & Year == 2012  ~ "S-Wheat",
                                                  treatment == "6" & Year == 2013  ~ "W-Barley",
                                                  treatment == "6" & Year == 2014  ~ "W-Oilseed",
                                                  treatment == "6" & Year == 2015  ~ "Oats",
                                                  treatment == "6" & Year == 2016  ~ "W-Wheat",
                                                  treatment == "6" & Year == 2017  ~ "W-Barley",
                                                  treatment == "14" & Year == 2011  ~ "Precrops",
                                                  treatment == "14" & Year == 2012  ~ "S-Wheat",
                                                  treatment == "14" & Year == 2013  ~ "W-Oilseed",
                                                  treatment == "14" & Year == 2014  ~ "W-Rye",
                                                  treatment == "14" & Year == 2015  ~ "Oats",
                                                  treatment == "14" & Year == 2016  ~ "W-Wheat",
                                                  treatment == "14" & Year == 2017  ~ "W-Barley",
                                                  treatment == "15" & Year == 2011  ~ "Precrops",
                                                  treatment == "15" & Year == 2012  ~ "S-Wheat",
                                                  treatment == "15" & Year == 2013  ~ "W-Barley",
                                                  treatment == "15" & Year == 2014  ~ "W-Oilseed",
                                                  treatment == "15" & Year == 2015  ~ "Oats",
                                                  treatment == "15" & Year == 2016  ~ "W-Wheat",
                                                  treatment == "15" & Year == 2017  ~ "W-Barley",
                                                  treatment == "21" & Year == 2011  ~ "Precrops",
                                                  treatment == "21" & Year == 2012  ~ "S-Wheat",
                                                  treatment == "21" & Year == 2013  ~ "W-Barley",
                                                  treatment == "21" & Year == 2014  ~ "W-Oilseed",
                                                  treatment == "21" & Year == 2015  ~ "Oats",
                                                  treatment == "21" & Year == 2016  ~ "W-Wheat",
                                                  treatment == "21" & Year == 2017  ~ "W-Barley",
                                                  treatment == "24" & Year == 2011  ~ "Precrops",
                                                  treatment == "24" & Year == 2012  ~ "S-Wheat",
                                                  treatment == "24" & Year == 2013  ~ "W-Barley", 
                                                  treatment == "24" & Year == 2014  ~ "W-Oilseed",
                                                  treatment == "24" & Year == 2015  ~ "Oats",
                                                  treatment == "24" & Year == 2016  ~ "W-Wheat",
                                                  treatment == "24" & Year == 2017  ~ "W-Barley"))

# there is only 1 measurement of treatment 23 (in 2011), i removed it
Nmin_B <- Nmin_B[!(Nmin_B$treatment == 23),]

# 2. calculating means and plotting ###
NmeanA <- Nmin_A %>% group_by(JDay, Year, treatment, precrop, precrop_duration, depth, main_crop) %>% 
  summarise(Nmean=mean(Nmin))

NmeanA <- transform(NmeanA, depth = as.character(depth), 
                   Year = as.character(Year), 
                   precrop = as.character(precrop), 
                   JDay = as.numeric(JDay), 
                   treatment = as.character(treatment), 
                   precrop_duration = as.character(precrop_duration))

NmeanB <- Nmin_B %>% group_by(JDay, Year, treatment, precrop, precrop_duration, depth, main_crop) %>% 
  summarise(Nmean=mean(Nmin))

NmeanB <- transform(NmeanB, Year = as.character(Year), 
                    precrop = as.character(precrop), 
                    JDay = as.numeric(JDay), 
                    treatment = as.character(treatment), 
                    precrop_duration = as.character(precrop_duration))

NmeanA$depth_f = factor(NmeanA$depth, levels = c("30", "45", "75", "105"))

ggplot(NmeanA, aes(x = as.Date(JDay, origin = as.Date("2010-01-01")), y = Nmean, colour = precrop, shape = precrop_duration)) +
  geom_point() + geom_line() +
  facet_grid(depth_f ~ Year + main_crop) +
  labs(x = "", y = "Nmin [kg * "~ha^-1 ~"]", title = "Trial A Nmin") +
  theme_bw() +
  scale_x_date(date_labels = "%b")+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title=element_text(size=11))

NmeanB$depth_f = factor(NmeanB$depth, levels = c("30", "45", "75", "105"))

ggplot(NmeanB, aes(x = as.Date(JDay, origin = as.Date("2012-01-01")), y = Nmean, colour = precrop, shape = precrop_duration)) +
  geom_point() + geom_line() +
  facet_grid(depth_f ~ Year + main_crop) +
  labs(x = "", y = "Nmin [kg * "~ha^-1 ~"]", title = "Trial B Nmin") +
  theme_bw() +
  scale_x_date(date_labels = "%b")+
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title=element_text(size=11))





















