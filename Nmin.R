library(tidyverse)
library(readxl)
library(chillR)
library(gridExtra)
library(ggpubr)

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

# filtering the treatments we are interested in
Nmin <- filter(Nmin, precrop_duration <= 2)
Nmin <- Nmin[!(Nmin$precrop == "lucerne" & Nmin$precrop_duration == 1),]
Nmin <- Nmin[!(Nmin$precrop == "chicory" & Nmin$precrop_duration == 1),]
Nmin <- Nmin[!(Nmin$trial == "trial_C"),]

# Some measurement dates have 3, other 4 measured depths, removing the ones with 3 and with Na's
Nmin <- Nmin[!(Nmin$Year == "2010" & Nmin$JDay == 61),] 
Nmin <- Nmin[!(Nmin$Year == "2011" & Nmin$JDay == 52),] 
Nmin <- Nmin[!(Nmin$Year == "2013" & Nmin$JDay == 53),] 
Nmin <- drop_na(Nmin, Nmin)

# changing the treatment names
Nmin$precrop[Nmin$precrop == "chicory"] <- "Chi"
Nmin$precrop[Nmin$precrop == "fescue"] <- "Fes"
Nmin$precrop[Nmin$precrop == "lucerne"] <- "Lu"

# making separat tables for trial a and b
Nmin_A <- filter(Nmin, trial == "trial_A")
Nmin_B <- filter(Nmin, trial == "trial_B")

# adding main crops to the tables
Nmin_A <- Nmin_A %>% mutate(main_crop = case_when(treatment == "4" & Year == 2010  ~ "Mallow",
                                              treatment == "4" & Year == 2011  ~ "Winter Barley",
                                              treatment == "4" & Year == 2012  ~ "Winter Rye",
                                              treatment == "5" & Year == 2010  ~ "Spring Wheat",
                                              treatment == "5" & Year == 2011  ~ "Winter Oilseed Rape",
                                              treatment == "5" & Year == 2012  ~ "Winter Rye",
                                              treatment == "6" & Year == 2010  ~ "Spring Wheat",
                                              treatment == "6" & Year == 2011  ~ "Winter Barley",
                                              treatment == "6" & Year == 2012  ~ "Winter Oilseed Rape",
                                              treatment == "14" & Year == 2010  ~ "Spring Wheat",
                                              treatment == "14" & Year == 2011  ~ "Winter Oilseed Rape",
                                              treatment == "14" & Year == 2011  ~ "Winter Rye",
                                              treatment == "15" & Year == 2010  ~ "Spring Wheat",
                                              treatment == "15" & Year == 2011  ~ "Winter Barley",
                                              treatment == "15" & Year == 2012  ~ "Winter Oilseed Rape",
                                              treatment == "21" & Year == 2010  ~ "Spring Wheat",
                                              treatment == "21" & Year == 2011  ~ "Winter Barley",
                                              treatment == "21" & Year == 2012  ~ "Winter Oilseed Rape",
                                              treatment == "24" & Year == 2010  ~ "Spring Wheat",
                                              treatment == "24" & Year == 2011  ~ "Winter Barley", 
                                              treatment == "24" & Year == 2012  ~ "Winter Oilseed Rape",))

Nmin_B <- Nmin_B %>% mutate(main_crop = case_when(treatment == "4" & Year == 2011  ~ "Precrops",
                                                  treatment == "4" & Year == 2012  ~ "Mallow",
                                                  treatment == "4" & Year == 2013  ~ "Winter Barley",
                                                  treatment == "4" & Year == 2014  ~ "Winter Rye",
                                                  treatment == "4" & Year == 2015  ~ "Oats",
                                                  treatment == "4" & Year == 2016  ~ "Winter Wheat",
                                                  treatment == "4" & Year == 2017  ~ "Winter Barley",
                                                  treatment == "5" & Year == 2011  ~ "Precrops",
                                                  treatment == "5" & Year == 2012  ~ "Spring Wheat",
                                                  treatment == "5" & Year == 2013  ~ "Winter Oilseed",
                                                  treatment == "5" & Year == 2014  ~ "Winter Rye",
                                                  treatment == "5" & Year == 2015  ~ "Oats",
                                                  treatment == "5" & Year == 2016  ~ "Winter Wheat",
                                                  treatment == "5" & Year == 2017  ~ "Winter Barley",
                                                  treatment == "6" & Year == 2011  ~ "Precrops",
                                                  treatment == "6" & Year == 2012  ~ "Spring Wheat",
                                                  treatment == "6" & Year == 2013  ~ "Winter Barley",
                                                  treatment == "6" & Year == 2014  ~ "Winter Oilseed Rape",
                                                  treatment == "6" & Year == 2015  ~ "Oats",
                                                  treatment == "6" & Year == 2016  ~ "Winter Wheat",
                                                  treatment == "6" & Year == 2017  ~ "Winter Barley",
                                                  treatment == "14" & Year == 2011  ~ "Precrops",
                                                  treatment == "14" & Year == 2012  ~ "Spring Wheat",
                                                  treatment == "14" & Year == 2013  ~ "Winter Oilseed Rape",
                                                  treatment == "14" & Year == 2014  ~ "Winter Rye",
                                                  treatment == "14" & Year == 2015  ~ "Oats",
                                                  treatment == "14" & Year == 2016  ~ "Winter Wheat",
                                                  treatment == "14" & Year == 2017  ~ "Winter Barley",
                                                  treatment == "15" & Year == 2011  ~ "Precrops",
                                                  treatment == "15" & Year == 2012  ~ "Spring Wheat",
                                                  treatment == "15" & Year == 2013  ~ "Winter Barley",
                                                  treatment == "15" & Year == 2014  ~ "Winter Oilseed Rape",
                                                  treatment == "15" & Year == 2015  ~ "Oats",
                                                  treatment == "15" & Year == 2016  ~ "Winter Wheat",
                                                  treatment == "15" & Year == 2017  ~ "Winter Barley",
                                                  treatment == "21" & Year == 2011  ~ "Precrops",
                                                  treatment == "21" & Year == 2012  ~ "Spring Wheat",
                                                  treatment == "21" & Year == 2013  ~ "Winter Barley",
                                                  treatment == "21" & Year == 2014  ~ "Winter Oilseed",
                                                  treatment == "21" & Year == 2015  ~ "Oats",
                                                  treatment == "21" & Year == 2016  ~ "Winter Wheat",
                                                  treatment == "21" & Year == 2017  ~ "Winter Barley",
                                                  treatment == "24" & Year == 2011  ~ "Precrops",
                                                  treatment == "24" & Year == 2012  ~ "Spring Wheat",
                                                  treatment == "24" & Year == 2013  ~ "Winter Barley", 
                                                  treatment == "24" & Year == 2014  ~ "Winter Oilseed Rape",
                                                  treatment == "24" & Year == 2015  ~ "Oats",
                                                  treatment == "24" & Year == 2016  ~ "Winter Wheat",
                                                  treatment == "24" & Year == 2017  ~ "Winter Barley"))

# there is only 1 measurement of treatment 23 (in 2011), i removed it
Nmin_B <- Nmin_B[!(Nmin_B$treatment == 23),]

# 2. calculating means
NmeanA <- Nmin_A %>% group_by(JDay, Year, precrop, precrop_duration, depth, main_crop) %>% 
  summarise(Nmean=mean(Nmin))

NmeanB <- Nmin_B %>% group_by(JDay, Year, precrop, precrop_duration, depth, main_crop) %>% 
  summarise(Nmean=mean(Nmin))

# combining precrop and precrop duration into treatment
NmeanB <- NmeanB %>% unite(precrop, precrop_duration, col = "Treatment", sep = "")
NmeanA <- NmeanA %>% unite(precrop, precrop_duration, col = "Treatment", sep = "")

#changing the names of the depth classes
NmeanA$depth[NmeanA$depth == "30"] <- "30 cm"
NmeanA$depth[NmeanA$depth == "45"] <- "45 cm"
NmeanA$depth[NmeanA$depth == "75"] <- "75 cm"
NmeanA$depth[NmeanA$depth == "105"] <- "105 cm"

NmeanB$depth[NmeanB$depth == "30"] <- "30 cm"
NmeanB$depth[NmeanB$depth == "45"] <- "45 cm"
NmeanB$depth[NmeanB$depth == "75"] <- "75 cm"
NmeanB$depth[NmeanB$depth == "105"] <- "105 cm"

# 3. lineplots ####
# TrialA
NmeanA$depth = factor(NmeanA$depth, levels = c("30 cm", "45 cm", "75 cm", "105 cm"))

ggplot(NmeanA, aes(x = as.Date(JDay, origin = as.Date("2010-01-01")), y = Nmean, colour = Treatment, 
                   linetype = Treatment, shape = Treatment)) +
  geom_point() + geom_line(size = 0.6) +
  facet_grid(depth ~ Year + main_crop) +
  labs(x = "", y = "Nmin [kg  "~ha^-1 ~"]") +
  theme_bw() +
  scale_x_date(date_labels = "%b")+
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = c(0.05, 0.88),
        legend.text = element_text(size = 12),
        legend.title=element_text(size=13))

# TrialB
NmeanB$depth = factor(NmeanB$depth, levels = c("30 cm", "45 cm", "75 cm", "105 cm"))
# removing fes1 because it was only measured for the first 4 years
NmeanB <- NmeanB[!(NmeanB$Treatment == "Fes1"),]


ggplot(NmeanB, aes(x = as.Date(JDay, origin = as.Date("2012-01-01")), y = Nmean, colour = Treatment, 
                   linetype = Treatment, shape = Treatment)) +
  geom_point() + geom_line() +
  facet_grid(depth ~ Year + main_crop) +
  labs(x = "", y = "Nmin [kg  "~ha^-1 ~"]") +
  theme_bw() +
  scale_x_date(date_labels = "%b")+
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = c(0.05, 0.65),
        legend.text = element_text(size = 12),
        legend.title=element_text(size=13))

# with stacked barplots ####
NmeanA <- NmeanA %>% unite(precrop, precrop_duration, col = "treatment", sep ="")

TA_2010 <-  filter(NmeanA, Year == 2010 & main_crop == "Spring Wheat")
TA_2011 <-  filter(NmeanA, Year == 2011 & main_crop == "Winter Barley")
TA_2012 <-  filter(NmeanA, Year == 2012 & main_crop == "Winter Oilseed Rape")
TA_2010$depth = factor(TA_2010$depth, levels = c("105", "75", "45", "30"))


a <- ggplot(TA_2010, aes(x = treatment, y = Nmean, fill = depth)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  facet_grid(Year + main_crop ~ JDay) +
  scale_fill_manual(values = c("khaki", "darkseagreen", "steelblue", "midnightblue"), 
                    labels = c("75-105 cm", "45-75 cm", "30-45 cm", "0-30 cm")) +
  labs(x = "Treatment", y = "Nmin [kg * "~ha^-1 ~"]", title = "Trial A Nmin 2010") +
  scale_y_reverse() +
  theme_bw() +
  guides(fill = guide_legend(reverse = T)) + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10),
        legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_blank())
a

TA_2011$depth = factor(TA_2011$depth, levels = c("105", "75", "45", "30"))
b <- ggplot(TA_2011, aes(x = treatment, y = Nmean, fill = depth)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  facet_grid(Year + main_crop ~ JDay) +
  scale_fill_manual(values = c("khaki", "darkseagreen", "steelblue", "midnightblue"), 
                    labels = c("75-105 cm", "45-75 cm", "30-45 cm", "0-30 cm")) +
  labs(x = "Treatment", 
       y = "Nmin [kg * "~ha^-1 ~"]", 
       title = "Trial A Nmin 2011") +
  scale_y_reverse() +
  theme_bw() +
  guides(fill = guide_legend(reverse = T)) + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10),
        legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_blank())
b

TA_2012$depth = factor(TA_2012$depth, levels = c("105", "75", "45", "30"))
c <- ggplot(TA_2012, aes(x = treatment, y = Nmean, fill = depth)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  facet_grid(Year + main_crop ~ JDay) +
  scale_fill_manual(values = c("khaki", "darkseagreen", "steelblue", "midnightblue"), 
                    labels = c("75-105 cm", "45-75 cm", "30-45 cm", "0-30 cm")) +
  labs(x = "Treatment", 
       y = "Nmin [kg * "~ha^-1 ~"]", 
       title = "Trial A Nmin 2012") +
  scale_y_reverse() +
  theme_bw() +
  guides(fill = guide_legend(reverse = T)) + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10),
        legend.position = c(0.075, 0.25),
        legend.text = element_text(size = 10),
        legend.title = element_blank())
c

ggarrange(a, b, c, ncol = 1, nrow = 3)












