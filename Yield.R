library(tidyverse)
library(readxl)
library(chillR)

yield <- read_excel("Daten_CeFiT_A_B_final.xlsx", sheet = "Ernteertraege")
yield2 <- read_excel("precrop_data_B.xlsx")
yield3 <- read_excel("Shoot_data_EH.xlsx", sheet = "Biomass harvest")
yield5 <- read_excel("Shoot_data_PK.xlsx")
yield6 <- read_excel("KA-19-A_Endernte_SW.xlsx", skip = 7)

# 1. changing the df's into something i can work with and calculating means
# 1.1 main table ####
yield[3] <- NULL
yield$precrop_duration[yield$precrop_duration == "1Y"] <- "1"
yield$precrop_duration[yield$precrop_duration == "2Y"] <- "2"
yield$precrop_duration[yield$precrop_duration == "3Y"] <- "3"

# problems accuring when there are % signs at the end of a column name -> rename them
names(yield)[10] <- "grain_N_content"
names(yield)[11] <- "grain_C_content"
names(yield)[12] <- "grain_P_content"
names(yield)[13] <- "grain_K_content"
names(yield)[16] <- "residues_N_content"
names(yield)[17] <- "residues_C_content"
names(yield)[18] <- "residues_P_content"
names(yield)[19] <- "residues_K_content"

# calculating means
yield_mean <- yield %>% group_by(trial, year, precrop, precrop_duration, crop) %>% 
  summarise(mean_grainFM = mean(grain_FM_86_kg_ha, na.rm = TRUE),
            sd_grainFM = sd(grain_FM_86_kg_ha, na.rm = TRUE), 
            mean_grainTM = mean(grain_TM_kg_ha, na.rm = TRUE),
            sd_grainTM = sd(grain_TM_kg_ha, na.rm = TRUE),
            mean_grainN = mean(grain_N_content, na.rm = TRUE),
            sd_grainN = sd(grain_N_content, na.rm = TRUE),
            mean_grainC = mean(grain_C_content, na.rm = TRUE),
            sd_grainC = sd(grain_C_content, na.rm = TRUE),
            mean_grainP = mean(grain_P_content, na.rm = TRUE),
            sd_grainP = sd(grain_P_content, na.rm = TRUE),
            mean_grainK = mean(grain_K_content, na.rm = TRUE),
            sd_grainK = sd(grain_K_content, na.rm = TRUE),
            mean_residuesFM = mean(residues_FM_86_kg_ha, na.rm = TRUE), 
            sd_residuesFM = sd(residues_FM_86_kg_ha, na.rm = TRUE),
            mean_residuesTM = mean(residues_TM_kg_ha, na.rm = TRUE),
            sd_residuesTM = sd(residues_TM_kg_ha, na.rm = TRUE),
            mean_residuesN = mean(residues_N_content, na.rm = TRUE),
            sd_residuesN = sd(residues_N_content, na.rm = TRUE),
            mean_residuesC = mean(residues_C_content, na.rm = TRUE),
            sd_residuesC = sd(residues_C_content, na.rm = TRUE),
            mean_residuesP = mean(residues_P_content, na.rm = TRUE),
            sd_residuesP = sd(residues_P_content, na.rm = TRUE),
            mean_residuesK = mean(residues_K_content, na.rm = TRUE),
            mean_GR_FM = mean(FM_grain_residues, na.rm = TRUE),
            sd_GR_FM = sd(FM_grain_residues, na.rm = TRUE),
            mean_GR_TM = mean(TM_grain_residues, na.rm = TRUE),
            sd_GR_TM = sd(TM_grain_residues, na.rm = TRUE))



# 1.2 precrop_data_B (SW, after 2. precrop phase 2017-2019) ####
yield2 <- filter(yield2, treatm == "Lu" | treatm == "Fe" | treatm == "Ch")

#not sure what dm mean mix means, but since this seems to be the only yiel with NPK informations i took this one
yield2 <- yield2 %>% group_by(treatm, Trial, ) %>%
  summarise(TM = mean(DM_mean_mix, na.rm = TRUE), N = mean(N_mean_mix, na.rm = TRUE), P = mean(P_mean_mix, na.rm = TRUE), 
            K = mean(K_mean_mix, na.rm = TRUE))
# -> only 1 precrop duration??

# adding necessary columns
yield2$crop <- rep("Spring Wheat", nrow(yield2))
yield2$Year <- rep("2019", nrow(yield2))
names(yield2)[1] <- "precrop"
names(yield2)[2] <- "trial"
yield2$precrop_duration <- rep("2", nrow(yield2))
yield2$precrop[yield2$precrop == "Lu"] <- "lucerne"
yield2$trial[yield2$trial == "trial B"] <- "trial_B"
yield2$precrop[yield2$precrop == "Ch"] <- "chicory"
yield2$precrop[yield2$precrop == "Fe"] <- "fescue"
yield2 <- yield2[,c(2, 8, 1, 9, 7, 3, 4, 5, 6)]

# 1.3 shoot_data_PK (fodder mallow 2010; measurement dates: 9.7., 20.7., 28.7.) ####
# creating 1 df that i can work with
tm <- gather(yield5, "TM1", "TM2", "TM3", key = "cut", value = "TM")
n <- gather(yield5, "N1", "N2", "N3", key = "cut", value = "N_p")
p <- gather(yield5, "P1", "P2", "P3", key = "cut", value = "P_p")
k <- gather(yield5, "K1", "K2", "K3", key = "cut", value = "K_p")

tm[4:22] <- NULL
n[4:22] <- NULL
p[4:22] <- NULL
k[4:22] <- NULL

yield5 <- cbind(tm, n[!names(n) %in% names(tm)], p[!names(p) %in% names(tm)], k[!names(k) %in% names(tm)])

# calculating the total NPK values for each sample
yield5 <- mutate(yield5, N = TM*(N_p/100), P = TM*(P_p/100), K = TM*(K_p/100))

# since there are values missing, i decided to first calculate means for each cut and then 
# the kumulative values
yield5 <- yield5 %>% group_by(Variante, cut) %>%
  summarise(TM = mean(TM, na.rm = TRUE), N = mean(N, na.rm = TRUE), P = mean(P, na.rm = TRUE), 
            K = mean(K, na.rm = TRUE))

yield5 <- yield5 %>% group_by(Variante) %>% summarise(TM = sum(TM), N = sum(N), P = sum(P), K = sum(K))

# adding necessary columns
yield5$year <- rep(2010, nrow(yield5))
yield5$trial <- rep("trial_A", nrow(yield5))
yield5$crop <- rep("Fodder Mallow", nrow(yield5))
names(yield5)[1] <- "treatment"

yield5 <- yield5 %>% mutate(precrop = case_when(treatment == 1 ~ "1", 
                                        treatment == 2 ~ "1",
                                        treatment == 3 ~ "1",
                                        treatment == 4 ~ "1",
                                        treatment == 5 ~ "1",
                                        treatment == 6 ~ "1",
                                        treatment == 7 ~ "1",
                                        treatment == 8 ~ "1",
                                        treatment == 9 ~ "1",
                                        treatment == 10 ~ "2",
                                        treatment == 11 ~ "2",
                                        treatment == 12 ~ "2",
                                        treatment == 13 ~ "2",
                                        treatment == 14 ~ "2",
                                        treatment == 15 ~ "2",
                                        treatment == 16 ~ "2",
                                        treatment == 17 ~ "2",
                                        treatment == 18 ~ "2",
                                        treatment == 19 ~ "3",
                                        treatment == 20 ~ "3",
                                        treatment == 21 ~ "3",
                                        treatment == 22 ~ "3",
                                        treatment == 23 ~ "3",
                                        treatment == 24 ~ "3",
                                        treatment == 25 ~ "3",
                                        treatment == 26 ~ "3",
                                        treatment == 27 ~ "3"), 
                    precrop_duration = case_when(treatment == 1 ~ "1", 
                                          treatment == 2 ~ "1",
                                          treatment == 3 ~ "1",
                                          treatment == 4 ~ "2",
                                          treatment == 5 ~ "2",
                                          treatment == 6 ~ "2",
                                          treatment == 7 ~ "3",
                                          treatment == 8 ~ "3",
                                          treatment == 9 ~ "3",
                                          treatment == 10 ~ "1",
                                          treatment == 11 ~ "1",
                                          treatment == 12 ~ "1",
                                          treatment == 13 ~ "2",
                                          treatment == 14 ~ "2",
                                          treatment == 15 ~ "2",
                                          treatment == 16 ~ "3",
                                          treatment == 17 ~ "3",
                                          treatment == 18 ~ "3",
                                          treatment == 19 ~ "1",
                                          treatment == 20 ~ "1",
                                          treatment == 21 ~ "1",
                                          treatment == 22 ~ "2",
                                          treatment == 23 ~ "2",
                                          treatment == 24 ~ "2",
                                          treatment == 25 ~ "3",
                                          treatment == 26 ~ "3",
                                          treatment == 27 ~ "3"))

yield5$precrop[yield5$precrop == "1"] <- "lucerne"
yield5$precrop[yield5$precrop == "2"] <- "chicory"
yield5$precrop[yield5$precrop == "3"] <- "fescue"
yield5 <- yield5[,c(7, 6, 9, 10, 8, 2, 3, 4, 5)]

# 1.4 shoot_data_EH (fodder mallow 2012) ####
yield3[5] <- NULL
names(yield3)[4] <- "treatment"
yield3[1] <- NULL
names(yield3)[4] <- "precrop"
names(yield3)[5] <- "precrop_duration"
names(yield3)[6] <- "crop"
yield3 <-  filter(yield3, crop == 2)
yield3$crop[yield3$crop == "2"] <- "Fodder Mallow"
yield3[14] <- NULL
yield3[12] <- NULL
yield3[10] <- NULL
yield3[9] <- NULL
yield3[7] <- NULL
names(yield3)[8] <- "TM"
names(yield3)[9] <- "N_p"
names(yield3)[10] <- "P_p"
names(yield3)[11] <- "K_p"

# calculating the total NPK values
yield3 <- mutate(yield3, N = TM*(N_p/100), P = TM*(P_p/100), K = TM*(K_p/100))

# calculating means for each cut and treatment
yield3 <- yield3 %>% group_by(Harvest, Year, precrop, precrop_duration, crop) %>%
  summarise(TM = mean(TM, na.rm = TRUE), N = mean(N, na.rm = TRUE), P = mean(P, na.rm = TRUE), 
            K = mean(K, na.rm = TRUE))

yield3 <- yield3 %>% group_by(Year, precrop, precrop_duration, crop) %>% 
  summarise(TM = sum(TM), N = sum(N), P = sum(P), K = sum(K))

yield3$trial <- rep("trial_B", nrow(yield3))
yield3 <- yield3[,c(9, 1, 2, 3, 4, 5, 6, 7, 8)]
yield3$precrop[yield3$precrop == "1"] <- "lucerne"
yield3$precrop[yield3$precrop == "2"] <- "chicory"
yield3$precrop[yield3$precrop == "3"] <- "fescue"

# 1.5 Trial A 2019 (Sommerweizen, nach 2. Vorfruchtphase 2016-2018) ####
# deleting unnecessary columns 
yield6[4:12] <- NULL
yield6[10:14] <- NULL
yield6[16:26] <- NULL
yield6[10] <- NULL
yield6[4] <- NULL
yield6[11] <- NULL
yield6[6] <- NULL

# changing names
names(yield6)[4] <- "TM_Korn"
names(yield6)[5] <- "N_Korn"
names(yield6)[6] <- "C_Korn"
names(yield6)[7] <- "P_Korn"
names(yield6)[8] <- "K_Korn"
names(yield6)[9] <- "TM_Stroh"
names(yield6)[10] <- "N_Stroh"
names(yield6)[11] <- "C_Stroh"
names(yield6)[12] <- "P_Stroh"
names(yield6)[13] <- "K_Stroh"

# changing units from dt/ha to t/ha
yield6$TM_Korn <- yield6$TM_Korn/10
yield6$TM_Stroh <- yield6$TM_Stroh/10

# calculating total values for NPK
yield6 <- mutate(yield6, N_Korn = TM_Korn*(N_Korn/100), P_Korn = TM_Korn*(P_Korn/100), 
                 K_Korn = TM_Korn*(K_Korn/100), N_Stroh = TM_Stroh*(N_Stroh/100), P_Stroh = TM_Stroh*(P_Stroh/100), 
                 K_Stroh = TM_Stroh*(K_Stroh/100))

# calculating means for each treatment
yield6 <- yield6 %>% group_by(Variante) %>%
  summarise(TM_Korn = mean(TM_Korn, na.rm = TRUE), N_Korn = mean(N_Korn, na.rm = TRUE), 
            P_Korn = mean(P_Korn, na.rm = TRUE), K_Korn = mean(K_Korn, na.rm = TRUE),
            TM_Stroh = mean(TM_Stroh, na.rm = TRUE), N_Stroh = mean(N_Stroh, na.rm = TRUE), 
            P_Stroh = mean(P_Stroh, na.rm = TRUE), K_Stroh = mean(K_Stroh, na.rm = TRUE))

# getting to precrops and precrop_durations
names(yield6)[1] <- "treatment"

yield6 <- yield6 %>% mutate(precrop = case_when(treatment == 1 ~ "1", 
                                                treatment == 2 ~ "1",
                                                treatment == 3 ~ "1",
                                                treatment == 4 ~ "1",
                                                treatment == 5 ~ "1",
                                                treatment == 6 ~ "1",
                                                treatment == 7 ~ "1",
                                                treatment == 8 ~ "1",
                                                treatment == 9 ~ "1",
                                                treatment == 10 ~ "2",
                                                treatment == 11 ~ "2",
                                                treatment == 12 ~ "2",
                                                treatment == 13 ~ "2",
                                                treatment == 14 ~ "2",
                                                treatment == 15 ~ "2",
                                                treatment == 16 ~ "2",
                                                treatment == 17 ~ "2",
                                                treatment == 18 ~ "2",
                                                treatment == 19 ~ "3",
                                                treatment == 20 ~ "3",
                                                treatment == 21 ~ "3",
                                                treatment == 22 ~ "3",
                                                treatment == 23 ~ "3",
                                                treatment == 24 ~ "3",
                                                treatment == 25 ~ "3",
                                                treatment == 26 ~ "3",
                                                treatment == 27 ~ "3"), 
                            precrop_duration = case_when(treatment == 1 ~ "1", 
                                                         treatment == 2 ~ "1",
                                                         treatment == 3 ~ "1",
                                                         treatment == 4 ~ "2",
                                                         treatment == 5 ~ "2",
                                                         treatment == 6 ~ "2",
                                                         treatment == 7 ~ "3",
                                                         treatment == 8 ~ "3",
                                                         treatment == 9 ~ "3",
                                                         treatment == 10 ~ "1",
                                                         treatment == 11 ~ "1",
                                                         treatment == 12 ~ "1",
                                                         treatment == 13 ~ "2",
                                                         treatment == 14 ~ "2",
                                                         treatment == 15 ~ "2",
                                                         treatment == 16 ~ "3",
                                                         treatment == 17 ~ "3",
                                                         treatment == 18 ~ "3",
                                                         treatment == 19 ~ "1",
                                                         treatment == 20 ~ "1",
                                                         treatment == 21 ~ "1",
                                                         treatment == 22 ~ "2",
                                                         treatment == 23 ~ "2",
                                                         treatment == 24 ~ "2",
                                                         treatment == 25 ~ "3",
                                                         treatment == 26 ~ "3",
                                                         treatment == 27 ~ "3"))


yield6$trial <- rep("trial_A", nrow(yield6))
yield6$Year <- rep("2019", nrow(yield6))
yield6$crop <- rep("Spring Wheat", nrow(yield6))
yield6$precrop[yield6$precrop == "1"] <- "lucerne"
yield6$precrop[yield6$precrop == "2"] <- "chicory"
yield6$precrop[yield6$precrop == "3"] <- "fescue"
yield6[1] <- NULL
yield6 <- yield6[,c(11, 12, 9, 10, 13, 1, 2, 3, 4, 5, 6, 7, 8)]

# 1.6 trialC ####














# 2. preparing the plotting ####
# filtering the main treatments
# yield_mean <- filter(yield_mean, precrop_duration <= 2)
# yield_mean <- yield_mean[!(yield_mean$precrop == "lucerne" & yield_mean$precrop_duration == 1),]
# yield_mean <- yield_mean[!(yield_mean$precrop == "chicory" & yield_mean$precrop_duration == 1),]

#combining the treatmetns
yield_mean <- yield_mean %>% unite(precrop, precrop_duration, col = "treatment", sep ="-")

#renaming crops and treatments
yield_mean$crop[yield_mean$crop == "Hafer"] <- "Oats"
yield_mean$crop[yield_mean$crop == "WGerste"] <- "Winter Barley"
yield_mean$crop[yield_mean$crop == "WRaps"] <- "Winter Oilseed Rape"
yield_mean$crop[yield_mean$crop == "WRoggen"] <- "Winter Rye"
yield_mean$crop[yield_mean$crop == "WWeizen"] <- "Winter Wheat"
yield_mean$crop[yield_mean$crop == "SWeizen"] <- "Spring Wheat"
yield_mean$treatment[yield_mean$treatment == "chicory-2"] <- "Chicory 2"
yield_mean$treatment[yield_mean$treatment == "fescue-2"] <- "Fescue 2"
yield_mean$treatment[yield_mean$treatment == "lucerne-2"] <- "Lucerne 2"
yield_mean$treatment[yield_mean$treatment == "fescue-1"] <- "Fescue 1"

# combining straw and grain yield to plot them together
yield_meany <- yield_mean %>% gather(mean_grainTM, mean_residuesTM, key = "Dry_Matter", value = "mean_tm")
yield_meanpk <- yield_mean %>% gather(mean_grainN, mean_grainP, mean_grainK, key = "nutrients", value = "mean_nutrient")
# yield_mean <- yield_mean %>% gather(mean_grainFM, mean_residuesFM, key = "Fresh_Matter", value = "mean_fm")

# making seperate tables for the facet wrap
wrapA <- filter(yield_meany, trial == "trial_A")
wrapB <- filter(yield_meany, trial == "trial_B")
NPK_A <- filter(yield_meanpk, trial == "trial_A")
NPK_B <- filter(yield_meanpk, trial == "trial_B")

# making a seperate tables for the different years (yield)
TrialA_2010 <- filter(yield_meany, trial == "trial_A" & year == 2010)
TrialA_2011 <- filter(yield_meany, trial == "trial_A" & year == 2011)
TrialA_2012 <- filter(yield_meany, trial == "trial_A" & year == 2012)
TrialA_2013 <- filter(yield_meany, trial == "trial_A" & year == 2013)
TrialA_2014 <- filter(yield_meany, trial == "trial_A" & year == 2014)
TrialA_2015 <- filter(yield_meany, trial == "trial_A" & year == 2015)

TrialB_2012 <- filter(yield_meany, trial == "trial_B" & year == 2012)
TrialB_2013 <- filter(yield_meany, trial == "trial_B" & year == 2013)
TrialB_2014 <- filter(yield_meany, trial == "trial_B" & year == 2014)
TrialB_2015 <- filter(yield_meany, trial == "trial_B" & year == 2015)
TrialB_2016 <- filter(yield_meany, trial == "trial_B" & year == 2016)
TrialB_2017 <- filter(yield_meany, trial == "trial_B" & year == 2017)

# making a seperate tables for the different years (NPK)
TrialA_2010_NPK <- filter(yield_meanpk, trial == "trial_A" & year == 2010)
TrialA_2011_NPK <- filter(yield_meanpk, trial == "trial_A" & year == 2011)
TrialA_2012_NPK <- filter(yield_meanpk, trial == "trial_A" & year == 2012)
TrialA_2013_NPK <- filter(yield_meanpk, trial == "trial_A" & year == 2013)
TrialA_2014_NPK <- filter(yield_meanpk, trial == "trial_A" & year == 2014)
TrialA_2015_NPK <- filter(yield_meanpk, trial == "trial_A" & year == 2015)

TrialB_2012_NPK <- filter(yield_meanpk, trial == "trial_B" & year == 2012)
TrialB_2013_NPK <- filter(yield_meanpk, trial == "trial_B" & year == 2013)
TrialB_2014_NPK <- filter(yield_meanpk, trial == "trial_B" & year == 2014)
TrialB_2015_NPK <- filter(yield_meanpk, trial == "trial_B" & year == 2015)
TrialB_2016_NPK <- filter(yield_meanpk, trial == "trial_B" & year == 2016)
TrialB_2017_NPK <- filter(yield_meanpk, trial == "trial_B" & year == 2017)

# creating new dataframes for the trials and for short- and longtime effects 
# TrialA_first <- filter(yield_mean, trial == "trial_A" & year == 2010)
# TrialB_first <- filter(yield_mean, trial == "trial_B" & year == 2012)
# TrialA_rest <- filter(yield_mean, trial == "trial_A" & year > 2010)
# TrialB_rest <- filter(yield_mean, trial == "trial_B" & year > 2012)

# plotting TrialA yield ####
# results of the first maincrop year
#TrialA_2010$treatment <-  factor(TrialA_2010$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))

ggplot(TrialA_2010, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial A Maincrop 2010 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TA 2011
TrialA_2011$treatment <-  factor(TrialA_2011$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))

ggplot(TrialA_2011, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial A Maincrop 2011 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TA 2012
TrialA_2012$treatment <-  factor(TrialA_2012$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialA_2012$crop <- factor(TrialA_2012$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialA_2012, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial A Maincrop 2012 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TA 2013
TrialA_2013$treatment <-  factor(TrialA_2013$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
#TrialA_2012$crop <- factor(TrialA_2012$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialA_2013, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial A Maincrop 2013 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TA 2014
TrialA_2014$treatment <-  factor(TrialA_2014$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
#TrialA_2012$crop <- factor(TrialA_2012$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialA_2014, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial A Maincrop 2014 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TA 2015
TrialA_2015$treatment <-  factor(TrialA_2015$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
#TrialA_2012$crop <- factor(TrialA_2012$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialA_2015, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial A Maincrop 2015 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

# plotting TrialB yield ####
#Tb 2012
# TrialB_2012$treatment <-  factor(TrialB_2012$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
# TrialB_2012$crop <- factor(TrialB_2012$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2012, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial B Maincrop 2012 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#Tb 2013
TrialB_2013$treatment <-  factor(TrialB_2013$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
# TrialB_2013$crop <- factor(TrialB_2013$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2013, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial B Maincrop 2013 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#Tb 2014
TrialB_2014$treatment <-  factor(TrialB_2014$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialB_2014$crop <- factor(TrialB_2014$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2014, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial B Maincrop 2014 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#Tb 2015
TrialB_2015$treatment <-  factor(TrialB_2015$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
# TrialB_2013$crop <- factor(TrialB_2013$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2015, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial B Maincrop 2015 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#Tb 2016
TrialB_2016$treatment <-  factor(TrialB_2016$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
# TrialB_2013$crop <- factor(TrialB_2013$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2016, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial B Maincrop 2016 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#Tb 2017
TrialB_2017$treatment <-  factor(TrialB_2017$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
# TrialB_2013$crop <- factor(TrialB_2013$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2017, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial B Maincrop 2017 DM") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

# plotting TA NPK ####
#TA 2010
TrialA_2010_NPK$treatment <-  factor(TrialA_2010_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialA_2010_NPK$nutrients <-  factor(TrialA_2010_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
# TrialA_2010_NPK$crop <- factor(TrialA_2010_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialA_2010_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial A Nutrients 2010") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TA 2011
TrialA_2011_NPK$treatment <-  factor(TrialA_2011_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialA_2011_NPK$nutrients <-  factor(TrialA_2011_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
# TrialA_2010_NPK$crop <- factor(TrialA_2010_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialA_2011_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial A Nutrients 2011") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TA 2012
TrialA_2012_NPK$treatment <-  factor(TrialA_2012_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialA_2012_NPK$nutrients <-  factor(TrialA_2012_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
TrialA_2012_NPK$crop <- factor(TrialA_2012_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialA_2012_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial A Nutrients 2012") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TA 2013
TrialA_2013_NPK$treatment <-  factor(TrialA_2013_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialA_2013_NPK$nutrients <-  factor(TrialA_2013_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
# TrialA_2012_NPK$crop <- factor(TrialA_2012_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialA_2013_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial A Nutrients 2013") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TA 2014
TrialA_2014_NPK$treatment <-  factor(TrialA_2014_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialA_2014_NPK$nutrients <-  factor(TrialA_2014_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
# TrialA_2012_NPK$crop <- factor(TrialA_2012_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialA_2014_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial A Nutrients 2014") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TA 2015
TrialA_2015_NPK$treatment <-  factor(TrialA_2015_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialA_2015_NPK$nutrients <-  factor(TrialA_2015_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
# TrialA_2012_NPK$crop <- factor(TrialA_2012_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialA_2015_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial A Nutrients 2015") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

# plotting TB NPK ####
#TB 2012
TrialB_2012_NPK$treatment <-  factor(TrialB_2012_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialB_2012_NPK$nutrients <-  factor(TrialB_2012_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
# TrialA_2010_NPK$crop <- factor(TrialA_2010_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2012_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial B Nutrients 2012") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())


#TB 2013
TrialB_2013_NPK$treatment <-  factor(TrialB_2013_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialB_2013_NPK$nutrients <-  factor(TrialB_2013_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
# TrialA_2010_NPK$crop <- factor(TrialA_2010_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2013_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial B Nutrients 2013") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TB 2014
TrialB_2014_NPK$treatment <-  factor(TrialB_2014_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialB_2014_NPK$nutrients <-  factor(TrialB_2014_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
TrialB_2014_NPK$crop <- factor(TrialB_2014_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2014_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial B Nutrients 2014") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TB 2015
TrialB_2015_NPK$treatment <-  factor(TrialB_2015_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialB_2015_NPK$nutrients <-  factor(TrialB_2015_NPK$nutrients, levels = c("mean_grainP", "mean_grainK"))
# TrialB_2015_NPK$crop <- factor(TrialB_2015_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2015_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial B Nutrients 2015") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TB 2016
TrialB_2016_NPK$treatment <-  factor(TrialB_2016_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialB_2016_NPK$nutrients <-  factor(TrialB_2016_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
# TrialB_2015_NPK$crop <- factor(TrialB_2015_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2016_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N","P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial B Nutrients 2016") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())

#TB 2017
TrialB_2017_NPK$treatment <-  factor(TrialB_2017_NPK$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
TrialB_2017_NPK$nutrients <-  factor(TrialB_2017_NPK$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))
# TrialB_2015_NPK$crop <- factor(TrialB_2015_NPK$crop, levels = c("Winter Rye", "Winter Oilseed Rape"))

ggplot(TrialB_2017_NPK, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N","P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial B Nutrients 2017") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom", 
        legend.title = element_blank())


# facet wrap yield ####
# TA
# wrapA$treatment <-  factor(wrapA$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))

ggplot(wrapA, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ year + crop) +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  # facet_grid(cols = vars(crop)) +
  # scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial A Maincrop DM Yield") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = c(0.85, 0.15), 
        legend.title = element_blank())

# TB
# wrapB$treatment <-  factor(wrapB$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))

ggplot(wrapB, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ year + crop) +
  geom_text(aes(label = round(mean_tm, 1)), position = position_stack(vjust = .5)) +
  # facet_grid(cols = vars(crop)) +
  # scale_fill_manual(values = c("cornflowerblue", "forestgreen"), labels = c("Grain DM", "Residue DM")) +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [kg* " ~ha^-1 ~"]"), 
       title = "Trial B Maincrop DM Yield") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = c(0.85, 0.15), 
        legend.title = element_blank())



# facet wrap NPK ####
#TA
# NPK_A$treatment <-  factor(NPK_A$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
NPK_A$nutrients <-  factor(NPK_A$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))

ggplot(NPK_A, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_wrap(~ year + crop) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial A Nutrient Content") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = c(0.85, 0.15),
        legend.title = element_blank())

#TB
#NPK_B$treatment <-  factor(NPK_B$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
NPK_B$nutrients <-  factor(NPK_B$nutrients, levels = c("mean_grainN", "mean_grainP", "mean_grainK"))

ggplot(NPK_B, aes(x = treatment, y = mean_nutrient, fill = nutrients)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(mean_nutrient, 2)), position = position_stack(vjust = .5)) +
  facet_wrap(~ year + crop) +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "firebrick"), labels = c("N", "P", "K")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Content Grain [%]"), 
       title = "Trial B Nutrient Content") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = c(0.85, 0.15),
        legend.title = element_blank())













