library(tidyverse)
library(readxl)
library(chillR)

yield <- read_excel("Daten_CeFiT_A_B_final.xlsx", sheet = "Ernteertraege")

# changing the df to something i can work with
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
yield_mean <- yield %>% group_by(trial, year, precrop, precrop_duration, treatment, crop) %>% 
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

# filtering the main treatments
yield_mean <- filter(yield_mean, precrop_duration <= 2)
yield_mean <- yield_mean[!(yield_mean$precrop == "lucerne" & yield_mean$precrop_duration == 1),]
yield_mean <- yield_mean[!(yield_mean$precrop == "chicory" & yield_mean$precrop_duration == 1),]

# creating new dataframes for the trials and for short- and longtime effects 
TrialA_first <- filter(yield_mean, trial == "trial_A" & year == 2010)
TrialB_first <- filter(yield_mean, trial == "trial_B" & year == 2012)
TrialA_rest <- filter(yield_mean, trial == "trial_A" & year > 2010)
TrialB_rest <- filter(yield_mean, trial == "trial_B" & year > 2012)

TrialA_first <- TrialA_first %>% unite(precrop, precrop_duration, col = "treatment", sep ="-")
TrialA_rest <- TrialA_rest %>% unite(precrop, precrop_duration, col = "treatment", sep ="-")

# results of the first maincrop year
ggplot(TrialA_first, aes(x = treatment, y = mean_grainTM, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean_grainTM-sd_grainTM, ymax = mean_grainTM+sd_grainTM), width=.2,
                position=position_dodge(.9)) +
  facet_grid(rows = vars(year), cols = vars(crop)) +
  labs(x = "Treatment", 
       y = bquote("Mean Grain Harvest [kg* " ~ha^-1 ~"]"), 
       title = "Trial A Maincrop first Year GrainTM") + 
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = c(0.125, 0.19))

ggplot(TrialA_rest, aes(x = treatment, y = mean_grainTM, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean_grainTM-sd_grainTM, ymax = mean_grainTM+sd_grainTM), width=.2,
                position=position_dodge(.9)) +
  facet_grid(rows = vars(year), cols = vars(crop)) +
  labs(x = "Treatment", 
       y = bquote("Mean Grain Harvest [kg* " ~ha^-1 ~"]"), 
       title = "Trial A Maincrop GrainTM, alles außer erstes Jahr") + 
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        axis.text.x = element_blank(),
        legend.position = c(0.1, 0.85))
















