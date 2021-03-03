library(tidyverse)
library(readxl)
library(chillR)

yield <- read_excel("Daten_CeFiT_A_B_final.xlsx", sheet = "Ernteertraege")
yield2 <- read_excel("precrop_data_B.xlsx")
yield3 <- read_excel("Shoot_data_EH.xlsx", sheet = "Biomass harvest")
yield5 <- read_excel("Shoot_data_PK.xlsx")
yield6 <- read_excel("KA-19-A_Endernte_SW.xlsx", skip = 7)
trialC <- read_excel("data Trial C_2020_06_10.xlsx", 
                     sheet = "PlantNutrients Bearbeitet", 
                     col_types = c("text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "date", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "numeric", 
                                   "text", "text"))

# a function to remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# 1. changing the df's into something i can work with, calculating means and statistics
# 1.1 main table ####
yield[3] <- NULL
yield$precrop_duration[yield$precrop_duration == "1Y"] <- "1"
yield$precrop_duration[yield$precrop_duration == "2Y"] <- "2"
yield$precrop_duration[yield$precrop_duration == "3Y"] <- "3"

# problems accuring when there are % signs at the end of a column name -> rename them
names(yield)[9] <- "TM_grain"
names(yield)[10] <- "N_grain"
names(yield)[11] <- "C_grain"
names(yield)[12] <- "P_grain"
names(yield)[13] <- "K_grain"
names(yield)[15] <- "TM_residues"
names(yield)[16] <- "N_residues"
names(yield)[17] <- "C_residues"
names(yield)[18] <- "P_residues"
names(yield)[19] <- "K_residues"

yield[20:23] <- NULL
yield[14] <- NULL
yield[8] <- NULL
yield[15] <- NULL
yield[10] <- NULL

# calculating total NPK values
yield <- mutate(yield, N_grain = TM_grain*(N_grain/100), P_grain = TM_grain*(P_grain/100), K_grain = TM_grain*(K_grain/100), 
                N_residues = TM_residues*(N_residues/100), P_residues = TM_residues*(P_residues/100), 
                K_residues = TM_residues*(K_residues/100))

# removing outliers for all measured values
yield <- yield %>% group_by(trial, year, precrop, precrop_duration, crop) %>% 
  mutate_at(vars(TM_grain, TM_residues, N_grain, N_residues, P_grain, P_residues, K_grain, K_residues),
            funs(remove_outliers))
# warning because i use an old function that current dplyr patches dont support anymore, still works though

# counting missing values per group
yield_sample_size <- yield %>% group_by(trial, year, precrop, precrop_duration, crop) %>% 
  summarise_all(funs(sum(!is.na(.))))

# calculating means
yield <- yield %>% group_by(trial, year, precrop, precrop_duration, crop) %>% 
  summarise(TM_grain = mean(TM_grain, na.rm = TRUE),
            N_grain = mean(N_grain, na.rm = TRUE),
            P_grain = mean(P_grain, na.rm = TRUE),
            K_grain = mean(K_grain, na.rm = TRUE),
            TM_residues = mean(TM_residues, na.rm = TRUE),
            N_residues = mean(N_residues, na.rm = TRUE),
            P_residues = mean(P_residues, na.rm = TRUE),
            K_residues = mean(K_residues, na.rm = TRUE))

#renaming crops and treatments
yield$crop[yield$crop == "Hafer"] <- "Oats"
yield$crop[yield$crop == "WGerste"] <- "Winter Barley"
yield$crop[yield$crop == "WRaps"] <- "Winter Oilseed Rape"
yield$crop[yield$crop == "WRoggen"] <- "Winter Rye"
yield$crop[yield$crop == "WWeizen"] <- "Winter Wheat"
yield$crop[yield$crop == "SWeizen"] <- "Spring Wheat"

# 1.2 precrop_data_B (SW, after 2. precrop phase 2017-2019) ####
yield2 <- filter(yield2, treatm == "Lu" | treatm == "Fe" | treatm == "Ch")

# removing outliers for all measured values
yield2 <- yield2 %>% group_by(Trial, treatm) %>% 
  mutate_at(vars(DM_mean_mix, N_mean_mix, P_mean_mix, K_mean_mix),
            funs(remove_outliers))
# warning because i use an old function that current dplyr patches dont support anymore, still works though

# counting missing values per group
yield2_sample_size <- yield2 %>% group_by(Trial, treatm) %>% 
  summarise_all(funs(sum(!is.na(.))))

#not sure what dm mean mix means, but since this seems to be the only yield with NPK informations i took this one
yield2 <- yield2 %>% group_by(treatm, Trial) %>%
  summarise(TM_mix = mean(DM_mean_mix, na.rm = TRUE), N_mix = mean(N_mean_mix, na.rm = TRUE), P_mix = mean(P_mean_mix, na.rm = TRUE), 
            K_mix = mean(K_mean_mix, na.rm = TRUE))
# -> only 1 precrop duration??

# adding necessary columns
yield2$crop <- rep("Spring Wheat", nrow(yield2))
yield2$year <- rep("2020", nrow(yield2))
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

# removing outliers for all measured values
yield5 <- yield5 %>% group_by(Variante, cut) %>% 
  mutate_at(vars(TM, N, P, K),
            funs(remove_outliers))

# counting missing values per group
yield5_sample_size <- yield5 %>% group_by(Variante, cut) %>% 
  summarise_all(funs(sum(!is.na(.))))

# i decided to first calculate means for each cut and then 
# the kumulative values
yield5 <- yield5 %>% group_by(Variante, cut) %>%
  summarise(TM_mix = mean(TM, na.rm = TRUE), N_mix = mean(N, na.rm = TRUE), P_mix = mean(P, na.rm = TRUE), 
            K_mix = mean(K, na.rm = TRUE))

yield5 <- yield5 %>% group_by(Variante) %>% summarise(TM_mix = sum(TM_mix), N_mix = sum(N_mix), P_mix = sum(P_mix),
                                                      K_mix = sum(K_mix))

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
names(yield3)[1] <- "year"

# calculating the total NPK values
yield3 <- mutate(yield3, N = TM*(N_p/100), P = TM*(P_p/100), K = TM*(K_p/100))

# removing outliers for all measured values
yield3 <- yield3 %>% group_by(year, precrop, precrop_duration, crop, Harvest) %>% 
  mutate_at(vars(TM, N, P, K),
            funs(remove_outliers))

# counting missing values per group
yield5_sample_size <- yield3 %>% group_by(year, precrop, precrop_duration, crop, Harvest) %>% 
  summarise_all(funs(sum(!is.na(.))))

# calculating means for each cut and treatment
yield3 <- yield3 %>% group_by(Harvest, year, precrop, precrop_duration, crop) %>%
  summarise(TM = mean(TM, na.rm = TRUE), N = mean(N, na.rm = TRUE), P = mean(P, na.rm = TRUE), 
            K = mean(K, na.rm = TRUE))

yield3 <- yield3 %>% group_by(year, precrop, precrop_duration, crop) %>% 
  summarise(TM_mix = sum(TM), N_mix = sum(N), P_mix = sum(P), K_mix = sum(K))

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
names(yield6)[4] <- "TM_grain"
names(yield6)[5] <- "N_grain"
names(yield6)[6] <- "P_grain"
names(yield6)[7] <- "K_grain"
names(yield6)[8] <- "TM_residues"
names(yield6)[9] <- "N_residues"
names(yield6)[10] <- "P_residues"
names(yield6)[11] <- "K_residues"

# changing units from dt/ha to t/ha
yield6$TM_grain <- yield6$TM_grain*100
yield6$TM_residues <- yield6$TM_residues*100

# calculating total values for NPK
yield6 <- mutate(yield6, N_grain = TM_grain*(N_grain/100), P_grain = TM_grain*(P_grain/100), 
                 K_grain = TM_grain*(K_grain/100), N_residues = TM_residues*(N_residues/100),
                 P_residues = TM_residues*(P_residues/100), K_residues = TM_residues*(K_residues/100))

# removing outliers for all measured values
yield6 <- yield6 %>% group_by(Variante) %>% 
  mutate_at(vars(TM_grain, N_grain, P_grain, K_grain, TM_residues, N_residues, P_residues, K_residues),
            funs(remove_outliers))

# counting missing values per group
yield6_sample_size <- yield6 %>% group_by(Variante) %>% 
  summarise_all(funs(sum(!is.na(.))))

# calculating means for each treatment
yield6 <- yield6 %>% group_by(Variante) %>%
  summarise(TM_grain = mean(TM_grain, na.rm = TRUE), N_grain = mean(N_grain, na.rm = TRUE), 
            P_grain = mean(P_grain, na.rm = TRUE), K_grain = mean(K_grain, na.rm = TRUE),
            TM_residues = mean(TM_residues, na.rm = TRUE), N_residues = mean(N_residues, na.rm = TRUE), 
            P_residues = mean(P_residues, na.rm = TRUE), K_residues = mean(K_residues, na.rm = TRUE))

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
yield6$year <- rep("2019", nrow(yield6))
yield6$crop <- rep("Spring Wheat", nrow(yield6))
yield6$precrop[yield6$precrop == "1"] <- "lucerne"
yield6$precrop[yield6$precrop == "2"] <- "chicory"
yield6$precrop[yield6$precrop == "3"] <- "fescue"
yield6[1] <- NULL
yield6 <- yield6[,c(11, 12, 9, 10, 13, 1, 2, 3, 4, 5, 6, 7, 8)]

# 1.6 trialC 2014 ####
trialC <- slice(trialC , 1:360)
names(trialC)[9] <- "date"
names(trialC)[10] <- "spross_dm"
names(trialC)[19] <- "grain_dm"
names(trialC)[28] <- "straw_dm"

# filtering last measurement date/harvest date
trialC <- separate(trialC, date, sep = "-", into =c("Year", "Month", "Day"))

trialC <- filter(trialC, Year == 2014)
trialC <- make_JDay(trialC)
trialC <- filter(trialC, JDay == max(JDay))

# adding precrops and precrop duration
trialC <- trialC %>% mutate(precrop_duration = case_when(plot == 1 ~ "1", 
                                                         plot == 2 ~ "1",
                                                         plot == 3 ~ "1",
                                                         plot == 4 ~ "2",
                                                         plot == 5 ~ "2",
                                                         plot == 6 ~ "2",
                                                         plot == 7 ~ "2",
                                                         plot == 8 ~ "2",
                                                         plot == 9 ~ "2",
                                                         plot == 10 ~ "1",
                                                         plot == 11 ~ "1",
                                                         plot == 12 ~ "1",
                                                         plot == 13 ~ "1",
                                                         plot == 14 ~ "1",
                                                         plot == 15 ~ "1",
                                                         plot == 16 ~ "2",
                                                         plot == 17 ~ "2",
                                                         plot == 18 ~ "2",
                                                         plot == 19 ~ "2",
                                                         plot == 20 ~ "2",
                                                         plot == 21 ~ "2",
                                                         plot == 22 ~ "1",
                                                         plot == 23 ~ "1",
                                                         plot == 24 ~ "1"), 
                            precrop = case_when(plot == 1 ~ "lucerne", 
                                                plot == 2 ~ "chicory",
                                                plot == 3 ~ "fescue",
                                                plot == 4 ~ "lucerne",
                                                plot == 5 ~ "chicory",
                                                plot == 6 ~ "fescue",
                                                plot == 7 ~ "chicory",
                                                plot == 8 ~ "fescue",
                                                plot == 9 ~ "lucerne",
                                                plot == 10 ~ "fescue",
                                                plot == 11 ~ "lucerne",
                                                plot == 12 ~ "chicory",
                                                plot == 13 ~ "chicory",
                                                plot == 14 ~ "fescue",
                                                plot == 15 ~ "lucerne",
                                                plot == 16 ~ "fescue",
                                                plot == 17 ~ "chicory",
                                                plot == 18 ~ "lucerne",
                                                plot == 19 ~ "chicory",
                                                plot == 20 ~ "lucerne",
                                                plot == 21 ~ "fescue",
                                                plot == 22 ~ "fescue",
                                                plot == 23 ~ "chicory",
                                                plot == 24 ~ "lucerne"))

# changing column titles
trialC[5:8] <- NULL
trialC[6:7] <- NULL
trialC[33:36] <- NULL

names(trialC)[10] <- "N_Spross"
names(trialC)[12] <- "P_Spross"
names(trialC)[14] <- "K_Spross"
names(trialC)[19] <- "N_grain"
names(trialC)[21] <- "P_grain"
names(trialC)[23] <- "K_grain"
names(trialC)[28] <- "N_residues"
names(trialC)[30] <- "P_residues"
names(trialC)[32] <- "K_residues"
names(trialC)[15] <- "TM_grain"
names(trialC)[24] <- "TM_residues"
names(trialC)[6] <- "TM_Spross"

# changing columns into numeric values and calculating means for each treatment
trialC <- transform(trialC, TM_Spross = as.numeric(TM_Spross), TM_grain = as.numeric(TM_grain), TM_residues = as.numeric(TM_residues), 
                    N_Spross = as.numeric(N_Spross), N_grain = as.numeric(N_grain), N_residues = as.numeric(N_residues),
                    P_Spross = as.numeric(P_Spross), P_grain = as.numeric(P_grain), P_residues = as.numeric(P_residues),
                    K_Spross = as.numeric(K_Spross), K_grain = as.numeric(K_grain), K_residues = as.numeric(K_residues))

# removing outliers for all measured values
trialC <- trialC %>% group_by(Trial, crop, Year, precrop, precrop_duration) %>% 
  mutate_at(vars(TM_grain, N_grain, P_grain, K_grain, TM_residues, N_residues, P_residues, K_residues, 
                 TM_Spross, N_Spross, P_Spross, K_Spross),
            funs(remove_outliers))

# counting missing values per group
trialC_sample_size <- trialC %>% group_by(Trial, crop, Year, precrop, precrop_duration) %>% 
  summarise_all(funs(sum(!is.na(.))))

trialC <- trialC %>% group_by(Trial, crop, Year, precrop, precrop_duration) %>%
  summarise(TM_grain = mean(TM_grain, na.rm = TRUE), N_grain = mean(N_grain, na.rm = TRUE), 
            P_grain = mean(P_grain, na.rm = TRUE), K_grain = mean(K_grain, na.rm = TRUE),
            TM_residues = mean(TM_residues, na.rm = TRUE), N_residues = mean(N_residues, na.rm = TRUE), 
            P_residues = mean(P_residues, na.rm = TRUE), K_residues = mean(K_residues, na.rm = TRUE),
            TM_Spross = mean(TM_Spross, na.rm = TRUE), N_Spross = mean(N_Spross, na.rm = TRUE), 
            P_Spross = mean(P_Spross, na.rm = TRUE), K_Spross = mean(K_Spross, na.rm = TRUE))

names(trialC)[1] <- "trial"
names(trialC)[3] <- "year"
trialC$crop[trialC$crop == "spring barley"] <- "Spring Barley"

# 2. preparing the plotting ####
# combining the dfs
yield <- transform(yield, year = as.character(year))
yield2 <- transform(yield2, year = as.character(year))
yield3 <- transform(yield3, year = as.character(year), precrop_duration = as.character(precrop_duration))
yield5 <- transform(yield5, year = as.character(year))
yield6 <- transform(yield6, year = as.character(year))

yield <- bind_rows(yield, yield2, yield3, yield5, yield6)

# combining the treatments
yield <- yield %>% unite(precrop, precrop_duration, col = "treatment", sep ="-")
yield$treatment[yield$treatment == "chicory-1"] <- "Chi1"
yield$treatment[yield$treatment == "chicory-2"] <- "Chi2"
yield$treatment[yield$treatment == "chicory-3"] <- "Chi3"
yield$treatment[yield$treatment == "fescue-1"] <- "Fes1"
yield$treatment[yield$treatment == "fescue-2"] <- "Fes2"
yield$treatment[yield$treatment == "fescue-3"] <- "Fes3"
yield$treatment[yield$treatment == "lucerne-1"] <- "Lu1"
yield$treatment[yield$treatment == "lucerne-2"] <- "Lu2"
yield$treatment[yield$treatment == "lucerne-3"] <- "Lu3"

# combining straw and grain yield to plot them together
yield_mean <- yield %>% gather(TM_grain, TM_residues, TM_mix, key = "Dry_Matter", value = "mean_tm")
yield_mean[5:13] <- NULL
yield_mean <- na.omit(yield_mean)
yield_mean$Dry_Matter[yield_mean$Dry_Matter == "TM_grain"] <- "Grain DM"
yield_mean$Dry_Matter[yield_mean$Dry_Matter == "TM_residues"] <- "Residue DM"
yield_mean$Dry_Matter[yield_mean$Dry_Matter == "TM_mix"] <- "Mixed DM"

# calculating total NPK uptake
# yield_npk <- yield %>% gather(N_grain, P_grain, K_grain, N_residues, P_residues, K_residues, 
#                               N_mix, P_mix, K_mix ,key = "nutrients", value = "mean_nutrient")
#yield_npk[5:7] <- NULL
#yield_npk <- na.omit(yield_npk)
yield_npk <- yield %>% replace(is.na(.), 0) %>% 
  mutate(N_total = N_grain + N_residues + N_mix, P_total = P_grain + P_residues + P_mix,
         K_total = P_grain + P_residues + P_mix)
yield_npk[5:16] <- NULL

# making seperate tables for the facet wrap
wrapA <- filter(yield_mean, trial == "trial_A")
wrapB <- filter(yield_mean, trial == "trial_B")
NPK_A <- filter(yield_npk, trial == "trial_A")
NPK_B <- filter(yield_npk, trial == "trial_B")

# removing WW 2014 (only 2 treatments measured) and the second precrop phase
wrapA <- wrapA[!(wrapA$crop == "Winter Wheat" & wrapA$year == 2014),]
wrapA <- wrapA[!(wrapA$crop == "Spring Wheat" & wrapA$year == 2019),]
wrapB <- wrapB[!(wrapB$crop == "Spring Wheat" & wrapB$year == 2020),]
wrapB <- wrapB[!(wrapB$crop == "Winter Oilseed Rape" & wrapB$Dry_Matter == "Residue DM"),]
NPK_A <- NPK_A[!(NPK_A$crop == "Winter Wheat" & NPK_A$year == 2014),]
NPK_A <- NPK_A[!(NPK_A$crop == "Spring Wheat" & NPK_A$year == 2019),]
NPK_B <- NPK_B[!(NPK_B$crop == "Spring Wheat" & NPK_B$year == 2020),]
NPK_B <- NPK_B[!(NPK_B$crop == "Winter Oilseed Rape" & NPK_B$Dry_Matter == "Residue DM"),]

# calculating cumumulative yields for trialA
wrapA$Dry_Matter[wrapA$Dry_Matter == "Mixed DM"] <- "Residue DM"
wrapA_cum <- wrapA %>% group_by(treatment, Dry_Matter) %>%
  summarise(mean_tm = sum(mean_tm))

wrapA_cum$year <- rep("2010-2015", nrow(wrapA_cum))
wrapA_cum$trial <- rep("trial_A", nrow(wrapA_cum))
wrapA_cum$crop <- rep("Cummulative", nrow(wrapA_cum))
wrapA_cum <- wrapA_cum[,c(5, 4, 1, 6, 2, 3)]
wrapA <- bind_rows(wrapA, wrapA_cum)
wrapA$mean_tm <- wrapA$mean_tm/1000

# calculating cumumulative yields for trialB
wrapB$Dry_Matter[wrapB$Dry_Matter == "Mixed DM"] <- "Residue DM"
wrapB_cum <- wrapB %>% group_by(treatment, Dry_Matter) %>%
  summarise(mean_tm = sum(mean_tm))

wrapB_cum$year <- rep("2012-2017", nrow(wrapB_cum))
wrapB_cum$trial <- rep("trial_A", nrow(wrapB_cum))
wrapB_cum$crop <- rep("Cummulative", nrow(wrapB_cum))
wrapB_cum <- wrapB_cum[,c(5, 4, 1, 6, 2, 3)]
wrapB <- bind_rows(wrapB, wrapB_cum)
wrapB$mean_tm <- wrapB$mean_tm/1000

# calculating cummulative nutrient uptake for trialA
NPK_A_cum <- NPK_A %>% group_by(treatment) %>%
  summarise(N_total = sum(N_total), P_total = sum(P_total), K_total = sum(K_total))

NPK_A_cum$year <- rep("2010-2015", nrow(NPK_A_cum))
NPK_A_cum$trial <- rep("trial_A", nrow(NPK_A_cum))
NPK_A_cum$crop <- rep("Cummulative", nrow(NPK_A_cum))
NPK_A_cum <- NPK_A_cum[,c(6, 5, 1, 7, 2, 3, 4)]
NPK_A <- NPK_A %>% gather(N_total, P_total, K_total, key = "nutrient", value = "value")
NPK_A_cum <- NPK_A_cum %>% gather(N_total, P_total, K_total, key = "nutrient", value = "value")
NPK_A <- bind_rows(NPK_A, NPK_A_cum)
NPK_A$nutrient[NPK_A$nutrient == "N_total"] <- "N"
NPK_A$nutrient[NPK_A$nutrient == "P_total"] <- "P"
NPK_A$nutrient[NPK_A$nutrient == "K_total"] <- "K"

# calculating cummulative nutrient uptake for trialB
NPK_B_cum <- NPK_B %>% group_by(treatment) %>%
  summarise(N_total = sum(N_total), P_total = sum(P_total), K_total = sum(K_total))

NPK_B_cum$year <- rep("2010-2015", nrow(NPK_B_cum))
NPK_B_cum$trial <- rep("trial_A", nrow(NPK_B_cum))
NPK_B_cum$crop <- rep("Cummulative", nrow(NPK_B_cum))
NPK_B_cum <- NPK_B_cum[,c(6, 5, 1, 7, 2, 3, 4)]
NPK_B <- NPK_B %>% gather(N_total, P_total, K_total, key = "nutrient", value = "value")
NPK_B_cum <- NPK_B_cum %>% gather(N_total, P_total, K_total, key = "nutrient", value = "value")
NPK_B <- bind_rows(NPK_B, NPK_B_cum)
NPK_B$nutrient[NPK_B$nutrient == "N_total"] <- "N"
NPK_B$nutrient[NPK_B$nutrient == "P_total"] <- "P"
NPK_B$nutrient[NPK_B$nutrient == "K_total"] <- "K"

# Plottig yield ####
# TA
wrapA$year <- factor(wrapA$year, levels = c("2010", "2011", "2012", "2013", "2015", "2010-2015"))

ggplot(wrapA, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  facet_wrap(~ year + crop, scales = "free") +
  geom_text(aes(label = round(mean_tm, 2)), position = position_stack(vjust = .5), size = 3.5) +
  # facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("darkgoldenrod1", "forestgreen"))  +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [t* " ~ha^-1~"]"), 
       title = "Trial A") +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.x = element_text(size = 10, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_blank())

# TB
wrapB$year <- factor(wrapB$year, levels = c("2012", "2013", "2014", "2015", "2016", "2017", "2012-2017"))

ggplot(wrapB, aes(x = treatment, y = mean_tm, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  facet_wrap(~ year + crop, scales = "free") +
  geom_text(aes(label = round(mean_tm, 2)), position = position_stack(vjust = .5), size = 3.5) +
  # facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("darkgoldenrod1", "forestgreen"))  +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [t* " ~ha^-1~"]"), 
       title = "Trial B") +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.x = element_text(size = 10, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_blank())

# second precrop phase and trialC

# Plottig NPK ####
#TA
# NPK_A$treatment <-  factor(NPK_A$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
NPK_A$year <- factor(NPK_A$year, levels = c("2010", "2011", "2012", "2013", "2015", "2010-2015"))
NPK_A$nutrient <-  factor(NPK_A$nutrient, levels = c("N", "P", "K"))

ggplot(NPK_A, aes(x = treatment, y = value, fill = nutrient)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  geom_text(aes(label = round(value)), position = position_stack(vjust = .5), size = 3.5) +
  facet_wrap(~ year + crop, scales = "free") +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "darkgoldenrod1")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Uptake [kg* " ~ha^-1~"]"), 
       title = "Trial A Nutrient Uptake") +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.x = element_text(size = 10, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_blank())

#TB
#NPK_B$treatment <-  factor(NPK_B$treatment, levels = c("Fescue 1", "Fescue 2", "Chicory 2", "Lucerne 2"))
NPK_B$year <- factor(NPK_B$year, levels = c("2012", "2013", "2014", "2015", "2016", "2017", "2012-2017"))
NPK_B$nutrient <-  factor(NPK_B$nutrient, levels = c("N", "P", "K"))

ggplot(NPK_B, aes(x = treatment, y = value, fill = nutrient)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  geom_text(aes(label = round(value)), position = position_stack(vjust = .5), size = 3.5) +
  facet_wrap(~ year + crop, scales = "free") +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "darkgoldenrod1")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Uptake [kg* " ~ha^-1~"]"), 
       title = "Trial B Nutrient Uptake") +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.x = element_text(size = 10, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_blank())

# Plotting TrialC and second precrop phase ####
trialC$TM_grain <- trialC$TM_grain/1000
trialC$TM_residues <- trialC$TM_residues/1000
yield6$TM_grain <- yield6$TM_grain/1000
yield6$TM_residues <- yield6$TM_residues/1000
yield22 <- bind_rows(trialC, yield6, yield2)

yield22 <- yield22 %>% unite(precrop, precrop_duration, col = "treatment", sep ="-")
yield22$treatment[yield22$treatment == "chicory-1"] <- "Chi1"
yield22$treatment[yield22$treatment == "chicory-2"] <- "Chi2"
yield22$treatment[yield22$treatment == "chicory-3"] <- "Chi3"
yield22$treatment[yield22$treatment == "fescue-1"] <- "Fes1"
yield22$treatment[yield22$treatment == "fescue-2"] <- "Fes2"
yield22$treatment[yield22$treatment == "fescue-3"] <- "Fes3"
yield22$treatment[yield22$treatment == "lucerne-1"] <- "Lu1"
yield22$treatment[yield22$treatment == "lucerne-2"] <- "Lu2"
yield22$treatment[yield22$treatment == "lucerne-3"] <- "Lu3"

# combining Year and Crop
yield22 <- yield22 %>% unite(crop, year, col = "year", sep =" - ")

# combining grain and straw yield
yield221 <- yield22 %>% gather(TM_grain, TM_residues, TM_mix, key = "Dry_Matter", value = "value")
yield221[4:16] <- NULL
yield221 <- na.omit(yield221)
yield221$Dry_Matter[yield221$Dry_Matter == "TM_grain"] <- "Grain DM"
yield221$Dry_Matter[yield221$Dry_Matter == "TM_residues"] <- "Residue DM"
yield221$Dry_Matter[yield221$Dry_Matter == "TM_mix"] <- "Residue DM"
yield221$trial[yield221$trial == "trial_A"] <- "Trial A"
yield221$trial[yield221$trial == "trial_B"] <- "Trial B"

# plotting yield
ggplot(yield221, aes(x = treatment, y = value, fill = Dry_Matter)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  facet_wrap(~ trial + year, scales = "free") +
  geom_text(aes(label = round(value, 2)), position = position_stack(vjust = .5), size = 3.5) +
  # facet_grid(cols = vars(crop)) +
  scale_fill_manual(values = c("darkgoldenrod1", "forestgreen"))  +
  labs(x = "Treatment", 
       y = bquote("Mean Yield [t* " ~ha^-1~"]"), 
       title = "Trial A") +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.x = element_text(size = 10, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_blank())

# combining npk
npk22 <- yield22 %>% replace(is.na(.), 0) %>% 
  mutate(N_total = N_grain + N_residues + N_mix, P_total = P_grain + P_residues + P_mix,
         K_total = P_grain + P_residues + P_mix)
npk22[4:19] <- NULL

npk22 <- npk22 %>% gather(N_total, P_total, K_total, key = "nutrient", value = "value")
npk22$nutrient[npk22$nutrient == "N_total"] <- "N"
npk22$nutrient[npk22$nutrient == "P_total"] <- "P"
npk22$nutrient[npk22$nutrient == "K_total"] <- "K"

# plotting
npk22$nutrient <-  factor(npk22$nutrient, levels = c("N", "P", "K"))

ggplot(npk22, aes(x = treatment, y = value, fill = nutrient)) +
  geom_bar(stat = "identity", position = "stack", colour = "black") +
  geom_text(aes(label = round(value)), position = position_stack(vjust = .5), size = 3.5) +
  facet_wrap(~ trial + year, scales = "free") +
  scale_fill_manual(values = c("cornflowerblue", "forestgreen", "darkgoldenrod1")) +
  labs(x = "Treatment", 
       y = bquote("Mean Nutrient Uptake [kg* " ~ha^-1~"]"), 
       title = "Trial A Nutrient Uptake") +
  theme_bw() +
  theme(axis.text = element_text(size = 10), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.x = element_text(size = 10, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_blank())


# stuff below here is probably not needed anymore #####
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












