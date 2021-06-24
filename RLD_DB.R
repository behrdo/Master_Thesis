library(tidyverse)
library(readxl)
library(chillR)
library(broom)
library(rstatix)
library(psych)
library(rcompanion)
library(ggrepel)


#importing the df
RLD <- read_excel("Daten_CeFiT_A_B_final.xlsx", 
                  sheet = "RLD_main_crops", col_types = c("text", 
                  "date", "text", "text", "text", "text", 
                  "text", "text", "numeric", "numeric", 
                  "numeric", "numeric", "numeric", 
                  "numeric", "numeric", "numeric", 
                  "numeric", "numeric", "numeric", 
                  "numeric", "numeric", "text", "numeric", 
                  "numeric", "numeric"), skip = 3)

#1. unify the table ####
#deleting the first row and some unnecessary columns
RLD <- RLD[-c(1), ] 
RLD[23:25] <- NULL

#splitting it into 7 dfs with similar structures
df1 <- slice(RLD, 1:373)
df2 <- slice(RLD, 375:3504)
df3 <- slice(RLD, 3506:22705)
df4 <- slice(RLD, 22707:104388)
df5 <- slice(RLD, 104390:108132)
df6 <- slice(RLD, 108134:108277)
df7 <- slice(RLD, 108279:n())

#calculating total RLD, RL and RL biopores for all dfs
df1[20:22] <- NULL
df1[18] <- NULL
df1[11:16] <- NULL
df1[6] <- NULL
df1[8] <- NULL
names(df1)[8]<-"RLD"
names(df1)[9]<-"RL_total"
names(df1)[10]<-"RL_bp"
df1 <- mutate(df1, RL_total = RLD/2)

df2[14:22] <- NULL
df2[9:10] <- NULL
df2[6] <- NULL
names(df2)[8]<-"RLD"
names(df2)[10]<-"RLD_bp"
df2 <- mutate(df2, RL_total = RLD/2, RL_bp = RLD_bp/2)
df2[9:10] <- NULL

df3[20:22] <- NULL
df3[18] <- NULL
df3[12:16] <- NULL
df3[6] <- NULL
df3[8:9] <- NULL
names(df3)[8]<-"RLD"
names(df3)[9]<-"RL_total"
names(df3)[10]<-"RL_bp"

df4[20:22] <- NULL
df4[18] <- NULL
df4[12:16] <- NULL
df4[6] <- NULL
df4[8:9] <- NULL
names(df4)[8]<-"RLD"
names(df4)[9]<-"RL_total"
names(df4)[10]<-"RL_bp"

#df5 is TrialC
df5[20:22] <- NULL
df5[18] <- NULL
df5[12:16] <- NULL
df5[6] <- NULL
df5[8:9] <- NULL
names(df5)[8]<-"RLD"
names(df5)[9]<-"RL_total"
names(df5)[10]<-"RL_bp"

df6[20:22] <- NULL
df6[18] <- NULL
df6[11:16] <- NULL
df6[6] <- NULL
df6[8] <- NULL
names(df6)[8]<-"RLD"
names(df6)[9]<-"RL_total"
names(df6)[10]<-"RL_bp"
df6 <- mutate(df6, RL_total = RLD/2)

#df7: RLU total seems to be shifted 1 column to the right here -> RLU bulk = RLU total
#calculating RLD out of RLU total first. RLU/2x5x5x0.5 = RLD
names(df7)[15]<-"RLU"
names(df7)[17]<-"RLU_bp"
df7 <- mutate(df7, RLD = RLU/(2*5*5*0.5), RLD_bp = RLU_bp/(2*5*5*0.5))
df7[10:22] <- NULL
df7[6] <- NULL
df7[8] <- NULL
df7 <- mutate(df7, RL_total = RLD/2, RL_bp = RLD_bp/2)
df7[9] <- NULL

#adding the tables together with the exception of df5 (TrialC)
df <- bind_rows(df1, df2, df3, df4, df6, df7)

#transforming the date into JDay and changing column names to make further measures easier
names(df)[2]<-"date"
df <- separate(df, date, sep = "-", into =c("Year", "Month", "Day"))
df <- make_JDay(df)

names(df)[6]<-"plot_ID"
names(df)[7]<-"fc_treatment"
names(df)[8]<-"crop"
names(df)[9]<-"depth"

df <- drop_na(df, RLD)

#2. adding precrop species and duration ####
df <- df %>% mutate(precrop = case_when(treatment == 1 ~ "1", 
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
                    precrop_d = case_when(treatment == 1 ~ "1", 
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

df <- transform(df, precrop = as.numeric(precrop),
                precrop_d = as.numeric(precrop_d))

df <- drop_na(df, RLD)

df$crop[df$crop == "rapeseed"] <- "winter oilseed rape"

#3. Splitting the df into TrialA and TrialB ####
TrialA <- filter(df, Trial == "Trial A")
trialA <- filter(df, Trial == "trial A")
TrialB <- filter(df, Trial == "Trial B")
trialB <- filter(df, Trial == "trial B")

trialB <- bind_rows(trialB, TrialB)
trialA <- bind_rows(trialA, TrialA)

#4. Plot1: TrialA - Lu2 and Ch2 depth differentiated during flowering ####
#getting last measurement date
fm_plot1 <- filter(trialA, Year == "2010" & crop == "fodder mallow")
fm_plot1 <- filter(fm_plot1, JDay == 230)
#-> same dates as in the Detailübersicht sheet. last date: 18.08/230

sw_plot1 <- filter(trialA, Year == "2010" & crop == "spring wheat")
sw_plot1 <- filter(sw_plot1, JDay == 208)
#-> without monolith data (03.08/2015). last date: 27.07/208

wb_plot1 <- filter(trialA, Year == "2011" & crop == "winter barley")
wb_plot1 <- filter(wb_plot1, JDay == 136)
#Only 1y Fe and 2y Ch for 01.07/161 and 26.04/182 -> using 26.05/136

plot1 <- bind_rows(fm_plot1, sw_plot1, wb_plot1)

#introducing depth_class, which basically combines 2 measurement depths for a clearer visualization
plot1 <- transform(plot1, depth = as.numeric(depth))

plot1 <- plot1 %>% mutate(depth_class = case_when(depth <= 10 ~ "1", 
                                            depth > 10 & depth <= 20 ~ "2",
                                            depth > 20 & depth <= 30 ~ "3",
                                            depth > 30 & depth <= 40 ~ "4",
                                            depth > 40 & depth <= 50 ~ "5",
                                            depth > 50 & depth <= 60 ~ "6",
                                            depth > 60 & depth <= 70 ~ "7",
                                            depth > 70 & depth <= 80 ~ "8",
                                            depth > 80 & depth <= 90 ~ "9",
                                            depth > 90 & depth <= 100 ~ "10",
                                            depth > 100 & depth <= 110 ~ "11",
                                            depth > 110 & depth <= 120 ~ "12",
                                            depth > 120 & depth <= 130 ~ "13",
                                            depth > 130 & depth <= 140 ~ "14",
                                            depth > 140 & depth <= 150 ~ "15",
                                            depth > 150 & depth <= 160 ~ "16",
                                            depth > 160 & depth <= 170 ~ "17",
                                            depth > 170 & depth <= 180 ~ "18",
                                            depth > 180 & depth <= 190 ~ "19",
                                            depth > 190 & depth <= 200 ~ "20",
                                            ))

#removing depths from 180-200 cm, because of small samplesizes and a better visualization
plot1 <- plot1[!(plot1$depth_class == "20"),]
plot1 <- plot1[!(plot1$depth_class == "19"),]

#filtering Lu1+2 and Ch1+2 
Lu1 <- filter(plot1, precrop == 1 & precrop_d == 1)
Ch1 <- filter(plot1, precrop == 2 & precrop_d == 1)
Lu2 <- filter(plot1, precrop == 1 & precrop_d == 2)
Ch2 <- filter(plot1, precrop == 2 & precrop_d == 2)
Fe1 <- filter(plot1, precrop == 3 & precrop_d == 1)

plot1 <- bind_rows(Lu1, Ch1, Lu2, Ch2, Fe1)

#statistics: 1. Checking for normality
shapiro <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d) %>%
  summarise(statistic = shapiro.test(RLD)$statistic,
            p.value = shapiro.test(RLD)$p.value)
# -> most groups are not normally distributed => non parametric tests

plot1$precrop[plot1$precrop == "1"] <- "Lu"
plot1$precrop[plot1$precrop == "2"] <- "Chi"
plot1$precrop[plot1$precrop == "3"] <- "Fes"

#statistics: 2. Kruskal-Wallis test for the 1-Year Precrops (since 3 precrops)
KW <- filter(plot1, precrop_d == 1)
KW <- KW %>% group_by(depth_class, crop) %>% kruskal_test(RLD ~ precrop)
# -> some groups have p values below 0.05 and thus differ -> WIlcoxon-Test

KW <- filter(plot1, precrop_d == 1)
KW <- KW %>% group_by(depth_class, crop) %>% pairwise_wilcox_test(RLD ~ precrop, paired = FALSE, 
                                                                  p.adjust.method = "bonferroni")

#adding significance letters according to the wilcoxon test to the df ####
plot1 <- mutate(plot1, letters = case_when(crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 1 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 1 ~ "b",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 1 ~ "b", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 2 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 2 ~ "a",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 2 ~ "b",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 3 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 3 ~ "a",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 3 ~ "b", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 4 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 4 ~ "a",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 4 ~ "b", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 5 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 5 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 5 ~ "", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 6 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 6 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 6 ~ "", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 7 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 7 ~ "b",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 7 ~ "b",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 8 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 8 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 8 ~ "", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 9 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 9 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 9 ~ "",  
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 10 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 10 ~ "ab",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 10 ~ "b", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 11 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 11 ~ "b",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 11 ~ "b", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 12 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 12 ~ "b",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 12 ~ "c", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 13 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 13 ~ "b",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 13 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 14 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 14 ~ "b",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 14 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 15 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 15 ~ "b",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 15 ~ "a",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 1 & depth_class == 16 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes" & precrop_d == 1 & depth_class == 16 ~ "b",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 1 & depth_class == 16 ~ "a", 
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 1 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 1 ~ "b",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 1 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 2 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 2 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 2 ~ "", 
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 3 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 3 ~ "b",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 3 ~ "a",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 4 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 4 ~ "b",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 4 ~ "a",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 5 ~ "a",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 5 ~ "b",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 5 ~ "c",  
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 6 ~ "ab",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 6 ~ "a",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 6 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 7 ~ "a",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 7 ~ "b",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 7 ~ "a",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 8 ~ "a",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 8 ~ "b",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 8 ~ "c",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 9 ~ "ab",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 9 ~ "a",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 9 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 10 ~ "",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 10 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 10 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 11 ~ "",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 11 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 11 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 12 ~ "a",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 12 ~ "b",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 12 ~ "a",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 13 ~ "a",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 13 ~ "b",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 13 ~ "a",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 14 ~ "ab",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 14 ~ "a",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 14 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 15 ~ "a",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 15 ~ "ab",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 15 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 1 & depth_class == 16 ~ "",
                                           crop == "spring wheat" & precrop == "Fes" & precrop_d == 1 & depth_class == 16 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 1 & depth_class == 16 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 1 ~ "a",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 1 ~ "b",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 1 ~ "a",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 2 ~ "a",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 2 ~ "b",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 2 ~ "a",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 3 ~ "a",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 3 ~ "a",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 3 ~ "b",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 4 ~ "ab",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 4 ~ "a",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 4 ~ "b",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 5 ~ "a",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 5 ~ "b",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 5 ~ "a",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 6 ~ "",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 6 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 6 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 7 ~ "",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 7 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 7 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 8 ~ "a",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 8 ~ "b",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 8 ~ "b",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 9 ~ "a",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 9 ~ "b",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 9 ~ "b",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 10 ~ "a",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 10 ~ "b",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 10 ~ "b",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 11 ~ "",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 11 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 11 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 12 ~ "",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 12 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 12 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 13 ~ "",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 13 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 13 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 14 ~ "",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 14 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 14 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 15 ~ "",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 15 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 15 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 16 ~ "a",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 16 ~ "b",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 16 ~ "c",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 17 ~ "a",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 17 ~ "b",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 17 ~ "a",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 1 & depth_class == 18 ~ "a",
                                           crop == "winter barley" & precrop == "Fes" & precrop_d == 1 & depth_class == 18 ~ "b",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 1 & depth_class == 18 ~ "a",
                                           ))

#####

#statistics: 3. Wilcoxon-Test for the 2YPrecrops (since Fes is missing and we have only 2 groups)
WT <- filter(plot1, precrop_d == 2)
WT <- WT %>% group_by(depth_class, crop) %>% pairwise_wilcox_test(RLD ~ precrop, paired = FALSE, 
                                                                  p.adjust.method = "bonferroni")

#adding significance letters according to the wilcoxon test to the df ####
plot1 <- mutate(plot1, letters2 = case_when(crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 1 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 1 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 2 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 2 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 3 ~ "a",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 3 ~ "b",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 4 ~ "a",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 4 ~ "b",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 5 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 5 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 6 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 6 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 7 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 7 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 8 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 8 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 9 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 9 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 10 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 10 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 11 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 11 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 12 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 12 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 13 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 13 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 14 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 14 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 15 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 15 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi" & precrop_d == 2 & depth_class == 16 ~ "",
                                           crop == "fodder mallow" & precrop == "Lu" & precrop_d == 2 & depth_class == 16 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 1 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 1 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 2 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 2 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 3 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 3 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 4 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 4 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 5 ~ "a",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 5 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 6 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 6 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 7 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 7 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 8 ~ "a",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 8 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 9 ~ "a",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 9 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 10 ~ "a",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 10 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 11 ~ "a",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 11 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 12 ~ "a",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 12 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 13 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 13 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 14 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 14 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 15 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 15 ~ "",
                                           crop == "spring wheat" & precrop == "Chi" & precrop_d == 2 & depth_class == 16 ~ "",
                                           crop == "spring wheat" & precrop == "Lu" & precrop_d == 2 & depth_class == 16 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 1 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 1 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 2 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 2 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 3 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 3 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 4 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 4 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 5 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 5 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 6 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 6 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 7 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 7 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 8 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 8 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 9 ~ "a",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 9 ~ "b",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 10 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 10 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 11 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 11 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 12 ~ "a",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 12 ~ "b",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 13 ~ "a",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 13 ~ "b",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 14 ~ "a",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 14 ~ "b",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 15 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 15 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 16 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 16 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 17 ~ "",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 17 ~ "",
                                           crop == "winter barley" & precrop == "Chi" & precrop_d == 2 & depth_class == 18 ~ "a",
                                           crop == "winter barley" & precrop == "Lu" & precrop_d == 2 & depth_class == 18 ~ "b"
                                           
))
#####

#changing the two columns with significance groups into one
plot1 <- unite(plot1, letters, letters2, col = "sign", sep = "")
plot1$sign[plot1$sign == "NA"] <- ""
plot1$sign[plot1$sign == "aNA"] <- "a"
plot1$sign[plot1$sign == "NAa"] <- "a"
plot1$sign[plot1$sign == "bNA"] <- "b"
plot1$sign[plot1$sign == "NAb"] <- "b"
plot1$sign[plot1$sign == "cNA"] <- "c"
plot1$sign[plot1$sign == "NAc"] <- "c"
plot1$sign[plot1$sign == "abNA"] <- "ab"
plot1$sign[plot1$sign == "NAab"] <- "ab"

#removing winter oilseed rape since no data for precrop 1
plot1 <- plot1[!(plot1$crop == "winter oilseed rape"),]

#calculating means for each plot, maincrop, date and depth_class
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth_class, precrop, precrop_d, sign) %>% 
  summarise(mean_RLD = mean(RLD))

#count number of observations/plots and depth_class
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d, sign) %>%
  mutate(count = n())

#calculating means for each treatment, year, date and depth_class
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d, sign) %>% 
  summarise(mean_RLD2 = mean(mean_RLD))

#changing variable names
plot1$precrop_d[plot1$precrop_d == "1"] <- "1 Precrop Year"
plot1$precrop_d[plot1$precrop_d == "2"] <- "2 Precrop Years"
plot1$precrop[plot1$precrop == "1"] <- "Lu"
plot1$precrop[plot1$precrop == "2"] <- "Chi"
plot1$precrop[plot1$precrop == "3"] <- "Fes"

plot1 <- transform(plot1, precrop = as.factor(precrop), 
                   precrop_d = as.factor(precrop_d), 
                   depth_class = as.character(depth_class))

colnames(plot1)[5] <- "Precrop"

#sorting depth class
plot1$depth_class <- factor(plot1$depth_class, levels = c("18", "17", "16", "15", "14",
                                                          "13", "12", "11", "10", "9", "8", "7", "6",
                                                         "5", "4", "3", "2", "1"))

ggplot(plot1, aes(y = mean_RLD2, x = depth_class, colour = Precrop, group = Precrop, linetype = Precrop)) + 
  geom_point(aes(shape = Precrop)) + geom_line() + 
  facet_grid(precrop_d ~ Year + crop) +
  labs(x = bquote("Soil Depth [cm]"), y = "Mean Rootlength Density [cm " ~cm^-3 ~"]") +
  scale_colour_manual(values = c("red1", "steelblue1", "forestgreen")) + 
  scale_x_discrete(labels = c("170-180", "160-170", "150-160", "140-150",
                              "130-140", "120-130", "110-120", "100-110", "90-100", "80-90", "70-80",
                              "60-70", "50-60", "40-50", "30-40", "20-30", "10-20", "0-10")) +
  geom_text_repel(aes(label = sign), nudge_y = 0.25, show.legend = FALSE, direction = c("x"), 
                  segment.colour = NA)+
  coord_flip()+
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size=14)) +
  theme_bw()


#5. Plot2: TrialB - Fe2 depth differentiated during flowering ####
#getting the last measurement date
trialB <- filter(trialB, Year <= 2013)

fm_plot1 <- filter(trialB, Year == "2012" & crop == "fodder mallow")
fm_plot1 <- filter(fm_plot1, JDay == 198) #16.07.2012

sw_plot1 <- filter(trialB, Year == "2012" & crop == "spring wheat")
sw_plot1 <- filter(sw_plot1, JDay == 174) #Lu only on 174/22.06.2012

wb_plot1 <- filter(trialB, Year == "2013" & crop == "winter barley")
wb_plot1 <- filter(wb_plot1, JDay == 147) #Lu only on 147/27.05.2012

wosr_plot1 <- filter(trialB, Year == "2013" & crop == "winter oilseed rape")
wosr_plot1 <- filter(wosr_plot1, JDay == 177) #26.06.2013

plot1 <- bind_rows(fm_plot1, sw_plot1, wb_plot1, wosr_plot1)

#introducing depth_class
plot1 <- transform(plot1, depth = as.numeric(depth))

plot1 <- plot1 %>% mutate(depth_class = case_when(depth <= 10 ~ "1", 
                                                  depth > 10 & depth <= 20 ~ "2",
                                                  depth > 20 & depth <= 30 ~ "3",
                                                  depth > 30 & depth <= 40 ~ "4",
                                                  depth > 40 & depth <= 50 ~ "5",
                                                  depth > 50 & depth <= 60 ~ "6",
                                                  depth > 60 & depth <= 70 ~ "7",
                                                  depth > 70 & depth <= 80 ~ "8",
                                                  depth > 80 & depth <= 90 ~ "9",
                                                  depth > 90 & depth <= 100 ~ "10",
                                                  depth > 100 & depth <= 110 ~ "11",
                                                  depth > 110 & depth <= 120 ~ "12",
                                                  depth > 120 & depth <= 130 ~ "13",
                                                  depth > 130 & depth <= 140 ~ "14",
                                                  depth > 140 & depth <= 150 ~ "15",
                                                  depth > 150 & depth <= 160 ~ "16",
                                                  depth > 160 & depth <= 170 ~ "17",
                                                  depth > 170 & depth <= 180 ~ "18",
                                                  depth > 180 & depth <= 190 ~ "19",
                                                  depth > 190 & depth <= 200 ~ "20",
))

#removing depths from 200-110 cm, because of no differences and for better visualization
plot1 <- plot1[!(plot1$depth_class == "20"),]
plot1 <- plot1[!(plot1$depth_class == "19"),]
plot1 <- plot1[!(plot1$depth_class == "18"),]
plot1 <- plot1[!(plot1$depth_class == "17"),]
plot1 <- plot1[!(plot1$depth_class == "16"),]
plot1 <- plot1[!(plot1$depth_class == "15"),]
plot1 <- plot1[!(plot1$depth_class == "14"),]
plot1 <- plot1[!(plot1$depth_class == "13"),]
plot1 <- plot1[!(plot1$depth_class == "12"),]
plot1 <- plot1[!(plot1$depth_class == "11"),]

#filtering Lu1+2 and Ch1+2 and their last measurement date/flowering
Lu2 <- filter(plot1, precrop == 1 & precrop_d == 2)
Ch2 <- filter(plot1, precrop == 2 & precrop_d == 2)
Fe2 <- filter(plot1, precrop == 3 & precrop_d == 2)

plot1 <- bind_rows(Lu2, Ch2, Fe2)

#removing 2016 and 2017 since no data for precrops 2 and 3
plot1 <- plot1[!(plot1$Year == "2016"),]
plot1 <- plot1[!(plot1$Year == "2017"),]

#removing groups with only 0 as RLD results (shapiro wont work otherwise)
statist <- plot1[!(plot1$depth_class == "10" & plot1$crop == "spring wheat" & plot1$precrop == "1"),]

#statistics: 1. Checking for normality
shapiro <- statist %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d) %>%
  summarise(statistic = shapiro.test(RLD)$statistic,
            p.value = shapiro.test(RLD)$p.value)
# -> small sample sizes lead to most of the data being normally distributed. since that is most likely
# not the case and since i am to lazy to plot hundrets of samples to check this also visually, 
# i continue as in trialA with the wilcoxon test

plot1$precrop[plot1$precrop == "1"] <- "Lu"
plot1$precrop[plot1$precrop == "2"] <- "Chi"
plot1$precrop[plot1$precrop == "3"] <- "Fes"

#statistics: 2. Kruskal-Wallis test for spring wheat and winter barley (since 3 precrop groups)
KW <- filter(plot1, crop == c("spring wheat", "winter barley"))
KW <- KW %>% group_by(depth_class, crop) %>% kruskal_test(RLD ~ precrop)
# -> no signifficant differences

# statistics: 3. Wilcoxon test for fodder mallow and wosr
KW <- filter(plot1, crop == c("fodder mallow", "winter oilseed rape"))
KW <- KW %>% group_by(depth_class, crop) %>% pairwise_wilcox_test(RLD ~ precrop, paired = FALSE, 
                                                                  p.adjust.method = "bonferroni")

#calculating means for each plot, maincrop, date and depth_class
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth_class, precrop, precrop_d) %>% 
  summarise(mean_RLD = mean(RLD))

#count number of observations/plots
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d) %>%
  mutate(count = n())

#calculating means for each treatment, year, date and depth class
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d) %>% 
  summarise(mean_RLD2 = mean(mean_RLD))

# changing variable names
plot1$precrop_d[plot1$precrop_d == "2"] <- "2 Precrop Years"
plot1$precrop[plot1$precrop == "1"] <- "Lu"
plot1$precrop[plot1$precrop == "2"] <- "Chi"
plot1$precrop[plot1$precrop == "3"] <- "Fes"

plot1 <- transform(plot1, depth_class = as.numeric(depth_class), 
                   precrop = as.factor(precrop), 
                   precrop_d = as.factor(precrop_d))

colnames(plot1)[5] <- "Precrop"

#removing depths from 180-200 cm, because of small samplesizes and a better visualization
plot1 <- plot1[!(plot1$depth_class == "20"),]
plot1 <- plot1[!(plot1$depth_class == "19"),]

#sorting depth class
plot1$depth_class <- factor(plot1$depth_class, levels = c("10", "9", "8", "7", "6",
                                                          "5", "4", "3", "2", "1"))

ggplot(plot1, aes(y = mean_RLD2, x = depth_class, colour = Precrop, group = Precrop, linetype = Precrop)) + 
  geom_point(aes(shape = Precrop)) + geom_line() + 
  facet_grid(precrop_d ~ Year + crop) +
  labs(y = bquote("Soil Depth [cm]"), x = "Mean Rootlength Density [cm *" ~cm^-3 ~"]", 
       title = "Trial B") +
  theme_bw() +
  scale_colour_manual(values = c("red1", "steelblue1", "forestgreen")) + 
  scale_x_discrete(labels = c("90-100", "80-90", "70-80",
                              "60-70", "50-60", "40-50", "30-40", "20-30", "10-20", "0-10")) +
  coord_flip()+
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title=element_text(size=14)) +
  theme_bw()

#6. Plot3: TrialA - Ch2 and Fe1 on different dates (RLD) ####
#creating two depth groups: <30cm and >30cm
plot1 <- transform(trialA, depth = as.numeric(depth))

plot1 <- plot1 %>% mutate(depth_class = case_when(depth < 30 ~ "1", 
                                                  depth >= 30 & depth <= 100  ~ "2", 
                                                  depth > 100 ~ "3"))

plot1 <- transform(plot1, depth_class = as.numeric(depth_class))

#filtering Lu1+2 and Ch1+2 
Ch2 <- filter(plot1, precrop == 2 & precrop_d == 2)
Fe1 <- filter(plot1, precrop == 3 & precrop_d == 1)
plot1 <- bind_rows(Ch2, Fe1)

#filtering the dates to check for problems
plot1 <- plot1[!(plot1$crop == "spring wheat" & plot1$JDay == 215),] #removing monolith

#calculating means for each plot, maincrop, date and depth_class
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth_class, precrop, precrop_d) %>% 
  summarise(mean_RLD = mean(RLD))

#count number of observations/plots
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d) %>%
  mutate(count = n())

#calculating means for each treatment, year, date and depth
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d) %>% 
  summarise(mean_RLD2 = mean(mean_RLD))

#checking if JDays are fine (used a leap year at the beginning so they are all 1 off)
df2010 <- filter(plot1, Year == 2010)
df2011 <- filter(plot1, Year == 2011)
df2012 <- filter(plot1, Year == 2012)

df2010$JDay <- as.Date(df2010$JDay, "2010-01-01")
df2011$JDay <- as.Date(df2011$JDay, "2011-01-01")
df2012$JDay <- as.Date(df2012$JDay, "2012-01-01")

#fixing this
plot1$JDay <- plot1$JDay-1

#formating the df
plot1 <- transform(plot1, depth_class = as.numeric(depth_class))

plot1$depth_class[plot1$depth_class == "1"] <- "<=30 cm"
plot1$depth_class[plot1$depth_class == "2"] <- "30-100 cm"
plot1$depth_class[plot1$depth_class == "2"] <- ">100 cm"

plot1$precrop[plot1$precrop == "2"] <- "Chi2"
plot1$precrop[plot1$precrop == "3"] <- "Fes1"

plot1 <- transform(plot1, depth_class = as.factor(depth_class), 
                   Year = as.factor(Year), 
                   precrop = as.factor(precrop), 
                   JDay = as.numeric(JDay))

ggplot(plot1, aes(x = as.Date(JDay, origin = as.Date("2010-01-01")), y = mean_RLD2, colour = precrop, linetype = depth_class, shape = precrop)) + 
  geom_line() + geom_point() +
  facet_grid(cols = vars(Year, crop)) +
  labs(y = "Mean Rootlength Density [cm *" ~cm^-3 ~"]", 
       title = "Trial A") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  theme_bw() +
  scale_colour_manual(values = c("red1", "steelblue1")) + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title=element_blank(),
        legend.position = c(0.09, 0.82),)



#7. Plot4: TrialB - Ch2 and Fe2 on different dates (RLD) ####
plot1 <- transform(trialB, depth = as.numeric(depth))

plot1 <- plot1 %>% mutate(depth_class = case_when(depth <= 30 ~ "1", 
                                                    depth > 30 ~ "2") )
plot1 <- transform(plot1, depth_class = as.factor(depth_class))

#calculating means for each plot, maincrop, date and !!depth_class!!
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth_class, precrop, precrop_d) %>% 
  summarise(mean_RLD = mean(RLD))

#count number of observations/plots
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d) %>%
  mutate(count = n())

#calculating means for each treatment, year, date and depth
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d) %>% 
  summarise(mean_RLD2 = mean(mean_RLD))

#filtering Lu1+2 and Ch1+2 and their last measurement date (should be flowering????????)
Ch2 <- filter(plot1, precrop == 2 & precrop_d == 2)
Fe1 <- filter(plot1, precrop == 3 & precrop_d == 2)

plot1 <- bind_rows(Ch2, Fe1)

plot1$JDay <- plot1$JDay-1

#checking if JDays are fine (used a leap year at the beginning so they are all 1 off)
df2012 <- filter(plot1, Year == 2012)
df2013 <- filter(plot1, Year == 2013)

df2012$Date <- as.Date(df2012$JDay, "2012-01-01")
df2013$Date <- as.Date(df2013$JDay, "2013-01-01")
#same problem as in trialA -> fixed this above

#to better detect which dates strangely spike in 2013, i decided to keep the normal dates for now
plot1 <- bind_rows(df2012, df2013)

#removing 02.05(121) and 03.06(153) (both monolith methods)
plot1 <- plot1[!(plot1$Year == "2013" & plot1$JDay == 121),] 
plot1 <- plot1[!(plot1$Year == "2013" & plot1$JDay == 153),] 

plot1 <- transform(plot1, depth_class = as.numeric(depth_class))

plot1$depth_class[plot1$depth_class == "1"] <- "<= 30cm"
plot1$depth_class[plot1$depth_class == "2"] <- "> 30cm"
plot1$precrop[plot1$precrop == "2"] <- "Ch2"
plot1$precrop[plot1$precrop == "3"] <- "Fe2"

plot1 <- transform(plot1, depth_class = as.factor(depth_class), 
                   Year = as.factor(Year), 
                   precrop = as.factor(precrop), 
                   JDay = as.numeric(JDay))

colnames(plot1)[4] <- "Depth"
colnames(plot1)[5] <- "Precrop"

ggplot(plot1, aes(x = as.Date(JDay, origin = as.Date("2012-01-01")), y = mean_RLD2, colour = Precrop, linetype = Depth, shape = Precrop)) + 
  geom_line() + geom_point() +
  facet_grid(cols = vars(Year, crop)) +
  labs(y = "Mean Rootlength Density [cm *" ~cm^-3 ~"]", 
       title = "Trial B") +
  theme_bw() +
  scale_colour_manual(values = c("red1", "steelblue1")) + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = c(0.09, 0.75),
        legend.text = element_text(size = 12),
        legend.title=element_text(size=14))


#8. Plot5: TrialA - Ch2 and Fe1 on different dates (RL) ####
#creating three depth groups: <30cm, 30-100cm and >30cm
plot1 <- transform(trialA, depth = as.numeric(depth))

plot1 <- plot1 %>% mutate(depth_class = case_when(depth < 30 ~ "1", 
                                                  depth >= 30 & depth <= 100  ~ "2", 
                                                  depth > 100 ~ "3") )

plot1 <- transform(plot1, depth_class = as.numeric(depth_class))

#filtering Lu1+2 and Ch1+2 
Ch2 <- filter(plot1, precrop == 2 & precrop_d == 2)
Fe1 <- filter(plot1, precrop == 3 & precrop_d == 1)

plot1 <- bind_rows(Ch2, Fe1)

plot1$precrop[plot1$precrop == "2"] <- "Chi2"
plot1$precrop[plot1$precrop == "3"] <- "Fes1"

#filtering the dates to check for problems
plot1 <- plot1[!(plot1$crop == "spring wheat" & plot1$JDay == 215),] #removing monolith

#statistics: 1. Checking for normality
shapiro <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d) %>%
  summarise(statistic = shapiro.test(RL_total)$statistic,
            p.value = shapiro.test(RL_total)$p.value)
# -> most groups are not normally distributed => non parametric tests

#statistics: 2. Wilcoxon-Test
WT <- plot1 %>% group_by(depth_class, crop, JDay) %>% pairwise_wilcox_test(RL_total ~ precrop, paired = FALSE, 
                                                                  p.adjust.method = "bonferroni")

#adding significance levels to the precrops, based on WT
plot1 <- mutate(plot1, letters = case_when(crop == "fodder mallow" & precrop == "Chi2" & depth_class == 1 & JDay == 186 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 1 & JDay == 186 ~ "b", 
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 2 & JDay == 186 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 2 & JDay == 186 ~ "b",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 1 & JDay == 201 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 1 & JDay == 201 ~ "", 
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 2 & JDay == 201 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 2 & JDay == 201 ~ "b",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 3 & JDay == 201 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 3 & JDay == 201 ~ "a",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 1 & JDay == 215 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 1 & JDay == 215 ~ "b", 
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 2 & JDay == 215 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 2 & JDay == 215 ~ "b",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 3 & JDay == 215 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 3 & JDay == 215 ~ "b",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 1 & JDay == 230 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 1 & JDay == 230 ~ "", 
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 2 & JDay == 230 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 2 & JDay == 230 ~ "b",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 3 & JDay == 230 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 3 & JDay == 230 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 1 & JDay == 151 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 1 & JDay == 151 ~ "b", 
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 2 & JDay == 151 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 2 & JDay == 151 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 1 & JDay == 165 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 1 & JDay == 165 ~ "b", 
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 2 & JDay == 165 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 2 & JDay == 165 ~ "",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 1 & JDay == 179 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 1 & JDay == 179 ~ "", 
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 2 & JDay == 179 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 2 & JDay == 179 ~ "",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 1 & JDay == 194 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 1 & JDay == 194 ~ "", 
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 2 & JDay == 194 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 2 & JDay == 194 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 1 & JDay == 208 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 1 & JDay == 208 ~ "b", 
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 2 & JDay == 208 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 2 & JDay == 208 ~ "b", 
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 3 & JDay == 208 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 3 & JDay == 208 ~ "b",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 1 & JDay == 94 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 1 & JDay == 94 ~ "b", 
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 2 & JDay == 94 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 2 & JDay == 94 ~ "", 
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 3 & JDay == 208 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 3 & JDay == 208 ~ "b",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 1 & JDay == 116 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 1 & JDay == 116 ~ "b", 
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 2 & JDay == 116 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 2 & JDay == 116 ~ "b", 
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 3 & JDay == 116 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 3 & JDay == 116 ~ "b",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 1 & JDay == 136 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 1 & JDay == 136 ~ "b", 
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 2 & JDay == 136 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 2 & JDay == 136 ~ "b", 
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 3 & JDay == 136 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 3 & JDay == 136 ~ "b",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 1 & JDay == 161 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 1 & JDay == 161 ~ "b", 
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 2 & JDay == 161 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 2 & JDay == 161 ~ "b", 
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 3 & JDay == 161 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 3 & JDay == 161 ~ "b",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 1 & JDay == 182 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 1 & JDay == 182 ~ "b", 
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 2 & JDay == 182 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 2 & JDay == 182 ~ "", 
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 3 & JDay == 182 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 3 & JDay == 182 ~ "b",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 1 & JDay == 86 ~ "a", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 1 & JDay == 86 ~ "b", 
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 2 & JDay == 86 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 2 & JDay == 86 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 3 & JDay == 86 ~ "a", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 3 & JDay == 86 ~ "b",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 1 & JDay == 129 ~ "a", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 1 & JDay == 129 ~ "b", 
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 2 & JDay == 129 ~ "a", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 2 & JDay == 129 ~ "b", 
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 3 & JDay == 129 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 3 & JDay == 129 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 1 & JDay == 142 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 1 & JDay == 142 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 2 & JDay == 142 ~ "a", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 2 & JDay == 142 ~ "b", 
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 3 & JDay == 142 ~ "a", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 3 & JDay == 142 ~ "b",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 1 & JDay == 150 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 1 & JDay == 150 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 2 & JDay == 150 ~ "a", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 2 & JDay == 150 ~ "b", 
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 3 & JDay == 150 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 3 & JDay == 150 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 1 & JDay == 163 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 1 & JDay == 163 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 2 & JDay == 163 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 2 & JDay == 163 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 3 & JDay == 163 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 3 & JDay == 163 ~ "",
))

#calculating mean RL for each plot, maincrop, date and depth
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth, depth_class, precrop, precrop_d, letters) %>% 
  summarise(mean_RL = mean(RL_total))

#calculating total RL for each plot, maincrop, date and depth_class
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth_class, precrop, precrop_d, letters) %>% 
  summarise(total_RL = sum(mean_RL))

#calculating means for each treatment, year, date and depth_class
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d, letters) %>% 
  summarise(mean_RL2 = mean(total_RL))

#checking if JDays are fine (used a leap year at the beginning so they are all 1 off)
df2010 <- filter(plot1, Year == 2010)
df2011 <- filter(plot1, Year == 2011)
df2012 <- filter(plot1, Year == 2012)

df2010$JDay <- as.Date(df2010$JDay, "2010-01-01")
df2011$JDay <- as.Date(df2011$JDay, "2011-01-01")
df2012$JDay <- as.Date(df2012$JDay, "2012-01-01")

#fixing this
plot1$JDay <- plot1$JDay-1

#formating the df
plot1 <- transform(plot1, depth_class = as.numeric(depth_class))

plot1 <- transform(plot1, depth_class = as.numeric(depth_class), 
                   Year = as.factor(Year), 
                   precrop = as.factor(precrop), 
                   JDay = as.numeric(JDay))

plot1$depth_class[plot1$depth_class == "1"] <- "<30 cm"
plot1$depth_class[plot1$depth_class == "2"] <- "30-100 cm"
plot1$depth_class[plot1$depth_class == "3"] <- ">100 cm"

plot1$depth_class <- factor(plot1$depth_class, levels = c("<30 cm", "30-100 cm", ">100 cm"))

ggplot(plot1, aes(x = as.Date(JDay, origin = as.Date("2010-01-01")), y = mean_RL2, colour = precrop, linetype = depth_class, shape = precrop)) + 
  geom_line() + geom_point() +
  facet_grid(cols = vars(Year, crop)) +
  labs(y = "Total Rootlength [km " ~m^-2 ~"]", x = "Soil Depth [cm]",
       title = "Trial A") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  geom_text_repel(aes(label = letters), nudge_y = 0.25, show.legend = FALSE, direction = c("x"), 
                  segment.colour = NA) +
  theme_bw() +
  scale_colour_manual(values = c("red1", "steelblue1")) + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title=element_blank(),
        legend.position = c(0.09, 0.82),)

#9. Plot6: TrialB - Ch2 and Fe2 on different dates (RL) ####
plot1 <- transform(trialB, depth = as.numeric(depth))

plot1 <- plot1 %>% mutate(depth_class = case_when(depth < 30 ~ "1", 
                                                  depth >= 30 & depth <= 125  ~ "2", 
                                                  depth > 125 ~ "3") )

plot1 <- transform(plot1, depth_class = as.numeric(depth_class))

#filtering Lu1+2 and Ch1+2 and their last measurement date (should be flowering????????)
Ch2 <- filter(plot1, precrop == 2 & precrop_d == 2)
Fe1 <- filter(plot1, precrop == 3 & precrop_d == 2)

plot1 <- bind_rows(Ch2, Fe1)

plot1$precrop[plot1$precrop == "2"] <- "Chi2"
plot1$precrop[plot1$precrop == "3"] <- "Fes1"

#removing 02.05(121) and 03.06(153) (both monolith methods)
plot1 <- plot1[!(plot1$Year == "2013" & plot1$JDay == 121),] 
plot1 <- plot1[!(plot1$Year == "2013" & plot1$JDay == 153),] 

#filtering the dates to check for problems
plot1 <- plot1[!(plot1$crop == "spring wheat" & plot1$JDay == 215),] #removing monolith
plot1 <- plot1[!(plot1$crop == "spring wheat" & plot1$JDay == 206),] #only measured for some depths and strange values (very low compared to other dates)
plot1 <- plot1[!(plot1$crop == "winter barley" & plot1$JDay == 122),] #only measured for some depths and strange values (very high)
plot1 <- plot1[!(plot1$crop == "winter barley" & plot1$JDay == 154),] #only measured for some depths and strange values (very high)
plot1 <- plot1[!(plot1$crop == "winter oilseed rape" & plot1$JDay == 122),] #only measured for some depths and strange values (very high)

#removing measurements with only 0 as RL results (shapiro wont work otherwise)
statist <- plot1[!(plot1$depth_class == "3" & plot1$crop == "fodder mallow" & plot1$JDay == "186"),]
statist <- statist[!(statist$depth_class == "3" & statist$crop == "fodder mallow" & statist$JDay == "198"),]
statist <- statist[!(statist$depth_class == "3" & statist$crop == "spring wheat" & statist$JDay == "174"),]
statist <- statist[!(statist$depth_class == "3" & statist$crop == "winter barley" & statist$JDay == "100"),]

#statistics: 1. Checking for normality
shapiro <- statist %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d) %>%
  summarise(statistic = shapiro.test(RL_total)$statistic,
            p.value = shapiro.test(RL_total)$p.value)
# -> most groups are not normally distributed => non parametric tests

#statistics: 2. Wilcoxon-Test
WT <- statist %>% group_by(depth_class, crop, JDay) %>% pairwise_wilcox_test(RL_total ~ precrop, paired = FALSE, 
                                                                           p.adjust.method = "bonferroni")

#adding statistics to the main df
plot1 <- mutate(plot1, letters = case_when(crop == "fodder mallow" & precrop == "Chi2" & depth_class == 1 & JDay == 129 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 1 & JDay == 129 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 2 & JDay == 129 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 2 & JDay == 129 ~ "", 
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 1 & JDay == 142 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 1 & JDay == 142 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 2 & JDay == 142 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 2 & JDay == 142 ~ "", 
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 1 & JDay == 170 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 1 & JDay == 170 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 2 & JDay == 170 ~ "a", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 2 & JDay == 170 ~ "b", 
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 1 & JDay == 186 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 1 & JDay == 186 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 2 & JDay == 186 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 2 & JDay == 186 ~ "", 
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 1 & JDay == 198 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 1 & JDay == 198 ~ "",
                                           crop == "fodder mallow" & precrop == "Chi2" & depth_class == 2 & JDay == 198 ~ "", 
                                           crop == "fodder mallow" & precrop == "Fes1" & depth_class == 2 & JDay == 198 ~ "", 
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 1 & JDay == 128 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 1 & JDay == 128 ~ "",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 2 & JDay == 128 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 2 & JDay == 128 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 1 & JDay == 142 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 1 & JDay == 142 ~ "",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 2 & JDay == 142 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 2 & JDay == 142 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 1 & JDay == 157 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 1 & JDay == 157 ~ "",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 2 & JDay == 157 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 2 & JDay == 157 ~ "",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 1 & JDay == 174 ~ "a", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 1 & JDay == 174 ~ "b",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 2 & JDay == 174 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 2 & JDay == 174 ~ "",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 1 & JDay == 198 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 1 & JDay == 198 ~ "",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 2 & JDay == 198 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 2 & JDay == 198 ~ "",
                                           crop == "spring wheat" & precrop == "Chi2" & depth_class == 3 & JDay == 198 ~ "", 
                                           crop == "spring wheat" & precrop == "Fes1" & depth_class == 3 & JDay == 198 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 1 & JDay == 100 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 1 & JDay == 100 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 2 & JDay == 100 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 2 & JDay == 100 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 1 & JDay == 114 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 1 & JDay == 114 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 2 & JDay == 114 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 2 & JDay == 114 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 3 & JDay == 114 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 3 & JDay == 114 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 1 & JDay == 133 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 1 & JDay == 133 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 2 & JDay == 133 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 2 & JDay == 133 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 3 & JDay == 133 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 3 & JDay == 133 ~ "b",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 1 & JDay == 147 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 1 & JDay == 147 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 2 & JDay == 147 ~ "a", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 2 & JDay == 147 ~ "b",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 3 & JDay == 147 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 3 & JDay == 147 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 1 & JDay == 170 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 1 & JDay == 170 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 2 & JDay == 170 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 2 & JDay == 170 ~ "",
                                           crop == "winter barley" & precrop == "Chi2" & depth_class == 3 & JDay == 170 ~ "", 
                                           crop == "winter barley" & precrop == "Fes1" & depth_class == 3 & JDay == 170 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 1 & JDay == 105 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 1 & JDay == 105 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 2 & JDay == 105 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 2 & JDay == 105 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 3 & JDay == 105 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 3 & JDay == 105 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 1 & JDay == 116 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 1 & JDay == 116 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 2 & JDay == 116 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 2 & JDay == 116 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 3 & JDay == 116 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 3 & JDay == 116 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 1 & JDay == 141 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 1 & JDay == 141 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 2 & JDay == 141 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 2 & JDay == 141 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 3 & JDay == 141 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 3 & JDay == 141 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 1 & JDay == 163 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 1 & JDay == 163 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 2 & JDay == 163 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 2 & JDay == 163 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 3 & JDay == 163 ~ "a", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 3 & JDay == 163 ~ "b",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 1 & JDay == 177 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 1 & JDay == 177 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 2 & JDay == 177 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 2 & JDay == 177 ~ "",
                                           crop == "winter oilseed rape" & precrop == "Chi2" & depth_class == 3 & JDay == 177 ~ "", 
                                           crop == "winter oilseed rape" & precrop == "Fes1" & depth_class == 3 & JDay == 177 ~ "",
))

#calculating mean RL for each plot, maincrop, date and depth
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth, depth_class, precrop, precrop_d, letters) %>% 
  summarise(mean_RL = mean(RL_total))

#calculating total RL for each plot, maincrop, date and depth_class
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth_class, precrop, precrop_d, letters) %>% 
  summarise(total_RL = sum(mean_RL))

#calculating means for each treatment, year, date and depth_class
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth_class, precrop, precrop_d, letters) %>% 
  summarise(mean_RL2 = mean(total_RL))

#checking if JDays are fine (used a leap year at the beginning so they are all 1 off)
plot1$JDay <- plot1$JDay-1

plot1 <- transform(plot1, depth_class = as.numeric(depth_class), 
                   Year = as.factor(Year), 
                   precrop = as.factor(precrop), 
                   JDay = as.numeric(JDay))

plot1$depth_class[plot1$depth_class == "1"] <- "<30 cm"
plot1$depth_class[plot1$depth_class == "2"] <- "30-100 cm"
plot1$depth_class[plot1$depth_class == "3"] <- ">100 cm"

plot1$depth_class <- factor(plot1$depth_class, levels = c("<30 cm", "30-100 cm", ">100 cm"))

colnames(plot1)[5] <- "Precrop"

ggplot(plot1, aes(x = as.Date(JDay, origin = as.Date("2012-01-01")), y = mean_RL2, colour = Precrop, linetype = depth_class, shape = Precrop)) + 
  geom_line() + geom_point() +
  facet_grid(cols = vars(Year, crop)) +
  labs(y = "Total Rootlength [km " ~m^-2 ~"]", x = "Soil Depth [cm]",
       title = "Trial B") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  theme_bw() +
  geom_text_repel(aes(label = letters), nudge_y = 0.25, show.legend = FALSE, direction = c("x"), 
                  segment.colour = NA) +
  scale_colour_manual(values = c("red1", "steelblue1")) + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title=element_blank(),
        legend.position = c(0.09, 0.82),)
#warning occurse because of the rows which had to be removed for the shapiro test (only 0's), were not added in the statistics
# -> shouldnt be a problem though


#10.Plot7: TrialA - share of roots in biopores ####
#trying to find the last measurement date
fm_plot1 <- filter(trialA, Year == "2010" & crop == "fodder mallow")
fm_plot1 <- filter(fm_plot1, JDay == 230)
#-> same dates as in the Detailübersicht sheet. last date: 18.08/230

sw_plot1 <- filter(trialA, Year == "2010" & crop == "spring wheat")
sw_plot1 <- filter(sw_plot1, JDay == 208)
#-> without monolith data (03.08/2015). last date: 27.07/208

wb_plot1 <- filter(trialA, Year == "2011" & crop == "winter barley")
wb_plot1 <- filter(wb_plot1, JDay == 136)
#Only 1y Fe and 2y Ch for 01.07/161 and 26.04/182 -> using 26.05/136

plot1 <- bind_rows(fm_plot1, sw_plot1, wb_plot1)

#calculating means for each plot, maincrop, date and depth
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth, precrop, precrop_d) %>% 
  summarise(mean_RLtot = mean(RL_total), mean_RLbio = mean(RL_bp))

#count number of observations/plots
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth, precrop, precrop_d) %>%
  mutate(count = n())

#calculating means for each treatment, year, date and depth
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth, precrop, precrop_d) %>% 
  summarise(mean_RLtot2 = mean(mean_RLtot), mean_RLbio2 = mean(mean_RLbio))

plot1 <- plot1 %>% mutate(percentage = mean_RLbio2/mean_RLtot2*100)

#filtering Lu1+2 and Ch1+2 and their last measurement date (should be flowering????????)
Lu1 <- filter(plot1, precrop == 1 & precrop_d == 1)
Ch1 <- filter(plot1, precrop == 2 & precrop_d == 1)
Lu2 <- filter(plot1, precrop == 1 & precrop_d == 2)
Ch2 <- filter(plot1, precrop == 2 & precrop_d == 2)
Fe1 <- filter(plot1, precrop == 3 & precrop_d == 1)

plot1 <- bind_rows(Lu1, Ch1, Lu2, Ch2, Fe1)

#removing winter oilseed rape since no data for precrop 1
plot1 <- plot1[!(plot1$crop == "winter oilseed rape"),]

plot1$precrop_d[plot1$precrop_d == "1"] <- "1 Precrop Year"
plot1$precrop_d[plot1$precrop_d == "2"] <- "2 Precrop Years"
plot1$precrop[plot1$precrop == "1"] <- "Lu"
plot1$precrop[plot1$precrop == "2"] <- "Ch"
plot1$precrop[plot1$precrop == "3"] <- "Fe"


plot1 <- transform(plot1, depth = as.numeric(depth), 
                   precrop = as.factor(precrop), 
                   precrop_d = as.factor(precrop_d))

colnames(plot1)[5] <- "Precrop"

ggplot(plot1, aes(x = depth, y = percentage, colour = Precrop)) + 
  geom_line() +
  facet_grid(precrop_d ~ Year + crop) +
  labs(x = bquote("Soil Depth [cm]"), y= "Share of Roots in Biopores [%]", 
       title = "Trial A") +
  theme_bw() +
  scale_colour_manual(values = c("red1", "steelblue1", "forestgreen")) + 
  scale_x_reverse()+
  coord_flip()+
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title=element_text(size=14)) +
  theme_bw()


# statistics ####
#TrialA - depth differentiated
statTAd

#filtering Lu1+2 and Ch1+2 
Lu1 <- filter(statTAd, precrop == 1 & precrop_d == 1)
Ch1 <- filter(statTAd, precrop == 2 & precrop_d == 1)
Lu2 <- filter(plot1, precrop == 1 & precrop_d == 2)
Ch2 <- filter(plot1, precrop == 2 & precrop_d == 2)
Fe1 <- filter(plot1, precrop == 3 & precrop_d == 1)

plot1 <- bind_rows(Lu1, Ch1, Lu2, Ch2, Fe1)

#removing winter oilseed rape since no data for precrop 1
plot1 <- plot1[!(plot1$crop == "winter oilseed rape"),]

#removing depths from 180-200 cm, because of small samplesizes and a better visualization
plot1 <- plot1[!(plot1$depth_class == "20"),]
plot1 <- plot1[!(plot1$depth_class == "19"),]
















