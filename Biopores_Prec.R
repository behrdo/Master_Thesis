library(tidyverse)
library(readxl)
library(rstatix)
library(agricolae)

#1. Loading the table and making it easier to work with ####
biopores <- read_excel("Daten_CeFiT_A_B_final.xlsx", sheet = "Bioporen")

biopores[12:13] <- NULL
biopores[8] <- NULL

names(biopores)[8] <- ">5 mm"
names(biopores)[9] <- "2-5 mm"
names(biopores)[10] <- "Total"

biopores$precrop_duration[biopores$precrop_duration == "1Y"] <- "1"
biopores$precrop_duration[biopores$precrop_duration == "2Y"] <- "2"
biopores$precrop_duration[biopores$precrop_duration == "3Y"] <- "3"
biopores$precrop[biopores$precrop == "lucerne"] <- "Lu"
biopores$precrop[biopores$precrop == "fescue"] <- "Fes"
biopores$precrop[biopores$precrop == "chicory"] <- "Chi"

#combining the pore types in 1 column
biopores <- gather(biopores, "2-5 mm", ">5 mm", "Total", key = "type", value = "number")

#combining the treatment and duration
biopores <- biopores %>% unite(precrop, precrop_duration, col = "treatment", sep ="")

#statistics ####
#1. Checking for normality
shapiro <- biopores %>% group_by(type, trial, year, treatment) %>%
  summarise(statistic = shapiro.test(number)$statistic,
            p.value = shapiro.test(number)$p.value)
#most groups seem to be normal distributed -> ANOVA and Tukey Test

#statistics: 2. Anova
#first we have to split the dataframe into the groups we want to compare
#trialA
df1 <- filter(biopores, trial == "trial_A" & year == 2010 & type == "2-5 mm")
df2 <- filter(biopores, trial == "trial_A" & year == 2010 & type == ">5 mm")
df3 <- filter(biopores, trial == "trial_A" & year == 2010 & type == "Total")
df4 <- filter(biopores, trial == "trial_A" & year == 2012 & type == "2-5 mm")
df5 <- filter(biopores, trial == "trial_A" & year == 2012 & type == ">5 mm")
df6 <- filter(biopores, trial == "trial_A" & year == 2012 & type == "Total")

#anova to see in which df significant differences occur
an1 <- aov(number ~ treatment, data = df1)
summary(an1)
an2 <- aov(number ~ treatment, data = df2)
summary(an2)
an3 <- aov(number ~ treatment, data = df3)
summary(an3)
an4 <- aov(number ~ treatment, data = df4)
summary(an4)
an5 <- aov(number ~ treatment, data = df5)
summary(an5)
an6 <- aov(number ~ treatment, data = df6)
summary(an6)

#only df6 showed significant results -> Tukey test to see which ones differ
treatment <- df6$treatment
hsd <- HSD.test(an6, "treatment", group = TRUE)
plot(hsd) # according to that they are all in the same group

#trialB
df1 <- filter(biopores, trial == "trial_B" & year == 2012 & type == "2-5 mm")
df2 <- filter(biopores, trial == "trial_B" & year == 2012 & type == ">5 mm")
df3 <- filter(biopores, trial == "trial_B" & year == 2012 & type == "Total")
df4 <- filter(biopores, trial == "trial_B" & year == 2014 & type == "2-5 mm")
df5 <- filter(biopores, trial == "trial_B" & year == 2014 & type == ">5 mm")
df6 <- filter(biopores, trial == "trial_B" & year == 2014 & type == "Total")

an1 <- aov(number ~ treatment, data = df1)
summary(an1)
an2 <- aov(number ~ treatment, data = df2)
summary(an2)
an3 <- aov(number ~ treatment, data = df3)
summary(an3)
an4 <- aov(number ~ treatment, data = df4)
summary(an4)
an5 <- aov(number ~ treatment, data = df5)
summary(an5)
an6 <- aov(number ~ treatment, data = df6)
summary(an6)
# -> no significant effects in trialB either

#plotting ####
#calculating means and sd
biopores_mean <- biopores %>% group_by(trial, year, treatment, type) %>%
  summarise(mean = mean(number), sd = sd(number))

biopores_mean$type <- factor(biopores_mean$type, levels = c("2-5 mm", ">5 mm", "Total"))
                                                          
poresA <- filter(biopores_mean, trial == "trial_A")
poresB <- filter(biopores_mean, trial == "trial_B")

ggplot(poresA, aes(x = treatment, y = mean, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=.2,
                position=position_dodge(.9)) +
  facet_grid(rows = vars(year), cols = vars(type)) +
  labs(fill = "Treatment",  x = "Treatment", y = bquote("Mean Biopores per " ~m^2),
       title = "Biopores Precrops TrialA") + 
  theme_bw() +
  scale_fill_manual(values = c("tomato", "red1", "red4", "cornflowerblue", "dodgerblue",
                                 "darkblue", "darkolivegreen4", "forestgreen", "darkgreen")) + 
  theme(axis.text = element_text(size = 12, face = "bold"), 
        axis.title = element_text(size = 14, face = "bold"), 
        plot.title = element_text(size = 15, face = "bold"), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10),
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15, face = "bold"), 
        legend.position = "none")

ggplot(poresB, aes(x = treatment, y = mean, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=.2,
                position=position_dodge(.9)) +
  facet_grid(rows = vars(year), cols = vars(type)) +
  labs(fill = "Treatment",  x = "Treatment", y = bquote("Mean Biopores per " ~m^2),
       title = "Biopores Precrops TrialB") + 
  theme_bw() +
  scale_fill_manual(values = c("tomato", "red1", "red4", "cornflowerblue", "dodgerblue",
                               "darkblue", "darkolivegreen4", "forestgreen", "darkgreen")) + 
  theme(axis.text = element_text(size = 12, face = "bold"), 
        axis.title = element_text(size = 14, face = "bold"), 
        plot.title = element_text(size = 15, face = "bold"), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10),
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15, face = "bold"), 
        legend.position = "none")












