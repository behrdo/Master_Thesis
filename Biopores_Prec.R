library(tidyverse)
library(readxl)

# 1. Loading the table and making it easier to work with ####
biopores <- read_excel("Daten_CeFiT_A_B_final.xlsx", sheet = "Bioporen")

biopores[12:13] <- NULL
biopores[8] <- NULL

names(biopores)[8] <- ">5mm"
names(biopores)[9] <- "2-5mm"

biopores$precrop_duration[biopores$precrop_duration == "1Y"] <- "1"
biopores$precrop_duration[biopores$precrop_duration == "2Y"] <- "2"
biopores$precrop_duration[biopores$precrop_duration == "3Y"] <- "3"

biopores$precrop_duration <- as.numeric(biopores$precrop_duration)

# filtering the core treatments
biopores <- filter(biopores, precrop_duration <= 2)
biopores <- biopores[!(biopores$precrop == "lucerne" & biopores$precrop_duration == 1),]
biopores <- biopores[!(biopores$precrop == "chicory" & biopores$precrop_duration == 1),]

#combining the pore types in 1 column
biopores <- gather(biopores, "2-5mm", ">5mm", "total_BP", key = "type", value = "number")

# calculating means and sd
biopores_mean <- biopores %>% group_by(trial, year, precrop, precrop_duration, type) %>%
  summarise(mean = mean(number), sd = sd(number))

biopores_mean <- biopores_mean %>% unite(precrop, precrop_duration, col = "treatment", sep =" - ")

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
  theme(axis.text = element_text(size = 12, face = "bold"), 
        axis.title = element_text(size = 14, face = "bold"), 
        plot.title = element_text(size = 15, face = "bold"), 
        strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10),
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15, face = "bold"), 
        legend.position = "none")












