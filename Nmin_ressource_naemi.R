#changing data frame ######
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lme4)
library(chillR)
N <- read_delim("Nmin.csv", ";", escape_double = FALSE, 
                        locale = locale(decimal_mark = ",", grouping_mark = ""), 
                        trim_ws = TRUE)
N[1] <- NULL
N[4] <- NULL
N[6] <- NULL
N[6] <- NULL
names(N)[5]  <- "Nmin"
names(N)[4]  <- "depth"
names(N)[1] <- "date"
Fe2 <- filter(N, treatment == "6")
Ch2 <- filter(N, treatment == "5")
N <- bind_rows(Fe2, Ch2)
N$treatment[N$treatment == "6"]<- "Fe"
N$treatment[N$treatment == "5"]<- "Ch"
N <- separate(N, date, sep = "-", into =c("Year", "Month", "Day"))
N <- make_JDay(N)
N[2:3] <- NULL
N$Year[N$Year == "2013"]<- "2013 Precrop"
N$Year[N$Year == "2014"]<- "2014 Spring Barley"
N$Year[N$Year == "2015"]<- "2015 S. Oilseed Rape"
N$Year[N$Year == "2016"]<- "2016 Winter Barley"
N$Year[N$Year == "2017"]<- "2017 Oats"


#JDay Solution-----------------------
# make data
value <- rnorm(365, mean = 0, sd = 5)
jday <- 1:365 # represents your Julian days 1-365; assumes no leap years

# make data frame and add julian day
d <- data.frame(jday = jday, value = value, stringsAsFactors = FALSE)
head(d)

#Backup----------
N0 <- N
N <- N0

# Total Nmin for different depth <30 / >30 throughout the year---------------------------------------------------

N_d <- N %>% group_by(JDay, Year, treatment, depth) %>% 
  summarise(Nmin=mean(Nmin))

ggplot(N_d, aes(x = as.Date(JDay, origin = as.Date("2013-01-01")), y = Nmin, 
                   colour = treatment,
                   group = treatment)) + 
  geom_point() + geom_line() +
  facet_grid(cols = vars(Year), rows = vars(depth)) +
  scale_colour_manual(values = c("brown4", "brown2"), name = "Treatment",
                      labels = c( "Ch", "Fe"))+
  labs(x = "", y = "Nmin [kg * "~ha^-1 ~"]") +
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

