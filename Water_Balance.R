library(tidyverse)
library(readxl)

df08 <- read_excel("days_2008_calc korrig.xls", skip = 34)
df09 <- read_excel("days_2009_calc korrig.xls", skip = 34)
df10 <- read_excel("days_2010_calc korrig.xls", skip = 34)
df11 <- read_excel("days_2011_calc korrig.xls", skip = 41)
df12 <- read_excel("days_2012_calc korrig.xls", skip = 41)
df13 <- read_excel("days_2013_calc korrig.xlsx", skip = 41)
df14 <- read_excel("days_2014_calc korrig.xlsx", skip = 41)
df15 <- read_excel("days_2015_calc korrig.xlsx", skip = 41)
df16 <- read_excel("days_2016_calc korrig.xlsx", skip = 41)
df17 <- read_excel("days_2017_calc korrig.xlsx", skip = 41)
df18 <- read_excel("days_2018_calc korrig.xlsx", skip = 41)
df19 <- read_excel("days_2019_calc korrig.xlsx", skip = 41)
df20 <- read_excel("days_2020_calc korrig.xlsx", skip = 41)

names(df08)[20] <- "wasserbilanz"
names(df09)[20] <- "wasserbilanz"
names(df10)[20] <- "wasserbilanz"
names(df11)[20] <- "wasserbilanz"
names(df12)[20] <- "wasserbilanz"
names(df13)[20] <- "wasserbilanz"
names(df14)[20] <- "wasserbilanz"
names(df15)[20] <- "wasserbilanz"
names(df16)[20] <- "wasserbilanz"
names(df17)[20] <- "wasserbilanz"
names(df18)[20] <- "wasserbilanz"
names(df19)[20] <- "wasserbilanz"
names(df20)[20] <- "wasserbilanz"

df08[2:19] <- NULL
df08[3:22] <- NULL
df09[2:19] <- NULL
df09[3:22] <- NULL
df10[2:19] <- NULL
df10[3:22] <- NULL
df11[2:19] <- NULL
df11[3:22] <- NULL
df12[2:19] <- NULL
df12[3:22] <- NULL
df13[2:19] <- NULL
df13[3:22] <- NULL
df14[2:19] <- NULL
df14[3:22] <- NULL
df15[2:19] <- NULL
df15[3:22] <- NULL
df16[2:19] <- NULL
df16[3:22] <- NULL
df17[2:19] <- NULL
df17[3:22] <- NULL
df18[2:19] <- NULL
df18[3:23] <- NULL
df19[2:19] <- NULL
df19[3:23] <- NULL
df20[2:19] <- NULL
df20[3:23] <- NULL

#values between 08-11.02.2013 are missing and wont get removed with na.omit
#removing them here
df13 <- df13[-c(39, 40, 41, 42), ]

df08 <- separate(df08, date, sep = "-", into =c("Year", "Month", "Day"))
df09 <- separate(df09, date, sep = "-", into =c("Year", "Month", "Day"))
df10 <- separate(df10, date, sep = "-", into =c("Year", "Month", "Day"))
df11 <- separate(df11, date, sep = "-", into =c("Year", "Month", "Day"))
df12 <- separate(df12, date, sep = "-", into =c("Year", "Month", "Day"))
df13 <- separate(df13, date, sep = "-", into =c("Year", "Month", "Day"))
df14 <- separate(df14, date, sep = "-", into =c("Year", "Month", "Day"))
df15 <- separate(df15, date, sep = "-", into =c("Year", "Month", "Day"))
df16 <- separate(df16, date, sep = "-", into =c("Year", "Month", "Day"))
df17 <- separate(df17, date, sep = "-", into =c("Year", "Month", "Day"))
df18 <- separate(df18, date, sep = "-", into =c("Year", "Month", "Day"))
df19 <- separate(df19, date, sep = "-", into =c("Year", "Month", "Day"))
df20 <- separate(df20, date, sep = "-", into =c("Year", "Month", "Day"))

df08$wasserbilanz <- as.numeric(df08$wasserbilanz)
df09$wasserbilanz <- as.numeric(df09$wasserbilanz)
df10$wasserbilanz <- as.numeric(df10$wasserbilanz)
df11$wasserbilanz <- as.numeric(df11$wasserbilanz)
df12$wasserbilanz <- as.numeric(df12$wasserbilanz)
df13$wasserbilanz <- as.numeric(df13$wasserbilanz)
df14$wasserbilanz <- as.numeric(df14$wasserbilanz)
df15$wasserbilanz <- as.numeric(df15$wasserbilanz)
df16$wasserbilanz <- as.numeric(df16$wasserbilanz)
df17$wasserbilanz <- as.numeric(df17$wasserbilanz)
df18$wasserbilanz <- as.numeric(df18$wasserbilanz)
df19$wasserbilanz <- as.numeric(df19$wasserbilanz)
df20$wasserbilanz <- as.numeric(df20$wasserbilanz)

df <- bind_rows(df08, df09, df10, df11, df12,df12, df13, df14, df15, df16, df17, df18, df19, df20)
df <- na.omit(df)

#monthly
df_months <- df %>% group_by(Year, Month) %>% summarise(sum_wasserbilanz = sum(wasserbilanz))

ggplot(df_months, aes(x = Month, y = sum_wasserbilanz)) +
  geom_col(fill = "#0072B2", color = "black") +
  scale_fill_manual(values = c("#0072B2")) +
  labs(x = "Months", y = bquote("Monthly Climatic Water Balance [mm]")) + 
  facet_wrap(vars(Year)) +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                              "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13))

#with 3 month-categories
df$Month <- as.numeric(df$Month)

df_cat <- df %>% mutate(group = case_when(Month == 1 ~ "1",
                                          Month == 2 ~ "1",
                                          Month == 3 ~ "2",
                                          Month == 4 ~ "2",
                                          Month == 5 ~ "2",
                                          Month == 6 ~ "3",
                                          Month == 7 ~ "3",
                                          Month == 8 ~ "Na",
                                          Month == 9 ~ "Na",
                                          Month == 10 ~ "1",
                                          Month == 11 ~ "1",
                                          Month == 12 ~ "1",
                                          ))

df_cat <- df_cat %>% group_by(Year, group) %>% summarise(sum_wasserbilanz = sum(wasserbilanz))

df_cat <- df_cat[!(df_cat$group == "Na"),] 

ggplot(df_cat, aes(x = group, y = sum_wasserbilanz)) +
  geom_col(fill = "#0072B2", color = "black") +
  scale_fill_manual(values = c("#0072B2")) +
  labs(x = "Months", y = bquote("Climatic Water Balance [mm]")) + 
  facet_wrap(vars(Year)) +
  scale_x_discrete(labels = c("Okt-Feb", "Mar-May", "Jun-Jul")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13))

