library(tidyverse)
library(readxl)
library(chillR)

#downloading the table
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

#calculating total RLD for all dfs
df1[11:22] <- NULL
df1[6] <- NULL
names(df1)[9]<-"RLD"

df2[14:22] <- NULL
df2[12:13] <- NULL
df2[10] <- NULL
df2[6] <- NULL
names(df2)[9]<-"RLD"

df3[12:22] <- NULL
df3[10] <- NULL
df3[6] <- NULL
names(df3)[9]<-"RLD"

df4[12:22] <- NULL
df4[10] <- NULL
df4[6] <- NULL
names(df4)[9]<-"RLD"

#df5 is TrialC
df5[12:22] <- NULL
df5[10] <- NULL
df5[6] <- NULL
names(df5)[9]<-"RLD"

df6[11:22] <- NULL
df6[6] <- NULL
names(df6)[9]<-"RLD"

#df7: RLU total seems to be shifted 1 column to the right here -> RLU bulk = RLU total
#calculating RLD out of RLU total first. RLU/2x5x5x0.5 = RLD
names(df7)[15]<-"RLU"
df7 <- mutate(df7, RLD = RLU/(2*5*5*0.5))

df7[10:22] <- NULL
df7[6] <- NULL

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
t1 <- filter(df, treatment == 1)
t2 <- filter(df, treatment == 2)
t3 <- filter(df, treatment == 3)
t4 <- filter(df, treatment == 4)
t5 <- filter(df, treatment == 5)
t6 <- filter(df, treatment == 6)
t7 <- filter(df, treatment == 7)
t8 <- filter(df, treatment == 8)
t9 <- filter(df, treatment == 9)
t10 <- filter(df, treatment == 10)
t11 <- filter(df, treatment == 11)
t12 <- filter(df, treatment == 12)
t13 <- filter(df, treatment == 13)
t14 <- filter(df, treatment == 14)
t15 <- filter(df, treatment == 15)
t16 <- filter(df, treatment == 16)
t17 <- filter(df, treatment == 17)
t18 <- filter(df, treatment == 18)
t19 <- filter(df, treatment == 19)
t20 <- filter(df, treatment == 20)
t21 <- filter(df, treatment == 21)
t22 <- filter(df, treatment == 22)
t23 <- filter(df, treatment == 23)
t24 <- filter(df, treatment == 24)
t25 <- filter(df, treatment == 25)
t26 <- filter(df, treatment == 26)
t27 <- filter(df, treatment == 27)

#no data for treatments 11, 16, 17, 25, 26, 27. treatment 14, 22, 23, 24 have low row numbers
#treatments 15 and 21 have very high observation numbers
t1$precrop <- rep(1, nrow(t1))
t1$precrop_d <- rep(1, nrow(t1))
t2$precrop <- rep(1, nrow(t2))
t2$precrop_d <- rep(1, nrow(t2))
t3$precrop <- rep(1, nrow(t3))
t3$precrop_d <- rep(1, nrow(t3))

t4$precrop <- rep(1, nrow(t4))
t4$precrop_d <- rep(2, nrow(t4))
t5$precrop <- rep(1, nrow(t5))
t5$precrop_d <- rep(2, nrow(t5))
t6$precrop <- rep(1, nrow(t6))
t6$precrop_d <- rep(2, nrow(t6))

t7$precrop <- rep(1, nrow(t7))
t7$precrop_d <- rep(3, nrow(t7))
t8$precrop <- rep(1, nrow(t8))
t8$precrop_d <- rep(3, nrow(t8))
t9$precrop <- rep(1, nrow(t9))
t9$precrop_d <- rep(3, nrow(t9))

t10$precrop <- rep(2, nrow(t10))
t10$precrop_d <- rep(1, nrow(t10))
t11$precrop <- rep(2, nrow(t11))
t11$precrop_d <- rep(1, nrow(t11))
t12$precrop <- rep(2, nrow(t12))
t12$precrop_d <- rep(1, nrow(t12))

t13$precrop <- rep(2, nrow(t13))
t13$precrop_d <- rep(2, nrow(t13))
t14$precrop <- rep(2, nrow(t14))
t14$precrop_d <- rep(2, nrow(t14))
t15$precrop <- rep(2, nrow(t15))
t15$precrop_d <- rep(2, nrow(t15))

t16$precrop <- rep(2, nrow(t16))
t16$precrop_d <- rep(3, nrow(t16))
t17$precrop <- rep(2, nrow(t17))
t17$precrop_d <- rep(3, nrow(t17))
t18$precrop <- rep(2, nrow(t18))
t18$precrop_d <- rep(3, nrow(t18))

t19$precrop <- rep(3, nrow(t19))
t19$precrop_d <- rep(1, nrow(t19))
t20$precrop <- rep(3, nrow(t20))
t20$precrop_d <- rep(1, nrow(t20))
t21$precrop <- rep(3, nrow(t21))
t21$precrop_d <- rep(1, nrow(t21))

t22$precrop <- rep(3, nrow(t22))
t22$precrop_d <- rep(2, nrow(t22))
t23$precrop <- rep(3, nrow(t23))
t23$precrop_d <- rep(2, nrow(t23))
t24$precrop <- rep(3, nrow(t24))
t24$precrop_d <- rep(2, nrow(t24))

t25$precrop <- rep(3, nrow(t25))
t25$precrop_d <- rep(3, nrow(t25))
t26$precrop <- rep(3, nrow(t26))
t26$precrop_d <- rep(3, nrow(t26))
t27$precrop <- rep(3, nrow(t27))
t27$precrop_d <- rep(3, nrow(t27))

df1 <- bind_rows(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, 
                t19, t20, t21, t22, t23, t24, t25, t26, t27)

df1 <- drop_na(df1, RLD)

#3. calculation of mean values ####
#calculating means for each plot, year, date and depth
df11 <- df1 %>% group_by(Trial, crop, JDay, Year, treatment, plot_ID, depth, precrop, precrop_d) %>% 
  summarise(mean_RLD = mean(RLD))

#calculating means for each treatment, year, date and depth
df12 <- df11 %>% group_by(Trial, crop, JDay, Year, treatment, depth, precrop, precrop_d) %>% 
  summarise(mean_RLD2 = mean(mean_RLD))

#filtering the last date on which measurements were taken 
#(alternatively we could calculate means for all dates?)
df13 <- df12 %>% group_by(Trial, crop, Year, treatment, depth, precrop, precrop_d) %>% filter(JDay == max(JDay))
df13 <- drop_na(df13, mean_RLD2)

#splitting df into TrialA and TrialB
df13$precrop_d[df13$precrop_d == "1"] <- "1 Precrop Year"
df13$precrop_d[df13$precrop_d == "2"] <- "2 Precrop Years"
df13$precrop_d[df13$precrop_d == "3"] <- "3 Precrop Years"

TrialA <- filter(df13, Trial == "Trial A")
trialA <- filter(df13, Trial == "trial A")
TrialB <- filter(df13, Trial == "Trial B")
trialB <- filter(df13, Trial == "trial B")

trialB <- bind_rows(trialB, TrialB)

#4. plotting ####
trialB <- transform(trialB, depth = as.numeric(depth), 
                    precrop = as.factor(precrop), 
                    precrop_d = as.factor(precrop_d))

trialA <- transform(trialA, depth = as.numeric(depth), 
                    precrop = as.factor(precrop), 
                    precrop_d = as.factor(precrop_d))

ggplot(trialA, aes(x = depth, y = mean_RLD2, colour = precrop)) + 
  geom_line() +
  facet_grid(Year + crop ~ precrop_d) +
  labs(x = bquote("Soil Depth [cm]"), y= "Rootlength Density [cm *" ~cm^-3 ~"]") +
  theme_bw() +
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

ggplot(trialB, aes(x = depth, y = mean_RLD2, colour = precrop)) + 
  geom_line() +
  facet_grid(Year + crop ~ precrop_d) +
  labs(x = bquote("Soil Depth [cm]"), y= "Rootlength Density [cm *" ~cm^-3 ~"]") +
  theme_bw() +
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



