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

df1$crop[df1$crop == "rapeseed"] <- "winter oilseed rape"

#3. Splitting the df into TrialA and TrialB ####
TrialA <- filter(df1, Trial == "Trial A")
trialA <- filter(df1, Trial == "trial A")
TrialB <- filter(df1, Trial == "Trial B")
trialB <- filter(df1, Trial == "trial B")

trialB <- bind_rows(trialB, TrialB)
trialA <- bind_rows(trialA, TrialA)


#4. Plot1: TrialA - Lu2 and Ch2 depth differentiated during flowering ####
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
  summarise(mean_RLD = mean(RLD))

#calculating means for each treatment, year, date and depth
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth, precrop, precrop_d) %>% 
  summarise(mean_RLD2 = mean(mean_RLD))

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

ggplot(plot1, aes(x = depth, y = mean_RLD2, colour = precrop)) + 
  geom_line() +
  facet_grid(precrop_d ~ Year + crop) +
  labs(x = bquote("Soil Depth [cm]"), y= "Rootlength Density [cm *" ~cm^-3 ~"]", 
       title = "Trial A") +
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


#5. Plot2: TrialB - Fe2 depth differentiated during flowering ####
#finding the last measurement date
fm_plot1 <- filter(trialB, Year == "2012" & crop == "fodder mallow")
fm_plot1 <- filter(fm_plot1, JDay == 198)

sw_plot1 <- filter(trialB, Year == "2012" & crop == "spring wheat")
sw_plot1 <- filter(sw_plot1, JDay == 198)

wb_plot1 <- filter(trialB, Year == "2013" & crop == "winter barley")
wb_plot1 <- filter(wb_plot1, JDay == 170)

wosr_plot1 <- filter(trialB, Year == "2013" & crop == "winter oilseed rape")
wosr_plot1 <- filter(wosr_plot1, JDay == 177)

plot1 <- bind_rows(fm_plot1, sw_plot1, wb_plot1, wosr_plot1)

#calculating means for each plot, maincrop, date and depth
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth, precrop, precrop_d) %>% 
  summarise(mean_RLD = mean(RLD))

#calculating means for each treatment, year, date and depth
plot1 <- plot1 %>% group_by(crop, JDay, Year, depth, precrop, precrop_d) %>% 
  summarise(mean_RLD2 = mean(mean_RLD))

#filtering Lu1+2 and Ch1+2 and their last measurement date (should be flowering????????)
Lu2 <- filter(plot1, precrop == 1 & precrop_d == 2)
Ch2 <- filter(plot1, precrop == 2 & precrop_d == 2)
Fe2 <- filter(plot1, precrop == 3 & precrop_d == 2)

plot1 <- bind_rows(Lu2, Ch2, Fe2)

#removing 2016 and 2017 since no data for precrops 2 and 3
plot1 <- plot1[!(plot1$Year == "2016"),]
plot1 <- plot1[!(plot1$Year == "2017"),]

plot1$precrop_d[plot1$precrop_d == "2"] <- "2 Precrop Years"
plot1$precrop[plot1$precrop == "1"] <- "Lu"
plot1$precrop[plot1$precrop == "2"] <- "Ch"
plot1$precrop[plot1$precrop == "3"] <- "Fe"

plot1 <- transform(plot1, depth = as.numeric(depth), 
                   precrop = as.factor(precrop), 
                   precrop_d = as.factor(precrop_d))

ggplot(plot1, aes(x = depth, y = mean_RLD2, colour = precrop)) + 
  geom_line() +
  facet_grid(precrop_d ~ Year + crop) +
  labs(x = bquote("Soil Depth [cm]"), y= "Rootlength Density [cm *" ~cm^-3 ~"]", 
       title = "Trial B") +
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

#6. Plot3: TrialA - Ch2 and Fe1 on different dates ####
#creating two depth groups: <30cm and >30cm
trialA <- transform(trialA, depth = as.numeric(depth))

trialA <- trialA %>% mutate(depth_class = case_when(depth <= 30 ~ "1", 
                                                    depth > 30 ~ "2") )
trialA <- transform(trialA, depth_class = as.factor(depth_class))

#filtering Lu1+2 and Ch1+2 
plot1 <- trialA

Ch2 <- filter(plot1, precrop == 2 & precrop_d == 2)
Fe1 <- filter(plot1, precrop == 3 & precrop_d == 1)

plot1 <- bind_rows(Ch2, Fe1)

#filtering the dates to check for problems

df1 <- filter(plot1, Year == "2010" & JDay == 208 & crop == "spring wheat")#way more data
df1 <- filter(plot1, Year == "2010" & JDay == 215 & crop == "spring wheat")#monolith -> raus!
plot1 <- plot1[!(plot1$crop == "spring wheat" & plot1$JDay == 215),] #removing monolith

#calculating means for each plot, maincrop, date and !!depth_class!!
plot1 <- plot1 %>% group_by(crop, JDay, Year, plot_ID, depth_class, precrop, precrop_d) %>% 
  summarise(mean_RLD = mean(RLD))

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

plot1$depth_class[plot1$depth_class == "1"] <- "<= 30cm"
plot1$depth_class[plot1$depth_class == "2"] <- "> 30cm"
plot1$precrop[plot1$precrop == "2"] <- "Ch2"
plot1$precrop[plot1$precrop == "3"] <- "Fe1"

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



#7. Plot4: TrialB - Ch2 and Fe2 on different dates ####
trialB <- transform(trialB, depth = as.numeric(depth))

trialB <- trialB %>% mutate(depth_class = case_when(depth <= 30 ~ "1", 
                                                    depth > 30 ~ "2") )
trialB <- transform(trialB, depth_class = as.factor(depth_class))

#calculating means for each plot, maincrop, date and !!depth_class!!
plot1 <- trialB %>% group_by(crop, JDay, Year, plot_ID, depth_class, precrop, precrop_d) %>% 
  summarise(mean_RLD = mean(RLD))

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

ggplot(plot1, aes(x = as.Date(JDay, origin = as.Date("2012-01-01")), y = mean_RLD2, colour = precrop, linetype = depth_class, shape = precrop)) + 
  geom_line() + geom_point() +
  facet_grid(cols = vars(Year, crop)) +
  labs(y = "Mean Rootlength Density [cm *" ~cm^-3 ~"]", 
       title = "Trial B") +
  theme_bw() +
  scale_colour_manual(values = c("red1", "steelblue1")) + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 15), 
        strip.text.y = element_text(size = 13), 
        strip.text.x = element_text(size = 13),
        legend.position = c(0.09, 0.82),
        legend.text = element_text(size = 12),
        legend.title=element_text(size=14))









