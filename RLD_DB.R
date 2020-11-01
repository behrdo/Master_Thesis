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
df7 <- slice(RLD, 108279:122201)

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

#transforming the date into JDay to make further measures easier
names(df)[2]<-"date"
df <- separate(df, date, sep = "-", into =c("Year", "Month", "Day"))
df <- make_JDay(df)












