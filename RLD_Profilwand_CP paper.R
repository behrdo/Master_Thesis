library(tidyr)
library(dplyr)
library(ggplot2)
library (DunnettTests)
library (DescTools)
library("agricolae")

#Thema WLD erstellen
Thema_WLD <- theme(panel.background = element_rect(fill = "white"),
                   axis.line = element_line())


#Vorbereitungen------------
#ursprüngliche Datentabelle einlesen
Rohdaten_PW <- read.csv2("PW FV I-2 2019 CP 2.csv")

#RLD in bulk, Bioporen und gesamt berechnen
#Rohdaten_PW$RLD_bulk <- Rohdaten_PW$RLU.bulk / 12.5/2
#Rohdaten_PW$RLD_BP <- Rohdaten_PW$RLU.BP / 12.5/2
#Rohdaten_PW$RLD_total <- Rohdaten_PW$RLD_BP+Rohdaten_PW$RLD_bulk

Rohdaten_PW$RLDtotal_cmcm3 <- as.numeric(Rohdaten_PW$RLDtotal_cmcm3)
Rohdaten_PW$RLDBP_cmcm3 <- as.numeric(Rohdaten_PW$RLDBP_cmcm3)


#Anteil Wurzeln in Bioporen berechnen
Rohdaten_PW$share_RLD_BP <- Rohdaten_PW$RLDBP_cmcm3 / Rohdaten_PW$RLDtotal_cmcm3 * 100

neworder <- c("at","near","far", "none")
Rohdaten_PW2 <- arrange(mutate(Rohdaten_PW,
                               position_=factor(position,levels=neworder)),position)

Rohdaten_PW2$position <- forcats::fct_explicit_na(Rohdaten_PW2$position)

#Datenverarbeitung: Daten genauer anschauen--------------

#Durchschnitt und Standardabweichung der Abstände pro Abstandsklasse und Parzelle
Durchschnitt_pro_Abstandsklasse <- Rohdaten_PW2 %>%
  group_by(position, fieldrep,plot, treatment, depth, Counting) %>%
  summarise(mean_RLD_cm_cm3_Abstaende=mean(RLDtotal_cmcm3, na.rm=TRUE), SD_RLD_cm_cm3_Abstaende=sd(RLDtotal_cmcm3, na.rm=TRUE), Anzahl_Abstaende=length(RLDtotal_cmcm3))

Durchschnitt_pro_Abstandsklasse$fieldrep<-as.character(Durchschnitt_pro_Abstandsklasse$fieldrep)
Durchschnitt_pro_Abstandsklasse$treatment<-as.character(Durchschnitt_pro_Abstandsklasse$treatment)
Durchschnitt_pro_Abstandsklasse$position<-as.character(Durchschnitt_pro_Abstandsklasse$position)

#Durchschnitt und Standardabweichung Variante, Tiefe 
Durchschnitt_Variante_Tiefe <- Durchschnitt_pro_Abstandsklasse %>%
  group_by(position,treatment,depth,Anzahl_Abstaende, Counting) %>% 
  summarize(Anzahl_Parzellen=length(mean_RLD_cm_cm3_Abstaende),mean_RLD_cm_cm3=mean(mean_RLD_cm_cm3_Abstaende), SD_RLD_cm_cm3=sd(SD_RLD_cm_cm3_Abstaende))

Durchschnitt_Variante_Tiefe$treatment<-as.character(Durchschnitt_Variante_Tiefe$treatment)


#Abbildungen: Auswertung der TLK-Varianten auf und neben der Melioration-----------

#Datenverarbeitung: Vergleich der Varianten innerhalb der Tiefen- und Abstandsklassen

#Schlüssel zu den Tiefenklassen einlesen
Schluessel <- read.csv2("Schluessel PW FV I-2 2019.csv",
                        stringsAsFactors = FALSE)

#Schluessel und Daten verschneiden
Rohdaten_PW_gesamt <- full_join(Rohdaten_PW2, Schluessel)

#Durchschnitt und Standardabweichung der Kästchen 
#pro Abstandsklasse und Tiefenklasse und Parzelle

Rohdaten_PW_gesamt$position <- forcats::fct_explicit_na(Rohdaten_PW_gesamt$position)

Durchschnitt_Abstandsklasse_Tiefenklasse <- Rohdaten_PW_gesamt %>%
  group_by(fieldrep,plot,treatment,Tiefenklasse,depth,position, Counting) %>%
  summarise(mean_RLD_cm_cm3_square=mean(RLDtotal_cmcm3, na.rm=TRUE), SD_RLD_cm_cm3_square=sd(RLDtotal_cmcm3, na.rm=TRUE), Anzahl_square=length(RLDtotal_cmcm3))
Durchschnitt_Abstandsklasse_Tiefenklasse_Parzelle <- Durchschnitt_Abstandsklasse_Tiefenklasse %>%
  group_by(treatment,Tiefenklasse,depth,position, Counting) %>%
  summarise(mean_RLD_cm_cm3=mean(mean_RLD_cm_cm3_square), SD_RLD_cm_cm3=sd(mean_RLD_cm_cm3_square), Anzahl_Parzellen=length(mean_RLD_cm_cm3_square))

Durchschnitt_Abstandsklasse_Tiefenklasse_Parzelle$treatment<-as.factor(Durchschnitt_Abstandsklasse_Tiefenklasse_Parzelle$treatment)


#Termin f?r die Abbildung ausw?hlen
Durchschnitt_Abstandsklasse_Tiefenklasse_Parzelle_date <-
  Durchschnitt_Abstandsklasse_Tiefenklasse_Parzelle[Durchschnitt_Abstandsklasse_Tiefenklasse_Parzelle$Counting == 2,]

ohnefar <- Durchschnitt_Abstandsklasse_Tiefenklasse_Parzelle_date[Durchschnitt_Abstandsklasse_Tiefenklasse_Parzelle_date$position!="far",]


#Farben der Varianten festlegen
Farben_Varianten <- c("1"="chartreuse1", "2"="royalblue1", "4"="tomato1", "7"="tomato4")

#line graph
tiff("WLD_Linien.tiff",
     width=15,height=10,unit="cm",res=400)
ggplot(ohnefar[(ohnefar$treatment == "1"|
                  ohnefar$treatment == "2"|
                  ohnefar$treatment == "4"|
                  ohnefar$treatment == "7"),],
       aes(x=depth,y=mean_RLD_cm_cm3, fill=treatment))+
  geom_line(aes(col=treatment))+
  #geom_bar(stat="identity",position="dodge", color="black", size=0.3)+
  facet_wrap(~position, ncol=2)+ 
  xlab("soil depth (cm)")+
  ylab("RLD (cm*cm-3)")+
  #geom_vline(xintercept = 30,linetype = 2)+
  Thema_WLD+
  scale_y_continuous(position = "right",breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     labels = c("0","0.2","0.4","0.6", "0.8", "1.0"),
                     limits=c(-0.15,1.0))+
  #geom_errorbar(aes(ymin=mean_RLD_cm_cm3-SD_RLD_cm_cm3,
                    #ymax=mean_RLD_cm_cm3+SD_RLD_cm_cm3),position=position_dodge(width=0.9))+
  scale_x_reverse(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180), labels=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100", "110", "120", "130", "140", "150", "160", "170", "180"))+
  coord_flip()+
  scale_color_manual(values = Farben_Varianten, labels=c("Control", "DL", "DLB", "DLG"))+
  theme(legend.title=element_blank(),
        legend.position="right",
        legend.text = element_text(size = 12),
        legend.spacing.x = unit(0.2, 'cm'))
dev.off()

#Kruskal-Wallis-Test und ANOVA

#Auswählen einer Position und einer Tiefenklasse 
Rohdaten_PW_gesamt_date <- Rohdaten_PW_gesamt[Rohdaten_PW_gesamt$Counting == "1",]
Rohdaten_PW_gesamt_date_TK <- Rohdaten_PW_gesamt_date[Rohdaten_PW_gesamt_date$depth == "30",]
Rohdaten_PW_gesamt_date_TK_position <- Rohdaten_PW_gesamt_date_TK[Rohdaten_PW_gesamt_date_TK$position == "at",]
Daten_aov <- Rohdaten_PW_gesamt_date_TK_position %>%
  group_by(fieldrep,treatment)%>%
  summarise (mean_RLD=mean(RLDtotal_cmcm3, na.rm=TRUE))
Daten_aov_Var <- Daten_aov[(Daten_aov$treatment==1|
                              Daten_aov$treatment==2|
                              Daten_aov$treatment==4|
                              Daten_aov$treatment==7),]
#transformieren
Daten_aov_Var$mean_RLD_ln <- log(Daten_aov_Var$mean_RLD)
Daten_aov_Var$mean_RLD_log10 <- log10(Daten_aov_Var$mean_RLD)
Daten_aov_Var$mean_RLD_sqrt <- sqrt(Daten_aov_Var$mean_RLD)
#ANOVA einfaktoriell
Daten_aov_Var$treatment <- as.factor (Daten_aov_Var$treatment)
Daten_aov_Var$fieldrep <- as.factor (Daten_aov_Var$fieldrep)
AOV_Daten_aov <- aov(mean_RLD ~ treatment + (fieldrep), data = Daten_aov_Var)
Tukey_Daten_aov <-HSD.test(AOV_Daten_aov, "treatment",
                           group = TRUE,
                           console = TRUE)
plot(fitted(AOV_Daten_aov), resid(AOV_Daten_aov))
qqnorm(residuals(AOV_Daten_aov))
qqline(residuals(AOV_Daten_aov))
summary(AOV_Daten_aov)
library(car)

leveneTest(mean_RLD ~ treatment, data = Daten_aov_Var)

#Dunnett Test
DunnettTest(x=Daten_aov_Var$mean_RLD, g=Daten_aov_Var$treatment, control="1")


##-------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#maximale Wurzeltiefe
#reduzierte Datentabelle einlesen
Rohdaten_PW_Tiefe <- read.csv2("PW FV I-2 2018 CP.csv")

Tiefe <- Rohdaten_PW_Tiefe[Rohdaten_PW_Tiefe$RLDtotal_cmcm3!=0,]
Tiefeohnefar <- Tiefe[Tiefe$position!="far",]


Tiefeohnefar$position <- as.character(Tiefeohnefar$position)

Tiefe_max <- Tiefeohnefar %>%
  
  group_by(Counting, position, treatment, fieldrep) %>%
  
  summarize("maxTiefe"=max(depth,na.rm=TRUE))

# Abbildung für max. Wurzeltiefe

#Datenvorbereitung
#Durchschnitt und Standardabweichung Variante, Tiefe 
Durchschnitt_Variante_Tiefe_max <- Tiefe_max %>%
  group_by(treatment, Counting, position) %>% 
  summarize(Anzahl_Parzellen=length(maxTiefe),mean_Tiefemax=mean(maxTiefe), SD_Tiefemax=sd(maxTiefe))

Durchschnitt_Variante_Tiefe_max$treatment<-as.character(Durchschnitt_Variante_Tiefe_max$treatment)

Durchschnitt_Variante_Tiefe_max_position <- Durchschnitt_Variante_Tiefe_max[Durchschnitt_Variante_Tiefe_max$position == "at",]

write.csv(Durchschnitt_Variante_Tiefe_max_position, "C:\\R\\R FV I\\Tiefe_max FV I-1 2018.csv", row.names = FALSE)

#für ANOVA----------------------

Tiefe_max_date <- Tiefe_max[Tiefe_max$Counting == "2",]
Tiefe_max_date_position <- Tiefe_max_date[Tiefe_max_date$position == "near",]

Daten_aov <- Tiefe_max_date_position%>%
    group_by(fieldrep,treatment)%>%
    summarise (mean_depth=mean(maxTiefe))

Daten_aov_Var <- Daten_aov[(Daten_aov$treatment=="1"|
                              Daten_aov$treatment=="2"|
                              Daten_aov$treatment=="4"|
                              Daten_aov$treatment=="7"),]

#transformieren
Daten_aov_Var$mean_depth_ln <- log(Daten_aov_Var$mean_depth)
Daten_aov_Var$mean_depth_log10 <- log10(Daten_aov_Var$mean_depth)
Daten_aov_Var$mean_depth_sqrt <- sqrt(Daten_aov_Var$mean_depth)


#ANOVA einfaktoriell

library("agricolae")

Daten_aov_Var$treatment <- as.factor (Daten_aov_Var$treatment)
Daten_aov_Var$fieldrep <- as.factor (Daten_aov_Var$fieldrep)

AOV_Daten_aov <- aov(mean_depth ~ treatment + (fieldrep), data = Daten_aov_Var)
Tukey_Daten_aov <-HSD.test(AOV_Daten_aov, "treatment",
                           group = TRUE,
                           console = TRUE)

plot(fitted(AOV_Daten_aov), resid(AOV_Daten_aov))
qqnorm(residuals(AOV_Daten_aov))
qqline(residuals(AOV_Daten_aov))
summary(AOV_Daten_aov)

leveneTest(mean_depth_sqrt ~ treatment, data = Daten_aov_Var)


#-----------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------



#kumulative Wurzellänge--------------

#Durchschnitt und Standardabweichung der K?stchen pro Parzelle

Rohdaten_PW_gesamt_datekum <- Rohdaten_PW_gesamt[Rohdaten_PW_gesamt$Counting == "1",]
Rohdaten_PW_gesamt_datekum_pos <- Rohdaten_PW_gesamt_datekum[Rohdaten_PW_gesamt_datekum$position == "at",]

Durchschnitt_pro_Parzelle <- Rohdaten_PW_gesamt_datekum_pos %>%
  group_by(treatment, fieldrep, depth) %>%
  summarise(RL=mean(RLDtotal_cmcm3, na.rm=TRUE))

Durchschnitt_pro_Parzelle_breit <- spread(Durchschnitt_pro_Parzelle, depth, RL)

Durchschnitt_pro_Parzelle_breit$RL_gesamt <- as.numeric(apply(Durchschnitt_pro_Parzelle_breit[,3:38], 1, sum,na.rm=TRUE))

Durchschnitt_pro_Parzelle_lang <- 
  gather (Durchschnitt_pro_Parzelle_breit,  "depth", "RL", 
          -c(treatment, fieldrep, RL_gesamt))

Durchschnitt_pro_Parzelle_lang$Anteil <- Durchschnitt_pro_Parzelle_lang$RL / Durchschnitt_pro_Parzelle_lang$RL_gesamt

Durchschnitt_pro_Parzelle_cum <-Durchschnitt_pro_Parzelle_lang %>%
  group_by(treatment, fieldrep) %>%
  mutate(cum_Anteil = cumsum(Anteil))

Durchschnitt_pro_Parzelle_cum <- as.data.frame(Durchschnitt_pro_Parzelle_cum)
Durchschnitt_pro_Parzelle_cum$depth <- as.numeric(Durchschnitt_pro_Parzelle_cum$depth)
Durchschnitt_pro_Parzelle_cum$treatment <- as.factor(Durchschnitt_pro_Parzelle_cum$treatment)

#Farben der Varianten festlegen
Farben_Varianten <- c("1"="chartreuse1", "2"="royalblue1", "4"="tomato1", "7"="tomato4")

ggplot(Durchschnitt_pro_Parzelle_cum,
       aes(x=depth, y=cum_Anteil, color=treatment))+
  geom_point( size=3)+
  scale_y_continuous(position = "right")+
  scale_x_reverse()+
  scale_color_manual(values = Farben_Varianten)+
  coord_flip()

Durchschnitt_Variante_cum <-Durchschnitt_pro_Parzelle_cum %>%
  group_by(treatment, depth) %>%
  summarize("mean_cum_Anteil"=mean(cum_Anteil,na.rm=TRUE))

tiff("WLD_kumulativ.tiff",
     width=12,height=10,unit="cm",res=200)
ggplot(Durchschnitt_Variante_cum,
       aes(x=depth, y=mean_cum_Anteil, color=treatment))+
  geom_line( size=1)+
  Thema_WLD+
  xlab("soil depth (cm)")+
  ylab("cumulative root distribution (%)")+
  scale_y_continuous(position = "right")+
  scale_x_reverse()+
  scale_color_manual(values = Farben_Varianten, labels=c("Control", "DL", "DLB", "DLG"))+
  coord_flip()
  theme(legend.title=element_blank(),
      legend.position="right",
      legend.text = element_text(size = 12),
      legend.spacing.x = unit(0.2, 'cm'))
dev.off()













