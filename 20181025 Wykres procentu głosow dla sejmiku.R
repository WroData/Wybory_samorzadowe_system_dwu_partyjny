
setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\20181022 Wyniki wyborów samorz¹dowyh - spadek poparcia partii alternatywnych\\")



options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "en")
options(scipen=999)

##########################################################################
#### upewnienie siê ¿e nie ma ¿adnych pakietów za³adowanych ####
gc(reset = TRUE)
rm(list = ls())
#od³¹czeni wszytkich pakietów - stowrzebnuie funkcji
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo³anie funkcji 


##########################################################################
#### za³adowanie pakietów #### 
library(dplyr)
library(ggplot2)
library(extrafont) #do czcionek
library(scales)


loadfonts(device="win")


##########################################################################
#### wczytanie danych #### 
load("dane//wyniki_komitetow_sejmik_2010 2018-10-25 .RData")
load("dane//wyniki_komitetow_sejmik_2014 2018-10-25 .RData")
load("dane//wyniki_komitetow_sejmik_2018 2018-10-25 .RData")


POPIS <- c("KOMITET WYBORCZY PLATFORMA OBYWATELSKA RP"
           ,"KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŒÆ"
           ,"KW Prawo i Sprawiedliwoœæ"
           ,"KW Platforma Obywatelska RP"
           ,"KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŒÆ"
           ,"KOALICYJNY KOMITET WYBORCZY PLATFORMA.NOWOCZESNA KOALICJA OBYWATELSKA"
)


##########################################################################
#### Poczatkowe obliczenia #### 

# 2018
wyniki_komitetow_sejmik_2018_Zbiorcze <- wyniki_komitetow_sejmik_2018 %>%
  mutate(POPIS = ifelse(Komitet %in% POPIS, "POPIS", "Inne")) %>%
  group_by(POPIS) %>%
  summarise(n_glosow = sum(n_of_votes, na.rm = T)) %>%
  mutate(pct_glosow  = n_glosow / sum(n_glosow, na.rm = T),
         data = "2018-10-21",
         rejon = "Polska")


wyniki_komitetow_sejmik_2018_DL <- wyniki_komitetow_sejmik_2018 %>%
  filter(Wojewódzctwo == "dolnoœl¹skie") %>%
  mutate(POPIS = ifelse(Komitet %in% POPIS, "POPIS", "Inne")) %>%
  group_by(POPIS) %>%
  summarise(n_glosow = sum(n_of_votes, na.rm = T)) %>%
  mutate(pct_glosow  = n_glosow / sum(n_glosow, na.rm = T),
         data = "2018-10-21",
         rejon = "Dolny Œl¹sk")

# 2014
wyniki_komitetow_sejmik_2014_Zbiorcze <- wyniki_komitetow_sejmik_2014 %>%
  mutate(POPIS = ifelse(Komitet %in% POPIS, "POPIS", "Inne")) %>%
  group_by(POPIS) %>%
  summarise(n_glosow = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow  = n_glosow / sum(n_glosow, na.rm = T),
         data = "2014-11-16",
         rejon = "Polska")

wyniki_komitetow_sejmik_2014_DL <- wyniki_komitetow_sejmik_2014 %>%
  filter(Wojewódzctwo == "dolnoœl¹skie") %>%
  mutate(POPIS = ifelse(Komitet %in% POPIS, "POPIS", "Inne")) %>%
  group_by(POPIS) %>%
  summarise(n_glosow = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow  = n_glosow / sum(n_glosow, na.rm = T),
         data = "2014-11-16",
         rejon = "Dolny Œl¹sk")

#2010
wyniki_komitetow_sejmik_2010_Zbiorcze <- wyniki_komitetow_sejmik_2010 %>%
  mutate(POPIS = ifelse(Komitet %in% POPIS, "POPIS", "Inne")) %>%
  group_by(POPIS) %>%
  summarise(n_glosow = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow  = n_glosow / sum(n_glosow, na.rm = T),
         data = "2010-11-21",
         rejon = "Polska")


wyniki_komitetow_sejmik_2010_DL <- wyniki_komitetow_sejmik_2010 %>%
  filter(Wojewódzctwo == "dolnoœl¹skie") %>%
  mutate(POPIS = ifelse(Komitet %in% POPIS, "POPIS", "Inne")) %>%
  group_by(POPIS) %>%
  summarise(n_glosow = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow  = n_glosow / sum(n_glosow, na.rm = T),
         data = "2010-11-21",
         rejon = "Dolny Œl¹sk")


Dane <- rbind(wyniki_komitetow_sejmik_2018_Zbiorcze,
              wyniki_komitetow_sejmik_2018_DL,
              wyniki_komitetow_sejmik_2014_Zbiorcze,
              wyniki_komitetow_sejmik_2014_DL,
              wyniki_komitetow_sejmik_2010_Zbiorcze,
              wyniki_komitetow_sejmik_2010_DL)

names(Dane)
Dane_W <- Dane %>%
  filter(POPIS == "POPIS") %>%
  mutate(data = (as.Date(data)),
         Rok  = format(data, "%Y"))

Breakes <- as.Date(c("2010-11-21", "2014-11-16", "2018-10-21"))

##########################################################################
#### Wykresy #### 


Theme <-  theme(legend.position="bottom",
                legend.key.width = unit(1,"cm"),
                legend.title = element_blank(),
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.text       = element_text(family = "Ubuntu", size = 12, hjust = 0, color = "#22211d"),
                #axis.title   = element_text(family = "Ubuntu", size = 14, color = "#22211d"),
                #axis.title   = element_blank(),
                #axis.text.y  = element_blank(),
                #axis.ticks.y = element_blank(),
                
                #axis.ticks.x = element_text(family = "Ubuntu", size = 11, color = "#22211d"),
                text = element_text(family = "Ubuntu", size = 10, color = "#22211d"),
                axis.title   = element_text(family = "Ubuntu", size = 13, color = "#22211d"),
                axis.text.x  = element_text(family = "Ubuntu", size = 11, color = "#22211d"),
                axis.text.y  = element_text(family = "Ubuntu", size = 11, color = "#22211d"),
                strip.text.x = element_text(family = "Ubuntu", size = 12, color = "#22211d"),
                #panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
                panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
                plot.title    = element_text(family = "Ubuntu", size = 20,  hjust = 0.5,  color = "#4e4d47"),
                plot.subtitle = element_text(family = "Ubuntu", size = 13,  hjust = 0.01,  face = "italic", color = "#4e4d47"),
                plot.caption  = element_text(family = "Ubuntu", size = 11,  hjust = 0.99, color = "#4e4d47"),
                panel.border  = element_blank()
)  




names(Dane_W)
W <-
  ggplot(data=Dane_W, aes(y = pct_glosow, x = Rok, fill = rejon)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(
    values = c("#fe9929", "#d95f0e"),
    breaks = c("Dolny Œl¹sk", "Polska"),
    labels = c("Dolny Œl¹sk", "Polska")) +
  labs(title = paste0("Czy Polska zmierza do systemu dwu-partyjnemu?"),
       subtitle = paste0("Procent g³osów wa¿nych oddanych na PO i PiS w wyborach do sejmików wojewódzkich"),
       x = "",
       y = "",
       caption = "Autor: WroData | Na podstawie danych z pkw.gov.pl" ) +
  scale_y_continuous(labels  = percent,
                     breaks  = seq(from = 0, to = 1, by = 0.2)) +
  coord_cartesian(ylim = c(0, 0.6)) +
  #scale_x_date(date_labels = "%Y")   +
  Theme 
#+
#  guides(color = guide_legend(override.aes = list(size = 4 ))) 
  
plot(W)


# zapis wykresu
a <- 6
png(filename = paste("W ", Sys.Date(), ".png", sep=""),
    bg="white", width = a * 1.5, height = a * 1, units = 'in', res = 150)
    plot(W)
dev.off()


