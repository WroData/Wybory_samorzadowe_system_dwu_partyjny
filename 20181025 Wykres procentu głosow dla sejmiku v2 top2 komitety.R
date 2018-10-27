
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
library(stringr) #do wrap labels



loadfonts(device="win")


##########################################################################
#### wczytanie danych #### 
load("dane//wyniki_komitetow_sejmik_1998 2018-10-25 .RData")
load("dane//wyniki_komitetow_sejmik_2002 2018-10-25 .RData")
load("dane//wyniki_komitetow_sejmik_2006 2018-10-25 .RData")
load("dane//wyniki_komitetow_sejmik_2010 2018-10-25 .RData")
load("dane//wyniki_komitetow_sejmik_2014 2018-10-25 .RData")
load("dane//wyniki_komitetow_sejmik_2018 2018-10-25 .RData")

Podpisy <- c("PiS+PO.N",       "PiS+PO.N",
             "PiS+PO",         "PO+PiS",
             "PO+PiS",         "PO+R.Dutkiewicz",
             "PO+PiS",         "PO+PiS",
             "SLDUP+Samoobrona", "SLDUP+Samoobrona",
             "AWS+SLD",        "SLD+AWS")

Korekta <- rev(
           c(-5, +0,
             +3, -3,
             +3, -3,
             -3, +3,
             +3, -3,
             -3, +3)) / 100

TOP_N <- 2


##########################################################################
#### Poczatkowe obliczenia #### 

# 2018
wyniki_komitetow_sejmik_2018_Zbiorcze <- wyniki_komitetow_sejmik_2018 %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(n_of_votes, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "2018-10-21",
         rejon = "Polska")


wyniki_komitetow_sejmik_2018_DL <- wyniki_komitetow_sejmik_2018 %>%
  filter(Wojewódzctwo == "dolnoœl¹skie") %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(n_of_votes, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "2018-10-21",
         rejon = "Dolny Œl¹sk")

# 2014
wyniki_komitetow_sejmik_2014_Zbiorcze <- wyniki_komitetow_sejmik_2014 %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "2014-11-16",
         rejon = "Polska")

wyniki_komitetow_sejmik_2014_DL <- wyniki_komitetow_sejmik_2014 %>%
  filter(Wojewódzctwo == "dolnoœl¹skie") %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "2014-11-16", 
         rejon = "Dolny Œl¹sk")

#2010
wyniki_komitetow_sejmik_2010_Zbiorcze <- wyniki_komitetow_sejmik_2010 %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "2010-11-21",
         rejon = "Polska")


wyniki_komitetow_sejmik_2010_DL <- wyniki_komitetow_sejmik_2010 %>%
  filter(Wojewódzctwo == "dolnoœl¹skie") %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "2010-11-21",
         rejon = "Dolny Œl¹sk")



#2006
wyniki_komitetow_sejmik_2006_Zbiorcze <- wyniki_komitetow_sejmik_2006 %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(N_glosow, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "2006-11-12",
         rejon = "Polska")


wyniki_komitetow_sejmik_2006_DL <- wyniki_komitetow_sejmik_2006 %>%
  filter(Wojewódzctwo == "dolnoœl¹skie") %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(N_glosow, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "2006-11-12",
         rejon = "Dolny Œl¹sk")


#2002
wyniki_komitetow_sejmik_2002_Zbiorcze <- wyniki_komitetow_sejmik_2002 %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(N_glosow, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "2002-10-27",
         rejon = "Polska")


wyniki_komitetow_sejmik_2002_DL <- wyniki_komitetow_sejmik_2002 %>%
  filter(Wojewódzctwo == "dolnoœl¹skie") %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(N_glosow, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "2002-10-27",
         rejon = "Dolny Œl¹sk")


#1998
wyniki_komitetow_sejmik_1998_Zbiorcze <- wyniki_komitetow_sejmik_1998 %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(Polska, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "1998-10-11",
         rejon = "Polska")


wyniki_komitetow_sejmik_1998_DL <- wyniki_komitetow_sejmik_1998 %>%
  group_by(Komitet) %>%
  summarise(n_glosow = sum(DolnyŒl¹sk, na.rm = T)) %>%
  mutate(pct_glosow = n_glosow / sum(n_glosow, na.rm = T)) %>%
  arrange(desc(pct_glosow)) %>%
  top_n(n = TOP_N) %>%
  summarise(pct_glosow = sum(pct_glosow, na.rm = T),
            partie = paste(Komitet, collapse = " ||| ")) %>%
  mutate(TOP_N = TOP_N,
         data = "1998-10-11",
         rejon = "Dolny Œl¹sk")


Dane_W <- rbind(wyniki_komitetow_sejmik_2018_Zbiorcze,
              wyniki_komitetow_sejmik_2018_DL,
              wyniki_komitetow_sejmik_2014_Zbiorcze,
              wyniki_komitetow_sejmik_2014_DL,
              wyniki_komitetow_sejmik_2010_Zbiorcze,
              wyniki_komitetow_sejmik_2010_DL,
              wyniki_komitetow_sejmik_2006_Zbiorcze,
              wyniki_komitetow_sejmik_2006_DL,
              wyniki_komitetow_sejmik_2002_Zbiorcze,
              wyniki_komitetow_sejmik_2002_DL,
              wyniki_komitetow_sejmik_1998_Zbiorcze,
              wyniki_komitetow_sejmik_1998_DL) %>%
  cbind(Podpisy, Korekta) %>%
  mutate(data = (as.Date(data)),
         Rok  = format(data, "%Y"))


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
  ggplot(data=Dane_W, aes(y = pct_glosow, x = Rok, colour = rejon, group = rejon)) +
  geom_line(size = 1.1) +
  geom_label(data=Dane_W[Dane_W$rejon == "Polska", ], 
             aes(y = pct_glosow + Korekta, x = Rok, 
                 label = paste0(Podpisy, " \n ", percent(pct_glosow))),
             alpha = 0.3,
             fill = "#d95f0e",
             # label.padding=.1,
             # seed = 1234, 
             family  = "Ubuntu", size = 2, color = "#22211d") +
  geom_label(data=Dane_W[Dane_W$rejon == "Dolny Œl¹sk", ], 
             aes(y = pct_glosow + Korekta, x = Rok, 
                 label = paste0(Podpisy, " \n ", percent(pct_glosow))),
             alpha = 0.3,
             fill = "#fe9929",
             # label.padding=.1,
             # seed = 1234, 
             family  = "Ubuntu", size = 2, color = "#22211d") +
  scale_colour_manual(
    values = c("#fe9929", "#d95f0e"),
    breaks = c("Dolny Œl¹sk", "Polska"),
    labels = c("Dolny Œl¹sk", "Polska")) +
  labs(title = paste0("Czy Polska zmierza do systemu dwupartyjnego?"),
       subtitle = paste0("Procent g³osów wa¿nych oddanych w sumie na dwa najwiêksze komitety w wyborach do sejmików\nwojewódzkich"),
       x = "",
       y = "",
       caption = "Autor: WroData | Na podstawie danych z pkw.gov.pl" ) +
  scale_y_continuous(labels  = percent,
                     breaks  = seq(from = 0, to = 1, by = 0.2)) +
  coord_cartesian(ylim = c(0, 0.65)) +
  #scale_x_date(date_labels = "%Y")   +
  Theme +
  guides(color = guide_legend(override.aes = list(size = 5))) 
  
plot(W)


# zapis wykresu
a <- 6
png(filename = paste("W ", Sys.Date(), " v2 .png", sep=""),
    bg="white", width = a * 1.5, height = a * 1, units = 'in', res = 150)
    plot(W)
dev.off()


