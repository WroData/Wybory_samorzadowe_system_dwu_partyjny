
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
#install.packages("rvest")
#library(rvest)
library(dplyr)
#install.packages("RCurl")
#library(RCurl)
#install.packages("htmltab")
library(htmltab)
library(stringr)



##########################################################################
#### zmiane sta³e ####
Wojewodztwa <- c("dolnoœl¹skie"
                 , "kujawsko-pomorskie"
                 , "lubelskie"
                 , "lubuskie"
                 , "³ódzkie"
                 , "ma³opolskie"
                 , "mazowieckie"
                 , "opolskie"
                 , "podkarpackie"
                 , "podlaskie"
                 , "pomorskie"
                 , "œl¹skie"
                 , "œwiêtokrzyskie"
                 , "warmiñsko-mazurskie"
                 , "wielkopolskie"
                 , "zachodniopomorskie")

KW_Glówny_nurt <- c("KOMITET WYBORCZY PLATFORMA OBYWATELSKA RP"
                    ,"KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŒÆ"
                    ,"KOMITET WYBORCZY POLSKIE STRONNICTWO LUDOWE"
                    ,"KOMITET WYBORCZY SOJUSZ LEWICY DEMOKRATYCZNEJ"
                    #,"KOMITET WYBORCZY NASZ DOM POLSKA - SAMOOBRONA ANDRZEJA LEPPERA"
                    ,"KW Prawo i Sprawiedliwoœæ"
                    ,"KW Platforma Obywatelska RP"
                    ,"Komitet Wyborczy PSL"
                    ,"KKW SLD Lewica Razem"
                    ,"KW Twój Ruch")

POPIS <- c("KOMITET WYBORCZY PLATFORMA OBYWATELSKA RP"
           ,"KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŒÆ"
           ,"KW Prawo i Sprawiedliwoœæ"
           ,"KW Platforma Obywatelska RP"
           )

##########################################################################
#### wybory 2014 ####
wyniki_komitetow_sejmik_2014 <- data.frame()
for(i in c(1:16)){
  print(paste0(i, " : ", Wojewodztwa[i]))
  html <- paste0(
    "https://samorzad2014.pkw.gov.pl/357_rady_woj/0/" ,
    str_pad(i * 2, width = 2, side = "left", pad = "0"))
  #wzbranie odpowiedniej tabeli
  tmp <- htmltab(doc = html, which = 1)
  #wubranie odpowiedniej kolumnz
  tmp <- tmp[, c(1,2,ncol(tmp))]
  #poprawa formatów danych
  Encoding(names(tmp)) <- "UTF-8"
  Encoding(tmp[,2])    <- "UTF-8"
  tmp[,1] <- as.numeric(tmp[,1])
  tmp[,3] <- as.numeric(tmp[,3])
  tmp[,"Wojewódzctwo"] <- Wojewodztwa[i]
  tmp[,"Rok"] <- 2014
  
  
  wyniki_komitetow_sejmik_2014 <- rbind(wyniki_komitetow_sejmik_2014, tmp)
}


wyniki_komitetow_sejmik_2014_Zbiorcze <- wyniki_komitetow_sejmik_2014 %>%
  group_by(Komitet) %>%
  summarise(n_glosow_2014 = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow_2014  = n_glosow_2014 / sum(n_glosow_2014, na.rm = T))

##########################################################################
#### wybory 2010 ####
wyniki_komitetow_sejmik_2010 <- data.frame()
for(i in c(1:16)){
  print(paste0(i, " : ", Wojewodztwa[i]))
  
  
  html <- paste0("http://wybory2010.pkw.gov.pl/geo/pl/",
                 str_pad(i * 2, width = 2, side = "left", pad = "0"),
                 "0000/",
                 str_pad(i * 2, width = 2, side = "left", pad = "0"),
                 "0000.html#tabs-6")
  
  tmp <- htmltab(doc = html, which = 18)
  #poprawa tytu³u
  names(tmp) <- tmp[2, ]
  #usuniêcie zbêdnych wierszy
  tmp <- tmp[c(-1, -2, -nrow(tmp)), ]
  #usuniêcie zbêdnych kolumn
  tmp <- tmp[, c(1, 2, 3)]
  #poprawa formatów danych
  Encoding(names(tmp)) <- "UTF-8"
  Encoding(tmp[,2])    <- "UTF-8"
  tmp[,1] <- as.numeric(tmp[,1])
  tmp[,3] <- as.numeric(tmp[,3])
  tmp[,"Wojewódzctwo"] <- Wojewodztwa[i]
  tmp[,"Rok"] <- 2014
  
  
  wyniki_komitetow_sejmik_2010 <- rbind(wyniki_komitetow_sejmik_2010, tmp)
}

names(wyniki_komitetow_sejmik_2010) <- names(wyniki_komitetow_sejmik_2014)


wyniki_komitetow_sejmik_2010_Zbiorcze <- wyniki_komitetow_sejmik_2010 %>%
  group_by(Komitet) %>%
  summarise(n_glosow_2010 = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow_2010  = n_glosow_2010 / sum(n_glosow_2010, na.rm = T))


##########################################################################
#### Partie gOwnego nurtu ####
All <- wyniki_komitetow_sejmik_2010_Zbiorcze %>%
  merge(wyniki_komitetow_sejmik_2014_Zbiorcze, by = "Komitet", all = T) %>%
  mutate(Glowny_nurt = ifelse(Komitet %in% KW_Glówny_nurt, "Partie Sejmowe", "Inne"),
         POPIS       = ifelse(Komitet %in% POPIS, "POPIS", "Inne")) 

POPIS_w_czasie <- All %>%
  group_by(POPIS) %>%
  summarise(n_glosow_2010   = sum(n_glosow_2010, na.rm = T),
            pct_glosow_2010 = sum(pct_glosow_2010, na.rm = T),
            n_glosow_2014   = sum(n_glosow_2014, na.rm = T),
            pct_glosow_2014 = sum(pct_glosow_2014, na.rm = T))



G³ówny_nurt_w_czasie <- All %>%
  group_by(Glowny_nurt) %>%
  summarise(n_glosow_2010   = sum(n_glosow_2010, na.rm = T),
            pct_glosow_2010 = sum(pct_glosow_2010, na.rm = T),
            n_glosow_2014   = sum(n_glosow_2014, na.rm = T),
            pct_glosow_2014 = sum(pct_glosow_2014, na.rm = T))


#write.csv2(All, file = "wszytkie dane zbiorcze.csv")


