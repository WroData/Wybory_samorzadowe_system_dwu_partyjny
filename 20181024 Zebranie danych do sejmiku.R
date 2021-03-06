

setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\20181022 Wyniki wybor�w samorz�dowyh - spadek poparcia partii alternatywnych\\")



options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "en")
options(scipen=999)

##########################################################################
#### upewnienie si� �e nie ma �adnych pakiet�w za�adowanych ####
gc(reset = TRUE)
rm(list = ls())
#od��czeni wszytkich pakiet�w - stowrzebnuie funkcji
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo�anie funkcji 


##########################################################################
#### za�adowanie pakiet�w ####
#install.packages("rvest")
#library(rvest)
library(dplyr)
#install.packages("RCurl")
#library(RCurl)
#install.packages("htmltab")
library(htmltab)
library(stringr)
#install.packages("magrittr")
library(magrittr) # do extract w pipeline


##########################################################################
#### zmiane sta�e ####
Wojewodztwa <- c("dolno�l�skie"
                 , "kujawsko-pomorskie"
                 , "lubelskie"
                 , "lubuskie"
                 , "��dzkie"
                 , "ma�opolskie"
                 , "mazowieckie"
                 , "opolskie"
                 , "podkarpackie"
                 , "podlaskie"
                 , "pomorskie"
                 , "�l�skie"
                 , "�wi�tokrzyskie"
                 , "warmi�sko-mazurskie"
                 , "wielkopolskie"
                 , "zachodniopomorskie")

KW_Gl�wny_nurt <- c("KOMITET WYBORCZY PLATFORMA OBYWATELSKA RP"
                    ,"KOMITET WYBORCZY PRAWO I SPRAWIEDLIWO��"
                    ,"KOMITET WYBORCZY POLSKIE STRONNICTWO LUDOWE"
                    ,"KOMITET WYBORCZY SOJUSZ LEWICY DEMOKRATYCZNEJ"
                    #,"KOMITET WYBORCZY NASZ DOM POLSKA - SAMOOBRONA ANDRZEJA LEPPERA"
                    ,"KW Prawo i Sprawiedliwo��"
                    ,"KW Platforma Obywatelska RP"
                    ,"Komitet Wyborczy PSL"
                    ,"KKW SLD Lewica Razem"
                    ,"KW Tw�j Ruch")

POPIS <- c("KOMITET WYBORCZY PLATFORMA OBYWATELSKA RP"
           ,"KOMITET WYBORCZY PRAWO I SPRAWIEDLIWO��"
           ,"KW Prawo i Sprawiedliwo��"
           ,"KW Platforma Obywatelska RP"
           ,"KOMITET WYBORCZY PRAWO I SPRAWIEDLIWO��"
           ,"KOALICYJNY KOMITET WYBORCZY PLATFORMA.NOWOCZESNA KOALICJA OBYWATELSKA"
           )


#https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

##########################################################################
#### wybory 2018 ####

############# 
# #moja pr�ba
# 
# #https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
# substrRight <- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }
# 
# tmp <- htmltab::htmltab(doc = html, which = 9) 
# 
# tmp2 <- xml2::read_html(html) %>%
#   rvest::html_nodes("tbody") %>%
#   magrittr::extract2(9)  %>%
#   rvest::html_nodes("tr") %>%
#   rvest::html_nodes("td") %>%
#   rvest::html_nodes("span") %>%
#   rvest::html_text() %>%
#   matrix(ncol = 4, byrow = T) %>%
#   data.frame() 
# 
# names(tmp) <- c("a", "b", "c", "d", "e", "f", "g")
# tmp3 <- cbind(tmp, tmp2) %>%
#   mutate(n_to_delate = nchar(X1),
#          c1 = as.character(c),
#          n_whole = nchar(c1),
#          c2 = substrRight(c1, n_whole - n_to_delate),
#          c3 = gsub(" ", "", c2),
#          c4 = as.numeric(c3)) %>%
#   select(b, c4)
# 
# names(tmp3) <- c("party", "n_of_votes")
# 
# 
# for(i in 1:30){
#   print(i)
#   tmp <- htmltab::htmltab(doc = html, which = i) 
#   #tmp[, "nchar"] <- nchar(tmp[,3])
#   Encoding(names(tmp)) <- "UTF-8"
#   
#   print(tmp)
# }
# 

wyniki_komitetow_sejmik_2018 <- data.frame()
for(i in c(1:16)){
  #i=3
  print(paste0(i, " : ", Wojewodztwa[i]))
  html <- paste0(
    "https://wybory2018.pkw.gov.pl/pl/geografia/" ,
    str_pad(i * 2, width = 2, side = "left", pad = "0"),
    "0000#results_vote_council")
  

  tmp <- htmltab::htmltab(doc = html, which = 7) 
  
  tmp2 <- xml2::read_html(html) %>%
    rvest::html_nodes("tbody") %>%
    magrittr::extract2(7)  %>%
    rvest::html_nodes("tr") %>%
    rvest::html_nodes("td") %>%
    rvest::html_nodes("span") %>%
    rvest::html_text() %>%
    matrix(ncol = 3, byrow = T) %>%
    data.frame() 
  
  names(tmp) <- letters[1:ncol(tmp)]
  tmp3 <- cbind(tmp, tmp2) %>%
    mutate(n_to_delate = nchar(X1),
           c1 = as.character(c),
           n_whole = nchar(c1),
           c2 = substrRight(c1, n_whole - n_to_delate),
           c3 = gsub(" ", "", c2),
           c4 = as.numeric(c3)) %>%
    select(b, c4)
  
  names(tmp3) <- c("Komitet", "n_of_votes")
  
  tmp3[,"Wojew�dzctwo"] <- Wojewodztwa[i]
  tmp3[,"Rok"] <- 2018
  
  
  wyniki_komitetow_sejmik_2018 <- rbind(wyniki_komitetow_sejmik_2018, tmp3)
}

Encoding(wyniki_komitetow_sejmik_2018[,1]) <- "UTF-8"

save(wyniki_komitetow_sejmik_2018, 
     file = paste0("dane//wyniki_komitetow_sejmik_2018 ", Sys.Date(), " .RData"))

wyniki_komitetow_sejmik_2018_Zbiorcze <- wyniki_komitetow_sejmik_2018 %>%
  group_by(Komitet) %>%
  summarise(n_glosow_2018 = sum(n_of_votes, na.rm = T)) %>%
  mutate(pct_glosow_2018  = n_glosow_2018 / sum(n_glosow_2018, na.rm = T))

wyniki_komitetow_sejmik_2018_Zbiorcze %>% arrange(desc(pct_glosow_2018)) %>% select(Komitet)

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
  #poprawa format�w danych
  Encoding(names(tmp)) <- "UTF-8"
  Encoding(tmp[,2])    <- "UTF-8"
  tmp[,1] <- as.numeric(tmp[,1])
  tmp[,3] <- as.numeric(tmp[,3])
  tmp[,"Wojew�dzctwo"] <- Wojewodztwa[i]
  tmp[,"Rok"] <- 2014
  
  
  wyniki_komitetow_sejmik_2014 <- rbind(wyniki_komitetow_sejmik_2014, tmp)
}


save(wyniki_komitetow_sejmik_2014, 
     file = paste0("dane//wyniki_komitetow_sejmik_2014 ", Sys.Date(), " .RData"))

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
  #poprawa tytu�u
  names(tmp) <- tmp[2, ]
  #usuni�cie zb�dnych wierszy
  tmp <- tmp[c(-1, -2, -nrow(tmp)), ]
  #usuni�cie zb�dnych kolumn
  tmp <- tmp[, c(1, 2, 3)]
  #poprawa format�w danych
  Encoding(names(tmp)) <- "UTF-8"
  Encoding(tmp[,2])    <- "UTF-8"
  tmp[,1] <- as.numeric(tmp[,1])
  tmp[,3] <- as.numeric(tmp[,3])
  tmp[,"Wojew�dzctwo"] <- Wojewodztwa[i]
  tmp[,"Rok"] <- 2014
  
  
  wyniki_komitetow_sejmik_2010 <- rbind(wyniki_komitetow_sejmik_2010, tmp)
}

names(wyniki_komitetow_sejmik_2010) <- names(wyniki_komitetow_sejmik_2014)



save(wyniki_komitetow_sejmik_2010, 
     file = paste0("dane//wyniki_komitetow_sejmik_2010 ", Sys.Date(), " .RData"))

wyniki_komitetow_sejmik_2010_Zbiorcze <- wyniki_komitetow_sejmik_2010 %>%
  group_by(Komitet) %>%
  summarise(n_glosow_2010 = sum(Razem, na.rm = T)) %>%
  mutate(pct_glosow_2010  = n_glosow_2010 / sum(n_glosow_2010, na.rm = T))


##########################################################################
#### Partie gOwnego nurtu ####
All <- wyniki_komitetow_sejmik_2010_Zbiorcze %>%
  merge(wyniki_komitetow_sejmik_2014_Zbiorcze, by = "Komitet", all = T) %>%
  merge(wyniki_komitetow_sejmik_2018_Zbiorcze, by = "Komitet", all = T) %>%
  mutate(Glowny_nurt = ifelse(Komitet %in% KW_Gl�wny_nurt, "Partie Sejmowe", "Inne"),
         POPIS       = ifelse(Komitet %in% POPIS, "POPIS", "Inne")) 

POPIS_w_czasie <- All %>%
  group_by(POPIS) %>%
  summarise(n_glosow_2010   = sum(n_glosow_2010, na.rm = T),
            pct_glosow_2010 = sum(pct_glosow_2010, na.rm = T),
            n_glosow_2014   = sum(n_glosow_2014, na.rm = T),
            pct_glosow_2014 = sum(pct_glosow_2014, na.rm = T),
            n_glosow_2018   = sum(n_glosow_2018, na.rm = T),
            pct_glosow_2018 = sum(pct_glosow_2018, na.rm = T))



G��wny_nurt_w_czasie <- All %>%
  group_by(Glowny_nurt) %>%
  summarise(n_glosow_2010   = sum(n_glosow_2010, na.rm = T),
            pct_glosow_2010 = sum(pct_glosow_2010, na.rm = T),
            n_glosow_2014   = sum(n_glosow_2014, na.rm = T),
            pct_glosow_2014 = sum(pct_glosow_2014, na.rm = T))


#write.csv2(All, file = "wszytkie dane zbiorcze.csv")


