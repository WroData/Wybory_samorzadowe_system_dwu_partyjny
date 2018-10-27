#20181023 œci¹gniêcie wsytkich wyników dla 2010 roku

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
library(XML)
library(dplyr)
library(rvest)

library(htmltab)




##########################################################################
#### funkcja do scrapowania ####
# https://gist.github.com/paulrougieux/e1ee769577b40cd9ed9db7f75e9a2cc2

scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  
  return(data_frame(link = link_, url = url_) 
         #%>%
        #   mutate(stat = substr(url, 1, 6),
        #          Num = as.numeric(stat),
        #          pelen_link = paste0(url_pod, links)) %>%
        #   filter(!is.na(Num))
  )
}




##########################################################################
#### zmiane sta³e ####
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
#### funkcja do scrapowania 2010 ####
url_pod <- "http://wybory2010.pkw.gov.pl/geo/pl/"
url <- "http://wybory2010.pkw.gov.pl/geo/pl/000000.html"




df_links = scraplinks(url) %>%
   mutate(stat = substr(url, 1, 6),
          Num = as.numeric(stat),
          pelen_link = paste0(url_pod, url)) %>%
   filter(!is.na(Num))

Wynik_rad_powiatow = data.frame()

for (i in 1:nrow(df_links)){
  print(i)
  # i = 1 
  url = as.character(df_links[i, "pelen_link"])
  url_pod = substr(url, 1, nchar(url)-11)
  df_links_2 = scraplinks(url) %>%
    mutate(stat = substr(url, 1, 6),
           Num = as.numeric(stat)) %>%
    filter(!is.na(Num)) %>%
    filter(nchar(url) == 11) %>%
    mutate(pelen_link = paste0(url_pod, url, "#tabs-3"))
  
  for (j in 1:nrow(df_links_2)){
    print(j)
    #j = 3
    
    url = as.character(df_links_2[j, "pelen_link"])
    
    #wzbranie odpowiedniej tabeli - róznie zaleœnie czy jest miasto czy nie
    if(substr(as.character(df_links_2[j, "link"]), 1, 3) == "m. "){
      tmp_powiat <- htmltab(doc = url, which = 8)
    } else {
      tmp_powiat <- htmltab(doc = url, which = 9)
    }
    #poprawa nazw kolumn
    names(tmp_powiat) <- tmp_powiat[2,]
    #wubranie odpowiedniej kolumnz
    tmp_powiat <- tmp_powiat[, c(2, 3)]
    #wubranie odpowiedniej kolumnz
    tmp_powiat <- tmp_powiat[ c(-1, -2, -nrow(tmp_powiat)),]
    #poprawa formatów danych
    Encoding(names(tmp_powiat)) <- "UTF-8"
    Encoding(tmp_powiat[,1])    <- "UTF-8"
    tmp_powiat[,2] <- as.numeric(tmp_powiat[,2])
    #dodanie metryczki
    tmp_powiat[,"Wojewódzctwo"] <- as.character(df_links  [i, "link"])
    tmp_powiat[,"Powiat"]        <- as.character(df_links_2[j, "link"])
    tmp_powiat[,"Rok"] <- 2014
    
    print(paste(i, " : ", j , " : ", tmp_powiat[1, c(3, 4)]))
    
    Wynik_rad_powiatow <- rbind(Wynik_rad_powiatow, tmp_powiat)
    
  }
  
  
}


##########################################################################
#### Partie gOwnego nurtu ####
Wynik_rad_powiatow %>%
  select(Wojewódzctwo, Powiat) %>% distinct() %>%
  group_by(Wojewódzctwo) %>%
  summarise(n = n())



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

