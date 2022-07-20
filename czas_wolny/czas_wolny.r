library(dplyr)
data_czas_wolny <- read.csv("czas_wolny.csv")
data_czas_wolny %>%
  filter((liczba_dzieci > 0) == czy_dzieci) %>%
  filter(is.na(wiek_dziecka) != czy_dzieci) %>%
  filter(czy_dzieci == 1) %>%
  filter(wiek_dziecka <= 10) -> data_czas_wolny

summarise_czas_wolny <- function(data_czas_wolny) {
  hist(data_czas_wolny$wiek)
  
  #l_grup <- 4
  #kat_wiekowe <- floor(seq(min(data_czas_wolny$wiek), max(data_czas_wolny$wiek), length.out = l_grup+1))
  kat_wiekowe <- quantile(data_czas_wolny$wiek)
  
  data_czas_wolny <- data_czas_wolny %>% mutate(kategoria_wiekowa = cut(wiek, breaks = kat_wiekowe, include.lowest = TRUE))
  data_czas_wolny <- data_czas_wolny %>% group_by(kategoria_wiekowa)
  
  data_summary <- data_czas_wolny %>% summarise(
    srednia_liczba_dzieci = mean(liczba_dzieci),
    sredni_czas_z_dziecmi = mean(czas_z_dziecmi),
    sredni_czas_wolny = mean(czas_wolny),
    licznosc_grupy = n()
  )
  data_summary
}

summarise_czas_wolny(data_czas_wolny)

data_czas_wolny <- filter(data_czas_wolny, wiek_dziecka <= (wiek - 16))

summarise_czas_wolny(data_czas_wolny)

