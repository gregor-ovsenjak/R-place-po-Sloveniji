require("dplyr")
require("tidyr")
require("readr")
library("openxlsx")
require("readxl")7

#UVOZ IN OCISCENJE CSV-
podatki <- read.csv2("./podatki/Neocisceni_podatki/povprecne-mesecne-place-po-statisticnih-regijah.csv",
                        quote = "\"",
                        na=c('-','z','na','--'),
                        skip=2) %>%
                     rename( # preimenuj stolpce
                         STATISTICNA_REGIJA = 1, 
                         STAROST = 2,
                         SPOL = 3,
                         MERITVE = 4,
                         LETO=5,
                         POVPRECNA_PLACA=6) %>% 
                     drop_na(LETO) %>% # izbriši vrednosti NA v stolpcu LETO, ker takrat itak ne vemo povp.place
                     select(-MERITVE) %>% # nepotreben stolpec z eno samo vrednostjo
                     pivot_wider(names_from=SPOL, values_from=POVPRECNA_PLACA) # loci moske in ženske ?

podatki$STAROST <- case_when(podatki$STAROST == "65 let ali veè" ~ "65 let >",
                             podatki$STAROST == "15-64 let" ~ "15-64 let",
                             podatki$STAROST == "15-24 let" ~ "15-24 let",
                             podatki$STAROST == "25-34 let" ~ "25-34 let",
                             podatki$STAROST == "35-44 let" ~ "35-44 let",
                             podatki$STAROST == "45-54 let" ~ "45-54 let",
                             podatki$STAROST == "55-64 let" ~ "55-64 let")
View(podatki)                     

