require("dplyr")
require("tidyr")
require("readr")
library("openxlsx")
require("readxl")

#UVOZ IN OCISCENJE CSV- povprecne mesecne place po statisticnih regijah
place_po_regijah <- read.csv2("./podatki/Neocisceni_podatki/povprecne-mesecne-place-po-statisticnih-regijah.csv",
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

place_po_regijah$STAROST <- case_when(place_po_regijah$STAROST == "65 let ali veè" ~ "65 let >",
                                      place_po_regijah$STAROST == "15-64 let" ~ "15-64 let",
                                      place_po_regijah$STAROST == "15-24 let" ~ "15-24 let",
                                      place_po_regijah$STAROST == "25-34 let" ~ "25-34 let",
                                      place_po_regijah$STAROST == "35-44 let" ~ "35-44 let",
                                      place_po_regijah$STAROST == "45-54 let" ~ "45-54 let",
                                      place_po_regijah$STAROST == "55-64 let" ~ "55-64 let")
View(place_po_regijah)                     


# PLACE PO SEKTORJIH
place_po_sektorjih <- read.csv2("./podatki/Neocisceni_podatki/povprecne-mesecne-place-v-J-Z.csv",
                                skip=2,
                                na=c('-','z','na','--')) %>%
                              rename( # preimenuj stolpce
                                SEKTOR = 1, 
                                IZOBRAZBA = 2,
                                SPOL = 3,
                                MERITVE = 4,
                                LETO=5,
                                POVPRECNA_PLACA=6) %>%
                              drop_na(LETO) %>% 
                              select(-MERITVE) # nepotreben stolpec z eno samo vrednostjo





# uvoz in ociscenje podatkov za place po dejavnostih
place_po_dejavnosti <- read_excel(path="./podatki/Neocisceni_podatki/povp-place-po-dejavnostih.xlsx",
                                    col_names = FALSE,
                                    skip=2,
                                    n_max=33661,
                                    na = c('z','N','M','-')
                                  ) %>% 
                                  fill(1:3) %>% 
                                  select(-c(5,7)) %>%
                                  rename(
                                    REGIJA = 1,
                                    DEJAVNOST = 2,
                                    DATUM = 3,
                                    TIP_PLACE = 4,
                                    MERITEV = 5
                                  )  %>% 
                                  separate(DATUM, into=c("LETO", "MESEC"), sep='M') %>%
                                  pivot_wider(names_from=TIP_PLACE, values_from=MERITEV) %>% 
                                  select(-MESEC) %>%
                                  group_by(LETO,DEJAVNOST) %>% 
                                  rename(
                                    BRUTO = 4,
                                    NETO = 5
                                  ) %>%
                                  summarize(SKUPNO=sum(BRUTO))
View(place_po_dejavnosti)




write.csv2(place_po_dejavnosti,"./podatki/Pocisceni_podatki/place_po_dejavnosti.csv")
write.csv2(place_po_sektorjih,"./podatki/Pocisceni_podatki/place_po_sektorjih.csv")
write.csv2(place_po_regijah,"./podatki/Pocisceni_podatki/place_po_regijah.csv")
