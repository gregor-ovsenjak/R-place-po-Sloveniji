require("dpylr")
require("tidyr")
require("readr")
library("openxlsx")
require("readxl")
podatki <- read.csv2("./podatki/Neocisceni_podatki/povprecne-mesecne-place-po-statisticnih-regijah.csv",
                     quote = "\"",
                      na=c('-','z','na','--'),
                      skip=2,
                     drop_na(LETO))
sapply(podatki,class)
