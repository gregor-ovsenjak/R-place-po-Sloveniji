library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(openxlsx)
library(readxl)
options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("../lib/uvozi.zemljevid.r", encoding="UTF-8")

