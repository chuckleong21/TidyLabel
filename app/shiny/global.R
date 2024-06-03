library(shiny)
library(shinyjs)
library(htmltools)
library(purrr)
library(officer)
library(flextable)
library(stringr)
library(magrittr)
library(tidyr)
library(dplyr)
library(tippy)
library(RDCOMClient)
library(tabulapdf)
library(rlang)
library(shiny.i18n)
library(tidystringdist)
library(gt)

source("tidy_label.R")
source("tidy_tax.R")
source("tagsTextInput.R")
source("format_baaksai.R")
source("tidyup2.R")
source("validate_label.R")

translator <- Translator$new(translation_json_path = "translations.json")
translator$set_translation_language("zh")
version_list <- set_names(
  c("auto", "7805640197", "2540263576", "7801664214", "7816168667"), 
  c("Auto", 
    "ООО «САРМАНТ-ЮГ» РОССИЯ, 196006, Г. САНКТ-ПЕТЕРБУРГ,\nУЛИЦА НОВОРОЩИНСКАЯ, ДОМ 4, ЛИТ. А, 1Н, ПОМ. 363, ОФИС\n609-1\n1147847003193", 
    "ООО \"ВЕГА КАПЕЛЛА\"\nРОССИЯ, ПРИМОРСКИЙ КРАЙ, Г ВЛАДИВОСТОК, УЛ\nМОРДОВЦЕВА, Д. 3, КАБИНЕТ, 804, 89025202031\n1212500021414", 
    "ООО \"БАДИС ПРЕМИУМ\"\nРОССИЯ, 199034, Г.САНКТ-ПЕТЕРБУРГ, 8-Я ЛИНИЯ В.О.,\nД.1/20, ПОМ.7-Н\nN 1197847105730", 
    "ООО \"БАДИС\"\nРОССИЯ, 199034, ГОРОД, САНКТ-ПЕТЕРБУРГ, В.О., 8-Я\nЛИНИЯ, Д. 1/20, ПОМ. 7Н\n1027808012870")
)
