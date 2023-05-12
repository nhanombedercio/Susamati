library(shiny)
library(bslib)
library(ggthemes)
library(RColorBrewer)
library(sf)
library(shinythemes)
library(lubridate)
library(jsonlite)
library(stringr)
library(readr)
library(dplyr)
library(tidyverse)
library(shinyjs)
library(plotly)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(bslib)
library(readxl)
library(readr)
library(esquisse)

dir <- "data"
Nampula_clear <- read_excel(paste(dir, "/Nampula_clear.xls", sep=""))
# Carregar os dados dos distritos
sf_districts <- readRDS(paste(dir, "/sf_districts.rds", sep = ""))

# Filtrar os distritos de Nampula
distritos_nampula <- sf_districts %>%
  filter(provincia == "Nampula")
#esquisser(Nampula_clear)
# Filtrar apenas os distritos de Nacala, Ribaue e Monapo
distritos_destacados <- distritos_nampula %>%
  filter(distrito %in% c("Nacala-A-Velha", "Ribaue", "Monapo"))

#############################################################################
#esquisser(readRDS(paste(dir, "/sf_districts.rds", sep="")))
monapo <- Nampula_clear %>% filter(District_ID %in% "MZ0715") ## Monapo 
monapo$tipo_casa_banho1<- ifelse(is.na(monapo$tipo_casa_banho1), "Sem Latrina", monapo$tipo_casa_banho1)
## shapfile de distritos
monapo_sf <- readRDS(paste(dir, "/sf_districts.rds", sep="")) %>% filter(District_ID %in% "MZ0715")

 
###JUNTAR BASE DE DADOS E SHAPFILE

 
########MAPA DE MONAPO###################

#### GRAFICO de subapa economia
filtro_economia_monapo<- radioButtons("monapo_filtro_economica", "Selecione uma opcção:",
                     c("Tipo de Rendimento","Rendimento Mensal"))


 
 
