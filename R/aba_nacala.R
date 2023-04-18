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

dir <- "data"
Nampula_clear <- read_excel(paste(dir, "/Nampula_clear.xls", sep=""))

nacala <- Nampula_clear %>% filter(District_ID %in% "MZ0720") ## nacala 
nacala$tipo_casa_banho1<- ifelse(is.na(nacala$tipo_casa_banho1), "Sem Latrina", nacala$tipo_casa_banho1)
## shapfile de distritos
nacala_sf <- readRDS(paste(dir, "/sf_districts.rds", sep="")) %>% filter(District_ID %in% "MZ0720")


###JUNTAR BASE DE DADOS E SHAPFILE


########MAPA DE nacala###################

#### GRAFICO de subapa economia
filtro_economia_nacala<- radioButtons("nacala_filtro_economica", "Selecione uma opcção:",
                                      c("Tipo de Rendimento","Rendimento Mensal"))

 
