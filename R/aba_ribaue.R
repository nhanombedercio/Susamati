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


dir <- "C:/Users/Dercio/OneDrive/Projectos Rstudio/2023/Susamati/data"
Nampula_clear <- read_excel(paste(dir, "/Nampula_clear.xls", sep=""))

ribaue <- Nampula_clear %>% filter(District_ID %in% "MZ0723") ## Monapo 
ribaue$tipo_casa_banho1<- ifelse(is.na(ribaue$tipo_casa_banho1), "Sem Latrina", ribaue$tipo_casa_banho1)
## shapfile de distritos
ribaue_sf <- readRDS(paste(dir, "/sf_districts.rds", sep="")) %>% filter(District_ID %in% "MZ0723")


###JUNTAR BASE DE DADOS E SHAPFILE


########MAPA DE MONAPO###################

#### GRAFICO de subapa economia
filtro_economia_ribaue<- radioButtons("ribaue_filtro_economica", "Selecione uma opcção:",
                                      c("Tipo de Rendimento","Rendimento Mensal"))

