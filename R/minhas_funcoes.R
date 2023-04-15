### funcao para criar instalar e ler pacotes

##Funcao para instalar pacotes importantes
install_data_packages <- function() {
  packages <- c("ggthemes","RColorBrewer","leaflet","esquisse", "sf","shiny", "shinythemes", "shinydashboard", "shinyWidgets", "DT", "ggplot2", "leaflet", "plotly", "flexdashboard", "shinyjs","tidyverse", "data.table", "dplyr", "readr", "readxl", "googlesheets4", "jsonlite", "httr", "stringr", "lubridate", "janitor")
  # Verifica se cada pacote da lista está instalado e instala se não estiver
  # Verifica se cada pacote da lista está instalado e instala se não estiver
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
      library(package)
      
    } else {
      print("PACOTE INSTALADO E CARREGADO")
    }
  }
  
  
  
  # Carrega todos os pacotes instalados
  #library(packages)
  # Carrega todos os pacotes instalados
  lapply(packages, library, character.only = TRUE)
  print("PACOTE INSTALADO E CARREGADO")
}


##funcao para contar numero de observacoes da variavel que quero plotar 
  
  create_summary_table <- function(data, group_vars, count_var) {
   summary_table <- data %>%
      group_by(across(all_of(group_vars))) %>%
     summarise(Total= n()) %>%
      mutate(percentagem = round(Total / sum(Total)*100, digits = 2 ))
     
    return(summary_table)
  }

  ## funcao que cria um grafico
  grafico <- function(dados, x, y, fill, dados_total, string_familias){
    library(ggplot2)
    ggplot(dados, aes(text =paste("Numero de familias:", {{y}}, "de", nrow(dados_total), string_familias),x = {{x}}, y = {{y}}, fill = {{fill}})) +
      geom_col() +
      scale_fill_hue(direction = 1) +ggthemes::theme_stata()+labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
                                                                  x="",
                                                                  y="Numero de Familias"
      )+theme(legend.position = "top")+
      scale_y_continuous(limits = c(0,250), breaks = seq(0,250,by=50))
  }
 
  
  
 
  
  ###funcao para mapa
  
 funcao_mapa<-function(data_sf, dado_dist){
   library(stringr)
   dados_mapa<-left_join(monapo_sf, monapo %>% group_by(District_ID,distrito,tipo_casa_banho1) %>% 
                           summarise(Total=n()) %>% mutate(percentagem=round(Total/sum(Total)*100, digits = 2 )), by = "District_ID")
 
   
   mapa <- ggplot(dados_mapa) +
     aes(text = paste("Tipo de latrina:", tipo_casa_banho1), fill = percentagem) +
     geom_sf(size = 1.5) +
     scale_fill_distiller(palette = "Purples", direction = 1) +
     theme_bw() +
     facet_wrap(vars(tipo_casa_banho1), nrow = 1L, labeller = labeller(tipo_casa_banho1 = function(x) str_wrap(x, width = 30))) +
     theme(
       strip.text = element_text(
         size = 12, # Tamanho do texto aumentado para 10
         face = "bold",
         color = "black",
         margin = margin(10, 10, 10, 10, "pt")
       ),
   axis.text.x = element_blank(), 
   axis.text.y = element_blank()
     )+
     labs(x = "Longitude", y = "Latitude")
   return(mapa)
 }
   
  