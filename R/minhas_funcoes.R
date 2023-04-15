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
   dados_mapa <- left_join(data_sf, dado_dist %>% group_by(District_ID, distrito, tipo_casa_banho1) %>% 
                             summarise(Total = n()) %>% mutate(percentagem = round(Total / sum(Total) * 100, digits = 2)),
                           by = c("District_ID" = "District_ID"), multiple = "all")
   
   
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
         margin = margin(12, 12, 12, 12, "pt")
       ),
   axis.text.x = element_blank(), 
   axis.text.y = element_blank()
     )+
     labs(x = "Longitude", y = "Latitude")
   return(mapa)
 }
   
 ############Rendimento mensal 
 tipo_redimento<-function(dado) {
   visitas <- as.name("famílias visitadas")
   
   grafico(create_summary_table(dado, "rendimento_tipo"),x=rendimento_tipo,y=Total, fill=rendimento_tipo, dado,visitas)+
     labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
          x="",
          y="Numero de Familias")+
     theme(axis.text.x = element_blank(), # remove os valores do eixo x
           axis.title.x = element_blank(), # remove o título do eixo x
           axis.line.x = element_blank())+ # remove a linha do eixo x
     theme(legend.position = "top") 
 }
 
############## 
 faixa_rendimento  <- function(dados) {
   tem_latrina <- as.name("famílias visitadas")
   grafico(create_summary_table(dado, "rendimento_faixa"), x = rendimento_faixa, y = Total, fill = rendimento_faixa,dados,tem_latrina) +
     labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
          x = "",
          y = "Numero de Familias") +
     theme(axis.text.x = element_blank(), # remove os valores do eixo x
           axis.title.x = element_blank(), # remove o título do eixo x
           axis.line.x = element_blank())+ # remove a linha do eixo x
     theme(legend.position = "top")
   #return(grafico(monapo, tem_latrina, 250, 50))
 }