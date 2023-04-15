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
 
# Define UI
ui <- fluidPage(
  
  # Define o tema
  theme = shinytheme("flatly"),
  
  # Define a barra de navegação
  navbarPage(
    # Define o título da barra de navegação
    title = "SUSAMATI" ,
    header = "Meu Cabeçalho",
   # tags$head(tags$style(HTML('.navbar-default {background-color: #a742f5;}'))),
    #title = "ASSOCIAÇÃO MUVA",
    
    #BASELINE  
    navbarMenu("MONAPO", icon=icon("exchange-alt"),
               tabPanel("MAPA-Tipo de saneamento",
                        column(12,
                               wellPanel(h4("Mapa de variação em baixo mostra 
                               tipo de latrina mais frequente em Monapo. 
                                            Quando mais carregado for 
                                            a cor do mapa mais familias usam esse tipo de latrina.
                                            Deslize o mouse nos mapas para verificar a percentagem 
                                            das familias que usa determinado tipo de latrina
                                            ")),
                               plotlyOutput("monapo_mapa", height = 500)
                        )),
               tabPanel("Cobertura de saneamento",tabname="monapo_saneamento", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        
                        column(12,
                               wellPanel(radioButtons("monapo_filtro_cobertura", "Selecione uma opcção:",
                                                      c("Tem Latrina?","Latrina esta sempre disponivel","tem casota")),
                               )),
                        column(6, 
                               #h4("BASELINE"),
                               # h4("Em Monapo Visitamos", count(monapo), "familias", "onde 1", "familia não aceitou ser entrevistada"),
                               plotlyOutput("monapo_saneamento_baseline", height = 500)),
                        
                        
                        column(6,
                               plotlyOutput("monapo_saneamento_endline", height = 500)
                        ),
                        
                        
               ),  
               tabPanel("Vontade de contribuir",tabname="monapo_vontade", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel( radioButtons("monapo_filtro_vontade", "Selecione uma opcção:",
                                                       c("Podes contribuir para melhorar a sua sanita?","Com Quanto gostaria de constribuir?")),
                               )
                        ),
                        
                        
                        column(6,
                               plotlyOutput("monapo_vontade_baseline",height = 500)
                        ),  
                        column(6,
                               plotlyOutput("monapo_vontade_endline",height = 500)
                        ),                
                        
               ),
               tabPanel("Situação económica",tabname="monapo_situacao", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12,
                              # wellPanel(filtro_economia_monapo)
                        ),
                        
                        
                        column(6,
                               plotlyOutput("monapo_economica_baseline",height = 500)
                        ),  
                        column(6,
                               plotlyOutput("monapo_economica_endline",height = 500)
                        ),               
                        
               )
               
    ), # Monapo
    
    ##RIBAUE
    navbarMenu("RIBAUE", icon=icon("exchange-alt"),
               tabPanel("MAPA-Tipo de saneamento",
                        column(12,
                               wellPanel(h4("Mapa de variação em baixo mostra 
                               tipo de latrina mais frequente em Ribaue. 
                                            Quando mais carregado for 
                                            a cor do mapa mais familias usam esse tipo de latrina.
                                            Deslize o mouse nos mapas para verificar a percentagem 
                                            das familias que usa determinado tipo de latrina
                                            ")),
                               plotlyOutput("ribaue_mapa", height = 500)
                        )),
               tabPanel("Cobertura de saneamento",tabname="ribaue_saneamento", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel(radioButtons("ribaue_filtro_cobertura", "Selecione uma opcção:",
                                                      c("Tem Latrina?" ,"Latrina esta sempre disponivel","tem casota")),
                               )
                        ),
                        
                        
                        column(6,
                               plotlyOutput("ribaue_saneamento_baseline")
                        ),  
                        column(6,
                               plotlyOutput("ribaue_saneamento_endline")
                        ), 
                        
               ), 
               tabPanel("Vontade de contribuir",tabname="ribaue_vontade", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel( radioButtons("ribaue_filtro_vontade", "Selecione uma opcção:",
                                                       c("Podes contribuir para melhorar a sua sanita?","Com Quanto gostaria de constribuir?")),
                               )
                        ),
                        
                        
                        column(6,
                               plotlyOutput("ribaue_vontade_baseline")
                        ),  
                        column(6,
                               plotlyOutput("ribaue_vontade_endline")
                        ),                
                        
               ),
               tabPanel("Situação económica",tabname="ribaue_situacao", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel(radioButtons("ribaue_filtro_economica", "Selecione uma opcção:",
                                                      c("Tipo de Rendimento","Rendimento Mensal")
                               ),
                               )
                        ),
                        
                        column(6,
                               plotlyOutput("ribaue_economica_baseline")
                        ),  
                        column(6,
                               plotlyOutput("ribaue_economica_endline")
                        ),               
                        
               ),
               
    ), # ribaue
    ##NACALA-A-VELHA
    navbarMenu("NACALA-A-VELHA", icon=icon("exchange-alt"),
               tabPanel("MAPA-Tipo de saneamento",
                        column(12,
                               wellPanel(h4("Mapa de variação em baixo mostra 
                               tipo de latrina mais frequente em Nacala. 
                                            Quando mais carregado for 
                                            a cor do mapa mais familias usam esse tipo de latrina.
                                            Deslize o mouse nos mapas para verificar a percentagem 
                                            das familias que usa determinado tipo de latrina
                                            ")),
                               plotlyOutput("nacala_mapa", height = 500)
                        )),
               tabPanel("Cobertura de saneamento",tabname="nacala_saneamento", icon=icon("chart-line"),
                        column(12,
                               wellPanel( radioButtons("nacala_filtro_cobertura", "Selecione uma opcção:",
                                                       c("Tem Latrina?","Latrina esta sempre disponivel","tem casota")),
                               )
                        ),
                        
                        
                        column(6,
                               plotlyOutput("nacala_saneamento_baseline")
                        ),  
                        column(6,
                               plotlyOutput("nacala_saneamento_endline")
                        ), 
                        
               ), 
               tabPanel("Vontade de contribuir",tabname="nacala_vontade", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel( radioButtons("nacala_filtro_vontade", "Selecione uma opcção:",
                                                       c("Podes contribuir para melhorar a sua sanita?","Com Quanto gostaria de constribuir?")),
                               )
                        ),
                        
                        column(6,
                               plotlyOutput("nacala_vontade_baseline")
                        ),  
                        column(6,
                               plotlyOutput("nacala_vontade_endline")
                        ),                
                        
               ),
               tabPanel("Situação económica",tabname="nacala_situacao", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel( radioButtons("nacala_filtro_economica", "Selecione uma opcção:",
                                                       c("Tipo de Rendimento","Rendimento Mensal")),
                               )
                        ),
                        
                        
                        column(6,
                               plotlyOutput("nacala_economica_baseline")
                        ),  
                        column(6,
                               plotlyOutput("nacala_economica_endline")
                        ),               
                        
               ),
               
    ), # nacala-a-velha
    ##CHUIBA
    navbarMenu("CHUIBA", icon=icon("exchange-alt"),
               tabPanel("MAPA-Tipo de saneamento",
                        column(12,
                               wellPanel(h4("Mapa de variação em baixo mostra 
                               tipo de latrina mais frequente em Chuiba 
                                            Quando mais carregado for 
                                            a cor do mapa mais familias usam esse tipo de latrina.
                                            Deslize o mouse nos mapas para verificar a percentagem 
                                            das familias que usa determinado tipo de latrina
                                            ")),
                               plotlyOutput("chuiba_mapa", height = 500)
                        )),
               tabPanel("Cobertura de saneamento",tabname="Chuiba_saneamento", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel( radioButtons("chuiba_filtro_saneamento", "Selecione uma opcção:",
                                                       c("Tem Latrina?","Latrina esta sempre disponivel","tem casota")),
                               )
                        ),
                        
                        column(6,
                               plotlyOutput("Chuiba_saneamento_baseline")
                        ),  
                        column(6,
                               plotlyOutput("Chuiba_saneamento_endline")
                        ), 
                        
               ), 
               tabPanel("Vontade de contribuir",tabname="Chuiba_vontade", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel( radioButtons("chuiba_filtro_vontade", "Selecione uma opcção:",
                                                       c("Podes contribuir para melhorar a sua sanita?","Com Quanto gostaria de constribuir?")),
                               )
                        ),
                        
                        
                        column(6,
                               plotlyOutput("Chuiba_vontade_baseline")
                        ),  
                        column(6,
                               plotlyOutput("Chuiba_vontade_endline")
                        ),                
                        
               ),
               tabPanel("Situação económica",tabname="Chuiba_situacao", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel( radioButtons("chuiba_filtro_economica", "Selecione uma opcção:",
                                                       c("Tipo de Rendimento","Rendimento Mensal")),
                               )
                        ),
                        
                        
                        column(6,
                               plotlyOutput("Chuiba_economica_baseline")
                        ),  
                        column(6,
                               plotlyOutput("Chuiba_economica_endline")
                        ),               
                        
               ),
               
    ), # Chuiba
    ###Mandimba   
    
    ##Mandimba
    navbarMenu("MANDIMBA", icon=icon("exchange-alt"),
               tabPanel("MAPA-Tipo de saneamento",
                        column(12,
                               wellPanel(h4("Mapa de variação em baixo mostra 
                               tipo de latrina mais frequente em Mandimba 
                                            Quando mais carregado for 
                                            a cor do mapa mais familias usam esse tipo de latrina.
                                            Deslize o mouse nos mapas para verificar a percentagem 
                                            das familias que usa determinado tipo de latrina
                                            ")),
                               plotlyOutput("mandimba_mapa", height = 500)
                        )),
               tabPanel("Cobertura de saneamento",tabname="Mandimba_saneamento", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel( radioButtons("mandimba_filtro_saneamento", "Selecione uma opcção:",
                                                       c("Tem Latrina?","Latrina esta sempre disponivel","tem casota")),
                               )
                        ),
                        
                        
                        column(6,
                               plotlyOutput("Mandimba_saneamento_baseline")
                        ),  
                        column(6,
                               plotlyOutput("Mandimba_saneamento_endline")
                        ), 
                        
               ), 
               tabPanel("Vontade de contribuir",tabname="Mandimba_vontade", icon=icon("chart-line"),
                        # Define as colunas do layout de grade 
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel( radioButtons("mandimba_filtro_vontade", "Selecione uma opcção:",
                                                       c("Podes contribuir para melhorar a sua sanita?","Com Quanto gostaria de constribuir?")),
                               )
                        ),
                        
                        
                        column(6,
                               plotlyOutput("Mandimba_vontade_baseline")
                        ),  
                        column(6,
                               plotlyOutput("Mandimba_vontade_endline")
                        ),                
                        
               ),
               tabPanel("Situação económica",tabname="Mandimba_situacao", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel( radioButtons("mandimba_filtro_economica", "Selecione uma opcção:",
                                                       c("Tipo de Rendimento","Rendimento Mensal")),
                               )
                        ),
                        
                        
                        column(6,
                               plotlyOutput("Mandimba_economica_baseline")
                        ),  
                        column(6,
                               plotlyOutput("Mandimba_economica_endline")
                        ),               
                        
               ),
               
    ), # Mandimba    
    
    
    
  )
  
)
#)


# Define o servidor
server <- function(input, output) {
  ###########MONAPO#
  output$monapo_mapa<- renderPlotly({
     funcao_mapa(monapo_sf, monapo)
    })
  
  ###########MONAPO#
  output$ribaue_mapa<- renderPlotly({
    funcao_mapa(ribaue_sf, ribaue)
  })

}
# Execute o aplicativo Shiny
shinyApp(ui, server)

