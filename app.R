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
   # header = "Meu Cabeçalho",
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
                              wellPanel(filtro_economia_monapo)
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
                               plotlyOutput("chuiba_saneamento_baseline")
                        ),  
                        column(6,
                               plotlyOutput("chuiba_saneamento_endline")
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
                               plotlyOutput("chuiba_vontade_baseline")
                        ),  
                        column(6,
                               plotlyOutput("chuiba_vontade_endline")
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
                               plotlyOutput("chuiba_economica_baseline")
                        ),  
                        column(6,
                               plotlyOutput("chuiba_economica_endline")
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
  
#______________mapas______________________#
  
  ###########MONAPO#
  output$monapo_mapa<- renderPlotly({
     funcao_mapa(monapo_sf, monapo,12,30)
    })
  
  ###########Ribaue#
  output$ribaue_mapa<- renderPlotly({
    funcao_mapa(ribaue_sf, ribaue,11,30)
  })
  ###########mapa nacala#
  output$nacala_mapa<- renderPlotly({
    funcao_mapa(nacala_sf, nacala,11,30)
  })
  #################MAPA DE PEMBA
  
  #__________________cobertura DE SANEAME____________________________

  ####MONAPO
  output$monapo_saneamento_baseline <- renderPlotly({
    if(input$monapo_filtro_cobertura=="Tem Latrina?") {
    tem_latrina(monapo,300,50)
    } else 
      if(input$monapo_filtro_cobertura=="Latrina esta sempre disponivel") {
        disponivel_casa_banho(monapo,300,50)
        
      }else
        if(input$monapo_filtro_cobertura=="tem casota"){
          tem_casota(monapo,300,50)}})
  ####ribaue
  output$ribaue_saneamento_baseline <- renderPlotly({
    if(input$ribaue_filtro_cobertura=="Tem Latrina?") {
      tem_latrina(ribaue,300,50)
    } else 
      if(input$ribaue_filtro_cobertura=="Latrina esta sempre disponivel") {
        disponivel_casa_banho(ribaue,300,50)
        
      }else
        if(input$ribaue_filtro_cobertura=="tem casota"){
          tem_casota(ribaue,300, 50)}})
  ####nacala
  output$nacala_saneamento_baseline <- renderPlotly({
    if(input$nacala_filtro_cobertura=="Tem Latrina?") {
      tem_latrina(nacala,300,50)
    } else 
      if(input$nacala_filtro_cobertura=="Latrina esta sempre disponivel") {
        disponivel_casa_banho(nacala,300,50)
        
      }else
        if(input$nacala_filtro_cobertura=="tem casota"){
          tem_casota(nacala,300, 50)}})
  
  output$chuiba_saneamento_baseline <- renderPlotly({
    if(input$chuiba_filtro_saneamento=="Tem Latrina?") {
      tem_latrina(pemba,1200, 400)
    } else 
      if(input$chuiba_filtro_saneamento=="Latrina esta sempre disponivel") {
        disponivel_casa_banho(pemba,300,50)
        
      }else
        if(input$chuiba_filtro_saneamento=="tem casota"){
          tem_casota(pemba,1000,150)}
    })
  
  
  #______________Vontade de contribuir___________________________________
  ##monapo
  output$monapo_vontade_baseline <- renderPlotly({
  if(input$monapo_filtro_vontade=="Podes contribuir para melhorar a sua sanita?") {
    tem_condicoes(monapo,300,50)
  }else 
    if(input$monapo_filtro_vontade=="Com Quanto gostaria de constribuir?") {
      valor_contribuir(monapo)
       
    } 
  })
  ##monapo
  output$ribaue_vontade_baseline <- renderPlotly({
    if(input$ribaue_filtro_vontade=="Podes contribuir para melhorar a sua sanita?") {
      tem_condicoes(ribaue,300,50)
    }else 
      if(input$ribaue_filtro_vontade=="Com Quanto gostaria de constribuir?") {
        valor_contribuir(ribaue)
        
      } 
  })
  ##nacala
  output$nacala_vontade_baseline <- renderPlotly({
    if(input$nacala_filtro_vontade=="Podes contribuir para melhorar a sua sanita?") {
      tem_condicoes(nacala,300,50)
    }else 
      if(input$nacala_filtro_vontade=="Com Quanto gostaria de constribuir?") {
        valor_contribuir(nacala)
      # ggplotly(hist(monapo$quanto_contribuir,breaks = 20, col = "lightblue")) 
      } 
  })
  
  #_______________Enconomia____________________________________________________
  ##monapo
  output$monapo_economica_baseline <- renderPlotly({
    
    if(input$monapo_filtro_economica=="Tipo de Rendimento") {
       tipo_redimento(monapo,300,50)
      
    } else 
      if(input$monapo_filtro_economica=="Rendimento Mensal") {
        rendimento_faixa(monapo,300,50)
      } })
  ## ribaue
  output$ribaue_economica_baseline <- renderPlotly({
    
    if(input$ribaue_filtro_economica=="Tipo de Rendimento") {
      tipo_redimento(ribaue,300,50)
       
    } else 
      if(input$ribaue_filtro_economica=="Rendimento Mensal") {
        rendimento_faixa(ribaue,300,50)
      } })
  ## nacala
  output$nacala_economica_baseline <- renderPlotly({
    
    if(input$nacala_filtro_economica=="Tipo de Rendimento") {
      tipo_redimento(nacala,300,50)
      
    } else 
      if(input$nacala_filtro_economica=="Rendimento Mensal") {
        rendimento_faixa(nacala,300,50)
      } })
}
# Execute o aplicativo Shiny
shinyApp(ui, server)

