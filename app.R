library(shiny)
library(bslib)
library(ggspatial)
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
library(shinycssloaders)
library(ggmap)
library(ggspatial)
library(rmarkdown)
 
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
    
   ##VISAO GERAL
  navbarMenu("SANEAR NAMPULA",
             tabPanel("Overview",
             #uiOutput("rmd_page")
                       h2("SUSAMATI"),
                       wellPanel(p("Susamati é um negócio social que traz soluções inovadoras para o saneamento em Moçambique, desenvolvendo
                       projectos a níveis de diferentes províncias do país com o objectivo de melhorar o saneamento das famílias,
                       principalmente as que vivem em zonas suburbanas e rurais")),
                       h2("Sanear Nampula"),
                       wellPanel(p("")),
             column(12,
                    wellPanel(withSpinner(plotlyOutput("mapa_nampula", height = 500),color="black"))),
             p("VISITAMOS 722 FAMILIAS NOS 3 DISTRITOS COMO MOSTRA O GRAFICO EM BAIXO"),
             column(12,
                    wellPanel(withSpinner(plotlyOutput("dados_nampula", height = 500),color="black"))),
             
             
   )),
  
 
   
    #BASELINE  
    navbarMenu("MONAPO", icon=icon("exchange-alt"),
               tabPanel("MAPA-Tipo de saneamento",
                        column(12,
                               wellPanel(h4("Mapa de variação em baixo mostra 
                               tipo de latrina mais frequente em Monapo. 
                                            Quanto mais carregado for 
                                            a cor do mapa mais familias usam esse tipo de latrina.
                                            Deslize o mouse nos mapas para verificar a percentagem 
                                            das familias que usa determinado tipo de latrina
                                            ")),
                               withSpinner(plotlyOutput("monapo_mapa", height = 500),color="black")
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
                               withSpinner( plotlyOutput("monapo_saneamento_baseline", height = 500),color="black")),
                        
                        
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
                               withSpinner(plotlyOutput("monapo_vontade_baseline",height = 500),color="purple")
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
                               withSpinner(plotlyOutput("monapo_economica_baseline",height = 500),color="purple")
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
                                            Quanto mais carregado for 
                                            a cor do mapa mais familias usam esse tipo de latrina.
                                            Deslize o mouse nos mapas para verificar a percentagem 
                                            das familias que usa determinado tipo de latrina
                                            ")),
                               withSpinner(plotlyOutput("ribaue_mapa", height = 500),color="purple")
                        )),
               tabPanel("Cobertura de saneamento",tabname="ribaue_saneamento", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel(radioButtons("ribaue_filtro_cobertura", "Selecione uma opcção:",
                                                      c("Tem Latrina?" ,"Latrina esta sempre disponivel","tem casota")),
                               )
                        ),
                        
                        
                        column(6,
                               withSpinner(plotlyOutput("ribaue_saneamento_baseline"),color="purple")
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
                               withSpinner(plotlyOutput("ribaue_vontade_baseline"),color="purple")
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
                               withSpinner(plotlyOutput("ribaue_economica_baseline"),color="purple")
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
                                            Quanto mais carregado for 
                                            a cor do mapa mais familias usam esse tipo de latrina.
                                            Deslize o mouse nos mapas para verificar a percentagem 
                                            das familias que usa determinado tipo de latrina
                                            ")),
                               withSpinner(plotlyOutput("nacala_mapa", height = 500),color="purple")
                        )),
               tabPanel("Cobertura de saneamento",tabname="nacala_saneamento", icon=icon("chart-line"),
                        column(12,
                               wellPanel( radioButtons("nacala_filtro_cobertura", "Selecione uma opcção:",
                                                       c("Tem Latrina?","Latrina esta sempre disponivel","tem casota")),
                               )
                        ),
                        
                        
                        column(6,
                               withSpinner(plotlyOutput("nacala_saneamento_baseline"),color="purple")
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
                               withSpinner(plotlyOutput("nacala_vontade_baseline"),color="purple")
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
                               withSpinner(plotlyOutput("nacala_economica_baseline") ,color="purple")
                               
                        ),  
                        column(6,
                               plotlyOutput("nacala_economica_endline")
                        ),               
                        
               ),
               
    ), # nacala-a-velha

 
    
    
    
  )
  
)
#)


# Define o servidor
server <- function(input, output) {
  
#______________mapas______________________#
  output$mapa_nampula<- renderPlotly({
    # Criar uma paleta de cores para os distritos destacados
    cores_distritos <- c("Nacala-A-Velha" = "#9C51B6", "Ribaue" = "#FF8C00", "Monapo" = "#7FFF00")
    # Plotar o mapa de Nampula com os distritos destacados
    
    # Definir a ordem dos níveis da variável distrito
  #  distritos_nampula$distrito <- factor(distritos_nampula$distrito, levels = c("Nacala-A-Velha", "Ribaue", "Monapo"))
    ggplot(distritos_nampula) +
      aes(fill = distrito, colour=provincia) +
      geom_sf(size = 1.2) +
      scale_fill_manual(values =c("Nacala-A-Velha" = "#9C51B6", "Ribaue" = "#FF8C00", "Monapo" = "#7FFF00")) +
     scale_color_hue(direction = 1) +
      labs(title = "Provincia de Nampula") +
      theme(panel.background = element_rect(fill = "#Fdd2d4", color = NA)) +
      theme_light()+theme(panel.grid = element_blank(),               # Remover linhas de grade
            axis.text = element_blank(),                # Remover rótulos dos eixos
            axis.title = element_blank(),               # Remover títulos dos eixos
            legend.position = "bottom",                 # Posicionar a legenda abaixo do mapa
            legend.title = element_blank(),             # Remover título da legenda
            legend.key.width = unit(0.7, "cm"),          # Ajustar a largura das cores da legenda
            legend.key.height = unit(0.3, "cm")) #,         # Ajustar a altura das cores da legenda
         #   legend.background = element_rect(fill = "white", color = "black")
         #)  # Estilizar fundo da legenda
      #guides(fill = guide_legend(override.aes = list(colour = cores_distritos)))
  })
  
  output$dados_nampula<- renderPlotly({
    
  # Plotar o gráfico de barras com o tema "stata" e os valores das colunas
  ggplot(Nampula_clear) +
    aes(x = distrito, fill = distrito) +
    geom_bar(stat = "count", show.legend = FALSE) +
    #scale_fill_manual(values = stata_colors(length(unique(Nampula_clear$distrito)))) +
    geom_text(aes(label = stat(count)), stat = "count", vjust = -0.5) +
    theme_stata() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Numero de familias visitadas por distrito", x = "Distrito", y = "Contagem")
  })
  
  ###########MONAPO#
  output$monapo_mapa<- renderPlotly({
     funcao_mapa(monapo_sf, monapo,8,40)
    })
  
  ###########Ribaue#
  output$ribaue_mapa<- renderPlotly({
    funcao_mapa(ribaue_sf, ribaue,6,40)
  })
  ###########mapa nacala#
  output$nacala_mapa<- renderPlotly({
    funcao_mapa(nacala_sf, nacala,6,40)
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
 
  
  ####PDF
  #output$rmd_page <- renderUI({
   # includeMarkdown("visaogeral.Rmd")
  #})
}
# Execute o aplicativo Shiny
shinyApp(ui, server)

