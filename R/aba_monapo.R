install_data_packages()

dir <- "C:/Users/Dercio/OneDrive/Projectos Rstudio/2023/Susamati/data"
Nampula_clear <- read_excel(paste(dir, "/Nampula_clear.xls", sep=""))

monapo <- Nampula_clear %>% filter(District_ID %in% "MZ0715") ## Monapo 
monapo$tipo_casa_banho1<- ifelse(is.na(monapo$tipo_casa_banho1), "Sem Latrina", monapo$tipo_casa_banho1)
## shapfile de distritos
monapo_sf <- readRDS(paste(dir, "/sf_districts.rds", sep="")) %>% filter(District_ID %in% "MZ0715")

 
###JUNTAR BASE DE DADOS E SHAPFILE

 
########MAPA DE MONAPO###################

 



































#### GRAFICO de subapa economia
filtro_economia_monapo<- radioButtons("monapo_filtro_economica", "Selecione uma opcção:",
                     c("Tipo de Rendimento","Rendimento Mensal"))


############Rendimento mensal 
rendimento_monapo<-function() {
  visitas <- as.name("famílias visitadas")
  
  grafico(create_summary_table(monapo, "rendimento_tipo"),x=rendimento_tipo,y=Total, fill=rendimento_tipo, monapo,visitas)+
    labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
         x="",
         y="Numero de Familias")+
    theme(axis.text.x = element_blank(), # remove os valores do eixo x
          axis.title.x = element_blank(), # remove o título do eixo x
          axis.line.x = element_blank())+ # remove a linha do eixo x
    theme(legend.position = "top") 
}
 
Tipo_rendimento_monapo <- function() {
  tem_latrina <- as.name("famílias visitadas")
  grafico(create_summary_table(monapo, "rendimento_faixa"), x = rendimento_faixa, y = Total, fill = rendimento_faixa,monapo,tem_latrina) +
    labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
         x = "",
         y = "Numero de Familias") +
    theme(axis.text.x = element_blank(), # remove os valores do eixo x
          axis.title.x = element_blank(), # remove o título do eixo x
          axis.line.x = element_blank())+ # remove a linha do eixo x
    theme(legend.position = "top")
  #return(grafico(monapo, tem_latrina, 250, 50))
}
 
