library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(shinythemes)
library(readxl)
library(esquisse)
library(leaflet)
library(RColorBrewer)
library(esquisse)
#table(nacala$tem_condicoes)
#table(nacala$quanto_contribuir)
###base de dados de nampula

 
##DEFINIR DIRECTORIO 
setwd("C:/Users/Dercio/OneDrive/Projectos Rstudio/2023/app_susamati/nampula")
list.files()

#Livro_clear <- read_excel("C:/Users/Dercio/OneDrive/Projectos Rstudio/2023/app_susamati/nampula/test.xlsx")
## CARREGAR AS BASE DE DADOS
Nampula_clear <- read_excel("Nampula_clear.xls")  
Nampula <- read_excel("Nampula.xls", na = "##N/A##")
 
###shapfile provincia e Distrito 
provincia <- readRDS("shapfiles/sf_provinces.rds")
distritoSH <- readRDS("shapfiles/sf_districts.rds") 
distritoribaue <- readRDS("shapfiles/sf_districts.rds") %>% filter(District_ID %in% "MZ0723")
distritonacala <- readRDS("shapfiles/sf_districts.rds") %>% filter(District_ID %in% "MZ0720")



## Distrito de Ribaue

##BASE DE DADOS DE Ribaue
nacala <- Nampula_clear %>% filter(District_ID %in% "MZ0720")  

nacala$tipo_casa_banho1<- ifelse(is.na(nacala$tipo_casa_banho1), "Sem Latrina", nacala$tipo_casa_banho1)

#####
dadosnacala <- nacala %>% 
  group_by(District_ID, distrito,tipo_casa_banho1) %>% 
  summarise(n=n()) %>% mutate(per=n/sum(n)*100)

dados_mapanacala = merge(distritonacala, dadosnacala, by = "District_ID")
 
##distritoribaue %>% left_join(dados)

###MAPAS DE BASELINE DE MONAPO
  nacala_mapa_Bl<-ggplot(dados_nacala) +
  aes(fill = percentagem, size = distrito.x, group = percentagem) +
  geom_sf() +
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  labs(fill = "Percentagem ", title = "" )  +
  theme_linedraw() +
  theme(plot.caption = element_text(hjust = 1), 
        axis.title.y = element_text(hjust = 1),
        axis.text.x = element_blank())  +
  facet_wrap(vars(tipo_casa_banho1), nrow = 1L)

  nacala_mapa_Bl


# Salvar o gráfico em um arquivo PNG
ggsave("nacala_mapa_Bl.png", nacala_mapa_Bl, width = 20, height = 10, dpi = 500)





 
##BASE DE DADOS DE MONAPO
monapo <- Nampula_clear %>% filter(District_ID %in% "MZ0715")  

monapo$tipo_casa_banho <- ifelse(is.na(monapo$tipo_casa_banho), "Sem Latrina", monapo$tipo_casa_banho)



#monapo$Gps_fora__Latitude
#monapo$Gps_fora__Longitude



dados <- monapo %>% 
  group_by(District_ID, distrito,tipo_casa_banho,Gps_fora__Latitude,Gps_fora__Longitude) %>% 
  summarise(n=n())  %>% mutate(per=n/sum(n)*100)
 
dados_mapa = distritoSH %>% left_join(dados)


###MAPAS DE BASELINE DE MONAPO
monapo_mapa_Bl<-ggplot(dados_monapo) +
  aes(fill = percentagem, size = distrito.x, group = percentagem) +
  geom_sf() +
  scale_fill_distiller(palette = "Blues", 
                    direction = 1) +
  labs(fill = "Percentagem ", title = "BASELINE" )  +
  theme_linedraw() +
  theme(plot.caption = element_text(hjust = 0.5), 
        axis.title.y = element_text(hjust = 0),
        axis.text.x = element_blank())  +
  facet_wrap(vars(tipo_casa_banho1), nrow = 1L)

#esquisser(dados_mapa)
monapo_mapa_Bl


# Salvar o gráfico em um arquivo PNG
ggsave("monapo_mapa_Bl.png", monapo_mapa_Bl, width = 20, height = 10, dpi = 500)


####MAPAS DE MONAPO
ribaue_mapa<-ggplot(dados_ribaue) +
  aes(fill = percentagem, size = distrito.x, group = percentagem) +
  geom_sf() +
  scale_fill_distiller(palette = "BuGn", 
                       direction = 1, limits = c(0, 100)) +
  labs(fill = "Percentagem ", title = "ENDLINE", c(0,100) )  +
  theme_linedraw() +
  theme(plot.caption = element_text(hjust = 0.5), 
        axis.title.y = element_text(hjust = 0),
        axis.text.x = element_blank()) +
  facet_wrap(vars(tipo_casa_banho1), nrow = 1L)

ribaue_mapa

ggsave("ribaue_mapa.png", ribaue_mapa, width = 20, height = 10, dpi = 500)

##################################################################################################

   localidade_monapo= Nampula_clear %>% filter(District_ID %in% "MZ0715")  

#esquisser(Nampula_clear)
library(dplyr)
library(ggplot2)

 
##localidade monapo
localidade_monapo=Nampula_clear %>%
 filter(distrito %in% "Monapo") %>% group_by(localidade)%>%
  summarise(n = n())%>% 
 ggplot() + aes(x = localidade, y=n, fill = localidade) +
 geom_col()+labs(y = "Nr de familias mapeadas",x="Localidade")+
  theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+
  scale_y_continuous(limits=c(0,250))+ ggtitle("Distrito de monapo")+
  geom_text(aes(label=n), vjust=-1.6, color="black",size=3.5)
localidade_monapo
ggsave("localidade_monapo.png", localidade_monapo, width = 6, height = 4, dpi = 300)



##localidade Rubaue
localidade_ribaue=Nampula_clear %>%
  filter(distrito %in% "Ribaué") %>% group_by(localidade)%>%
  summarise(n = n())%>% 
  ggplot() + aes(x = localidade, y=n, fill = localidade) +
  geom_col()+labs(y = "Nr de familias mapeadas",x="Localidade")+
  theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+
  scale_y_continuous(limits=c(0,250))+ ggtitle("Distrito de Ribaué")+
  geom_text(aes(label=n), vjust=-1.5, color="black",size=3.5)
localidade_ribaue
ggsave("localidade_ribaue.png", localidade_ribaue, width = 6, height = 4, dpi = 300)


##localidade Nacala-a-velha
localidade_ribaue=Nampula_clear %>%
  filter(distrito %in% "Nacala-a-velha") %>% group_by(localidade)%>%
  summarise(n = n())%>% 
  ggplot() + aes(x = localidade, y=n, fill = localidade) +
  geom_col()+labs(y = "Nr de familias mapeadas",x="Localidade")+
  theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+
  scale_y_continuous(limits=c(0,250))+ ggtitle("Distrito de Nacala-a-velha")+
  geom_text(aes(label=n), vjust=-1.5, color="black",size=3.5)
 
ggsave("localidade_Nacala-a-velha.png", localidade_ribaue, width = 8, height = 4, dpi = 300)

d=Nampula_clear %>% group_by(distrito) %>% summarise(n = n())

##localidade Naampula
localidade_nampula=Nampula_clear %>% group_by(distrito)%>%
  summarise(n = n())

 
localidade_nampula=ggplot(localidade_nampula) + aes(x = distrito, y=n, fill = distrito) +
  geom_col()+labs(y = "Nr de familias mapeadas",x="distrito")+
  theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+
  scale_y_continuous(limits=c(0,250))+ ggtitle("Sanear Nampula")+
  geom_text(aes(label=n), vjust=1.6, color="black",size=3.5)
localidade_nampula
# Salvar o gráfico em um arquivo PNG
ggsave("localidade_Nampula.png", localidade_nampula, width = 6, height = 4, dpi = 300)


###SEXO DE CHEFE DE FAMILIA
sexo_chefe=Nampula_clear %>% group_by(sexo_chefe, distrito)%>%
  summarise(n = n())
 
sexo_chefegrafico=ggplot(sexo_chefe) +
 aes(x = sexo_chefe, y = n, fill = sexo_chefe) +
 geom_col() +
 scale_fill_hue(direction = 1) +
 theme_minimal() +
 facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas",x="distrito")+
  theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+
  scale_y_continuous(limits=c(0,250))+ ggtitle("Sanear Nampula")+
  geom_text(aes(label=n), vjust=-0.5, color="black",size=3.5)
sexo_chefegrafico
# Salvar o gráfico em um arquivo PNG
ggsave("sexo_chefe.png", sexo_chefegrafico, width = 10, height = 4, dpi = 400)



#####grafico sobre a idade do chefe de familia por distrito 
idadeChefe=Nampula_clear  %>% group_by(distrito, int_idadeChefe)%>%
  summarise(n = n())
ggsave("idade_chefe.png",
ggplot(idadeChefe) +
  aes(x =int_idadeChefe, y=n, fill = int_idadeChefe) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas",x="Idade")+
  theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+ 
  geom_text(aes(label=n), vjust=0, color="black",size=3.5), 
width = 10, height = 4, dpi = 400)
 


##paredes da casa

parede=Nampula_clear  %>% group_by(distrito, parede)%>%
  summarise(n = n())
ggsave("parede.png",
       ggplot(parede) +
         aes(x =parede, y=n, fill = parede) +
         geom_col() +
         scale_fill_hue(direction = 1) + coord_flip()  +
         facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas",x="Paredes da casa")+
         theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+ 
         geom_text(aes(label=n), hjust=0, color="black",size=3.5)+ylim(c(0,250)), 
       width = 15, height = 6, dpi = 350)
 


####chao da casa
chao=Nampula_clear  %>% group_by(distrito, chao_casa)%>%
  summarise(n = n())
ggsave("chao.png",
       ggplot(chao) +
         aes(x =chao_casa, y=n, fill = chao_casa) +
         geom_col() +
         scale_fill_hue(direction = 1) + coord_flip()  +
         facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas", x="Chão da casa")+
         theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+ 
         geom_text(aes(label=n), hjust=0, color="black",size=3.5)+ylim(c(0,250)), 
       width = 18, height = 6, dpi = 350)
 
##fonte_agua

fonte_agua=Nampula_clear  %>% group_by(distrito, fonte_agua)%>%
  summarise(n = n())
ggsave("fonte_agua.png",
       ggplot(fonte_agua) +
         aes(x =fonte_agua, y=n, fill = fonte_agua) +
         geom_col() +
         scale_fill_hue(direction = 1) + coord_flip()  +
         facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas", x="Fonte de Agua")+
         theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+ 
         geom_text(aes(label=n), hjust=0, color="black",size=3.5)+ylim(c(0,250)), 
       width = 18, height = 6, dpi = 350)


#### fonte_energia

fonte_energia=Nampula_clear  %>% group_by(distrito, fonte_energia)%>%
  summarise(n = n())
ggsave("fonte_energia.png",
       ggplot(fonte_energia) +
         aes(x =fonte_energia, y=n, fill = fonte_energia) +
         geom_col() +
         scale_fill_hue(direction = 1) + coord_flip()  +
         facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas", x="Fonte de Energia")+
         theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+ 
         geom_text(aes(label=n), hjust=0, color="black",size=3.5)+ylim(c(0,250)), 
       width = 18, height = 6, dpi = 350)

#### rendimento_tipo

rendimento_tipo=Nampula_clear  %>% group_by(distrito, rendimento_tipo)%>%
  summarise(n = n())
ggsave("rendimento_tipo.png",
       ggplot(rendimento_tipo) +
         aes(x =rendimento_tipo, y=n, fill = rendimento_tipo) +
         geom_col() +
         scale_fill_hue(direction = 1) + coord_flip()  +
         facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas", x="Tipo rendimento")+
         theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+ 
         geom_text(aes(label=n), hjust=0, color="black",size=3.5)+ylim(c(0,250)), 
       width = 18, height = 6, dpi = 350)

#### rendimento_tipo

rendimento_faixa=Nampula_clear  %>% group_by(distrito, rendimento_faixa)%>%
  summarise(n = n())
ggsave("rendimento_faixa.png",
       ggplot(rendimento_faixa) +
         aes(x =rendimento_faixa, y=n, fill = rendimento_faixa) +
         geom_col() +
         scale_fill_hue(direction = 1) + coord_flip()  +
         facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas", x="Rendimento da familia")+
         theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+ 
         geom_text(aes(label=n), hjust=0, color="black",size=3.5)+ylim(c(0,250)), 
       width = 18, height = 6, dpi = 350)


#quanto_contribuir %>%


tem_condicoes=Nampula_clear  %>% filter(!is.na(tem_condicoes)) %>% group_by(distrito, tem_condicoes)%>%
  summarise(n = n())
ggsave("tem_condicoes.png",
       ggplot(tem_condicoes) +
         aes(x =tem_condicoes, y=n, fill = tem_condicoes) +
         geom_col() +
         scale_fill_hue(direction = 1) + coord_flip()  +
         facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas", x="Tem condições em contribuir para melhorar a sua sanita")+
         theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+ 
         geom_text(aes(label=n), hjust=0, color="black",size=3.5)+ylim(c(0,250)), 
       width = 18, height = 6, dpi = 350)


##quanto_contribuir
quanto_contribuir= Nampula_clear  %>% filter(!is.na(tem_condicoes), Nampula_clear$tem_condicoes=="SIM") 
quanto_contribuir =quanto_contribuir %>% group_by(distrito, quanto_contribuir) %>% summarise(n = n())

graficoContribui=quanto_contribuir %>%
  filter(!is.na(quanto_contribuir1)) %>%
  ggplot() +
  aes(x = quanto_contribuir1, y = n, fill = distrito) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_light()  +coord_flip() +
  facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas", x="Por quanto pode contribuir")+
  theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+
  geom_text(aes(label=n), hjust=-1, color="black",size=3.5)+ylim(c(0,250)) 

graficoContribui
ggsave("quanto_contribuir.png",graficoContribui, 
       width = 18, height = 6, dpi = 350)

#esquisser(quanto_contribuir)


### Mapa de Ribaue 
##BASE DE DADOS DE Ribaue


Ribaue <- Nampula_clear %>% filter(District_ID %in% "MZ0723")  


Ribauedados <- Ribaue %>% 
  group_by(distrito, tipo_casa_banho1) %>% 
  summarise(n=n())  %>% mutate(per=n/sum(n)*100)

distritoribaue <- readRDS("shapfiles/sf_districts.rds") %>% filter(District_ID %in% "MZ0723")
esquisser(distritoribaue)
Ribauemapa = distritoribaue %>% left_join(Ribauedados)

###MAPAS DE BASELINE DE Ribaue
Ribaue_mapa_Bl<-ggplot(Ribaue) +
  aes(fill = n, size = distrito, group = per) +
  geom_sf() +
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  labs(fill = "Percentagem ", title = "BASELINE" )  +
  theme_linedraw() +
  theme(plot.caption = element_text(hjust = 0.5), 
        axis.title.y = element_text(hjust = 0),
        axis.text.x = element_blank())  +
  facet_wrap(vars(tipo_casa_banho), nrow = 1L)

Ribaue_mapa_Bl

### nampula
 
#Tem_latrina= Nampula_clear  %>% filter(!is.na(Tem_latrina)) 
Tem_latrina =Nampula_clear %>% group_by(distrito, Tem_latrina)%>% summarise(n = n())

graficoTem_latrina=Tem_latrina %>%
  ggplot() +
  aes(x = Tem_latrina, y = n, fill = distrito) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_light()  +coord_flip() +
  facet_wrap(vars(distrito))+labs(y = "Nr de familias mapeadas", x="Por quanto pode contribuir")+
  theme_bw(base_size =14, base_line_size =11/22, base_rect_size = 11/22)+
  geom_text(aes(label=n), hjust=0, color="black",size=3.5)+ylim(c(0,180)) 


ggsave("Tem_latrina.png",graficoTem_latrina, 
       width = 18, height = 6, dpi = 350)
Nampula_clear <- read_excel("Nampula_clear.xls")  

 
 