rm(list=ls())

library(dplyr)
library(readxl)
library(writexl)

caminho <- "D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/multimídia/multimídia.xlsx"

lista <- read_excel(caminho)

# Formatar links

lista$Link[1:4] <- gsub("^", "{{Botão clicável 2|Youtube|class=mw-ui-destructive|url=", lista$Link[1:4])
lista$Link[1:4] <- gsub("$", " }}", lista$Link[1:4])

lista$Link[5:121] <- gsub("^", "style=\"text-align: center;\"|{{Botão clicável 2|Flickr|class=mw-ui-progressive|url=", lista$Link[5:121])
lista$Link[5:121] <- gsub("$", " }}", lista$Link[5:121])

lista$Link[122:180] <- gsub("^", "{{Botão clicável 2|Youtube|class=mw-ui-destructive|url=", lista$Link[122:180])
lista$Link[122:180] <- gsub("$", " }}", lista$Link[122:180])

# Centralizar texto

lista$Autoria <- gsub("^", "style=\"text-align: center;\"|", lista$Autoria)
lista$Ocupação <- gsub("^", "style=\"text-align: center;\"|", lista$Ocupação)
lista$Mídia <- gsub("^", "style=\"text-align: center;\"|", lista$Mídia)

# Formatar nome (inserir tipo de conteúdo no início do)

lista$Nome[122:167] <- gsub("#VariaHistoria", "(Entrevista) #VariaHistoria", lista$Nome[122:167])
lista$Nome[168:171] <- gsub("H60M", "(Entrevista) H60M", lista$Nome[168:171])
lista$Nome[172:180] <- gsub("Podcast do HuMANAS: Pesquisadoras em Rede -", "(Podcast)", lista$Nome[172:180])

# Exportar
setwd("D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/multimídia")
write_xlsx(lista, "multimídiafinal.xlsx")