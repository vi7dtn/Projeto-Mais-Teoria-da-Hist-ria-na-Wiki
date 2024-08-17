### Limpar o ambiente do RSTudio

rm(list=ls())

### Carregar pacotes

library(dplyr)
library(readxl)
library(writexl)

### Definir caminho do arquivo

caminho_multimidia <- ".xlsx"

### Carregar arquivo

multimida <- read_excel(caminho)

### Tratamento dos dados

## Inserir botão clicável na coluna Link

# Para vídeos hospedados no Youtube (adaptar linhas)

multimida$Link[1:4] <- gsub("^", "{{Botão clicável 2|Youtube|class=mw-ui-destructive|url=", multimida$Link[1:4]) # Adaptar linhas
multimida$Link[1:4] <- gsub("$", " }}", multimida$Link[1:4])

# Para imagens hospedadas no Flickr (adaptar linhas)

multimida$Link[5:121] <- gsub("^", "style=\"text-align: center;\"|{{Botão clicável 2|Flickr|class=mw-ui-progressive|url=", lista$Link[5:121])
multimida$Link[5:121] <- gsub("$", " }}", multimida$Link[5:121])

# Para áudios hospedados no Youtube (adaptar linhas)

multimida$Link[122:180] <- gsub("^", "{{Botão clicável 2|Youtube|class=mw-ui-destructive|url=", multimida$Link[122:180])
multimida$Link[122:180] <- gsub("$", " }}", multimida$Link[122:180])

## Centralizar texto

multimidia$Autoria <- gsub("^", "style=\"text-align: center;\"|", multimidia$Autoria)
multimidia$Ocupação <- gsub("^", "style=\"text-align: center;\"|", multimidia$Ocupação)
multimidia$Mídia <- gsub("^", "style=\"text-align: center;\"|", multimidia$Mídia)

## Formatar nome (inserir tipo de conteúdo no início da célula)

multimidia$Nome[122:167] <- gsub("^", "(Podcast)", multimidia$Nome[122:167]) # Adaptar linhas
multimidia$Nome[168:171] <- gsub("^", "(Entrevista)", multimidia$Nome[168:171])
multimidia$Nome[172:180] <- gsub("^", "(Live)", multimidia$Nome[172:180])

### Exportar

setwd("")
write_xlsx(multimidia, ".xlsx")