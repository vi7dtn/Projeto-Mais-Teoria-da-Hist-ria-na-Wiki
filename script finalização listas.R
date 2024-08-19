rm(list=ls())

# Carregando os pacotes necessários

library(dplyr)
library(readxl)
library(writexl)
library(stringr)

#Carregar arquivos

lista <- ".xlsx"
caminho_destaque <- ".xlsx"
caminho_orto <- ".xlsx"

lista <- read_excel(camiho_lista)
destaque <- read_excel(caminho_destaque)
orto <- read_excel(caminho_orto)

# "Incluir coluna "Erros ortográficos"

orto$`Erros ortográficos` <- orto$Artigo

names(orto) <- c("Artigo em português", 
                 "Erros ortográficos")

lista <- left_join(lista, orto, by = "Artigo em português")

tratamentoV <- function(string) {
  gsub("\\[\\[(.*)\\]\\]", "style=\"text-align: center;\"|{{Botão clicável|[[\\1|Corrigir]]}}", string)
}

lista$`Erros ortográficos` <- ifelse(
  is.na(lista$`Erros ortográficos`),
  lista$`Erros ortográficos`,
  tratamentoV(lista$`Erros ortográficos`)
)

# Incluir coluna "Possível destaque"

destaque$Artigo <- gsub("^", "[[", destaque$Artigo)

destaque$`Possível destaque` <- destaque$Artigo

names(destaque) <- c("Artigo em português", 
                     "Possível destaque")

lista <- left_join(lista, destaque, by = "Artigo em português")

tratamentoVI <- function(string) {
  gsub("\\[\\[(.*)\\]\\]", "style=\"text-align: center;\"|{{Botão clicável|[[\\1|Destacar]]}}", string)
}

lista$`Possível destaque` <- ifelse(
  is.na(lista$`Possível destaque`),
  lista$`Possível destaque`,
  tratamentoVI(lista$`Possível destaque`)
)

### Exportar

setwd("")
write_xlsx(lista, "")