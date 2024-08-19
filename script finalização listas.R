### Limpar ambiente do RStudio

rm(list=ls())

### Carregar os pacotes necessários

library(dplyr)
library(readxl)
library(writexl)
library(stringr)

### Definir o caminho dos arquivos

lista <- ".xlsx"
caminho_destaque <- ".xlsx"
caminho_orto <- ".xlsx"

### Carregar os arquivos

lista <- read_excel(camiho_lista)
destaque <- read_excel(caminho_destaque)
orto <- read_excel(caminho_orto)

### "Incluir coluna "Erros ortográficos"

# Duplicar coluna com outro nome

orto$`Erros ortográficos` <- orto$Artigo

# Renomear colunas

names(orto) <- c("Artigo em português", 
                 "Erros ortográficos")

# Identificar correspondências

lista <- left_join(lista, orto, by = "Artigo em português")

# Formatar coluna para criar botão clicável

tratamentoV <- function(string) {
  gsub("\\[\\[(.*)\\]\\]", "style=\"text-align: center;\"|{{Botão clicável|[[\\1|Corrigir]]}}", string)
}

lista$`Erros ortográficos` <- ifelse(
  is.na(lista$`Erros ortográficos`),
  lista$`Erros ortográficos`,
  tratamentoV(lista$`Erros ortográficos`)
)

### Incluir coluna "Possível destaque"

# Duplicar coluna com outro nome

destaque$`Possível destaque` <- destaque$Artigo

# Renomear colunas

names(destaque) <- c("Artigo em português", 
                     "Possível destaque")

# Identificar correspondências

lista <- left_join(lista, destaque, by = "Artigo em português")

# Formatar coluna para criar botão clicável

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