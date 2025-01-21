### Limpar ambiente do RStudio

rm(list=ls())

### Carregar os pacotes necessários

library(dplyr)
library(readxl)
library(writexl)
library(stringr)

### Definir o caminho dos arquivos

caminho_lista <- ".xlsx"
caminho_destaque <- ".xlsx"
caminho_orto <- ".xlsx"
caminho_starcheck <- ".xlsx"
caminho_infocaixa <- ".xlsx"
caminho_link <- ".xlsx"

### Carregar os arquivos

lista <- read_excel(caminho_lista)
destaque <- read_excel(caminho_destaque)
orto <- read_excel(caminho_orto)
starcheck <- read_excel(caminho_starcheck)
infocaixa <- read_excel(caminho_infocaixa)
dead_link <- read_excel(caminho_link)

############################################ LISTA MANUTENÇÃO

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

check_icon <- "[[File:OOjs UI icon check-green.svg|center|20px]]"

lista$`Erros ortográficos` <- ifelse(
  is.na(lista$`Erros ortográficos`),
  check_icon,
  tratamentoV(lista$`Erros ortográficos`)
)

### "Incluir coluna "Caixa de informações"

# Duplicar coluna com outro nome

infocaixa$`Caixa de informações` <- infocaixa$Artigo

# Renomear colunas

names(infocaixa) <- c("Artigo em português", 
                 "Caixa de informações")

# Identificar correspondências

lista <- left_join(lista, infocaixa, by = "Artigo em português")

# Formatar coluna para criar botão clicável

tratamentoVIII <- function(string) {
  gsub("\\[\\[(.*)\\]\\]", "style=\"text-align: center;\"|{{Botão clicável|[[\\1|Adicionar]]}}", string)
}

lista$`Caixa de informações` <- ifelse(
  is.na(lista$`Caixa de informações`),
  check_icon,
  tratamentoVIII(lista$`Caixa de informações`)
)

### "Incluir coluna "Links inativos" AINDA NÃO ESTÁ DISPONÍVEL

# Duplicar coluna com outro nome

lista$`Links inativos` <- "style=\"text-align: center;\"|Em breve"

# Renomear colunas

names(infocaixa) <- c("Artigo em português", 
                     "Caixa de informações")

# Identificar correspondências

lista <- left_join(lista, infocaixa, by = "Artigo em português")

# Formatar coluna para criar botão clicável

tratamentoVIII <- function(string) {
  gsub("\\[\\[(.*)\\]\\]", "style=\"text-align: center;\"|{{Botão clicável|[[\\1|Adicionar]]}}", string)
}

lista$`Caixa de informações` <- ifelse(
  is.na(lista$`Caixa de informações`),
  lista$`Erros ortográficos`,
  tratamentoVIII(lista$`Caixa de informações`)
)

### Selecionar artigos apenas da wiki_pt

lista <- lista %>% filter(is.na(`Artigo em inglês`) & is.na(`Artigo em espanhol`))

### Selecionar colunas pertinentes

lista <- select(lista, `Artigo em português`, `Erros ortográficos`, `Caixa de informações`, `Links inativos`)

### Exportar

setwd("")
write_xlsx(lista, "doc_1.xlsx")

############################################ LISTA CRIAÇÃO

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

### Incluir coluna "Starcheck"

# Duplicar coluna com outro nome

starcheck$`Artigo bom ou destacado na Wiki inglesa ou espanhola` <- starcheck$Artigo

# Renomear colunas

names(starcheck) <- c("Artigo em português", 
                     "Artigo bom ou destacado na Wiki inglesa ou espanhola")

# Identificar correspondências

lista <- left_join(lista, starcheck, by = "Artigo em português")

# Formatar coluna para criar botão clicável

tratamentoVII <- function(string) {
  gsub("\\[\\[(.*)\\]\\]", "[[File:Good_article_star_2.svg|center|20px]]", string)
}

lista$`Artigo bom ou destacado na Wiki inglesa ou espanhola` <- ifelse(
  is.na(lista$`Artigo bom ou destacado na Wiki inglesa ou espanhola`),
  lista$`Artigo bom ou destacado na Wiki inglesa ou espanhola`,
  tratamentoVII(lista$`Artigo bom ou destacado na Wiki inglesa ou espanhola`)
)

### Exportar

setwd("")
write_xlsx(lista, "doc_2.xlsx")
