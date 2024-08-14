### Limpara o ambiente do RStudio

rm(list=ls())

### Carregar pacotes

library(dplyr)
library(readxl)
library(writexl)

### Definir caminho dos arquivos

caminho_pt <- ".csv"
caminho_en <- ".csv"
caminho_es <- ".csv"

### Carregar arquivos

organizações_pt <- read.csv(caminho_pt)
organizações_en <- read.csv(caminho_en)
organizações_es <- read.csv(caminho_es)

### Juntar tabelas com base na coluna "item"

organizações <- full_join(organizações_en, organizações_es, by = "item", suffix = c("_en", "_es")) %>%
  full_join(organizações_pt, by = "item", suffix = c("", "_pt"))

### Tratamento dos dados

# Selecionar colunas pertinentes

organizações <- select(organizações, article, article_en, article_es, countryLabel, countryLabel_en, countryLabel_es, item)

# Copiar os valores das colunas

organizações$article[1:67] <- organizações$article_en[1:67] # Adaptar
organizações$countryLabel[1:67] <- organizações$countryLabel_en[1:67]
organizações$article[68:74] <- organizações$article_es[68:74]
organizações$countryLabel[68:74] <- organizações$countryLabel_es[68:74]

organizações <- select(organizações, article, article_en, article_es, countryLabel, item)

# Tratamento das colunas de tradução (quando encontrar um valor, substituir por)

tratamento0 <- function(x) {
  x <- gsub("\\[\\[", "", x)
  x <- gsub("\\]\\]","", x)
  x <- gsub(" ", "+", x)
}

organizações$article_en[1:67] <- tratamento0(organizações$article_en[1:67])

trans_en <- "style=\"text-align: center;\"|{{User:Stanglavine/en|"

tratamentoI <- function(x) {
  x <- paste0(trans_en, x)
  x <- gsub("$", "}}", x)
}

organizações$article_en[1:67] <- ifelse(
  is.na(organizações$article_en[1:67]),
  organizações$article_en[1:67],
  tratamentoI(organizações$article_en[1:67])
)

trans_es <- "style=\"text-align: center;\"|{{User:Stanglavine/es|"

organizações$article_es[1:74] <- tratamento0(organizações$article_es[1:74])

tratamentoII <- function(x) {
  x <- paste0(trans_es, x)
  x <- gsub("$", "}}", x)
}

organizações$article_es[1:74] <- ifelse(
  is.na(organizações$article_es[1:74]),
  organizações$article_es[1:74],
  tratamentoII(organizações$article_es[1:74])
)

# Tratamento da coluna "Wikidata"

new_item <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar!|class=mw-ui-progressive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

organizações$item <- ifelse(
  is.na(organizações$item),
  paste0(new_item, organizações$item),
  gsub("(Q[0-9]+)", "style=\"text-align: center;\"|[[d:\\1|\\1]]", organizações$item)
)


# Renomeando as colunas

names(organizações) <- c("Artigo em português", 
                   "Artigo em inglês",
                   "Artigo em espanhol",
                   "Local")

### Exportar

setwd("")
write_xlsx(organizações, ".xlsx")
