### Limpar o environment do RStudio
rm(list=ls())

### Carregar os pacotes (lembre-se de instalar com install.packages("") a primeira vez que for rodar o código)
library(dplyr)
library(readxl)
library(writexl)

### Definir o caminho dos arquivos (faça para Obras cinematográficas conforme sua necessidade)
caminho_pt <- ".csv"
caminho_en <- ".csv"
caminho_es <- ".csv"

# Carregar os arquivos
filmes_pt <- read.csv(caminho_pt)
filmes_en <- read.csv(caminho_en)
filmes_es <- read.csv(caminho_es)

### Limpeza dos dados
# Remover duplicatas baseadas na coluna article
filmes_pt <- filmes_pt %>%
  distinct(article, .keep_all = TRUE)

filmes_en <- filmes_en %>%
  distinct(article, .keep_all = TRUE)

filmes_es <- filmes_es %>%
  distinct(article, .keep_all = TRUE)

# Formatar o valor das células para que a Wikipédia entenda como um link interno
transform_articlePT <- function(link) {
  link <- sub("https://pt.wikipedia.org/wiki/", "[[", link)
  link <- sub("$", "]]", link)
  link <- gsub("_", " ", link)
  return(link)
}

transform_articleEN <- function(link) {
  link <- sub("https://en.wikipedia.org/wiki/", "[[", link)
  link <- sub("$", "]]", link)
  link <- gsub("_", " ", link)
  return(link)
}

transform_articleES <- function(link) {
  link <- sub("https://es.wikipedia.org/wiki/", "[[", link)
  link <- sub("$", "]]", link)
  link <- gsub("_", " ", link)
  return(link)
}

# Aplicar as funções nas colunas
filmes_pt$article <- sapply(filmes_pt$article, transform_articlePT)
filmes_en$article <- sapply(filmes_en$article, transform_articleEN)
filmes_es$article <- sapply(filmes_es$article, transform_articleES)

# Função para decodificar percent-encoding
filmes_pt$article <- sapply(filmes_pt$article, URLdecode)
filmes_en$article <- sapply(filmes_en$article, URLdecode)
filmes_es$article <- sapply(filmes_es$article, URLdecode)

### Tratamento dos dados
# Fazer a junção das tabelas com base na coluna 'item'

filmes <- full_join(filmes_pt, filmes_en, by = "item", suffix = c("_pt", "_en")) %>%
  full_join(filmes_es, by = "item", suffix = c("", "_es"))

# Limpar início das células da coluna Item
filmes$item <- gsub("http://www.wikidata.org/entity/", "", filmes$item)

# Copiar valores para a coluna 'title <- pt' de acordo com as linhas

filmes$article_pt[23:83] <- filmes$article_en[23:83]
filmes$article_pt[84:85] <- filmes$article[84:85]
filmes$directorLabel_pt[23:83] <- filmes$directorLabel_en[23:83]
filmes$directorLabel_pt[84:85] <- filmes$directorLabel[84:85]

# Seleção das colunas pertinentes
filmes <- select(filmes, article_pt, directorLabel_pt, article_en, article, item)

# Tratamento da coluna Tradução
tratamento0 <- function(x) {
  x <- gsub("\\[\\[", "", x)
  x <- gsub("\\]\\]","", x)
  x <- gsub(" ", "+", x)
}

filmes$article_en[23:83] <- tratamento0(filmes$article_en[23:83])

trans_en <- "style=\"text-align: center;\"|{{User:Stanglavine/en|"
tratamentoI <- function(x) {
  x <- paste0(trans_en, x)
  x <- gsub("$", "}}", x)
}

filmes$article_en[23:83] <- ifelse(
  is.na(filmes$article_en[23:83]),
  filmes$article_en[23:83],
  tratamentoI(filmes$article_en[23:83])
)

trans_es <- "style=\"text-align: center;\"|{{User:Stanglavine/es|"
filmes$article[23:85] <- tratamento0(filmes$article[23:85])

tratamentoII <- function(x) {
  x <- paste0(trans_es, x)
  x <- gsub("$", "}}", x)
}

filmes$article[23:85] <- ifelse(
  is.na(filmes$article[23:85]),
  filmes$article[23:85],
  tratamentoII(filmes$article[23:85])
)

# Tratamento da coluna "Wikidata"
new_item <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar!|class=mw-ui-progressive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"
filmes$item <- ifelse(
  is.na(filmes$item),
  new_item,
  gsub("(Q[0-9]+)", "style=\"text-align: center;\"|[[d:\\1|\\1]]", filmes$item)
)

# Renomeando as colunas
names(filmes) <- c("Artigo em português", "Direção", "Artigo em inglês", "Artigo em espanhol", "Item no Wikidata")

### Exportar
setwd("")
write_xlsx(filmes, ".xlsx")
