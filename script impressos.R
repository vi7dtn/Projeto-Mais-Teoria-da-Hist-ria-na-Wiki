### Limpar o environment do RStudio

rm(list=ls())

### Carregar os pacotes (lembrar de instalar com install.packages("") a primeira vez que for rodar o código)

library(dplyr)
library(readxl)
library(writexl)

### Definir o caminho em que estão no seu PC os arquivos (faça para Obras literárias e não-ficção, conforme sua necessidade)

caminho_pt <- ".csv"
caminho_en <- ".csv"
caminho_es <- ".csv"

impressos_pt <- read.csv(caminho_pt)
impressos_en <- read.csv(caminho_en)
impressos_es <- read.csv(caminho_es)

### Limpeza dos dados

# Remover duplicatas baseadas na coluna article

impressos_pt <- impressos_pt %>%
  distinct(article, .keep_all = TRUE)

impressos_en <- impressos_en %>%
  distinct(article, .keep_all = TRUE)

impressos_es <- impressos_es %>%
  distinct(article, .keep_all = TRUE)

# Formatar o valor das células para que a Wikipédia entenda como um link interno

transform_articlePT <- function(link) {
  link <- sub("https://pt.wikipedia.org/wiki/", "[[", link)  # Substitui o início
  link <- sub("$", "]]", link)  # Adiciona ]] no final
  link <- gsub("_", " ", link)  # Substitui _ por espaço
  return(link)
}

transform_articleEN <- function(link) {
  link <- sub("https://en.wikipedia.org/wiki/", "[[", link)  # Substitui o início
  link <- sub("$", "]]", link)  # Adiciona ]] no final
  link <- gsub("_", " ", link)  # Substitui _ por espaço
  return(link)
}

transform_articleES <- function(link) {
  link <- sub("https://es.wikipedia.org/wiki/", "[[", link)  # Substitui o início
  link <- sub("$", "]]", link)  # Adiciona ]] no final
  link <- gsub("_", " ", link)  # Substitui _ por espaço
  return(link)
}

# Aplicar as funções nas colunas

impressos_pt$article <- sapply(impressos_pt$article, transform_articlePT)
impressos_en$article <- sapply(impressos_en$article, transform_articleEN)
impressos_es$article <- sapply(impressos_es$article, transform_articleES)

# Função para decodificar percent-encoding

impressos_pt$article <- sapply(impressos_pt$article, URLdecode)
impressos_en$article <- sapply(impressos_en$article, URLdecode)
impressos_es$article <- sapply(impressos_es$article, URLdecode)

### Tratamento dos dados

# Fazer a junção das tabelas com base na coluna 'item'

impressos <- full_join(impressos_pt, impressos_en, by = "item", suffix = c("_pt", "_en")) %>%
  full_join(impressos_es, by = "item", suffix = c("", "_es"))

# Copiar os valores das colunas 'title_en' e 'title' para a coluna 'title <- pt' de acordo com as linhas

impressos$article_pt[697:2599] <- impressos$article_en[697:2599] # Alterar as linhas de acordo com os dados
impressos$article_pt[2600:2655] <- impressos$article[2600:2655]
impressos$authorLabel_pt[697:2599] <- impressos$authorLabel_en[697:2599]
impressos$authorLabel_pt[2600:2655] <- impressos$authorLabel[2600:2655]


impressos <- select(impressos, article_pt, authorLabel_pt, article_en, article, item)

# Tratamento da coluna Tradução (quando encontrar um valor, substituir por)

tratamento0 <- function(x) {
  x <- gsub("\\[\\[", "", x)
  x <- gsub("\\]\\]","", x)
  x <- gsub(" ", "+", x)
}

impressos$article_en[697:2599] <- tratamento0(impressos$article_en[697:2599]) # Alterar as linhas de acordo com os dados

trans_en <- "style=\"text-align: center;\"|{{User:Stanglavine/en|"

tratamentoI <- function(x) {
  x <- paste0(trans_en, x)
  x <- gsub("$", "}}", x)
}

impressos$article_en[697:2599] <- ifelse(  # Alterar as linhas de acordo com os dados
  is.na(impressos$article_en[697:2599]),
  impressos$article_en[697:2599],
  tratamentoI(impressos$article_en[697:2599])
)

trans_es <- "style=\"text-align: center;\"|{{User:Stanglavine/es|"

impressos$article[697:2655] <- tratamento0(impressos$article[697:2655])  # Alterar as linhas de acordo com os dados

tratamentoII <- function(x) {
  x <- paste0(trans_es, x)
  x <- gsub("$", "}}", x)
}

impressos$article[697:2655] <- ifelse(  # Alterar as linhas de acordo com os dados
  is.na(impressos$article[697:2655]),
  impressos$article[697:2655],
  tratamentoII(impressos$article[697:2655])
)

# Tratamento da coluna "Wikidata"

new_item <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar!|class=mw-ui-progressive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

impressos$item <- ifelse(
  is.na(impressos$item),
  new_item,
  gsub("(Q[0-9]+)", "style=\"text-align: center;\"|[[d:\\1|\\1]]", impressos$item)
)

# Renomeando as colunas

names(impressos) <- c("Artigo em português",
                      "Autoria",
                      "Artigo em inglês",
                      "Artigo em espanhol", 
                      "Item no Wikidata")

### Exportar
setwd("")
write_xlsx(impressos, ".xlsx")