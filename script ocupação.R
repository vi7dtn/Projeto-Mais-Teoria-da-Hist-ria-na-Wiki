rm(list=ls())

library(dplyr)
library(readxl)
library(writexl)

rm(list=ls())

caminho_pt <- "D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/ocupação/economistas/economistasptclean.csv"
caminho_en <- "D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/ocupação/economistas/economistasenclean.csv"
caminho_es <- "D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/ocupação/economistas/economistasesclean.csv"
caminho_destaque <- "D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/destacados.xlsx"
caminho_orto <- "D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/orto.csv"

wiki_pt <- read.csv(caminho_pt)
wiki_en <- read.csv(caminho_en)
wiki_es <- read.csv(caminho_es)
destaque <- read_excel(caminho_destaque)
orto <- read.csv(caminho_orto)

# Ajuste Destaque

destaque$Article <- gsub("^", "[[", destaque$Article)
destaque$Destacado <- destaque$Article

# Ajuste Orto

orto$Orto <- orto$Artigo

# Tratamento da coluna "imageLabel"

transform_imageLabel <- function(imageLabel) {
  if (nzchar(imageLabel)) {  # Verifica se a string não está vazia
    imageLabel <- sub("http://commons.wikimedia.org/wiki/Special:FilePath/", "[[File:", imageLabel)  # Substitui o início
    imageLabel <- paste0(imageLabel, "|center|80px]]")  # Adiciona o sufixo no final
  }
  return(imageLabel)
}

wiki_pt$imageLabel <- sapply(wiki_pt$imageLabel, transform_imageLabel)

wiki_pt$imageLabel[wiki_pt$imageLabel == "" | wiki_pt$imageLabel == " "] <- NA  # Converter strings vazias e espaços em branco para NA

enviar_imagem <- "style=\"text-align: center;\"|{{Botão clicável 2|Enviar|class=|url=https://commons.wikimedia.org/wiki/Special:UploadWizard?campaign=MaisDiversidade}}"

wiki_pt$imageLabel <- ifelse(
  is.na(wiki_pt$imageLabel), 
  enviar_imagem, 
  "")

# Fazendo o 'match' com base na coluna 'item'
result <- full_join(wiki_en, wiki_es, by = "item", suffix = c("_en", "_es")) %>%
  full_join(wiki_pt, by = "item", suffix = c("", "_pt")) %>%
  select(article1 = article_en, article2 = article_es, article3 = article, item, imageLabel)

result <- select(result, article3, article1, article2, item, imageLabel)

# Copiar os valores das colunas 'article_en' e 'article_es' para a coluna 'article' se estiverem presentes
result$article3 <- ifelse(is.na(result$article3), result$article2, result$article3)
result$article3 <- ifelse(is.na(result$article3), result$article1, result$article3)


# Tratamento da coluna Tradução (quando encontrar um valor, substituir por)

trans_en <- "style=\"text-align: center;\"|{{User:Stanglavine/en|"

tratamentoI <- function(x) {
  x <- gsub("\\[\\[", "", x)
  x <- gsub("\\]\\]", "", x)
  x <- gsub(" ", "+", x)
  return(x)
}

result$article1 <- ifelse(
  is.na(result$article1),
  result$article1,
  tratamentoI(result$article1)
)

tratamentoII <- function(x) {
  x <- paste0(trans_en, x)
  x <- gsub("$", "}}", x)
}

result$article1 <- ifelse(
  is.na(result$article1),
  result$article1,
  tratamentoII(result$article1)
)

trans_es <- "style=\"text-align: center;\"|{{User:Stanglavine/es|"

tratamentoIII <- function(x) {
  x <- gsub("\\[\\[", "", x)
  x <- gsub("\\]\\]", "", x)
  x <- gsub(" ", "+", x)
  return(x)
}

result$article2 <- ifelse(
  is.na(result$article2),
  result$article2,
  tratamentoIII(result$article2)
)

tratamentoIV <- function(x) {
  x <- paste0(trans_es, x)
  x <- gsub("$", "}}", x)
}

result$article2 <- ifelse(
  is.na(result$article2),
  result$article2,
  tratamentoIV(result$article2)
)

# Tratamento da coluna "Item"

new_item <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar|class=mw-ui-constructive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

result$item <- ifelse(
  is.na(result$item),
  paste0(new_item, result$item),
  gsub("(Q[0-9]+)", "style=\"text-align: center;\"|[[d:\\1|\\1]]", result$item)
)

# "Incluir coluna "Correções ortográficas"

names(orto) <- c("article3", 
                   "Orto")

result <- left_join(result, orto, by = "article3")

tratamentoV <- function(string) {
  gsub("\\[\\[(.*)\\]\\]", "style=\"text-align: center;\"|{{Botão clicável|[[\\1|Corrigir]]}}", string)
}

result$Orto <- ifelse(
  is.na(result$Orto),
  result$Orto,
  tratamentoV(result$Orto)
)

# Tratamento coluna "Possível destaque?"

names(destaque) <- c("article3", 
                 "Destacado")

result <- left_join(result, destaque, by = "article3")

tratamentoVI <- function(string) {
  gsub("\\[\\[(.*)\\]\\]", "style=\"text-align: center;\"|{{Botão clicável|[[\\1|Destacar]]}}", string)
}

result$Destacado <- ifelse(
  is.na(result$Destacado),
  result$Destacado,
  tratamentoVI(result$Destacado)
)


# Renomeando as colunas
names(result) <- c("Artigo em português", 
                   "Artigo em inglês",
                   "Artigo em espanhol", 
                   "Item no Wikidata",
                   "Imagem",
                   "Erros ortográficos",
                   "Possível destaque")

# Incluir tabelas "Antropólogas a criar"

caminho_wikipedia <- "D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/ocupação/economistas/economistasacriarwikipedia.xlsx"
caminho_wikidata <- "D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/ocupação/economistas/economistasacriarwikidata.xlsx"
  
wp_criar <- read_excel(caminho_wikipedia)
wd_criar <- read_excel(caminho_wikidata)

# Remover duplicatas: aquilo que estiver em wp_criar, não deve estar em wd_criar

wd_criar <- wd_criar %>%
  anti_join(wp_criar, by = "Artigo em português")

############ Tratamento wp_criar

# Item no Wikidata 

wd_link <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar|class=mw-ui-constructive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

wp_criar$`Item no Wikidata`<- ifelse(
  is.na(wp_criar$`Item no Wikidata`),
  wd_link,
  gsub("(Q[0-9]+)", "style=\"text-align: center;\"|[[d:\\1|\\1]]", wp_criar$`Item no Wikidata`)
)

# Artigo em portuguÊs

apply_hiperlink <- function(x) {
  x <- sub("^", "[[", x)  # Adiciona ]] no começo
  x <- sub("$", "]]", x)  # Adiciona ]] no final
}

wp_criar$`Artigo em português` <- apply_hiperlink(wp_criar$`Artigo em português`)

############ Tratamento wd_criar

# Item no Wikidata

wd_criar$`Item no Wikidata`<- ifelse(
  is.na(wd_criar$`Item no Wikidata`),
  wd_link,
  wd_criar$`Item no Wikidata`
)

########## Juntar dataframes

result <- result %>%
  full_join(wp_criar, by = c("Artigo em português", "Item no Wikidata"))

result <- result %>%
  full_join(wd_criar, by = c("Artigo em português", "Item no Wikidata"))


# Exportar
setwd("D:/reflection/UFRGS/Doutorado/2024/Projeto Mais+/Mais Mulheres/listas a editar/ocupação/economistas")
write_xlsx(result, "economistasfinal.xlsx")