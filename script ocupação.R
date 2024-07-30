### Limpar o environment
rm(list=ls())

### Carregar os pacotes (lembrar de instalar com install.packages("") a primeira vez que for rodar o código)

library(dplyr) 
library(readxl)
library(writexl)

### Definir o caminho em que os arquivos gerados pelo Wikidata estão em seu PC

# Lembrar de sempre utilizar a barra invertida '/' no caminho do arquivo do seu PC"

caminho_pt <- ".csv" # Arquivo que contém os verbetes na Wikipédia lusófona
caminho_en <- ".csv" # Arquivo que contém os verbetes na Wikipédia anglo
caminho_es <- ".csv" # Arquivo que contém os verbetes na Wikipédia hispânica

### Carregar os arquivos

wiki_pt <- read.csv(caminho_pt)
wiki_en <- read.csv(caminho_en)
wiki_es <- read.csv(caminho_es)

### Juntar as três tabelas em uma só com base na coluna "item" (o QID do Wikidata)

result <- full_join(wiki_en, wiki_es, by = "item", suffix = c("_en", "_es")) %>%
  full_join(wiki_pt, by = "item", suffix = c("", "_pt")) %>%
  select(article1 = article_en, article2 = article_es, article3 = article, item, imageLabel)

### Selecionar as colunas pertinentes nesse nova tabela

result <- select(result, article3, article1, article2, item, imageLabel)

### Copiar os valores das colunas 'article_en' e 'article_es' para a coluna 'article' se estiverem presentes

# Assim colocamos os verbetes provenientes das outras tabelas na coluna principal, que será a coluna "Artigos em português"

result$article3 <- ifelse(is.na(result$article3), result$article2, result$article3)
result$article3 <- ifelse(is.na(result$article3), result$article1, result$article3)

### Tratamento da coluna "imageLabel"

# Aqui a ideia é inserir o botão "enviar" na coluna "imagem" quando não houver o link para o Commons

# O código aqui está pouquíssimo otimizado mas por enquanto vou manter assim

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

### Tratamento da coluna article_en e article_es

# Aqui a ideia é inserir o botão clicável "Traduzir" quando encontrar for encontrado um valor

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

### Tratamento da coluna "Item"

# Aqui a ideia é inserir o botão clicável "Criar" quando não for encontrado valor nenhum na célula
# Quando encontrar, formatar o valor para virar um link para o Wikidata

new_item <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar|class=mw-ui-constructive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

result$item <- ifelse(
  is.na(result$item),
  paste0(new_item, result$item),
  gsub("(Q[0-9]+)", "style=\"text-align: center;\"|[[d:\\1|\\1]]", result$item)
)

### Renomeando as colunas

names(result) <- c("Artigo em português", 
                   "Artigo em inglês",
                   "Artigo em espanhol", 
                   "Item no Wikidata",
                   "Imagem")

### Exportar

setwd("")
write_xlsx(result, ".xlsx")