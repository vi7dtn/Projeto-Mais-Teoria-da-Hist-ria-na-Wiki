### Limpar o environment do RStudio

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

museusarquivos_pt <- read.csv(caminho_pt)
museusarquivos_en <- read.csv(caminho_en)
museusarquivos_es <- read.csv(caminho_es)

### Tratamento das colunas "article"

# Remover duplicatas baseadas na coluna article

museusarquivos_pt <- wiki_pt %>%
  distinct(article, .keep_all = TRUE)

museusarquivos_en <- wiki_en %>%
  distinct(article, .keep_all = TRUE)

museusarquivos_es <- wiki_es %>%
  distinct(article, .keep_all = TRUE)

# Função de formatação

# A ideia aqui é formatar o valor das células para que a Wikipédia entenda como um link interno

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

museusarquivos_pt$article <- sapply(museusarquivos_pt$article, transform_articlePT)
museusarquivos_en$article <- sapply(museusarquivos_en$article, transform_articleEN)
museusarquivos_es$article <- sapply(museusarquivos_es$article, transform_articleES)

# Função para decodificar percent-encoding

museusarquivos_pt$article <- sapply(museusarquivos_pt$article, URLdecode)
museusarquivos_en$article <- sapply(museusarquivos_en$article, URLdecode)
museusarquivos_es$article <- sapply(museusarquivos_es$article, URLdecode)

### Unir dados através da coluna 'item'

museusarquivos <- full_join(museusarquivos_en, museusarquivos_es, by = "item", suffix = c("_en", "_es")) %>%
  full_join(museusarquivos_pt, by = "item", suffix = c("", "_pt"))

museusarquivos <- select(museusarquivos, article, article_en, article_es, countryLabel, countryLabel_en, countryLabel_es, image, image_en, image_es, item)

### Copiar os valores das colunas

museusarquivos$article[1:136] <- museusarquivos$article_en[1:136] # Alterar as linhas de acordo com os dados
museusarquivos$countryLabel[1:136] <- museusarquivos$countryLabel_en[1:136]
museusarquivos$image[1:136] <- museusarquivos$image_en[1:136]
museusarquivos$article[137:154] <- museusarquivos$article_es[137:154]
museusarquivos$countryLabel[137:154] <- museusarquivos$countryLabel_es[137:154]
museusarquivos$image[137:154] <- museusarquivos$image_es[137:154]

museusarquivos <- select(museusarquivos, article, article_en, article_es, countryLabel, image, item)

### Tratamento dos dados

# Tratamento da coluna "imageLabel"

enviar_imagem <- "style=\"text-align: center;\"|{{Botão clicável 2|Enviar imagem|class=|url=https://commons.wikimedia.org/wiki/Special:UploadWizard?campaign=MaisDiversidade}}"

museusarquivos$image <- ifelse(
  is.na(museusarquivos$image), 
  enviar_imagem, 
  "")

# Tratamento das colunas de tradução (quando encontrar um valor, substituir por)

tratamento0 <- function(x) {
  x <- gsub("\\[\\[", "", x)
  x <- gsub("\\]\\]","", x)
  x <- gsub(" ", "+", x)
}

museusarquivos$article_en[1:136] <- tratamento0(museusarquivos$article_en[1:136]) # Alterar as linhas de acordo com os dados

trans_en <- "style=\"text-align: center;\"|{{User:Stanglavine/en|"

tratamentoI <- function(x) {
  x <- paste0(trans_en, x)
  x <- gsub("$", "}}", x)
}

museusarquivos$article_en[1:136] <- ifelse( # Alterar as linhas de acordo com os dados
  is.na(museusarquivos$article_en[1:136]),
  museusarquivos$article_en[1:136],
  tratamentoI(museusarquivos$article_en[1:136])
)

trans_es <- "style=\"text-align: center;\"|{{User:Stanglavine/es|"

museusarquivos$article_es[1:154] <- tratamento0(museusarquivos$article_es[1:154]) # Alterar as linhas de acordo com os dados

tratamentoII <- function(x) {
  x <- paste0(trans_es, x)
  x <- gsub("$", "}}", x)
}

museusarquivos$article_es[1:154] <- ifelse( # Alterar as linhas de acordo com os dados
  is.na(museusarquivos$article_es[1:154]),
  museusarquivos$article_es[1:154],
  tratamentoII(museusarquivos$article_es[1:154])
)

# Tratamento da coluna "Wikidata"

new_item <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar!|class=mw-ui-progressive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

museusarquivos$item <- ifelse(
  is.na(museusarquivos$item),
  paste0(new_item, museusarquivos$item),
  gsub("(Q[0-9]+)", "style=\"text-align: center;\"|[[d:\\1|\\1]]", museusarquivos$item)
)



# Renomeando as colunas
names(museusarquivos) <- c("Artigo em português", 
                   "Artigo em inglês",
                   "Artigo em espanhol",
                   "Local",
                   "Imagem",
                   "Item no Wikidata",
                   "Erros ortográficos",
                   "Possível destaque")

### Configurar coluna "Local" 

# Adicionar texto em .css para centralizar na tabela quando publicado no domínio da Wikipédia

museusarquivos$Local <- gsub("^", "style=\"text-align: center;\"|", museusarquivos$Local)

### Exportar

setwd("")
write_xlsx(museusarquivos, "museusfinal.xlsx")