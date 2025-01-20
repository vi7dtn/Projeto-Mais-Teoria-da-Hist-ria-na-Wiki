### Limpar o environment do RStudio

rm(list=ls())

### Carregar os pacotes (lembrar de instalar com install.packages("") a primeira vez que for rodar o código)

library(dplyr) 
library(readxl)
library(writexl)

### Definir o caminho em que os arquivos gerados pelo Wikidata estão em seu PC

# Lembrar de sempre utilizar a barra invertida '/' no caminho do arquivo do seu PC"

caminho_pt <- "" # Arquivo que contém os verbetes na Wikipédia lusófona
caminho_en <- "" # Arquivo que contém os verbetes na Wikipédia anglo
caminho_es <- "" # Arquivo que contém os verbetes na Wikipédia hispânica

### Carregar os arquivos

instorg_pt <- read.csv(caminho_pt)
instorg_en <- read.csv(caminho_en)
instorg_es <- read.csv(caminho_es)

### Tratamento das colunas "article"

# Remover duplicatas baseadas na coluna article

instorg_pt <- instorg_pt %>%
  distinct(article, .keep_all = TRUE)

instorg_en <- instorg_en %>%
  distinct(article, .keep_all = TRUE)

instorg_es <- instorg_es %>%
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

instorg_pt$article <- sapply(instorg_pt$article, transform_articlePT)
instorg_en$article <- sapply(instorg_en$article, transform_articleEN)
instorg_es$article <- sapply(instorg_es$article, transform_articleES)

# Função para decodificar percent-encoding

instorg_pt$article <- sapply(instorg_pt$article, URLdecode)
instorg_en$article <- sapply(instorg_en$article, URLdecode)
instorg_es$article <- sapply(instorg_es$article, URLdecode)

### Unir dados através da coluna 'item'

instorg <- full_join(instorg_en, instorg_es, by = "item", suffix = c("_en", "_es")) %>%
  full_join(instorg_pt, by = "item", suffix = c("", "_pt"))

instorg <- select(instorg, article, article_en, article_es, countryLabel, countryLabel_en, countryLabel_es, image, image_en, image_es, item)

### Copiar os valores das colunas

instorg$article[1:4] <- instorg$article_en[1:4] # Alterar as linhas de acordo com os dados
instorg$countryLabel[1:4] <- instorg$countryLabel_en[1:4]
instorg$image[1:4] <- instorg$image_en[1:4]
instorg$article[13:23] <- instorg$article_es[13:23]
instorg$countryLabel[13:23] <- instorg$countryLabel_es[13:23]
instorg$image[13:23] <- instorg$image_es[13:23]

# Excluir parte do valor da coluna item

instorg$item <- gsub("http://www.wikidata.org/entity/", "", instorg$item)

# Selecionar colunas pertinentes

instorg <- select(instorg, article, article_en, article_es, countryLabel, image, item)

### Tratamento dos dados

# Tratamento da coluna "imageLabel"

enviar_imagem <- "style=\"text-align: center;\"|{{Botão clicável 2|Enviar imagem|class=|url=https://commons.wikimedia.org/wiki/Special:UploadWizard?campaign=MaisDiversidade}}"

transform_image <- function(image) {
  image <- gsub("http://commons.wikimedia.org/wiki/Special:FilePath/", "[[File:", image)
  image <- paste0(image, "|center|80px]]")
  return(image)
}

instorg$image[instorg$image == "" | instorg$image == " "] <- NA  # Converter strings vazias e espaços em branco para NA

instorg$image <- ifelse(
  is.na(instorg$image), 
  enviar_imagem, 
  transform_image(instorg$image))

# Tratamento das colunas de tradução (quando encontrar um valor, substituir por)

tratamento0 <- function(x) {
  x <- gsub("\\[\\[", "", x)
  x <- gsub("\\]\\]","", x)
  x <- gsub(" ", "+", x)
}

instorg$article_en[1:4] <- tratamento0(instorg$article_en[1:4]) # Alterar as linhas de acordo com os dados

trans_en <- "style=\"text-align: center;\"|{{User:Stanglavine/en|"

tratamentoI <- function(x) {
  x <- paste0(trans_en, x)
  x <- gsub("$", "}}", x)
}

instorg$article_en[1:4] <- ifelse( # Alterar as linhas de acordo com os dados
  is.na(instorg$article_en[1:4]),
  instorg$article_en[1:4],
  tratamentoI(instorg$article_en[1:4])
)

trans_es <- "style=\"text-align: center;\"|{{User:Stanglavine/es|"

instorg$article_es[3] <- tratamento0(instorg$article_es[3]) # Alterar as linhas de acordo com os dados

tratamentoII <- function(x) {
  x <- paste0(trans_es, x)
  x <- gsub("$", "}}", x)
}

instorg$article_es[3] <- ifelse( # Alterar as linhas de acordo com os dados
  is.na(instorg$article_es[3]),
  instorg$article_es[3],
  tratamentoII(instorg$article_es[3])
)

# Tratamento da coluna "Wikidata"

new_item <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar!|class=mw-ui-progressive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

instorg$item <- ifelse(
  is.na(instorg$item),
  paste0(new_item, instorg$item),
  gsub("(Q[0-9]+)", "style=\"text-align: center;\"|[[d:\\1|\\1]]", instorg$item)
)

# Renomeando as colunas
names(instorg) <- c("Artigo em português", 
                   "Artigo em inglês",
                   "Artigo em espanhol",
                   "Local",
                   "Imagem",
                   "Item no Wikidata")

### Configurar coluna "Local" 

# Adicionar texto em .css para centralizar na tabela quando publicado no domínio da Wikipédia

instorg$Local <- gsub("^", "style=\"text-align: center;\"|", instorg$Local)

### Exportar

setwd("")
write_xlsx(instorg, "organizações científicas.xlsx")