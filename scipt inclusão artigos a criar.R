### Limpar o environment do RStudio

rm(list=ls())

### Carregar os pacotes (lembrar de instalar com install.packages("") a primeira vez que for rodar o código)

library(dplyr)
library(readxl)
library(writexl)

### Definir o caminho dos arquivos

caminho_wikipedia_criar <- ".xlsx" # Lista de verbetes na Wikipédia a criar
caminho_wikidata_criar <- ".xlsx" # Lista de itens no Wikidata a criar
caminho_lista_ocupação <- ".xlsx" # Lista produzida com os dados de verbetes/itens existentes

### Carregar os arquivos

wp_criar <- read_excel(caminho_wikipedia_criar)
wd_criar <- read_excel(caminho_wikidata_criar)
lista_ocupacao <- read_excel(caminho_lista_ocupação)

### Remover duplicatas 

# Aquilo que estiver já está para criar na Wikipédia, não precisa estar em a criar no Wikidata

wd_criar <- wd_criar %>%
  anti_join(wp_criar, by = "Artigo em português")

### Tratamento wp_criar

# Inserir botão clicável para criação de item no Wikidata e formatação se o dado já for existente

wd_link <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar|class=mw-ui-constructive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

wp_criar$`Item no Wikidata`<- ifelse(
  is.na(wp_criar$`Item no Wikidata`),
  wd_link,
  gsub("(Q[0-9]+)", "style=\"text-align: center;\"|[[d:\\1|\\1]]", wp_criar$`Item no Wikidata`)
)

# Formatar a coluna "Artigo em português" para ficar com a hiperligação interna quando publicarmos a tabela na Wikipédia

apply_hiperlink <- function(x) {
  x <- sub("^", "[[", x)  # Adiciona ]] no começo
  x <- sub("$", "]]", x)  # Adiciona ]] no final
}

wp_criar$`Artigo em português` <- apply_hiperlink(wp_criar$`Artigo em português`)

### Tratamento wd_criar

# Item no Wikidata

wd_criar$`Item no Wikidata`<- ifelse(
  is.na(wd_criar$`Item no Wikidata`),
  wd_link,
  wd_criar$`Item no Wikidata`
)

### Juntar dataframes

# Feita a formatação das tabelas, fazer a junção com a lista de verbetes/itens a editar

lista_ocupacao <- lista_ocupacao %>%
  full_join(wp_criar, by = c("Artigo em português", "Item no Wikidata"))

lista_ocupacao <- lista_ocupacao %>%
  full_join(wd_criar, by = c("Artigo em português", "Item no Wikidata"))

### Exportar

setwd("")
write_xlsx(lista_ocupacao, ".xlsx")