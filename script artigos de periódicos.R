### Limpar ambiente do RStudio

rm(list=ls())

### Carregar pacotes

library(dplyr)
library(readxl)
library(writexl)

### Definir caminho do arquivo

caminho_periodicos <- ".xlsx"

### Carregar arquivo

periodicos <- read_excel(caminho_periodicos)

### Tratamento dos dados

# Criar coluna "Item"

periodicos$`Item no Wikidata` <- "style=\"text-align: center;\"|{{Botão clicável 2|class=mw-ui-progressive|Criar|url=https://www.wikidata.org/wiki/Special:NewItem}}"

# Criar coluna "Instância de" com o valor de "artigo acadêmico" no Wikidata

periodicos$`Instância de` <- "Q13442814"

# Formatar coluna "URL"

URL_transform <- function(x) {
  x <- gsub("^", "style=\"text-align: center;\"|{{Botão clicável 2|class=mw-ui-destructive|PDF|url=", x)
  x <- gsub("$", "}}", x)
}

periodicos$URL <- if_else(
  is.na(periodicos$URL),
  periodicos$URL,
  URL_transform(periodicos$URL)
)

# Selecionar colunas

periodicos <- select(periodicos, Título, Autoria, `Item no Wikidata`, `Instância de`, Periódico, Volume, Número, URL)

# Renomear

names(periodicos) <- c("Título", 
                   "Autoria",
                   "Item no Wikidata", 
                   "Instância de",
                   "Periódico",
                   "Volume",
                   "Número", 
                   "Carregar no Commons")

# Garantir que o número esteja formatado como número inteiro

periodicos$Número <- as.integer(periodicos$Número)

# Incluir texto em formato css para centralizar valor na coluna

center <- "style=\"text-align: center;\"|"

periodicos$`Instância de` <- gsub("^", center, periodicos$`Instância de`)
periodicos$Periódico <- gsub("^", center, periodicos$Periódico)
periodicos$Volume <- gsub("^", center, periodicos$Volume)
periodicos$Número <- gsub("^", center, periodicos$Número)

### Exportar

setwd("")
write_xlsx(, ".xlsx")