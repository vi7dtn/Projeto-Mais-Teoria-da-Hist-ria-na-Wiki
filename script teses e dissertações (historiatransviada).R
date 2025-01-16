### Limpar o ambiente do RSTudio

rm(list=ls())

### Carregar pacotes

library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(tools)

### Definir caminho do arquivo

caminho_ht <- ".xlsx"

### Carregar arquivo

ht <- read_excel(caminho_ht)

### Selecionar colunas pertinentes

ht <- ht %>% select(`AUTORA/AUTOR`, `TÍTULO DO TRABALHO`, `Pós-Graduação - Nível`, `Sigla IES`, `DATA DE DEFESA`, `PALAVRAS-CHAVES`, `LINK de acesso`)

### Renomear colunas

names(ht) <- c("Autoria",
               "Título do trabalho",
               "Nível",
               "Instituição",
               "Data de defesa",
               "Palavras-chave",
               "Link de acesso")

### Tratamento das células

# Função de transformação

transformar_nome <- function(nome) {
  # Divide o nome em duas partes: antes e depois da vírgula
  partes <- str_split(nome, ",\\s*", simplify = TRUE)
  
  # Capitaliza a parte após a vírgula
  parte_1 <- str_to_title(partes[1])
  
  # Cria o novo nome juntando a parte após a vírgula com a parte antes da vírgula
  nome_modificado <- paste(partes[2], parte_1)
  
  return(nome_modificado)
}

# Aplicar função

ht <- ht %>%
  mutate(Autoria = sapply(Autoria, transformar_nome))

# Formatar coluna "Link de acesso"

ht$`Link de acesso` <- gsub("^", "style=\"text-align: center;\"|{{Botão clicável 2|Link|class=mw-ui-destructive|url=", ht$`Link de acesso`) # Adaptar linhas
ht$`Link de acesso` <- gsub("$", " }}", ht$`Link de acesso`)

# Centralizar texto em certas colunas

ht$`Nível` <- gsub("^", "style=\"text-align: center;\"|", ht$`Nível`)
ht$`Instituição` <- gsub("^", "style=\"text-align: center;\"|", ht$`Instituição`)
ht$`Data de defesa` <- gsub("^", "style=\"text-align: center;\"|", ht$`Data de defesa`)

### Adicionar hiperlinks na coluna de palavras-chave

adicionar_colchetes <- function(texto) {
  # Divide a string usando a vírgula como separador
  partes <- strsplit(texto, ",\\s*")[[1]]
  
  # Adiciona os colchetes a cada parte
  partes_colchetes <- paste0("[[", partes, "]]")
  
  # Junta as partes de volta com vírgulas
  resultado <- paste(partes_colchetes, collapse = ", ")
  
  return(resultado)
}

# Padronizar

ht$`Palavras-chave` <- gsub(";", ",", ht$`Palavras-chave`)
ht$`Palavras-chave` <- gsub("\\.", ",", ht$`Palavras-chave`)

# Aplicar função

ht <- ht %>%
  mutate(`Palavras-chave` = sapply(`Palavras-chave`, adicionar_colchetes))

# Inserir coluna de criação de item

ht$`Item no Wikidata` <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar|class=mw-ui-progressive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

### Exportar

setwd("")
write_xlsx(ht, ".xlsx")