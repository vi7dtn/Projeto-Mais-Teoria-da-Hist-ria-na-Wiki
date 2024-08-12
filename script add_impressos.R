### Limpar o environment do RStudio

rm(list=ls())

### Carregar os pacotes

library(dplyr)
library(readxl)
library(writexl)

#### Literárias

### Definir o caminho em que estão no seu PC os arquivos

caminho_add_impressos <- ".xlsx"
caminho_literarias <- ".xlsx"

# Carregar os arquivos

add_impressos <- read_excel(caminho_add_impressos)
literarias <- read_excel(caminho_literarias)

### Selecionar no arquivo add_impressos aquilo que diz respeito a obras literárias

# Selecionar apenas as linhas que contém valores nas colunas de obras literárias

add_literarias <- add_impressos %>%
  filter(!(is.na(`obra literária I`) & is.na(`obra literária II`)))

# Duplicar linhas que a autoria possua mais de uma obra 

novas_linhas <- add_literarias %>%
  filter(!is.na(`obra literária II`)) %>%
  select(item, article, nome, `obra literária II`) %>% 
  mutate(`obra literária I` = `obra literária II`) %>% 
  select(item, article, nome, `obra literária I`)

# Juntar a tabela com as linhas da coluna I e da coluna II

add_literarias__final <- bind_rows(add_literarias, novas_linhas) %>%
  arrange(item) %>% 
  select(`obra literária I`, nome)

# Renomear

names(add_literarias__final) <- c("Artigo em português",
                                  "Autoria")

### Juntar na tabela de impressos existentes as linhas de impressos a criar

literarias_final <- bind_rows(literarias, add_literarias__final) %>% 
  arrange(Autoria)

### Tratatamento da coluna Item

new_item <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar!|class=mw-ui-progressive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

literarias_final$`Item no Wikidata` <- if_else(
  is.na(literarias_final$`Item no Wikidata`),
  new_item,
  literarias_final$`Item no Wikidata`
)

### Exportar
setwd("")
write_xlsx(literarias_final, ".xlsx")

### Não Ficção

### Limpar o environment do RStudio

rm(list=ls())

### Definir o caminho em que estão no seu PC os arquivos

caminho_add_impressos <- ".xlsx"
caminho_nao_ficcao <- ".xlsx"

# Carregar os arquivos

add_impressos <- read_excel(caminho_add_impressos)
nao_ficcao <- read_excel(caminho_nao_ficcao)

### Selecionar no arquivo add_impressos aquilo que diz respeito a obras de não-ficção

# Selecionar apenas as linhas que contém valores nas colunas de obras de não-ficção

add_nao_ficcao <- add_impressos %>%
  filter(!(is.na(`obra de não-ficção I`) & is.na(`obra de não-ficção II`)))

# Duplicar linhas que a autoria possua mais de uma obra 

novas_linhas <- add_nao_ficcao %>%
  filter(!is.na(`obra de não-ficção II`)) %>%
  select(item, article, nome, `obra de não-ficção II`) %>% 
  mutate(`obra de não-ficção I` = `obra de não-ficção II`) %>% 
  select(item, article, nome, `obra de não-ficção I`)

# Juntar a tabela com as linhas da coluna I e da coluna II

add_nao_ficcao__final <- bind_rows(add_nao_ficcao, novas_linhas) %>%
  arrange(item) %>% 
  select(`obra de não-ficção I`, nome)

# Renomear

names(add_nao_ficcao__final) <- c("Artigo em português",
                                  "Autoria")

### Juntar na tabela de impressos existentes as linhas de impressos a criar

nao_ficcao_final <- bind_rows(nao_ficcao, add_nao_ficcao__final) %>% 
  arrange(Autoria)

### Tratamento da coluna Item

new_item <- "style=\"text-align: center;\"|{{Botão clicável 2|Criar!|class=mw-ui-progressive center|url=https://www.wikidata.org/wiki/Special:NewItem}}"

nao_ficcao_final$`Item no Wikidata` <- if_else(
  is.na(nao_ficcao_final$`Item no Wikidata`),
  new_item,
  nao_ficcao_final$`Item no Wikidata`
)

### Exportar

setwd("")
write_xlsx(nao_ficcao_final, ".xlsx")
