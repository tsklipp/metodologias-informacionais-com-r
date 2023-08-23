# Importando bibliotecas do tidyverse
library(tidyverse)

# Lendo dados da população brasileira por grupo de idade
# Fonte: PNAD Continua do IBGE
dt <- readr::read_csv("dados/populacao_brasil_pnadc.csv")

# Iniciaremos a limpeza dos dados usando dplyr::filter()
populacao_total <- dt |> dplyr::filter(sexo == 'Total' | grupo_idade == 'Total')
dt <- dt |>
  dplyr::filter(sexo != 'Total' & grupo_idade != 'Total')

# Converter as classes das colunas grupo_idade e sexo de 'character' para 'factor' com dplyr::mutate() e as.factor()
dt <- dt |>
  dplyr::mutate(sexo = as.factor(sexo), grupo_idade = as.factor(grupo_idade))

# Criando uma ordenação manual para os níveis do tipo 'factor' da coluna grupo_idade
faixa_etaria <- c("0 a 4 anos", "5 a 9 anos", "5 a 13 anos", "10 a 13 anos", "14 a 15 anos",
                  "14 a 17 anos", "16 a 17 anos",    "18 a 19 anos",    "20 a 24 anos",
                  "25 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos",
                  "60 a 64 anos", "60 anos ou mais", "65 anos ou mais")

# Aplicando a ordenação manual ao converter novamente a variável para o tipo 'factor', mas informando os níveis
dt <- dt |>
  dplyr::mutate(grupo_idade = factor(grupo_idade, levels = faixa_etaria))

# Salvar o novo data frame, agora com dados já no formato tidy, para posterior uso
readr::write_csv(dt, "dados/popupacao_brasil_pnadc_tidy.csv")

# Algumas operações sobre os dados
dt |>
  dplyr::filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
  dplyr::summarise(total = sum(populacao), .by = c(sexo, ano))

dt |>
  dplyr::filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
  dplyr::summarise(minimo = min(populacao), media = mean(populacao), maximo = max(populacao), desvio_padrao = sd(populacao), .by = c(grupo_idade))
