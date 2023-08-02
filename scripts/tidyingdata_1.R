# Importando bibliotecas do tidyverse
library(tidyverse)

# Lendo dados da população brasileira por grupo de idade
# Fonte: PNAD Continua do IBGE
dt <- readr::read_csv("slides/files/popupacao_brasil_pnadc.csv")

# Iniciaremos a limpeza dos dados usando dplyr::filter()
populacao_total <- dt |> dplyr::filter(sexo == 'Total' | grupo_idade == 'Total')
dt <- dt |>
  dplyr::filter(sexo != 'Total' & grupo_idade != 'Total')

# Converter a classe da colunas grupo_idade e sexo de 'character' para 'factor' com dplyr::mutate() e as.factor()
dt <- dt |>
  dplyr::mutate(sexo = as.factor(sexo), grupo_idade = as.factor(grupo_idade))

# Salvar o novo data frame, agora com dados já no formato tidy, para posterior uso
readr::write_csv(dt, "slides/files/popupacao_brasil_pnadc_tidy.csv")

# Algumas operações sobre os dados
dt |>
  dplyr::filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
  dplyr::summarise(total = sum(populacao), .by = c(sexo, ano))

dt |>
  ggplot2::ggplot(aes(x = ano, y = populacao, color = sexo)) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(~sexo)
