# Importando bibliotecas do tidyverse
library(tidyverse)

# Lendo dados já no formato tidy
dt <- readr::read_csv('dados/populacao_brasil_pnadc_tidy.csv')

# Criando gráficos dos dados referentes à população brasileira
dt |>
  ggplot2::ggplot(aes(x = ano, y = populacao, color = sexo)) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(~sexo)

dt |>
  dplyr::filter(sexo == "Mulheres") |>
  ggplot2::ggplot(aes(ano, populacao)) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(sexo ~ grupo_idade, nrow = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = element_text(angle = 60, hjust = .9))


dt |>
  ggplot2::ggplot(aes(x = ano, y = populacao)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(sexo ~ grupo_idade) +
  ylab("População Brasileira") +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 65, vjust = .5))

# Lendo dados de taxa de desocupação já no formato tidy
dt <- readr::read_csv('dados/taxa_desocupacao_idade_2012-2013_tidy.csv')

dt |>
  dplyr::arrange(trimestre) |>
  ggplot2::ggplot(aes(x = trimestre, y = taxa_desocupacao, group = faixa_etaria, color = faixa_etaria)) +
  ggplot2::geom_line() +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = element_text(angle = 90))
