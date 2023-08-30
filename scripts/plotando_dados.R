# Importando bibliotecas do tidyverse
library(tidyverse)

# Lendo dados já no formato tidy
dt <- readr::read_csv('dados/populacao_brasil_pnadc_tidy.csv')

# Criando gráficos dos dados referentes à população brasileira
dt |>
  ggplot2::ggplot(aes(x = ano, y = populacao)) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(~sexo)

dt |>
  dplyr::filter(sexo == "Mulheres") |>
  ggplot2::ggplot(aes(ano, populacao)) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(sexo ~ grupo_idade, nrow = 2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = element_text(angle = 60, hjust = .9), strip.text.x = element_text(margin = margin(.5,0,.5,0, "cm")))

# Criando uma ordenação manual para os níveis do tipo 'factor' da coluna grupo_idade
faixa_etaria_labelA <- c("0 a 4 anos", "5 a 9 anos", "5 a 13 anos", "10 a 13 anos", "14 a 15 anos",
                  "14 a 17 anos", "16 a 17 anos",    "18 a 19 anos",    "20 a 24 anos",
                  "25 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos",
                  "60 a 64 anos", "60 anos ou mais", "65 anos ou mais")

faxa_etaria_labelB <- c("0 a 4\n anos", "5 a 9\n anos", "5 a 13\n anos", "10 a 13\n anos",
                        "14 a 15 \n anos",    "14 a 17\n anos",    "16 a 17\n anos",
                        "18 a 19\n anos" ,   "20 a 24\n anos" ,   "25 a 29\n anos" ,
                        "30 a 39\n anos",    "40 a 49\n anos", "50 a 59\n anos"  ,
                        "60 a 64\n anos"  ,  "60 anos\n ou mais", "65 anos\n ou mais")

# Aplicando a ordenação manual ao converter novamente a variável para o tipo 'factor', mas informando os níveis
dt |>
  dplyr::mutate(grupo_idade = factor(grupo_idade, levels = faixa_etaria_labelA)) |>
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
