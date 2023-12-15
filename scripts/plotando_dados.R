## -------------------------------------------------------------------------------------------------------------------------
# Importando bibliotecas
library(tidyverse)
library(plotly)
library(scales)
library(ggsci)


## -------------------------------------------------------------------------------------------------------------------------
dt <- readr::read_csv('dados/populacao_brasil_pnadc_tidy.csv')


## -------------------------------------------------------------------------------------------------------------------------
p1_a <- dt |>
        filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |> # os grupos filtrados sobrepoem os dados
        summarise(total = sum(populacao), .by = c(sexo, ano)) |>
        ggplot(aes(x = ano, y = total, fill = sexo)) +
        geom_col() +
        facet_wrap(~sexo)	# painelizar os dados pela variável 'sexo'
p1_b <- dt |>
  filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
  summarise(total = sum(populacao), .by = c(sexo, ano)) |>
  ggplot(aes(x = ano, y = total / 10^6, fill = sexo)) +
  geom_col() +
  facet_wrap(~sexo)
p1_a


## -------------------------------------------------------------------------------------------------------------------------
p1_a + scale_x_continuous(breaks = unique(dt$ano)) + theme_classic()


## -------------------------------------------------------------------------------------------------------------------------
p1 <- p1_a + ylab("População Brasileira") + theme(axis.title.x = element_blank())


## -------------------------------------------------------------------------------------------------------------------------
ggplotly(p1) # usando função de converção do plotly


-------------------------------------------------------------------------------------------------------------------------

dt |>
  filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
  summarise(total = sum(populacao), .by = c(sexo, ano)) |>
  plot_ly(x = ~ano,  
          y = ~total,
          color = ~sexo,
          type = 'bar') |> # usando função do plotly
  layout(yaxis = list(title = "População Brasileira"),
         xaxis = list(title = ""))


## -------------------------------------------------------------------------------------------------------------------------
dt |>
  filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
  filter(sexo == "Mulheres") |>
  #filter(sexo == "Homens") |>
  ggplot(aes(ano, populacao)) +
  geom_col() +
  facet_wrap(sexo ~ grupo_idade, nrow = 2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = .9),
        strip.text.x = element_text(margin = margin(.4,.2,.4,.2, "cm")))


## -------------------------------------------------------------------------------------------------------------------------
ggplotly(p2)


## -------------------------------------------------------------------------------------------------------------------------
dt |>
    filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
    mutate(grupo_idade = factor(grupo_idade, levels = faixa_etaria_labels)) |>          # alternativa 1
    #mutate(grupo_idade = fct_reorder(factor(grupo_idade), populacao)) |>               # alternativa 2
    group_by(sexo, grupo_idade) |>
    plot_ly(x = ~ano, 
            y = ~populacao,
            color = ~sexo,
            split = ~grupo_idade,
            type = "bar") |>
    layout(yaxis = list(title = "População Brasileira"),
           xaxis = list(title = ""))

## -------------------------------------------------------------------------------------------------------------------------
dt <- readr::read_csv('dados/taxa_desocupacao_idade_2012-2013_tidy.csv')


## -------------------------------------------------------------------------------------------------------------------------
p3 <- dt |>
  mutate(ano = as.numeric(str_sub(trimestre, nchar(trimestre) -3, nchar(trimestre)))) |>
  mutate(trimestre = fct_reorder(factor(trimestre), ano)) |>
  ggplot(aes(x = trimestre, y = taxa_desocupacao, group = faixa_etaria, color = faixa_etaria)) +
  geom_line() +
  ylab("Taxa de Desocupação \n População Economicamente Ativa (%)") +
  scale_color_uchicago() +
  theme_classic() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), axis.title.x = element_blank())
p3


## -------------------------------------------------------------------------------------------------------------------------
ggplotly(p3)


## -------------------------------------------------------------------------------------------------------------------------
dt |>
  mutate(ano = as.numeric(str_sub(trimestre, nchar(trimestre) -3, nchar(trimestre)))) |>
  mutate(trimestre = fct_reorder(factor(trimestre), ano)) |>
  plot_ly(x = ~trimestre, y = ~taxa_desocupacao, color = ~faixa_etaria, type = "scatter", mode = "lines+markers") |>
  layout(
    yaxis = list(title = 'Taxa de Desocupação \n População Economicamente Ativa (%)"'),
    xaxis = list(title = '')
  )


## -------------------------------------------------------------------------------------------------------------------------
dt |>
  mutate(ano = as.numeric(str_sub(trimestre, nchar(trimestre) -3, nchar(trimestre)))) |>
  summarise(taxa_desocupacao = mean(taxa_desocupacao), .by = c(ano, faixa_etaria)) |>
  plot_ly(x = ~ano,
          y = ~taxa_desocupacao,
          color = ~faixa_etaria,
          type = "scatter",
          mode = "lines+markers") |>
  layout(yaxis = list(title = "Taxa de Desocupação \n População Economicamente Ativa (%)"),
         xaxis = list(title = ""))

