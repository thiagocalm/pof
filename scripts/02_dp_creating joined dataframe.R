#'###########################################################################
#'@project POF 2017-2018 in R
#'@responsible-script Thiago Cordeiro Almeida
#'@contact thiagocordalmeida_at_gmail.com
#'@date-update 2024-11-24
#'@description Data processing - creating dataframe with UC variables at individual level
#'###########################################################################
options(scipen = 99999)
rm(list = ls())

# Bibliotecas -------------------------------------------------------------
library(pacman)
p_load(tidyverse, arrow)

# Definicao de objetos de importacao e exportacao -------------------------

INPUT_DIR <- "./input/raw"
OUTPUT_DIR <- "./input"

# Base: despesas individuais --------------------------------------

# Importacao dos dados

desp_individual <- read_parquet(file = file.path(INPUT_DIR,"desp_individual.parquet"))

# Agregar as despesas para a unidade de analise das UCs

desp_individual <- desp_individual |>
  group_by(COD_UPA, NUM_DOM, NUM_UC) |>
  reframe(
    valor_mensal_desp_individual = sum(valor_mensal)
  )

# Exportacao dos dados agregados por UC

write_parquet(desp_individual, file.path(OUTPUT_DIR,"raw","desp_individual agregada UC.parquet"))
rm(desp_individual)

# Base: despesas coletivas --------------------------------------

# Importacao dos dados

desp_coletiva <- read_parquet(file = file.path(INPUT_DIR,"despesa_coletiva.parquet"))

# Agregar as despesas para a unidade de analise das UCs

desp_coletiva <- desp_coletiva |>
  group_by(COD_UPA, NUM_DOM, NUM_UC) |>
  reframe(
    valor_mensal_desp_coletiva = sum(valor_mensal)
  )

# Exportacao dos dados agregados por UC

write_parquet(desp_coletiva, file.path(OUTPUT_DIR,"raw","desp_coletiva agregada UC.parquet"))
rm(desp_coletiva)

# Base: Aluguel estimado --------------------------------------

# Importacao dos dados

aluguel_estimado <- read_parquet(file = file.path(INPUT_DIR,"aluguel_estimado.parquet"))

# Agregar as despesas para a unidade de analise das UCs

aluguel_estimado <- aluguel_estimado |>
  select(COD_UPA, NUM_DOM, NUM_UC, valor_mensal_aluguel = valor_mensal) |>
  arrange(COD_UPA, NUM_DOM, NUM_UC)

# Exportacao dos dados agregados por UC

write_parquet(aluguel_estimado, file.path(OUTPUT_DIR,"raw","aluguel_estimado por UC.parquet"))
rm(aluguel_estimado)

# Base: Despesa com servico domestico --------------------------------------

# Importacao dos dados

desp_coletiva_servico_domestico <- read_parquet(file = file.path(INPUT_DIR,"despesa_coletiva.parquet"))

# Selecao de casos para despesas associadas ao servico domestico e cuidado

desp_coletiva_servico_domestico <- desp_coletiva_servico_domestico |>
  mutate(
    servico_domestico = case_when(
      QUADRO == 19 ~ 1,
      TRUE ~ 0),
    despesa_servico_domestico = case_when(
      servico_domestico == 1 ~ valor_mensal,
      TRUE ~ 0),
    cuidado = case_when(
      V9001 %in% c(1901201,1901202,1901203,1901204,1900701) ~ 1,
      TRUE ~ 0)
    ) |>
  filter(servico_domestico == 1) |>
  select(COD_UPA, NUM_DOM, NUM_UC, servico_domestico, cuidado, despesa_servico_domestico) |>
  group_by(COD_UPA, NUM_DOM, NUM_UC) |>
  reframe(despesa_servico_domestico = sum(despesa_servico_domestico)) |>
  arrange(COD_UPA, NUM_DOM, NUM_UC)

# Exportacao dos dados agregados por UC

write_parquet(desp_coletiva_servico_domestico, file.path(OUTPUT_DIR,"raw","despesa_coletiva com servico domestico.parquet"))
rm(desp_coletiva_servico_domestico)

# Juntar as bases ---------------------------------------------------------

# Importacao de todas as bases

moradores <- read_parquet(file = file.path(INPUT_DIR,"MORADOR.parquet"))
desp_individual_uc <- read_parquet(file = file.path(INPUT_DIR,"desp_individual agregada UC.parquet"))
desp_coletiva_uc <- read_parquet(file = file.path(INPUT_DIR,"desp_coletiva agregada UC.parquet"))
aluguel_estimado_uc <- read_parquet(file = file.path(INPUT_DIR,"aluguel_estimado por UC.parquet"))
desp_serv_domestico <- read_parquet(file = file.path(INPUT_DIR,"despesa_coletiva com servico domestico.parquet"))

# Juncao dos dados --------------------------------------------------------

# 1 - juntando com despesas individuais
moradores_juncao1 <- moradores |>
  arrange(COD_UPA, NUM_DOM, NUM_UC) |>
  left_join(desp_individual_uc, by = c("COD_UPA","NUM_DOM","NUM_UC"), keep = FALSE,
            # multiple = "first"
  )

# 2 - juntando com despesas coletivas
moradores_juncao2 <- moradores_juncao1 |>
  arrange(COD_UPA, NUM_DOM, NUM_UC) |>
  left_join(desp_coletiva_uc, by = c("COD_UPA","NUM_DOM","NUM_UC"), keep = FALSE,
            # multiple = "first"
  )

# 3 - juntando com aluguel estimado
moradores_juncao3 <- moradores_juncao2 |>
  arrange(COD_UPA, NUM_DOM, NUM_UC) |>
  left_join(aluguel_estimado_uc, by = c("COD_UPA","NUM_DOM","NUM_UC"), keep = FALSE,
            # multiple = "first"
  )

# 4 - juntando com aluguel estimado
moradores_juncao4 <- moradores_juncao3 |>
  arrange(COD_UPA, NUM_DOM, NUM_UC) |>
  left_join(desp_serv_domestico, by = c("COD_UPA","NUM_DOM","NUM_UC"), keep = FALSE,
            multiple = "first"
  )

# Tratamento dos dados depois da juncao

moradores_agregado <- moradores_juncao4 |>
  select(1:56,valor_mensal_desp_individual,valor_mensal_desp_coletiva,valor_mensal_aluguel,
         despesa_servico_domestico) |>
  as_tibble()

# removendo bases que nao serao utilizadas mais
rm(aluguel_estimado_uc, desp_coletiva_uc, desp_individual_uc, desp_serv_domestico,
   moradores, moradores_juncao1,moradores_juncao2,moradores_juncao3,moradores_juncao4)

# exportacao da base de moradores com variaveis agregadas para domicilio
write_parquet(moradores_agregado, file.path(OUTPUT_DIR,",moradores_agregado.parquet"))
