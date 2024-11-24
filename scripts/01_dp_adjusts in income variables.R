#'###########################################################################
#'@project POF 2017-2018 in R
#'@responsible-script Thiago Cordeiro Almeida
#'@contact thiagocordalmeida_at_gmail.com
#'@date-update 2024-11-24
#'@description Creating auxiliar variables to deal with earning income and consumption
#'###########################################################################
options(scipen = 99999)
rm(list = ls())

# Bibliotecas -------------------------------------------------------------
library(pacman)
p_load(tidyverse, arrow)

# Definicao de objetos de importacao e exportacao -------------------------

INPUT_DIR <- "./input/raw"
OUTPUT_DIR <- "./input/raw"

# Importacao e manipulacao dos dados --------------------------------------

##  Leitura - RENDIMENTO DO TRABALHO

rendimento_trabalho <- read_parquet(file.path(INPUT_DIR,"RENDIMENTO_TRABALHO.parquet"))

# Tratamento (deflacionamento, anualizacao e mensalizacao)
rendimento_trabalho <- rendimento_trabalho %>%
  mutate(valor_mensal = (V8500_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
         prev_pub_mensal = (V531112_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
         imp_renda_mensal = (V531122_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
         iss_mensal = (V531132_DEFLA*V9011*FATOR_ANUALIZACAO)/12)


# Leitura  - OUTROS RENDIMENTOS

outros_rendimentos <- read_parquet(file.path(INPUT_DIR, "OUTROS_RENDIMENTOS.parquet"))

# Tratamento (deflacionamento, anualizacao e mensalizacao)

outros_rendimentos <- outros_rendimentos %>%
  mutate(valor_mensal = case_when(
    QUADRO==54 ~ (V8500_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
    TRUE ~ (V8500_DEFLA*FATOR_ANUALIZACAO)/12
  ),
  deducao_mensal = case_when(
    QUADRO==54 ~ (V8501_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
    TRUE ~ (V8501_DEFLA*FATOR_ANUALIZACAO)/12
  ))

# Leitura - MORADOR

MORADOR <- read_parquet(file.path(INPUT_DIR,"MORADOR.parquet"))

#  Leitura - ALUGUEL ESTIMADO

aluguel_estimado <- read_parquet(file.path(INPUT_DIR,"ALUGUEL_ESTIMADO.parquet"))

# Tratamento (deflacionamento, anualizacao e mensalizacao)

aluguel_estimado <- aluguel_estimado %>%
  mutate(valor_mensal = (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12)

# Leitura - DESPESA COLETIVA

despesa_coletiva <- read_parquet(file.path(INPUT_DIR,"DESPESA_COLETIVA.parquet"))

# Tratamento (deflacionamento, anualizacao e mensalizacao)

despesa_coletiva <- despesa_coletiva %>%
  mutate(valor_mensal = case_when(
    QUADRO==10 | QUADRO==19 ~ (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
    TRUE ~ (V8000_DEFLA*FATOR_ANUALIZACAO)/12
  ),
  inss_mensal = (V1904_DEFLA*V9011*FATOR_ANUALIZACAO)/12)

#  Leitura - CADERNETA COLETIVA (Questionario POF 3)

caderneta_coletiva <- read_parquet(file.path(INPUT_DIR, "CADERNETA_COLETIVA.parquet"))

# Tratamento (deflacionamento, anualizacao e mensalizacao)

caderneta_coletiva <- caderneta_coletiva %>%
  mutate(valor_mensal = (V8000_DEFLA*FATOR_ANUALIZACAO)/12)

# Leitura - DESPESA INDIVIDUAL

desp_individual <- read_parquet(file.path(INPUT_DIR, "DESPESA_INDIVIDUAL.parquet"))

# Tratamento (deflacionamento, anualizacao e mensalizacao)

desp_individual <- desp_individual %>%
  mutate(valor_mensal = case_when(
    QUADRO %in% c(44,47,48,49,50) ~ (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
    TRUE ~ (V8000_DEFLA*FATOR_ANUALIZACAO)/12))

# Exportacao das bases ----------------------------------------------------

write_parquet(rendimento_trabalho, file.path(OUTPUT_DIR,"rendimento_trabalho.parquet"))
write_parquet(outros_rendimentos,file.path(OUTPUT_DIR,"outros_rendimentos.parquet"))
write_parquet(MORADOR, file.path(OUTPUT_DIR,"MORADOR.parquet"))
write_parquet(aluguel_estimado, file.path(OUTPUT_DIR,"aluguel_estimado.parquet"))
write_parquet(despesa_coletiva, file.path(OUTPUT_DIR,"despesa_coletiva.parquet"))
write_parquet(caderneta_coletiva, file.path(OUTPUT_DIR,"caderneta_coletiva.parquet"))
write_parquet(desp_individual, file.path(OUTPUT_DIR,"desp_individual.parquet"))
