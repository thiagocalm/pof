#'@project POF 2017-2018 in R
#'@responsible-script Thiago Cordeiro Almeida
#'@contact thiagocordalmeida_at_gmail.com
#'@date-update 2024-08-28
#'@description Reading POF and exporting data in parquet format

rm(list = ls())
invisible(gc())

# Packages ----------------------------------------------------------------

library(pacman)
p_load(tidyverse, arrow, glue)
source("./scripts/X_function_import_export.R")


# Parameters --------------------------------------------------------------

ROOT_data <- "../pof 17-18 - material bruto/Dados_20230713"
ROOT_export <- "./input/raw"
dfs <- c(
  "DOMICILIO","MORADOR","MORADOR_QUALI_VIDA","ALUGUEL_ESTIMADO","DESPESA_COLETIVA",
  "SERVICO_NAO_MONETARIO_POF2","INVENTARIO","CADERNETA_COLETIVA","DESPESA_INDIVIDUAL",
  "SERVICO_NAO_MONETARIO_POF4","RESTRICAO_PRODUTOS_SERVICOS_SAUDE","RENDIMENTO_TRABALHO",
  "OUTROS_RENDIMENTOS","CONDICOES_VIDA","CARACTERISTICAS_DIETA","CONSUMO_ALIMENTAR"
)

# Importing and exporting -------------------------------------------------

for(i in seq_along(dfs)){
  df <- dfs[i]
  # import and export process
  func_import_export(x = df)
  # Next loop
  invisible(gc())
  print(glue::glue("Finalizamos a importacao e exportacao para {df} ({i/length(dfs)*100}% concluído...)"))
}
