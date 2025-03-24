#'###########################################################################
#'@project POF 2017-2018 in R
#'@responsible-script Thiago Cordeiro Almeida
#'@contact thiagocordalmeida_at_gmail.com
#'@date-update 2025-03-24
#'@description Data handling - adjusting dataframe at individual level for household level
#'@details
#' [PT]
#' ---
#' Variavel: Tipo de serviço domestico (ver arquivo '.docs/metodologias_definicoes e ajustes.ods')
#' Na tese de Guerra (2017), ha uma definicao mais restrita das ocupacoes associadas ao servico
#' domestico. Aqui, utilizou-se a definicao tambem adotada por Guerra (2017).
#' Todavia, melhorias podem ser tomadas na linha de expandir essa definicao, algo possivel na POF
#' (e tambem feito no paper 'Paid and Unpaid care').
#' ---
#'
#'###########################################################################
options(scipen = 99999)
rm(list = ls())

#STEPS #
# Filtrar ocupacoes igual a Fatinha optou por fazer (decisao metodologica a ser discutida, destacar isso no codigo)
# Criar variavel dependente tal qual feito no script anterior
# Seguir para variaveis independentes
