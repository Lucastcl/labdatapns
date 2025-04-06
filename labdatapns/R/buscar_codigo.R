#' Buscar variáveis no dicionário da PNS
#'
#' Filtra o dicionário da PNS procurando por uma palavra-chave no texto da pergunta.
#'
#' @param palavra Palavra ou expressão a ser buscada nas perguntas.
#' @param ignore_case Lógico. Se `TRUE`, a busca ignora maiúsculas e minúsculas.
#'
#' @return Um data frame com as colunas `codigo`, `pergunta` e `Ano` das variáveis que correspondem à busca.
#'
#' @details O objeto `dicionario` deve estar disponível no ambiente e conter as colunas `codigo`, `pergunta` e `Ano`.
#'
#' @export
buscar_codigo <- function(palavra, ignore_case = TRUE) {
  # Verifica se as colunas necessárias existem
  if (!all(c("codigo", "pergunta", "Ano") %in% names(dicionario))) {
    stop("O dicionário precisa ter as colunas: codigo, pergunta e Ano.")
  }

  # Filtra as linhas que contêm a palavra na pergunta
  resultados <- dicionario[grepl(palavra, dicionario$pergunta, ignore.case = ignore_case), ]

  # Retorna apenas as colunas principais
  return(resultados[, c("codigo", "pergunta", "Ano")])
}
