#' Buscar variáveis no dicionário da PNS
#'
#' Filtra o dicionário da PNS procurando por uma palavra-chave no texto da pergunta.
#'
#' @param palavra Palavra ou expressão regular a ser buscada nas perguntas.
#' @param ignore_case Lógico. Se `TRUE`, a busca ignora maiúsculas e minúsculas.
#' @param ano Opcional. Pode ser 2013, 2019 ou `NULL` para ambos.
#'
#' @return Um data frame com as colunas `codigo`, `pergunta` e `Ano` das variáveis que correspondem à busca.
#'
#' @export
buscar_codigo <- function(palavra, ignore_case = TRUE, ano = NULL) {
  # Verificações
  if (!exists("dicionario")) stop("O objeto `dicionario` não foi encontrado no ambiente.")
  if (!all(c("codigo", "pergunta", "Ano") %in% names(dicionario))) {
    stop("O dicionário precisa ter as colunas: codigo, pergunta e Ano.")
  }

  # Filtragem por ano (se fornecido)
  dados <- dicionario
  if (!is.null(ano)) {
    dados <- subset(dados, Ano %in% ano)
  }

  # Busca pelas palavras na pergunta
  resultados <- dados[grepl(palavra, dados$pergunta, ignore.case = ignore_case), ]

  if (nrow(resultados) == 0) {
    message("Nenhuma variável encontrada para a palavra-chave fornecida.")
    return(invisible(NULL))
  }

  return(resultados[order(resultados$Ano, resultados$codigo), c("codigo", "pergunta", "Ano")])
}
