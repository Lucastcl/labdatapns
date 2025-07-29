#' Buscar variáveis no dicionário da PNS de forma inteligente
#'
#' Filtra o dicionário da PNS procurando por um código de variável ou por uma
#' palavra-chave no texto da pergunta. A busca por palavra-chave ignora
#' acentos e maiúsculas/minúsculas.
#'
#' @param termo O termo a ser buscado. Pode ser um código de variável (ex: "C00101")
#'   ou uma palavra-chave (ex: "alimentacao").
#' @param ano Opcional. Pode ser 2013, 2019 ou `NULL` para ambos.
#'
#' @return Um data frame com as colunas `codigo`, `pergunta` e `Ano` das
#'   variáveis que correspondem à busca. Retorna um data frame vazio se nada for encontrado.
#'
#' @export
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_to_upper str_detect
buscar_codigo <- function(termo, ano = NULL) {
  # --- 1. Validação do Dicionário ---
  if (!exists("dicionario")) {
    stop("Objeto de dados 'dicionario' não encontrado. Ele deve ser carregado ou exportado pelo pacote.")
  }
  if (!all(c("codigo", "pergunta", "Ano") %in% names(dicionario))) {
    stop("O dicionário precisa ter as colunas: codigo, pergunta e Ano.")
  }

  # --- 2. Preparação dos Dados ---
  dados_filtrados <- dicionario
  if (!is.null(ano)) {
    dados_filtrados <- subset(dados_filtrados, Ano %in% ano)
  }

  termo_limpo <- trimws(termo)
  resultados <- data.frame()

  # --- 3. Lógica de Busca Inteligente ---
  if (nchar(termo_limpo) > 0) {
    if (grepl("^[A-Z][0-9]{5}$", termo_limpo, ignore.case = TRUE)) {
      resultados <- dados_filtrados[stringr::str_to_upper(dados_filtrados$codigo) == stringr::str_to_upper(termo_limpo), ]
    } else {
      termo_norm <- stringi::stri_trans_general(tolower(termo_limpo), "Latin-ASCII")
      pergunta_norm <- stringi::stri_trans_general(tolower(dados_filtrados$pergunta), "Latin-ASCII")
      indices <- stringr::str_detect(pergunta_norm, termo_norm)

      resultados <- dados_filtrados[indices, ]
    }
  }

  # --- 4. Retorno dos Resultados ---
  if (nrow(resultados) == 0) {
    message("Nenhuma variável encontrada para o termo fornecido.")
    return(data.frame())
  }

  return(resultados[order(resultados$Ano, resultados$codigo), c("codigo", "pergunta", "Ano")])
}
