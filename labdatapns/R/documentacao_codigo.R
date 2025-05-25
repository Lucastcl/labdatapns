#' Exibir documentação de uma variável da PNS
#'
#' Dada uma variável (pelo código), exibe a pergunta correspondente e as categorias/respostas possíveis.
#'
#' @param codigo Código da variável (ex: "Q0301").
#' @param ano Opcional. Se fornecido, restringe a busca ao ano indicado.
#'
#' @return Um `list` com `pergunta` e `categorias`. Também imprime a documentação.
#'
#' @export
documentacao_codigo <- function(codigo, ano = NULL) {
  if (!exists("dicionario") || !exists("dicionario_v")) {
    stop("Os objetos `dicionario` e `dicionario_v` devem estar disponíveis no ambiente.")
  }

  info_var <- dicionario[dicionario$codigo == codigo, ]
  if (!is.null(ano)) {
    info_var <- info_var[info_var$Ano == ano, ]
  }

  if (nrow(info_var) == 0) {
    return(NULL)
  }

  texto_pergunta <- unique(info_var$pergunta)
  texto_pergunta <- texto_pergunta[!is.na(texto_pergunta)][1]  # Evita NA duplicado

  categorias_var <- dicionario_v[dicionario_v$`Código da variável` == codigo, ]
  if (!is.null(ano)) {
    categorias_var <- categorias_var[categorias_var$Ano == ano, ]
  }

  if (nrow(categorias_var) == 0) {
    return(list(pergunta = texto_pergunta, categorias = NULL))
  }

  resultado <- categorias_var[, c("Códigos das respostas", "Respostas Possíveis")]
  colnames(resultado) <- c("Código", "Resposta")

  return(list(pergunta = texto_pergunta, categorias = resultado))
}
