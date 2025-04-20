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
documentacao_variavel <- function(codigo, ano = NULL) {
  # Verificações
  if (!exists("dicionario") || !exists("dicionario_v")) {
    stop("Os objetos `dicionario` e `dicionario_v` devem estar disponíveis no ambiente.")
  }

  # Filtra a pergunta
  info_var <- dicionario[dicionario$codigo == codigo, ]
  if (!is.null(ano)) {
    info_var <- info_var[info_var$Ano == ano, ]
  }

  if (nrow(info_var) == 0) {
    stop("Código não encontrado no dicionário para o ano especificado.")
  }

  texto_pergunta <- unique(info_var$pergunta)
  cat("Pergunta:\n", texto_pergunta, "\n\n")

  # Filtra categorias
  categorias_var <- dicionario_v[dicionario_v$`Código da variável` == codigo, ]
  if (!is.null(ano)) {
    categorias_var <- categorias_var[categorias_var$Ano == ano, ]
  }

  if (nrow(categorias_var) == 0) {
    cat("Sem categorias registradas para essa variável.\n")
    return(invisible(list(pergunta = texto_pergunta, categorias = NULL)))
  }

  resultado <- categorias_var[, c("Códigos das respostas", "Respostas Possíveis")]
  colnames(resultado) <- c("Código", "Resposta")

  print(resultado, row.names = FALSE)

  return(invisible(list(pergunta = texto_pergunta, categorias = resultado)))
}
