#' Carrega dados da PNS 2019 no ambiente global
#'
#' Esta função carrega os dados da PNS 2019 com as variáveis solicitadas
#' e algumas variáveis fixas obrigatórias para o design da amostra.
#'
#' Os objetos `dados_pns_design` e `vars` são salvos no ambiente global.
#'
#' @param vars Vetor de nomes de variáveis adicionais a serem incluídas.
#'
#' @return Nenhum valor é retornado. Objetos são salvos no ambiente global.
#' @export
dados_pns_2019 <- function(vars = c()) {
  vars <- c(vars, "C006", "VDF002", "VDF003", "V0001", "V0026", "D001")
  assign(
    "dados_pns_design",
    dados_pns_design <- get_pns(
      year = 2019,
      labels = TRUE,
      vars = vars,
      design = TRUE
    ),
    envir = .GlobalEnv
  )
  assign("vars", vars, envir = .GlobalEnv)
}