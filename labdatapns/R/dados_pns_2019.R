#' Carrega os dados da PNS 2019 com desenho amostral
#'
#' Esta função baixa e prepara os dados da PNS 2019 com as variáveis solicitadas
#' e o desenho amostral (survey design). Algumas variáveis obrigatórias para o design
#' são sempre incluídas automaticamente.
#'
#' @param vars Vetor de nomes de variáveis adicionais a serem carregadas.
#' @param global Se TRUE (padrão), os objetos são atribuídos no ambiente global.
#'
#' @return Atribui `dados_pns_design` e `vars` ao ambiente global (ou retorna em lista, se `global = FALSE`).
#' @export
dados_pns_2019 <- function(vars = c(), global = TRUE) {
  # Variáveis obrigatórias para o design
  vars_fixas <- c("C006", "VDF002", "VDF003", "V0001", "V0026", "D001")
  vars <- unique(c(vars, vars_fixas))

  # Obtém os dados com o survey design
  dados_pns_design <- get_pns(
    year = 2019,
    labels = TRUE,
    vars = vars,
    design = TRUE,
    selected=TRUE,
    anthropometry=TRUE
  )

  options(survey.lonely.psu = "adjust")

  if (global) {
    assign("dados_pns_design", dados_pns_design, envir = .GlobalEnv)
    assign("vars", vars, envir = .GlobalEnv)
    invisible(NULL)
  } else {
    return(list(dados_pns_design = dados_pns_design, vars = vars))
  }
}
