#' Tabela com totais e intervalos de confiança para variável da PNS
#'
#' Calcula os totais, coeficientes de variação e intervalos de confiança para a primeira variável indicada,
#' usando o desenho amostral da PNS 2019.
#'
#' @param variaveis Vetor com nomes das variáveis da PNS a serem carregadas. A primeira será usada na tabulação.
#'
#' @return Um data frame com colunas: Categoria, Total, Coeficiente_de_Variacao, Intervalo_inferior, Intervalo_superior.
#'
#' @import survey
#' @importFrom PNSIBGE get_pns
#' @importFrom survey svytotal svymean svyby cv svydesign
#' @importFrom stats reformulate confint
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_wider
#' @export
#'
tabela_totais <- function(variaveis = c()) {

  vars_base <- c("C006", "VDF002", "VDF003", "V0001", "V0026", "D001")
  vars <- c(variaveis, vars_base)

  dados_pns_design <- get_pns(year = 2019, labels = TRUE, vars = vars, design = TRUE)

  total_var1 <- svytotal(as.formula(paste("~", vars[1])), design = dados_pns_design, na.rm = TRUE)
  cv_total_var1 <- cv(object = total_var1)
  intervalo_total_var1 <- confint(total_var1)

  intervalo_total_var1 <- as.data.frame(intervalo_total_var1)
  rownames(intervalo_total_var1) <- gsub(vars[1], "", rownames(intervalo_total_var1))

  total_var1 <- as.data.frame(total_var1)
  rownames(total_var1) <- gsub(vars[1], "", rownames(total_var1))

  cv_total_var1 <- as.data.frame(cv_total_var1)
  rownames(cv_total_var1) <- gsub(vars[1], "", rownames(cv_total_var1))

  n <- length(total_var1)

  tabela_var1 <- data.frame(
    Categoria = rownames(total_var1),
    Total = c(total_var1[1:n, 1]),
    Coeficiente_de_Variacao = c(cv_total_var1[1:n, 1]),
    Intervalo_inferior = c(intervalo_total_var1[1:n, 1]),
    Intervalo_superior = c(intervalo_total_var1[1:n, 2])
  )

  return(tabela_var1)
}
