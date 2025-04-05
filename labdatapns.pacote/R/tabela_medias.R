#' Tabela de totais por UF e categoria para variável da PNS
#'
#' Calcula os totais, coeficientes de variação e intervalos de confiança por UF e categoria da primeira variável informada,
#' com base na PNS 2019.
#'
#' @param variaveis Vetor com nomes das variáveis da PNS a serem carregadas. A primeira será usada na tabulação por UF.
#'
#' @return Um data frame em formato wide com totais e medidas de erro por UF e categoria.
#'
#' @import survey
#' @importFrom PNSIBGE get_pns
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_wider
#' @export
tabela_totais_uf <- function(variaveis = c()) {

  vars_base <- c("C006", "VDF002", "VDF003", "V0001", "V0026", "D001")
  vars <- c(variaveis, vars_base)

  dados_pns_design <- get_pns(year = 2019, labels = TRUE, vars = vars, design = TRUE)

  total_var1_uf <- svytotal(reformulate(paste0("~interaction(V0001, ", vars[1], ")")), design = dados_pns_design, na.rm = TRUE)
  cv_total_var1_uf <- as.data.frame(cv(object = total_var1_uf))
  intervalo_total_var1_uf <- as.data.frame(confint(total_var1_uf))
  total_var1_uf <- as.data.frame(total_var1_uf)

  limpar_rownames_uf <- function(nome) {
    nome <- as.data.frame(nome)
    nomes_atuais <- rownames(nome)
    if (is.null(nomes_atuais)) stop("O objeto fornecido não tem rownames.")

    nomes_limpos <- sub(".*\\)", "", nomes_atuais)
    partes_lista <- strsplit(nomes_limpos, "\\.")

    partes_validas <- sapply(partes_lista, length) == 2
    if (!any(partes_validas)) stop("Nenhum nome de linha pôde ser dividido em duas partes por '.'")

    nome <- nome[partes_validas, , drop = FALSE]
    partes_lista <- partes_lista[partes_validas]
    partes <- do.call(rbind, partes_lista)
    nome$UF <- partes[, 1]
    nome$categoria <- partes[, 2]
    rownames(nome) <- NULL

    nome <- nome[nome$categoria != "Ignorado", , drop = FALSE]
    return(nome)
  }

  intervalo_total_var1_uf <- limpar_rownames_uf(intervalo_total_var1_uf)
  total_var1_uf <- limpar_rownames_uf(total_var1_uf)
  cv_total_var1_uf <- limpar_rownames_uf(cv_total_var1_uf)

  criar_tabela_resumo <- function(total, intervalo, cv) {
    colnames(total) <- c("total", "erro_padrao", "UF", "categoria")
    colnames(intervalo) <- c("limite_inferior", "limite_superior", "UF", "categoria")
    colnames(cv) <- c("cv", "UF", "categoria")

    base_completa <- total %>%
      left_join(intervalo, by = c("UF", "categoria")) %>%
      left_join(cv, by = c("UF", "categoria"))

    base_wide <- base_completa %>%
      pivot_wider(
        names_from = categoria,
        values_from = c(total, erro_padrao, limite_inferior, limite_superior, cv),
        names_glue = "{.value}_{categoria}"
      )

    return(base_wide)
  }

  tabela_total_var1_uf <- criar_tabela_resumo(total_var1_uf, intervalo_total_var1_uf, cv_total_var1_uf)

  return(tabela_total_var1_uf)
}
