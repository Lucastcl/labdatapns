#' Calcula médias por UF da variável de interesse
#'
#' @param variaveis Vetor com nome(s) da(s) variável(is) de interesse.
#'
#' @return Data frame com médias, erro padrão, intervalo de confiança e coeficiente de variação por categoria e UF.
#' @export
#'@importFrom PNSIBGE get_pns
#' @importFrom survey svytotal svymean svyby cv svydesign
#' @importFrom stats reformulate confint
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_wider
#' @examples
#' tabela_medias_uf(variaveis = c("P040"))
tabela_medias_uf <- function(variaveis = c()) {

  vars_base <- c("C006", "VDF002", "VDF003", "V0001", "V0026", "D001")
  vars <- c(variaveis, vars_base)

  dados_pns_design <- get_pns(year = 2019, labels = TRUE, vars = vars, design = TRUE)

  medias_uf <- svyby(
    formula = reformulate(vars[1]),
    by = ~V0001,
    design = dados_pns_design,
    FUN = svymean,
    na.rm = TRUE
  )

  intervalo_uf <- confint(medias_uf)
  cv_uf <- cv(object = medias_uf)

  medias_uf <- as.data.frame(medias_uf)
  intervalo_uf <- as.data.frame(intervalo_uf)
  cv_uf <- as.data.frame(cv_uf)

  limpar_rownames_uf <- function(df) {
    nomes <- rownames(df)
    nomes_limpos <- sub(".*\\)", "", nomes)
    partes <- strsplit(nomes_limpos, "\\.")
    partes_validas <- sapply(partes, length) == 2

    df <- df[partes_validas, , drop = FALSE]
    partes <- do.call(rbind, partes[partes_validas])
    df$UF <- partes[, 1]
    df$categoria <- partes[, 2]
    rownames(df) <- NULL

    df[df$categoria != "Ignorado", , drop = FALSE]
  }

  medias_uf <- limpar_rownames_uf(medias_uf)
  intervalo_uf <- limpar_rownames_uf(intervalo_uf)
  cv_uf <- limpar_rownames_uf(cv_uf)

  colnames(medias_uf)[1:2] <- c("media", "erro_padrao")
  colnames(intervalo_uf)[1:2] <- c("limite_inferior", "limite_superior")
  colnames(cv_uf)[1] <- "cv"

  base_completa <- medias_uf %>%
    left_join(intervalo_uf, by = c("UF", "categoria")) %>%
    left_join(cv_uf, by = c("UF", "categoria"))

  base_wide <- base_completa %>%
    pivot_wider(
      names_from = categoria,
      values_from = c(media, erro_padrao, limite_inferior, limite_superior, cv),
      names_glue = "{.value}_{categoria}"
    )

  return(base_wide)
}
