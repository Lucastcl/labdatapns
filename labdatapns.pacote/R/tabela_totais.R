#' Calcula totais por variável, com ou sem domínio
#'
#' Esta função calcula totais, intervalo de confiança e coeficiente de variação
#' para uma variável específica, com possibilidade de filtro e desagregação por domínio.
#'
#' @param variaveis Vetor de variáveis (apenas a primeira será usada).
#' @param filtro Expressão lógica para filtrar os dados (ex: idade >= 18).
#' @param dominio Define o domínio para desagregação: "nenhum", "UF" ou "V0026".
#'
#' @return Um data.frame com totais, intervalos de confiança e coeficientes de variação.
#' @importFrom survey svytotal confint cv
#' @importFrom dplyr left_join
#' @importFrom rlang enquo quo_is_null eval_tidy
#' @importFrom stats reformulate as.formula
#' @export
tabela_totais <- function(variaveis = c(), filtro = NULL, dominio = c("nenhum", "UF", "V0026")) {
  dominio <- match.arg(dominio)
  filtro_expr <- enquo(filtro)

  if (!quo_is_null(filtro_expr)) {
    dados_filtrados <- subset(dados_pns_design, eval_tidy(filtro_expr, data = dados_pns_design$variables))
  } else {
    dados_filtrados <- dados_pns_design
  }

  var <- variaveis[1]

  if (dominio == "nenhum") {
    total_var <- svytotal(as.formula(paste0("~", var)), design = dados_filtrados, na.rm = TRUE)
    total_df <- as.data.frame(total_var)
    cv_df <- as.data.frame(cv(total_var))
    intervalo_df <- as.data.frame(confint(total_var))

    limpar_nomes <- function(df) {
      rownames(df) <- gsub(var, "", rownames(df))
      df
    }

    total_df <- limpar_nomes(total_df)
    cv_df <- limpar_nomes(cv_df)
    intervalo_df <- limpar_nomes(intervalo_df)

    tabela <- data.frame(
      Categoria = rownames(total_df),
      Total = total_df[, 1],
      Coeficiente_de_Variacao = cv_df[, 1],
      Intervalo_inferior = intervalo_df[, 1],
      Intervalo_superior = intervalo_df[, 2]
    )

    tabela <- tabela[tabela$Categoria != "Ignorado" & tabela$Total != 0, ]
    return(tabela)
  }

  dominio_var <- ifelse(dominio == "UF", "V0001", "V0026")
  formula_inter <- reformulate(paste0("interaction(", dominio_var, ",", var, ")"))

  total_dom <- svytotal(formula_inter, design = dados_filtrados, na.rm = TRUE)
  intervalo_dom <- confint(total_dom)
  cv_dom <- cv(total_dom)

  total_dom_df <- as.data.frame(total_dom)
  intervalo_dom_df <- as.data.frame(intervalo_dom)
  cv_dom_df <- as.data.frame(cv_dom)

  limpar_rownames_inter <- function(nome_df) {
    nomes <- rownames(nome_df)
    nomes_limpos <- sub(".*\\)", "", nomes)
    partes <- strsplit(nomes_limpos, "\\.")

    partes_validas <- sapply(partes, length) == 2
    partes <- partes[partes_validas]

    df_limpo <- nome_df[partes_validas, , drop = FALSE]
    partes_mat <- do.call(rbind, partes)

    df_limpo$dominio <- partes_mat[, 1]
    df_limpo$categoria <- partes_mat[, 2]

    rownames(df_limpo) <- NULL
    df_limpo <- df_limpo[df_limpo$categoria != "Ignorado", , drop = FALSE]
    df_limpo
  }

  total_df <- limpar_rownames_inter(total_dom_df)
  intervalo_df <- limpar_rownames_inter(intervalo_dom_df)
  cv_df <- limpar_rownames_inter(cv_dom_df)

  colnames(total_df)[1] <- "total"
  colnames(intervalo_df)[1:2] <- c("limite_inferior", "limite_superior")
  colnames(cv_df)[1] <- "cv"

  tabela_final <- total_df %>%
    left_join(intervalo_df, by = c("dominio", "categoria")) %>%
    left_join(cv_df, by = c("dominio", "categoria"))

  colnames(tabela_final)[colnames(tabela_final) == "dominio"] <- dominio
  tabela_final <- tabela_final[tabela_final$total != 0, ]
  return(tabela_final)
}
