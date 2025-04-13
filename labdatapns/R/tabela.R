#' Geração de tabela com estimativas da PNS
#'
#' @param variaveis Vetor de variáveis de interesse.
#' @param filtro Expressão lógica para filtragem dos dados.
#' @param dominio Agrupamento adicional ("nenhum", "UF", ou "V0026").
#' @param metrica Tipo de métrica: "total", "media" ou "prop".
#' @param agrupar_por Variáveis adicionais para cruzamento.
#'
#' @return Um data.frame com estimativas, erro amostral e intervalo de confiança.
#' @export
tabela <- function(variaveis = c(),
                   filtro = NULL,
                   dominio = c("nenhum", "UF", "V0026"),
                   metrica = c("total", "media", "prop"),
                   agrupar_por = NULL) {

  dominio <- match.arg(dominio)
  metrica <- match.arg(metrica)
  filtro_expr <- rlang::enquo(filtro)

  dados_base <- dados_pns_design

  if (!rlang::quo_is_null(filtro_expr)) {
    dados_filtrados <- subset(dados_base, rlang::eval_tidy(filtro_expr, data = dados_base$variables))
  } else {
    dados_filtrados <- dados_base
  }

  var <- variaveis[1]
  classe_var <- class(dados_base$variables[[var]])
  is_continua <- any(classe_var %in% c("numeric", "integer", "double"))

  estimador <- switch(
    metrica,
    total = function(x, design) survey::svytotal(x, design = design, na.rm = TRUE),
    media = function(x, design) survey::svymean(x, design = design, na.rm = TRUE),
    prop  = function(x, design) survey::svymean(x, design = design, na.rm = TRUE)
  )

  executar_estima <- function(filtro_extra = NULL) {
    dados_sub <- if (!is.null(filtro_extra)) {
      subset(dados_filtrados, eval(filtro_extra, envir = dados_filtrados$variables))
    } else dados_filtrados

    if (metrica == "media" && is_continua && (dominio != "nenhum" || !is.null(agrupar_por))) {
      dominio_var <- if (dominio == "UF") "V0001" else if (dominio == "V0026") "V0026" else NULL
      by_vars <- c(agrupar_por, dominio_var)
      by_vars <- by_vars[!is.null(by_vars)]

      if (length(by_vars) == 0) {
        est <- survey::svymean(as.formula(paste0("~", var)), dados_sub, na.rm = TRUE)
        intervalo <- stats::confint(est)
        cv_valores <- survey::cv(est)
        return(data.frame(
          categoria = "media",
          metrica = as.numeric(est),
          cv = as.vector(cv_valores),
          ic_inferior = intervalo[, 1],
          ic_superior = intervalo[, 2]
        ))
      }

      combinacoes <- expand.grid(
        lapply(by_vars, function(v) unique(na.omit(dados_base$variables[[v]]))),
        stringsAsFactors = FALSE
      )
      names(combinacoes) <- by_vars

      resultados_lista <- apply(combinacoes, 1, function(comb) {
        condicoes <- mapply(function(v, val) rlang::expr(!!rlang::sym(v) == !!val),
                            names(comb), comb, SIMPLIFY = FALSE)
        filtro_completo <- Reduce(function(x, y) rlang::expr((!!x) & (!!y)), condicoes)
        dados_combinado <- subset(dados_sub, eval(filtro_completo, envir = dados_sub$variables))

        if (nrow(dados_combinado$variables) == 0) return(NULL)

        est <- survey::svymean(as.formula(paste0("~", var)), dados_combinado, na.rm = TRUE)
        intervalo <- stats::confint(est)
        cv_valores <- survey::cv(est)
        resultado <- data.frame(
          metrica = as.numeric(est),
          cv = as.vector(cv_valores),
          ic_inferior = intervalo[, 1],
          ic_superior = intervalo[, 2],
          categoria = "media",
          stringsAsFactors = FALSE
        )
        for (v in names(comb)) {
          resultado[[v]] <- comb[[v]]
        }
        return(resultado)
      })
      return(dplyr::bind_rows(resultados_lista))
    }

    if (dominio == "nenhum") {
      est <- estimador(as.formula(paste0("~", var)), dados_sub)
      intervalo <- stats::confint(est)
      cv_valores <- survey::cv(est)
      categoria_val <- if (metrica == "media") "media" else rownames(est)

      resultado <- data.frame(
        categoria = categoria_val,
        metrica = as.numeric(est),
        cv = as.vector(cv_valores),
        ic_inferior = intervalo[, 1],
        ic_superior = intervalo[, 2]
      )
      return(resultado[resultado$metrica != 0 & resultado$categoria != "Ignorado", ])
    } else {
      dominio_var <- if (dominio == "UF") "V0001" else "V0026"
      formula_inter <- reformulate(paste0("interaction(", dominio_var, ",", var, ")"))
      est <- estimador(formula_inter, dados_sub)
      intervalo <- stats::confint(est)
      cv_res <- survey::cv(est)

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
        df_limpo
      }

      est_df <- limpar_rownames_inter(as.data.frame(est))
      intervalo_df <- limpar_rownames_inter(as.data.frame(intervalo))
      cv_df <- limpar_rownames_inter(as.data.frame(cv_res))

      colnames(est_df)[1] <- "metrica"
      colnames(intervalo_df)[1:2] <- c("ic_inferior", "ic_superior")
      colnames(cv_df)[1] <- "cv"

      resultado <- est_df %>%
        dplyr::left_join(intervalo_df, by = c("dominio", "categoria")) %>%
        dplyr::left_join(cv_df, by = c("dominio", "categoria"))

      resultado <- resultado[resultado$metrica != 0 & resultado$categoria != "Ignorado", ]
      rownames(resultado) <- NULL
      return(resultado)
    }
  }

  if (is.null(agrupar_por)) {
    return(executar_estima())
  }

  lista_niveis <- lapply(agrupar_por, function(v) unique(na.omit(dados_base$variables[[v]])))
  names(lista_niveis) <- agrupar_por
  combinacoes <- expand.grid(lista_niveis, stringsAsFactors = FALSE)

  resultados_lista <- apply(combinacoes, 1, function(comb) {
    condicoes <- mapply(function(var, val) rlang::expr(!!rlang::sym(var) == !!val),
                        names(comb), comb, SIMPLIFY = FALSE)
    filtro_completo <- Reduce(function(x, y) rlang::expr((!!x) & (!!y)), condicoes)
    tabela_filtrada <- executar_estima(filtro_extra = filtro_completo)
    for (var in names(comb)) {
      tabela_filtrada[[var]] <- comb[[var]]
    }
    return(tabela_filtrada)
  })

  resultado_final <- dplyr::bind_rows(resultados_lista)
  rownames(resultado_final) <- NULL
  resultado_final <- resultado_final[!is.na(resultado_final$metrica), ]
  return(resultado_final)
}
