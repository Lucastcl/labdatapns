#' Gera tabela a partir do desenho amostral da PNS
#'
#' Esta função calcula totais, médias ou proporções de variáveis da PNS,
#' com opção de filtragem, domínio e agrupamento.
#'
#' @param variaveis Variável ou variáveis a serem analisadas (símbolos com ou sem uso de +).
#' @param filtro Expressão lógica para filtrar os dados.
#' @param dominio Domínio para desagregação: "nenhum", "UF" ou "V0026".
#' @param metrica Tipo de métrica: "total", "media" ou "prop".
#' @param desagregar Variável(s) para desagregação adicional (símbolos ou uso de +).
#'
#' @return Um data frame com os resultados da estimativa, intervalo de confiança e coeficiente de variação.
#' @export
tabela <- function(variaveis, filtro = NULL, dominio = NULL, metrica = NULL, desagregar = NULL) {
  suppressWarnings({
    library(dplyr)
    library(rlang)
    library(survey)

    # Função auxiliar para extrair nomes de variáveis
    extrair_nomes <- function(expr) {
      if (is_call(expr, "+")) {
        unlist(lapply(call_args(expr), extrair_nomes))
      } else if (is_symbol(expr)) {
        as_string(expr)
      } else {
        stop("Expressão inválida. Use nomes de variáveis com ou sem '+'.")
      }
    }

    # Captura expressões dos argumentos
    variaveis_expr <- enexpr(variaveis)
    variaveis_chr <- extrair_nomes(variaveis_expr)

    dominio_expr <- enexpr(dominio)
    metrica_expr <- enexpr(metrica)
    desagregar_expr <- enexpr(desagregar)
    filtro_expr <- enquo(filtro)

    # Converte para string
    dominio_chr <- if (!quo_is_null(enquo(dominio))) as_string(dominio_expr) else "nenhum"
    metrica_chr <- if (!quo_is_null(enquo(metrica))) as_string(metrica_expr) else "total"

    # Garante valores válidos
    dominio_chr <- match.arg(dominio_chr, choices = c("nenhum", "UF", "V0026"))
    metrica_chr <- match.arg(metrica_chr, choices = c("total", "media", "prop", "prop_pop"))

    # Extrai variáveis para desagregação
    desagregar_chr <- if (!quo_is_null(enquo(desagregar))) extrair_nomes(desagregar_expr) else character(0)

    # Se metrica for prop_pop, delega para tabela_prop
    if (metrica_chr == "prop_pop") {
      resultado <- tabela_prop(
        variaveis = !!variaveis_expr,
        filtro = !!filtro_expr,
        dominio = !!dominio_expr,
        desagregar = !!desagregar_expr
      )
      return(resultado)
    }

    # Aplica filtro se necessário
    dados_filtrados <- if (!rlang::quo_is_null(filtro_expr)) {
      subset(dados_pns_design, rlang::eval_tidy(filtro_expr, data = dados_pns_design$variables))
    } else {
      dados_pns_design
    }

    resultados_final <- lapply(variaveis_chr, function(var) {
      classe_var <- class(dados_pns_design$variables[[var]])
      is_continua <- any(classe_var %in% c("numeric", "integer", "double"))

      # Define o estimador conforme métrica
      estimador <- switch(
        metrica_chr,
        total = function(x, design) svytotal(x, design = design, na.rm = TRUE),
        media = function(x, design) svymean(x, design = design, na.rm = TRUE),
        prop  = function(x, design) svymean(x, design = design, na.rm = TRUE)
      )

      executar_estima <- function(filtro_extra = NULL) {
        dados_sub <- if (!is.null(filtro_extra)) {
          subset(dados_filtrados, eval(parse_expr(filtro_extra), envir = dados_filtrados$variables))
        } else dados_filtrados

        if (dominio_chr == "nenhum") {
          formula_est <- as.formula(paste0("~", var))
          est <- estimador(formula_est, dados_sub)
          intervalo <- confint(est)
          cv_valores <- cv(est)
          categorias <- rownames(est)

          if (is.null(categorias) || any(nchar(categorias) == 0)) {
            categorias <- names(est)
          }
          categorias <- gsub(paste0("^", var), "", categorias)

          resultado <- data.frame(
            categoria = categorias,
            metrica = as.numeric(est),
            cv = as.vector(cv_valores),
            ic_inferior = intervalo[, 1],
            ic_superior = intervalo[, 2],
            stringsAsFactors = FALSE
          )

          resultado <- resultado[resultado$metrica != 0 & resultado$categoria != "Ignorado", ]
          rownames(resultado) <- NULL
          return(resultado)
        }

        # Se houver domínio
        dominio_var <- ifelse(dominio_chr == "UF", "V0001", "V0026")

        # Para média de variáveis contínuas
        if (metrica_chr == "media" && is_continua) {
          dominios <- unique(na.omit(dados_sub$variables[[dominio_var]]))
          resultados <- lapply(dominios, function(dom) {
            valor_expr <- shQuote(as.character(dom))
            filtro_dom <- paste0(dominio_var, " == ", valor_expr)
            dados_dom <- subset(dados_sub, eval(parse_expr(filtro_dom), envir = dados_sub$variables))

            est <- svymean(as.formula(paste0("~", var)), dados_dom, na.rm = TRUE)
            intervalo <- confint(est)
            cv_valores <- cv(est)

            data.frame(
              dominio = dom,
              categoria = "media",
              metrica = as.numeric(est),
              cv = as.vector(cv_valores),
              ic_inferior = intervalo[, 1],
              ic_superior = intervalo[, 2]
            )
          })
          resultado <- do.call(rbind, resultados)
          rownames(resultado) <- NULL
          return(resultado)
        }

        # Para variáveis categóricas com domínio
        formula_inter <- reformulate(paste0("interaction(", dominio_var, ",", var, ")"))
        est <- estimador(formula_inter, dados_sub)
        intervalo <- confint(est)
        cv_valores <- cv(est)

        est_df <- as.data.frame(est)
        intervalo_df <- as.data.frame(intervalo)
        cv_df <- as.data.frame(cv_valores)

        limpar_rownames_inter <- function(nome_df) {
          nomes <- rownames(nome_df)
          nomes_limpos <- sub(".*\\)", "", nomes)
          partes <- strsplit(nomes_limpos, "\\.")
          partes_validas <- sapply(partes, length) == 2
          partes <- partes[partes_validas]
          df_limpo <- nome_df[partes_validas, , drop = FALSE]
          partes_mat <- do.call(rbind, partes)
          df_limpo$dominio <- partes_mat[, 1]
          df_limpo$categoria <- gsub(paste0("^", var), "", partes_mat[, 2])
          rownames(df_limpo) <- NULL
          df_limpo
        }

        est_df <- limpar_rownames_inter(est_df)
        intervalo_df <- limpar_rownames_inter(intervalo_df)
        cv_df <- limpar_rownames_inter(cv_df)

        colnames(est_df)[1] <- "metrica"
        colnames(intervalo_df)[1:2] <- c("ic_inferior", "ic_superior")
        colnames(cv_df)[1] <- "cv"

        resultado <- est_df %>%
          left_join(intervalo_df, by = c("dominio", "categoria")) %>%
          left_join(cv_df, by = c("dominio", "categoria"))

        resultado <- resultado[resultado$metrica != 0 & resultado$categoria != "Ignorado", ]
        rownames(resultado) <- NULL

        return(resultado)
      }

      if (length(desagregar_chr) == 0) {
        resultado <- executar_estima()
      } else {
        lista_niveis <- lapply(desagregar_chr, function(v) unique(na.omit(dados_pns_design$variables[[v]])))
        names(lista_niveis) <- desagregar_chr
        combinacoes <- expand.grid(lista_niveis, stringsAsFactors = FALSE)

        resultados_lista <- apply(combinacoes, 1, function(comb) {
          filtros <- mapply(function(var, val) {
            paste0(var, " == ", if (is.character(val)) paste0("\"", val, "\"") else val)
          }, names(comb), comb, SIMPLIFY = TRUE)
          filtro_cat <- paste(filtros, collapse = " & ")
          tabela_filtrada <- executar_estima(filtro_extra = filtro_cat)
          if (nrow(tabela_filtrada) == 0) return(NULL)
          for (v in names(comb)) {
            tabela_filtrada[[v]] <- comb[[v]]
          }
          return(tabela_filtrada)
        })

        resultado <- bind_rows(purrr::compact(resultados_lista))
      }

      # Renomear colunas
      col_renomear <- colnames(resultado)
      if ("metrica" %in% col_renomear) colnames(resultado)[col_renomear == "metrica"] <- paste0(metrica_chr, "_", var)
      if ("cv" %in% col_renomear) colnames(resultado)[col_renomear == "cv"] <- paste0("cv_", var)
      if ("ic_inferior" %in% col_renomear) colnames(resultado)[col_renomear == "ic_inferior"] <- paste0("ic_inferior_", var)
      if ("ic_superior" %in% col_renomear) colnames(resultado)[col_renomear == "ic_superior"] <- paste0("ic_superior_", var)

      return(resultado)
    })

    final <- purrr::reduce(resultados_final, left_join, by = c("dominio", "categoria", desagregar_chr))
    rownames(final) <- NULL

    return(final)
  })
}
