#' Gera tabela a partir do desenho amostral da PNS
#'
#' Esta função calcula totais, médias ou proporções de variáveis da PNS,
#' com opção de filtragem, domínio e desagregação. Suporta também a derivação
#' de variáveis indicadoras binárias com `filtro_binario = TRUE`.
#'
#' @param codigo Variável ou variáveis a serem analisadas (símbolos com ou sem uso de +).
#' @param filtro Expressão lógica para filtrar os dados.
#' @param metrica Tipo de métrica: "total", "media", "prop" ou "prop_pop".
#' @param desagregar Variável(s) para desagregação adicional (símbolos ou uso de +).
#' @param filtro_binario Quando TRUE, cria uma variável categórica binária auxiliar
#'   "grupo_filtro" que evita perdas por NA em filtros lógicos compostos.
#'
#' @return Um data frame com os resultados da estimativa, intervalo de confiança e coeficiente de variação.
#' @export

tabela <- function(codigo, filtro = NULL, metrica = NULL, desagregar = NULL, filtro_binario = FALSE) {
  suppressWarnings({
    library(dplyr)
    library(rlang)
    library(survey)

    # --- Funções auxiliares da sua versão ---
    extrair_nomes <- function(expr) {
      if (is_call(expr, "+")) {
        unlist(lapply(call_args(expr), extrair_nomes))
      } else if (is_symbol(expr)) {
        as_string(expr)
      } else {
        stop("Expressão inválida. Use nomes de variáveis com ou sem '+'.")
      }
    }

    extrair_idade_filtro <- function(expr) {
      if (is.null(expr)) return(NULL)
      expr <- get_expr(expr)
      if (is_call(expr)) {
        op <- call_name(expr)
        if (op %in% c("&", "|")) {
          esquerda <- extrair_idade_filtro(expr[[2]])
          direita <- extrair_idade_filtro(expr[[3]])
          if (!is.null(esquerda) && !is.null(direita)) {
            return(call2(op, esquerda, direita))
          } else if (!is.null(esquerda)) {
            return(esquerda)
          } else if (!is.null(direita)) {
            return(direita)
          } else {
            return(NULL)
          }
        } else {
          lado_esquerdo <- expr[[2]]
          if (is_symbol(lado_esquerdo) && as_string(lado_esquerdo) == "idade") {
            return(expr)
          }
        }
      }
      return(NULL)
    }

    # --- Início da lógica principal da função (da sua versão) ---
    codigo_expr <- enexpr(codigo)
    codigo_chr <- extrair_nomes(codigo_expr)

    metrica_expr <- enexpr(metrica)
    desagregar_expr <- enexpr(desagregar)
    filtro_expr <- enquo(filtro) # Usar o filtro_expr original aqui

    metrica_chr <- if (!quo_is_null(enquo(metrica))) as_string(metrica_expr) else "total"
    metrica_chr <- match.arg(metrica_chr, choices = c("total", "media", "prop", "prop_pop"))

    desagregar_chr <- if (!quo_is_null(enquo(desagregar))) extrair_nomes(desagregar_expr) else character(0)

    # Delegação para tabela_prop (da sua versão)
    if (metrica_chr == "prop_pop") {
      resultado <- tabela_prop(
        codigo = !!codigo_expr,
        filtro = !!filtro_expr,
        desagregar = !!desagregar_expr,
        filtro_binario = filtro_binario
      )
      return(resultado)
    }

    # Bloco filtro_binario (da sua versão, com rótulos dinâmicos inseridos)
    if (!rlang::quo_is_null(filtro_expr) && filtro_binario) {

      # --- INÍCIO DA LÓGICA DE RÓTULOS DINÂMICOS ---
      extrair_valores_do_codigo <- function(expr, var_codigo) {
        if (is_call(expr)) {
          if (call_name(expr) %in% c("==", "%in%") && is_symbol(expr[[2]]) && as_string(expr[[2]]) == var_codigo) {
            return(as.character(eval(expr[[3]])))
          }
          results <- lapply(call_args(expr), extrair_valores_do_codigo, var_codigo)
          return(unique(unlist(results)))
        }
        return(NULL)
      }

      extrair_vars_condicionais <- function(expr) {
        vars <- c()
        recursive_walk <- function(e) {
          if (is_call(e, c("==", "%in%", ">=", "<=", ">", "<", "!="))) {
            lhs <- e[[2]]
            if (is_symbol(lhs)) {
              vars <<- c(vars, as_string(lhs))
            }
          } else if (is_call(e)) {
            lapply(call_args(e), recursive_walk)
          }
        }
        recursive_walk(expr)
        return(unique(vars[!vars %in% "idade"]))
      }

      categorias_pertence_main <- extrair_valores_do_codigo(get_expr(filtro_expr), codigo_chr[1])
      label_pertence <- paste(na.omit(categorias_pertence_main), collapse = " ou ")

      vars_no_filtro <- extrair_vars_condicionais(get_expr(filtro_expr))

      todas_categorias_nao_pertence <- lapply(vars_no_filtro, function(var_name) {
        tryCatch({
          valores_usados <- extrair_valores_do_codigo(get_expr(filtro_expr), var_name)
          ano_pesquisa <- unique(dados_pns_design$variables$ano)[1] # Assume 'ano' existe
          doc_obj <- documentacao_codigo(var_name, ano = ano_pesquisa) # Assume documentacao_codigo existe
          todas_categorias_var <- doc_obj$categorias$Resposta
          todas_categorias_var <- todas_categorias_var[todas_categorias_var != "Ignorado"]
          setdiff(todas_categorias_var, valores_usados)
        }, error = function(e) NULL)
      })

      label_nao_pertence <- paste(unique(unlist(todas_categorias_nao_pertence)), collapse = " ou ")

      if (nchar(label_pertence) == 0) label_pertence <- "Grupo do filtro" # Fallback
      if (nchar(label_nao_pertence) == 0) label_nao_pertence <- "Grupo fora do filtro" # Fallback
      # --- FIM DA LÓGICA DE RÓTULOS DINÂMICOS ---

      # Lógica original da sua função para filtro_base_expr, etc.
      filtro_base_expr <- extrair_idade_filtro(filtro_expr) # Usa o filtro_expr original
      filtro_base_expr <- if (!is.null(filtro_base_expr)) new_quosure(filtro_base_expr) else quo(TRUE)

      dados_pns_design$variables <- dados_pns_design$variables %>%
        mutate(grupo_filtro = factor(
          ifelse(!!rlang::get_expr(filtro_expr), 1, 0), # Usa o filtro_expr original para definir os grupos
          levels = c(1, 0),
          labels = c(label_pertence, label_nao_pertence) # <<< RÓTULOS DINÂMICOS APLICADOS AQUI
        ))

      codigo_chr <- "grupo_filtro"
      filtro_expr <- filtro_base_expr # filtro_expr é atualizado para a próxima etapa de subsetting
    }

    # Criação de dados_filtrados (da sua versão)
    dados_filtrados <- if (!rlang::quo_is_null(filtro_expr)) {
      # Se filtro_binario foi TRUE, filtro_expr aqui é filtro_base_expr
      # Se filtro_binario foi FALSE, filtro_expr aqui é o filtro original
      subset(dados_pns_design, rlang::eval_tidy(filtro_expr, data = dados_pns_design$variables))
    } else {
      dados_pns_design
    }

    # Lógica de cálculo (da sua versão)
    resultados_final <- lapply(codigo_chr, function(var) {
      classe_var <- class(dados_pns_design$variables[[var]])
      is_continua <- any(classe_var %in% c("numeric", "integer", "double"))

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

      col_renomear <- colnames(resultado)
      if ("metrica" %in% col_renomear) colnames(resultado)[col_renomear == "metrica"] <- paste0(metrica_chr, "_", var)
      if ("cv" %in% col_renomear) colnames(resultado)[col_renomear == "cv"] <- paste0("cv_", var)
      if ("ic_inferior" %in% col_renomear) colnames(resultado)[col_renomear == "ic_inferior"] <- paste0("ic_inferior_", var)
      if ("ic_superior" %in% col_renomear) colnames(resultado)[col_renomear == "ic_superior"] <- paste0("ic_superior_", var)

      return(resultado)
    })

    final <- purrr::reduce(resultados_final, left_join, by = c("categoria", desagregar_chr))
    rownames(final) <- NULL

    return(final)
  })
}
