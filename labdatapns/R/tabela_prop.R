#' Calcula proporção populacional ajustada
#'
#' Calcula a proporção de uma variável sobre a população total,
#' considerando filtros e desagregações específicas.
#'
#' @param variaveis Variável ou combinação de variáveis para o numerador (usar `+` para múltiplas).
#' @param filtro Expressão lógica opcional para aplicar no numerador.
#' @param dominio Domínio para desagregação: "nenhum", "UF" ou "V0026".
#' @param desagregar Variáveis adicionais para desagregar os resultados.
#'
#' @return Um data frame com proporção, erro padrão, coeficiente de variação e intervalos de confiança.
#' @export
tabela_prop <- function(variaveis, filtro = NULL, dominio = NULL, desagregar = NULL) {
  library(dplyr)
  library(rlang)

  variaveis_expr <- enexpr(variaveis)
  filtro_expr <- enexpr(filtro)
  dominio_expr <- enexpr(dominio)
  desagregar_expr <- enexpr(desagregar)

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
    if (is.null(expr)) {
      return(NULL)
    }
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
        } else {
          return(NULL)
        }
      }
    } else {
      return(NULL)
    }
  }

  desagregar_chr <- if (!quo_is_null(enquo(desagregar))) {
    extrair_nomes(desagregar_expr)
  } else {
    character(0)
  }

  variaveis_chr <- extrair_nomes(variaveis_expr)
  var_nome <- variaveis_chr[[1]]

  # Numerador
  num <- tabela(
    variaveis = !!variaveis_expr,
    filtro = !!filtro_expr,
    dominio = !!dominio_expr,
    desagregar = !!desagregar_expr,
    metrica = total
  )

  # Filtro apenas de idade (para o denominador)
  filtro_idade <- extrair_idade_filtro(get_expr(filtro_expr))
  filtro_idade_expr <- if (!is.null(filtro_idade)) {
    new_quosure(filtro_idade)
  } else {
    NULL
  }

  # Denominador
  denom <- tabela(
    variaveis = V0025A,
    filtro = !!filtro_idade_expr,
    dominio = !!dominio_expr,
    desagregar = !!desagregar_expr,
    metrica = total
  )

  total_num_col <- paste0("total_", var_nome)
  cv_num_col <- paste0("cv_", var_nome)
  total_denom_col <- "total_V0025A"
  cv_denom_col <- "cv_V0025A"

  # Juntar numerador e denominador
  base <- left_join(num, denom, by = desagregar_chr)

  # Ajustar categoria
  if ("categoria.x" %in% names(base)) {
    base <- base %>% rename(categoria = categoria.x)
  } else {
    base <- base %>% mutate(categoria = "Total")
  }

  # Calcular proporção
  base <- base %>%
    mutate(
      prop = !!sym(total_num_col) / !!sym(total_denom_col),
      se_num = !!sym(cv_num_col) * !!sym(total_num_col),
      se_denom = !!sym(cv_denom_col) * !!sym(total_denom_col),
      se_prop = se_num / !!sym(total_denom_col),
      cv_prop = se_prop / prop,
      ic_inferior = prop - 1.96 * se_prop,
      ic_superior = prop + 1.96 * se_prop
    ) %>%
    select(all_of(desagregar_chr), categoria, prop, cv_prop, ic_inferior, ic_superior)

  return(base)
}
