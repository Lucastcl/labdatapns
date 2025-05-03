#' Calcula proporção populacional da variável de interesse
#'
#' Esta função calcula a proporção populacional de uma variável categórica,
#' com base no total populacional estimado pela variável V0015. Para filtros
#' compostos com possíveis `NA`, pode-se ativar a criação de uma variável
#' indicadora auxiliar com `filtro_binario = TRUE`.
#'
#' @param codigo Variável ou expressão de variáveis (com `+`) para o numerador da proporção.
#' @param filtro Expressão lógica com as condições para o numerador.
#' @param dominio Variável de domínio para desagregação ("UF" ou "V0026").
#' @param desagregar Variáveis adicionais para desagregação.
#' @param filtro_binario Quando TRUE, cria uma variável categórica auxiliar com dois rótulos para evitar perdas por `NA`.
#'
#' @return Um data frame com proporções, erro padrão, coeficiente de variação e intervalos de confiança.
#' @export

tabela_prop <- function(codigo, filtro = NULL, dominio = NULL, desagregar = NULL, filtro_binario = FALSE) {
  library(dplyr)
  library(rlang)

  codigo_expr <- enexpr(codigo)
  filtro_expr <- enquo(filtro)
  dominio_expr <- enexpr(dominio)
  desagregar_expr <- enexpr(desagregar)

  extrair_nomes <- function(expr) {
    if (is_call(expr, "+")) {
      unlist(lapply(call_args(expr), extrair_nomes))
    } else if (is_symbol(expr)) {
      as_string(expr)
    } else {
      stop("Expressão inválida para variáveis.")
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

  codigo_chr <- extrair_nomes(codigo_expr)
  var_nome <- if (filtro_binario) "grupo_filtro" else codigo_chr[[1]]
  desagregar_chr <- if (!quo_is_null(enquo(desagregar))) extrair_nomes(desagregar_expr) else character(0)

  num <- tabela(
    codigo = !!codigo_expr,
    filtro = !!filtro_expr,
    dominio = !!dominio_expr,
    desagregar = !!desagregar_expr,
    metrica = total,
    filtro_binario = filtro_binario
  )

  filtro_idade_expr <- extrair_idade_filtro(filtro_expr)
  if (!is.null(filtro_idade_expr)) {
    filtro_idade_expr <- new_quosure(filtro_idade_expr)
  }

  denom <- tabela(
    codigo = V0015,
    filtro = !!filtro_idade_expr,
    dominio = !!dominio_expr,
    desagregar = !!desagregar_expr,
    metrica = total
  )

  total_num_col <- paste0("total_", var_nome)
  cv_num_col <- paste0("cv_", var_nome)
  total_denom_col <- "total_V0015"
  cv_denom_col <- "cv_V0015"

  colunas_join <- c()
  if ("dominio" %in% names(num)) {
    colunas_join <- c(colunas_join, "dominio")
  }
  colunas_join <- c(colunas_join, desagregar_chr)

  base <- left_join(num, denom, by = colunas_join)

  if ("categoria.x" %in% names(base)) {
    base <- base %>% rename(categoria = categoria.x)
  } else {
    base <- base %>% mutate(categoria = "Total")
  }

  if ("dominio" %in% names(base)) {
    base <- base %>% rename(UF = dominio)
  }

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
    select(all_of(c(if ("UF" %in% names(base)) "UF", desagregar_chr)), categoria, prop, cv_prop, ic_inferior, ic_superior)

  return(base)
}
