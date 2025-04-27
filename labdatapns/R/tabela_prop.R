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

  # Captura expressões dos argumentos
  variaveis_expr <- enexpr(variaveis)
  filtro_expr <- enquo(filtro)
  dominio_expr <- enexpr(dominio)
  desagregar_expr <- enexpr(desagregar)

  # Função para extrair nomes de variáveis
  extrair_nomes <- function(expr) {
    if (is_call(expr, "+")) {
      unlist(lapply(call_args(expr), extrair_nomes))
    } else if (is_symbol(expr)) {
      as_string(expr)
    } else {
      stop("Expressão inválida para variáveis.")
    }
  }

  # Função para extrair apenas filtro relacionado à idade
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

  # Extrai nomes
  variaveis_chr <- extrair_nomes(variaveis_expr)
  var_nome <- variaveis_chr[[1]]
  desagregar_chr <- if (!quo_is_null(enquo(desagregar))) extrair_nomes(desagregar_expr) else character(0)

  # Numerador
  num <- tabela(
    variaveis = !!variaveis_expr,
    filtro = !!filtro_expr,
    dominio = !!dominio_expr,
    desagregar = !!desagregar_expr,
    metrica = total
  )

  # Denominador
  filtro_idade_expr <- extrair_idade_filtro(filtro_expr)
  if (!is.null(filtro_idade_expr)) {
    filtro_idade_expr <- new_quosure(filtro_idade_expr)
  }

  denom <- tabela(
    variaveis = V0025A,
    filtro = !!filtro_idade_expr,
    dominio = !!dominio_expr,
    desagregar = !!desagregar_expr,
    metrica = total
  )

  # Ajusta nomes
  total_num_col <- paste0("total_", var_nome)
  cv_num_col <- paste0("cv_", var_nome)
  total_denom_col <- "total_V0025A"
  cv_denom_col <- "cv_V0025A"

  # Ajuste para fazer o join correto
  colunas_join <- c()
  if ("dominio" %in% names(num)) {
    colunas_join <- c(colunas_join, "dominio")
  }
  colunas_join <- c(colunas_join, desagregar_chr)

  base <- left_join(num, denom, by = colunas_join)

  # Ajuste categoria
  if ("categoria.x" %in% names(base)) {
    base <- base %>% rename(categoria = categoria.x)
  } else {
    base <- base %>% mutate(categoria = "Total")
  }

  # Se existir domínio, renomear para UF
  if ("dominio" %in% names(base)) {
    base <- base %>% rename(UF = dominio)
  }

  # Calcula proporção e medidas associadas
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
