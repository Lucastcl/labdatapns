#' Carrega os dados da PNS com desenho amostral e colunas derivadas
#'
#' Esta função baixa e prepara os dados da PNS 2013 ou 2019 com as variáveis solicitadas,
#' o desenho amostral (survey design), e adiciona variáveis derivadas como faixa etária,
#' região, IMC categorizado e variáveis renomeadas.
#'
#' @param ano Ano da PNS (2013 ou 2019).
#' @param vars Vetor de nomes de variáveis adicionais a serem carregadas.
#' @param global Se TRUE (padrão), os objetos são atribuídos no ambiente global.
#'
#' @return Atribui `dados_pns_design` e `vars` ao ambiente global (ou retorna em lista, se `global = FALSE`).
#' @export

dados_pns <- function(ano, vars = c(), global = TRUE) {
  if (!ano %in% c(2013, 2019)) {
    stop("O argumento 'ano' deve ser 2013 ou 2019.")
  }

  options(survey.lonely.psu = "adjust")

  # Mapeamento dos módulos por tipo de questionário para cada ano
  modulo_tipo_2013 <- c(
    A = "basico", B = "basico", C = "basico", D = "basico", E = "basico",
    F = "basico", G = "basico", I = "basico", J = "basico", K = "basico",
    L = "basico", U = "basico", M = "selecionado", N = "selecionado",
    O = "selecionado", P = "selecionado", Q = "selecionado",
    R = "selecionado", S = "selecionado", X = "selecionado"
  )

  modulo_tipo_2019 <- c(
    A = "basico", B = "basico", C = "basico", D = "basico", E = "basico",
    F = "basico", G = "basico", H = "selecionado", I = "basico",
    J = "basico", K = "basico", L = "basico", M = "selecionado",
    N = "selecionado", O = "selecionado", P = "selecionado",
    Q = "selecionado", R = "selecionado", S = "selecionado",
    T = "selecionado", U = "selecionado", V = "selecionado",
    W = "antropometria", Y = "selecionado", Z = "selecionado", AA = "selecionado"
  )

  modulo_tipo <- if (ano == 2013) modulo_tipo_2013 else modulo_tipo_2019

  # Variáveis obrigatórias comuns
  vars_fixas <- c("C006", "C008", "C009", "D001", "V0001", "V0026", "VDF003")

  # Variáveis obrigatórias por ano
  vars_ano <- if (ano == 2019) {
    c("P00404", "P00104", "VDF004", "V0025A", "P04801")
  } else {
    c("P00401", "P00101", "V0025", "P048")
  }

  # Identifica variáveis adicionais fornecidas pelo usuário
  vars_usuario <- setdiff(vars, c(vars_fixas, vars_ano))

  # Verifica o tipo das variáveis fornecidas
  verificar_tipo_variaveis <- function(vars_usuario, modulo_tipo) {
    if (length(vars_usuario) == 0) {
      return(list(selecionado = FALSE, antropometria = FALSE))
    }

    prefixos <- toupper(gsub("[^A-Z]", "", vars_usuario))
    prefixos <- ifelse(substr(prefixos, 1, 2) %in% names(modulo_tipo),
                       substr(prefixos, 1, 2),
                       substr(prefixos, 1, 1))
    tipos <- modulo_tipo[prefixos]

    if (any(is.na(tipos))) {
      stop("Variáveis com prefixos não reconhecidos: ",
           paste(unique(prefixos[is.na(tipos)]), collapse = ", "))
    }

    tipos_unicos <- unique(tipos)

    if (length(tipos_unicos) == 1) {
      return(list(
        selecionado = tipos_unicos == "selecionado",
        antropometria = tipos_unicos == "antropometria"
      ))
    } else {
      conflitos <- split(vars_usuario, tipos)
      mensagem <- "As variáveis fornecidas pertencem a diferentes tipos de questionário:\n"
      for (tipo in names(conflitos)) {
        mensagem <- paste0(mensagem, "- ", tipo, ": ", paste(conflitos[[tipo]], collapse = ", "), "\n")
      }
      mensagem <- paste0(mensagem, "Evite misturar variáveis de diferentes tipos de questionário na mesma análise.")
      stop(mensagem)
    }
  }

  # Avaliação do tipo de questionário com base no ano
  parametros <- verificar_tipo_variaveis(vars_usuario, modulo_tipo)
  selecionado <- parametros$selecionado
  antropometria <- parametros$antropometria

  # Todas as variáveis a serem carregadas
  vars <- unique(c(vars, vars_fixas, vars_ano))

  # Carrega os dados com o survey design
  dados_pns_design <- get_pns(
    year = ano,
    labels = TRUE,
    vars = vars,
    design = TRUE,
    selected = selecionado,
    anthropometry = antropometria
  )

  # Criação de variáveis derivadas
  dados_pns_design$variables <- dados_pns_design$variables %>%
    mutate(
      faixa_idade = case_when(
        C008 >= 18 & C008 <= 29 ~ "18 a 29",
        C008 >= 30 & C008 <= 59 ~ "30 a 59",
        C008 >= 60 & C008 <= 64 ~ "60 a 64",
        C008 >= 65 & C008 <= 74 ~ "65 a 74",
        C008 >= 75             ~ "75 ou mais",
        TRUE                   ~ NA_character_
      ),
      faixa_idade_5 = case_when(
        C008 < 18 ~ NA_character_,
        TRUE ~ cut(C008,
                   breaks = c(18, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, Inf),
                   right = TRUE, include.lowest = TRUE,
                   labels = c("18 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44",
                              "45 a 49", "50 a 54", "55 a 59", "60 a 64", "65 a 69",
                              "70 a 74", "75 ou mais"))
      ),
      regiao = case_when(
        V0001 %in% c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins") ~ "Norte",
        V0001 %in% c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba",
                     "Pernambuco", "Alagoas", "Sergipe", "Bahia") ~ "Nordeste",
        V0001 %in% c("Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo") ~ "Sudeste",
        V0001 %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul") ~ "Sul",
        V0001 %in% c("Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal") ~ "Centro-Oeste",
        TRUE ~ NA_character_
      )
    )

  # Cálculo do IMC
  altura <- if (ano == 2019) dados_pns_design$variables$P00404 else dados_pns_design$variables$P00401
  peso   <- if (ano == 2019) dados_pns_design$variables$P00104 else dados_pns_design$variables$P00101

  dados_pns_design$variables$imc_valor <- ifelse(
    !is.na(altura) & !is.na(peso) & altura > 0,
    peso / ((altura / 100)^2),
    NA
  )

  # Classificação do IMC
  dados_pns_design$variables <- dados_pns_design$variables %>%
    mutate(
      imc = case_when(
        is.na(imc_valor)                    ~ NA_character_,
        imc_valor < 18.5                    ~ "Abaixo do peso",
        imc_valor >= 18.5 & imc_valor < 25  ~ "Peso normal",
        imc_valor >= 25   & imc_valor < 30  ~ "Sobrepeso",
        imc_valor >= 30   & imc_valor < 35  ~ "Obesidade grau I",
        imc_valor >= 35   & imc_valor < 40  ~ "Obesidade grau II",
        imc_valor >= 40                     ~ "Obesidade grau III"
      ),
      domicilio = V0026,
      sexo = C006,
      idade = C008,
      uf = V0001,
      renda = VDF003,
      faixa_renda = if (ano == 2019) VDF004 else NULL,
      raça = C009,
      ler_escrever = D001,
      atividade_fisica = if (ano == 2019) P04801 else P048
    )

  if (global) {
    assign("dados_pns_design", dados_pns_design, envir = .GlobalEnv)
    assign("vars", vars, envir = .GlobalEnv)
    invisible(NULL)
  } else {
    return(list(dados_pns_design = dados_pns_design, vars = vars))
  }
}
