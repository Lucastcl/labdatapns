#' Carrega os dados da PNS 2019 com desenho amostral e colunas derivadas
#'
#' Esta função baixa e prepara os dados da PNS 2019 com as variáveis solicitadas,
#' o desenho amostral (survey design), e adiciona variáveis derivadas como faixa etária,
#' região, IMC categorizado e variáveis renomeadas.
#'
#' @param vars Vetor de nomes de variáveis adicionais a serem carregadas.
#' @param global Se TRUE (padrão), os objetos são atribuídos no ambiente global.
#' @param selecionado Se TRUE (padrão), inclui apenas os entrevistados selecionados.
#' @param antropometria Se TRUE (padrão), inclui variáveis de antropometria.
#'
#' @return Atribui `dados_pns_design` e `vars` ao ambiente global (ou retorna em lista, se `global = FALSE`).
#' @export
dados_pns_2019 <- function(vars = c(), global = TRUE, selecionado = TRUE, antropometria = TRUE) {

  options(survey.lonely.psu = "adjust")

  # Variáveis obrigatórias para o design
  vars_fixas <- c("C006", "C008","C009","D001","P00404", "P00104", "VDF003", "VDF004", "VDF002",
                  "V0001", "V0026", "D001","P04801","V0025A")
  vars <- unique(c(vars, vars_fixas))

  # Obtém os dados com o survey design
  dados_pns_design <- get_pns(
    year = 2019,
    labels = TRUE,
    vars = vars,
    design = TRUE,
    selected = selecionado,
    anthropometry = antropometria
  )

  # Criação de colunas derivadas
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
      dados_pns_design$variables <- dados_pns_design$variables %>%
        mutate(
          faixa_idade_5 = case_when(
            C008 < 18 ~ NA_character_,
            TRUE ~ cut(
              C008,
              breaks = c(18, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, Inf),
              right = TRUE,
              include.lowest = TRUE,
              labels = c(
                "18 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44",
                "45 a 49", "50 a 54", "55 a 59", "60 a 64",
                "65 a 69", "70 a 74", "75 ou mais"
              )
            )
          )
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
  altura <- dados_pns_design$variables$P00404
  peso <- dados_pns_design$variables$P00104

  dados_pns_design$variables$imc_valor <- ifelse(
    !is.na(altura) & !is.na(peso) & altura > 0,
    peso / ((altura / 100)^2),
    NA
  )

  dados_pns_design$variables <- dados_pns_design$variables %>%
    mutate(
      imc = case_when(
        is.na(imc_valor)              ~ NA_character_,
        imc_valor < 18.5              ~ "Abaixo do peso",
        imc_valor >= 18.5 & imc_valor < 25  ~ "Peso normal",
        imc_valor >= 25   & imc_valor < 30  ~ "Sobrepeso",
        imc_valor >= 30   & imc_valor < 35  ~ "Obesidade grau I",
        imc_valor >= 35   & imc_valor < 40  ~ "Obesidade grau II",
        imc_valor >= 40               ~ "Obesidade grau III"
      ),
      domicilio = V0026,
      sexo = C006,
      idade = C008,
      uf = V0001,
      renda = VDF003,
      faixa_renda = VDF004,
      raça = C009,
      ler_escrever = D001,
      atividade_fisica = P04801
    )

  # Retorno
  if (global) {
    assign("dados_pns_design", dados_pns_design, envir = .GlobalEnv)
    assign("vars", vars, envir = .GlobalEnv)
    invisible(NULL)
  } else {
    return(list(dados_pns_design = dados_pns_design, vars = vars))
  }
}
