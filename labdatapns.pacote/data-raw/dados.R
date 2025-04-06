# data-raw/dados.R
dados <- readr::read_csv("data-raw/dicionario_pns.csv")

# Salva em formato .rda para ser incluído no pacote
usethis::use_data(dados, overwrite = TRUE)
