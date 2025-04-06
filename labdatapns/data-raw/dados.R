# data-raw/dados.R
dicionario <- readr::read_csv("data-raw/dicionario_pns.csv")

# Salva em formato .rda para ser incluído no pacote
usethis::use_data(dicionario, overwrite = TRUE)
