library(magrittr)

# 1. Escreva uma função que recebe uma data e retorna a tabela dos mananciais
# ...
# Dica: u_base <- "http://mananciais.sabesp.com.br/api/Mananciais/ResumoSistemas/"


baixar_sabesp <- function(data) {
  u_base <-
    "http://mananciais.sabesp.com.br/api/Mananciais/ResumoSistemas/"

  raw_mananciais <- paste0(u_base, data) %>%
    httr::GET() %>%
    httr::content(simplifyDataFrame = TRUE)

  data_correta <-
    raw_mananciais$ReturnObj$DataString %>%
    readr::parse_date(format = "%d/%m/%Y")

  tabela_mananciais <-
    raw_mananciais$ReturnObj$sistemas %>% tibble::as_tibble()

  # A API, caso seja fornecido uma data inválida, redireciona para outra data.
  # Portanto adicionei o if abaixo para checar se a data retornada é igual a
  # data informada como argumento.

  if (data == data_correta) {
    return(tabela_mananciais)
  } else {
    warning(
      "A data informada retorna resultados inválidos. Tente novamente! \n
      Lembre-se que a API é atualizada às 9h."
    )
  }
}


# testando alguns exemplos

baixar_sabesp(Sys.Date()) # Esse exemplo pode funcionar ou não! Depende do horário.

baixar_sabesp(Sys.Date() + 1) # Esse é um exemplo para mostrar o warning.
# Não é possível retornar dados do futuro!


baixar_sabesp("2000-01-01") # Funciona!

baixar_sabesp("1999-12-31") # Não é possível retornar dados anteriores à2000.

# 2. Armazene no objeto tab_sabesp a tabela do dia `Sys.Date() - 1` (ontem)

tab_sabesp <- baixar_sabesp(Sys.Date() - 1)

tab_sabesp


# 3. [extra] Arrume os dados para que fique assim:

# Observations: 7
# Variables: 2
# $ nome   <fct> Cantareira, Alto Tietê, Guarapiranga, Cotia, Rio Grande, Rio Claro, São Lourenço
# $ volume <dbl> 63.25681, 90.35307, 84.25839, 102.28429, 93.66445, 99.85615, 97.33682

sabesp_final <- tab_sabesp %>%
  janitor::clean_names() %>%
  dplyr::select(nome, volume_porcentagem) %>%
  dplyr::rename("volume" = volume_porcentagem) %>%
  dplyr::mutate(nome = forcats::as_factor(nome))

dplyr::glimpse(sabesp_final)
