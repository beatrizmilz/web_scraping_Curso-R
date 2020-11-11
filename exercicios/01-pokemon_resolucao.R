# library(httr)
# library(magrittr)
# library(jsonlite)
library(magrittr)

u_base <- "https://pokeapi.co/api/v2/"

# 1. Acesse todos os resultados de "type"
# Dica: qual é o endpoint que devemos utilizar?

# https://pokeapi.co/docs/v2#types


endpoint_type <- "/type"
url_type <- paste0(u_base, endpoint_type)


type_results <- url_type %>%
  httr::GET() %>%
  httr::content(simplifyDataFrame = TRUE) %>%
  .$results %>%
  tibble::as_tibble()


# 2. Encontre o link do type "grass" e guarde em um objeto.
# Dica: você pode fazer isso manualmente ou com um código em R, usando {purrr}

url_grass <- type_results %>%
  dplyr::filter(name == "grass") %>%
  dplyr::pull(url)


# 3. Crie um data.frame com os 20 primeiros pokemons do tipo "grass"
# Dica: utilize o parâmetro query=.
# Além disso, tabelas ficam mais fáceis de visualizar quando rodamos
# tibble::as_tibble(tab)


grass <- url_grass %>%
  # dúvida: esse limite não está retornando 20 primeiros pokemons.
  httr::GET(query = list(limit = 20)) %>%
  httr::content(simplifyDataFrame = TRUE) %>%
  .$pokemon %>%
  tibble::as_tibble() %>%
  dplyr::slice(1:20)
# 20 primeiros pokemons, mas esse não é o jeito correto


grass
