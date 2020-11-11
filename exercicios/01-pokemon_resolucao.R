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


type_get <- url_type %>%
  httr::GET()


 type_results <- type_get %>%
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


grass_get <- url_grass %>%
  httr::GET()


grass <- grass_get %>%
  httr::content(simplifyDataFrame = TRUE) %>%
  .$pokemon %>%
  tibble::as_tibble() %>%
  dplyr::slice(1:20)
# 20 primeiros pokemons


grass


# Feedback: Julio Trecenti
# uma sugestão: sempre deixar separado o comando GET() e os outros comandos.
# Fica mais bonito com o pipe, mas pra cada vez que você roda, o programa acessa a internet.
# Então geralmente é mais eficiente separar.
