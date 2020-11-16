# library(tidyverse)
# library(httr)
#u_msaude <- "https://covid.saude.gov.br"

# 1. baixe e carregue a base do covid no objeto tab_covid

# https://opendatasus.saude.gov.br/dataset/casos-nacionais/resource/30c7902e-fe02-4986-b69d-906ca4c2ec36


u_elastic <- "https://elasticsearch-saps.saude.gov.br/desc-notificacoes-esusve-*/_search?pretty"

# Autenticar

e_usuario <- "user-public-notificacoes"
e_senha <-  "Za4qNXdyQNSa9YaA"



# Não to sabendo autenticar T_T
httr::POST(u_elastic, query = list(Username = e_usuario, Password = e_senha))


# Buscando resutlados?
r_elastic <- httr::GET(u_elastic)

tab_covid <- httr::content(r_elastic)  # Erro pois precisa autenticar!


# 2. Rode tibble::glimpse(tab_covid, 50)



# 3. [extra] monte um gráfico mostrando taxa acumulada de mortes pela população,
# ao longo das semanas epidemiológicas, para cada estado.


