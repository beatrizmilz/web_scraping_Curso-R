library(magrittr)

# 1. Baixe as posicoes pelo endpoint /Posicao

# URL
u_sptrans <- "http://api.olhovivo.sptrans.com.br/v2.1"

# Autenticar / fazer o login
api_key <-
  "4af5e3112da870ac5708c48b7a237b30206806f296e1d302e4cb611660e2e03f"
q_sptrans_login <- list(token = api_key)
u_sptrans_login <- paste0(u_sptrans, "/Login/Autenticar")
r_sptrans_login <-
  httr::POST(u_sptrans_login, query = q_sptrans_login)

# Verificar se estamos autenticadas
httr::content(r_sptrans_login)

# Fazer a busca
u_sptrans_busca <- paste0(u_sptrans, "/Posicao")
r_sptrans_busca <- httr::GET(u_sptrans_busca)
tab_sptrans <-
  httr::content(r_sptrans_busca, simplifyDataFrame = TRUE)

# 2. Rode tibble::glimpse(tab_sptrans$l, 50)
## e cole o resultado abaixo. Deixe o resultado como um comentário
## Dica: Selecione as linhas que deseja comentar e aplique Ctrl+Shift+C


# > tibble::glimpse(tab_sptrans$l, 50)
# Rows: 1,949
# Columns: 7
# $ c   <chr> "695Y-10", "746C-10", "8600-10", ...
# $ cl  <int> 33966, 783, 1432, 132, 33553, 134...
# $ sl  <int> 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 2, ...
# $ lt0 <chr> "METRÔ VL. MARIANA", "STO. AMARO"...
# $ lt1 <chr> "TERM. PARELHEIROS", "JD. TABOÃO"...
# $ qv  <int> 11, 9, 5, 3, 6, 7, 5, 6, 8, 6, 2,...
# $ vs  <list> [<data.frame[11 x 5]>, <data.fra...



# 3. Quantas/quais linhas de ônibus temos com o nome LAPA?
## Dica: descubra o endpoint e use um parâmetro de busca!


#u_linha_lapa <- paste0(u_sptrans, "/Linha/Buscar?termosBusca=LAPA")


u_linha <- paste0(u_sptrans, "/Linha/Buscar")

r_linha <- httr::GET(u_linha, query = list(termosBusca = "LAPA"))

tab_linha <- httr::content(r_linha, simplifyDataFrame = TRUE)


# Olhando a documentação, o letreiro numérico da linha é formado por lt-tl        ex.: 5011-10
linhas_lapa <- tab_linha %>%
  dplyr::mutate(codigo_linha = glue::glue("{lt}-{tl}")) %>%
  dplyr::distinct(codigo_linha)

# Número de linhas
nrow(linhas_lapa)



# 4. [extra] Escolha uma linha e obtenha a posição de todos os ônibus dessa linha.
# Obtenha uma tabela com as coordenadas de latitude e longitude.

tabela_posicoes_eldorado <- tab_sptrans %>%
  purrr::pluck("l") %>%
  tibble::as_tibble() %>%
  dplyr::filter(c == "5011-10") %>%
  tidyr::unnest(cols = c(vs))

tabela_posicoes_eldorado

# 5. [extra] use o pacote leaflet para montar um mapa contendo a posição de todos
# os ônibus da linha.

# Busão pra eldorado!

# http://www.sptrans.com.br/desenvolvedores/api-do-olho-vivo-guia-de-referencia/documentacao-api/#docApi-posicao

# Cor dos pontos: sentido da linha.


tabela_posicoes_eldorado %>%
  dplyr::mutate(cor = dplyr::case_when(sl == 1 ~ "blue",
                                       sl == 2 ~ "green")) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircles(lng = ~ px,
                      lat = ~ py,
                      color = ~ cor)
