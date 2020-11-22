library(magrittr)

# 1. Crie uma função tjsp_baixar_pag() que baixa a página
## Dica: usar httr::write_disk()

tjsp_baixar_pag <-   function(path_download,
                              termo_busca = "coronavirus",
                              baixar_tudo = FALSE,
                              max_download = 10) {
  # Carregar pipe
  `%>%` <- magrittr::`%>%`

  # Esse path existe? se não existe, precisa criar!
  if (!dir.exists(path_download)) {
    dir.create(path_download)
  }

  # Primeiro: fazer a busca inicial!
  u_tjsp <- "https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do"

  b_tjsp <- list(dados.buscaInteiroTeor = termo_busca,
                 dados.origensSelecionadas = "T")

  r_tjsp <- httr::POST(u_tjsp, body = b_tjsp, encode = "form")

  # Quantas páginas são no total?

  paginacao_inferior <- httr::content(r_tjsp) %>%
    xml2::xml_find_all("//div[@id='paginacaoInferior-A']") %>%
    xml2::xml_text() %>%
    stringr::str_squish() %>%
    stringr::str_split(pattern = " ")


  # Número total de resultados
  total_resultados <- as.double(paginacao_inferior[[1]][6])

  # São 20 resultados por página
  n_paginas <- ceiling(total_resultados / 20) # total de páginas

  if (baixar_tudo == TRUE) {
    paginas_baixar <- n_paginas
  } else {
    paginas_baixar <- max_download
  }

  # Página dos resultados
  u_tjsp_pag <- "https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do"

  # Agora precisamos iterar por página

  for (n_pag in 1:paginas_baixar) {
    q_tjsp_pag <-
      list(
        tipoDeDecisao = "A",
        pagina = n_pag,
        conversationId = ""
      )

    r_tjsp_pag <-
      httr::GET(u_tjsp_pag, query = q_tjsp_pag, httr::write_disk(
        path = glue::glue("{path_download}busca_{termo_busca}_pag_{n_pag}.html")
      ))

  }


}


# 2. Baixe todos os resultados


tjsp_baixar_pag(path_download = "exercicios/tjsp/", termo_busca = "coronavirus")


# 3. Crie uma funcao ler_item que lê cada item da tabela, como fizemos

ler_item <- function(html_baixado) {
  # Essa função usa muito do que já estava pronto na aula :)

  # Carregar pipe
  `%>%` <- magrittr::`%>%`

  # apagar isso depois
  html_baixado <- "exercicios/tjsp/busca_coronavirus_pag_1.html"

  tabela_pagina <- html_baixado %>%
    xml2::read_html(encoding = "UTF-8") %>%
    xml2::xml_find_all("//tr[@class='fundocinza1']//table") %>%
    purrr::map(~ rvest::html_table(.x, fill = TRUE)) %>%
    purrr::map(~ tibble::as_tibble(.x)) %>%
    purrr::map(~ dplyr::mutate(
      .x,
      X1 = stringr::str_squish(.x$X1),
      X2 = stringr::str_squish(.x$X2)
    )) %>%
    purrr::map(~ dplyr::select(.x, X1)) # essa coluna X2 tem repetição

  t_numero <- tabela_pagina %>%
    purrr::map(~ dplyr::slice(.x, 1)) %>%
    purrr::map(~ tidyr::separate(
      .x,
      X1,
      into = c("numero", "numero_texto"),
      sep = " ",
      fill = "right",
      extra = "merge"
    ))


  t_restante <- tabela_pagina %>%
    purrr::map(~ dplyr::slice(.x, -1)) %>%
    purrr::map(~ tidyr::separate(
      .x,
      X1,
      into = c("nome_col", "descricao"),
      sep = ": ",
      fill = "right",
      extra = "merge"
    )) %>%
    purrr::map(~ tidyr::pivot_wider(.x, names_from = nome_col, values_from = descricao)) %>%
    purrr::map(~ janitor::clean_names(.x))



  tabela_pagina %>%
    purrr::map(~ dplyr::bind_cols(.x, t_restante))


  df <-
    dplyr::bind_cols(dplyr::bind_rows(t_numero), dplyr::bind_rows(t_restante))


  df

}


# 4. Aplique a sua função em todos os elementos da saída e empilhe
## Dica: usar purrr::map_dfr()

tab_tjsp <- list.files(path = "exercicios/tjsp/") %>%
  purrr::map_dfr(ler_item)

# 5. Separe o título do conteúdo
## Dica: usar tidyr::separate()

# feito antes

# 6. Aplique tidyr::pivot_wider() para ficar com uma linha
## para cada processo. Cuidado: a linha com o número do processo é diferente

# Não entendi o comentário no "cuidado"

# 7. Armazene o resultado final em

# 8.  Rode tibble::glimpse(tab_tjsp)
## e cole o resultado abaixo. Deixe o resultado como um comentário
## Dica: Selecione as linhas que deseja comentar e aplique Ctrl+Shift+C


# tem 200 linhas pois só baixei as 10 primeiras páginas

# > tibble::glimpse(tab_tjsp)
# Rows: 200
# Columns: 9
# $ numero             <chr> "2214737-92.2020.8.26.0000", "2213950-63.2020.8.26.00...
# $ numero_texto       <chr> "Habeas corpus – Crimes de responsabilidade (Decreto ...
# $ classe_assunto     <chr> "Habeas Corpus Criminal / Crimes da Lei de licitações...
# $ relator_a          <chr> "Otavio Rocha", "Otavio Rocha", "Otavio Rocha", "Otav...
# $ comarca            <chr> "Fartura", "São Bernardo do Campo", "Presidente Prude...
# $ orgao_julgador     <chr> "7ª Câmara de Direito Criminal", "7ª Câmara de Direit...
# $ data_do_julgamento <chr> "22/11/2020", "22/11/2020", "22/11/2020", "22/11/2020...
# $ data_de_publicacao <chr> "22/11/2020", "22/11/2020", "22/11/2020", "22/11/2020...
# $ ementa             <chr> "Habeas corpus – Crimes de responsabilidade (Decreto ...
# >


# 9. [extra] monte um wordcloud.
## Dica: use tidytext para separar as palavras e ggwordcloud para o gráfico.
## Dica: mostre apenas palavras que aparecem mais do que 5 vezes.

stop_words_pt <-  tibble::tibble(word = tm::stopwords("pt")) %>%
  tibble::add_row(word = c("nº", "n", "r"))


palavras <- tab_tjsp %>%
  dplyr::select(ementa) %>%
  tidytext::unnest_tokens(input = ementa, output = "word") %>%
  dplyr::anti_join(stop_words_pt) %>%
  dplyr::mutate(word = tm::removeNumbers(word)) %>%
  dplyr::filter(word != "") %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::filter(n > 50)

wordcloud2::wordcloud2(palavras)
