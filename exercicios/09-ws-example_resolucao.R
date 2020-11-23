# 1. Adicione tratamento de erros e barras de progresso na função
# de baixar uma página

# http://example.webscraping.com

# 2. Rode a função de baixar páginas em paralelo

# 3. Verifique se todos os arquivos baixaram corretamente
# Obs: Pode ser que seu IP tenha sido bloqueado!

# 4. Como você resolveria esse problema?


# JÚLIO RESOLVEU ESSE EXERCÍCIO NA AULA 4!
# Cópia do código da aula 4:

library(magrittr)

u <- "http://example.webscraping.com"

# login

u_login <-
  "http://example.webscraping.com/places/default/user/login"
r_login <- httr::GET(u_login)

form_key <- r_login %>%
  xml2::read_html() %>%
  xml2::xml_find_first("//*[@name='_formkey']") %>%
  xml2::xml_attr("value")

# abjutils::chrome_to_body()

# Meu .Renviron não está sendo lido! Tive que passar o path.

path_to_Renviron <- "C:/Users/beatr/Documents/.Renviron"
readRenviron(path_to_Renviron)


body <- list(
  "email" = Sys.getenv("WS_LOGIN"),
  "password" = Sys.getenv("WS_PWD"),
  "_next" = "/places/default/index",
  "_formkey" = form_key,
  "_formname" = "login"
)

r_logado <- httr::POST(u_login,
                       body = body,
                       httr::write_disk("exemplos_de_aula/ws_site.html", overwrite = TRUE))



# baixar uma pagina

baixar_pagina <- function(pag, pasta) {
  arquivo <- paste0(pasta, "/", pag, ".html")

  if (!file.exists(arquivo)) {
    u_pag <-
      paste0("http://example.webscraping.com/places/default/index/",
             pag - 1)

    httr::GET(u_pag, httr::write_disk(arquivo, overwrite = TRUE))

    arquivo
  }



}

maybe_baixar_pagina <- purrr::possibly(baixar_pagina, "erro")


maybe_baixar_pagina_progresso <- function(pag, pasta, prog) {
  if (!missing(prog)) prog()
  Sys.sleep(1)
  maybe_baixar_pagina(pag, pasta)
}

future::plan(future::sequential())

progressr::with_progress({
  paginas <- 1:26
  p <- progressr::progressor(length(paginas))
  furrr::future_map(paginas,
                    maybe_baixar_pagina_progresso,
                    pasta = "exemplos_de_aula/ws_site",
                    prog = p)
})





# Vou avançar no exemplo de classe: scrapear uma pagina, baixar um pais, scrapear um pais

# Buscar a URL dos países para baixar!
ler_pagina <- function(pag, pasta) {
  `%>%` <- magrittr::`%>%`

  html <-  paste0(pasta, '/', pag, ".html")

  results <- xml2::read_html(html) %>%
    xml2::xml_find_all("//div[@id='results']")

  url_paises <- results %>%
    xml2::xml_find_all("//table//a") %>%
    xml2::xml_attr("href") %>%
    paste0("http://example.webscraping.com", .)

  nomes_paises <- results %>%
    xml2::xml_find_all("//table//a") %>%
    xml2::xml_text("a") %>%
    stringr::str_squish()

  df <- tibble::tibble(nome = nomes_paises, link = url_paises)

  df
}


paises <-  purrr::map2_df(.x = 1:26, .y =   "exemplos_de_aula/ws_site", .f = ler_pagina)

readr::write_rds(paises, "exercicios/ws_paises.rds")

# Já temos uma base com o nome dos países, e a URL para baixar!

paises <- readr::read_rds("exercicios/ws_paises.rds")

# Adaptei a função que o Julio fez na aula

baixar_pagina_pais <- function(n_linha, df, pasta) {
  arquivo <- paste0(pasta, "/", df$nome[n_linha], ".html")

  if (!file.exists(arquivo)) {
    u_pag <- df$link[n_linha]

    httr::GET(u_pag, httr::write_disk(arquivo, overwrite = TRUE))

    arquivo
  }
}

# testando se funciona, funciona :)
# baixar_pagina_pais(1, paises, "exercicios/ws_paises")




maybe_baixar_pagina_pais <-
  purrr::safely(baixar_pagina_pais, "erro")

# testando se funciona, funciona :)
# maybe_baixar_pagina_pais(1,paises, "exercicios/ws_paises")



maybe_baixar_pagina_pais_progresso <-
  function(n_linha, df, pasta,  prog) {
    if (!missing(prog)) prog()
    Sys.sleep(1)
    maybe_baixar_pagina_pais(n_linha, df, pasta)
  }

# testando se funciona, funciona :)
# maybe_baixar_pagina_pais_progresso(1, paises, "exercicios/ws_paises")

# Acabei fazendo apenas com purrr, e sem barra de progresso pq eu to errando algo no código abaixo.
# AQUI BAIXA O HTML DE CADA PAÍS.
purrr::map(
  .x =  1:nrow(paises),
  .f = maybe_baixar_pagina_pais_progresso,
  df = paises,
  pasta = "exercicios/ws_paises"
)


# ABAIXO: Tentei adaptar do código da aula porém ele executa,
# não funciona e dá erro.
# Preciso debugar.


# future::plan(future::sequential())
#
#
# progressr::with_progress({
#   paginas <- 1:nrow(paises)
#   p <- progressr::progressor(nrow(paises))
#   furrr::future_map(paginas,
#                     .f = maybe_baixar_pagina_pais_progresso,
#                     df = paises,
#                     pasta = "exercicios/ws_paises/",
#                     prog = p)
# })



# Agora é preciso abrir os html e transformar os dados em uma tibble


sites_paises <-
  list.files("exercicios/ws_paises/", full.names = TRUE)

ler_pais <- function(file) {
  df <- file %>% xml2::read_html() %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    tibble::as_tibble() %>%
    dplyr::select(-X3) %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      area_sq_km = readr::parse_number(area),
      population = readr::parse_number(population)
    ) %>%
    dplyr::relocate(area_sq_km, .after = area) %>%
    dplyr::select(-national_flag, -area)


  df
}


df_paises <- purrr::map_dfr(.x = sites_paises, .f = ler_pais)

readr::write_rds(df_paises, "exercicios/df_paises_completo.Rds")

# > dplyr::glimpse(df_paises)
# Rows: 252
# Columns: 14
# $ area_sq_km         <dbl> 647500, 1580, 28748, 2381740, 199, 468, 1246700, 102, 14000000, 443, 2766890, 29800, 193, 7686850,...
# $ population         <dbl> 29121286, 26711, 2986952, 34586184, 57881, 84000, 13068161, 13254, 0, 86754, 41343201, 2968000, 71...
# $ iso                <chr> "AF", "AX", "AL", "DZ", "AS", "AD", "AO", "AI", "AQ", "AG", "AR", "AM", "AW", "AU", "AT", "AZ", "B...
# $ country            <chr> "Afghanistan", "Aland Islands", "Albania", "Algeria", "American Samoa", "Andorra", "Angola", "Angu...
# $ capital            <chr> "Kabul", "Mariehamn", "Tirana", "Algiers", "Pago Pago", "Andorra la Vella", "Luanda", "The Valley"...
# $ continent          <chr> "AS", "EU", "EU", "AF", "OC", "EU", "AF", NA, "AN", NA, "SA", "AS", NA, "OC", "EU", "AS", NA, "AS"...
# $ tld                <chr> ".af", ".ax", ".al", ".dz", ".as", ".ad", ".ao", ".ai", ".aq", ".ag", ".ar", ".am", ".aw", ".au", ...
# $ currency_code      <chr> "AFN", "EUR", "ALL", "DZD", "USD", "EUR", "AOA", "XCD", "", "XCD", "ARS", "AMD", "AWG", "AUD", "EU...
# $ currency_name      <chr> "Afghani", "Euro", "Lek", "Dinar", "Dollar", "Euro", "Kwanza", "Dollar", "", "Dollar", "Peso", "Dr...
# $ phone              <chr> "93", "+358-18", "355", "213", "+1-684", "376", "244", "+1-264", "", "+1-268", "54", "374", "297",...
# $ postal_code_format <chr> "", "#####", "", "#####", "", "AD###", "", "", "", "", "@####@@@", "######", "", "####", "####", "...
# $ postal_code_regex  <chr> "", "^(?:FI)*(\\d{5})$", "", "^(\\d{5})$", "", "^(?:AD)*(\\d{3})$", "", "", "", "", "^([A-Z]\\d{4}...
# $ languages          <chr> "fa-AF,ps,uz-AF,tk", "sv-AX", "sq,el", "ar-DZ", "en-AS,sm,to", "ca", "pt-AO", "en-AI", "", "en-AG"...
# $ neighbours         <chr> "TM CN IR TJ PK UZ", "", "MK GR CS ME RS XK", "NE EH LY MR TN MA ML", "", "ES FR", "CD NA ZM CG", ...
