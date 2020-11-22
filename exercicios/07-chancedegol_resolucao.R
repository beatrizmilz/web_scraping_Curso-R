library(magrittr)



# 1. Obtenha o vetor com as probabilidades dos resultados que realmente aconteceram
## Dica: qual é a cor deles?

# Júlio fez na aula

u_cdg <- "http://www.chancedegol.com.br/br19.htm"
cdg_html <- httr::GET(u_cdg)

cdg_table <- cdg_html  %>%
  xml2::read_html() %>%
  xml2::xml_find_first('//table') %>%
  rvest::html_table() %>%
  tibble::as_tibble() %>%
  set_names(janitor::make_clean_names(.[1, ])) %>%
  dplyr::slice(-1)


cores <- cdg_html %>%
  xml2::read_html() %>%
  xml2::xml_find_all('//font[@color="#FF0000"]') %>%
  xml2::xml_text()

# 2. Construa a tabela final e armazene em tab_cdg
## Dica: utilize rvest::html_table() e adicione a coluna cores

# Júlio fez na aula

cdg_table$vermelho <- cores


# Arrumar a base
tab_cdg <- cdg_table %>%
  dplyr::rename("resultado" = x) %>%
  dplyr::mutate(
    data = readr::parse_date(data, format = "%d/%m/%Y"),
    dplyr::across(.cols = vitoria_do_mandante:vermelho, .fns = readr::parse_number)
  )





# 3. Rode tibble::glimpse(tab_cdg)
## e cole o resultado abaixo. Deixe o resultado como um comentário
## Dica: Selecione as linhas que deseja comentar e aplique Ctrl+Shift+C

tibble::glimpse(tab_cdg)

# > tibble::glimpse(tab_cdg)
# Rows: 376
# Columns: 8
# $ data                 <date> 2019-04-27, 2019-04-27, 2019-04-27, 2019-04-27, 2019-04-28, 2019-04-28, 2019-04-28, 20...
# $ mandante             <chr> "Atlético MG", "São Paulo", "Flamengo", "Chapecoense", "Grêmio", "Ceará", "Palmeiras", ...
# $ resultado            <chr> "2x1", "2x0", "3x1", "2x0", "1x2", "4x0", "4x0", "0x1", "3x2", "4x1", "2x1", "1x0", "2x...
# $ visitante            <chr> "Avaí", "Botafogo", "Cruzeiro", "Internacional", "Santos", "CSA", "Fortaleza", "Goiás",...
# $ vitoria_do_mandante  <dbl> 54.6, 56.9, 51.7, 24.2, 60.4, 56.3, 69.4, 51.3, 47.0, 53.3, 33.3, 57.4, 34.0, 58.2, 36....
# $ empate               <dbl> 24.5, 27.8, 25.1, 31.3, 23.7, 26.2, 24.3, 24.7, 32.5, 24.2, 29.4, 24.2, 25.9, 27.6, 26....
# $ vitoria_do_visitante <dbl> 20.9, 15.2, 23.2, 44.6, 15.8, 17.5, 6.3, 23.9, 20.5, 22.5, 37.3, 18.3, 40.1, 14.2, 37.4...
# $ vermelho             <dbl> 54.6, 56.9, 51.7, 24.2, 15.8, 56.3, 69.4, 23.9, 47.0, 53.3, 33.3, 57.4, 34.0, 58.2, 37....

# 4. [extra] Construa um gráfico que mostra qual é a proporção
# de acertos do Chance de Gol por time. Os passos são
# a) obter qual seria o chute do Chance de Gol, dado pelo resultado com
# maior probabilidade em cada jogo
# b) construir uma coluna "acertou", que é TRUE se o modelo acertou
# e FALSE caso contrário

tab_cdg_acerto <- tab_cdg %>%
  dplyr::mutate(
    chute = dplyr::case_when(
      vitoria_do_mandante > vitoria_do_visitante  &
        vitoria_do_mandante > empate ~ "vitória do mandante",
      vitoria_do_visitante > vitoria_do_mandante  &
        vitoria_do_visitante > empate ~ "vitória do visitante",
      empate > vitoria_do_mandante  &
        empate > vitoria_do_visitante ~ "empate"
    ),

    resultado_final = dplyr::case_when(
      vermelho == vitoria_do_mandante ~ "vitória do mandante",
      vermelho == vitoria_do_visitante ~ "vitória do visitante",
      vermelho == empate ~ "empate",
    )
  ) %>%
  dplyr::mutate(acertou = chute == resultado_final)


# c) empilhar a base (usar tidyr::gather ou tidyr::pivot_longer) para considerar
# tanto mandantes quanto visitantes
# d) agrupar por time e calcular a proporção de acertos. Ordenar a variável
# pela proporção de acertos
# e) montar o gráfico!

library(ggplot2)

tab_cdg_acerto %>% tidyr::pivot_longer(
  cols = c(mandante, visitante),
  names_to = "mandante_ou_visitante",
  values_to = "time"
) %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(
    total_acertos = sum(acertou),
    total_jogos = dplyr::n(),
    proporcao_acertos = total_acertos / total_jogos
  ) %>%
  dplyr::mutate(time = forcats::fct_reorder(time, proporcao_acertos)) %>%
  ggplot() +
  geom_col(aes(x = proporcao_acertos, y = time), fill = "lightblue") +
  theme_bw() +
  labs(y = "Time", x = "Proporção de acertos") +
  scale_x_continuous(labels = scales::percent)

# alterei algumas coisas no gráfico depois da correção da aula!
