library(tidyverse)
library(httr)
library(rvest)

u_cdg <- "http://www.chancedegol.com.br/br19.htm"

cdg_html <- GET(u_cdg)


cdg_table <- cdg_html  %>%
  read_html() %>%
  xml_find_first('//table') %>%
  html_table() %>%
  tibble::as_tibble() %>%
  set_names(janitor::make_clean_names(.[1,])) %>%
  slice(-1)


cores <- cdg_html %>%
  read_html() %>%
  xml_find_all('//font[@color="#FF0000"]') %>%
  xml_text()


cdg_table$vermelho <- cores


cdg_table
