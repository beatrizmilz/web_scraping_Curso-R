library(xml2)

# ler o HTML
html <- xml2::read_html("exemplos_de_aula/html_exemplo.html")

# 1. Qual a diferença entre xml_find_all() e xml_find_first()?

xml2::xml_find_all(html, "//p")

xml2::xml_find_first(html, "//p")


# xml_find_all retorna todas as ocorrências encontradas,
# e xml_find_first retorna apenas a primeira ocorrência.


# 2. O que faz a função contains() aplicada dentro do XPath?
## Dica: xml_find_all(html, "//p[contains(@style, 'blue')]")


xml_find_all(html, "//p[contains(@style, 'blue')]")

# Funciona como um filtro nos atributos (nesse caso, filtrou os nodes em que
# no atributo style tem a property blue)

