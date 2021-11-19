#Nombres
# intial configuration
rm(list = ls()) # limpia el entorno de R
require(pacman)
p_load(tidyverse,data.table,plyr,XML,rvest,xml2) # carga





#############
#  Punto 3  #
#############

#3.1
browseURL(url = 'https://es.wikipedia.org/wiki/Departamentos_de_Colombia',browser = getOption('browser'))

html_wiki = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
html_wiki = read_html(html_wiki)
class(html_wiki)

#3.2
titulo = html_wiki %>% html_nodes(xpath = '//*[@id="firstHeading"]') %>% html_text() # Convertir en texto
titulo

#3.3
tabla_departamentos = html_wiki %>% html_nodes('table') 
tabla_departamentos

#econtramos la tabla que tiene los departamentos
tabla_departamentos[4] %>% html_table(header = T,fill=T)

departamentos = tabla_departamentos[4] %>% html_table(header = T,fill=T)  %>% as.data.frame()
