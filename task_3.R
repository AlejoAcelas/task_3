#Nombres
# intial configuration
if (!require("pacman")) install.packages("pacman") # Isntalar pacman (sino estÃ¡ instalada)
require(pacman) # llamar pacman
p_load(tidyverse,viridis,sf,leaflet, rio, skimr) # llamar y/o instalar librerias
p_load(broom, # tidy-coefficients
       margins,  # marginal effects
       modelsummary, # Coefplot with modelplot
       stargazer,
       rockchalk, htmltools)

#############
#  Punto 1  #
#############

#1.1.1
via = st_read("data/input/VIAS.shp")

puntos = st_read("data/input/MGN_URB_TOPONIMIA.shp")

#1.1.2
c_medico = puntos %>% filter(CSIMBOL %in% c("021001", "021002", "021003"))

#1.1.3
c_poblado = import("data/input/c poblado (2017).rds") %>%  
  filter(cod_dane >= 54001, cod_dane < 55000)

depto = import("data/input/dp deptos (2017).rds") %>% 
  filter(name_dpto=="NORTE DE SANTANDER")

mapmuse = import("data/input/victimas_map-muse.rds")

#1.2-1.3.2
all = list(via, puntos, c_medico, c_poblado, depto, mapmuse)
names(all) = c("via", "puntos", "c_medico", "c_poblado", "depto", "mapmuse")

crs0 = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs"

####lapply(all, skim)
lapply(all, st_crs)
lapply(all, st_bbox)
all = lapply(all, st_transform, crs=crs0)
leaflet() %>% addTiles() %>% addCircleMarkers(data = puntos)

#1.4.1
mapmuse=st_transform(mapmuse,crs=4326)
depto=st_transform(depto,crs=4326)
mapmuse_depto=st_intersection(mapmuse, depto)

#1.4.2

cp_n=c_poblado%>%subset(codmpio==54820)
ggplot() + geom_sf(data=cp_n , col = "red", fill=NA ) + geom_sf(data=via,col="blue")
#linea 51 sobre vias y municipios
st_length(st_intersection(via, cp_n))%>%sum

#1.5.1
leaflet(depto) %>% addTiles() %>% addPolygons(fillColor="yellow",fill="green",weight=2) #falta anadir los centros poblados


#############
#  Punto 2  #
#############}


# 2.0
mapmuse$dist_vias = st_distance(mapmuse, via)
mapmuse$dist_cmedico = st_distance(mapmuse, c_medico)
mapmuse$dist_cpoblado = st_distance(mapmuse, c_poblado)
mapmuse = mapmuse %>% mutate(fallecido=if_else(estado=="Muerto", 1, 
                                               if_else(estado=="Herido", 0, NULL)))

mapmuse = mapmuse %>% dplyr::select(!c("estado", "month", "geometry"))
export(mapmuse, "data/output/mapmuse.rds")

# 2.1

# Importamos la base de datos ya tratada

mapmuse = import("data/output/mapmuse.rds")

# Para no demorme corriendolo voy a hacer un subset
# mapmuse = mapmuse %>% filter(runif(nrow(mapmuse)) < 0.05)


# Darme cuenta de cómo funcionaba el vector de units fue un dolor de cabeza
# No me dejaba hacer las regresiones bien
units_to_num = function(v) {
  n = nrow(v)
  return(as.numeric(v[1:n]))
}

mapmuse = mapmuse %>% mutate(across(!starts_with("dist")&!fallecido, factor))
mapmuse = mapmuse %>% mutate(across(starts_with("dist"), units_to_num))

ols = lm(fallecido ~ ., data=mapmuse)

# 2.2

graph1 = modelplot(ols) + labs(title = "Efecto en la probabilidad de fallecer")
graph1
ggsave("views/coef_plot_ols.jpeg", graph1)

# Esa tabla incluye demasiado así que haré una más chiquita
vars = ols$coefficients %>% names()
to_exclude = grep("cod|year", vars)
vars = vars[-to_exclude]
graph4 = modelplot(ols, coef_map=vars) + labs(title = "Efecto en la probabilidad de fallecer")
graph4
ggsave("views/coef_plot_ols2.jpeg", graph4)

# 2.3

logit = glm(fallecido  ~ ., data=mapmuse, family=binomial(link="logit"))
probit = logit = glm(fallecido  ~ ., data=mapmuse, 
                     family=binomial(link="probit"))


# 2.4

mods = list('Logit' = logit , 'Probit' = probit , "OLS" = ols)
table1 = outreg(mods, type="html")
cat(table1, flie="views/Tabla_conjunta.html")

# 2.5

logit_marg = margins(logit)
probit_marg = margins(probit)

graph2 = modelplot(logit_marg, coef_map = "dist_cmedico") + 
  labs(title = "Efecto en la probabilidad de fallecer")
graph2
ggsave("views/coef_plot_logit.jpeg", graph2)

graph3 = modelplot(probit_marg, coef_map = "dist_cmedico") + 
  labs(title = "Efecto en la probabilidad de fallecer")
graph3
ggsave("views/coef_plot_probit.jpeg", graph3)



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
