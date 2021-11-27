#Nombres
# intial configuration
if (!require("pacman")) install.packages("pacman") # Isntalar pacman (sino estÃ¡ instalada)
require(pacman) # llamar pacman
p_load(tidyverse,viridis,sf,leaflet, rio, skimr) # llamar y/o instalar librerias
p_load(broom, # tidy-coefficients
       mfx, # marginal effects
       margins,  # marginal effects
       estimatr, # robust standard errors
       lmtest, # HAC (Newey-West) standard errors
       fixest, # hdfe regressions (feols)
       modelsummary, # Coefplot with modelplot
       stargazer)

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

lapply(all, skim)
lapply(all, st_crs)
lapply(all, st_bbox)
all = lapply(all, st_transform, crs=crs0)

leaflet() %>% addTiles() %>% addCircleMarkers(data = puntos)


#############
#  Punto 2  #
#############}

mapmuse$dist_vias = st_distance(mapmuse, via)
mapmuse$dist_cmedico = st_distance(mapmuse, c_medico)
mapmuse$dist_cpoblado = st_distance(mapmuse, c_poblado)
mapmuse = mapmuse %>% mutate(fallecido=if_else(estado=="Muerto", 1, 
                                               if_else(estado=="Herido", 0, NULL)))

mapmuse = mapmuse %>% dplyr::select(!c("estado", "month", "geometry"))
export(mapmuse, "data/output/mapmuse.rds")

# Importamos la base de datos ya tratada

mapmuse = import("data/output/mapmuse.rds")

# Para no demorme corriendolo voy a hacer un subset
mapmuse = mapmuse %>% filter(runif(nrow(mapmuse)) < 0.05)


# Darme cuenta de cómo funcionaba el vector de units fue un dolor de cabeza
# No me dejaba hacer las regresiones bien
units_to_num = function(v) {
  n = nrow(v)
  return(as.numeric(v[1:n]))
}
mapmuse = mapmuse %>% mutate(across(!starts_with("dist")&!fallecido, factor))
mapmuse = mapmuse %>% mutate(across(starts_with("dist"), units_to_num))


# Corremos las tres regresiones
ols = lm(fallecido ~ dist_cpoblado + dist_cmedico, data=mapmuse)
logit = glm(fallecido  ~ dist_cpoblado + dist_cmedico, data=mapmuse, family=binomial(link="logit"))
probit = logit = glm(fallecido  ~ dist_cpoblado + dist_cmedico, data=mapmuse, 
                     family=binomial(link="probit"))

logit_marg = margins(logit)
probit_marg = margins(probit)


mods = list('Logit' = logit , 'Probit' = probit , "OLS" = ols)
modelplot(mods) + coord_flip() + 
  labs(title = "Probabilidad de fallecer")

# Tabla conjunta (hay que hacerla con outreg eventualmente)
msummary(mods)



#outreg(mods)
#msummary(ols)
#glance(probit)

# Tablita con el tamaño de los efectos marginales
logit_marg %>% tidy(conf.int = TRUE)
probit_marg %>% tidy(conf.int = TRUE)




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
