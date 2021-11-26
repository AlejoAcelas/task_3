#Nombres
# intial configuration
if (!require("pacman")) install.packages("pacman") # Isntalar pacman (sino está instalada)
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
#############
mapmuse$dist_vias = st_distance(mapmuse, via)
mapmuse$dist_cmedico = st_distance(mapmuse, c_medico)
mapmuse$dist_cpoblado = st_distance(mapmuse, c_poblado)
mapmuse = mapmuse %>% mutate(fallecido=if_else(estado=="Muerto", 1, 
                                               if_else(estado=="Herido", 0, NULL)))

mapmuse = mapmuse %>% dplyr::select(!c("estado", "month", "geometry"))
export(mapmuse, "data/output/mapmuse.rds")

# Importamos la base de datos ya tratada

mapmuse = import("data/output/mapmuse.rds")

# mapmuse = mapmuse %>% subset(runif(nrow(mapmuse)) < 0.1)

mapmuse = mapmuse %>% mutate(across(!starts_with("dist"), factor))


# Corremos las tres regresiones
ols = lm(fallecido ~ ., data=mapmuse)
logit = glm(fallecido ~ ., data=mapmuse, family=binomial(link="logit"))
probit = logit = glm(fallecido ~ ., data=mapmuse, family=binomial(link="probit"))

mods = list('Logit' = logit , 'Probit' = probit , "OLS" = ols)
outreg(mods)

msummary(ols)

logit_marg = margins(logit)
probit_marg = margins(probit)



modelplot(mods) + coord_flip() + 
  labs(title = "Probability to pay with credit card" , subtitle = "Comparing models")


# marginal effects

logit_marg %>% tidy(conf.int = TRUE)

probit_marg %>% tidy(conf.int = TRUE)


# joint models (modelsummary)
msummary(list(ols, ols2 , ols_robust , ols_stata , ols_hac))

# export table
stargazer(ols, ols2,
          type= 'text',
          dep.var.labels = c('','Number of flights',''), 
          df = FALSE,
          digits = 3, 
          out = paste0('data_10/output/ols.text'))

# coefplot
mods = list('Logit' = logit_marg , 'Probit' = probit_marg , "OLS" = ols_lineal)



tidy(ols, conf.int = TRUE)
glance(ols)



ggplot() + geom_sf(data=boston , col="black" , aes(fill=dist_CBD)) + 
  scale_fill_viridis(option="A" , alpha=0.9 , direction=-1 , name="Dist. CBD (miles)") +
  geom_sf(data=boston_cbd , col = "green" , size = 5) + theme_bw()

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
