#############################################################################################################

# Load the httr package
library(httr)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(tidyjson)

# Create list with nationality and country elements
#query_params <- list(departamento_nom = "CORDOBA", LIMIT = NULL)
api_url <- "https://www.datos.gov.co/resource/gt2j-8ykr.json?departamento_nom=CORDOBA&$limit=1755000"

# Make parameter-based call to httpbin, with query_params
resp <- GET(api_url)

status_code(resp)
#headers(resp)

########## Query con método GET 

covid_cor <- jsonlite::fromJSON(content(resp, as = "text"), flatten = TRUE) 

##########################################################################################################


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse, leaflet)

# Load data for map--------------------------------------------------------
mps <- shapefile('Data/Maps/mpios_geo_ok.shp')
mps <- mps[mps@data$NOMBRE_DPT == 'CÓRDOBA',]
map_cor <- spTransform(mps, CRS("+init=epsg:4326"))


##################################################################

casos_cor <- 
    covid_cor %>% 
    dplyr::group_by(ciudad_municipio_nom) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(ciudad_municipio_nom) %>% 
    dplyr::mutate(NOM_MUNICI = sort(map_cor$NOM_MUNICI))

map_cor <- merge(map_cor, casos_cor, by.x = "NOM_MUNICI", by.y = "NOM_MUNICI")






























# 
# library(RSocrata)
# library(readr)
# library(dplyr)
# 
# Dcovid <- read.socrata(
#     url = "https://www.datos.gov.co/resource/gt2j-8ykr.json",
#     app_token = "BNXrxzfOpI09SItb52Bx3nl7c",
#     email     = "klpc2116@gmail.com",
#     password  = "Msckevin1064*"
# ) 
# 
# 
# 
# 
# name <- c(
#         "Fweb",
#         "Id",
#         "Fnoti",
#         "Codept",
#         "Ndept",
#         "Codmun",
#         "Nmun",
#         "Edad",
#         "Uedad",
#         "Sexo",
#         "Tipo_cont",
#         "Ubi_caso",
#         "Estado",
#         "ISO_pais",
#         "Npais",
#         "Recuperado",
#         "FiniSin",
#         "Fmuerte",
#         "Fdiag",
#         "Frecu",
#         "Tipo_recu",
#         "Petnica",
#         "Netnico"
#     )
# 
# colnames(Dcovid) <- name

