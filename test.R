covid %>% 
    dplyr::filter(Ndept == "CORDOBA") %>% dplyr::arrange(Nmun) %>% dplyr::distinct(Nmun) 


covid %>% dplyr::distinct(Ndept)


covid %>% 
    dplyr::filter(Ndept == "CORDOBA", Nmun == "MONTERIA") %>% 
    dplyr::group_by(Recuperado) %>% 
    dplyr::summarise(mean = mean(Edad), n = n())

covid %>% 
    dplyr::filter(Ndept == "CORDOBA", Nmun == "MONTERIA", Recuperado == "Activo") %>% 
    dplyr::summarise(n = n())

covid %>% 
dplyr::filter(Ndept == "CORDOBA", Nmun == "MONTERIA", Recuperado %in% c("Fallecido", "fallecido"))




data_example <-covid %>% 
    dplyr::filter(Ndept == "CORDOBA", Nmun == "MONTERIA") %>% 
    dplyr::mutate(Recuperado = replace(Recuperado, Recuperado == "fallecido", "Fallecido")) %>% 
    dplyr::select(Fnoti, Recuperado) %>% tidyr::separate(Fnoti, c("Fecha", "Hora"), sep = " ", remove = TRUE) %>% 
    dplyr::select(-Hora)


data_e <- data_example %>% 
    group_by(Fecha, Recuperado) %>% 
    summarise(n = n()) %>% ungroup() %>% 
    spread(Recuperado, n) %>% mutate(Fecha = lubridate::dmy(Fecha)) %>% rowwise(Fecha) %>% 
    mutate(Positivos = sum(c(Recuperado, Activo), na.rm = T))

data_xts <- xts::xts(x = data_e[, c(2, 3, 5, 6)], order.by = data_e$Fecha)



data_e <- data_example %>% 
    group_by(Fecha, Recuperado) %>% 
    summarise(n = n()) %>% ungroup() %>% 
    spread(Recuperado, n) %>% mutate(Fecha = lubridate::dmy(Fecha)) %>% rowwise() %>% 
    mutate(Positivos = sum(c(Recuperado, Activo), na.rm = T)) %>% ungroup() %>% arrange(Fecha) %>% 
    mutate(Cum_sum = cumsum(Positivos), Cum_fal = cumsum(ifelse(!is.na(Fallecido), Fallecido, 0)), 
           Cum_rec = cumsum(ifelse(!is.na(Recuperado), Recuperado, 0)))

data_xts <- xts::xts(x = data_e[, 7:9], order.by = data_e$Fecha)

library(dygraphs)

dygraph(data_xts) %>% 
    dySeries("Cum_sum", label = "Positivos") %>%
    dySeries("Cum_fal", label = "Fallecidos") %>%
    dyOptions(stackedGraph = TRUE, colors = RColorBrewer::brewer.pal(4, "Set2")) %>%
    dyRangeSelector(height = 20)
    dyRangeSelector()


df <- tibble(x = 1:2, y = 3:4, z = 5:6)
df %>% rowwise()


###########################################################################################################
## Number of positive cases ----------------------------------------------

cmun <- 
    data %>% 
    dplyr::group_by(ciudad_municipio_nom) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(ciudad_municipio_nom) %>% 
    dplyr::mutate(NOM_MUNICI = sort(proj_Moj$NOM_MUNICI)) 
    



hist(cmun$n)$breaks 





# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse, leaflet)

# Load data ---------------------------------------------------------------
mps <- shapefile('Data/Maps/mpios_geo_ok.shp')
dpt <- aggregate(mps, 'NOMBRE_DPT')
#lbl <- data.frame(month_abb = month.abb, mes = 1:12)

# Selecting only Valle del Cauca ------------------------------------------
mps <- mps[mps@data$NOMBRE_DPT == 'CÓRDOBA',]



proj_Moj <- spTransform(mps, CRS("+init=epsg:4326"))

#ciudad_municipio_nom
# 
# data_cor <-  as.tibble(proj_Moj) %>% 
#     dplyr::left_join(cmun, by = c('NOM_MUNICI' = 'ciudad_municipio_nom'))

newobj <- merge(proj_Moj, cmun, by.x = "NOM_MUNICI", by.y = "NOM_MUNICI")




###############################################################

bins <- round(quantile(cmun$n), 0)

pal <- colorBin("YlOrRd", domain = proj_Moj$n, bins = bins)

binpal <- colorBin("YlOrRd", proj_Moj$n, 6, pretty = FALSE)

labels <- sprintf(
    "<strong>%s</strong><br/>%g Casos confirmados",
    newobj$NOM_MUNICI, newobj$n
) %>% lapply(htmltools::HTML)

newobj %>%
    leaflet() %>%
    addProviderTiles("CartoDB.Positron", group = "OSM (default)") %>%
    addPolygons(
        fillColor = ~ pal(n),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7, 
        highlight = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE), 
        label = labels,
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
    ) %>%
    addLegend(
        pal = pal,
        values = ~ n,
        opacity = 0.7,
        title = NULL,
        position = "topright"
    )



# 
# fluidRow(
#     valueBoxOutput("box_con"),
#     valueBoxOutput("box_rec" ),
#     valueBoxOutput("box_act"),
#     valueBoxOutput("box_fall")
#     
# ), 
# fluidRow(
#     column(
#         width = 10,
#         box(width = NULL, solidHeader = FALSE, 
#             dygraphOutput("CasosD", height = "300px")
#         )),
#     column(
#         width = 2,
#         filterModuleUI("general")
#     )
# )



# 
# ######################## Filtro 
# callModule(filterModuleServer, "general", data = Dcovid)
# 
# ####################### ValueBox 
# fil_data <- reactive({
#     Dcovid %>% dplyr::filter(Ndept == input$"general-selectDept", Nmun == input$"general-selectMun")
# })
# 
# output$box_confirmados <- renderValueBox({
#     shinydashboard::valueBox(
#         value = fil_data() %>%
#             dplyr::select(Recuperado) %>%
#             dplyr::summarise(n = n()),
#         subtitle = "Confirmados",
#         icon = icon("fas fa-users"),
#         color = "fuchsia", 
#         width = 3
#     )
# })
# 
# output$box_recuperados <- renderValueBox({
#     shinydashboard::valueBox(
#         value = fil_data() %>%
#             dplyr::filter(Recuperado == "Recuperado") %>%
#             dplyr::summarise(n = n()),
#         subtitle = "Recuperados",
#         icon = icon("fas fa-user-check"),
#         color = "green", 
#         width = 3    
#     )
# })
# 
# output$box_activos <- renderValueBox({
#     shinydashboard::valueBox(
#         value = fil_data() %>%
#             dplyr::filter(Recuperado == "Activo") %>%
#             dplyr::summarise(n = n()),
#         subtitle = "Activos",
#         icon = icon("fas fa-hospital-user"),
#         color = "orange",
#         width = 3  
#     )
# })
# 
# output$box_fallecidos <- renderValueBox({
#     shinydashboard::valueBox(
#         value = fil_data() %>%
#             dplyr::filter(Recuperado %in% c("Fallecido", "fallecido")) %>%
#             dplyr::summarise(n = n()),
#         subtitle = "Fallecidos",
#         icon = icon("fas fa-ribbon"),
#         color = "purple",
#         width = 3  
#     )
# })
# 
# output$CasosD <- renderDygraph({
#     
#     data_xts <- fil_data() %>% 
#         dplyr::mutate(Recuperado = replace(Recuperado, Recuperado == "fallecido", "Fallecido")) %>% 
#         dplyr::select(Fnoti, Recuperado) %>% 
#         tidyr::separate(Fnoti, c("Fecha", "Hora"), sep = " ", remove = TRUE) %>% 
#         dplyr::select(-Hora) %>% 
#         dplyr::group_by(Fecha, Recuperado) %>% 
#         dplyr::summarise(n = n()) %>% dplyr::ungroup() %>% 
#         tidyr::spread(Recuperado, n) %>% 
#         dplyr::mutate(Fecha = lubridate::dmy(Fecha)) %>% 
#         dplyr::rowwise(Fecha) %>% 
#         dplyr::mutate(Positivos = sum(c(Recuperado, Activo), na.rm = T))
#     
#     data_xts <- xts::xts(x = data_xts[, c(2, 3, 5, 6)], order.by = data_xts$Fecha)
#     
#     dygraph(data_xts) %>% 
#         dySeries("Positivos", label = "Positivos") %>%
#         dySeries("Recuperado", label = "Recuperados") %>%
#         dySeries("Fallecido", label = "Fallecidos") %>%
#         dySeries("Activo", label = "Activos") %>%
#         dyOptions(stackedGraph = TRUE, colors = RColorBrewer::brewer.pal(4, "Set2")) %>%
#         dyRangeSelector(height = 20)
# })






