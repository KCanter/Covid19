library(dplyr)
library(shiny)
library(shinydashboard)
library(lubridate)

##############################################################################################################

MapModuleUI <- function(id) {
    ns <- NS(id)
    shiny::fluidRow(
    shinydashboard::box(
        width = NULL,
        solidHeader = F,
        leaflet::leafletOutput(ns("map_cor"), height = 900)
    ))
}

##############################################################################################################

MapModuleSR <- function(input, output, session, data) {
    
    
    ## Se crean los labels para los municipios 
    labels <-
        sprintf("<strong>%s</strong><br/>%g Casos confirmados",
                data$NOM_MUNICI,
                data$n) %>% lapply(htmltools::HTML)
    
    ## Se crea la paleta de colores 
    bins <- round(quantile(data$n), 0)
    pal <- colorBin("YlOrRd", domain = data$n, bins = bins)
    
    output$map_cor <- renderLeaflet({
        data %>%
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
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addLegend(
                pal = pal,
                values = ~ n,
                opacity = 0.7,
                title = NULL,
                position = "topright"
            )
    })
    
    
    
}


