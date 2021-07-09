library(dplyr)
library(shiny)
library(shinydashboard)

AnalyticModuleUI <- function(id, data) {
    ns <- NS(id)
    
    shiny::fluidRow(
        valueBoxOutput(ns("Prev_dep")),
        valueBoxOutput(ns("Prev_mun")),
        valueBoxOutput(ns("Inc_dep")),
        valueBoxOutput(ns("Inc_mun")),
        
        shinydashboard::box(
            solidHeader = T,
            width = 10,
            dygraphOutput(ns("chance_rate")),
            plotOutput(ns("rate_edad_sexo"))
        ),
        
        shinydashboard::box(
            solidHeader = T,
            width = 2,
            selectizeInput(
                inputId = ns("Mun"),
                label = "Municipio",
                choices = c(
                    "Escoja" = "",
                    data %>%
                        dplyr::distinct(ciudad_municipio_nom) %>%
                        dplyr::arrange(ciudad_municipio_nom) %>%
                        dplyr::pull()
                ),
                selected = "MONTERIA",
                options = list(create = TRUE)
            ),
            dateRangeInput(
                ns("fechas_analytics"),
                label = "Rango de fechas",
                start = "2020-03-25",
                end = "2021-06-30",
                startview = "month"
            )
        )
        
        
        
    )
    
}



AnalyticModuleSR <- function(input, output, session, data) {
    
    
    fil_data <- reactive({
        data %>%
            tidyr::separate(
                fecha_de_notificaci_n,
                c("Fecha", "Hora"),
                sep = " ",
                remove = TRUE
            ) %>%
            dplyr::mutate(Fecha = lubridate::dmy(Fecha),
                recuperado = replace(recuperado, recuperado == "fallecido", "Fallecido")) %>%
            dplyr::filter(ciudad_municipio_nom == input$Mun,
                Fecha >= input$fechas_analytics[1] & Fecha <= input$fechas_analytics[2]
            )
    })
    
    output$Prev_dep <- renderValueBox({
        
        Prev <- data %>%
            dplyr::mutate( recuperado = replace(recuperado, recuperado == "fallecido", "Fallecido")) %>%
            dplyr::group_by(recuperado) %>%
            dplyr::summarise(n = n()) %>%
            dplyr::mutate(Prev = n / sum(n)) %>% 
            dplyr::filter(recuperado == "Fallecido") %>% 
            dplyr::select(Prev) %>% 
            dplyr::pull()
        
        shinydashboard::valueBox(
            value = round(Prev, 3),
            subtitle = paste("Prevalencia en el Departamento:",round(Prev*10000, 0), "por 10000 Hab."),
            icon = icon("fas fa-ribbon"),
            color = "purple",
            width = 3
        )
    })
    
    output$Prev_mun <- renderValueBox({
        
        Prev <- fil_data() %>%
            dplyr::group_by(recuperado) %>%
            dplyr::summarise(n = n()) %>%
            dplyr::mutate(Prev = n / sum(n)) %>% 
            dplyr::filter(recuperado == "Fallecido") %>% 
            dplyr::select(Prev) %>% 
            dplyr::pull()
        
        shinydashboard::valueBox(
            value = round(Prev, 3),
            subtitle = paste("Prevalencia en el Municipio:", round(Prev * 1000, 0), "por 1000 Hab."),
            icon = icon("fas fa-ribbon"),
            color = "purple",
            width = 3
        )
    })
    
    output$chance_rate <- renderDygraph({
        crate <- 
        fil_data() %>% 
            dplyr::group_by(Fecha) %>% 
            dplyr::summarise(ncasos = n()) %>% 
            dplyr::mutate(ch_rate = (ncasos - lag(ncasos))*100/lag(ncasos))
        
        
        data_xts <- xts::xts(x = crate[, 3], order.by = crate$Fecha)
        
        dygraph(data_xts) %>%
            dySeries("ch_rate", label = "Variación en la tasa de contagios") %>%
            dyHighlight(highlightCircleSize = 3, 
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE) %>% 
            dyAxis("y", label = "Porcentaje de la variación de los casos") %>%
            dyLegend(width = 400) %>% 
            dyRangeSelector(height = 20)
        
        
    })
    
    output$rate_edad_sexo <- renderPlot({
        
        fil_data() %>% 
            dplyr::mutate(Fecha = lubridate::dmy(Fecha), edad = as.numeric(edad), sexo = as.factor(sexo), 
                          fuente_tipo_contagio = as.factor(fuente_tipo_contagio), 
                          edad_group = dplyr::case_when(
                              edad < 1            ~ "Menor a 1 año",
                              edad >= 1 & edad <= 9 ~ "1 a 9 años",
                              edad >=10 & edad <= 19 ~ "10 a 19 años",
                              edad >=20 & edad <= 29 ~ "20 a 29 años",
                              edad >=30 & edad <= 39 ~ "30 a 39 años",
                              edad >=40 & edad <= 49 ~ "40 a 49 años",
                              edad >=50 & edad <= 59 ~ "50 a 59 años",
                              edad >=60 & edad <= 69 ~ "60 a 69 años",
                              edad >=70 & edad <= 79 ~ "70 a 79 años",
                              edad >=80 & edad <= 89 ~ "80 a 89 años",
                              edad >=90 & edad <= 99 ~ "90 a 99 años",
                              edad > 100             ~ "Mayor a 100 años"
                          ),
                          # Convert to factor
                          edad_group = factor(
                              edad_group,
                              level = c("Menor a 1 año", 
                                        "1 a 9 años", 
                                        "10 a 19 años",
                                        "20 a 29 años",
                                        "30 a 39 años",
                                        "40 a 49 años",
                                        "50 a 59 años",
                                        "60 a 69 años",
                                        "70 a 79 años",
                                        "80 a 89 años",
                                        "90 a 99 años",
                                        "Mayor a 100 años"
                              )
                          )
            ) %>% 
            dplyr::filter(recuperado == "Fallecido") %>%
            dplyr::mutate(dias = interval(ymd(date1), ymd(date2)) %/% days(1)) %>%
            dplyr::group_by(edad_group, sexo) %>%
            dplyr::summarise(cumcasos = sum(n()) / dias * 1000) %>%
            ggplot(aes(x = edad_group, y = cumcasos, fill = sexo)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            geom_text(
                aes(label = round(cumcasos, 0)),
                vjust = 1.6,
                color = "white",
                position = position_dodge(0.9),
                size = 3.5
            ) +
            scale_fill_brewer(palette = "Paired") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45)) +
            ylab("Fallecidos por 1000 en el periodo de pandemia") + xlab("Grupo etario")
    })
    
}
