library(dplyr)
library(shiny)
library(shinydashboard)
library(lubridate)


AnalyticModuleUI <- function(id, data) {
    ns <- NS(id)
    
    shiny::fluidRow(
        valueBoxOutput(ns("Prev_dep")),
        valueBoxOutput(ns("Prev_mun")),
        valueBoxOutput(ns("Inc_dep")),
        # valueBoxOutput(ns("Inc_mun")),
        
        
        
        shinydashboard::tabBox(
            title = "Estadísticas de contagios",
            id = ns("tabset"),
            height = "800px",
            width = 10,
            tabPanel(
                title = "Tasa de contagios",
                shinydashboard::box(solidHeader = T,
                                    width = 12,
                                    dygraphOutput(ns("chance_rate"), height = "600px"))
            ),
            tabPanel(
                title = "Prevalencia por edad-sexo",
                
                shinydashboard::box(solidHeader = T,
                                    width = 12,
                                    plotOutput(ns("rate_edad_sexo"), height = "600px"))
                ), 
            tabPanel(
                title = "Incidencia por municipios",
                
                shinydashboard::box(solidHeader = T,
                                    width = 12,
                                    plotOutput(ns("plot_inc_mun"), height = "600px"))
            ), 
            
            tabPanel(
                title = "Incidencia por periodo",
                shinydashboard::box(solidHeader = T,
                                    width = 12,
                                    dygraphOutput(ns("Inc_plot_per"), height = "600px"))
            )
            
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
            )
        )
        
        
        
    )
    
}



AnalyticModuleSR <- function(input, output, session, data, data_mun) {
    
    
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
            dplyr::filter(ciudad_municipio_nom == input$Mun)
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
    
    output$Inc_dep <- renderValueBox({
        
        Ncasos <-   data %>% 
            dplyr::mutate(
                ciudad_municipio_nom = replace(ciudad_municipio_nom, 
                                               ciudad_municipio_nom == "MO¾ITOS", "MOÑITOS")) %>% 
            dplyr::summarise(ncasos = n()) %>% dplyr::pull(ncasos)
        
        Total_Pob <- 
        data_mun %>% 
            dplyr::filter(Entidad == "Córdoba", Año == 2021) %>% 
            dplyr::pull(Valor)
        
        
        shinydashboard::valueBox(
            value = round(Ncasos / Total_Pob, 3),
            subtitle = paste(
                "Incidencia de casos en el Depto.:",
                round(Ncasos / Total_Pob * 10000, 0),
                "por 10000 Hab."
            ),
            icon = icon("fas fa-user-check"),
            color = "green",
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
            dplyr::mutate(edad = as.numeric(edad), 
                          sexo = as.factor(sexo), 
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
            dplyr::mutate(dias = interval(min(Fecha), max(Fecha)) %/% days(1)) %>% 
            dplyr::group_by(edad_group, sexo) %>%
            dplyr::summarise(cumcasos = sum(n())/dias *1000) %>% 
            dplyr::ungroup() %>% 
            ggplot(aes(x = edad_group, y = cumcasos, fill = sexo)) +
            geom_bar(stat = "identity", position = position_dodge(0.9)) +
            geom_text(
                aes(label = round(cumcasos, 0)),
                vjust = 1.6,
                color = "white",
                position = position_dodge(0.9),
                size = 3
            ) +
            scale_fill_brewer(palette = "Paired") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45)) +
            ylab("Fallecidos por 1000 en el periodo de pandemia") + xlab("Grupo etario")
    })
    
    output$plot_inc_mun <- renderPlot({
        
        Ncasos <-   data %>% 
            dplyr::mutate(
                ciudad_municipio_nom = replace(ciudad_municipio_nom, 
                                               ciudad_municipio_nom == "MO¾ITOS", "MOÑITOS"),
                ciudad_municipio_nom = stringr::str_to_title(ciudad_municipio_nom)) %>% 
            dplyr::group_by(ciudad_municipio_nom) %>% 
            dplyr::summarise(ncasos = n()) %>%  dplyr::arrange(ciudad_municipio_nom) 
            
        incMun <- 
            data_mun %>% 
            dplyr::filter(Entidad != "Córdoba", Año == 2021) %>% 
            dplyr::arrange(Entidad) %>% 
            dplyr::mutate(ncasos = Ncasos$ncasos, Inc = ncasos / Valor * 100)
        
        incMun %>%
            ggplot2::ggplot(aes(x = Entidad, y = Inc)) +
            geom_bar(stat = "identity", position = position_dodge(0.9), fill="steelblue") +
            geom_text(
                aes(label = round(Inc, 1)),
                vjust = 1.6,
                color = "white",
                position = position_dodge(0.9),
                size = 3
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45)) +
            ylab("(%) Porcentaje de Incidencia") + xlab("Municipio")    
        
    })
    
    
    output$Inc_plot_per <- renderDygraph({
        
        Nombres <- 
            data %>% 
            dplyr::mutate(
                ciudad_municipio_nom = replace(ciudad_municipio_nom, ciudad_municipio_nom == "MO¾ITOS", "MOÑITOS")) %>% 
            dplyr::distinct(ciudad_municipio_nom) %>% 
            dplyr::arrange(ciudad_municipio_nom) %>% dplyr::pull()
        
        TotPob <- 
            data_mun %>% 
            dplyr::filter(Entidad != "Córdoba", Año == 2021) %>% 
            dplyr::arrange(Entidad) %>% 
            dplyr::mutate(Nombre_mun = Nombres) %>% 
            dplyr::filter(Nombre_mun == input$Mun) %>% 
            dplyr::pull(Valor)
        
        Casos_fil <- 
        fil_data() %>% 
            dplyr::mutate(Mes = lubridate::floor_date(Fecha, "month")) %>% 
            dplyr::group_by(Mes) %>% 
            dplyr::summarise(Total_mes = n() ) %>% 
            dplyr::mutate(Inc = (Total_mes / (TotPob - lag(Total_mes)))*100, 
                          Inc = tidyr::replace_na(Inc, (Total_mes[1]/ TotPob)*100)) 
        
        data_xts <- xts::xts(x = Casos_fil[, 3], order.by = Casos_fil$Mes)
        
        dygraph(data_xts) %>%
            dySeries("Inc", label = "Incidencia") %>%
            dyHighlight(
                highlightCircleSize = 3,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE
            ) %>%
            dyAxis("y", label = "(%) Incidencia") %>%
            dyLegend(width = 400) %>%
            dyRangeSelector(height = 20)
        
    })
    
    
    
}
