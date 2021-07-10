library(dplyr)
library(shiny)
library(shinydashboard)
library(lubridate)

MonModuleUI <- function(id, data) {
    ns <- NS(id)
    
    
    shiny::fluidRow(
        valueBoxOutput(ns("con")),
        valueBoxOutput(ns("rec")),
        valueBoxOutput(ns("act")),
        valueBoxOutput(ns("fall")),
        
        shinydashboard::tabBox(
            title = "Monitoreo de contagios",
            id = ns("tabset"),
            height = "500px",
            width = 10,
            tabPanel(
                "Casos confirmados",
                shinydashboard::box(
                    solidHeader = T,
                    width = 6,
                    dygraphOutput(ns("Casos"))
                ),
                shinydashboard::box(
                    solidHeader = T,
                    width = 6,
                    plotOutput(ns("casos_edad_sexo"))
                )
            ),
            tabPanel(
                "Recuperados",
                shinydashboard::box(
                    solidHeader = T,
                    width = 6,
                    dygraphOutput(ns("Recuperados"))
                ),
                shinydashboard::box(
                    solidHeader = T,
                    width = 6,
                    plotOutput(ns("recuperados_edad_sexo"))
                )
            ),
            tabPanel(
                "Activos",
                shinydashboard::box(
                    solidHeader = T,
                    width = 6,
                    dygraphOutput(ns("Activos"))
                ),
                shinydashboard::box(
                    solidHeader = T,
                    width = 6,
                    plotOutput(ns("activos_edad_sexo"))
                )
            ),
            tabPanel(
                "Fallecidos",
                shinydashboard::box(
                    solidHeader = T,
                    width = 6,
                    dygraphOutput(ns("Fallecidos"))
                ),
                shinydashboard::box(
                    solidHeader = T,
                    width = 6,
                    plotOutput(ns("fallecidos_edad_sexo"))
                )
            ),
            tabPanel(
                "Acumulados",
                shinydashboard::box(
                    solidHeader = T,
                    width = 12,
                    dygraphOutput(ns("Acumulados"))
                )
            )
        ),
        shinydashboard::box(
            width = 2,
            solidHeader = T,
            selectizeInput(
                inputId = ns("selectMun"),
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
                ns("fechas"),
                label = "Rango de fechas",
                start = "2020-03-25",
                end = "2021-06-30",
                startview = "month"
            )
        )
    )
}


MonModulerSR <- function(input, output, session, data) {
    fil_data <- reactive({
        data %>%
            tidyr::separate(
                fecha_de_notificaci_n,
                c("Fecha", "Hora"),
                sep = " ",
                remove = TRUE
            ) %>%
            dplyr::mutate(
                Fecha = lubridate::dmy(Fecha),
                recuperado = replace(recuperado,
                                     recuperado == "fallecido", "Fallecido")
            ) %>%
            dplyr::filter(
                ciudad_municipio_nom == input$selectMun,
                Fecha >= input$fechas[1] &
                    Fecha <= input$fechas[2]
            )
    })
    
    output$con <- renderValueBox({
        shinydashboard::valueBox(
            value = fil_data() %>%
                dplyr::summarise(n = n()),
            subtitle = "Confirmados",
            icon = icon("fas fa-users"),
            color = "fuchsia",
            width = 3
        )
    })
    
    output$rec <- renderValueBox({
        shinydashboard::valueBox(
            value = fil_data() %>%
                dplyr::filter(recuperado == "Recuperado") %>%
                dplyr::summarise(n = n()),
            subtitle = "Recuperados",
            icon = icon("fas fa-user-check"),
            color = "green",
            width = 3
        )
    })
    
    output$act <- renderValueBox({
        shinydashboard::valueBox(
            value = fil_data() %>%
                dplyr::filter(recuperado == "Activo") %>%
                dplyr::summarise(n = n()),
            subtitle = "Activos",
            icon = icon("fas fa-hospital-user"),
            color = "orange",
            width = 3
        )
    })
    
    output$fall <- renderValueBox({
        shinydashboard::valueBox(
            value = fil_data() %>%
                dplyr::filter(recuperado %in% c("Fallecido")) %>%
                dplyr::summarise(n = n()),
            subtitle = "Fallecidos",
            icon = icon("fas fa-ribbon"),
            color = "purple",
            width = 3
        )
    })
    
    output$Casos <- renderDygraph({
        Casos_fil <-
            fil_data() %>%
            dplyr::count(Fecha, sexo) %>%
            tidyr::spread(sexo, n) %>%
            dplyr::rename("MPositivos" = "F", "HPositivos" = "M") %>%
            dplyr::rowwise(Fecha) %>%
            dplyr::mutate(Positivos = sum(c(MPositivos, HPositivos), na.rm = T))
        
        data_xts <-
            xts::xts(x = Casos_fil[, 2:4], order.by = Casos_fil$Fecha)
        
        dygraph(data_xts) %>%
            dySeries("Positivos", label = "Positivos") %>%
            dySeries("MPositivos", label = "Mujeres") %>%
            dySeries("HPositivos", label = "Hombres") %>%
            dyHighlight(
                highlightCircleSize = 3,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE
            ) %>%
            dyAxis("y", label = "Número de Casos") %>%
            dyLegend(width = 400) %>%
            dyRangeSelector(height = 20)
    })
    
    output$casos_edad_sexo <- renderPlot({
        fil_data() %>%
            mutate(
                edad = as.numeric(edad),
                sexo = as.factor(sexo),
                fuente_tipo_contagio = as.factor(fuente_tipo_contagio),
                edad_group = dplyr::case_when(
                    edad < 1            ~ "Menor a 1 año",
                    edad >= 1 & edad <= 9 ~ "1 a 9 años",
                    edad >= 10 & edad <= 19 ~ "10 a 19 años",
                    edad >= 20 & edad <= 29 ~ "20 a 29 años",
                    edad >= 30 & edad <= 39 ~ "30 a 39 años",
                    edad >= 40 & edad <= 49 ~ "40 a 49 años",
                    edad >= 50 & edad <= 59 ~ "50 a 59 años",
                    edad >= 60 & edad <= 69 ~ "60 a 69 años",
                    edad >= 70 & edad <= 79 ~ "70 a 79 años",
                    edad >= 80 & edad <= 89 ~ "80 a 89 años",
                    edad >= 90 & edad <= 99 ~ "90 a 99 años",
                    edad > 100             ~ "Mayor a 100 años"
                ),
                # Convert to factor
                edad_group = factor(
                    edad_group,
                    level = c(
                        "Menor a 1 año",
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
            ggplot2::ggplot(aes(x = edad_group, fill = sexo)) +
            geom_bar(stat = "count", position = position_dodge()) +
            scale_fill_brewer(palette = "Paired") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45)) +
            ylab("Número de casos") + xlab("Grupo etario")
    })
    
    output$Recuperados <- renderDygraph({
        Casos_fil <-
            fil_data() %>%
            dplyr::filter(recuperado == "Recuperado") %>%
            dplyr::count(Fecha, sexo) %>%
            tidyr::spread(sexo, n) %>%
            dplyr::rename("MRecuperadas" = "F",
                          "HRecuperados" = "M") %>%
            dplyr::rowwise(Fecha) %>%
            dplyr::mutate(Recuperados = sum(c(MRecuperadas, HRecuperados), na.rm = T))
        
        data_xts <-
            xts::xts(x = Casos_fil[, 2:4], order.by = Casos_fil$Fecha)
        
        dygraph(data_xts) %>%
            dySeries("Recuperados", label = "Recuperados") %>%
            dySeries("MRecuperadas", label = "Mujeres") %>%
            dySeries("HRecuperados", label = "Hombres") %>%
            dyHighlight(
                highlightCircleSize = 3,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE
            ) %>%
            dyAxis("y", label = "Número de Casos") %>%
            dyLegend(width = 400) %>%
            dyRangeSelector(height = 20)
    })
    
    output$recuperados_edad_sexo <- renderPlot({
        fil_data() %>%
            dplyr::filter(recuperado == "Recuperado") %>%
            dplyr::mutate(
                edad = as.numeric(edad),
                sexo = as.factor(sexo),
                fuente_tipo_contagio = as.factor(fuente_tipo_contagio),
                edad_group = dplyr::case_when(
                    edad < 1            ~ "Menor a 1 año",
                    edad >= 1 & edad <= 9 ~ "1 a 9 años",
                    edad >= 10 & edad <= 19 ~ "10 a 19 años",
                    edad >= 20 & edad <= 29 ~ "20 a 29 años",
                    edad >= 30 & edad <= 39 ~ "30 a 39 años",
                    edad >= 40 & edad <= 49 ~ "40 a 49 años",
                    edad >= 50 & edad <= 59 ~ "50 a 59 años",
                    edad >= 60 & edad <= 69 ~ "60 a 69 años",
                    edad >= 70 & edad <= 79 ~ "70 a 79 años",
                    edad >= 80 & edad <= 89 ~ "80 a 89 años",
                    edad >= 90 & edad <= 99 ~ "90 a 99 años",
                    edad > 100             ~ "Mayor a 100 años"
                ),
                # Convert to factor
                edad_group = factor(
                    edad_group,
                    level = c(
                        "Menor a 1 año",
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
            ggplot2::ggplot(aes(x = edad_group, fill = sexo)) +
            ggplot2::geom_bar(stat = "count", position = position_dodge()) +
            scale_fill_brewer(palette = "Paired") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45)) +
            ylab("Número de casos recuperados") + xlab("Grupo etario")
    })
    
    output$Activos <- renderDygraph({
        Casos_fil <-
            fil_data() %>%
            dplyr::filter(recuperado == "Activo") %>%
            dplyr::count(Fecha, sexo) %>%
            tidyr::spread(sexo, n) %>%
            dplyr::rename("MActivo" = "F", "HActivo" = "M") %>%
            dplyr::rowwise(Fecha) %>%
            dplyr::mutate(Activos = sum(c(MActivo, HActivo), na.rm = T))
        
        data_xts <-
            xts::xts(x = Casos_fil[, 2:4], order.by = Casos_fil$Fecha)
        
        dygraph(data_xts) %>%
            dySeries("Activos", label = "Activos") %>%
            dySeries("MActivo", label = "Mujeres") %>%
            dySeries("HActivo", label = "Hombres") %>%
            dyHighlight(
                highlightCircleSize = 3,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE
            ) %>%
            dyAxis("y", label = "Número de Casos") %>%
            dyLegend(width = 400) %>%
            dyRangeSelector(height = 20)
    })
    
    output$activos_edad_sexo <- renderPlot({
        fil_data() %>%
            dplyr::filter(recuperado == "Activo") %>%
            dplyr::mutate(
                edad = as.numeric(edad),
                sexo = as.factor(sexo),
                fuente_tipo_contagio = as.factor(fuente_tipo_contagio),
                edad_group = dplyr::case_when(
                    edad < 1            ~ "Menor a 1 año",
                    edad >= 1 & edad <= 9 ~ "1 a 9 años",
                    edad >= 10 &
                        edad <= 19 ~ "10 a 19 años",
                    edad >= 20 &
                        edad <= 29 ~ "20 a 29 años",
                    edad >= 30 &
                        edad <= 39 ~ "30 a 39 años",
                    edad >= 40 &
                        edad <= 49 ~ "40 a 49 años",
                    edad >= 50 &
                        edad <= 59 ~ "50 a 59 años",
                    edad >= 60 &
                        edad <= 69 ~ "60 a 69 años",
                    edad >= 70 &
                        edad <= 79 ~ "70 a 79 años",
                    edad >= 80 &
                        edad <= 89 ~ "80 a 89 años",
                    edad >= 90 &
                        edad <= 99 ~ "90 a 99 años",
                    edad > 100             ~ "Mayor a 100 años"
                ),
                # Convert to factor
                edad_group = factor(
                    edad_group,
                    level = c(
                        "Menor a 1 año",
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
            ggplot2::ggplot(aes(x = edad_group, fill = sexo)) +
            ggplot2::geom_bar(stat = "count", position = position_dodge()) +
            scale_fill_brewer(palette = "Paired") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45)) +
            ylab("Número de casos activos") + xlab("Grupo etario")
    })
    
    
    output$Fallecidos <- renderDygraph({
        Casos_fil <-
            fil_data() %>%
            dplyr::filter(recuperado == "Fallecido") %>%
            dplyr::count(Fecha, sexo) %>%
            tidyr::spread(sexo, n) %>%
            dplyr::rename("MFallecida" = "F", "HFallecido" = "M") %>%
            dplyr::rowwise(Fecha) %>%
            dplyr::mutate(Fallecidos = sum(c(MFallecida, HFallecido), na.rm = T))
        
        data_xts <-
            xts::xts(x = Casos_fil[, 2:4], order.by = Casos_fil$Fecha)
        
        dygraph(data_xts) %>%
            dySeries("Fallecidos", label = "Fallecidos") %>%
            dySeries("MFallecida", label = "Mujeres") %>%
            dySeries("HFallecido", label = "Hombres") %>%
            dyHighlight(
                highlightCircleSize = 3,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE
            ) %>%
            dyAxis("y", label = "Número de Casos") %>%
            dyLegend(width = 400) %>%
            dyRangeSelector(height = 20)
    })
    
    output$fallecidos_edad_sexo <- renderPlot({
        fil_data() %>%
            dplyr::filter(recuperado == "Fallecido") %>%
            dplyr::mutate(
                edad = as.numeric(edad),
                sexo = as.factor(sexo),
                fuente_tipo_contagio = as.factor(fuente_tipo_contagio),
                edad_group = dplyr::case_when(
                    edad < 1            ~ "Menor a 1 año",
                    edad >= 1 & edad <= 9 ~ "1 a 9 años",
                    edad >= 10 &
                        edad <= 19 ~ "10 a 19 años",
                    edad >= 20 &
                        edad <= 29 ~ "20 a 29 años",
                    edad >= 30 &
                        edad <= 39 ~ "30 a 39 años",
                    edad >= 40 &
                        edad <= 49 ~ "40 a 49 años",
                    edad >= 50 &
                        edad <= 59 ~ "50 a 59 años",
                    edad >= 60 &
                        edad <= 69 ~ "60 a 69 años",
                    edad >= 70 &
                        edad <= 79 ~ "70 a 79 años",
                    edad >= 80 &
                        edad <= 89 ~ "80 a 89 años",
                    edad >= 90 &
                        edad <= 99 ~ "90 a 99 años",
                    edad > 100             ~ "Mayor a 100 años"
                ),
                # Convert to factor
                edad_group = factor(
                    edad_group,
                    level = c(
                        "Menor a 1 año",
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
            ggplot2::ggplot(aes(x = edad_group, fill = sexo)) +
            ggplot2::geom_bar(stat = "count", position = position_dodge()) +
            scale_fill_brewer(palette = "Paired") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45)) +
            ylab("Número de casos fallecidos") + xlab("Grupo etario")
    })
    
    
    output$Acumulados <- renderDygraph({
        Casos_fil <-
            fil_data() %>%
            dplyr::count(Fecha, recuperado) %>%
            tidyr::spread(recuperado, n) %>%
            dplyr::rowwise(Fecha) %>%
            dplyr::mutate(Positivos = sum(c(
                Activo, Fallecido, `N/A`, Recuperado
            ), na.rm = T)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
                Fallecido = replace_na(Fallecido, 0),
                Recuperado = replace_na(Recuperado, 0),
                cumPosi = cumsum(Positivos),
                cumFall = cumsum(Fallecido),
                cumRec = cumsum(Recuperado)
            )
        
        data_xts <-
            xts::xts(x = Casos_fil[, 7:9], order.by = Casos_fil$Fecha)
        
        dygraph(data_xts) %>%
            dySeries("cumPosi", label = "Positivos") %>%
            dySeries("cumRec", label = "Recuperados") %>%
            dySeries("cumFall", label = "Fallecidos") %>%
            dyHighlight(
                highlightCircleSize = 3,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE
            ) %>%
            dyAxis("y", label = "Número de Casos") %>%
            dyLegend(width = 400) %>%
            dyRangeSelector(height = 20)
    })
    
}