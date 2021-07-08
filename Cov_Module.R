library(dplyr)
library(shiny)
library(shinydashboard)



filterModuleUI <- function(id, data) {
    
    ns <- NS(id) 
    
    shinydashboard::box(width = NULL, solidHeader = FALSE, title = "Filtro",
        
        selectizeInput(
            inputId = ns("selectDept"),
            label = "Departamento",
            choices = c("Seleccione" = "", Dcovid %>% dplyr::distinct(Ndept)),
            options = list(create = TRUE)
            ),
        selectizeInput(
            inputId = ns("selectMun"),
            label = "Municipio",
            choices = c("Escoja" = "", Dcovid %>% dplyr::distinct(Nmun)),
            selected = "MONTERIA",
            options = list(create = TRUE)
        )
        )
}

filterModuleServer  <- function(input, output, session, data) {

    updateSelectizeInput(session, 
                         "selectDept", 
                         choices = c("Seleccione" = "", data %>% dplyr::distinct(Ndept)),
                         selected = "CORDOBA",
                         options = list(create = TRUE)
                         )

    observe({
        updateSelectizeInput(
            session,
            "selectMun",
            "Municipio:",
            choices = data %>%
                dplyr::filter(Ndept == input$selectDept) %>% dplyr::distinct(Nmun) %>% dplyr::arrange(Nmun),
            selected = "MONTERIA",
            options = list(create = TRUE)
        )
    })
    
}


# filterModuleUI <- function(id) {
#     ns <- NS(id)
#     
#     shinydashboard::box(
#         title = "filter",
#         selectInput(
#             inputId = ns("selectDept"),
#             label = "Select Departamento",
#             choices = covid %>% dplyr::distinct(Ndept),
#             selected = "CORDOBA"
#         ),
#         selectInput(
#             inputId = ns("selectMun"),
#             label = "Select Municipio",
#             choices = covid %>% dplyr::distinct(Nmun),
#             selected = "MONTERIA"
#         )
#     )
#     
# }
# 
# 
# filterModuleServer  <- function(input, output, session, data) {
#     
#     Data_Mun <- reactive({
#         data %>% 
#         dplyr::filter(Ndept == input$selectDept) %>% dplyr::distinct(Nmun) %>% dplyr::arrange(Nmun)
#     })
#     
#     
#     observe({
#         updateSelectInput(
#             session,
#             "selectMun",
#             "Select Municipio:",
#             choices = Data_Mun(),
#             selected = "MONTERIA"
#         )
#     })
# }