
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(tidyverse)
library(htmlwidgets)
library(leaflet)
library(dygraphs)

########################################################################################################################

# Carga de los datos de la API de datos abiertos 
source("AuxData.R")
source('Module_Map.R')
source("Module_Monitoring.R")
source('Module_Analytics.R')

########################################################################################################################

# Header of the App 
Header <- dashboardHeader(title = "Covid Tracker Córdoba")

##############################################################################################################
# Sidebar ... Modules for the App
Sidebar <- dashboardSidebar(sidebarMenu(
    menuItem(
        "GeoRef",
        icon = icon("fas fa-map-marked-alt"),
        tabName = "mapa"
    ),
    
    menuItem(
        "Monitoring",
        icon = icon("fas fa-tachometer-alt"),
        tabName = "General"
    ),
    
    menuItem(
        "Analytics",
        icon = icon("fas fa-chart-bar"),
        tabName = "Analytics"
    ),
    
    menuItem(
        "Predicts",
        icon = icon("fas fa-chart-line"),
        tabName = "Predicts"
    )
))

##############################################################################################################
## Body 
Body <- dashboardBody(
    
    ####################################################################################################################
    ## Mapa de Colombia con el shape de córdoba 
    tabItems(
        
        tabItem(tabName = "mapa", MapModuleUI("GeoRef")),  
    ####################################################################################################################
    ## Tab con las estadísticas de resumen generales 
        tabItem(tabName = "General", MonModuleUI("General", data = covid_cor)),
    
    ##########################################################################################################
    ## Tab con el análisis de tasas y calculos epidemiologicos 
        tabItem(tabName = "Analytics", AnalyticModuleUI("Analytic", data = covid_cor))
    )
    )
    



########################################################################################################################
# Interfaz gráfica de usuario 

ui <- dashboardPage(skin = "black", Header, Sidebar, Body)

########################################################################################################################
# Funciones del server 


server <- function(input, output, session) {
    ### Funtions for tab Dasboard 
    callModule(MapModuleSR, "GeoRef", data = map_cor)
    callModule(MonModulerSR, "General", data = covid_cor)
    callModule(AnalyticModuleSR, "Analytic", data = covid_cor)
}

shinyApp(ui, server)












