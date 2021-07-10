
dat <- 
covid_cor %>% 
    mutate(edad = as.numeric(edad), sexo = as.factor(sexo), 
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
    )







dat %>%
    dplyr::filter(ciudad_municipio_nom == "MONTERIA") %>%
    ggplot2::ggplot(aes(x = edad_group, fill = sexo)) +
    geom_bar(stat = "count", position = position_dodge()) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45)) +
    ylab("Número de casos") + xlab("Grupos etarios")

    

dat %>%
    dplyr::filter(ciudad_municipio_nom == "MONTERIA") %>%
    ggplot2::ggplot(aes(x = fuente_tipo_contagio, fill = sexo)) +
    geom_bar(stat = "count", position = position_dodge()) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45)) +
    ylab("Número de casos") + xlab("Grupos etarios")





acumu <- 
covid_cor %>% dplyr::filter(ciudad_municipio_nom == "MONTERIA") %>%
    tidyr::separate(fecha_de_notificaci_n, c("Fecha", "Hora"), sep = " ", remove = TRUE) %>% 
    dplyr::mutate(Fecha = lubridate::dmy(Fecha), 
                  recuperado = replace(recuperado, 
                                       recuperado == "fallecido", "Fallecido")) %>% 
    dplyr::group_by(Fecha) %>% 
    dplyr::summarise(n_casos = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(csum = cumsum(n_casos))


data_xts <- xts::xts(x = acumu[, 3], order.by = acumu$Fecha)

dygraph(data_xts) %>%
    dySeries("csum", label = "Positivos") %>%
    dyHighlight(highlightCircleSize = 3, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>% 
    dyAxis("y", label = "Número de Casos") %>%
    dyLegend(width = 400) %>% 
    dyRangeSelector(height = 20)





acumu <- 
    covid_cor %>% dplyr::filter(ciudad_municipio_nom == "MONTERIA") %>%
    tidyr::separate(fecha_de_notificaci_n, c("Fecha", "Hora"), sep = " ", remove = TRUE) %>% 
    dplyr::mutate(Fecha = lubridate::dmy(Fecha), 
                  recuperado = replace(recuperado, 
                                       recuperado == "fallecido", "Fallecido")) %>% 
    dplyr::count(Fecha, recuperado) %>% 
    tidyr::spread(recuperado, n) %>% 
    dplyr::rowwise(Fecha) %>% 
    dplyr::mutate(Positivos = sum(c(Activo, Fallecido, `N/A`, Recuperado), na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(Fallecido = replace_na(Fallecido, 0), Recuperado = replace_na(Recuperado, 0), 
                  cumPosi = cumsum(Positivos), cumFall = cumsum(Fallecido), cumRec = cumsum(Recuperado))




##############################################################################################################



covid_cor %>%
    tidyr::separate(fecha_de_notificaci_n,
                    c("Fecha", "Hora"),
                    sep = " ",
                    remove = TRUE) %>%
    dplyr::mutate(
        Fecha = lubridate::dmy(Fecha),
        recuperado = replace(recuperado,
                             recuperado == "fallecido", "Fallecido")
    ) %>%
    dplyr::group_by(recuperado) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(Prev = n / sum(n) * 10000)



covid_cor %>%
    dplyr::filter(ciudad_municipio_nom == "MONTERIA") %>%
    tidyr::separate(fecha_de_notificaci_n,
                    c("Fecha", "Hora"),
                    sep = " ",
                    remove = TRUE) %>%
    dplyr::mutate(
        Fecha = lubridate::dmy(Fecha),
        recuperado = replace(recuperado,
                             recuperado == "fallecido", "Fallecido")
    ) %>%
    dplyr::group_by(recuperado) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(Prev = n / sum(n) * 1000)



Prev <- covid_cor %>%
    dplyr::mutate( recuperado = replace(recuperado, recuperado == "fallecido", "Fallecido")) %>%
    dplyr::group_by(recuperado) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(Prev = n / sum(n)) %>% 
    dplyr::filter(recuperado == "Fallecido") %>% 
    dplyr::select(Prev) %>% 
    dplyr::pull()


##############################################################################################################

acumu <- 
covid_cor %>% 
    dplyr::filter(ciudad_municipio_nom == "MONTERIA") %>%
    tidyr::separate(fecha_de_notificaci_n,
                    c("Fecha", "Hora"),
                    sep = " ",
                    remove = TRUE) %>%
    dplyr::mutate(
        Fecha = lubridate::dmy(Fecha),
        recuperado = replace(recuperado,
                             recuperado == "fallecido", "Fallecido")
    ) %>%
   dplyr::group_by(Fecha) %>% 
    dplyr::summarise(ncasos = n()) %>% 
    dplyr::mutate(ch_rate = (ncasos - lag(ncasos))*100/lag(ncasos))


data_xts <- xts::xts(x = acumu[, 3], order.by = acumu$Fecha)

dygraph(data_xts) %>%
    dySeries("ch_rate", label = "Variación en la tasa de contagios") %>%
    dyHighlight(highlightCircleSize = 3, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>% 
    dyAxis("y", label = "Número de Casos") %>%
    dyLegend(width = 400) %>% 
    dyRangeSelector(height = 20)

##############################################################################################################


dat <- 
    covid_cor %>% 
    tidyr::separate(fecha_de_notificaci_n,
                    c("Fecha", "Hora"),
                    sep = " ",
                    remove = TRUE) %>%
    mutate(Fecha = lubridate::dmy(Fecha), edad = as.numeric(edad), sexo = as.factor(sexo), 
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
    )

dat %>% 
    dplyr::filter(ciudad_municipio_nom == "MONTERIA", recuperado == "Fallecido") %>% 
    dplyr::count(Fecha, sexo, edad) %>% 
    dplyr::mutate(dias = interval(min(Fecha), max(Fecha)) %/% days(1))
    
    
interval(min(dat$Fecha), max(dat$Fecha) %/% days(1))


  prueba <-   
    dat %>% 
    dplyr::filter(ciudad_municipio_nom == "MONTERIA", recuperado == "Fallecido") %>% 
    dplyr::mutate(dias = interval(min(Fecha), max(Fecha)) %/% days(1)) %>% 
    dplyr::group_by(edad_group, sexo) %>%
    dplyr::summarise(cumcasos = sum(n())/dias *1000)


  
  
  
  prueba %>%
      ggplot(aes(x = edad_group, y = cumcasos, fill = sexo)) +
      geom_bar(stat = "identity", position = position_dodge(0.9)) +
      geom_text(
          aes(label = round(cumcasos, 0)),
          vjust = 1.6,
          color = "white",
          position = position_dodge(0.9),
          size = 3
      ) +
      scale_fill_brewer(palette="Blues") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45)) +
      ylab("Fallecidos por 1000 en el periodo de pandemia") + xlab("Grupo etario")
 
  





##############################################################################################################
  
  PobMun %>% 
      dplyr::select(Entidad, Indicador, Valor, Año) %>% 
      dplyr::filter(Año == 2021, 
                    Indicador %in% c("Población total de hombres", "Población total de mujeres")) %>% 
      dplyr::group_by(Entidad) %>% 
      dplyr::summarise(Total = sum(Valor, na.rm = T))
  
  
  
  
  ############################################################################################################
  
  library(forecast)
  
  fit_data <- 
  covid_cor %>% 
      dplyr::filter(ciudad_municipio_nom == "MONTERIA") %>%
      tidyr::separate(fecha_de_notificaci_n,
                      c("Fecha", "Hora"),
                      sep = " ",
                      remove = TRUE) %>%
      dplyr::mutate(
          Fecha = lubridate::dmy(Fecha),
          recuperado = replace(recuperado, recuperado == "fallecido", "Fallecido")
      ) %>%
      dplyr::group_by(Fecha) %>%
      dplyr::summarise(n_casos = n())

  
  y <- msts(fit_data$n_casos, seasonal.periods=c(7,365.25))
  fit <- tbats(y)
  fc <- forecast(fit, h = 60)
  plot(fc)
  
  
  fc$lower[, 2]
  
  
  
  
  
  
  
  
  
  
  dat <- fc$x
  lower <- fc$lower[,2] # confidence intervals lower bound
  upper <- fc$upper[,2] # confidence intervals upper bound
  pforecast <- fc$mean #

  
  seq.Date(max(fit_data$Fecha), length = 30, by = '1 day')
  
  c(rep(NA, length(fit_data$n_casos)), as.numeric(lower))
  
  mydata <- data.frame(Fecha = c(fit_data$Fecha, seq.Date(max(fit_data$Fecha), length = 30, by = '1 day')),
                  Casos = c(fit_data$n_casos, rep(NA, 30)),
                  Low = c(rep(NA, length(fit_data$n_casos)), as.numeric(lower)),
                  Upp = c(rep(NA, length(fit_data$n_casos)), as.numeric(upper)),
                  Mea = c(rep(NA, length(fit_data$n_casos)), as.numeric(pforecast))
                  )
  
  
  data_xts <- xts::xts(x = mydata[, 2:5], order.by = mydata$Fecha)
  
  
  library(dygraphs)
  
  dygraph(data_xts, main = "Casos positivos confirmados") %>% 
      dySeries("Casos", label = "Actual") %>%
      dySeries(c("Low", "Mea", "Upp"), label = "Predicted") %>% 
      dyRangeSelector()                       
      
      
      
      # the zoom tool
      dySeries(name = "dat", label = "Revenue Data") %>%                                   # add time series which are store in: data <- nnetforecast$x
      dySeries(c("lower","pforecast","upper"), label = "Revenue Forecast") %>%              # add the forecast and CI
      dyLegend(show = "always", hideOnMouseOut = FALSE) %>%                                 # add the legend (time series + forecast)
      dyAxis("y", label = "Monthly Revenue USD") %>%                                        # label the y axis
      dyHighlight(highlightCircleSize = 5,                                                  # specify what happen when the mouse in hovering the graph
                  highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyOptions(axisLineColor = "navy", gridLineColor = "grey")                    # set axis and fridline colors
      

      
      
      
      
      #######################################################################################################
      
      
      hw <- HoltWinters(ldeaths)
      p <- predict(hw, n.ahead = 36, prediction.interval = TRUE)
      all <- cbind(ldeaths, p)
      
      dygraph(all, "Deaths from Lung Disease (UK)") %>%
          dySeries("ldeaths", label = "Actual") %>%
          dySeries(c("p.lwr", "p.fit", "p.upr"), label = "Predicted")