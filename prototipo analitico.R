# paquetes requeridos ----
Sys.setlocale("LC_TIME", "C")
options(scipen = 999)
library(shiny)
require(shinydashboard)
require(TTR)
require(stringi)
require(xts)
require(dygraphs)
require(moments)
require(RPostgreSQL)
require(dplyr)
# funciones ----
# funcion papi
#agregar dias para prediccion
addDays <- function(data,date,days) {
  for(i in 1:days){
    data[length(data)+1] <- NA
    date[length(date)+1] <- date[length(date)]+1
  }
  y <- xts(data,order.by = date)
  return(y)
}
addDaysForecast <- function(forecast,date,days) {
  data <- rep(NA,length(date))
  for(i in 1:days){
    data[length(data)+1] <- forecast[i]
    date[length(date)+1] <- date[length(date)]+1
  }
  y <- xts(data,order.by = date)
  return(y)
}
plotForecast <- function(table,forecast,name) {
  days <-length(forecast)
  date <-as.Date(table[,"Date"])
  values <- as.numeric(table[,"Close"])
  series <- addDays(values,date,days)
  serieForecast <- addDaysForecast(forecast,date,days)
  day1 <- date[length(date)-days*2]
  day2 <- date[length(date)]+7
  curvas <- cbind(series,serieForecast)
  graf <- dygraph(curvas, main = name) %>% 
    dySeries("..1", label = "datos", color = "black") %>%
    dySeries("..2", label = " Forecast",  stepPlot = TRUE, color = "green") %>%
    dyAxis("y", label = "CLOSINGPRICE") %>%
    dyCrosshair(direction = "vertical") %>% 
    dyRangeSelector(dateWindow = c(day1, day2)) %>%
    ##dyOptions(maxNumberWidth = 20, stackedGraph = FALSE) %>%
    dyLegend(width = 400) %>%
    dyHighlight(highlightCircleSize = 3, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>%
    dyRangeSelector() %>%
    dyCSS("legend.css")
  return(graf)
}
plotNormal <- function(table,thing,name,show) {
  date <-as.Date(table[,"Date"])
  values <- as.numeric(table[,thing])
  series <- xts(values, order.by = date)
  ma1 <- xts(runMean(values, n = 6),order.by = date)
  ma2 <- xts(runMean(values, n = 12),order.by = date)
  ma3 <- xts(runMean(values, n = 20),order.by = date)
  mv1 <- xts(runVar(values, n = 6), order.by = date)
  mv2 <- xts(runVar(values, n = 12), order.by = date)
  mv3 <- xts(runVar(values, n = 20), order.by = date)
  ske1 <- xts(movskew(values,6), order.by = date)
  ske2 <- xts(movskew(values,12), order.by = date)
  ske3 <- xts(movskew(values,20), order.by = date)
  curvas <- cbind(series,ma1,ma2,ma3,mv1,mv2,mv3,ske1,ske2,ske3)
  graf <- dygraph(curvas, main = name, group = "ALL") %>% 
    dySeries("..1", label = "datos", color = "black") %>%
    dySeries("..2", label = "Ma6", color = "red") %>%
    dySeries("..3", label = "Ma12", color = "blue") %>%
    dySeries("..4", label = "Ma20", color = "green") %>%
    dySeries("..5", label = "Mv6",strokePattern = "dashed",axis = 'y2', color = "red") %>%
    dySeries("..6", label = "Mv12",strokePattern = "dashed",axis = 'y2', color = "blue") %>%
    dySeries("..7", label = "Mv20",strokePattern = "dashed",axis = 'y2',color = "green") %>%
    dySeries("..8", label = " as 6",  stepPlot = TRUE, color = "red") %>%
    dySeries("..9", label = " as 12",  stepPlot = TRUE, color = "blue") %>%
    dySeries("..10", label = " as 20",  stepPlot = TRUE, color = "green") %>%
    dyAxis("y", label = thing) %>%
    dyCrosshair(direction = "vertical") %>%
    ##dyOptions(maxNumberWidth = 20, stackedGraph = FALSE) %>%
    dyLegend(width = 400) %>%
    dyHighlight(highlightCircleSize = 3, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>%
    dyRangeSelector() %>%
    ##dyCSS("legend.css") %>%
    dyVisibility(visibility=c(show[1],
                              rep(show[2],3),
                              rep(show[3],3),
                              rep(show[4],3)))
  return(graf)
}
#genera el promedio de diferencias t2 - t1(para ver la tendencia de la prediccion)
status <- function(nemo,date){
  forecast <- getforecast2(nemo,1,date)
  if(length(forecast)>1){
    sum <- vector()
    for(i in 1:(length(forecast)-1)){
      sum[i] <- forecast[i+1] - forecast[i]
    }
    return(mean(sum))
  }else{
    return(FALSE)
  }
}
##skewness moving
movskew <- function(values,n) {
  values2 <- values
  for(i in 1:n){
    values2[i] <- NA
  }
  num <- n
  for(i in 1:(length(values)-n)){
    num <- num + 1
    values2[num] <- as.numeric(skewness(values[i:num]))
  }
  return(values2*10)
}
#leer los archvios
#numero de observaciones
numObs <- function(table,n) {
  if(nrow(table)>n){
    table <- table[(nrow(table)-n):nrow(table),]
  }
  return(table)
}
dyVisibility <- function (dygraph, visibility = TRUE){
  dygraph$x$attrs$visibility <- visibility
  dygraph
}
whatshow  <- function(array) {
  showthis <- vector()
  for(i in 1:5){
    showthis[i] <- any(array==i)
  }
  return(showthis)
}
# market data
getforecast <- function(nemo,what) {
  url <- paste0("http://127.0.0.1:9090/api/forecast/", nemo, "?format=json")
  response <- jsonlite::fromJSON(url)
  if(what==1){
    if(length(response)>1){
      return(response$forecast)
    }else{
      return(-1)
    }
  }else if(what==2){
    if(length(response)>1){
      response$predicted$fecha <- stri_sub(response$predicted$fecha,1,12)
      for(i in 1:nrow(response$predicted)){
        x <- stri_sub(response$predicted$fecha[i],6,6)
        y <- stri_sub(response$predicted$fecha[i],1,3)
        if(x==","){
          response$predicted$fecha[i] <- paste0(stri_sub(response$predicted$fecha[i],1,4),
                                                "0",stri_sub(response$predicted$fecha[i],5,12))
        }
        if(y=="Aug"){
          response$predicted$fecha[i] <- paste0("ago",stri_sub(response$predicted$fecha[i],4,12))
        }
        response$predicted$fecha[i] <- paste0(stri_sub(response$predicted$fecha[i],1,3),"/"
                                              ,stri_sub(response$predicted$fecha[i],5,6),"/"
                                              ,stri_sub(response$predicted$fecha[i],9,12))
      }
      return(response$predicted)
    }else{
      return(-1)
    }
  }
}
# bolsa de santiago
getResponse <- function(nemo,what,date){
  url <- paste0("http://localhost:9090/api/forecast/all/",nemo)
  response <- jsonlite::fromJSON(url)
  dato <- 06-03-2018 %in% response$date_forecast
}
getforecast2 <- function(nemo,what,date) {
  #url <- paste0("http://localhost:9090/api/forecast/all/",nemo)
  url <- paste0("http://127.0.0.1:9090/api/forecast/", nemo,"/",turndate(date))
  response <- jsonlite::fromJSON(url)
  #dato <- 06-03-2018 %in% response$date_forecast
  if(what==0){
    if(length(response)>1){
      return(response$t)
    }else{
      return(-1)
    }
  }else if(what==1){
    if(length(response)>1){
      return(response$forecast)
    }else{
      return(-1)
    }
  }else if(what==2){
    if(length(response)>1){
      response$predicted$fecha <- stri_sub(response$predicted$fecha,1,12)
      for(i in 1:nrow(response$predicted)){
        x <- stri_sub(response$predicted$fecha[i],6,6)
        if(x==","){
          response$predicted$fecha[i] <- paste0(stri_sub(response$predicted$fecha[i],1,4),
                                                "0",stri_sub(response$predicted$fecha[i],5,12))
        }
        response$predicted$fecha[i] <- paste0(stri_sub(response$predicted$fecha[i],1,3),"/"
                                              ,stri_sub(response$predicted$fecha[i],5,6),"/"
                                              ,stri_sub(response$predicted$fecha[i],9,12))
      }
      response$predicted$fecha <- as.Date(response$predicted$fecha, format = "%h/%d/%Y")
      return(response$predicted)
    }else{
      return(-1)
    }
  }else if(what==3){
    if(length(response)>1){
      response$datos$fecha <- stri_sub(response$datos$fecha,1,12)
      for(i in 1:nrow(response$datos)){
        x <- stri_sub(response$datos$fecha[i],6,6)
        if(x==","){
          response$datos$fecha[i] <- paste0(stri_sub(response$datos$fecha[i],1,4),
                                            "0",stri_sub(response$datos$fecha[i],5,12))
        }
        response$datos$fecha[i] <- paste0(stri_sub(response$datos$fecha[i],1,3),"/"
                                          ,stri_sub(response$datos$fecha[i],5,6),"/"
                                          ,stri_sub(response$datos$fecha[i],9,12))
      }
      response$datos$fecha <- as.Date(response$datos$fecha, format = "%h/%d/%Y")
      return(response$datos)
    }else{
      return(-1)
    }
  }
}
getTable <- function(nemo) {
  url <- paste0("http://www.bolsadesantiago.com/DatosGraficos/DatosGraficos-ChartIQ/", nemo, "-days.js")
  response <- jsonlite::fromJSON(url)
  if(length(response)>1){
    return(response[1:nrow(response)-1,])
  }else{
    return(-1)
  }
}
turndate <- function(date){
  fecha <- paste0(stri_sub(date,9,10),stri_sub(date,5,8),stri_sub(date,1,4))
  return(fecha)
}
# retorna el promedio de d - p y el promedio de la suma%
errorpredicted <- function(nemo,date) {
  table <- getforecast2(nemo,3,date)
  predicted <- getforecast2(nemo,2,date)
  t <- getforecast2(nemo,0,date)
  if(length(table)>1){
    predicted <- predicted[1:(nrow(predicted)-t),]
    table <- filter(table, table$fecha >= predicted$fecha[1])
    sum <- vector()
    sumporcentual <- vector()
    for(i in 1:nrow(table)){
      sum[i] <- (table$close_price[i]-predicted$close_price[i])
      sumporcentual[i] <- (sum[i]/table$close_price[i])*100
    }
    sum <- mean(sum)
    sumporcentual <- mean(sumporcentual)
    return(c(sum,sumporcentual))
  }else{
    return(-1)
  }
}
#  retorna 
acertividad <- function(nemo,date){
  date1 <- as.Date(date)
  date2 <- date1 - 1
  predicted <- getforecast2(nemo,2,date2)
  real <- getforecast2(nemo,3,date1)
  if(length(predicted)>1){
    predictcp <- dplyr::filter(predicted, predicted$fecha == date2)[1]
    realcp <-  dplyr::filter(real,real$fecha == date2)[1]
    diferencia <- realcp - predictcp
    diferenciaporcentual <- (diferencia/realcp)*100
    return(list(diferencia,diferenciaporcentual,realcp,predictcp))
  }else{
    return(-1)
  }
}
listaNemos <- c("CAMANCHACA","PEHUENCHE","AQUACHILE","SMU","RIPLEY","BSANTANDER","CAP","CENCOSUD","ENELAM","FALABELLA","SQM-B","VAPORES")
columnas <- c("Open","High","Low","Close","Volume","Adj_Close")
# estructura pagina ----
header <- dashboardHeader(title = "Dashboard")
sidebar <- dashboardSidebar( collapsed = TRUE,
                             sidebarMenu(
                               menuItem("Series nemotécnicos", tabName = "dashboard", icon = icon("dashboard")),
                               menuItem("Predicción", tabName = "widgets", icon = icon("th"))##,
                               ##menuItem("comparación bolsa/market", tabName = "widgets2", icon = icon("th"))
                             )
)
body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              column(width = 9,
                     box(title = "Grafico 1", status = "primary", width = NULL,
                         solidHeader = TRUE, collapsible = TRUE,
                         dygraphOutput("dyPlot1",height = "300px")),
                     box(title = "Grafico 2", status = "primary", width = NULL,
                         solidHeader = TRUE, collapsible = TRUE,
                         dygraphOutput("dyPlot2",height = "300px"))
              ),
              column(width = 3,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = NULL,
                         sliderInput("obs",
                                     "Number of observations:",20,2000,360),
                         selectInput("nemo",
                                     label = "Nemo",
                                     choices = listaNemos,
                                     selected = listaNemos[1]),
                         selectInput("var1", 
                                     label = "1) variable",
                                     choices = columnas,
                                     selected = "Close"),
                         selectInput("var2", 
                                     label = "2) variable",
                                     choices = columnas,
                                     selected = "Volume"),
                         checkboxGroupInput("checkGroup", label = h3("Ver:"), 
                                            choices = list("Datos" = 1, "Medias moviles" = 2, 
                                                           "varianzas moviles" = 3,"Asimetrias moviles"=4),
                                            selected = 1)
                     ) 
              )
            )
            
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            dateInput("date", 
                      h3("Date input"), 
                      value = Sys.Date(), min = "2017-10-25" , max = Sys.Date()),
            uiOutput("ui")
    ),
    tabItem(tabName = "widgets2",
            dateInput("date", 
                      h3("Date input"), 
                      value = Sys.Date(), min = "2017-10-25" , max = Sys.Date()),
            uiOutput("ui2")
    )
  )
)
# app ----
app <- shinyApp(
  #UI
  ui <- dashboardPage(header,sidebar,body),
  server <- function(input, output) {
    df <- reactive({
      input$nemo
    })
    output$dyPlot1 <- renderDygraph({
      nemo <- getTable(input$nemo)
      test <- numObs(nemo,input$obs)
      show <- whatshow(input$checkGroup)
      plotNormal(test,input$var1,df(),show)
    })
    output$dyPlot2 <- renderDygraph({
      nemo <- getTable(input$nemo)
      test <- numObs(nemo,input$obs)
      show <- whatshow(input$checkGroup)
      plotNormal(test,input$var2,df(),show)
    })
    output$ui <- renderUI({
      date <- input$date
      numAssets <- length(listaNemos)
      
      box(title = "Información",status = "warning", solidHeader = TRUE, width = NULL,
          lapply(1:numAssets, function(i) {
            
            acer <- acertividad(listaNemos[i],date)
            
            forecast <- getforecast2(listaNemos[i],1,date)
            
            error <- errorpredicted(listaNemos[i],date)
            
            x <- status(listaNemos[i],date)
            
            list(
              if(x>0){infoBox(
                listaNemos[i], paste0("Forecast para hoy: ",round(forecast[1],digits = 4)),
                subtitle = HTML(paste0("pendiente futura: ",round(x,digits = 4),'<br/>',
                                       "error promedio rnn: ",round(error[1],digits = 4),'<br/>',
                                       "error % rnn: ",round(error[2],digits = 4),"%",'<br/>',
                                       "CP real ayer: ",round(acer[[3]],digits = 4),'<br/>',
                                       "forecast de ayer: ",round(acer[[4]],digits = 4),'<br/>',
                                       "diferencia real/predicción: ",round(acer[[1]],digits = 4),'<br/>',
                                       "diferencia real/predicción: ",round(acer[[2]],digits = 4),"%")),
                color = "green",icon = icon("line-chart","fa-0.5x")
              )}else if(x<0){
                infoBox(
                  listaNemos[i], paste0("Forecast para hoy: ",round(forecast[1],digits = 4)),
                  subtitle = HTML(paste0("pendiente futura: ",round(x,digits = 4),'<br/>',
                                         "error promedio rnn: ",round(error[1],digits = 4),'<br/>',
                                         "error % rnn: ",round(error[2],digits = 4),"%",'<br/>',
                                         "CP real ayer: ",round(acer[[3]],digits = 4),'<br/>',
                                         "forecast de ayer: ",round(acer[[4]],digits = 4),'<br/>',
                                         "diferencia real/predicción: ",round(acer[[1]],digits = 4),'<br/>',
                                         "diferencia real/predicción: ",round(acer[[2]],digits = 4),"%")),
                  color = "red",icon = icon("line-chart","fa-0.5x"))
              }else if(x==FALSE){
                infoBox(
                  listaNemos[i], paste0("No hay predicción", date),
                  color = "black"
                )
              })  
          }))
      
    })
    output$ui2 <- renderUI({
      date <- input$date
      numAssets <- length(listaNemos)
      
      fluidRow(
        column(width = 4,
               box(title = "forecast bolsa",status = "warning", solidHeader = TRUE, width = NULL,
                   lapply(1:numAssets, function(i) {
                     
                     acer <- acertividad(listaNemos[i],date)
                     
                     forecast <- getforecast2(listaNemos[i],1,date)
                     
                     error <- errorpredicted(listaNemos[i],date)
                     
                     x <- status(listaNemos[i],date)
                     
                     list(box( title = listaNemos[i],width = NULL,
                               if(x>0){infoBox(
                                 listaNemos[i], paste0("Forecast para hoy: ",round(forecast[1],digits = 4)),
                                 subtitle = HTML(paste0("pendiente futura: ",round(x,digits = 4),'<br/>',
                                                        "error promedio rnn: ",round(error[1],digits = 4),'<br/>',
                                                        "error % rnn: ",round(error[2],digits = 4),"%",'<br/>',
                                                        "CP real ayer: ",round(acer[[3]],digits = 4),'<br/>',
                                                        "forecast de ayer: ",round(acer[[4]],digits = 4),'<br/>',
                                                        "diferencia real/predicción: ",round(acer[[1]],digits = 4),'<br/>',
                                                        "diferencia real/predicción: ",round(acer[[2]],digits = 4),"%")),
                                 color = "green",icon = icon("line-chart","fa-0.5x")
                               )}else if(x<0){
                                 infoBox(
                                   listaNemos[i], paste0("Forecast para hoy: ",round(forecast[1],digits = 4)),
                                   subtitle = HTML(paste0("pendiente futura: ",round(x,digits = 4),'<br/>',
                                                          "error promedio rnn: ",round(error[1],digits = 4),'<br/>',
                                                          "error % rnn: ",round(error[2],digits = 4),"%",'<br/>',
                                                          "CP real ayer: ",round(acer[[3]],digits = 4),'<br/>',
                                                          "forecast de ayer: ",round(acer[[4]],digits = 4),'<br/>',
                                                          "diferencia real/predicción: ",round(acer[[1]],digits = 4),'<br/>',
                                                          "diferencia real/predicción: ",round(acer[[2]],digits = 4),"%")),
                                   color = "red",icon = icon("line-chart","fa-0.5x"))
                               }else if(x==FALSE){
                                 infoBox(
                                   listaNemos[i], paste0("No hay predicción"),
                                   color = "black"
                                 )
                               }))  
                   }))),
        column( width = 2,
                img(src="versus.jpg", align = "right"),
                lapply(1:numAssets, function(i) {
                  list(img(src ="C:/Users/josseed/Google Drive/proyectos/scripts R/prototipo/versus.jpg"))
                })
        ),
        column(width = 4,
               box(title = "forecast market data",status = "warning", solidHeader = TRUE, width = NULL,
                   lapply(1:numAssets, function(i) {
                     
                     acer <- acertividad(listaNemos[i],date)
                     
                     forecast <- getforecast(listaNemos[i],1)
                     
                     error <- errorpredicted(listaNemos[i])
                     
                     x <- status(listaNemos[i],date)
                     
                     list(box( title = listaNemos[i],width = NULL,
                               if(x>0){infoBox(
                                 listaNemos[i], paste0("Forecast para hoy: ",round(forecast[1],digits = 4)),
                                 subtitle = HTML(paste0("pendiente futura: ",round(x,digits = 4),'<br/>',
                                                        "error promedio rnn: ",round(error[1],digits = 4),'<br/>',
                                                        "error % rnn: ",round(error[2],digits = 4),"%",'<br/>',
                                                        "CP real ayer: ",round(acer[[3]],digits = 4),'<br/>',
                                                        "forecast de ayer: ",round(acer[[4]],digits = 4),'<br/>',
                                                        "diferencia real/predicción: ",round(acer[[1]],digits = 4),'<br/>',
                                                        "diferencia real/predicción: ",round(acer[[2]],digits = 4),"%")),
                                 color = "green",icon = icon("line-chart","fa-0.5x")
                               )}else if(x<0){
                                 infoBox(
                                   listaNemos[i], paste0("Forecast para hoy: ",round(forecast[1],digits = 4)),
                                   subtitle = HTML(paste0("pendiente futura: ",round(x,digits = 4),'<br/>',
                                                          "error promedio rnn: ",round(error[1],digits = 4),'<br/>',
                                                          "error % rnn: ",round(error[2],digits = 4),"%",'<br/>',
                                                          "CP real ayer: ",round(acer[[3]],digits = 4),'<br/>',
                                                          "forecast de ayer: ",round(acer[[4]],digits = 4),'<br/>',
                                                          "diferencia real/predicción: ",round(acer[[1]],digits = 4),'<br/>',
                                                          "diferencia real/predicción: ",round(acer[[2]],digits = 4),"%")),
                                   color = "red",icon = icon("line-chart","fa-0.5x"))
                               }else if(x==FALSE){
                                 infoBox(
                                   listaNemos[i], paste0("No hay predicción"),
                                   color = "black"
                                 )
                               }))  
                   }))))
      
    })
  }
)
# Run the app ----
runApp(app,host="0.0.0.0",port=8080)