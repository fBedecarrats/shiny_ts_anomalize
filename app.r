# shiny app para series
#### config ####
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(lubridate)
library(anomalize)
library(DT)
library(plotly)
# install.packages("DT")
# install.packages("plotly")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("stringi")
# install.packages("anomalize")
# install.packages("lubridate")
#### datos ####
path_archivo <- paste0(getwd(),"/indicadores_algodonera_simple.csv")
datos <- read.csv(path_archivo,
                      sep = ",",header = TRUE,
                      stringsAsFactors = FALSE) %>% 
  mutate(fecha = ymd(indice_tiempo)) %>% 
  mutate(anio = year(indice_tiempo)) %>% 
  mutate(mes = month(indice_tiempo)) %>%
  mutate(dia =day(indice_tiempo)) %>%
  filter(alcance_tipo=="PROVINCIA") %>%
  filter(mes==12 & dia==31) %>%
  select(fecha,anio,alcance_nombre,sup_sembrada_algodon,sup_cosechada_algodon,prod_algodon) %>%
  rename(provincia = alcance_nombre) %>% 
  gather(key = variable, # new var
         value = valor,  # new value
         sup_sembrada_algodon,sup_cosechada_algodon,prod_algodon)  # vars to remove
#  group_by(fecha,anio,alcance_nombre,variable) %>%

  
# filtrar sin cultivo
exclude_sin_cultivo <- datos %>% 
  group_by(provincia,variable) %>% summarise(avg_valor=mean(valor)) %>% 
  filter(avg_valor == 0) %>% select(provincia) %>% unique() %>% as.data.frame(stringsAsFactors=FALSE) 

datos <- datos %>% filter(! ( provincia %in% exclude_sin_cultivo$provincia ))

# datos %>% filter( provincia %in% exclude_sin_cultivo$provincia ) %>% glimpse()
# datos %>% filter( !(provincia %in% exclude_sin_cultivo$provincia) ) %>% glimpse()

# datos %>% glimpse()
# datos %>% group_by(anio,provincia,variable) %>% tally() %>% filter (n>1)

# datos %>% filter(anio==1995,variable=="prod_algodon",provincia=="CAPITAL FEDERAL") %>%
#   filter(mes=="12" & dia == "31")
# glimpse(datos)
# datos %>% group_by(ciaID) %>% tally()

#### ui ####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      # consideracion: inflacion
      # input numerico de trend 
      # ( helper que explique que es el periodo considerado para analizar y 
      # considerar que es un outlier)
      numericInput("trend_value", "Trend",value =  12,min =  2),
      
      ##  control
      # slider para restringir periodo analizado entre T1 y T2 
      # popular segun dataset
      sliderInput("fechas", "Fechas",
                  value = c(quantile(datos$anio,probs = 0.25),
                            quantile(datos$anio,probs = 0.75)), 
                  min = min(datos$anio), max = max(datos$anio)),
      
      # select provincias
      # popular segun dataset
      selectInput("provincia", "Provincias",
                  choices = levels(as.factor(datos$provincia)),
                  multiple = FALSE,
                  selected = levels(as.factor(datos$provincia))[1]),
      # select provincias
      # popular segun dataset
      selectInput("vars", "Variables",
                  choices = levels(as.factor(datos$variable)),
                  multiple = FALSE,
                  selected = levels(as.factor(datos$variable))[1]),
      
      # plot type
      radioButtons("tipo_plot","Tipo Plot",choices = c("plotly","ggplot"))
    ),
    
    ## main
    # plot c / outliers
    # datatable
    mainPanel(
      # plotlyOutput("plot"),
      conditionalPanel("input.vars != '' && input.provincia!='' && output.cantidad_registros_text!=0 ",
                       conditionalPanel("input.tipo_plot == 'plotly'", plotlyOutput("plotly")),
                       conditionalPanel("input.tipo_plot == 'ggplot'", plotOutput("plot"))
      ),
      conditionalPanel("input.vars != ''&& input.provincia!='' && output.cantidad_registros_text!=0 ",DT::dataTableOutput("datos_asociados"))
    )
  )
)

#### ui ####
server <- function(input, output,session) {
  
  datos_reactive_filter <- reactive({
    req(input$fechas)
    req(input$provincia)
    req(input$vars)
    date_init_tmp <- paste0(input$fechas[1],"0101")
    date_fin_tmp <- paste0(input$fechas[2],"1231")
    
    resultado <- datos %>%
      filter(provincia == input$provincia )%>% # filtro provincias
      filter(variable == input$vars )%>% # filtro provincias
      filter(fecha >= ymd(date_init_tmp) & 
               fecha <= ymd(date_fin_tmp) )
    
    #glimpse(resultado)
  })
  
  datos_reactive <- reactive({
    datos_reactive_filter() %>%
      rename(date=fecha,count=valor,package=variable) 
  })
  
  datos_anomalies_reactive <- reactive({
    req(input$trend_value)
    reactive_result <- datos_reactive() %>% as_tibble() %>%
      # STL + IQR Anomaly Detection
      time_decompose(count, method = "stl", 
                     trend = paste0(input$trend_value," months")) %>%
      anomalize(remainder, method = "iqr") %>%
      time_recompose()
    # glimpse(reactive_result)
    reactive_result
  })
  
  output$cantidad_registros_text <- reactive({
    nrow(datos_reactive_sin_ramo())
  })
  
  output$plotly <- renderPlotly({

    tmp_plot <- datos_anomalies_reactive() %>%
      # Anomaly Visualization
      plot_anomalies(time_recomposed = TRUE) +
      # facet_wrap(~ package, scale = "free_y", ncol = 3) +
      labs(title = "Produccion Mensual Cias Aseguradoras Anomalias", subtitle = "STL + IQR Methods")

    # https://plot.ly/r/shinyapp-plotly-events/
    ggplotly(tmp_plot)
    #tmp_plot
  })
  
  output$plot <- renderPlot({
    tmp_plot <- datos_anomalies_reactive() %>%
      # Anomaly Visualization
      plot_anomalies(time_recomposed = TRUE) +
      # facet_wrap(~ package, scale = "free_y", ncol = 3) +
      labs(title = "Produccion Mensual Cias Aseguradoras Anomalias", subtitle = "STL + IQR Methods")
    tmp_plot
  })
  
  output$datos_asociados <- DT::renderDataTable({
    current_data <- datos_reactive() %>% 
      left_join(datos_anomalies_reactive(),by=c('date')) %>%
      mutate(anomalia=if_else(anomaly=='No','No','Si')) %>%
      select(colnames(datos_reactive()),anomalia)%>%
      rename(fecha=date,valor=count,provincia=package) 
    # current_data %>% 
    DT::datatable(current_data )
    
  })
}

shinyApp(ui, server)