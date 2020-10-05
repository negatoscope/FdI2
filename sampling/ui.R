# ------------------
# App Title: Sampling distribution demonstration
#    Author: Gail Potter
# ------------------


if (!require("devtools")) install.packages("devtools")

if (!require("shinyBS")) install.packages("shinyBS")
library(shinyBS)

if (!require(shinyIncubator)) devtools::install_github("rstudio/shiny-incubator")
library(shinyIncubator)

if (!require("shinysky")) devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)


shinyUI(fluidPage(
  includeCSS('styles.css'),
  
  #progressInit(),
  
  tags$head(tags$link(rel = "icon", type = "image/x-icon", href =  
                        "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),  
  
  h3("Distribuciones muestrales"),
  fluidRow(
    column(3,
           wellPanel( 
             selectInput("popdist", label = h5("Tipo de distribución"), 
                         choices = list("Normal" = "normal"
                                        #"Left-skewed" = "left.skewed","Uniform" = "uniform", "Right-skewed" = "right.skewed",
                                        #"Bimodal"="bimodal"
                                        ), selected = "normal"),
             br(),
             shinyalert("shinyalert3", TRUE, auto.close.after=5),
             numericInput("popmean", label = h5("Media"), value=0),
             br(),
             shinyalert("shinyalert4", TRUE, auto.close.after=5),
             numericInput("popsd", label = h5("Desviación estándar"), value=1),
             br(),
             shinyalert("shinyalert1", TRUE, auto.close.after=5),
             
             numericInput("n", label=h5("Tamaño de la muestra"), value=10, min=1, max=1000),
             selectInput("statistic", label = h5("Estadístico"), 
                         choices = list("Media" = "media", "Desviación estándar" = "standard deviation"
                                        #"Median" = "median",
                                        #"1st quartile (Q1)" = "Q1",
                                        #"3rd quartile (Q3)" = "Q3", 
                                        #"Maximum"="maximum", "Minimum"="minimum"
                                        ), selected = "media"))
           
    ),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    column(9, wellPanel(
      p("Selecciona en el panel izquierdo el tipo de distribución, tamaño de la muestra y estadístico de interés.
Para obtener una muestra aleatoria presiona el botón 'Extraer muestras'. A la izquierda observarás un histograma de la última muestra extraída. El estadístico recién calculado
será añadido al histograma derecho. Al aumentar el número de muestras extraídas podrás ver un cambio la forma de esa distribución, la 'distribución muestral' de ese estadístico
(media o desviación estándar)"),
      shinyalert("shinyalert2", TRUE, auto.close.after=5),
      
      numericInput("nsim", label=h5("Número de muestras"), value=1, min=1, max=1000000),
      actionButton("go", label = "Extraer muestras"),
      actionButton("clear",label="Borrar"),
      
      bsCollapse(multiple = FALSE, open = NULL, id = "collapse1",
                 bsCollapsePanel("Haz clic aquí para mostrar la distribución de la población", 
                                 plotOutput("popdistn", height="200px"), 
                                 id="popcurve", value="test3")
      ) ,
      
      plotOutput("dotplot", height="290px"),
      textOutput("numsims"),
      checkboxInput("display", label="Mostrar el resumen de los resultados de la distribución muestral"),
      htmlOutput("display")
    ))
  )
  
))