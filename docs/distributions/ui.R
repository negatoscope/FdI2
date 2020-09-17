library(shiny)
tabPanelAbout <- source("about.r")$value
library(shinythemes)
shinyUI(fluidPage(theme=shinytheme("united"),
	headerPanel(
		HTML('Distribuciones de Probabilidad'
		), "Distribuciones de Variables Aleatorias"
	),
	fluidRow(
		column(4,
			wellPanel( radioButtons("disttype","Tipo de Distribución:",list("Discreta","Continua"),selected="Discreta") ),
			wellPanel(	uiOutput("distName") ),
			wellPanel(
				numericInput("n","Tamaño muestral:",10000),
				uiOutput("dist1"),
				uiOutput("dist2"),
				uiOutput("dist3")
			),
			wellPanel(
				uiOutput("sampDens"),
				uiOutput("BW"),
				fluidRow(
					column(6, downloadButton("dlCurPlot", "Descargar Gráfica", class="btn-block btn-primary")),
					column(6, downloadButton("dldat", "Descargar Muestra", class="btn-block btn-warning"))
				)
			)
		),
		column(8,
			tabsetPanel(
				tabPanel("Gráfica",plotOutput("plot", width="100%", height="auto")),
				tabPanel("Resumen",verbatimTextOutput("summary")),
				tabPanel("Datos",tableOutput("table")),
				id="tsp"
			)
		)
	)
))
