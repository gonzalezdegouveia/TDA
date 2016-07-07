#HECHO POR
#RAFAEL GONZALEZ
#CIMAT - PROBABILIDAD Y ESTADISTICA
#ANALISIS TOPOLOGICO DE DATOS
library(shiny)
library(plot3D)
######################
shinyUI(fluidPage(
  titlePanel("Visualizacion Complejo VR en 3D (Esfera)"),
  
  sidebarLayout(
    sidebarPanel( 
      numericInput("n", label = "Numero de datos", 
                   value = 10,min = 5,max = 50,step = 5),
      #
      sliderInput("phi",label="Phi, arriba-abajo",value = 0,min = -180,max = 180,
                  animate=animationOptions(interval=60, loop=TRUE)),
      sliderInput("theta",label="Theta, derecha-izquierda",value = 90,min = 0,max = 360,
                  animate=animationOptions(interval=60, loop=TRUE)),
      #numericInput("r",label="Radio complejo VR",
      #             value =0.2,min = 0.1,max = 4,step=0.05)
      sliderInput("r",label="Radio complejo VR",value =0.1,min = 0.1,max = 3,
                  animate=animationOptions(interval=240, loop=TRUE))
    ),
    mainPanel(
      plotOutput("grafica",height = "300px"),
      plotOutput("grafica2",height = "200px"),
      width = 8
  )
))
)
