#HECHO POR
#RAFAEL GONZALEZ
#CIMAT - PROBABILIDAD Y ESTADISTICA
#ANALISIS TOPOLOGICO DE DATOS
library(shiny)
library(plot3D)
library(TDA)
#primer bloque, funciones: e.dist, diam, C1.list, C2.list
#segundo bloque, funcion pinta
#tercer bloque, funcion rapido
##########################
eu.dist <- function(x,y){sqrt(sum((x-y)^2))}

#X es la nube de puntos en R^3 
#la quiero entender como una matriz
#n*3, donde n es la cantidad de puntos
#es decir la primera columna son las coordenadas en X
#la segunda columna las coordenadas en Y
#la tercera columna las coordenadas en Z
diam <- function(X){
  n <- nrow(X)
  d <- 0
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      d <- max(d,eu.dist(X[i,],X[j,]))
    }
  }
  return(d)
}
#calculo de C_1(X) para un r>0 y X una nube de puntos (list)
C1.list <- function(r,X){
  C1 <- list()
  for(i in 1:(nrow(X)-1)){
    for(j in (i+1):nrow(X)){
      if(eu.dist(X[i,],X[j,])<2*r){
        Caux <- list(c(i,j))
        C1 <- c(C1,Caux)
      }
    }
  }
  return(C1)
}
#calculo de C_2(X) para un r>0 y X una nube de puntos (list)
C2.list <- function(r,X){
  C2 <- list()
  for(i in 1:(nrow(X)-2)){
    for(j in (i+1):(nrow(X)-1)){
      for(k in (j+1):nrow(X)){
        if(diam(X[c(i,j,k),])<2*r){
          Caux <- list(c(i,j,k))
          C2 <- c(C2,Caux)
        }
      }
    }
  }
  return(C2)
}
###################
#Para pintar las funciones C1 y C2 con plot3D
pinta <- function(C,X){
  n <- length(C)
  for(k in 1:n){
    x <- X[C[[k]],1] #tiene que tener las cordenadas de x
    y <- X[C[[k]],2]
    z <- X[C[[k]],3]
    polygon3D(x,y,z,border=1,lwd=2,col="lawngreen",alpha=0.8,add=T)
  }
}
######################
#rapido, es una funcion que dado un r, grafica el complejo VR de radio r
rapido <- function(r,X,phi,theta){
  x <- X[,1]
  y <- X[,2]
  z <- X[,3]
  n <- length(x)
  scatter3D(x,y,z,colvar=NULL,pch=20,phi=phi,theta=theta,
            col="blue",ticktype = "detailed",nticks=2,
            main=paste("Radio = ",2*r,". Phi = ",phi,". Theta = ",theta,
                       ". n = ",n))
  C1 <- C1.list(r,X) 
  C2 <- C2.list(r,X)
  if(length(C1)>0){
    pinta(C1,X)
  }
  if(length(C2)>0){
    pinta(C2,X)
  }
}

#################

shinyServer(function(input, output) {
  #data <- reactive({
  #  matrix(runif(3*input$n),nrow=input$n,ncol=3)  #datos
  #})
  data <- reactive({
    sphereUnif(input$n,2)  #datos
  })
  
  Diag <- reactive({
    ripsDiag(X=data(),maxdimension = 3,printProgress=T,maxscale=3)
  })

  output$caption <- renderPrint({
    paste("dist=",input$dist)
  })
  output$grafica <- renderPlot({
    #par(mfrow = c(2,1))
    phi <- input$phi
    theta <- input$theta
    r <- input$r
    x <- data()[,1]
    y <- data()[,2]
    z <- data()[,3]
    scatter3D(x,y,z,colvar=NULL,phi=phi,theta=theta,
              pch=20,col="blue",ticktype = "detailed",nticks=2)
    rapido(r/2,data(),phi,theta)
    #plot(Diag$diagram, barcode=T, main= "Barcode")
    #abline(v=r,col=2,lty=2)
    #scatter3D(x,y,z,colvar=NULL,#phi=phi,theta=theta,
    #          pch=20,col="blue",ticktype = "detailed",nticks=2)
    
  })
  output$grafica2 <- renderPlot({
    #Diag <- ripsDiag(X=data(),maxdimension = 3,printProgress=T,maxscale=1,
    #                 location=T,library="Dionysus")
    r <- input$r
    plot(Diag()$diagram, barcode=T, main= "Barcode")
    abline(v=r,col=2,lty=2,lwd=2)
  })
})
