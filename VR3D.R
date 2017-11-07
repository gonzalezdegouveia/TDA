#CORRER PRIMERO

#HECHO POR
#RAFAEL GONZALEZ
#CIMAT - PROBABILIDAD Y ESTADISTICA
#ANALISIS TOPOLOGICO DE DATOS
rm(list=ls())
#distancia euclideana entre dos vectores
eu.dist <- function(x,y){sqrt(sum((x-y)^2))}

#otra pueba de commit
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

#calculando C3, los tetrahedros
#para un r>0 y X una nube de puntos (list)
C3.list <- function(r,X){
  C3 <- list()
  for(i in 1:(nrow(X)-3)){
    for(j in (i+1):(nrow(X)-2)){
      for(k in (j+1):(nrow(X)-1)){
        for(l in (k+1):nrow(X)){
          if(diam(X[c(i,j,k,l),])<2*r){
            Caux <- list(c(i,j,k,l))
            C3 <- c(C3,Caux)
          }
        }
      }
    }
  }
  return(C3)
}



#pinta1 es funcion para graficar los 1-simplejos del VR(r),
#suponemos que C es una lista con los 1-simplejos dada por C1.list
#y la X es la nube de puntos
pinta1 <- function(C,X){
  n <- length(C)
  for(k in 1:n){
    xyz <- X[C[[k]],]    #los puntos del k-esimo 1-simplejo
    lines3d(xyz,col=4)
  }
}
#pinta2 es funcion para graficar los 2-simplejos del VR(r),
#suponemos que C es una lista con los 2-simplejos dada por C2.list
#y la X es la nube de puntos
pinta2 <- function(C,X,a=1){
  n <- length(C)
  for(k in 1:n){
    xyz <- X[C[[k]],]    #los puntos del k-esimo 2-simplejo
    triangles3d(xyz,col="lawngreen",alpha = a)
  }
}

#EJEMPLOS
#library(rgl)
#con datos normales
#n <- 10   #numero de puntos
#X <- matrix(rnorm(3*n),nrow=n,ncol=3)  #datos
#r <- 0.5
#C1 <- C1.list(r,X) 
#C2 <- C2.list(r,X)
#C3 <- C3.list(r,X)
#plot3d(X, col = 4, size = 4)
#pinta2(C2,X,a=1) #a para transparencia entre 0 y 1 (1 es opaco).
#pinta1(C1,X)
#text3d(X,texts = 1:n, adj = 1.5)

#con datos uniformes
#library(rgl)
#n <- 30   #numero de puntos
#X <- matrix(runif(3*n),nrow=n,ncol=3)  #datos
#r <- 0.5
#C1 <- C1.list(r,X) 
#C2 <- C2.list(r,X)
#C3 <- C3.list(r,X)
#plot3d(X, col = 4, size = 4,aspect = 1,main=paste("r=",r),
#       xlab="x",ylab="y",zlab="z")
#pinta2(C2,X,a=0.5) #a para transparencia entre 0 y 1 (1 es opaco).
#pinta1(C1,X)
#text3d(X,texts = 1:n, adj = 1.5)
#write.csv(X,file="ejemplo.csv")
#
#
#library(TDA)
#X <- sphereUnif(20,2)#datos de una esfera
#X <- torusUnif(10,1,6)
#plot3d(X, col = 4, size = 4,zlim=c(-6,6))
