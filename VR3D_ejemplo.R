#CORRER TERCERO

#HECHO POR
#RAFAEL GONZALEZ
#CIMAT - PROBABILIDAD Y ESTADISTICA
#ANALISIS TOPOLOGICO DE DATOS

#ahora todo completo para un ejemplo
library(rgl)
######################
#EJEMPLO 1: UNIFORMES#
######################
#los datos (uniformes) y el radio del complejo VR
n <- 5   #numero de puntos
n <- 10
X <- matrix(runif(3*n),nrow=n,ncol=3)  #datos
r <- 0.5

#grupos de p-cadenas
C1 <- C1.list(r,X) 
C2 <- C2.list(r,X)
C3 <- C3.list(r,X)

#grafica
plot3d(X, col = 4, size = 4,aspect = 1)
pinta2(C2,X,a=0.5) #a para transparencia entre 0 y 1 (1 es opaco).
pinta1(C1,X)
text3d(X,texts = 1:n, adj = 1.5)

#matrices de borde
bordeC1 <- borde1(C1,X)
bordeC2 <- borde2(C1,C2)
bordeC3 <- borde3(C2,C3)

#reduccion matricial
R1 <- reduce.borde(1,bordeC1)
R2 <- reduce.borde(1,bordeC2)
R3 <- reduce.borde(1,bordeC3)

#numeros de beti
beta0 <- nrow(X)-sum(diag(R1))
beta1 <- (ncol(R1)-sum(diag(R1)))-sum(diag(R2))
beta2 <- (ncol(R2)-sum(diag(R2)))-sum(diag(R3))


###################
#EJEMPLO 2: ESFERA#
###################
library(TDA)
#los datos (uniformes) y el radio del complejo VR
n <- 50   #numero de puntos
X <- sphereUnif(n,2) #n datos de una esfera de radio 2
r <- 0.65

#grupos de p-cadenas
C1 <- C1.list(r,X) 
C2 <- C2.list(r,X)
C3 <- C3.list(r,X)

#grafica
plot3d(X, col = 4, size = 4,aspect = 1)
#pinta2(C2,X,a=0.5) #a para transparencia entre 0 y 1 (1 es opaco).
pinta1(C1,X)
#text3d(X,texts = 1:n, adj = 1.5)

#matrices de borde
bordeC1 <- borde1(C1,X)
bordeC2 <- borde2(C1,C2)
bordeC3 <- borde3(C2,C3)

#reduccion matricial
R1 <- reduce.borde(1,bordeC1)
R2 <- reduce.borde(1,bordeC2)
R3 <- reduce.borde(1,bordeC3)

#numeros de beti
beta0 <- nrow(X)-sum(diag(R1))
beta1 <- (ncol(R1)-sum(diag(R1)))-sum(diag(R2))
beta2 <- (ncol(R2)-sum(diag(R2)))-sum(diag(R3))


#################
#EJEMPLO 3: TORO#
#################
#n datos y r radio del complejo VR
n <- 100
r <- 2
a <- 1
c <- 6
X <- torusUnif(n,a,c) #n numero de datos
                      #a the radius of the torus tube
                      #c the radius from the center of the 
                      #    to the center of the torus tube

#grupos de p-cadenas
C1 <- C1.list(r,X) 
C2 <- C2.list(r,X)
C3 <- C3.list(r,X)

#grafica
plot3d(X, col = 4, size = 4,zlim=c(-c,c))
pinta2(C2,X,a=0.5) #a para transparencia entre 0 y 1 (1 es opaco).
pinta1(C1,X)
#text3d(X,texts = 1:n, adj = 1.5)

#matrices de borde
bordeC1 <- borde1(C1,X)
bordeC2 <- borde2(C1,C2)
bordeC3 <- borde3(C2,C3)

#reduccion matricial
R1 <- reduce.borde(1,bordeC1)
R2 <- reduce.borde(1,bordeC2)
R3 <- reduce.borde(1,bordeC3)

#numeros de beti
beta0 <- nrow(X)-sum(diag(R1))
beta1 <- (ncol(R1)-sum(diag(R1)))-sum(diag(R2))
beta2 <- (ncol(R2)-sum(diag(R2)))-sum(diag(R3))

#################
#EJEMPLO 4: DADO#
#################

p1 <- c(0,0,0)
p2 <- c(0,0,1)
p3 <- c(0,1,0)
p4 <- c(0,1,1)
p5 <- c(1,0,0)
p6 <- c(1,0,1)
p7 <- c(1,1,0)
p8 <- c(1,1,1)
X <- rbind(p1,p2,p3,p4,p5,p6,p7,p8)
r <- 1.6/2
D <- ripsDiag(X,maxdimension = 3,maxscale = 3)
plot(D$diagram,barcode = T)
#grupos de p-cadenas
C1 <- C1.list(r,X) 
C2 <- C2.list(r,X)
C3 <- C3.list(r,X)

#grafica
plot3d(X, col = 4, size = 4,zlim=c(-1,2),
       ylim=c(-1,2),xlim=c(-1,2))
pinta2(C2,X,a=0.4) #a para transparencia entre 0 y 1 (1 es opaco).
pinta1(C1,X)
#text3d(X,texts = 1:n, adj = 1.5)

#matrices de borde
bordeC1 <- borde1(C1,X)
bordeC2 <- borde2(C1,C2)
bordeC3 <- borde3(C2,C3)

#reduccion matricial
R1 <- reduce.borde(1,bordeC1)
R2 <- reduce.borde(1,bordeC2)
R3 <- reduce.borde(1,bordeC3)

#numeros de beti
beta0 <- nrow(X)-sum(diag(R1))
beta1 <- (ncol(R1)-sum(diag(R1)))-sum(diag(R2))
beta2 <- (ncol(R2)-sum(diag(R2)))-sum(diag(R3))



