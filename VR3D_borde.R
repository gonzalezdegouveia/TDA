#CORRER SEGUNDO

#HECHO POR
#RAFAEL GONZALEZ
#CIMAT - PROBABILIDAD Y ESTADISTICA
#ANALISIS TOPOLOGICO DE DATOS

#################
#MATRIZ DE BORDE#
#################

#La matrix 1-borde
#arroja una matriz con tantas columnas como elementos de C1 (lineas)
#y tantas filas como elementos de C0 (los puntos)
borde1 <- function(C1,X){
  n0 <- nrow(X)
  n1 <- length(C1)
  borde1 <- matrix(0,nrow = n0,ncol=n1)
  for(i in 1:n0){
    for(j in 1:n1){
      if(i%in%C1[[j]]==TRUE){
        borde1[i,j] <- 1
      }
    }
  }
  return(borde1)
}

#funcion para calcular matriz de borde2, es decir,
#tengo tantas columnas como elementos en C2 (triangulos)
#y tantas filas como elementos de C1 (lineas)
borde2 <- function(C1,C2){
  n1 <- length(C1)
  n2 <- length(C2)
  borde2 <- matrix(0,nrow = n1,ncol=n2)
  for(i in 1:n1){
    for(j in 1:n2){
      if(min(C1[[i]]%in%C2[[j]])==1){
        borde2[i,j] <- 1
      }
    }
  }
  return(borde2)
}

#matriz de borde 3, esto es, 
#tengo tantas columnas como elementos en C3
#tantas filas como elementos en C2
borde3 <- function(C2,C3){
  n2 <- length(C2)
  n3 <- length(C3)
  borde2 <- matrix(0,nrow = n2,ncol=n3)
  for(i in 1:n2){
    for(j in 1:n3){
      if(min(C2[[i]]%in%C3[[j]])==1){
        borde2[i,j] <- 1
      }
    }
  }
  return(borde2)
}

#####################
#REDUCCION MATRICIAL#
#####################
#ahora dada la matriz de borde Dp tenemos que hacer la 
#reduccion normal smith para encontrar los rangos
#de Zp y Bp-1, quiero la funcion REDUCE.

#antes necesitamos la funcion checar
#esta funcion nos dice si quedan elementos a reducir
#esto es, si alguno de los elementos en la submatriz es 1
#la primera entrada es 1 si hay que reducir. 0 si la subamtriz ya es cero
#la segunda entreada el la fila donde esta el 1
#la tercera entrada es la columna donde esta el 1
checar <- function(x,N){
  np_1 <- nrow(N)
  np <- ncol(N)
  k <- 0
  aux <- 0
  for(i in x:np_1){
    for(j in x:np){
      if(N[i,j]==1){
        aux <- 1
        break
      }
    }
    if(aux==1){
      break
    }
  }
  if(i<np_1|j<np){
    k <- 1
  }
  return(c(k,i,j))
}
#para cambiar las filas n,m de una matriz
#A <- matrix(1:16,4,4,byrow=T)
#A[,c(2,3)] <- A[,c(3,2)] #para intercambiar las columnas 2 y 3
#A[c(1,4),] <- A[c(4,1),] #para intercambiar las filas 1 y 4C.1 <- bordeC1

#algoritmo para sumar la columna k a la columna l
addcol <- function(N,k,l){ 
  np_1 <- nrow(N)
  np <- ncol(N)
  V <- matrix(0,nrow=np,ncol=np)
  V[k,l] <- 1
  A <- diag(np)+V
  return(N%*%A%%2)
}
#algoritmo para sumar la fila k a la fila l
addrow <- function(N,k,l){ 
  np_1 <- nrow(N)
  np <- ncol(N)
  U <- matrix(0,nrow=np_1,ncol=np_1)
  U[l,k] <- 1
  A <- diag(np_1)+U
  return(A%*%N%%2)
}
#en la funcion reduce, x es un numero del indice en la diagonal
#N es la matriz de borde
#la salida de esta funcion es una matriz del mismo tamano que N
#pero reducida a la forma normal de smith
reduce.borde <- function(x,N){
  #print("###########################")
  #print(x)
  #print(N)
  np_1 <- nrow(N)
  np <- ncol(N)
  if(x<=np){
    a <- checar(x,N) #contiene la informacion de checar(x,N)
  }else{a <- 0}
  #print(a)
  if(a[1]==1){
    N[c(x,a[2]),] <- N[c(a[2],x),] #cambio de filas
    N[,c(x,a[3])] <- N[,c(a[3],x)] #cambio de columnas
    for(i in (x+1):np_1){
      if(N[i,x]==1){
        #add row x to i
        N <- addrow(N,x,i)
      }
    }
    if((x+1)<np){
      for(j in (x+1):np){
        if(N[x,j]==1){
          #add column x to j
          N <- addcol(N,x,j)
        }
      }
    }  
    N <- reduce.borde((x+1),N)
  }
  return(N)
}

#bordeC1 <- borde1(C1,X)
#bordeC2 <- borde2(C1,C2)
#bordeC3 <- borde3(C2,C3)

#R1 <- reduce.borde(1,bordeC1)
#R2 <- reduce.borde(1,bordeC2)
#R3 <- reduce.borde(1,bordeC3)

#beta0 <- nrow(X)-sum(diag(R1))
#beta1 <- (ncol(R1)-sum(diag(R1)))-sum(diag(R2))
#beta2 <- (ncol(R2)-sum(diag(R2)))-sum(diag(R3))

