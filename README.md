# Calculo de Numeros de Betti en R

##Objetivo

Reproducir los algoritmos para calcular los números de Betti en R3 en el lenguaje de programación R.
Presentar los resultados en un formato interactivo que pueda ser utilizado sin conocimiento previo de R.

## Resumen

En este proyecto se desarrolla un algoritmo para calcular los números de Betti a partir de una filtración
del complejo de Vietoris-Rips para un radio fijo. Se utiliza la matriz de Frontera y la forma normal de
Smith para calcular los números de Betti. La bibliografía utilizada son las Notas de Espinosa L. Malors
disponibles en la página de ATD de CIMAT y el libro “Computational Topology” de H. Edelsbrunner y
J. Harer. Posteriormente se utilizan estos algoritmos en conjunto con el paquete TDA en R para generar
una aplicación interactiva que permita a un usuario, sin necesidad de conocer R, visualizar la evolución de
la homología en un complejo simplicial a medida que varía el radio de la filtración de Vietoris-Rips. Esta
aplicación se creó utilizando el paquete “Shiny” en Rstudio.
