############
#Ejericio 1#
############
#Si enumeramos todos los números naturales por debajo de 10 que son múltiplos de 3 o 5, obtenemos 3, 5, 6 y 9. 
#La suma de estos múltiplos es 23.

#Encuentra la suma de todos los múltiplos de 3 o 5 por debajo de 1000.

Problema_1 <- function(numero){
  
  tabla <- data.frame(x = seq(numero))
  tabla$y <- ifelse(0 == tabla$x %% 3 | 0 == tabla$x %% 5,1,0)
  sum(tabla[which(tabla$y == 1),1]) - tail(tabla[which(tabla$y == 1),1],1)
  }

Problema_1(1000)

############
#Ejercio 2 #
############

#Cada nuevo término en la secuencia de Fibonacci se genera agregando los dos términos anteriores.
#Al comenzar con 1 y 2, los primeros 10 términos serán:
  
  #1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

#Al considerar los términos en la secuencia de Fibonacci cuyos valores no exceden los cuatro millones,
#encuentre la suma de los términos de valor par.

x <- c(1,2)

for(i in 2:40){
  x[i+1] <-  print(x[i] + x[i-1])
  }

tabla <- data.frame(x = x,y = x %% 2)
#La suma con terminos menores a 4 millones y pares es:
sum(tabla[which(tabla$x < 4000000 & tabla$y ==0),])

#############
#Ejercicio 3#
#############

#Los factores primos de 13195 son 5, 7, 13 y 29.

#¿Cuál es el factor primo más grande del número 600851475143?

#Antes de contestar la pregunta realize 3 funciones, una para preguntar si mi numero es primo funcion es_primo(), otra para encontrar una lista de valores primos, funcion encontrar_primos()
#y por ultimo por encontrar factores primos de un numero

##Funcion que encuentra numero es primo
es_primo <- function(num){
  
  contador = 0
  #Cuantos numeros son divisores tal que el residuo es 0
  for(i in 1:num){
    if(num %% i == 0){
      contador = contador + 1}}
  #Si solo tuvo 2 divisores 1 y el mismo numero, es primo
  if(contador == 2){
    print("Es primo")}
  else{print("No es primo")}
  }
  
es_primo(100)

##Encontrar numeros primos

encontrar_primos <- function(inicial, final){
  x = c()
  
  for(n in inicial:final){
    contador = 0
    for(i in 1:final){
      if(n %% i == 0){
        contador = contador + 1}
      }
    if(contador == 2){
      x <- c(x,n)
    }
  }
  print(x)
  }

x <- encontrar_primos(1,100)
x <- encontrar_primos(1527,1632)

##Descomponer un numero en sus factores primos

Factores_primos <- function(numero){

  x = c()
  
  for(i in 2:numero){
  #Valida cuantas veces puedo dividir por cada numero de (2:numero) 
  #En el momento que ya no puedo dividir entre el numero sigue al siguiente numero del for 
    while(numero %% i == 0) {
      #Forma de encontrarlo de forma clasica
      x <- c(x,i)
      numero = numero / i
                            }
                    }
  print(x)
                                  }

Factores_primos(13195)

# Factores_primos(600851475143) da un error por el tamaño

#Obte por ocupar mi funcion encontrar primos e ir descomponiendo mi numerote
encontrar_primos(1,100)

tabla <- data.frame(x = 600851475143 %% encontrar_primos(1,1000), y = encontrar_primos(1,1000))

tabla[which(tabla$x == 0),]

((600851475143 / 71)/839)

tabla <- data.frame(x = 10086647 %% encontrar_primos(1001,2000), y = encontrar_primos(1001,2000))

tabla[which(tabla$x == 0),]

10086647/1471

tabla <- data.frame(x = 6857 %% encontrar_primos(2001,3000), y = encontrar_primos(2001,3000))

tabla[which(tabla$x == 0),]

#Por lo tanto: el maximo factor primo del numero 600851475143 es 6857 :)
6857*1471*839*71 == 600851475143


a = 1
