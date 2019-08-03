
#Si enumeramos todos los números naturales por debajo de 10 que son múltiplos de 3 o 5, obtenemos 3, 5, 6 y 9. 
#La suma de estos múltiplos es 23.

#Encuentra la suma de todos los múltiplos de 3 o 5 por debajo de 1000.

Problema_1 <- function(numero){
  
  tabla <- data.frame(x = seq(numero))
  tabla$y <- ifelse(0 == tabla$x %% 3 | 0 == tabla$x %% 5,1,0)
  sum(tabla[which(tabla$y == 1),1]) - tail(tabla[which(tabla$y == 1),1],1)
  }

Problema_1(1000)


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
sum(tabla[which(tabla$x < 4000000 & tabla$y ==0),])

#Los factores primos de 13195 son 5, 7, 13 y 29.

#¿Cuál es el factor primo más grande del número 600851475143?

#Funcion que encuentra numero es primo
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

10 %% 5

x = c(2,3,5,7,11,13,17,19,23,24,31,37,41,43)
















