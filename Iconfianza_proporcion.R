#Creado por Sergio Guzmán Mercado 1/abril/2020

intervalo_proporcion <- function(x,n,i){
  #Datos:
  P<-(x/n)
  if(i<1){
    I<-i
  }
  else{
    I<-(i/100)
  }
  a<-(1-I)
  alfa<-(a/2)
  z<-(qnorm(alfa,0,1,lower.tail = FALSE))
  #Realizando el calculo del intervalo
  #Inferiror
  inferior<-(P-z*sqrt((P*(1-P))/n))
  superior<-(P+z*sqrt((P*(1-P))/n))
  cat("Intervalo de confianza: ",inferior,"< p <",superior)
}

intervalo_proporcion(3,50,95)


