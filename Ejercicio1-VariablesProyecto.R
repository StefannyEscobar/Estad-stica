Anime_limpio <- read.csv("C:/Users/Stefanny/OneDrive/estadística/Anime_limpio.csv")
Anime_limpio
dim(Anime_limpio)
#12017     8
str(Anime_limpio)
library(fdth)

# Determine una variable cuantitativa en su base de trabajo ---------------

#se seleccionan la variable rating con datos cuantitativos

rating = unique(Anime_limpio$rating)
rating
histograma = hist(rating, 
                  ylim = c(0,0.2), 
                  col = 'steelblue',
                  freq = FALSE,
                  breaks = 20,
                  main = 'Distribución de ratings')

# Establezca la posinle distribución de los datos apartir de la tabla de fre --------
tabla_frecuencias = fdt(Anime_limpio$rating, breaks = 'Sturges')
tabla_frecuencias
#tabla_frecuencias$breaks


# Muestreo repetido -------------------------------------------------------
n = 100 #tamaño de muestra
m = 1500 #cantidad de muestras
sample_new = sample(rating, n*m, replace = TRUE)
Muestreo_repetido =matrix(sample_new, nrow = n, ncol =
                           m)
Muestreo_repetido[,2]
medias_muestrales = apply(Muestreo_repetido, 2, mean) # columnas 2 filas 1
medias_muestrales


# Histogramas de medias ---------------------------------------------------

histograma = hist(medias_muestrales, 
                  ylim = c(0,3), 
                  col = 'steelblue',
                  freq = FALSE,
                  breaks = 20,
                  main = 'Distribución de medias muestrales')

# Aproximacion con la curva de densidad -----------------------------------
curve(dnorm(x, mean = mean(medias_muestrales), sd = sd(medias_muestrales)),
      col = 'red', lwd = "2", add = TRUE)

sd(medias_muestrales)
mean(medias_muestrales)
# N para que le margen de error de la media no sea superior a 0.3 ---------

#necesitamos saber que es:sigma y la probabilidad 
p_poblacional = mean(rating) #Cogemos sigma poblacional cogemos el n como 137 y sacamos la probabilidad de que z sea mayor a 0.3
p_muestral = mean(medias_muestrales)
desv_poblacional = sd(rating)
desv_muestral = sd(medias_muestrales)
Tamano_N <- function(s,error,p){
  n=((qnorm((1-p)/2)*s)/(-error))^2 
  print(n)
}

g = Tamano_N(desv_poblacional,0.3,0.95)
z =(0.3*sqrt(g))/desv_poblacional
pnorm(s)-pnorm(-s)

