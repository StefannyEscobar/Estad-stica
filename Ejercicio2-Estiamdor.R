Anime_limpio <- read.csv("C:/Users/Stefanny/OneDrive/estadística/Anime_limpio.csv")
Anime_limpio
str(Anime_limpio)
library(fdth)
library(dplyr)

# Separación de variables  -------------------------------------------------

vars =  c('TV', 'Movie')
VarBin = filter(Anime_limpio, type == vars )
dim(VarBin)
VarTV = filter(VarBin, type == 'TV' )
VarMovies = filter(VarBin, type == 'Movie' )
tipos = unique(VarBin$type)
tipos
rating =  unique(VarBin$rating)
rating

# Muestreo repetido -------------------------------------------------------
n = 100 
m = 1500
# Sample TV
sample_new = sample(VarTV$rating, n*m, replace = TRUE)
Muestreo_repetido =matrix(sample_new, nrow = n, ncol =                            m)
Muestreo_repetido[,2]
medias_muestralesTv = apply(Muestreo_repetido, 2, mean)
mean(medias_muestralesTv)
MiuTv = mean(VarTV$rating)
MiuTv
# Sample Movies
nn = 80
mm = 1500
sample_Movie = sample(VarMovies$rating, nn*mm, replace = TRUE)
Muestreo_Movie =matrix(sample_Movie, nrow = n, ncol =
                         m)
Muestreo_Movie[,2]
medias_muestralesM = apply(Muestreo_Movie, 2, mean) 
mean(medias_muestralesM)
MiuMovie = mean(VarMovies$rating)
MiuMovie
length(medias_muestralesM)
# Medias ------------------------------------------------------------------
mediamovie= mean(medias_muestralesM)
mediaTv =mean(medias_muestralesTv)
desv_movie = sd(medias_muestralesM)
desv_Tv = sd(medias_muestralesTv)
diferencias_estimada = medias_muestralesM-medias_muestralesTv
mean(diferencias_estimada)
difReal = MiuMovie-MiuTv
difReal
# Aproximación por medio de normal ----------------------------------------
histograma = hist(diferencias, 
                  xlim= c(-1,0),
                  ylim = c(0,3), 
                  col = 'steelblue',
                  freq = FALSE,
                  breaks = 20,
                  main = 'Distribución de medias muestrales con aproximación')

# Aproximacion con la curva de densidad -----------------------------------
curve(dnorm(x, mean = mean(diferencias), sd = sd(diferencias)),
      col = 'red', lwd = "2", add = TRUE)
abline(v=c(-0.0298,
           0.0298) )
# Intervalos de confianza -------------------------------------------------

qnorm(0.025)

pnorm(1.96)-pnorm(-1.96)


















