
# Encuentre la función de logverosimilitud relativa 1 (r_1) ---------------
r1=function(tetha1){(309*log(tetha1)+691*log(1-tetha1))+618.2982}

x=seq(0, 1, length.out = 1000)
valores_en_intervalo_r1=x[which(r1(x)>(-0.69))] #50 porciento

min_arg_r1=valores_en_intervalo_r1[which.min(valores_en_intervalo_r1)]
min_r1=r1(min_arg_r1)
max_arg_r1=valores_en_intervalo_r1[which.max(valores_en_intervalo_r1)]
max_r1=r1(max_arg_r1)
plot(x,r1(x), xlim = c(0,1), ylim = c(-500 ,-1),
     col="red", lty=1, type = "l", ylab = "verosimilitud relativa", 
     main = "Verosimilitud relativa 50%")
abline(h=min_r1, lty=2)
abline(v=min_arg_r1, lty=2, col="red")
abline(h=max_r1, lty=2)
abline(v=max_arg_r1, lty=2, col="red")

#cuente la cantidad de exitos, con esta información construya (r_2)----------
r2=function(tetha2){298*log(tetha2)+702*log(1-tetha2)+609.16}
x=seq(0, 1, length.out = 1000)
valores_en_intervalo_r2=x[which(r2(x)>(-0.69))]
min_arg_r2=valores_en_intervalo_r2[which.min(valores_en_intervalo_r2)]
min_r2=r2(min_arg_r2)
max_arg_r2=valores_en_intervalo_r2[which.max(valores_en_intervalo_r2)]
max_r2=r2(max_arg_r2)
plot(x,r2(x), xlim = c(0,1), ylim = c(-400,0.15),col="green", lty=1, type = "l",
     ylab = "verosimilitud relativa", main = "Verosimilitud relativa 50%")
abline(h=min_r2, lty=2)
abline(v=min_arg_r2, lty=2, col="green")
abline(h=max_r2, lty=2)
abline(v=max_arg_r2, lty=2, col="green")

#Luego construya una logverosimilitud relativa combinada r 
l12=function(tetha){(309+298)*log(tetha)+(2000-309-298)*log(1-tetha)}
r12=function(tetha){l12(tetha)+1227.601}

#Grafique 1,2,3. Para esto use el código que les compartí.
# para que valores r12 es igual a -------------------------------------------
x=seq(0, 1, length.out = 1000)
valores_en_intervalo_r12=x[which(r12(x)>(-0.69))]
min_arg_r12=valores_en_intervalo_r12[which.min(valores_en_intervalo_r12)]
min_r12=r12(min_arg_r12)
max_arg_r12=valores_en_intervalo_r12[which.max(valores_en_intervalo_r12)]
max_r12=r12(max_arg_r12)
plot(x,r12(x), xlim = c(0,1), ylim = c(-500,-1),col="blue", lty=1, type = "l", ylab = "verosimilitud relativa", main = "Verosimilitud relativa combinada")
abline(h=min_r12, lty=2)
abline(v=min_arg_r12, lty=2, col="blue")
abline(h=max_r12, lty=2)
abline(v=max_arg_r12, lty=2, col="blue")

x=seq(0,1, length.out = 100)

plot(x,r1(x), xlim = c(0,1), ylim = c(-400,0.15),col="red", lty=1, type = "l", ylab = "verosimilitud relativa", main = "Verosimilitud relativa tres modelos")
lines(x,r2(x), xlim = c(0,1), ylim = c(-400,0.15),col="green", lty=1, type = "l", add=TRUE)
lines(x,r12(x), xlim = c(0,1), ylim = c(-400,0.15),col="blue", lty=1, type = "l", add=TRUE)
legend(0.08, -2, legend=c("r1", "r2", "r12"),
       col=c("red", "green", "blue"), lty=1, cex=0.8)
abline(h=-0.69, lty=2)
abline(v=0.2922923, lty=2, col="red")
abline(v=0.3263263, lty=2, col="red")
abline(v= 0.2812813, lty=2, col="green")
abline(v= 0.3143143, lty=2, col="green")
abline(v=0.2922923, lty=2, col="blue")
abline(v=0.3153153, lty=2, col="blue")
text(0.2922923, -5.1, expression(0.2922923), col="red",cex=0.65, pos=3)
text(0.3263263, -5.1, expression(0.3263263), col="red", cex=0.65, pos=3)
text(0.2812813, -5.1, expression(0.2812813), col="green", cex=0.65, pos=3)
text(0.3143143, -5.1, expression(0.3143143), col="green", cex=0.65, pos=3)
text(0.2922923, -5.1, expression(0.2922923), col="blue", cex=0.65, pos=3)
text(0.3153153, -5.1, expression(0.3153153), col="blue", cex=0.65, pos=3)
text(0.1, -0.69, expression(R==0.5, r==-0.69))


