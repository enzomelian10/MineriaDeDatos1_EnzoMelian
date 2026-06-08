# --- Replicar en el entorno de desarrollo los códigos provistos como ejemplos en esta clase

#Ejemplo de aplicacion 1 
edad <- c(46,20,52,30,57,25,28,36,57,44,24,31,52,23,60,48,34,51,40,34) 
peso <- c(84,73,65,70,76,69,63,72,79,75,27,89,65,57,59,69,60,79,75,82) 
grasa <- c(354,190,405,263,451,302,288,325,402,365,209,290,346,254,395,434,220,374,308,220) 
datos <-data.frame(edad,peso,grasa)

pairs(datos) 
cor(datos)

reg <-lm(grasa~edad,data=datos)
reg
summary(reg)

plot(edad,grasa,xlab="Edad",ylab="Grasa en la sangre") 
abline(reg, col="blue")

#Calculo de predicciones
pred_edades <- data.frame(edad=seq(20,60))
predict(reg,pred_edades)

pred_edad <- data.frame(edad=c(10,80)) 
predict(reg,pred_edad)

#Ejemplo de aplicacion 2
salario<- c(11, 10, 8, 5, 9, 7, 3, 11, 8, 7) 
ausencias<- c(18, 17, 29, 36, 11, 28, 35, 14, 20, 32) 
df <-data.frame(salario,ausencias) 
df

shapiro.test( df$salario )

#Aplicacion del modelo de regresion lineal
modeloR <- lm( ausencias ~ salario, data = df)
summary( modeloR)

plot( df$salario, df$ausencias, xlim = c( 0, 14 ), ylim = c( 0, 70 ), xlab = "Salario", ylab = "Ausencias" ) 
abline( modeloR, col = "#e74c3c" )

#Ejemplo de aplicacion 3
precio <- c(250, 130, 165, 310, 320, 400, 200, 80, 69, 179, 120, 223, 300, 198, 165, 69, 73, 123, 356, 183) 
superficie <- c(120, 80, 100, 180, 190, 250, 99, 90, 60, 100, 110, 120, 180, 130, 90, 50, 60, 70, 120, 130) 
antiguedad <-c(15, 20, 30, 15, 12, 40, 30, 27, 14, 20, 22, 25, 21, 33, 5, 12, 6, 10, 28, 30) 
pisos <- data.frame(precio, superficie, antiguedad)
plot(pisos$precio,pisos$superficie)

reg_lin <- lm(precio ~ superficie, data = pisos) 
reg_lin summary(reg_lin)

reg_lin1 <- lm(precio ~ antiguedad, data = pisos) 
reg_lin1

lm(formula = precio ~ superficie + antiguedad, data = pisos)

# --- Replicar en el entorno de desarrollo los códigos provistos como ejemplos en esta clase

# Cargar dataset
datos1 <- read.csv("dataset.csv") 

# Instalacion de librerias
install.packages("MASS")
library(MASS) 

#_MODELO DE REGRESION LINEAL (con las variables ingreso y ahorro)
modelo_simple <- lm(Ahorro ~ Ingresos, data = datos1) 
summary(modelo_simple) 
cor(datos$Ingresos, datos$Ahorro)

plot( datos1$Ingresos, datos1$Ahorro, main = "Relación entre Ingresos y Ahorro", 
      xlab = "Ingresos", 
      ylab = "Ahorro", 
      pch = 19 ) 
abline(modelo_simple, col = "blue", lwd = 2)

modelo_completo <- lm( Ahorro ~ Edad + Experiencia + Horas_Estudio + Ingresos + Gasto_Mensual, data = datos1 ) 
modelo_step <- stepAIC( modelo_completo, direction = "both" ) 
summary(modelo_step)

#_MODELO DE REGRESION MULTIPLE

# MODELO 1 

modelo1 <- lm(
  Ahorro ~ Ingresos + Experiencia,
  data = datos1
)

summary(modelo1)


#MODELO 2 
modelo2 <- lm(
  Ahorro ~ Ingresos +
    Experiencia +
    Gasto_Mensual,
  data = datos1
)

summary(modelo2)

# Comparacion entre modelos
summary(modelo1)$r.squared
summary(modelo2)$r.squared

summary(modelo1)$adj.r.squared
summary(modelo2)$adj.r.squared

AIC(modelo1, modelo2)