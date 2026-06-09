#-----Ejercicio 1-------

mes<-c("Enero","Febrero","Marzo")
ingresos<-c(45000,41500,51200)
gastos<-c(33400,35400,35600)
impuestos<-c(6450,6300,7100)
#creacion del dataframe
df1<-data.frame(mes,ingresos,gastos,impuestos)
#nueva fila Abril
df1[nrow(df1) + 1, ] <- c("Abril",49700,36300,6850)
#cambio de ingresos en el mes Marzo
df1$ingresos[df1$mes == "Marzo"]<-50400
#crear nueva columna con benefcios de cada mes
df1$ingresos<-as.numeric(df1$ingresos)
df1$gastos<-as.numeric(df1$gastos)
df1$impuestos<-as.numeric(df1$impuestos)
df1$beneficios<-df1$ingresos-df1$gastos-df1$impuestos
#nueva columna con el factor Balance
df1$balance<-ifelse(df1$beneficios>0, "Positivo", "Negativo")



#-----Ejercicio 5------
df2<-data.frame(
  alumno<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
  php<-c(13,13,13,15,16,15,12,13,13,13,11,14,15,15,15),
  algoritmos<-c(15,14,16,20,18,16,13,16,15,14,12,16,17,19,13),
  bases.de.datos<-c(15,13,13,14,18,17,15,14,14,13,12,11,16,14,15),
  programacion<-c(13,12,14,16,17,15,11,15,13,10,10,14,15,16,10)
)
#modelo de regresion lineal multiple
rlm1 <- lm(php~algoritmos+bases.de.datos+programacion,data=df2)
# Resultados
summary(rlm1)
# Coeficientes
coef(rlm1)

#// Interpretación de coeficientes
#Algoritmos: Por cada punto adicional en Algoritmos, la nota en PHP aumenta en 0.58 puntos, manteniendo constantes las demás variables.
#Base de Datos: Por cada punto adicional en Base de Datos, PHP aumenta en 0.37 puntos.
#Programación: Por cada punto adicional en Programación, PHP disminuye en 0.24 puntos, pero este efecto no es estadísticamente significativo (podría deberse al azar).

#//Poder predictivo del modelo
#R² = 0.697 → El modelo explica el 69.7% de la variabilidad en las notas de PHP
#R² ajustado = 0.6144 → Considerando el número de variables, explica el 61.44%
#p-valor del modelo = 0.0034 → El modelo es globalmente significativo (las 3 variables juntas sí predicen PHP)

