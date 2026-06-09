#-----Ejercicio 6-------

#creacion del dataframe
df1<-protesta[, c("prot","coi.pol")]
#transformando variables
df1[, c("prot", "coi.pol")] <- lapply(df1[, c("prot", "coi.pol")], as.numeric)
df1$prot <- ifelse(df1$prot == 2, 1, 0)
df1$coi.pol <- ifelse(df1$coi.pol == 2, 1, 0)
#Ajuste de modelo
modelo1<-glm(
  prot~coi.pol,
  data=df1,
  family=binomial(link = "logit")
)

summary(modelo1)

#Probabilidad de que el protestante haya sido extorsionado
nuevo_si<-data.frame(coi.pol = 1)

predict(
  modelo1,
  newdata = nuevo_si,
  type = "response"
)
print("La probabilidad de que un protestante haya sido extorsionado por la policia es de 22,17%")

#Probabilidad de que el protestante no haya sido extorsionado
nuevo_no <- data.frame(coi.pol = 0)

predict(
  modelo1,
  newdata = nuevo_no,
  type = "response"
)
print("La probabilidad de que un protestante haya sido extorsionado por la policia es de 11,18%")



#creacion del segundo dataframe
df2<-protesta[, c("prot", "int.pol", "ciudad", "coi.pol")]
#transformando variables
df2$prot<-as.numeric(df2$prot)
df2$prot<-ifelse(df2$prot == 2, 1, 0)
df2$coi.pol<-as.numeric(df2$coi.pol)
df2$coi.pol<-ifelse(df2$coi.pol == 2, 1, 0)
df2$ciudad<-as.factor(df2$ciudad)
levels(df2$ciudad)
#Ajuste de malo
modelo2<-glm(
  prot~int.pol+ciudad+coi.pol,
  data=df2,
  family=binomial(link="logit")
)

summary(modelo2)

#Caso A: ciudadano protestante victima de extorsion, sin interes en politica 
#y que habita en una ciudad grande
casoA <- data.frame(
  int.pol = 4,
  ciudad = factor(
    "grande",
    levels = levels(df2$ciudad)
  ),
  coi.pol = 1
)

probA <- predict(
  modelo2,
  newdata = casoA,
  type = "response"
)

probA
print("La probabilidad de que el caso A ocurra es de 0,98%")

#Caso B: ciudadano protestante victima de extorsion, con interes en la politica
#y que habita en una ciudad pequenia
casoB <- data.frame(
  int.pol = 1,
  ciudad = factor(
    "pequeña",
    levels = levels(df2$ciudad)
  ),
  coi.pol = 1
)

probB <- predict(
  modelo2,
  newdata = casoB,
  type = "response"
)

probB
print("La probabilidad de que el caso B ocurra es de 54,18%")