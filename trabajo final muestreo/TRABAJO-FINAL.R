######################################################
######################################################
#####            TRABAJO FINAL                    ####
######################################################
######################################################
library(pacman)
p_load(ggplot2, tidyverse,readxl)

# LECTURA DE DATOS --------------------------------------------------------

data <- read.csv("drinking-water-quality-distribution-monitoring-data-1 (1).csv",
                 na.strings = " ")


# Calcula el número de valores perdidos en cada columna
missing_values <- colSums(is.na(data))

# Imprime el número de valores perdidos en cada columna
print(missing_values)


# Calcula el número de valores únicos en cada columna
unique_values <- sapply(data, function(x) length(unique(x)))

# Imprime el número de valores únicos en cada columna
print(unique_values)



# Eliminar la columna 9 del dataframe
data <- data[-9]



#columna "cloro"
datacloro <- data[!is.na(data[, 7]), ]
datacloro$observacion <- 1:nrow(datacloro)
datacloro$Residual.Free.Chlorine..mg.L. |>  mean()

ggplot(datacloro, aes(x = observacion, y = Residual.Free.Chlorine..mg.L.)) +
geom_point() +
  ylim(-1, 2.20)


#columna turbidez
dataturbidez <- data[!is.na(data[, 8]), ]
dataturbidez$observacion <- 1:nrow(dataturbidez)
dataturbidez$Turbidity..NTU. |>  mean()

ggplot(dataturbidez, aes(x = observacion, y = Turbidity..NTU.)) +
geom_point() +
  ylim(0.05, 7)
#observa que si existe un patrón 


#IMPUTACION DE LOS DATOS Observados 
data[,7][is.na(data[,7])] <- 0.5918561
data[,8][is.na(data[,8])] <- 0.76758

data <- data[-c(29719,45349,34701),]


# MUESTREO SISTEMATICO ----------------------------------------------------

# Trabajando el diseño sistemático para "cloro"
attach(data)
N <- dim(data)[1];N
varmu <- data$Residual.Free.Chlorine..mg.L. |> var()
B <-  (0.01)**2 / 4
nmu <-((N)*(varmu))/((N-1)*B + varmu);nmu
mediamu <- data$Residual.Free.Chlorine..mg.L. |> mean()

a <- 32.322 
floor(N/a)

grupo <- as.factor(array(1:a,N))
data.frame(grupo, data)[1:20,]
library(TeachingSampling)
set.seed(2023)
sam<-S.SY(N,a)
muestra<- data[sam,]
attach(muestra)

n <- dim(muestra)[1]
n


#Estimaciones para la variable de interés "Cloro"
estima <- data.frame(Residual.Free.Chlorine..mg.L.)
E.SY(N, a, estima)/N

options(scipen=999)
modelo=lm(Residual.Free.Chlorine..mg.L.~grupo, data)
anova=anova(modelo); anova

#coefiente de intra-clase
rho<- 1-((n/(n-1))*(22212.39/(22212.39+1.96)))
rho       
#se obtiene una buena métrica cercaco a 0 indicando 
#es decir se observa errores muy mínimos

#Efecto del diseño
Deff<-(N-1)*(1+(n-1)*rho)/(N-n) ; Deff
# se obtiene que el efecto de diseño es 0.09
#Ganancia 
1/Deff
#Observa que la ganancia adquerida es aproximadamente 11 veces
rho<1/(1-N)
#Se obtiene valor de verdad que si la ganancia tiene 
#mejor rendimiento que un diseño por MAS

# MUESTREO ESTRATIFICADO --------------------------------------------------

# ANÁLISIS EXPLORATORIO 
par(mfcol=c(1,2))
boxplot(data$Residual.Free.Chlorine..mg.L.,xlab= "Residual.Free.Chlorine",col ="#5F9EA0" )
boxplot(data$Turbidity..NTU.,xlab= "Turbidity",col="#5F9EA0")
par(mfcol=c(1,1))

data %>% summary

data$Sample.Site[1:100]
ggplot(data) + aes(x=Sample.Site) +  geom_bar(stat="count") + coord_flip()
data$Sample.Site


# Asignación proporcional
N = nrow(data);N
n=5033
p <- n/N;p

# Cantidad de estratos
data$Sample.Site %>% unique() %>% length()

# Frecuencia total de cada estrato
data$Sample.Site %>% table() %>% as.data.frame() -> frecuencia_total
frecuencia_total$Freq %>% sum()
Nh <- c(frecuencia_total$Freq); Nh

# Frecuencia de cada estrato para el muestreo
round(p*frecuencia_total$Freq) %>% as.data.frame() -> frecuencia_estratos
frecuencia_estratos$. %>% sum()
nh <- c(frecuencia_estratos$.);nh

# Selección de la muestra
set.seed(30)
sam <- S.STSI(data$Sample.Site,Nh,nh)
muestra <- data[sam,]
muestra %>% head()
muestra %>% nrow()


#Turbidez
anova(lm(muestra$Turbidity..NTU. ~muestra$Sample.Site))
deff_t <- 0.04748/(0.04748+0.14566);deff_t
1/deff_t

res_t<-E.STSI(muestra$Sample.Site,Nh,nh,muestra$Turbidity..NTU.)/N
str(res_t)

res_t[,,2][1,402]
res_t[,,2][3,402] %>% View
LI1_t<-res_t[,,2][1,402]-qnorm(1-0.10/2)*res_t[,,2][2,402]
LS1_t<-res_t[,,2][1,402]+qnorm(1-0.10/2)*res_t[,,2][2,402]
c(LI1_t,LS1_t)

#Residuo cloro
anova(lm(muestra$Residual.Free.Chlorine..mg.L.~muestra$Sample.Site))
deff_c <- 0.017733 /(0.017733 +0.293296);deff_c
1/deff_c 

res_c<-E.STSI(muestra$Sample.Site,Nh,nh,muestra$Residual.Free.Chlorine..mg.L.)/N
str(res_c)
res_c
res_c[,,2][3,402] %>% View
res_c[,,2][1,402]
LI1_c<-res_c[,,2][1,402]-qnorm(1-0.10/2)*res_c[,,2][2,402]
LS1_c<-res_c[,,2][1,402]+qnorm(1-0.10/2)*res_c[,,2][2,402]
c(LI1_c,LS1_c)


##
## Valor de n 

# Turbidez
B= 0.120196**2/4
D = B^2 / 4
N

# turbidez
data %>% group_by(Sample.Site) %>%  
  summarise(varianza=var(Turbidity..NTU.)) -> varianzas_t 
n_ideal_t = (sum(frecuencia_total$Freq*varianzas_t$varianza)) / 
  (N*D + (1/N)*(sum(frecuencia_total$Freq*varianzas_t$varianza)))
n_ideal_t


# Residuo cloro
B= 0.120196**2/4
D = B^2 / 4
data %>% group_by(Sample.Site) %>%  
  summarise(varianza=var(Residual.Free.Chlorine..mg.L.)) -> varianzas_c
n_ideal_c = (sum(frecuencia_total$Freq*varianzas_c$varianza)) / 
  (N*D + (1/N)*(sum(frecuencia_total$Freq*varianzas_c$varianza)))
n_ideal_c

# MUESTREO COMPLEJO ----------------------------------------------


##PARTE SISTEMATICO
N <- dim(data)[1];N
varmu <- data$Residual.Free.Chlorine..mg.L. |> var()
B <-  (0.01)**2 / 4
nmu <-((N)*(varmu))/((N-1)*B + varmu);nmu
mediamu <- data$Residual.Free.Chlorine..mg.L. |> mean()

a <- 32.322 
floor(N/a)

grupo <- as.factor(array(1:a,N))
data.frame(grupo, data)[1:20,]
library(TeachingSampling)
set.seed(2023)
sam<-S.SY(N,a)
muestra<- data[sam,]

##PARTE ESTRATIFICADO
# Asignación proporcional
N = nrow(muestra);N
n=167
p <- n/N;p

# Cantidad de estratos
muestra$Sample.Site %>% unique() %>% length()

# Frecuencia total de cada estrato
muestra$Sample.Site %>% table() %>% as.data.frame() -> frecuencia_total
frecuencia_total$Freq %>% sum()
Nh <- c(frecuencia_total$Freq); Nh

# Frecuencia de cada estrato para el muestreo
round(p*frecuencia_total$Freq) %>% as.data.frame() -> frecuencia_estratos
frecuencia_estratos$. %>% sum()
nh <- c(frecuencia_estratos$.);nh

# Selección de la muestra
set.seed(30)
sam <- S.STSI(muestra$Sample.Site,Nh,nh)
muestrafi <- muestra[sam,]
muestrafi %>% head()
muestrafi %>% nrow()

# turbidez
muestrafi %>% group_by(Sample.Site) %>%  
  summarise(varianza=var(Turbidity..NTU.)) -> varianzas_t 
n_ideal_t = (sum(frecuencia_total$Freq*varianzas_t$varianza)) / 
  (N*D + (1/N)*(sum(frecuencia_total$Freq*varianzas_t$varianza)))
n_ideal_t


# Residuo cloro
B= 0.120196**2/4
D = B^2 / 4
muestrafi %>% group_by(Sample.Site) %>%  
  summarise(varianza=var(Residual.Free.Chlorine..mg.L.)) -> varianzas_c
n_ideal_c = (sum(frecuencia_total$Freq*varianzas_c$varianza)) / 
  (N*D + (1/N)*(sum(frecuencia_total$Freq*varianzas_c$varianza)))
n_ideal_c


# MUESTREO POR CONGLOMERADOS -------------------------------------------------------

data|> select(-Sample.Number,
              -Sample.Date,
              -Sample.Time) |> 
  rename(turbidez =Turbidity..NTU. ,
         coliformes_100ml=Coliform..Quanti.Tray...MPN..100mL.,
         E.coli_100ml=E.coli.Quanti.Tray...MPN.100mL.,
         cloro_libre_residual=Residual.Free.Chlorine..mg.L.,
         conglomerados=Sample.Site)-> data


data <- data[complete.cases(data), ]

colSums(is.na(data))

library(tidyverse)
library(TeachingSampling)

#Parte a)
RNGkind(sample.kind="Rounding")
set.seed(1002)


data.in <- data$conglomerados |> table() |> data.frame()
nrow(data.in)

NI<-length(table(data.in$Var1)) # Número de conglomerados 
nI<-9      # Muestra de conglomerados 


samI<-S.SI(NI,nI) 

indices <- which(samI[, 1] != 0);indices


conglomerados <- data.in[indices,] |> select(Var1) ; conglomerados

###
muestra <- c( "13700" ,"20150" ,"24650", "25550", "34300" ,"38300", "43750" ,"52350" ,"76950" )


dataI<-data[data$conglomerados%in%muestra, ]
conglo<-as.factor(as.integer(dataI$conglomerados))

y<-T.SIC(dataI$turbidez,conglo)
res1<-E.SI(NI,nI,y);res1
# Intervalo de Confianza para la media de turbidez en los resultados 
N<-nrow(data)
res2<-res1/N;res2
deff <- res2[2,3]^2/res2[2,2]^2;deff
1/deff
LI1<-res2[1,3]-qnorm(1-0.06/2)*res2[2,3]
LS1<-res2[1,3]+qnorm(1-0.06/2)*res2[2,3]
c(LI1,LS1)

#   Estimación para el cloro 
dataI<-data[data$conglomerados%in%muestra, ]
conglo<-as.factor(as.integer(dataI$conglomerados))
y<-T.SIC(dataI$cloro_libre_residual,conglo)
res3<-E.SI(NI,nI,y)
N<-nrow(data)
res4<-res3/N;res4
deff <- res4[2,3]^2/res4[2,2]^2;deff
1/deff
LI2<-res4[1,3]-qnorm(1-0.06/2)*res4[2,3]
LS2<-res4[1,3]+qnorm(1-0.06/2)*res4[2,3]
c(LI2,LS2)

