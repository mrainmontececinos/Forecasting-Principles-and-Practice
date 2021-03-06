
library(fpp2)


# 1. Para la siguiente serie, encuentre una transformaci�n de Box-Cox apropiada
# para estabilizar la varianza.


# - usnetelec
lambda_usnetelec <- BoxCox.lambda(usnetelec)
print(c("valor lambda para usnetelec: ", 
        lambda_usnetelec))
autoplot(BoxCox(usnetelec, lambda_usnetelec))

# - usgdp
lambda_usgdp <- BoxCox.lambda(usgdp)
print(c("valor lambda para usgdp: ", 
        lambda_usgdp))
autoplot(BoxCox(usgdp, lambda_usgdp))

# - mcopper
lambda_mcopper <- BoxCox.lambda(mcopper)
print(c("valor lambda para mcopper: ", 
        lambda_mcopper))
autoplot(BoxCox(mcopper, lambda_mcopper))

# - enplanements
lambda_enplanements <- BoxCox.lambda(enplanements)
print(c("valor lambda para enplanements: ", 
        lambda_enplanements))
autoplot(BoxCox(enplanements, lambda_enplanements))


# 2.�Por qu� una transformaci�n de Box-Cox no es �til para los cangas datos?



autoplot(cangas)

lambda_cangas <- BoxCox.lambda(cangas)
autoplot(BoxCox(cangas, lambda_cangas))

# puede ver que la transformaci�n de Box-Cox no produce un modelo simple


# �Qu� transformaci�n de Box-Cox seleccionar�a para sus datos minoristas 
# (del ejercicio 3 de la secci�n 2.10 )?


retaildata <- xlsx::read.xlsx("retail.xlsx", sheetIndex = 1, startRow = 2)
myts <- ts(retaildata[,"A3349873A"], frequency=12, start=c(1982,4))

lambda_retail <- BoxCox.lambda(myts)
print(c("selected lambda:", lambda_retail))

fc_retail <- rwf(myts, 
                 drift = TRUE, 
                 lambda = lambda_retail,
                 h = 50,
                 level = 80)

fc_retail_biasadj <- rwf(myts, 
                         drift = TRUE, 
                         lambda = lambda_retail,
                         h = 50,
                         level = 80,
                         biasadj = TRUE)

autoplot(myts) +
  autolayer(fc_retail, series = "Drift method with Box-Cox Transformation") +
  autolayer(fc_retail_biasadj$mean, series = "Bias Adjusted") +
  guides(colour = guide_legend(title = "Forecast"))


# 4 Para cada una de las siguientes series, haz una gr�fica de los datos. 
# Si transformar parece apropiado, h�galo y describa el efecto. 
# dole, usdeaths, bricksq.


autoplot(dole)
lambda_dole <- BoxCox.lambda(dole)
autoplot(BoxCox(dole, lambda_dole))

# Para los datos de subsidio, parec�a que ser�a mejor utilizar 
# la transformaci�n Box-Cox para ver el patr�n
autoplot(usdeaths)
lambda_usdeaths <- BoxCox.lambda(usdeaths)
autoplot(BoxCox(usdeaths, lambda_usdeaths))

# Para los datos de usdeaths, parec�a que no ten�a sentido transformarlos

autoplot(bricksq)
lambda_bricksq <- BoxCox.lambda(bricksq)
autoplot(BoxCox(bricksq, lambda_bricksq))

# Para los datos bricksq, parec�a que no ten�a sentido transformarlos




# 5 Calcule los residuos de un pron�stico ingenuo estacional aplicado a 
# los datos trimestrales de producci�n de cerveza australiana de 1992.
# El siguiente c�digo le ayudar�.


beer <- window(ausbeer, start=1992)
fc <- snaive(beer)
autoplot(fc)
res <- residuals(fc)
autoplot(res)

checkresiduals(fc)



# 6. Repita el ejercicio para los datos WWWusagey bricksq. Utilice el 
# que de naive()o snaive()es m�s adecuado en cada caso.



snaive_www <- snaive(WWWusage, h = 15)
autoplot(snaive_www)
checkresiduals(snaive_www)

naive_www <- naive(WWWusage)
autoplot(naive_www)
checkresiduals(naive_www)

# El gr�fico ACF indica que los residuos no son ruido blanco debido a la existencia 
# de picos significativos. Y la prueba de Ljung-Box muestra que son estad�sticamente 
# significativos para ambos m�todos. Por tanto, no son ruido blanco. Y la distribuci�n 
# de residuos tampoco es normal.

# Si necesito elegir entre los 2 m�todos anteriores, elegir� el m�todo ingenuo porque no
# hay ning�n patr�n estacional particular en los datos y los valores Q de la prueba de 
#Ljung-Box eran los mismos para ambos m�todos.

snaive_bricksq <- snaive(bricksq)
autoplot(snaive_bricksq)
checkresiduals(snaive_bricksq)

naive_bricksq <- naive(bricksq)
autoplot(naive_bricksq)
checkresiduals(naive_bricksq)

# Para ambos m�todos, los resultados de la prueba de Ljung-Box muestran que los 
# residuos no son ruido blanco. Y los residuos tampoco ten�an una distribuci�n normal.
# Si necesito elegir entre los 2 m�todos anteriores, elegir� el m�todo snaive porque
# puedo ver la estacionalidad en los datos y el valor Q de Ljung-Box de los m�todos 
# naive fue menor que el valor del m�todo ingenuo.


# 7. �Son las siguientes afirmaciones verdaderas o falsas? Explica tu respuesta.


# a. Los buenos m�todos de pron�stico deben tener residuos distribuidos normalmente.
# R:  Es �til tener residuos distribuidos normalmente, pero no es necesario tenerlos 
#con buenos m�todos de pron�stico.

# b Un modelo con peque�os residuos dar� buenos pron�sticos.
#R:  Los buenos m�todos de pron�stico no necesitan tener peque�os residuos. 
#Pero sus residuos no deben estar correlacionados y tener una media cero.

#c. La mejor medida de precisi�n del pron�stico es MAPE.
#  La mejor medida de precisi�n del pron�stico es diferente seg�n el caso.

#d. Si su modelo no pronostica bien, deber�a hacerlo m�s complicado.
#R:  Un m�todo de pron�stico m�s complicado no garantiza un mejor pron�stico.

#e Elija siempre el modelo con la mejor precisi�n de pron�stico seg�n lo medido en 
#el equipo de prueba.

#R: Al elegir un modelo, tambi�n se debe considerar si los residuos tienen media cero 
#y si no est�n correlacionados entre s�. Simplemente elegir el modelo de mejor precisi�n
#no es bueno.



# 8. Para su serie de tiempo minorista (del ejercicio 3 en la secci�n 2.10):

# a. Divida los datos en dos partes usando


myts.train <- window(myts, end=c(2010,12))
myts.test <- window(myts, start=2011)

# b. Verifique que sus datos se hayan dividido adecuadamente produciendo el siguiente gr�fico.



autoplot(myts) + 
  autolayer(myts.train, series="Training") +
  autolayer(myts.test, series="Test")

# c. Calcule las previsiones utilizando snaiveaplicado a myts.train.

fc <- snaive(myts.train)

# d. Compare la precisi�n de sus pron�sticos con los valores reales almacenados en myts.test.

accuracy(fc,myts.test)

# e. Verifique los residuos.

checkresiduals(fc)


# los residuos se correlacionaron entre s� y no se distribuyeron normalmente

# f. �Qu� tan sensibles son las medidas de precisi�n para la divisi�n de entrenamiento / prueba?


# Pens� en la sensibilidad como la relaci�n entre el error del conjunto de prueba y el
#error del conjunto del tren. Cuando utilic� la definici�n, parec�a que Mean Error es
#muy sensible, RMSE, MAE, MPE, MASE son sensibles y MAPE y ACF1 no son muy sensibles.
# No s� si este m�todo es el que quer�a el autor para resolver esta cuesti�n de sensibilidad.




# 9. visnights contiene noches de visitantes trimestrales (en millones) desde 1998 hasta 2016 
#para veinte regiones de Australia.

#a. �selo window()para crear tres conjuntos de capacitaci�n para visnights[,"QLDMetro"],omitir
# los �ltimos 1, 2 y 3 a�os; ll�malos train1, train2 y train3, respectivamente.
#Por ejemplo train1 <- window(visnights[, "QLDMetro"], end = c(2015, 4)).

vn_Melbourne_train1 <- window(visnights[,"QLDMetro"], end = c(2014, 4))
vn_Melbourne_train2 <- window(visnights[,"QLDMetro"], end = c(2013, 4))
vn_Melbourne_train3 <- window(visnights[,"QLDMetro"], end = c(2012, 4))

#b. Calcule un a�o de pron�sticos para cada conjunto de entrenamiento usando el snaive
#()m�todo. Llame a estos fc1, fc2y fc3, respectivamente.

snaive_vn_Melbourne_train1 <- snaive(vn_Melbourne_train1, h = 4)
snaive_vn_Melbourne_train2 <- snaive(vn_Melbourne_train2, h = 4)
snaive_vn_Melbourne_train3 <- snaive(vn_Melbourne_train3, h = 4)

#c. �selo accuracy()para comparar el MAPE en los tres conjuntos de prueba. Comenta sobre estos.

vn_Melbourne_test1 <- window(visnights[,"QLDMetro"], start = c(2015, 1), end = c(2015, 4))
vn_Melbourne_test2 <- window(visnights[,"QLDMetro"], start = c(2014, 1), end = c(2014, 4))
vn_Melbourne_test3 <- window(visnights[,"QLDMetro"], start = c(2013, 1), end = c(2013, 4))

accuracy(snaive_vn_Melbourne_train1, vn_Melbourne_test1)
writeLines("")
accuracy(snaive_vn_Melbourne_train2, vn_Melbourne_test2)
writeLines("")
accuracy(snaive_vn_Melbourne_train3, vn_Melbourne_test3)


# MAPE fue el m�s peque�o para la predicci�n de 2015 y el m�s grande para la predicci�n de 
#2014. MAPE se hizo m�s peque�o en la predicci�n de 2013, pero no ser� m�s peque�o que el de 2015.



# 10. Utilice el �ndice Dow Jones (conjunto de datos dowjones) para hacer lo siguiente:

#a. Produce una gr�fica de tiempo de la serie.

autoplot(dowjones)

# b. Genere pron�sticos utilizando el m�todo de deriva y gr�belos.

drift_dj <- rwf(dowjones, drift = TRUE)
autoplot(drift_dj)

#c. Muestre que los pron�sticos son id�nticos a extender la l�nea trazada entre la 
#primera y la �ltima observaci�n.

dj_x <- c(1, 78)
dj_y <- c(dowjones[1], dowjones[78])
lm_dj <- lm(dj_y ~ dj_x)

autoplot(drift_dj) +
  geom_abline(intercept = lm_dj$coefficients[1],
              slope = lm_dj$coefficients[2],
              colour = "red")

autoplot(drift_dj) +
  geom_line(aes(x = c(1, 78),
                y = dowjones[c(1, 78)]), 
            colour = "red")


#d. Intente utilizar algunas de las otras funciones de referencia para pronosticar el 
#mismo conjunto de datos. �Cu�l crees que es mejor? �Por qu�?

checkresiduals(drift_dj)

mean_dj <- meanf(dowjones)
autoplot(mean_dj)

naive_dj <- naive(dowjones)
autoplot(naive_dj)
checkresiduals(naive_dj)

snaive_dj <- snaive(dowjones, h = 10)


# Creo que el m�todo ingenuo es el mejor porque es realmente dif�cil predecir el 
#precio de las acciones con observaciones pasadas. Creo que es m�s seguro simplemente 
#tomar el valor de la �ltima observaci�n utilizando un m�todo ingenuo.


#11. Considere los precios de las acciones de IBM de cierre diario (conjunto de datos ibmclose).

#a. Genere algunos gr�ficos de los datos para familiarizarse con ellos.

autoplot(ibmclose)

#b. Divida los datos en un conjunto de entrenamiento de 300 observaciones y un
#conjunto de prueba de 69 observaciones.
ibm_train <- subset(ibmclose, end = 300)
ibm_test <- subset(ibmclose, start = 301)

#c. Intente utilizar varios m�todos de referencia para pronosticar el conjunto de 
# entrenamiento y compare los resultados en el conjunto de prueba. �Qu� m�todo funcion� mejor?


snaive_ibm <- snaive(ibm_train, h = 69)
naive_ibm <- naive(ibm_train, h = 69)
drift_ibm <- rwf(ibm_train, drift = TRUE, h = 69)
mean_ibm <- meanf(ibm_train, h = 69)

autoplot(snaive_ibm) +
  autolayer(ibm_test)
autoplot(naive_ibm) +
  autolayer(ibm_test)
autoplot(drift_ibm) +
  autolayer(ibm_test)
autoplot(mean_ibm) +
  autolayer(ibm_test)

writeLines("Snaive method")
accuracy(snaive_ibm, ibm_test)

writeLines("\nNaive method")
accuracy(naive_ibm, ibm_test)

writeLines("\nDrift method")
accuracy(drift_ibm, ibm_test)

writeLines("\nMean method")
accuracy(mean_ibm, ibm_test)

e_snaive_ibm <- ibm_test - snaive_ibm$mean
e_naive_ibm <- ibm_test - naive_ibm$mean
e_drift_ibm <- ibm_test - drift_ibm$mean
e_mean_ibm <- ibm_test - mean_ibm$mean

autoplot(e_snaive_ibm^2, series = "snaive method") +
  autolayer(e_naive_ibm^2, series = "naive method") +
  autolayer(e_drift_ibm^2, series = "drift method") +
  autolayer(e_mean_ibm^2, series = "mean method") +
  guides(colour = guide_legend(title = "Forecast")) +
  ggtitle("Errors of the forecast of closing IBM stock price") +
  ylab(expression(paste("erro", r^{2})))

# El m�todo Drift funcion� mejor

# El m�todo de validaci�n cruzada de series de tiempo de la funci�n tsCV no
#usa datos completos a menos que h = 1. Por ejemplo, si los datos utilizables
#tienen 100 puntos yh = 3, tsCV predice el punto 101 con 98 puntos, el 102 con 99
#puntos y el 103 con 100 puntos. Por lo tanto, el valor del resultado de error de tsCV 
#no puede evitar diferir de los valores de la funci�n de precisi�n. La funci�n de
#precisi�n siempre obtiene el resultado de datos completos.

ibmclose %>% tsCV(forecastfunction = snaive, h = 69) ->  e_snaive_ibm_CV
ibmclose %>% tsCV(forecastfunction = naive, h = 69) ->  e_naive_ibm_CV
ibmclose %>% tsCV(forecastfunction = rwf, drift = TRUE, h = 69) ->  e_drift_ibm_CV
ibmclose %>% tsCV(forecastfunction = meanf, h = 69) ->  e_mean_ibm_CV

autoplot(subset(e_snaive_ibm_CV^2, start = 301), series = "snaive method") +
  autolayer(subset(e_naive_ibm_CV^2, start = 301), series = "naive method") +
  autolayer(subset(e_drift_ibm_CV^2, start = 301), series = "drift method") +
  autolayer(subset(e_mean_ibm_CV^2, start = 301), series = "mean method") +
  guides(colour = guide_legend(title = "Forecast")) +
  ggtitle("Errors of the forecast of closing IBM stock price",
          subtitle = "after using tsCV function") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ylab(expression(paste("erro", r^{2})))
# Seg�n los resultados devueltos de la funci�n tsCV, habr�a seleccionado 
# un m�todo ingenuo porque arroj� el error m�s peque�o.

# d. Verifique los residuos de su m�todo preferido. �Se parecen al ruido blanco?


checkresiduals(naive_ibm)
checkresiduals(drift_ibm)

# No. Incluso cuando verifiqu� los residuos del m�todo ingenuo porque los
# valores de error eran similares al resultado del m�todo de deriva, tampoco eran como el ruido blanco.

# 12 Considere las ventas de casas unifamiliares nuevas en los EE. UU., De enero de 1973 a 
# noviembre de 1995 (conjunto de datos hsales).

#a. Genere algunos gr�ficos de los datos para familiarizarse con ellos.
autoplot(hsales)

#b. Divida el hsalesconjunto de datos en un conjunto de entrenamiento y un conjunto de 
#prueba, donde el conjunto de prueba son los �ltimos dos a�os de datos.
hsales_train <- subset(hsales, end = length(hsales) - 24)
hsales_test <- subset(hsales, start = length(hsales) - 23)
                      
#c. Intente utilizar varios m�todos de referencia para pronosticar el conjunto de 
#entrenamiento y compare los resultados en el conjunto de prueba. �Qu� m�todo funcion� mejor?
snaive_hsales <- snaive(hsales_train, h = 24)
naive_hsales <- naive(hsales_train, h = 24)
drift_hsales <- rwf(hsales_train, drift = TRUE, h = 24)
mean_hsales <- meanf(hsales_train, h = 24)

autoplot(snaive_hsales) +
  autolayer(hsales_test)
autoplot(naive_hsales) +
  autolayer(hsales_test)
autoplot(drift_hsales) +
  autolayer(hsales_test)
autoplot(mean_hsales) +
  autolayer(hsales_test)

writeLines("Snaive method")
accuracy(snaive_hsales, hsales_test)

writeLines("\nNaive method")
accuracy(naive_hsales, hsales_test)

writeLines("\nDrift method")
accuracy(drift_hsales, hsales_test)

writeLines("\nMean method")
accuracy(mean_hsales, hsales_test)

#El m�todo naif estacional hizo el mejor.


#d. Verifique los residuos de su m�todo preferido. �Se parecen al ruido blanco?


checkresiduals(snaive_hsales) 
# Pero los residuos no se parecen al ruido blanco


