# Chapter 2


## 1. Utilice la funci�n de ayuda para explorar lo que la serie gold, woolyrnqy gasrepresentar.


library(fpp2)
str(gold)
str(woolyrnq)
str(gas)


# a. Use autoplot para trazar cada uno de estos en gr�ficos separados.

autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)
writeLines ("")


# b. �Cu�l es la frecuencia de cada serie de productos b�sicos?


frequency(gold)

frequency(woolyrnq)

frequency(gas)



# C. Utilice which.max () para detectar el valor at�pico en la serie


print("�Cu�ndo el oro (gold) alcanz� el valor m�ximo?")
which.max(gold)
print("�Cu�l fue el valor m�ximo del oro (gold) ? ")
gold[which.max(gold)]



## 2. Descargue el archivo tute1.csvdel sitio web del libro , �bralo en Excel (o alguna otra aplicaci�n de hoja de c�lculo) y revise su contenido. Deber�a encontrar cuatro columnas de informaci�n. Cada una de las columnas B a D contiene una serie trimestral, denominada Sales, AdBudget y GDP. Ventas contiene las ventas trimestrales de una peque�a empresa durante el per�odo 1981-2005. AdBudget es el presupuesto publicitario y el PIB es el producto interno bruto. Todas las series se han ajustado por inflaci�n.





# a.Puede leer los datos en R con el siguiente script:
tute1 <- read.csv("tute1.csv", header=TRUE)


# b. Convierta los datos en series de tiempo

mytimeseries <- ts(tute1[,-1], start=1981, frequency=4)


# (El [,-1] elimina la primera columna que contiene los cuartos, ya que no los necesitamos ahora).

# c. Construya gr�ficas de series de tiempo de cada una de las tres series

autoplot(mytimeseries, facets=TRUE)


autoplot(mytimeseries)





## 3.Descargue algunos datos minoristas australianos mensuales del sitio web del libro . Estos representan ventas minoristas en varias categor�as para diferentes estados australianos y se almacenan en un archivo MS-Excel




# a. Puede leer los datos en R con el siguiente script:

retaildata <- xlsx::read.xlsx("retail.xlsx", sheetIndex = 1, startRow = 2)


# b. Seleccione una de las series de tiempo de la siguiente manera (pero reemplace el nombre de la columna con su propia columna elegida):

myts <- ts(retaildata[,"A3349873A"], frequency=12, start=c(1982,4))

# c. Explore la serie de tiempo minorista elegida mediante las siguientes funciones:

autoplot(myts)
ggseasonplot(myts)
ggsubseriesplot(myts)
gglagplot(myts, lags = 12)
ggAcf(myts)



## 4.  Crear gr�ficos de tiempo de la siguiente serie de tiempo: bicoal, chicken, dole, usdeaths, lynx, goog, writing, fancy, a10, h02.


# - �selo help()para conocer los datos de cada serie.

help(bicoal)
help(chicken)
help(dole)
help(usdeaths)
help(lynx)
help(goog)
help(writing)
help(fancy)
help(a10)
help(h02)

# - Para el googgr�fico, modifique las etiquetas y el t�tulo del eje.
autoplot(goog) +
  ggtitle("Precios de cierre diarios de las acciones de Google Inc.") +
  xlab("Tiempo") +
  ylab("Precio(Unit: US$)")





## 5. Utilice los ggseasonplot() y ggsubseriesplot()las funciones para explorar los patrones estacionales en las siguientes series de tiempo:




# - �Qu� puedes decir sobre los patrones estacionales?
# - �Puedes identificar algunos a�os inusuales?

ggseasonplot(writing)
ggsubseriesplot(writing)
# La cantidad de papel vendido cae anualmente en agosto

ggseasonplot(fancy)
ggsubseriesplot(fancy)
#  En diciembre de 1992, las ventas mensuales de una tienda de souvenirs aumentaron dr�sticamente en comparaci�n con el mismo mes del a�o pasado.

ggseasonplot(a10)
ggsubseriesplot(a10)

# La cantidad  mensual de antidiabetes cae anualmente en febrero

ggseasonplot(h02)
ggsubseriesplot(h02)

#  La cantidad mensual de corticosteroides disminuye anualmente en febrero



## 6. Utilizar las funciones siguientes gr�ficos: autoplot(), ggseasonplot(), ggsubseriesplot(), gglagplot(), ggAcf()y explorar las caracter�sticas de las siguientes series de tiempo: hsales, usdeaths, bricksq, sunspotarea, gasoline.


# - �Puedes detectar alguna estacionalidad, ciclicidad y tendencia?
# - �Qu� aprendes sobre la serie?

autoplot(hsales)
ggseasonplot(hsales)
ggsubseriesplot(hsales)
gglagplot(hsales)
ggAcf(hsales, lag.max = 400)

#  puede detectar la estacionalidad y la ciclicidad. El per�odo del ciclo es de aproximadamente 4 a�os (100 meses)

autoplot(usdeaths)
ggseasonplot(usdeaths)
ggsubseriesplot(usdeaths)
gglagplot(usdeaths)
ggAcf(usdeaths, lag.max = 60)

# puede detectar la estacionalidad

autoplot(bricksq)
ggseasonplot(bricksq)
ggsubseriesplot(bricksq)
gglagplot(bricksq)
ggAcf(bricksq, lag.max = 200)

# puede detectar poca estacionalidad y una fuerte tendencia

autoplot(sunspotarea)

# ggseasonplot (sunspotarea)
# no estacional, no puedo dibujarlo
# ggsubseriesplot (sunspotarea)
# no estacional, in�til dibujarlo

gglagplot(sunspotarea)
ggAcf(sunspotarea, lag.max = 50)
#  puede detectar una ciclicidad fuerte



autoplot(gasoline)
ggseasonplot(gasoline)
# ggsubseriesplot(gasoline)
#  La cantidad de semanas es 52 y parece que es demasiado para la trama de la subserie.
gglagplot(gasoline)
ggAcf(gasoline, lag.max = 1000)
# puede detectar la estacionalidad y la tendencia



## 7. El arrivalsconjunto de datos comprende llegadas internacionales trimestrales (en miles) a Australia desde Jap�n, Nueva Zelanda, Reino Unido y Estados Unidos.



# ver estructura
str(arrivals)

# - Use autoplot, ggseasonplot and ggsubseriesplot to compare the differences between the arrivals from these four countries.Utilizar autoplot(), ggseasonplot()y ggsubseriesplot()para comparar las diferencias entre las llegadas de estos cuatro pa�ses.

# - �Puede identificar alguna observaci�n inusual?

autoplot(arrivals)

# El mayor n�mero de llegadas provino de Nueva Zelanda en la d�cada de 1980. Y el pa�s propietario del t�tulo cambi� a Jap�n en la d�cada de 1990 y regres� al Reino Unido en la d�cada de 2000.
# Los datos de llegada de Reino Unido muestran la mayor fluctuaci�n trimestral.

ggseasonplot(arrivals[, "Japan"])
ggseasonplot(arrivals[, "NZ"])
ggseasonplot(arrivals[, "UK"])
ggseasonplot(arrivals[, "US"])

ggsubseriesplot(arrivals[, "Japan"])
ggsubseriesplot(arrivals[, "NZ"])
ggsubseriesplot(arrivals[, "UK"])
ggsubseriesplot(arrivals[, "US"])

# Las llegadas de Jap�n disminuyen mucho en el 2� trimestre en comparaci�n con los otros cuartos.
# Las llegadas de Nueva Zelanda son m�s altas en el tercer trimestre y m�s bajas en el primer trimestre.
# Las llegadas de Reino Unido y EE. UU. Son bajas en el 2� y 3� trimestres y altas en el 1� y 4� trimestres.




##8. Las siguientes gr�ficas de tiempo y gr�ficas de ACF corresponden a cuatro series de tiempo diferentes. Su tarea es hacer coincidir cada gr�fico de tiempo en la primera fila con uno de los gr�ficos de ACF en la segunda fila.


autoplot(cowtemp)+
  ggtitle("Daily temperature of cow.")
acf(cowtemp)


autoplot(usdeaths)+ggtitle("Month accidental deaths.")
acf(usdeaths)


autoplot(airpass)+ggtitle("Month air passengers.")
acf(airpass)

autoplot(mink)+ggtitle("Annual number of minks trapped in McKenzie river district of northwest Canada: 1848-1911.")
acf(mink)





## 9. Los pigs datos muestran el n�mero total mensual de cerdos sacrificados en Victoria, Australia, desde enero de 1980 hasta agosto de 1995. Utilice mypigs <- window(pigs, start=1990)para seleccionar los datos a partir de 1990. Utilice autoploty ggAcfpara las mypigsseries y comp�relas con los gr�ficos de ruido blanco de las Figuras 2.17 y 2.18 .



mypigs <- window(pigs, start=1990)
str(mypigs)

autoplot(mypigs)
ggAcf(mypigs)
# # puede encontrar que 3 valores de autocorrelaci�n estaban fuera de los l�mites. Por lo tanto, mypigs probablemente no sea ruido blanco.



## 10. djcontiene 292 d�as de negociaci�n consecutivos del �ndice Dow Jones. �selo ddj <- diff(dj)para calcular los cambios diarios en el �ndice. Parcela ddjy su ACF. �Los cambios en el �ndice Dow Jones parecen ruido blanco?


ddj <- diff(dj)
str(ddj)

autoplot(ddj)
ggAcf(ddj)

#puede encontrar que sustancialmente menos del 5% de los valores de autocorrelaci�n estaban fuera de los l�mites. Por tanto, ddj puede ser ruido blanco.

