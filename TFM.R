setwd("C:/Users/User/Desktop/MASTERADO TFM/TFM DATA")

library(readxl)
library(ggplot2)
library(GGally)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(psych)
library(rrcov)
library(ggrepel)

df <- read_excel("Data_Final_Project.xlsx")
#View(df)

colnames(df)
#View(df)
# [1] "Cities"                                  "Walkability Categorical"                
# [3] "Contamination Categorical"               "More than 500,000 people"               
# [5] "Violent Crime per 1000"                  "Median Gross Rent 2017-2021 Census Data"
# [7] "Median household income"                 "2016 Vehicles Per Household"            
# [9] "Walk Score"                              "Income/Rent"                            
# [11] "Population July 2022"                    "Property Crime per 1000"                
# [13] "Total Crime per 1000"                   

df$`Walkability Categorical` <- factor(df$`Walkability Categorical`)
df$`Contamination Categorical` <- factor(df$`Contamination Categorical`)
df$Cities <- factor(df$Cities)
df$`More than 500,000 people` <- factor(df$`More than 500,000 people`)

dim(df)
# [1] 21 13

#View(df)

# Creamos un dataframe que tenga solamente las variables numéricas que 
# no tengan outliers por cuanto el PCA es sensible a outliers
df_numerical <- df[, 5:9]
colnames(df_numerical)
# [1] "Violent Crime per 1000"                  "Median Gross Rent 2017-2021 Census Data"
# [3] "Median household income"                 "2016 Vehicles Per Household"            
# [5] "Walk Score"   

# Y otro con las variables categóricas
df_categorical <- df[, 1:4]
#View(df_categorical)
colnames(df_categorical)
# [1] "Cities"                    "Walkability Categorical"  
# [3] "Contamination Categorical" "More than 500,000 people" 

df_numerical <- as.data.frame(df_numerical)
rownames(df_numerical) <- df$Cities
#View(df_numerical)

# Comparamos las diferentes variables numéricas
# usando boxplots para cada variable pero primero
# escalamos las variables para que todas estén
# en los mismos rangos

df_scale <- as.data.frame(scale(df_numerical))

# Convertimos las variables a una escala común, es decir normalizamos los datos.

# Transforma el DataFrame a formato largo
df_long_scale <- melt(df_scale , id.vars = NULL)

# Usamos los boxplots para comparar visualmentes las diferentes variables a nivel de EDA.
ggplot(data = df_long_scale, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Variable", y="Value", title="Boxplots of Numerical Variables") +
  coord_flip() +
  theme(legend.position = "none")

# Observamos que las variables que hemos elegido no tienen outliers
# Algo muy importante porque el PCA es sensible a outliers


# El conjunto de datos contiene información sobre 21 ciudades de los Estados Unidos:
# Las primeras variables son categóricas:
# Nombre de las ciudades
# Walkable o NO (Variable Binaria)
# Contaminación o NO (Variable Binaria)
# Más de medio millón de habitantes (Variable Binaria)

# Las últimas variables son numéricas:
# Número de Crímenes Violentos por Cada 1000 Personas
# Mediana de Valores Pagados de Renta 2017-2021
# Mediana de Ingresos
# Ingresos Dividido para Pago de Renta
# Número de Vehículos por Casa
# Calificación de Walkability de 0 a 100 (100 es Fantástico)

###########################################################

# Contar los números de cada nivel en la columna 'Walkability Categorical'
conteos <- table(df_categorical$"Walkability Categorical")

# Convertir la tabla en un dataframe para el gráfico
df_plot <- data.frame(Walkability_Categorical = names(conteos),
                      Count = as.vector(conteos))

# Crear el gráfico de barras
ggplot(df_plot, aes(x = factor(Walkability_Categorical), y = Count, fill=factor(Walkability_Categorical))) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("brown", "green")) +
  labs(x = "Walkability Categorical", y = "Count") +
  theme_minimal()

# Contar los números de cada nivel en la columna 'Contamination Categorical'
conteos <- table(df_categorical$"Contamination Categorical")

# Convertir la tabla en un dataframe para el gráfico
df_plot <- data.frame(Contamination_Categorical = names(conteos),
                      Count = as.vector(conteos))

# Crear el gráfico de barras
ggplot(df_plot, aes(x = factor(Contamination_Categorical), y = Count, fill = factor(Contamination_Categorical))) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Contamination Categorical", y = "Count") +
  theme_minimal()

# Contar los números de cada nivel en la columna 'More than 500,000 people'
conteos <- table(df_categorical$`More than 500,000 people`)

# Convertir la tabla en un dataframe para el gráfico
df_plot <- data.frame(More_than_500000_people = names(conteos),
                      Count = as.vector(conteos))

# Crear el gráfico de barras
ggplot(df_plot, aes(x = factor(More_than_500000_people), y = Count, fill = factor(More_than_500000_people))) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("green", "orange")) +
  labs(x = "More than 500,000 people", y = "Count") +
  theme_minimal()

sum(is.na(df_numerical))
# Observamos que no hay NA en el dataframe
# algo muy importante para el PCA análisis

# Como no tenemos ni NAs, ni outliers
# solamente nos quedaría aplicar el test
# de esfericidad de Barlett.

#######################################################################################
# Antes de realizar el PCA realizamos la Prueba de esfericidad de Bartlett
# Hipótesis nula: no existe colinealidad entre las variables (no se puede realizar el PCA)
# Usamos solamente las variables numéricas

cortest.bartlett(df_numerical)
# 
# R was not square, finding R from data
# $chisq
# [1] 71.58883
# 
# $p.value
# [1] 2.185706e-11
# 
# $df
# [1] 10

# Explicación de los resultados:

# El valor p es muy pequeño, 
# lo que rechaza la hipótesis nula de que no existe colinealidad
# entre las variables.

# Para un nivel de significacón del 5%, como p-value es <.05, podemos decir que
# las variables no son una ‘matriz identidad’ en la que las correlaciones ocurren solo
# por un error de muestreo. 

# Por lo tanto, la utilización de PCA es válida.

# Realizamos el PCA
list.pca <- PCA(df_numerical, scale.unit = TRUE, ncp = 2, graph = FALSE)

summary(list.pca)
# Al applicar summary a los resultados del PCA obtenemos un resumen 
# tanto de la varianza como de los individuos, como de las variables originales
# en relación con los PCA variables.

class(list.pca)

fviz_pca_var(list.pca, repel=TRUE)

# Recordamos lo siguiente:

# Ángulos:

# ángulo pequeño = correlación positiva
# si el ángulo es 0 la correlación es 1

# ángulo grande = correlación negativa
# grande significa en cuadrantes opuestos.
# correlación -1 es ángulo de 180 grados.

# ángulo = 90º, sin correlación, es decir correlación 0 o cercana a 0

# Distancia:

# flecha larga -> 
# buena representación en el PCA
# flecha corta -> 
# mala representación en el PCA

#INTERPRETACION INICIAL DE LOS RESULTADOS:
# Las dos primeras variables del PCA están interpretando
# al menos un 75% de la varianza.
# Observamos que la facilidad para caminar y el número de autos por casas
# son opuestas. Es decir que mientras más autos por casa menos facilidad para
# caminar en las ciudades. Lo cual se puede interpretar que si es fácil moverse
# a pie, la población compra menos autos. Esto sugeriría que tener un excelente
# sistema de transporte ayudaría a que hayan menos autos en las ciudades
# lo cual ayudaría a mejorar la calidad del aire en las ciudades.

# También observamos que hay un ángulo pequeño entre la 
# Mediana de Ingreso por hogar y la Mediana de lo que se paga por renta
# Esto se podría interpreta como que los dueños de casa incrementan el valor
# que cobran por renta, cuando saben que los clientes tienen recursos para pagar.
# Lo clientes compiten por las mejores ubicaciones pagando precios altos,
# siempre y cuando su presupuesto se lo permita.

#Algo que nos faltó mencionar es que
# Es que todas las flechas son grandes
# excepto los crímenes violentos por cada 1000 habitantes.
# Talvez se podría interpretar que no hay mayores
# diferencias en este aspecto

# Las otras flechas fueron analizadas anteriormente.

var <- get_pca_var(list.pca)
var

# Podemos ver los números
# cuánto influyen las diferentes variables
# en la dimensiones 1 y 2 del PCA

var$coord
var$cor
var$cos2
var$contrib

# Dim.1     Dim.2
# Violent Crime per 1000                   0.687368  8.176636
# Median Gross Rent 2017-2021 Census Data 25.019686 24.191729
# Median household income                 29.717046 15.294179
# 2016 Vehicles Per Household             23.164661 26.181427
# Walk Score                              21.411239 26.156028

# Estos resultados indican que la variable crimen
# no tiene mayor efecto
# Se podría interpretar que las otras variables
# afectan de alguna manera el número de autos
# por hogar, mientras que el crimen no.

# Podemos ver los números
# cuánto influyen las diferentes variables
# en la dimensiones 1 y 2 del PCA

# Violent Crime per 1000                  -0.1356380 -0.3002817
# Median Gross Rent 2017-2021 Census Data  0.8183287  0.5165055
# Median household income                  0.8918456  0.4106810
# 2016 Vehicles Per Household             -0.7874080  0.5373264
# Walk Score                               0.7570207 -0.5370657

# Mostrar la varianza explicada y los porcentajes acumulados

fviz_contrib(list.pca, choice="var", axes = 1)

fviz_contrib(list.pca, choice="var", axes = 2)

list.pca$eig
# 
# eigenvalue percentage of variance
# comp 1 2.67653979             53.5307959
# comp 2 1.10276502             22.0553004
# comp 3 1.00920295             20.1840590
# comp 4 0.18035942              3.6071885
# comp 5 0.03113281              0.6226562
# cumulative percentage of variance
# comp 1                          53.53080
# comp 2                          75.58610
# comp 3                          95.77016
# comp 4                          99.37734
# comp 5                         100.00000

# Observamos que los 3 primeros PCA son importantes.
# Sin embargo, como queremos hacer un gráfico
# en dos dimensiones usaremos solamente 2 PCA.
# La primera dimensión explica el 53.5%
# La segunda dimensión explica el 22%
# La tercera dimensión explica el 20%


# Scree Plot
# Gráfico de scree para ver la varianza explicada por cada componente
fviz_eig(list.pca, addlabels = TRUE)
# Observamos un codo luego de la segunda variable

get_eigenvalue(list.pca)
# Esta función explica tanto los PCA que se ven en el plot circular
# pero también los que no se ven.

fviz_cos2(list.pca, choice="var", axes=1)
#cos2 indica la calidad de la variable en relación al componente principal
# Recordemos que si el ángulo es 0 o cercano a 0
# la variable está bien representada en el componente principal
# También recordemos que el cos de 0 grados es 1
# El coseno del angulo entre la variable y el componente principal
# lo obtenemos al elevar al cuadrado la coordenada que es la correlación
# Por supuesto si es coseno es cercano a 0, 
# indica que el ángulo es 90 grados o cercano a 90 grados porque el coseno de 90 es 0
# Recordemos que cuando el angulo es 90 grados no hay correlación
# Es decir la variable no está representada en ese PCA.

# Observamos que para Dim 1
# las variables más importantes para la CALIDAD de la representación son:
#Todas menos crímenes violentos por cada 1000 habitantes

fviz_cos2(list.pca, choice="var", axes=2)
# Para Dim2, en lo referente a la CALIDAD DE LA REPRESENTACION nuevamente crímenes violentos por cada 1000 habitantes 
# no es importante.

# Podríamos intentar usar nuevamente el PCA, esta vez
# sin considerar los crímenes

#########################################################################

#############################################################

# dimdesc. Descripción de las dimensiones
# La función dimdesc se utiliza para obtener
# una descripción detallada de las dimensiones (o ejes)
# en un análisis de componentes principales

res_desc <- dimdesc(list.pca, axes= c(1,2), proba=0.05)

res_desc

res_desc$Dim.1
# 
# Link between the variable and the continuous variables (R-square)
# =================================================================================
#   correlation      p.value
# Median household income                   0.8918456 5.691343e-08
# Median Gross Rent 2017-2021 Census Data   0.8183287 5.811941e-06
# Walk Score                                0.7570207 7.107245e-05
# 2016 Vehicles Per Household              -0.7874080 2.272695e-05

# Aquí observamos los factores de correlación entre la Dimensión 1
# y las diferentes variables que tienen un p value inferior a 0.05

res_desc$Dim.2

# Para la dimensión 2 solamente hay 3 variables que tienen una
# correlación intermedia


# Para seleccionar un número automático de grupos
# usamos -1

hcpc_with_graph <- HCPC(list.pca, nb.clust = -1, graph=TRUE)

hcpc_without_graph <- HCPC(list.pca, nb.clust = -1, graph=FALSE)
# Esta función utiliza el criterio de Ward y K-Medias

# Visualize dendrogram
fviz_dend(hcpc_without_graph, show_labels = TRUE, rect = TRUE)

# Don't color labels, add rectangles
fviz_dend(hcpc_without_graph, cex = 0.5, k = 3, 
          color_labels_by_k = FALSE, rect = TRUE)

# Grafica los individuos en el mapa
fviz_cluster(hcpc_without_graph, show.clust.cent= TRUE)

# Gráfico Tridimensional
plot(hcpc_without_graph, choice = "3D.map", ind.names =  FALSE )
plot(hcpc_without_graph, choice = "3D.map", ind.names =  TRUE )

hcpc_without_graph$desc.var$quanti.var

hcpc_without_graph$desc.var$quanti

hcpc_without_graph$desc.ind$para
# para significa parangón (individuo modelo)
# los individuos más importantes son aquellos que 
# tienen menores números

hcpc_without_graph$desc.ind$dist
#dist es distancia
#Los resultados indican que tan lejos están los individuos
# de los otros individuos

##############################################################################

# Como punto final combinaremos los resultados de PCA
# con otras variables que tenemos en el conjunto de datos
# En particular con:
# 1) si una ciudad es contaminada o no
# 2) La población de cada ciudad
# 3) Usaremos ggplot2

# Crear un dataframe a partir de los resultados de PCA
df_pca <- data.frame(Dim.1 = list.pca$ind$coord[,1], Dim.2 = list.pca$ind$coord[,2], Contamination=df$`Contamination Categorical`,
                     Population = df$`Population July 2022`,
                     Walkability_Categorical= df$`Walkability Categorical`,
                     More_than_500000_people = df$`More than 500,000 people`)
#View(df_pca)

ggplot(data = df_pca, aes(x = Dim.1, y = Dim.2, color = Contamination, size = Population,  label = rownames(df_pca))) +
  geom_point() + 
  scale_color_manual(values = c("blue", "red")) +
  geom_text_repel(max.overlaps = Inf) +
  theme_minimal()

# Luego que hemos realizado el PCA
# realizamos un Scatter Plot, en el cual hemos agregado
# algunas columnas que no usamos para el PCA
# pero que nos permiten entender mejor
# como las ciudades se parecen o se diferencian.

# Observamos 4 clusters.

# El primer cluster lo forman las ciudades con gran población:
# New York,
# Chicago,
# Philadelphia,
# Washington DC
# San Diego,
# Honolulu,
# San Francisco,
# y Seattle.

# Tienen poblaciones grandes y 
# Contaminación
# La excepciones, en cuanto a contaminación son
# Montpelier y Honolulu.

# Montpelier, Vermont tiene una población pequeña 10,000 habitantes
# y se puede especular que por ello no tiene contaminación.
# Es muy interesante que Honolulu no tiene contaminación.
# Sin embargo, podemos suponer que la única causa que no 
# es contaminada es por su ubicación geográfica,
# al ser una isla en el medio del Oceano Pacífico.


# El segundo cluster lo forman,
# Buffalo, 
# Ponce, 
# Rochester,
# y New Orleans.
# Tiene población pequeña y no contaminación.
# Seguramente Montpelier, Vermont debería estar 
# en ese cluster. Está muy cerca

# Un tercer cluster de ciudades con
# alta contaminación lo forman:
# Las Vegas, 
# Phoenix,
# Dallas
# Salt Lake City,

# Un cuarto cluster, similar en muchas
# características, excepto la contaminación,
# lo forman:

# Fort Myers, 
# Orlando,
# Wilmington-North Carolina,
# y Charlottesville-Virginia

# Muy interesante que Salt Lake city
# a pesar de tener una población pequeña
# es contaminada.
# La potencial explicación es su ubicación geográfica.
# Está ubicada en un valle, y parece que eso
# agrava el problema de la contaminación

ggplot(data = df_pca, aes(x = Dim.1, y = Dim.2, color = Walkability_Categorical , size = Population,  label = rownames(df_pca))) +
  geom_point() + 
  scale_color_manual(values = c("orange", "green")) +
  geom_text_repel(max.overlaps = Inf) +
  theme_minimal()

# En este último gráfico observamos que la mayor parte de las 
# ciudades que son walkable tiene una gran población.
