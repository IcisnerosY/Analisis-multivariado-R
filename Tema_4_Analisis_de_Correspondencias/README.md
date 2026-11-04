# =====================================================================
# Analisis multivariado y metodos de clasificacion de datos con R
# Tema 4. Sesion 12: Analisis de correspondencias simple (practica)
# Profesor: Isaac Cisneros Yescas
# Centro de Estudios Sociologicos, El Colegio de Mexico
# Archivo: Sesion12_AnCorr_CNEP2024_v1_1.R
# =====================================================================
#
# OBJETIVO
# Aplicar el analisis de correspondencias (AC) simple en R, en dos
# partes:
#
#   Parte A (pasos 1 a 9). Replicar el ejemplo de la presentacion:
#            ideologia por disposicion politica (963 casos). La tabla se
#            escribe a mano, asi que no se necesita ninguna base.
#   Parte B (pasos 10 a 14). Ejercicio con la CNEP Mexico 2024:
#            autoubicacion ideologica por voto presidencial 2024.
#
# COMO USAR ESTE SCRIPT
# - Correr linea por linea: poner el cursor en la linea y presionar
#   Ctrl + Enter (PC) o Cmd + Enter (Mac). Si una instruccion ocupa
#   varias lineas, basta con poner el cursor en la primera.
# - Las lineas que empiezan con # son comentarios: R no las ejecuta.
# - En cada paso hay tres tipos de comentario:
#     QUE HACE     explica el codigo y sus argumentos
#     RESULTADO    lo que debe aparecer en la consola
#     LECTURA      como interpretar el resultado
# - Al copiar codigo, usar comillas rectas (" ") y no pegar los
#   simbolos > y + de la consola: provocan errores de sintaxis.
#
# ANTES DE EMPEZAR
# Si en la misma sesion de R se corrio la practica de MDS con la ONU,
# reiniciar R: Session > Restart R (Ctrl + Shift + F10 en PC;
# Cmd + Shift + F10 en Mac). Esa practica carga el paquete vegan, que
# tiene otra funcion llamada ca() y choca con la del paquete ca.
# rm(list = ls()) NO basta: borra los objetos, pero no descarga
# los paquetes.
#
# RECORDATORIO
# Los signos de los ejes son arbitrarios. Si un mapa sale reflejado
# respecto a la presentacion, el resultado es el mismo.
# =====================================================================


# =====================================================================
# PASO 0. PAQUETES
# =====================================================================
# QUE HACE
# Revisa que paquetes faltan, instala solo esos y luego los carga.
#   FactoMineR  funcion CA(): el analisis de correspondencias
#   ca          funcion ca(): solo para el mapa asimetrico (paso 9)
#   haven       funcion read_dta(): leer bases de Stata (.dta)
#   dplyr       funciones para preparar datos (mutate, case_when...)

paquetes <- c("FactoMineR", "ca", "haven", "dplyr")

# installed.packages() lista los paquetes instalados en la computadora.
# %in% pregunta "esta en la lista?"; el ! invierte la respuesta.
# Resultado: un vector con los paquetes que NO estan instalados.
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]

# Si falta alguno (length mayor que 0), lo instala. Si no, no hace nada.
if (length(faltan) > 0) install.packages(faltan)

# library() carga cada paquete en la sesion.
library(FactoMineR)
library(ca)
library(haven)
library(dplyr)

# RESULTADO: sin mensajes, o mensajes informativos de carga.
# Si aparece "there is no package called ...", el paquete no se
# instalo: correr de nuevo install.packages("nombre").


# =====================================================================
# PARTE A. REPLICAR EL EJEMPLO DE LA PRESENTACION
# =====================================================================


# =====================================================================
# PASO 1. ESCRIBIR LA TABLA DE CONTINGENCIA
# =====================================================================
# El AC solo necesita una tabla de contingencia. Cuando no se tienen
# los microdatos, sino solo la tabla (por ejemplo, de un articulo o
# de una salida de Stata), se puede escribir a mano.
#
# QUE HACE
# matrix() arma una tabla a partir de una lista de numeros.
#   c(...)        los 40 numeros de la tabla, en el orden en que se
#                 leen: primero la fila 1 completa, luego la fila 2...
#   nrow = 8      ocho filas (categorias de ideologia); R deduce que
#                 hay cinco columnas (40 / 8)
#   byrow = TRUE  llena la tabla por filas. IMPORTANTE: sin este
#                 argumento R la llena por columnas y la tabla queda mal
#   dimnames      nombres de filas y de columnas. Los nombres de la
#                 lista (ideologia, disposicion) aparecen como titulos.
#                 Se usan guiones bajos y no acentos para evitar
#                 problemas de codificacion entre Mac y PC.

tabla <- matrix(c(108, 67, 77, 56, 21,
                    6,  9,  5,  7, 11,
                   36, 38, 39, 34, 43,
                   10, 21, 30, 29, 43,
                   14, 18, 21, 27, 22,
                   13, 23, 31, 17, 17,
                   13, 14, 10, 13,  8,
                    5,  3,  2,  1,  1),
                nrow = 8, byrow = TRUE,
                dimnames = list(
                  ideologia = c("Sin_adscripcion", "Extrema_izq", "Izquierda",
                                "Centro_izq", "Centro", "Centro_der",
                                "Derecha", "Extrema_der"),
                  disposicion = c("Nula", "Baja", "Incipiente",
                                  "Moderada", "Alta")))

tabla   # imprime la tabla para revisarla

# addmargins() agrega una fila y una columna de totales (Sum).
# Sirve para verificar que la tabla se escribio bien.
addmargins(tabla)

# RESULTADO
# La esquina inferior derecha debe decir 963.
# Totales de fila: 329, 38, 190, 133, 102, 101, 58, 12.
# Totales de columna: 205, 193, 215, 184, 166.
# Si no coinciden, revisar los numeros o el argumento byrow = TRUE.
# Verificar con la diapositiva

# =====================================================================
# PASO 2. PERFILES FILA
# =====================================================================
# El AC no compara frecuencias, compara PERFILES: la distribucion en
# porcentajes de cada fila. Asi, dos categorias de tamano muy distinto
# pero con la misma distribucion son, para el AC, el mismo punto.
#
# QUE HACE
#   prop.table(tabla, 1)   divide cada celda entre el total de su fila.
#                          El 1 significa "por filas" (con 2 serian
#                          perfiles columna). Cada fila suma 1.
#   * 100                  lo pasa a porcentaje
#   round(..., 1)          redondea a un decimal, solo para leer mejor

round(prop.table(tabla, 1) * 100, 1)

# El PERFIL PROMEDIO es la distribucion del total de la tabla:
# total de cada columna entre el total general. Es el punto de
# referencia: en el mapa queda en el origen.
round(colSums(tabla) / sum(tabla) * 100, 1)

# RESULTADO
# Perfil promedio: Nula 21.3, Baja 20.0, Incipiente 22.3,
#                  Moderada 19.1, Alta 17.2.
#
# LECTURA
# - Sin_adscripcion: 32.8% en Nula y solo 6.4% en Alta.
# - Centro_izq: casi el espejo, 7.5% en Nula y 32.3% en Alta.
# - Extrema_der: 41.7% en Nula, pero son solo 12 casos.
# Se puede anticipar que Sin_adscripcion y Centro_izq quedaran en
# extremos opuestos del mapa.


# =====================================================================
# PASO 3. PRUEBA CHI-CUADRADO, RESIDUOS E INERCIA TOTAL
# =====================================================================
# Antes del AC hay que saber si hay asociacion (chi-cuadrado), donde
# esta (residuos) y que tan fuerte es (inercia).

# --- 3a. Prueba chi-cuadrado ---
# QUE HACE
# chisq.test() prueba la hipotesis nula de independencia: que la
# disposicion politica se distribuye igual en todas las categorias
# ideologicas. El resultado se guarda en "prueba" para extraer despues
# sus partes con $.
prueba <- chisq.test(tabla)
prueba

# RESULTADO
#   X-squared = 104.73, df = 28, p-value = 8.545e-11
#   Warning message: Chi-squared approximation may be incorrect
#
# LECTURA
# - X-squared: suma de (observado - esperado)^2 / esperado en las 40
#   celdas. Mide cuanto se aparta la tabla de la independencia.
# - df: grados de libertad = (8 - 1) x (5 - 1) = 28.
# - p-value = 0.00000000008545: se rechaza la independencia con mucha
#   holgura. Ideologia y disposicion politica estan asociadas.
# - Ojo: un resultado significativo dice que HAY asociacion en la
#   poblacion, no que sea FUERTE. Y uno no significativo no demuestra
#   independencia: puede deberse a una muestra pequena.
#
# La ADVERTENCIA no es un error. Aparece porque hay frecuencias
# esperadas menores que 5. No afecta al AC, que no usa el
# valor p. Si se quiere un valor p sin la aproximacion, se puede
# calcular por simulacion (opcional):
#   set.seed(2024)
#   chisq.test(tabla, simulate.p.value = TRUE, B = 10000)

# --- 3b. Frecuencias esperadas ---
# QUE HACE
# $expected da lo que habria en cada celda si las variables fueran
# independientes: total de fila x total de columna / n.
round(prueba$expected, 1)

# RESULTADO
# Toda la fila Extrema_der queda entre 2.1 y 2.7: por eso la
# advertencia. Es la categoria con solo 12 casos.

# --- 3c. Residuos de Pearson ---
# QUE HACE
# $residuals = (observado - esperado) / raiz(esperado).
#   Positivo  = mas casos de los esperados (atraccion)
#   Negativo  = menos casos de los esperados (rechazo)
#   Mayor que 2 en valor absoluto = desviacion clara
round(prueba$residuals, 1)

# RESULTADO Y LECTURA
# Los residuos mas grandes:
#   Sin_adscripcion x Alta  = -4.7  (21 observados frente a 57 esperados)
#   Sin_adscripcion x Nula  = +4.5  (108 frente a 70)
#   Centro_izq x Alta       = +4.2  (43 frente a 23)
#   Centro_izq x Nula       = -3.4  (10 frente a 28)
# Estas celdas construiran la dimension 1. El AC no descompone las
# frecuencias: descompone estos residuos.

# --- 3d. Inercia total ---
# QUE HACE
# Inercia total = chi-cuadrado / n. sum(tabla) es n = 963.
prueba$statistic / sum(tabla)

# RESULTADO: 0.1088. R lo rotula "X-squared" porque lo hereda de la
# prueba, pero ya no es la chi-cuadrado: es la inercia.
#
# LECTURA
# 1. Que es: mide cuanto difieren los perfiles de disposicion entre
#    las categorias ideologicas. Si todas tuvieran el mismo perfil,
#    seria 0.
# 2. Que tanto es: por si sola dice poco, porque su maximo depende
#    del tamano de la tabla (min(filas, columnas) - 1 = 4). Traducida
#    a V de Cramer, que va de 0 a 1:
sqrt((prueba$statistic / sum(tabla)) / (min(dim(tabla)) - 1))
#    V = 0.16: asociacion real pero debil a moderada. La ideologia no
#    determina la disposicion politica; la inclina.
# 3. Para que sirve: es el total que el AC va a repartir entre
#    dimensiones.
# 4. Ventaja sobre la chi-cuadrado: no depende de n. Con la misma tabla
#    de porcentajes y 200 o 5,000 casos, la inercia seria la misma.
# ERROR FRECUENTE: leer 0.1088 como "el 11% de algo". No es una
# proporcion: es una magnitud de asociacion.
#
# RESULTADO:
# "Ideologia y disposicion politica estan asociadas (chi2 = 104.73,
#  gl = 28, p < 0.001), aunque la asociacion es modesta (inercia =
#  0.109; V = 0.16)."


# =====================================================================
# PASO 4. ESTIMAR EL ANALISIS DE CORRESPONDENCIAS
# =====================================================================
# QUE HACE
# CA() es la funcion de FactoMineR para el AC simple.
#   as.data.frame.matrix(tabla)  convierte la tabla en data.frame
#                                conservando su forma (filas x columnas),
#                                que es lo que CA() espera.
#                                No usar as.data.frame(tabla): pondria
#                                una fila por celda.
#   graph = FALSE                no dibuja el mapa todavia; lo haremos
#                                en el paso 7, despues de revisar que
#                                categorias estan bien representadas.
res_a <- CA(as.data.frame.matrix(tabla), graph = FALSE)

# RESULTADO: no imprime nada. El objeto res_a es una lista con:
#   res_a$eig   inercia por dimension
#   res_a$row   estadisticos de las filas (ideologia)
#   res_a$col   estadisticos de las columnas (disposicion)
# Cada uno de $row y $col tiene $coord, $contrib, $cos2 e $inertia.
names(res_a)   # para ver los componentes


# =====================================================================
# PASO 5. INERCIA POR DIMENSION: CUANTAS DIMENSIONES RETENER
# =====================================================================
# QUE HACE
# $eig tiene tres columnas:
#   eigenvalue                          inercia principal de cada dimension
#   percentage of variance              porcentaje de la inercia total
#   cumulative percentage of variance   porcentaje acumulado
# FactoMineR dice "variance", pero se trata de inercia.
round(res_a$eig, 4)

# RESULTADO
#   dim 1   0.0926   85.13%   acumulado  85.13%
#   dim 2   0.0088    8.07%   acumulado  93.20%
#   dim 3   0.0048    4.44%
#   dim 4   0.0026    2.36%
# Igual que la salida de la diapositiva 8. Hay 4 dimensiones porque
# min(8, 5) - 1 = 4.
#
# LECTURA
# El plano (dimensiones 1 y 2) conserva el 93.2% de la asociacion:
# el mapa es muy fiel a la tabla. Casi todo esta en la dimension 1.

# Regla de la inercia promedio: retener las dimensiones cuya inercia
# supere el promedio (inercia total / numero de dimensiones).
mean(res_a$eig[, 1])

# RESULTADO: 0.0272. Solo la dimension 1 (0.0926) lo supera.
# LECTURA: la tabla es esencialmente unidimensional. Usamos el plano
# para leer mejor, pero la conclusion debe apoyarse en la dimension 1.


# =====================================================================
# PASO 6. ESTADISTICOS POR CATEGORIA
# =====================================================================
# Es el paso mas importante para interpretar bien el mapa.
#
# QUE HACE
# summary() muestra todo junto.
#   nb.dec = 3   numero de decimales
#   ncp = 2      numero de dimensiones a mostrar
summary(res_a, nb.dec = 3, ncp = 2)

# COMO LEER CADA COLUMNA DE summary()
#   Iner*1000  inercia de la categoria, multiplicada por 1000
#   Dim.1      coordenada en la dimension 1 (normalizacion principal)
#   ctr        CONTRIBUCION, en %: cuanto aporta la categoria a
#              construir la dimension. Sirve para NOMBRAR los ejes.
#   cos2       CALIDAD en esa dimension: que proporcion de la inercia
#              de la categoria explica la dimension. Va de 0 a 1.
#
# DIFERENCIA CLAVE
#   cos2 se mira desde la categoria: la representa bien el mapa?
#   ctr se mira desde el eje: quien lo construye?
#
# LECTURA (con Sin_adscripcion como guia)
#   Iner*1000 = 45.695 -> aporta 45.695 / 108.8 = 42% de la inercia
#   Dim.1 = -0.365     -> esta del lado negativo de la dimension 1
#   ctr = 49.079       -> construye casi la mitad de la dimension 1
#   cos2 = 0.994       -> la dimension 1 la representa casi perfecto
#
# Quien construye cada dimension (ctr mas altos):
#   Dimension 1: Sin_adscripcion (49%) y Centro_izq (34%);
#                Alta (52%) y Nula (45%).
#   Dimension 2: Centro_der (67%) e Incipiente (66%).

# --- 6a. Calidad en el plano ---
# QUE HACE
#   res_a$row$cos2      tabla de cos2 de cada fila en cada dimension
#   [, 1:2]             solo las dimensiones 1 y 2 (las del mapa)
#   rowSums()           suma los dos valores de cada fila. Se pueden
#                       sumar porque las dimensiones son ortogonales.
round(rowSums(res_a$row$cos2[, 1:2]), 3)   # filas
round(rowSums(res_a$col$cos2[, 1:2]), 3)   # columnas

# RESULTADO
# Ejemplo: Derecha = 0.143 (dim 1) + 0.164 (dim 2) = 0.307.
#
# LECTURA
# La calidad va de 0 a 1. Cerca de 1, el mapa muestra fielmente la
# posicion de la categoria. Cerca de 0, su diferencia respecto al
# promedio esta en dimensiones que no vemos (la 3 y la 4).
# Criterio practico: no interpretar categorias con calidad menor
# que 0.5.
#   Muy bien representadas: Sin_adscripcion (0.994), Centro_izq
#     (0.982), Centro_der (0.913).
#   Bien representadas: Extrema_izq, Izquierda, Centro, Extrema_der.
#   MAL representadas: Derecha (0.307), Baja (0.241), Moderada (0.359).
#     Aparecen cerca del origen, pero eso NO significa que tengan un
#     perfil promedio: el mapa no muestra su diferencia.

# --- 6b. Proporcion de la inercia que aporta cada fila ---
round(res_a$row$inertia / sum(res_a$row$inertia), 3)

# RESULTADO: Sin_adscripcion = 0.420; Centro_izq = 0.297.
# LECTURA: entre las dos concentran casi tres cuartas partes de toda la
# asociacion de la tabla.
# Comparar con Extrema_der: tiene el perfil mas extremo (41.7% en Nula),
# pero aporta solo 0.038 porque tiene muy poca masa (12 casos).
# Inercia de una categoria = masa x distancia al promedio al cuadrado.
#
# NOTA: las coordenadas de R estan en normalizacion principal, como el
# mapa de la diapositiva 10, con los signos invertidos. Las de la tabla
# de la diapositiva 9 estan en otra normalizacion y por eso son mas
# grandes (0.661 / 0.365 = 1 / raiz(0.304)). Las inercias, los
# porcentajes y los cos2 si coinciden exactamente.


# =====================================================================
# PASO 7. MAPA SIMETRICO
# =====================================================================
# QUE HACE
# plot() aplicado a un resultado de CA() dibuja filas (azul) y
# columnas (rojo), ambas en coordenadas principales: es el mapa
# simetrico.
#   title   titulo del mapa
plot(res_a, title = "Ideologia y disposicion politica")

# LECTURA (los cuatro criterios de la diapositiva 10)
# 1. Que tan fiel es: 85.1% + 8.1% = 93.2% de la inercia.
# 2. Que no se interpreta: Derecha, Baja y Moderada (calidad < 0.5).
# 3. Dimension 1, el eje principal:
#    - Lado izquierdo: Sin_adscripcion y Nula (y Extrema_der).
#    - Lado derecho: Centro_izq y Alta, con Centro, Izquierda y
#      Extrema_izq del mismo lado.
#    - La disposicion se ordena sola: Nula, Baja, Incipiente,
#      Moderada, Alta. El AC no conocia ese orden: es un resultado.
# 4. Dimension 2 (8%): Centro_der e Incipiente abajo; las extremas
#    arriba. Interpretable, pero debil.
#
# COMO LEER LAS RELACIONES
# - Distancia al origen: cuanto se aparta el perfil del promedio.
# - Cercania entre categorias de la MISMA variable: perfiles parecidos
#   (Centro e Izquierda estan juntas).
# - Direccion desde el origen: una fila y una columna en la misma
#   direccion se atraen (Sin_adscripcion y Nula: 108 frente a 70).
# - ERROR FRECUENTE: la distancia entre un punto azul y uno rojo NO se
#   interpreta en este mapa.
#
# INTERPRETACION SUSTANTIVA
# La dimension 1 no es un eje izquierda-derecha, o no solo eso: el
# polo mas fuerte lo ocupa Sin_adscripcion, que no es una posicion
# ideologica. Lo que organiza el mapa es la identificacion ideologica:
# ubicarse en la escala va de la mano con mayor disposicion politica;
# no ubicarse, con disposicion nula. La autoubicacion funciona tambien
# como indicador de involucramiento politico, no solo de orientacion.


# =====================================================================
# PASO 8. MAPA CON LA CALIDAD COMO FILTRO
# =====================================================================
# QUE HACE
# Aplica graficamente la regla del paso 6: solo etiqueta las
# categorias bien representadas.
#   selectRow = "cos2 0.5"   etiqueta solo las filas con calidad > 0.5
#   selectCol = "cos2 0.5"   lo mismo para las columnas
# Otras opciones: "contrib 5" (las 5 que mas contribuyen) o "cos2 5"
# (las 5 mejor representadas).
plot(res_a, selectRow = "cos2 0.5", selectCol = "cos2 0.5",
     title = "Solo categorias bien representadas")

# RESULTADO: Derecha, Baja y Moderada quedan en gris y sin etiqueta.
# Es una buena practica para los mapas que se publican.


# =====================================================================
# PASO 9. MAPA ASIMETRICO (OPCIONAL)
# =====================================================================
# En el mapa asimetrico, las filas van en coordenadas principales y
# las columnas en coordenadas estandar. Aqui SI se puede leer con
# rigor la relacion entre filas y columnas.
#
# QUE HACE
#   ca::ca(tabla)            estima el AC con el paquete ca. El "ca::"
#                            obliga a usar la funcion de ese paquete,
#                            aunque otro paquete (como vegan) tenga
#                            una funcion con el mismo nombre.
#   map = "rowprincipal"     filas en principal, columnas en estandar
#   arrows = c(FALSE, TRUE)  flechas solo para las columnas (el primer
#                            valor es para filas, el segundo para
#                            columnas)
#   main                     titulo
plot(ca::ca(tabla), map = "rowprincipal", arrows = c(FALSE, TRUE),
     main = "Mapa asimetrico")

# COMO LEERLO
# - Cada flecha apunta a un VERTICE: el perfil hipotetico de una
#   categoria con el 100% de sus casos en esa columna.
# - Cada punto ideologico es el CENTRO DE GRAVEDAD de los vertices:
#   cada flecha "jala" al punto segun el porcentaje de casos que
#   tiene en esa columna. Un perfil igual al promedio queda en el
#   origen.
# - Por eso aqui si importa la cercania: una fila cae mas cerca de la
#   columna en la que tiene mas peso relativo.
#
# LECTURA
# - Nula, Alta e Incipiente: flechas largas en tres direcciones. Son
#   las columnas que estructuran la tabla.
# - Baja y Moderada: flechas muy cortas porque el plano las representa
#   mal (calidad 0.24 y 0.36), no porque no importen.
# - Sin_adscripcion: del lado de Nula y opuesta a Alta.
# - Centro_izq: del lado de Alta y opuesta a Nula.
# - Centro_der: se desplaza hacia Incipiente (31 frente a 22.5).
# - Los puntos quedan amontonados en el centro porque la asociacion es
#   modesta: ningun perfil se acerca a un extremo. El mapa simetrico
#   agranda visualmente las diferencias; este muestra cuan lejos estan
#   los perfiles reales de los extremos posibles.
# - Los signos pueden no coincidir con los del paso 7.
#
# SI APARECE EL ERROR "map is not a graphical parameter":
# hay otra funcion ca() cargada (por ejemplo, de vegan). Reiniciar R o
# asegurarse de escribir ca::ca(tabla).


# =====================================================================
# PARTE B. EJERCICIO CON LA CNEP MEXICO 2024
# Autoubicacion ideologica por voto presidencial 2024
# =====================================================================
# Pregunta sustantiva: la ideologia ordena el voto presidencial? Y
# quienes no se ubican en la escala, a quien votaron?
#
# Variables de la base:
#   C_LRSelf            autoubicacion izquierda-derecha (P82)
#                       1 = izquierda ... 10 = derecha
#                       999 = no sabe o no usa la escala
#                       998 y -999 = no contesto / no aplica
#   H_VoteWhichRecent   voto presidencial 2024
#                       1 = Sheinbaum, 10 = Galvez, 13 = Maynez
#
# Advertencias:
# - La muestra sobrerrepresenta el voto por Sheinbaum (78% del voto
#   reportado frente a cerca de 60% del resultado oficial).
# - La base no tiene ponderadores utilizables: el analisis va sin
#   ponderar.


# =====================================================================
# PASO 10. CARGAR LA BASE
# =====================================================================
# QUE HACE
# read_dta() lee el archivo de Stata conservando las etiquetas.
# file.path() arma la ruta con el separador correcto en Mac (/) y en
# PC (\). Supone que la base esta en una carpeta "datos" dentro del
# proyecto de RStudio.
cnep <- read_dta(file.path("datos", "Mexico2024_Fin.dta"))
cnep <- read_dta("/Users/isaaccisneros/Desktop/ISAAC/CURSO/Cursos 2026/DOCUMENTOS CURSO/R/TEMA 6 CORRESPONDENCIAS/Mexico2024_Fin.dta")

# Alternativa: escribir la ruta completa del archivo en la computadora.
# Funciona, pero hay que cambiarla en cada computadora. 

dim(cnep)

# RESULTADO: 1205 347 (casos y variables).
# Si aparece "cannot open file", R no encuentra la base: revisar la
# carpeta de trabajo con getwd() o usar la ruta completa.
getwd()

# =====================================================================
# PASO 11. RECODIFICAR LAS DOS VARIABLES
# =====================================================================
# QUE HACE
#   transmute()     crea una base nueva solo con las variables que se
#                   indican (aqui, las dos originales con nombres cortos)
#   zap_labels()    quita las etiquetas de Stata y deja los numeros
#   mutate()        crea variables nuevas a partir de las existentes
#   case_when()     recodifica por condiciones. Se lee "si ... entonces
#                   ...". Las condiciones se revisan en orden.
#   self %in% 1:2   "si self vale 1 o 2"
#   TRUE ~ NA       todo lo que no cumplio ninguna condicion queda
#                   como faltante (998, -999 y otros codigos)
#   factor(..., levels = ...)  fija el orden de las categorias. Sin el,
#                   R las ordenaria alfabeticamente y la tabla perderia
#                   el orden izquierda-derecha.
#
# Ideologia: cinco grupos de dos puntos cada uno, mas una categoria
# para quienes no se ubican (999). Se conserva porque es sustantiva:
# en la Parte A, Sin_adscripcion construyo la mitad de la dimension 1.
datos <- cnep %>%
  transmute(
    self = zap_labels(C_LRSelf),
    voto_cod = zap_labels(H_VoteWhichRecent)
  ) %>%
  mutate(
    ideologia = case_when(
      self %in% 1:2  ~ "Izquierda",
      self %in% 3:4  ~ "Centro_izq",
      self %in% 5:6  ~ "Centro",
      self %in% 7:8  ~ "Centro_der",
      self %in% 9:10 ~ "Derecha",
      self == 999    ~ "No_se_ubica",
      TRUE ~ NA_character_),
    voto = case_when(
      voto_cod == 1  ~ "Sheinbaum",
      voto_cod == 10 ~ "Galvez",
      voto_cod == 13 ~ "Maynez",
      TRUE ~ NA_character_)
  ) %>%
  mutate(
    ideologia = factor(ideologia, levels = c("Izquierda", "Centro_izq",
                       "Centro", "Centro_der", "Derecha", "No_se_ubica")),
    voto = factor(voto, levels = c("Sheinbaum", "Galvez", "Maynez"))
  )

# Verificar la recodificacion: cada codigo original debe caer en UNA
# sola categoria nueva.
#   useNA = "ifany"   muestra tambien los faltantes
table(datos$self, datos$ideologia, useNA = "ifany")
table(datos$voto_cod, datos$voto, useNA = "ifany")

# RESULTADO
# Los codigos 1 y 2 de self solo en Izquierda; 999 solo en No_se_ubica;
# 998 y -999 solo en <NA>. En voto: 1, 10 y 13 en una sola categoria y
# el resto en <NA>.


# =====================================================================
# PASO 12. TABLA DE CONTINGENCIA
# =====================================================================
# QUE HACE
# table() cruza las dos variables y descarta los NA: solo entran
# quienes tienen ideologia (o No_se_ubica) y votaron por uno de los
# tres candidatos.
tabla_b <- table(datos$ideologia, datos$voto)
addmargins(tabla_b)                       # tabla con totales
round(prop.table(tabla_b, 1) * 100, 1)    # perfiles fila

# RESULTADO
# n = 779 casos. Votos: Sheinbaum 607, Galvez 92, Maynez 80.
# Grupos: Izquierda 162, Centro_izq 88, Centro 212, Centro_der 98,
# Derecha 119, No_se_ubica 100.
#
# LECTURA
# Perfil promedio del voto: Sheinbaum 77.9%, Galvez 11.8%,
# Maynez 10.3%. Lo que interesa no es que Sheinbaum gane en todos los
# grupos, sino que grupos se apartan de ese promedio.


# =====================================================================
# PASO 13. CHI-CUADRADO, FRECUENCIAS ESPERADAS, RESIDUOS E INERCIA
# =====================================================================
# Mismas funciones que en el paso 3, aplicadas a tabla_b.

# --- 13a. Prueba chi-cuadrado ---
prueba_b <- chisq.test(tabla_b)
prueba_b

# RESULTADO: X-squared = 32.32, df = 10, p-value = 0.00035
# LECTURA: se rechaza la independencia; ideologia y voto estan
# asociados. gl = (6 - 1) x (3 - 1) = 10.

# --- 13b. Frecuencias esperadas ---
round(prueba_b$expected, 1)

# RESULTADO
#               Sheinbaum Galvez Maynez
#   Izquierda       126.2   19.1   16.6
#   Centro_izq       68.6   10.4    9.0
#   Centro          165.2   25.0   21.8
#   Centro_der       76.4   11.6   10.1
#   Derecha          92.7   14.1   12.2
#   No_se_ubica      77.9   11.8   10.3
#
# LECTURA
# - La mas baja es 9.0: ninguna es menor que 5. A diferencia de la
#   Parte A, no aparece la advertencia y el valor p es confiable.
# - No_se_ubica tiene 100 casos, asi que sus esperados son directamente
#   la distribucion del voto total: bajo independencia, cada grupo
#   votaria asi.

# --- 13c. Residuos de Pearson ---
round(prueba_b$residuals, 1)

# RESULTADO
#               Sheinbaum Galvez Maynez
#   Izquierda         1.6   -2.1   -2.1
#   Centro_izq        0.5   -1.4    0.0
#   Centro           -1.3    1.0    2.4
#   Centro_der       -0.3    1.6   -1.0
#   Derecha           0.1    1.1   -1.5
#   No_se_ubica      -0.6   -0.2    1.8
#
# LECTURA
# Solo tres celdas superan 2 en valor absoluto: la asociacion es debil.
# - Izquierda vota por Galvez y Maynez menos de lo esperado (-2.1) y
#   por Sheinbaum algo mas (+1.6).
# - Centro vota por Maynez mas de lo esperado (+2.4): la desviacion
#   mas grande de la tabla.
# - No_se_ubica se inclina algo hacia Maynez (+1.8).
# - Centro_der se inclina algo hacia Galvez (+1.6).
# - Derecha x Sheinbaum = +0.1: la derecha voto por Sheinbaum en la
#   misma proporcion que el total. Llama la atencion.
# - La columna de Sheinbaum es casi plana: su voto atraviesa todo el
#   espectro ideologico. Ademas, como su frecuencia esperada es grande,
#   sus residuos tienden a ser pequenos.

# --- 13d. Inercia total y V de Cramer ---
prueba_b$statistic / sum(tabla_b)                                   # inercia
sqrt((prueba_b$statistic / sum(tabla_b)) / (min(dim(tabla_b)) - 1))  # V

# RESULTADO: inercia = 0.0415; V = 0.14.
#
# LECTURA
# Asociacion significativa pero debil.
# Comparacion con la Parte A:
#                                   inercia   maximo   V de Cramer
#   Ideologia x disposicion (8x5)    0.1088      4        0.16
#   Ideologia x voto (6x3)           0.0415      2        0.14
# Comparando inercias, la primera parece 2.6 veces mas fuerte. Pero las
# tablas tienen maximos distintos. Con la V, la diferencia es pequena.
# REGLA: la inercia sirve para comparar la misma tabla en distintas
# muestras; para comparar tablas de distinto tamano, la V de Cramer.


# =====================================================================
# PASO 14. ANALISIS DE CORRESPONDENCIAS Y MAPA
# =====================================================================
# Mismas funciones que en los pasos 4 a 7.
res_b <- CA(as.data.frame.matrix(tabla_b), graph = FALSE)

# --- 14a. Inercia por dimension ---
round(res_b$eig, 4)

# RESULTADO: dim 1 = 70.1%; dim 2 = 29.9%; acumulado = 100%.
# LECTURA
# Con 6 filas y 3 columnas solo hay min(6, 3) - 1 = 2 dimensiones: el
# plano muestra toda la asociacion. 100% NO significa asociacion fuerte:
# el mapa reproduce exactamente una asociacion debil. A diferencia de
# la Parte A, la dimension 2 pesa (30%) y conviene interpretarla.

# --- 14b. Estadisticos por categoria ---
summary(res_b, nb.dec = 3, ncp = 2)

# RESULTADO (extracto)
#   Filas         Dim.1   ctr   cos2  |  Dim.2   ctr   cos2
#   Izquierda    -0.257  47.1  0.940  | -0.065   7.0  0.060
#   Centro_izq   -0.089   3.1  0.325  | -0.128  15.0  0.675
#   Centro        0.198  36.5  0.990  | -0.020   0.9  0.010
#   Centro_der    0.005   0.0  0.001  |  0.190  36.7  0.999
#   Derecha      -0.071   2.7  0.180  |  0.152  28.5  0.820
#   No_se_ubica   0.155  10.6  0.675  | -0.108  12.0  0.325
#   Columnas
#   Sheinbaum    -0.087  20.3  0.965  | -0.017   1.7  0.035
#   Galvez        0.213  18.3  0.381  |  0.271  69.8  0.619
#   Maynez        0.417  61.3  0.835  | -0.185  28.4  0.165
#
# LECTURA
# - Calidad: con 2 dimensiones, todas las categorias estan perfectamente
#   representadas en el plano (sus cos2 suman 1). Lo que cambia es en
#   que dimension esta cada una. Centro_der esta casi toda en la
#   dimension 2 (cos2 = 0.999): no se interpreta en la dimension 1.
# - Dimension 1 (70%): voto oficialista de izquierda frente a voto por
#   Maynez. La construyen Izquierda (47%) y Centro (37%); Maynez (61%).
#   Polo negativo: Izquierda y Sheinbaum.
#   Polo positivo: Centro, No_se_ubica y Maynez.
#   Derecha queda del lado de Izquierda: en este eje no se distingue.
# - Dimension 2 (30%): Galvez frente a Maynez. La construyen Centro_der
#   (37%) y Derecha (28%); Galvez (70%). Es el unico eje con contenido
#   de derecha y sirve para distinguir a los dos opositores.
# - Inercia por categoria (Iner*1000 / 41.5): Maynez aporta el 51% de
#   toda la asociacion; Sheinbaum solo el 15%. Sheinbaum queda cerca
#   del origen no porque su voto no tenga perfil, sino porque reune el
#   78% de los casos: su perfil casi coincide con el promedio. Las
#   categorias con mucha masa tienden al origen.

# --- 14c. Mapa ---
plot(res_b, title = "Ideologia y voto presidencial, CNEP Mexico 2024")

# LECTURA DEL MAPA
# - Fidelidad: 2 dimensiones = 100% de la inercia (70.1% + 29.9%).
#   El mapa es exacto, pero la asociacion es debil (V = 0.14).
# - Los tres candidatos forman un triangulo; Sheinbaum, cerca del
#   origen por su masa.
# - Direcciones desde el origen (atracciones, verificables en 13c):
#     Izquierda -> Sheinbaum (y opuesta a Maynez, residuo -2.1)
#     Centro y No_se_ubica -> Maynez (Centro x Maynez = +2.4)
#     Centro_der y Derecha -> Galvez (Centro_der x Galvez = +1.6)
# - Cercanias entre filas: Centro y No_se_ubica votan parecido (el
#   punto medio de la escala como refugio de quienes no se ubican);
#   Centro_der y Derecha tambien.
# - Hallazgo principal: la ideologia no forma una linea izquierda-
#   derecha. Derecha regresa al lado de Izquierda en la dimension 1:
#   voto por Sheinbaum en la misma proporcion que el total. Puede
#   deberse en parte a la inversion de la escala detectada en la
#   sesion de MDS.
# - No interpretar la distancia entre un punto fila y uno columna.
#
# "La autoubicacion ideologica se asocia debilmente con el voto
#  presidencial (chi2 = 32.3, gl = 10, p < 0.001; inercia = 0.041;
#  V = 0.14). El 70% de la asociacion opone a la izquierda, concentrada
#  en Sheinbaum, al centro y a quienes no se ubican, que se inclinan
#  hacia Maynez. El 30% restante distingue el voto por Galvez, de
#  perfil de centro derecha."
#
# Cautelas: muestra sin ponderar que sobrerrepresenta el voto por
# Sheinbaum; Galvez (92) y Maynez (80) tienen pocos casos, asi que sus
# perfiles son menos estables.
# =====================================================================
# 1. Borrar todos los objetos creados (tabla, prueba, res_a, cnep...)
rm(list = ls())

# 2. Cerrar todas las graficas abiertas
graphics.off()

# 3. Limpiar la consola (equivale a Ctrl + L)
cat("\014")
