# =====================================================================
# Analisis multivariado y metodos de clasificacion de datos con R
# Tema 4 - Sesion 11: Escalamiento multidimensional (practica)
# Datos: CNEP Mexico 2024, version armonizada (Mexico2024_Fin.dta)
# Centro de Estudios Sociologicos, El Colegio de Mexico
# =====================================================================
#
# Estructura del script
#   Parte A. Preparacion de los datos                  (pasos 0 a 6)
#   Parte B. MDS de los siete partidos                 (pasos 7 a 16)
#   Parte C. Escalamiento de Aldrich-McKelvey          (pasos 17 a 21)
#   Parte D. Ejercicio: MDS de los temas de politica   (paso 22)
#
# Supuestos
#   - El script se corre dentro de un proyecto de RStudio que tiene una
#     carpeta "datos" con el archivo Mexico2024_Fin.dta.
#   - Los analisis son sin ponderar: las variables de pesos de la base
#     armonizada (L_WtWithin, L_WtCombined) valen -999 en todos los casos.
#   - La muestra sobrerrepresenta el voto por Sheinbaum (77% del voto
#     reportado frente a cerca de 60% del resultado oficial).
# =====================================================================


# =====================================================================
# PARTE A. PREPARACION DE LOS DATOS
# =====================================================================

# --- Paso 0. Paquetes ---------------------------------------------------
# La linea de instalacion se corre solo la primera vez
install.packages(c("haven", "dplyr", "smacof", "basicspace", "ggplot2", "ggrepel"))

library(haven)       # leer archivos de Stata (.dta)
library(dplyr)       # manipulacion de datos
library(smacof)      # MDS por SMACOF: stress, Shepard, permutacion, bootstrap
library(basicspace)  # escalamiento de Aldrich-McKelvey
library(ggplot2)     # graficas
library(ggrepel)     # etiquetas que no se enciman en las graficas


# --- Paso 1. Cargar la base -----------------------------------------------
# file.path() arma la ruta con el separador correcto en Mac y en PC.

cnep <- read_dta("/Users/isaaccisneros/Desktop/ISAAC/CURSO/Cursos 2026/DOCUMENTOS CURSO/R/TEMA 4 CONGLOMERADOS/WVS_Wave_7_Mexico_Stata_v5.1.dta")

cnep <- read_dta("/Users/isaaccisneros/Desktop/ISAAC/CURSO/Cursos 2026/DOCUMENTOS CURSO/R/TEMA 5 ESCALAMIENTO/Mexico2024_Fin.dta")

dim(cnep)   # 1205 casos y 347 variables


# --- Paso 2. Ubicaciones izquierda-derecha (P82 y P83 del cuestionario) ---
# La base armonizada usa codigos genericos; el cuestionario indica a que
# partido corresponde cada uno: Gov1 = Morena, Gov2 = PVEM, Gov3 = PT,
# Opp1 = PAN, Opp2 = PRI, Opp3 = PRD, Opp4 = MC.
# zap_labels() quita las etiquetas de Stata y deja solo los numeros
# (incluida la etiqueta erronea "5.5" del valor 6).
lr <- cnep %>%
  select(morena = C_LRGov1, pan = C_LROpp1, pri = C_LROpp2,
         prd = C_LROpp3, mc = C_LROpp4, pvem = C_LRGov2,
         pt = C_LRGov3, self = C_LRSelf) %>%
  zap_labels()


# --- Paso 3. Codigos especiales a NA --------------------------------------
# La escala valida va de 1 (izquierda) a 10 (derecha).
# 998 (no contesto), 999 (no sabe) y -999 pasan a NA.
lr <- lr %>%
  mutate(across(everything(), ~ ifelse(.x >= 1 & .x <= 10, .x, NA)))
summary(lr)   # todos los minimos deben ser 1 y todos los maximos 10


# --- Paso 4. Datos faltantes ----------------------------------------------
colSums(is.na(lr))        # NA por variable; la autoubicacion tiene mas (240)
# personas SIN respuesta valida en cada variable
# morena 156 (12.9%), pri 181, pan 182, prd 194, pvem 196, pt 197,
# mc 202 (16.8%), self 240 (19.9%), sobre 1205 entrevistados.
# - La autoubicacion es la que MAS faltantes tiene: 240 personas no se
#   ubicaron (194 "no se" y 46 sin respuesta). Es mas facil ubicar a los
#   partidos que ubicarse a uno mismo.
# - Morena, el partido mas visible, tiene menos faltantes; los partidos
#   pequenos (PVEM, PT, MC), mas.
# - Cada tecnica pierde casos segun lo que necesita: el MDS de partidos
#   no usa la autoubicacion (955 casos); Aldrich-McKelvey si (819).
sum(complete.cases(lr))   # 867 casos con las 8 ubicaciones completas


# --- Paso 5. Primera mirada: medias y distribucion -------------------------
round(colMeans(lr, na.rm = TRUE), 2)   # todas entre 4.3 y 5.7
table(lr$morena)                        # bimodal: muchos 1 y muchos 10
# Las medias casi no distinguen a los partidos porque los extremos se
# compensan. Las medias ocultan la estructura; hay que mirar relaciones.


# --- Paso 6. Morena a la derecha del PAN? ----------------------------------
mp <- na.omit(lr[, c("morena", "pan")])   # casos que ubican a ambos
lr[, c("morena", "pan")] # elige solo las columnas de Morena y PAN
# (antes de la coma, filas: vacio = todas; despues de la coma, columnas).
# na.omit() elimina a quien no ubico a alguno de los dos partidos.
# Se eligen primero las dos columnas y despues se quitan los NA; si se
# aplicara na.omit() a todo lr, se perderian casos sin motivo (867 en
# lugar de 1014).

nrow(mp)                                   # 1014

mean(mp$morena > mp$pan)                   # 0.46: Morena a la derecha
mean(mp$morena < mp$pan)                   # 0.43: Morena a la izquierda
# mp$morena > mp$pan compara, persona por persona, donde ubico a Morena y
# donde al PAN. Para cada persona da TRUE (puso a Morena mas a la derecha)
# o FALSE (no lo hizo). El simbolo $ toma una columna de la tabla mp.
# mean() trata cada TRUE como 1 y cada FALSE como 0; por eso el promedio
# es la proporcion de personas que cumplen la condicion.

# ¿¿ el electorado no comparte el significado de
# izquierda y derecha, o usa la escala de forma distinta (incluso invertida)?

# =====================================================================
# PARTE B. MDS DE LOS SIETE PARTIDOS
# =====================================================================
# Objetos: los 7 partidos. Proximidad: que tan parecido es el patron con
# que los entrevistados ubican a dos partidos (su correlacion).
# Advertencia: con 7 objetos, la regla 4k + 1 solo respalda plenamente
# una dimension; la segunda se explora con cautela.

# --- Paso 7. Casos completos y matriz de correlaciones ---------------------
# Usamos los casos que ubican a los 7 partidos, para que todas las
# correlaciones se calculen sobre las mismas personas.
partidos <- lr %>%
  select(morena, pt, pvem, mc, prd, pan, pri) %>%
  na.omit()
nrow(partidos)                  # 955 casos

r_part <- cor(partidos)         # correlaciones de Pearson entre partidos
round(r_part, 2)
# Morena correlaciona negativamente con PAN, PRI y PRD; quien
# ubica a Morena mas a la derecha tiende a ubicar a la oposicion mas
# a la izquierda, y viceversa.


# --- Paso 8. De correlaciones a disimilitudes ------------------------------
# sim2diss(method = "corr") calcula d = sqrt(1 - r).
# La formula de la presentacion, d = sqrt(2(1 - r)), es la misma
# multiplicada por sqrt(2): el mapa es identico salvo la escala.

# El MDS necesita disimilitudes (0 = identicos, valores altos = distintos),
# pero las correlaciones van al reves (1 = identicos). sim2diss() las
# convierte con d = sqrt(1 - r):
#   r = 1  -> d = 0     (se ubican igual)
#   r = 0  -> d = 1     (sin relacion)
#   r = -1 -> d = 1.41  (se ubican de forma opuesta)
# Ejemplos: PAN-PRI r = 0.81 -> d = 0.44; Morena-PAN r = -0.54 -> d = 1.24.
# La formula de la presentacion, sqrt(2(1 - r)), es la misma multiplicada
# por sqrt(2): el mapa es identico salvo la escala.
d_part <- sim2diss(r_part, method = "corr")
round(d_part, 2)   # 0 = identicos; valores altos = muy distintos

# --- Paso 9. MDS clasico (Torgerson) ----------------------------------------
# Este es el escalamiento: a partir de las disimilitudes, cmdscale()
# calcula las coordenadas de cada partido en el mapa. Internamente eleva
# las disimilitudes al cuadrado, hace el doble centrado y la
# descomposicion en valores propios (diapositiva 8).
#   d_part    = tabla de disimilitudes (paso 8)
#   k = 2     = numero de dimensiones del mapa
#   eig = TRUE = guarda los valores propios para evaluar el ajuste
# El resultado mds_cl contiene: $points (coordenadas), $eig (valores
# propios) y $GOF (indice de ajuste).
# cmdscale() hace el doble centrado y la descomposicion en valores propios.
# eig = TRUE guarda los valores propios para evaluar el ajuste.

mds_cl <- cmdscale(d_part, k = 2, eig = TRUE)

# Proporcion de la estructura que capta cada dimension
round(mds_cl$eig / sum(mds_cl$eig[mds_cl$eig > 0]), 3)
# La dimension 1 capta 55%; la 2, 15%.
# Si aparecieran valores propios negativos, la matriz no seria del todo
# euclidiana. Aqui no hay: los dos indices de GOF coinciden (0.70).

mds_cl$GOF          # indices de ajuste (con valores absolutos y con positivos)
# GOF (goodness of fit) = proporcion de la estructura que capta el mapa
# con las k dimensiones pedidas (0 a 1). a mayor valor, mejor
# ajuste (al reves que el stress, donde menor es mejor).
# Aqui, 70% (55% de la dimension 1 + 15% de la dimension 2).
# Son dos versiones: la primera divide entre la suma de todos los
# valores propios en valor absoluto; la segunda, solo entre los positivos.
# Si son iguales, no hay valores propios negativos: la matriz es
# euclidiana. Si la primera es menor, la matriz no es del todo euclidiana
# y conviene el MDS no metrico.

round(mds_cl$points, 2)   # coordenadas de cada partido
# Coordenadas de cada partido en el plano: columna 1 = eje horizontal
# (dimension 1), columna 2 = eje vertical (dimension 2).
# El origen (0, 0) es el centro del conjunto de partidos.
# Se interpretan las distancias entre puntos, no las coordenadas sueltas;
# el signo es arbitrario (el mapa podria salir volteado).
# Las distancias del mapa aproximan las disimilitudes: Morena-PAN 1.24 en
# los datos y 1.20 en el mapa; PAN-PRI 0.44 en los datos y 0.03 en el
# mapa. Lo que distingue al PAN del PRI esta en el 30% no captado.


# --- Paso 10. Mapa del MDS clasico -----------------------------------------
# Convertimos las coordenadas en un data frame para graficar con ggplot2.
coord_cl <- data.frame(partido = rownames(mds_cl$points),
                       D1 = mds_cl$points[, 1],
                       D2 = mds_cl$points[, 2])

ggplot(coord_cl, aes(x = D1, y = D2, label = partido)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(size = 3, colour = "darkred") +
  geom_text_repel(size = 4) +
  coord_equal() +   # misma escala en ambos ejes: las distancias no se deforman
  labs(title = "MDS clasico de los partidos",
       subtitle = "Disimilitudes a partir de las ubicaciones izquierda-derecha, CNEP 2024",
       x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()
# Lectura: la dimension 1 separa a Morena, PT y PVEM de PAN, PRI y PRD;
# MC queda en medio. El mapa recupera los bloques de coalicion aunque
# las medias del paso 5 casi no distinguian a los partidos.
# Recordatorio: el signo de los ejes es arbitrario.

# Interpretacion:
# 1. Bloques. La dimension 1 (55%) separa a Morena, PT y PVEM de PRD, PAN
#    y PRI; MC queda en el centro. Son las coaliciones de 2024, y el MDS
#    las recupera solo con la forma en que la gente ubica a los partidos.
# 2. Forma de cada bloque. La oposicion es compacta (PAN, PRI y PRD casi
#    encimados); el bloque de gobierno esta mas extendido: Morena se
#    distingue de sus aliados, que quedan mas cerca del centro.
# 3. Dimension 2 (15%), con cautela. Separa a Morena de PT, PVEM y MC.
#    Con 7 partidos, la regla 4k + 1 no respalda una segunda dimension.
# 4. Conclusion. Las medias del paso 5 casi no distinguian a los partidos;
#    el mapa muestra una estructura clara de bloques. Si la dimension 1 es
#    gobierno-oposicion o izquierda-derecha se prueba en el paso 15.
# 5. Advertencias. El signo es arbitrario (el mapa podria salir volteado).
#    PAN y PRI encimados no son identicos (disimilitud 0.44): lo que los
#    distingue esta en el 30% que el mapa no capta.


# --- Paso 11. MDS no metrico (ordinal) con smacof -------------------------
#type = "ordinal" solo preserva el orden de las disimilitudes.
#init = "torgerson" (valor por defecto) arranca desde la solucion clasica.

# set.seed() fija el generador de numeros aleatorios para que los
# resultados sean iguales cada vez que se corre el script. Aqui no es
# indispensable (el arranque por defecto es la solucion clasica, sin
# azar), pero si lo es en los pasos 12, 14 y 16.
set.seed(2024)

# mds() del paquete smacof estima el escalamiento:
#   d_part           = misma tabla de disimilitudes que el MDS clasico
#   ndim = 2         = mapa en dos dimensiones
#   type = "ordinal" = solo importa el orden de las disimilitudes (no
#                      metrico); con "interval" o "ratio" seria metrico
# El algoritmo arranca del mapa clasico y mueve los puntos hasta que el
# orden de las distancias coincida con el de las disimilitudes.
mds_ord <- mds(d_part, ndim = 2, type = "ordinal")
mds_ord               # stress-1 = 0.004

# Segun Kruskal seria "excelente", pero con 7 objetos y transformacion
# ordinal un stress casi nulo es facil de obtener: el algoritmo puede
# acomodar casi cualquier orden. Los pasos 12 a 14 revisan si el
# resultado es confiable.

# Comparamos con la version metrica de intervalo
# MDS metrico de intervalo: ademas del orden, debe respetar las
# diferencias entre disimilitudes. Es mas exigente que el ordinal.
mds_int <- mds(d_part, ndim = 2, type = "interval")

# Comparar los stress: c() junta los dos valores en un vector y los
# nombres (ordinal =, intervalo =) hacen que el resultado se lea claro.
c(ordinal = mds_ord$stress, intervalo = mds_int$stress)   # 0.004 y 0.068
# El ordinal siempre tiene stress menor o igual: tiene mas libertad.
# Con 7 objetos, el 0.004 del ordinal es casi automatico; el 0.068 del
# de intervalo (entre "bueno" y "aceptable" segun Kruskal) es mas
# informativo. Si ambos mapas muestran los mismos bloques, la conclusion
# no depende del tipo de MDS elegido.

# Rangos de Kruskal (1964) para el stress-1 (orientativos):
#   mayor a 0.20   pobre
#   0.10 a 0.20    entre aceptable y pobre
#   0.05 a 0.10    entre bueno y aceptable
#   0.025 a 0.05   bueno
#   menor a 0.025  excelente
#   0              perfecto
# Advertencia: dependen del numero de objetos, de dimensiones y del tipo
# de MDS. Con pocos objetos y MDS ordinal, un stress "excelente" es casi
# automatico. Hay que completar la evaluacion con la prueba de
# permutacion (paso 14).

# --- Paso 12. Cuantas dimensiones? Y un minimo local ------------------------
# Estimamos soluciones en 1, 2 y 3 dimensiones y comparamos el stress.
stress_dim <- sapply(1:3, function(k)
  mds(d_part, ndim = k, type = "ordinal")$stress)
names(stress_dim) <- paste0(1:3, "D")
round(stress_dim, 3)   # 1D = 0.001, 2D = 0.004, 3D = 0.004
plot(1:3, stress_dim, type = "b", pch = 19,
     xlab = "Numero de dimensiones", ylab = "Stress-1",
     main = "Stress segun numero de dimensiones")
# Algo anda mal: el stress deberia bajar al agregar dimensiones, pero la
# solucion en 2D tiene mas stress que la de 1D. Es un minimo local:
# el algoritmo se quedo en una solucion que no es la mejor.

# Probamos 50 arranques aleatorios y nos quedamos con el menor stress
# sapply(1:50, ...) repite el MDS 50 veces y junta los resultados;
# init = "random" hace que cada vez arranque desde un mapa al azar
# (en lugar del mapa clasico); $stress guarda el stress de cada intento.
set.seed(2024)
stress_aleat <- sapply(1:50, function(i)
  mds(d_part, ndim = 2, type = "ordinal", init = "random")$stress)
round(min(stress_aleat), 4)   # 0.0013: mejor que el arranque de Torgerson

# Interpretacion: el MDS del paso 11 (stress 0.004) era un minimo local,
# el algoritmo se quedo en una solucion que no era la mejor. Leccion: en
# el MDS no metrico hay que probar varios arranques.
# Aun con el mejor arranque, el 2D (0.0013) apenas mejora al 1D (0.001):
# la segunda dimension casi no aporta. Con 7 partidos basta una dimension.
# Conclusion: una sola dimension reproduce casi a la perfeccion el orden
# de las disimilitudes. Ademas, con 7 objetos la regla 4k + 1 solo
# respalda una dimension. Veamos ese orden:

# MDS ordinal en UNA dimension: los partidos quedan sobre una sola linea.
mds_1d <- mds(d_part, ndim = 1, type = "ordinal")

# $conf = coordenadas; [, 1] = la unica columna; sort() ordena de menor
# a mayor; round(..., 2) redondea a dos decimales.
round(sort(mds_1d$conf[, 1]), 2)
# Morena, PT, PVEM, MC, PRD, PAN, PRI: el eje gobierno-oposicion.
# Seguimos con la solucion en 2D solo para ilustrar las herramientas
# de diagnostico e interpretacion.
# morena -1.05, pt -0.65, pvem -0.34, mc 0.06, prd 0.46, pan 0.76, pri 0.76
# Interpretacion:
# 1. El orden reproduce el eje gobierno-oposicion: Morena, PT y PVEM;
#    MC en el centro; PRD, PAN y PRI.
# 2. Morena queda en el extremo, separado de sus aliados; PAN y PRI
#    quedan empatados: el electorado no los distingue.
# 3. MC (0.06) no se asocia con ninguno de los dos bloques.
# 4. Es el mismo orden que encuentra Aldrich-McKelvey (parte C): dos
#    metodos distintos llegan al mismo eje.
# El signo es arbitrario: el orden podria salir invertido.


# --- Paso 13. Diagnostico: Shepard y stress por punto -----------------------
# Diagrama de Shepard: disimilitudes (eje x) contra distancias del mapa
# (eje y). Hay 21 puntos, uno por cada par de partidos.
plot(mds_ord, plot.type = "Shepard",
     main = "Diagrama de Shepard (MDS ordinal)")

# Como leerlo:
# - La linea escalonada es normal en el MDS ordinal: es la regresion
#   monotona (solo sube o se queda plana). Los tramos planos son pares
#   cuyo orden estaba invertido y se promediaron (por ejemplo, cuatro
#   pares quedan en 1.20).
# - El ajuste se evalua por la distancia de los puntos a la linea, no por
#   su forma. Aqui los puntos caen sobre la linea: stress de 0.004.
# - La linea es curva: los pares de la oposicion (PAN-PRI 0.44, PRD-PRI
#   0.57, PRD-PAN 0.59) quedan casi encimados en el mapa (0.10 a 0.18).
#   El MDS ordinal comprime a la oposicion; el de intervalo no puede
#   hacerlo (linea recta) y por eso su stress es mayor (0.068).
# - Con 7 partidos y 21 pares, el ajuste casi perfecto era esperable.

# Stress por punto: porcentaje del stress que aporta cada partido.
# Un partido con un valor muy alto esta mal representado en el mapa.
round(sort(mds_ord$spp, decreasing = TRUE), 1)   
# el PT aporta mas (29%)
# Interpretacion:
# - Si todos contribuyeran igual, cada uno tendria 14.3% (100 / 7).
# - El PT es el peor representado (29%, el doble del promedio): su
#   posicion en el mapa es la menos confiable. Posible lectura: el
#   electorado lo ubica de forma ambigua.
# - Morena (6.5%) y PRD (0.5%) son los mejor representados.
# - Es una proporcion, no una magnitud: como el stress total es minimo
#   (0.004), aun el PT esta bien representado en terminos absolutos.
#   Esta regla importa mas cuando el stress total es alto (ejercicio
#   de los temas: 0.098).

# Bubbleplot: el mapa del MDS ordinal con un circulo en cada partido
# cuyo tamano es proporcional a su stress por punto. Muestra donde esta
# el error, no solo cuanto.
plot(mds_ord, plot.type = "bubbleplot",
     main = "Configuracion con stress por punto")

# Lectura: el PT tiene el circulo mas grande (29% del error): su posicion
# es la menos confiable. PRD (0.5%) y Morena (6.5%), circulos pequenos:
# bien representados. PAN y PRI se enciman porque estan casi en el mismo
# lugar.
# Limites: el circulo dice cuanto error aporta cada partido, no hacia
# donde esta mal ubicado. Y son proporciones: con stress total de 0.004,
# aun el PT tiene poco error en terminos absolutos.

# --- Paso 14. Prueba de permutacion ----------------------------------------
# Permuta los datos originales, reestima el MDS y compara el stress
# observado con el que se obtiene por azar. Tarda unos segundos.

set.seed(2024)

# Revuelve al azar las respuestas de cada partido entre personas (rompe la
# relacion entre partidos), recalcula el MDS y guarda el stress. Lo hace
# 500 veces y cuenta cuantas veces el azar da un stress igual o menor
# que el observado.
perm <- permtest(mds_ord, data = partidos, method.dat = "pearson",
                 nrep = 500, verbose = FALSE)
perm   # p = 0.012: la estructura no es producto del azar
# Interpretacion: solo 1.2% de las permutaciones (unas 6 de 500) dio un
# stress tan bajo. Como p < 0.05, la estructura del mapa no es producto
# del azar. Es la forma correcta de evaluar el stress, sobre todo con
# pocos objetos, donde los umbrales de Kruskal no sirven.
# La prueba dice que HAY estructura, no CUAL es: eso lo dice el paso 15.

#El valor p de 0.012 significa que solo en el 1.2% de las permutaciones, unas 6 de 500, 
#los datos sin estructura produjeron un mapa tan bueno como el nuestro. 
#Como es menor que 0.05, se concluye que la estructura del mapa no es producto del azar: 
#las relaciones entre las ubicaciones de los partidos son reales.

plot(perm, main = "Prueba de permutacion")
# Distribucion de los 500 stress aleatorios y linea con el observado: si
# la linea queda en el extremo izquierdo, la estructura es real.
# Como leerlo:
# - Eje x: valores de stress; eje y: proporcion acumulada de las 500
#   permutaciones con stress igual o menor.
# - La curva: los stress aleatorios van casi todos de 0.05 a 0.15
#   (alrededor de 0.10 en el centro).
# - Linea vertical: stress observado (0.004), en el extremo izquierdo.
# - Linea punteada horizontal: nivel de 0.05.
# - El valor p es la altura de la curva donde la cruza la linea vertical
#   (0.012): esta por debajo de 0.05, el resultado es significativo.
# Mensaje: al azar, un MDS ordinal de 7 partidos ya da stress cercano a
# 0.10 ("aceptable" segun Kruskal). Lo que prueba que el mapa capta algo
# real no es que el stress sea bajo, sino que es mucho mas bajo que el
# que produce el azar.

# - Eje x: valores de stress. Termina en 0.20 solo porque el stress
#   aleatorio mas alto fue 0.198; no es una referencia.
# - Eje y: proporcion acumulada de las 500 permutaciones con stress
#   igual o menor.
# - La mediana de los stress aleatorios es 0.10: donde la curva llega a
#   0.5 en el eje y. Es el stress "tipico" que produce el azar.
# - Linea vertical: stress observado (0.004), en el extremo izquierdo.
# - El valor p es la altura de la curva donde la cruza la linea vertical
#   (0.012): por debajo de 0.05, el resultado es significativo.
# Dato: el stress aleatorio minimo fue 0.003, menor que el
# observado. Con pocos objetos el azar puede dar stress muy bajos; por
# eso no bastan los umbrales de Kruskal.

# --- Paso 15. Interpretar con variables externas (property fitting) -------
# Regresamos sobre las coordenadas dos atributos de los partidos que
# no se usaron para construir las disimilitudes:
#   gobierno = 1 si el partido formo parte de la coalicion de Sheinbaum
#   ubic_media = ubicacion izquierda-derecha promedio (paso 5)
# Para nombrar las dimensiones con evidencia y no por intuicion
# (diapositiva 14). Aqui se arma la tabla de atributos de los partidos;
# la regresion sobre el mapa se hace con biplotmds().
# Dos candidatos para interpretar la dimension 1:
#   gobierno   = 1 si el partido formo parte de la coalicion de Sheinbaum
#   ubic_media = ubicacion izquierda-derecha promedio
# El orden de los valores de "gobierno" debe coincidir con el de las
# columnas de partidos: morena, pt, pvem, mc, prd, pan, pri.
externas <- data.frame(
  gobierno   = c(1, 1, 1, 0, 0, 0, 0),   # orden: morena pt pvem mc prd pan pri
  ubic_media = colMeans(partidos)
)
rownames(externas) <- colnames(partidos)
externas
# Las medias usan los 955 casos completos; por eso difieren un poco de las
# del paso 5. "gobierno" es totalmente externa (dato institucional);
# "ubic_media" viene de las mismas preguntas, aunque mide el nivel
# promedio y no las correlaciones.

# biplotmds() hace una regresion para cada variable externa con las
# coordenadas del mapa como predictores: variable = b0 + b1*D1 + b2*D2
# (7 observaciones, una por partido). Los coeficientes dicen en que
# direccion del mapa crece la variable; el R2, que tan bien la explica.
ajuste <- biplotmds(mds_ord, extvar = externas)
ajuste$R2vec   # R2: gobierno = 0.87, ubic_media = 0.61

# Mapa con una flecha por variable: apunta hacia donde la variable crece.
# vecscale ajusta el largo de las flechas; label.conf pone el nombre de
# cada partido arriba de su punto (pos = 3).
plot(ajuste, main = "Configuracion con variables externas",
     vecscale = 0.8, label.conf = list(label = TRUE, pos = 3))
# Lectura: "gobierno" se ajusta mejor que la ubicacion media y su vector
# apunta a lo largo de la dimension 1: esa dimension es, ante todo,
# gobierno frente a oposicion. Con solo 7 puntos, los R2 son inestables;
# se leen como indicio, no como prueba.

# Interpretacion:
# - "gobierno" (coef. D1 = -1.45, D2 = 0.13): flecha casi horizontal hacia
#   Morena, PT y PVEM. La dimension 1 es gobierno frente a oposicion.
# - "ubic_media" (coef. D1 = 0.95, D2 = 1.56): flecha diagonal, mas
#   vertical que horizontal. La ubicacion ideologica promedio no organiza
#   el eje principal.
# - Conclusion: el electorado distingue a los partidos por bloque de
#   coalicion mas que por su ubicacion izquierda-derecha promedio.
# - Cautela: con 7 puntos los R2 son inestables, y "gobierno" es 0/1.
#   Son un indicio fuerte, no una prueba.

# --- Paso 16. Estabilidad: bootstrap ----------------------------------------
# Remuestrea a los entrevistados, recalcula correlaciones y MDS, y dibuja
# regiones de confianza para cada partido (Jacoby y Armstrong, 2014).
# Remuestrea a los 955 entrevistados con reemplazo, recalcula
# correlaciones, disimilitudes y MDS, alinea el mapa nuevo con el original
# y lo repite 200 veces. Pregunta: si hubieramos entrevistado a otras
# personas parecidas, cuanto se moveria cada partido?
# (Jacoby y Armstrong, 2014)
#   data = partidos         datos originales, para remuestrear personas
#   method.dat = "pearson"  como calcular las proximidades en cada vuelta
#   nrep = 200              numero de repeticiones
set.seed(2024)

boot <- bootmds(mds_ord, data = partidos, method.dat = "pearson",
                nrep = 200, verbose = FALSE)
boot   # coeficiente de estabilidad = 0.98
# stress promedio 0.004 (0.0012 a 0.0075); estabilidad = 0.98
# Coeficiente de estabilidad: de 0 a 1; cerca de 1, el mapa casi no
# cambia de una muestra a otra.

plot(boot, main = "Regiones de confianza (bootstrap)")
# Elipses que no se traslapan = posiciones distinguibles con estos datos.
# Cada elipse es la region donde cayo el partido en el 95% de las
# repeticiones.
# - Morena, PT y PVEM: elipses pequenas y separadas, posiciones claras.
# - PAN, PRI y PRD: elipses traslapadas, no distinguibles entre si; la
#   oposicion se percibe como un solo bloque.
# - MC: elipse alargada en vertical; su posicion en la dimension 2 es
#   incierta.
# Conclusion: la distancia entre bloques es solida; las diferencias
# dentro de la oposicion no deben interpretarse.

# =====================================================================
# PARTE C. ESCALAMIENTO DE ALDRICH-MCKELVEY
# =====================================================================
# Idea: cada entrevistado usa la escala 1-10 con su propio desplazamiento
# y estiramiento, e incluso puede invertirla. El metodo estima esa
# distorsion para cada persona y recupera:
#   - una posicion comun para cada partido, y
#   - la autoubicacion de cada persona en esa misma metrica (punto ideal).
# Estima una sola dimension.

# --- Paso 17. Preparar la matriz ---------------------------------------------
# aldmck() pide una matriz numerica: las 7 ubicaciones de partidos y, al
# final, la autoubicacion (aqui si entra, para calcular puntos ideales).
# El orden importa: en el paso 18, respondent = 8 indica que la columna 8
# es la autoubicacion, y polarity = 1 que la columna 1 (Morena) va del
# lado negativo.
am_datos <- as.matrix(lr[, c("morena", "pan", "pri", "prd", "mc",
                             "pvem", "pt", "self")])

# aldmck() no trabaja con NA: necesita un codigo de faltante. Los NA se
# marcan con 999 (en el paso 18: missing = 999). No deshace el paso 3:
# ahi se limpiaron 998, 999 y -999; aqui se usa un solo codigo.
am_datos[is.na(am_datos)] <- 999

dim(am_datos)
# No hace falta filtrar: la funcion usa sola a los 819 que ubican a los
# 7 partidos y a si mismos sin dar la misma calificacion a todos.


# --- Paso 18. Estimar el modelo ----------------------------------------------
# respondent = 8   la columna 8 es la autoubicacion
# missing = 999    codigo de faltante
# polarity = 1     la columna 1 (Morena) se fija del lado negativo.
#                  Es solo una convencion de signo: el metodo no puede
#                  saber que lado es "izquierda".
am <- aldmck(am_datos, respondent = 8, missing = 999,
             polarity = 1, verbose = FALSE)
summary(am)
# Interpretacion:
# - Stimuli: 7 partidos. Respondents Scaled: 819 (867 con todas las
#   ubicaciones, menos 48 que calificaron igual a todos los partidos).
# - Pesos: 378 positivos (46%) y 441 negativos (54%). Un peso negativo
#   significa que la persona usa la escala al reves respecto de la
#   dimension estimada (pone a Morena hacia la derecha). La mayoria la
#   invierte; coherente con el 46% del paso 6. Es una inversion relativa:
#   el metodo no sabe que lado es la "izquierda verdadera".
# - Reduction of normalized variance: 0.21. Al corregir el uso de la
#   escala, el desacuerdo sobre donde estan los partidos baja 21%. El
#   resto es desacuerdo real o ruido. Ajuste modesto.
# - Posiciones (unidades arbitrarias; signo fijado con polarity = 1):
#   morena -0.73, pt -0.22, pvem -0.12, mc -0.03, prd 0.27, pan 0.39,
#   pri 0.44.
#   El orden coincide con el MDS en 1D (paso 12). Morena queda solo en
#   su extremo, lejos incluso de sus aliados; PT, PVEM y MC cerca del
#   centro; la oposicion compacta, con el PRI en el extremo.
# - Frente a las medias crudas (todas entre 4.3 y 5.7), la correccion
#   separa a Morena de la oposicion.


# Que corrige Aldrich-McKelvey: las RESPUESTAS, no a las personas.
# Cada persona usa la escala 1-10 a su manera: unas usan solo el centro,
# otras toda la escala, otras la desplazan y otras la invierten. Con las
# ubicaciones que cada persona da a los 7 partidos, el metodo estima:
#   - su centro (desplazamiento)
#   - su amplitud (estiramiento), y si la invierte (peso negativo)
# y traduce todas sus respuestas (partidos y autoubicacion) a una escala
# comun.
# Ejemplo: quien ubica Morena 9, MC 5.5, PAN 2 invierte la escala. Si se
# ubica en 8, en crudo parece de derecha, pero corregido queda en -0.71,
# cerca de Morena.
# No decide quien tiene razon (el signo es una convencion) ni corrige
# opiniones: solo el uso lineal de la escala. Lo demas queda como
# desacuerdo real o ruido.

# --- Paso 19. Posiciones de los partidos -----------------------------------
# am$stimuli = posiciones de los 7 partidos en la escala comun (las mismas
# del summary). sort() las ordena de menor a mayor; se guardan en pos_am
# porque se usan en la tabla de comparacion y en la grafica.
pos_am <- sort(am$stimuli)
round(pos_am, 2)
# morena -0.73, pt -0.22, pvem -0.12, mc -0.03, prd 0.27, pan 0.39, pri 0.44

# Comparacion con las medias crudas del paso 5
# Tabla para comparar medias crudas y posiciones de Aldrich-McKelvey.
# names(pos_am) da los partidos en el orden de A-M; con eso se toman las
# columnas de lr en el mismo orden, para que cada fila sea el mismo
# partido en ambas columnas.
comparacion <- data.frame(
  medias_crudas = round(colMeans(lr[, names(pos_am)], na.rm = TRUE), 2),
  aldrich_mckelvey = round(pos_am, 2)
)
comparacion
# Las medias crudas ponian al PAN y a Morena casi juntos; A-M los separa
# como extremos del espacio.
# Las unidades son distintas (1-10 frente a escala comun): se comparan el
# orden y las separaciones, no los numeros.
# - Medias crudas: Morena (5.25) queda a la derecha de MC y PRD, mezclado
#   con la oposicion.
# - A-M: Morena en un extremo, aliados y MC cerca del centro, oposicion
#   del otro lado.
# - Morena es el que mas cambia: como la mitad lo pone a la izquierda y la
#   otra mitad a la derecha (paso 6), su media cruda cae en el centro.

# Grafica de las posiciones

# Grafica de las posiciones de Aldrich-McKelvey sobre una sola linea (el
# metodo estima una dimension). Las distancias entre puntos son las
# distancias entre partidos en la escala comun.
#   aes(x = pos, y = 0)        todos en y = 0: puntos sobre una linea
#   geom_hline                 dibuja esa linea
#   geom_text_repel(...)       etiquetas arriba; solo se mueven en vertical
#   scale_y_continuous(...)    espacio vertical para las etiquetas
#   theme(...)                 quita numeros del eje y y la cuadricula
ggplot(data.frame(partido = names(pos_am), pos = pos_am),
       aes(x = pos, y = 0, label = partido)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_point(size = 3, colour = "darkred") +
  geom_text_repel(size = 4, direction = "y", nudge_y = 0.05) +
  scale_y_continuous(limits = c(-0.1, 0.15)) +
  labs(title = "Posiciones de los partidos (Aldrich-McKelvey)",
       x = "Dimension estimada (signo fijado con Morena a la izquierda)",
       y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), panel.grid = element_blank())

# Interpretacion:
# - Morena aislado en su extremo (-0.73): esta mas lejos del PT (0.51)
#   que el PT del PRD (0.48).
# - PT, PVEM y MC, juntos cerca del centro (-0.22 a -0.03).
# - PRD, PAN y PRI, compactos del otro lado (0.27 a 0.44).
# - La frontera entre bloques esta entre MC y PRD (salto de 0.30).
# El espacio no es simetrico: Morena domina un extremo solo, sus aliados
# se perciben cerca del centro y la oposicion como un bloque compacto.

# --- Paso 20. Los pesos individuales: quien invierte la escala? -----------
# am$respondents: una fila por entrevistado con
#   intercept  desplazamiento de su escala
#   weight     peso: como traduce sus respuestas a la escala comun
#   idealpt    punto ideal = intercept + weight * selfplace
#   selfplace  autoubicacion original (1-10)
#   polinfo    informacion politica (no se usa)
resp <- am$respondents
summary(resp$weight)
# Min -1.16, mediana -0.08, max 1.16; 386 NA (no escalados: 1205 - 819).
# El signo del peso indica la direccion: negativo = escala invertida.
# El tamano indica cuanto vale cada punto de su escala: grande = la
# persona comprime a los partidos en pocos puntos.
# Mediana negativa: mas de la mitad de los pesos son negativos.

mean(resp$weight < 0, na.rm = TRUE)   # 0.54
# Proporcion de pesos negativos entre los 819 escalados (441 personas).

# Que significan estos resultados:
# - El electorado se divide en dos lecturas de la escala casi del mismo
#   tamano (54% invertida, 46% convencional). Ambos grupos ven la misma
#   estructura de bloques, pero le ponen las etiquetas al reves.
# - Son dos grupos claros: el histograma tiene dos jorobas (cerca de -0.2
#   y +0.2) y pocas personas cerca de 0.
# - La inversion se relaciona con el voto: 54.5% entre votantes de
#   Sheinbaum y 31% entre votantes de Galvez (paso 21).
# Lecturas posibles: "derecha" con sentido positivo; etiquetas sin
# significado compartido; proyeccion partidista.
# Implicacion: comparar la autoubicacion cruda entre grupos puede ser
# enganoso; la escala hay que corregirla o interpretarla con cuidado.
# Cautelas: inversion relativa (signo por convencion); solo 819 casos;
# sin ponderar, con sobrerrepresentacion de votantes de Sheinbaum.

hist(resp$weight, breaks = 40, col = "grey80",
     main = "Pesos individuales de Aldrich-McKelvey",
     xlab = "Peso (negativo = escala invertida)")
abline(v = 0, col = "darkred", lwd = 2)
# Pregunta para la clase: que significa que la mayoria use la escala en
# sentido contrario al orden que distingue a los bloques? Posibles
# lecturas: asociar "derecha" con "lo correcto", confundir los extremos,
# o un significado de izquierda y derecha distinto del de las elites.


# --- Paso 21. Puntos ideales y voto ------------------------------------------
# Comparamos la autoubicacion cruda con el punto ideal corregido,
# segun el voto presidencial.
# Codigos de la base armonizada: 1 = Sheinbaum, 10 = Galvez, 13 = Maynez.
voto <- zap_labels(cnep$H_VoteWhichRecent)
resp$voto <- case_when(voto == 1  ~ "Sheinbaum",
                       voto == 10 ~ "Galvez",
                       voto == 13 ~ "Maynez",
                       TRUE ~ NA_character_)

# Antes, una advertencia: algunos puntos ideales son extremos (hasta 9.9).
# Son entrevistados con pesos cercanos a cero, cuya correccion se dispara.
summary(resp$idealpt)
sum(abs(resp$idealpt) > 2, na.rm = TRUE)   # 12 casos
# Por eso comparamos medianas, que no se dejan arrastrar por esos casos.
# Comparar a los votantes de cada candidato:
#   filter(!is.na(...))  solo quienes votaron por un candidato y tienen
#                        punto ideal (! significa "no")
#   group_by(voto)       separa en tres grupos; lo demas se calcula por grupo
#   summarise(...)       n, mediana de autoubicacion cruda, mediana del
#                        punto ideal y % con peso negativo
# Se usan medianas por los 12 puntos ideales extremos.
resp %>%
  filter(!is.na(voto), !is.na(idealpt)) %>%
  group_by(voto) %>%
  summarise(n = n(),
            autoubicacion_cruda = round(median(selfplace), 2),
            punto_ideal_AM = round(median(idealpt), 2),
            pct_pesos_negativos = round(100 * mean(weight < 0), 1))
# Resultado: con la escala cruda, las medianas de autoubicacion de los
# votantes de Sheinbaum, Maynez y Galvez son muy parecidas (en medias:
# 5.0, 5.3 y 6.2). Con los puntos ideales corregidos se ordenan con
# claridad: Sheinbaum -0.55, Maynez -0.09, Galvez 0.18.
# La correccion por uso de la escala hace visible una estructura que la
# escala cruda oculta.
# - La razon: el 54.5% de los votantes de Sheinbaum invierte la escala;
#   al corregir, quedan donde realmente estan respecto de los partidos.
# Mensaje: la escala cruda no mostraba diferencias porque la mitad la usa
# al reves y eso las cancela; la correccion hace visible la estructura.
# Cautela: Galvez y Maynez son grupos pequenos (67 y 55).

# Grafica: distribucion de los puntos ideales corregidos por voto. Cada
# curva es un histograma suavizado; alpha = 0.4 las hace transparentes
# para ver el traslape. Se excluyen los 12 casos extremos (|idealpt| > 2).
ggplot(filter(resp, !is.na(voto), abs(idealpt) <= 2),
       aes(x = idealpt, fill = voto)) +
  geom_density(alpha = 0.4) +
  labs(title = "Puntos ideales corregidos segun voto presidencial",
       x = "Punto ideal (Aldrich-McKelvey)", y = "Densidad", fill = "Voto") +
  theme_minimal()
# Interpretacion:
# - Tres centros en el orden esperado: Sheinbaum (mediana -0.55), Maynez
#   (-0.08), Galvez (0.18), como sus partidos.
# - Traslape amplio: Sheinbaum y Galvez comparten cerca de 50% del area;
#   Maynez, 67% a 70% con los otros dos.
# - Diferencia real en promedio: un votante de Galvez al azar queda a la
#   derecha de uno de Sheinbaum en 82% de los casos. Pero no los separa:
#   muchos votantes de ambos estan en posiciones parecidas.
# - La ideologia ordena a los electorados en promedio, pero no explica
#   sola el voto (evaluacion de gobierno, programas, candidatos, partido).
# Advertencias: la altura no indica cuantas personas hay (cada curva tiene
# area 1); Maynez es alta porque esta concentrada, no porque sea mayor.
# Galvez (66) y Maynez (53) son grupos pequenos.

# =====================================================================
# PARTE D. MDS DE LOS TEMAS DE POLITICA
# =====================================================================
# Objetos: 14 temas de politica (preguntas 109 a 124 del cuestionario),
# cada uno en una tarjeta de 1 a 10 con una frase en cada extremo.
# Pregunta: los temas se agrupan segun el voto o segun la ideologia?

# --- D1. Temas en la misma direccion ---------------------------------------
temas_vars <- c("J_Abortion_F", "J_Conflict", "J_Customs", "J_EcoGrow",
                "J_EqualInd", "J_GovWellBeing_F", "J_Immigrants",
                "J_OrderLib", "J_OurWay", "J_Particip", "J_PrivPub2",
                "J_ReligLaw", "J_ServTax", "J_SexRole")

# Verificar la direccion: que significa el 1 en cada tema?
sapply(temas_vars, function(v) {
  et <- attr(cnep[[v]], "labels")
  names(et)[et == 1]
})
# En los 14, el 1 es el polo liberal o igualitario (aborto legal, ley
# secular, redistribucion, migrantes bienvenidos...). La base armonizada
# ya los dejo en la misma direccion: 1 = liberal, 10 = conservador.
# (En el cuestionario no era asi; la CNEP invirtio 7 temas al armonizar.)

temas <- cnep %>%
  select(all_of(temas_vars)) %>%
  zap_labels() %>%
  mutate(across(everything(), ~ ifelse(.x >= 1 & .x <= 10, .x, NA)))

# Nombres cortos en espanol; el orden debe coincidir con temas_vars.
colnames(temas) <- c("aborto", "conflicto", "costumbres", "ambiente",
                     "igualdad", "bienestar", "migrantes", "orden",
                     "lo_propio", "participacion", "privatizacion",
                     "ley_religiosa", "impuestos", "roles_genero")
nombres_temas <- colnames(temas)

temas_c <- na.omit(temas)   # quienes respondieron los 14 temas
nrow(temas_c)               # 1050 casos


# --- D2. Las matrices ---------------------------------------------------------
r_temas <- cor(temas_c)          # correlaciones entre temas
round(r_temas, 2)
d_temas <- sim2diss(r_temas, method = "corr")   # disimilitudes
round(d_temas, 2)
# Lectura: las correlaciones son debiles (casi todas entre -0.3 y 0.3) y
# las disimilitudes estan todas cerca de 1 (0.82 a 1.14). Los temas se
# relacionan poco entre si: quien es liberal en un tema no necesariamente
# lo es en otro (baja coherencia ideologica, en terminos de Converse).
# Las correlaciones mas altas: orden-conflicto (0.29), roles_genero-
# privatizacion (0.22), ley_religiosa-roles_genero (0.20).


# --- D3. El mapa de los temas -------------------------------------------------
set.seed(2024)
mds_temas <- mds(d_temas, ndim = 2, type = "ordinal")
mds_temas   # stress-1 = 0.098: limite de "aceptable" segun Kruskal

set.seed(2024)
perm_temas <- permtest(mds_temas, data = temas_c, method.dat = "pearson",
                       nrep = 500, verbose = FALSE)
perm_temas  # p < 0.001: hay estructura, aunque debil

coord_t <- data.frame(tema = rownames(mds_temas$conf), mds_temas$conf)
ggplot(coord_t, aes(D1, D2, label = tema)) +
  geom_point(colour = "darkred") +
  geom_text_repel() +
  coord_equal() +
  labs(title = "MDS ordinal de los temas de politica") +
  theme_minimal()
# Lectura del mapa:
# - Un grupo de temas morales y religiosos: ley_religiosa, roles_genero,
#   aborto y privatizacion.
# - Un grupo de orden y defensa de lo propio: orden, conflicto, lo_propio.
# - Un grupo distributivo y de inclusion: igualdad, impuestos, bienestar,
#   participacion, costumbres, ambiente, migrantes.
# Advertencia: la dimension horizontal coincide con los 7 temas que la
# CNEP invirtio al armonizar. Puede reflejar en parte el formato de la
# tarjeta (personas que eligen siempre la frase de un mismo lado), no
# solo ideologia. Por eso se interpreta con cuidado.


# --- D4. Voto o ideologia? ----------------------------------------------------
# Usamos el voto presidencial y la ideologia corregida de Aldrich-
# McKelvey (punto ideal, parte C). resp tiene una fila por entrevistado,
# en el mismo orden que cnep.
temas_vi <- temas %>%
  mutate(voto = resp$voto,
         ideologia = ifelse(abs(resp$idealpt) <= 2, resp$idealpt, NA))

# a) Promedio de cada tema segun el voto (1 = liberal, 10 = conservador)
medias_voto <- temas_vi %>%
  filter(!is.na(voto)) %>%
  group_by(voto) %>%
  summarise(across(all_of(nombres_temas), ~ mean(.x, na.rm = TRUE)))
round(t(data.frame(medias_voto[, -1], row.names = medias_voto$voto)), 1)
# Lectura: los votantes de los tres candidatos tienen promedios muy
# parecidos en casi todos los temas (diferencias menores a un punto en
# una escala de 10). Las mayores diferencias: ley_religiosa (Galvez 4.2,
# Sheinbaum 3.8, Maynez 3.0), costumbres e impuestos. Los votantes de
# Galvez son algo mas conservadores en ley religiosa y aborto.

# b) Correlacion de cada tema con el voto por cada candidato y con la
#    ideologia. Cada fila es un tema; cada columna, una variable externa.
externas_t <- data.frame(
  sheinbaum = sapply(nombres_temas, function(t)
    cor(temas_vi[[t]], temas_vi$voto == "Sheinbaum", use = "pairwise")),
  galvez    = sapply(nombres_temas, function(t)
    cor(temas_vi[[t]], temas_vi$voto == "Galvez", use = "pairwise")),
  maynez    = sapply(nombres_temas, function(t)
    cor(temas_vi[[t]], temas_vi$voto == "Maynez", use = "pairwise")),
  ideologia = sapply(nombres_temas, function(t)
    cor(temas_vi[[t]], temas_vi$ideologia, use = "pairwise"))
)
round(externas_t, 2)
# Todas las correlaciones son pequenas (menores a 0.12 en valor
# absoluto): ningun tema distingue con fuerza a los votantes. Las
# mayores con la ideologia: aborto (0.11), privatizacion (0.11) y
# ley_religiosa (0.10): quien esta mas a la derecha es algo mas
# conservador en esos temas.

# c) Se alinea el mapa con el voto o con la ideologia? (property fitting)
ajuste_t <- biplotmds(mds_temas, extvar = externas_t)
round(ajuste_t$R2vec, 2)
# R2: ideologia 0.67, galvez 0.36, sheinbaum 0.13, maynez 0.08
plot(ajuste_t, main = "Temas: voto frente a ideologia",
     vecscale = 0.8, label.conf = list(label = TRUE, pos = 3))
# Lectura:
# - La ideologia es la que mejor se alinea con el mapa (R2 = 0.67): su
#   flecha apunta hacia el grupo moral-religioso (ley_religiosa,
#   privatizacion, aborto, roles_genero). La forma en que se acomodan
#   los temas tiene que ver con la ideologia, sobre todo con estos temas.
# - El voto se alinea poco: Galvez algo (0.36), Sheinbaum y Maynez casi
#   nada (0.13 y 0.08). Los temas no se agrupan segun el voto.
#
# CONCLUSION DE LA PARTE D
# - Los temas se relacionan debilmente entre si.
# - Lo poco de estructura ideologica que hay se concentra en los temas
#   morales y religiosos.
# - Esos temas casi no distinguen a los votantes: el voto de 2024 no se
#   organizo por estas actitudes, sino por otros factores (evaluacion
#   del gobierno, programas, candidatos, bloques de coalicion).
# Cautela: con 14 temas el R2 es orientativo, y las correlaciones de las
# que parte son muy pequenas

rm(list = ls())   # borra todos los objetos del Environment (cnep, lr, mds_ord...)
graphics.off()    # cierra todas las graficas abiertas
cat("\014")       # limpia la consola (equivale a Ctrl + L)