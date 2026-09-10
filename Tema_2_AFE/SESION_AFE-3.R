# ============================================================
# TEMA 2 - Análisis Factorial Exploratorio (AFE)
# Sesiones 4-5
# Curso: Análisis multivariado y métodos de clasificación de datos con R
# El Colegio de México - Centro de Estudios Sociológicos
# Profesor: Isaac Cisneros Yescas
# ============================================================
# Otden de este script: los mismos pasos, aplicados dos veces.
# Primero con datos donde TÚ construyes la estructura (para saber
# de antemano qué debería encontrar el análisis), y después con
# datos reales, donde la estructura hay que descubrirla de verdad.
# ============================================================

#Instrucciones: correr este bloque UNA SOLA VEZ antes de la
# Sesión 4. Si ya tienen instalados algunos de estos paquetes
# desde el Tema 1 (tidyverse, corrplot, haven), R los omite
# automáticamente sin volver a instalarlos.

install.packages(c(
  "tidyverse",    # manipulación de datos y gráficos
  "psych",        # fa(), fa.parallel(), vss(), fa.diagram(), KMO(), polychoric()
  "GPArotation",  # requerido por psych para la rotación oblimin
  "corrplot",     # visualización de matrices de correlación
  "haven",        # lectura de archivos de Stata (.dta)
  "lavaan"        # análisis factorial confirmatorio (Sesión 6)
))

# ------------------------------------------------------------
# 0. PAQUETES
# ------------------------------------------------------------
library(tidyverse)   # manipulación de datos y gráficos
library(psych)       # fa(), fa.parallel(), vss(), fa.diagram(), KMO(), cortest.bartlett()
library(GPArotation) # requerido por psych para rotaciones oblicuas (oblimin)
library(corrplot)    # visualización de matrices de correlación
library(haven)       # lectura de archivos de Stata (.dta)


# ============================================================
# BLOQUE 1: DATOS SIMULADOS CON DOS FACTORES CONOCIDOS
# ============================================================
# Idea pedagógica: construimos 6 ítems a partir de DOS factores
# latentes distintos -- "apoyo a la democracia" y "eficacia
# política" -- que además están correlacionados entre sí (como
# es realista esperar en ciencia política: dos actitudes afines,
# no independientes). El objetivo del ejercicio es ver si el AFE
# logra RECUPERAR esta estructura de 2 factores a partir de los
# 6 ítems observados, sin que el análisis "sepa" de antemano cómo
# se construyeron.

#1) La hipótesis propuesta
#Teoría de fondo: La literatura sobre confianza institucional distingue entre confianza en el aparato estatal/político(instituciones formales, con poder coercitivo o de representación) 
#y confianza en el tejido asociativo de la sociedad civil(organizaciones voluntarias, no estatales). 
#Esta distinción tiene raíces en la tradición del capital social (Putnam, 1993, 2000) y en la literatura de "ciudadanos críticos" (Norris, 1999, 2011 — que ya está en tus lecturas del Tema 5).
#Hipótesis (H1):
#La confianza institucional en México no es unidimensional, sino que se organiza en dos dimensiones distintas y correlacionadas: (1) confianza en instituciones políticas/estatales y (2) confianza en organizaciones de la sociedad civil.

set.seed(2026)
n <- 500

# --- Paso 1: crear los DOS factores latentes, correlacionados ---
# Generamos dos variables normales independientes...
f_independiente1 <- rnorm(n)
f_independiente2 <- rnorm(n)

# ...y las combinamos para que terminen correlacionadas entre sí.
# Este es un truco estadístico simple: si Y = a*X + b*Z (con X y Z
# independientes), entonces X y Y quedan correlacionados en una
# magnitud que depende de "a" y "b". Aquí fijamos una correlación
# aproximada de 0.40 entre los dos factores -- ni tan alta que
# sean el mismo concepto, ni tan baja que sean irrelevantes entre sí.
apoyo_democracia   <- f_independiente1
eficacia_politica  <- 0.40*f_independiente1 + sqrt(1 - 0.40^2)*f_independiente2
# La raíz cuadrada asegura que eficacia_politica también tenga
# varianza 1 (como una variable normal estándar) -- es un detalle
# técnico para que la correlación resultante sea EXACTAMENTE 0.40,
# no necesitas memorizar la fórmula, solo saber qué hace.

# --- Paso 2: construir los 6 ítems observados a partir de los 2 factores ---
# Los primeros 3 ítems ("ing") cargan en apoyo_democracia.
# Los últimos 3 ítems ("efi") cargan en eficacia_politica.
# Cada ítem = (peso) * su factor + ruido individual -- la misma
# lógica de medición que ya usamos en el Tema 1.

datos_afe <- tibble(
  ing1 = 0.75*apoyo_democracia  + rnorm(n, 0, 0.6),
  ing2 = 0.70*apoyo_democracia  + rnorm(n, 0, 0.6),
  ing3 = 0.68*apoyo_democracia  + rnorm(n, 0, 0.6),
  efi1 = 0.72*eficacia_politica + rnorm(n, 0, 0.6),
  efi2 = 0.69*eficacia_politica + rnorm(n, 0, 0.6),
  efi3 = 0.74*eficacia_politica + rnorm(n, 0, 0.6)
)
# Con esto YA SABEMOS la respuesta correcta antes de analizar nada:
# deberían aparecer 2 factores, el primero definido por ing1-ing3,
# el segundo por efi1-efi3, y correlacionados alrededor de 0.40.
# Esa es la ventaja pedagógica de empezar con datos simulados.


# --- Paso 3: matriz de correlación ---
cor(datos_afe)
corrplot(cor(datos_afe),
         method = 'color', type = 'upper', addCoef.col = 'black',
         tl.col = 'black', tl.srt = 45,
         title = 'Correlación entre los 6 ítems simulados',
         mar = c(0,0,1,0))
# Qué esperar: los 3 ítems "ing" deberían correlacionar fuerte
# entre sí, los 3 "efi" también fuerte entre sí, pero la
# correlación ENTRE un ítem "ing" y uno "efi" debería ser más
# débil (porque pasa "a través" de la correlación 0.40 entre los
# dos factores, no es una relación directa).


# --- Paso 4: adecuación muestral ---
KMO(datos_afe)
cortest.bartlett(cor(datos_afe), n = nrow(datos_afe))
# Igual que en el Tema 1: confirma si tiene sentido seguir adelante
# con un análisis factorial. Con esta estructura simulada, se
# espera un KMO alto y un Bartlett altamente significativo.


# --- Paso 5: ¿cuántos factores retener? ---
# Aquí es donde el AFE va más allá del PCA: en vez de solo mirar
# el scree plot, usamos análisis paralelo, que compara la
# varianza real de tus datos contra la varianza que saldría de
# datos puramente ALEATORIOS del mismo tamaño.

fa.parallel(datos_afe, fa = 'fa', main = 'Análisis paralelo')
# fa = 'fa' le dice a la función que compare específicamente para
# análisis factorial (existe también la opción 'pc' para PCA).
# Se dibujan dos líneas: la de tus datos reales, y la de datos
# simulados al azar. La recomendación es retener tantos factores
# como puntos de tu línea queden POR ENCIMA de la línea aleatoria.
# Con esta simulación, se espera que la función recomiende
# exactamente 2 factores -- justo lo que construimos.

vss(datos_afe, n = 4)
# VSS (Very Simple Structure) es un criterio alternativo/
# complementario: prueba distintas soluciones (1 factor, 2
# factores, 3 factores...) y reporta cuál ajusta mejor con la
# estructura más simple posible. n = 4 le pide que pruebe hasta
# un máximo de 4 factores.

vss() ("Very Simple Structure") prueba distintas soluciones de
# 1 a 4 factores (n = 4) y evalúa, con varios criterios a la vez,
# cuál número de factores ofrece el mejor equilibrio entre ajuste
# y simplicidad.
#
# --- Los 4 criterios del encabezado, y por qué todos apuntan a 2 ---
#
# VSS complexity 1: ajuste asumiendo que cada ítem carga en UN SOLO
#   factor (estructura más simple posible). Máximo = 0.85 con 2
#   factores (más alto es mejor).
# VSS complexity 2: mismo criterio, pero permitiendo que cada ítem
#   cargue en hasta DOS factores. Máximo = 0.91 con 2 factores.
# Velicer MAP: basado en correlaciones parciales residuales.
#   Mínimo = 0.10 con 2 factores (aquí, más bajo es mejor).
# BIC: penaliza modelos innecesariamente complejos. Mínimo = -23.4
#   con 2 factores (más negativo es mejor).
#
# Cuatro criterios matemáticamente distintos entre sí, los cuatro
# coinciden en 2 factores -- señal robusta de que esa es la
# estructura correcta (y, en este caso, confirma la estructura de
# 2 factores construida deliberadamente en la simulación).
#
# --- La tabla "Statistics by number of factors", fila por fila ---
#
# Cada fila es una solución con 1, 2, 3 o 4 factores.
#
# prob (p-valor de la prueba de chi-cuadrado del modelo):
#   1 factor:  p = 3.0e-97 ->  rechaza fuertemente el ajuste
#              (el modelo de 1 factor NO describe bien los datos)
#   2 factores: p = 0.83   -> no se rechaza el ajuste
#              (el modelo de 2 factores SÍ describe bien los datos)

#En palabras simples: H0 H0 dice "el modelo con k factores explica bien las correlaciones observadas — cualquier diferencia entre 
#lo que el modelo predice y lo que realmente se observó es solo ruido muestral, no evidencia de mal ajuste."

#Aquí la lógica es inversa a la mayoría de las pruebas de hipótesis que conoces: normalmente "rechazar" es la buena noticia (encontraste un efecto). 
#Aquí es al revés — rechazar 
#es la mala noticia (tu modelo no ajusta bien), y no rechazar es lo que buscas 
#(tu modelo sí describe adecuadamente los datos).
#
# RMSEA: 0.32 con 1 factor (mal ajuste, > 0.10) vs. 0.00 con
#   2 factores (ajuste excelente).
#
# dof (grados de libertad): con 3 factores llega a 0, y con 4
#   factores a -3 (negativo) -- el modelo ya no tiene suficiente
#   información para estimarse de forma confiable. Por eso prob,
#   RMSEA, BIC y SABIC aparecen como NA en esas filas: ni siquiera
#   se pueden calcular.
#
# --- Conclusión ---
# Con 6 ítems, un modelo de 2 factores no solo es el que mejor
# equilibra ajuste y simplicidad: es, literalmente, el límite de
# lo que se puede estimar de forma confiable con estos datos.
# Se procede con nfactors = 2 en los pasos siguientes.

# --- Paso 6: extracción con máxima verosimilitud (ML) ---
fa_sim <- fa(datos_afe, nfactors = 2, fm = 'ml', rotate = 'none')
# fa() es la función central del análisis factorial en psych
# (distinta de principal(), que es para PCA).
#   - nfactors = 2: extraemos 2 factores, según lo que ya
#     confirmamos con el análisis paralelo.
#   - fm = 'ml' ("factor method: maximum likelihood"): el método
#     de extracción más común y el que se conecta después con el
#     análisis factorial confirmatorio de la Sesión 6.
#   - rotate = 'none': solución sin rotar, como punto de partida.

print(fa_sim$loadings, cutoff = 0.3)
# Qué esperar: SIN rotar, es común que los 2 factores salgan
# "mezclados" -- que un ítem cargue en ambos factores a la vez,
# aunque matemáticamente la solución sea correcta. Esto es
# exactamente lo que motiva el siguiente paso: rotar para
# facilitar la interpretación.

# EXPLICACIÓN DEL RESULTADO (extracción SIN rotar)
# ------------------------------------------------------------
# Cada celda es la carga del ítem en ese factor (ML1, ML2), sin
# ninguna rotación aplicada todavía.
#
# Problema visible: los 6 ítems cargan en AMBOS factores a la vez
# (ninguna carga quedó oculta por el cutoff = 0.3) -- ing1-ing3
# tienen carga negativa en ML2, y efi1-efi3 tienen carga positiva
# en ML2, en vez de estar cerca de 0. Es una estructura "mezclada",
# difícil de interpretar -- por eso el siguiente paso es rotar.
#
# SS loadings / Proportion Var / Cumulative Var: ML1 explica 39.5%
# de la varianza y ML2 un 20.5% adicional, 60.0% acumulado entre
# los dos -- este total NO cambia al rotar (rotar solo reparte la
# varianza entre los factores de otra forma, no la aumenta).

# --- Paso 7: rotación oblimin (oblicua) ---
# Elegimos oblimin directamente (no varimax) porque ya SABEMOS,
# por construcción, que los 2 factores están correlacionados
# (0.40) -- forzar una rotación ortogonal (varimax) escondería
# artificialmente esa relación real entre los factores.

fa_sim_oblimin <- fa(datos_afe, nfactors = 2, fm = 'ml', rotate = 'oblimin')
print(fa_sim_oblimin$loadings, cutoff = 0.3)
# Qué esperar: ahora sí debería verse una estructura limpia --
# ing1, ing2, ing3 cargando fuerte en un factor, y efi1, efi2,
# efi3 cargando fuerte en el otro, cada ítem prácticamente sin
# carga relevante en el factor que no le corresponde.

fa_sim_oblimin$Phi
# Correlación entre los 2 factores rotados. Se espera un valor
# cercano a 0.40 -- si sale así, significa que el AFE recuperó
# correctamente tanto la estructura de los ítems COMO la
# correlación real entre los dos conceptos latentes.


# --- Paso 8: comparación visual con varimax (para contraste pedagógico) ---
fa_sim_varimax <- fa(datos_afe, nfactors = 2, fm = 'ml', rotate = 'varimax')
print(fa_sim_varimax$loadings, cutoff = 0.3)
# Comparar esta tabla contra la de oblimin: con varimax, la
# correlación entre factores se fuerza a 0, así que la técnica
# "no ve" la relación real de 0.40 que sí existe entre apoyo a la
# democracia y eficacia política. Ilustra por qué la elección de
# rotación no es solo un detalle técnico, sino una decisión que
# puede ocultar (o revelar) una relación teóricamente importante.

fa_sim_varimax$Phi
# Con varimax (rotación ORTOGONAL), $Phi devuelve NULL -- psych ni
# siquiera calcula esta matriz aquí, porque varimax FUERZA a que
# los factores queden en ángulo de 90° (correlación 0 por
# definición, no por resultado del análisis).

cor(fa_sim_varimax$scores)
# Alternativa para comprobarlo con un número real: se calcula la
# correlación entre las PUNTUACIONES (scores) de cada factor, para
# cada individuo. Al ser factores ortogonales, debe salir muy
# cercana a 0 -- a diferencia de fa_sim_oblimin$Phi (~0.40), que sí
# refleja la correlación real que existe entre apoyo a la
# democracia y eficacia política.

# --- Paso 9: diagrama del modelo factorial ---
fa.diagram(fa_sim_oblimin, main = 'Diagrama factorial (oblimin)')
# fa.diagram() dibuja el modelo de medición: los factores como
# círculos, los ítems como cajas, y flechas con el valor de cada
# carga. Es la forma más intuitiva de mostrarle a un grupo nuevo
# en el tema qué significa "variable latente": algo que no se ve
# directamente (no tiene caja, tiene círculo) pero que se infiere
# a partir de los ítems que sí se observaron.

fa_sim_oblimin$communality
# Comunalidad de cada uno de los 6 ítems. Como tú mismo construiste
# los pesos factoriales (0.75, 0.70, 0.68 para ing1-ing3; 0.72, 0.69,
# 0.74 para efi1-efi3), puedes verificar "a mano" que el ítem con
# el peso más alto (ing) debería tener también la comunalidad
# más alta -- con un factor único, comunalidad ≈ peso².

# --- Paso 10: puntuaciones factoriales ---
fa_sim_oblimin$scores
# $scores contiene, para cada uno de los 500 casos simulados, su
# puntuación estimada en cada uno de los 2 factores -- la versión
# "factorial" del índice sumatorio que construimos en el Tema 1.
# Estas puntuaciones se podrían usar después como variables en un
# modelo de regresión, por ejemplo, para predecir participación
# política a partir del apoyo a la democracia estimado aquí.


# ============================================================
# BLOQUE 2: DATOS REALES -- REANÁLISIS DE LAPOP CON AFE
# ============================================================
# Reutilizamos los mismos 5 ítems de confianza institucional del
# Tema 1, pero ahora con fa() en vez de principal()/PCA(). La
# pregunta que responde este bloque: si es la MISMA base de datos,
# ¿por qué cambian los resultados al cambiar de técnica?

lapop <- read_dta('/Users/isaaccisneros/Desktop/ISAAC/CURSO/Cursos 2026/DOCUMENTOS CURSO/R/SESION 2/2023_MEX_2023_LAPOP_AmericasBarometer_v1.0_w.dta')
# Ajusta la ruta según donde tengas guardado el archivo, y recuerda
# usar "/" en vez de "\" (ver notas del Tema 1 sobre este error).

items_lapop <- lapop |>
  select(conf_ejecutivo   = b21a,
         conf_legislativo = b13,
         conf_judicial    = b31,
         conf_partidos    = b21,
         conf_electoral   = b47a) |>
  drop_na()

# Nota sobre la escala: los ítems de LAPOP usan 7 categorías (1-7),
# a diferencia de los 4 de WVS (ver Bloque 3). Con 5 categorías o
# más, tratar los ítems como continuos (Pearson, ML estándar) suele
# considerarse una aproximación razonable en la práctica -- por
# eso este bloque NO usa correlaciones policóricas. Con 4 categorías
# o menos (como en WVS), la aproximación continua se vuelve más
# problemática y conviene corregirla explícitamente.

# --- Adecuación muestral (ya la calculamos en el Tema 1, se repite aquí por completitud) ---
KMO(items_lapop)
cortest.bartlett(cor(items_lapop), n = nrow(items_lapop))

# --- ¿Cuántos factores? ---
fa.parallel(items_lapop, fa = 'fa', main = 'Análisis paralelo: LAPOP')
# A diferencia de los datos simulados, aquí NO sabemos de antemano
# cuántos factores esperar -- el análisis paralelo es quien lo dice.
# Con estos 5 ítems de confianza institucional, es razonable
# esperar que recomiende retener 1 solo factor (ya vimos en el
# Tema 1 que un componente concentraba 60.64% de la varianza).

# --- Extracción con 1 factor (ML) ---
fa_lapop <- fa(items_lapop, nfactors = 1, fm = 'ml')
print(fa_lapop$loadings, cutoff = 0.3)

# --- Comparación directa contra el PCA del Tema 1 ---
pca_lapop_comparar <- principal(items_lapop, nfactors = 1, rotate = 'none')
print(pca_lapop_comparar$loadings, cutoff = 0.3)

# Con un solo factor no hay nada que rotar (rotar necesita al
# menos 2 factores/componentes), así que esta comparación aísla
# limpiamente la diferencia entre PCA y AFE: mismos datos, mismo
# número de factores, pero un modelo distinto por debajo.
# Qué esperar: las cargas del AFE deberían ser un poco MÁS BAJAS
# que las del PCA -- porque el AFE ya "descontó" la varianza única
# de cada ítem antes de repartir el resto, mientras que el PCA
# reparte la varianza total sin distinguir esa diferencia.

fa.diagram(fa_lapop, main = 'Diagrama factorial: confianza institucional (LAPOP)')

fa_lapop$communality
# A diferencia de pca_lapop_comparar (PCA), este objeto SÍ separa
# varianza común de varianza única -- es la comunalidad la que no
# tiene equivalente en el PCA de la sección anterior, y por eso es
# el mejor punto del script para mostrar esa diferencia con un
# número real: conf_ejecutivo debería salir con la comunalidad más
# baja de las 5 (ya lo vimos: ≈0.331), coherente con que también
# tuvo la carga más baja tanto en PCA como en AFE.

# ------------------------------------------------------------
# EJERCICIO OPCIONAL DE ROBUSTEZ: Pearson vs. policórica en LAPOP
# ------------------------------------------------------------
# Como vimos con WVS (Bloque 3), la corrección policórica importa
# más entre menos categorías tenga la escala. LAPOP usa 7 categorías,
# que la literatura metodológica considera "zona segura" para
# tratar como continuas -- pero podemos comprobarlo directamente
# en vez de solo confiar en la regla general.

fa_lapop_poly <- fa(items_lapop, nfactors = 1, fm = 'ml', cor = 'poly')
print(fa_lapop_poly$loadings, cutoff = 0.3)

# Comparación lado a lado de las cargas (Pearson vs. policórica):
data.frame(
  item              = rownames(fa_lapop$loadings),
  pearson           = as.numeric(fa_lapop$loadings),
  policorica        = as.numeric(fa_lapop_poly$loadings)
)
# Qué esperar: las dos columnas deberían ser MUY parecidas entre sí
# (diferencias de centésimas, no de décimas) -- a diferencia de lo
# que se observó con WVS (4 categorías), donde el cambio fue más
# notorio. Esto confirma, con evidencia propia y no solo con la
# regla general, que con 7 categorías la aproximación continua era
# razonable desde el inicio.


# ============================================================
# BLOQUE 3: SEGUNDO EJEMPLO DE DATOS REALES -- WVS MÉXICO (2 FACTORES)
# ============================================================
# A diferencia de LAPOP (1 factor), aquí usamos la Encuesta Mundial de
# Valores (World Values Survey, Ola 7, México) para mostrar un caso real
# con DOS factores de confianza institucional, claramente diferenciados:
#   Factor 1: confianza en instituciones POLÍTICAS/ESTATALES
#   Factor 2: confianza en organizaciones de la SOCIEDAD CIVIL
#
# Escala de todos los ítems: 1 = "mucha confianza" ... 4 = "ninguna
# confianza" (escala INVERTIDA respecto a LAPOP -- ¡cuidado al
# interpretar el signo de las cargas!).

wvs <- read_dta('/Users/isaaccisneros/Desktop/ISAAC/CURSO/Cursos 2026/DOCUMENTOS CURSO/R/SESION 2/WVS_Wave_7_Mexico_Stata_v5.1.dta')
# Ajusta la ruta según donde tengas guardado el archivo.

items_wvs <- wvs |>
  select(fuerzas_armadas = Q65,
         prensa          = Q66,
         television      = Q67,
         sindicatos      = Q68,
         policia         = Q69,
         tribunales      = Q70,
         gobierno        = Q71,
         partidos        = Q72,
         parlamento      = Q73,
         serv_civiles    = Q74,
         elecciones      = Q76,
         mov_ambiental   = Q79,
         mov_mujeres     = Q80,
         org_caritativas = Q81) |>
  drop_na()

nrow(items_wvs)

# --- Correlación (Pearson, de referencia) ---
cor(items_wvs)
corrplot(cor(items_wvs),
         method = 'color', type = 'upper', addCoef.col = 'black',
         tl.col = 'black', tl.srt = 45, number.cex = 0.6,
         title = 'Correlación: confianza institucional (WVS México)',
         mar = c(0,0,1,0))
# Qué esperar: dos "bloques" visibles en el mapa de calor -- los 11
# primeros ítems (instituciones políticas/estatales) correlacionando
# fuerte entre sí, y los 3 últimos (movimientos/organizaciones civiles)
# correlacionando aún más fuerte entre sí, con una correlación más
# débil ENTRE ambos bloques.

# ------------------------------------------------------------
# AJUSTE IMPORTANTE: correlaciones POLICÓRICAS, no Pearson
# ------------------------------------------------------------
# Los 14 ítems son ordinales de solo 4 categorías (1 = "mucha
# confianza" ... 4 = "ninguna confianza"), no variables continuas.
# cor() con Pearson SUBESTIMA sistemáticamente la correlación real
# entre variables ordinales con pocas categorías -- por eso, antes
# de seguir con KMO, Bartlett, análisis paralelo o la extracción,
# hay que recalcular la matriz de correlación con un método
# apropiado para datos ordinales.

poly_wvs <- polychoric(items_wvs)
# polychoric() (de psych) estima la correlación asumiendo que
# detrás de cada ítem ordinal de 4 categorías hay una variable
# continua "verdadera" no observada -- es el equivalente, para
# datos categóricos, de lo que Pearson hace con datos continuos.

R_poly <- poly_wvs$rho
# $rho es la matriz de correlaciones policóricas resultante (14x14).
# A partir de aquí, TODO el análisis debe usar R_poly en vez de
# cor(items_wvs).

corrplot(R_poly,
         method = 'color', type = 'upper', addCoef.col = 'black',
         tl.col = 'black', tl.srt = 45, number.cex = 0.6,
         title = 'Correlación POLICÓRICA: confianza institucional (WVS)',
         mar = c(0,0,1,0))
# Compara este mapa de calor contra el anterior (Pearson): es
# normal que las correlaciones policóricas salgan un poco más
# ALTAS que las de Pearson para los mismos pares de variables --
# es justamente la corrección que estábamos buscando.

# --- Adecuación muestral (con la matriz policórica) ---
KMO(R_poly)
cortest.bartlett(R_poly, n = nrow(items_wvs))
# Ambas funciones aceptan una matriz de correlación ya calculada
# en vez de los datos crudos -- por eso aquí se les pasa R_poly.

# --- ¿Cuántos factores? (también con la matriz policórica) ---
fa.parallel(items_wvs, fa = 'fa', cor = 'poly',
            main = 'Análisis paralelo: WVS México (policórico)')
# cor = 'poly' le pide a fa.parallel() calcular internamente la
# matriz policórica antes de comparar contra datos aleatorios --
# no hace falta pasarle R_poly manualmente aquí.

# --- Extracción (ML) + rotación oblimin ---
# Se usa oblimin directamente: no hay razón teórica para asumir que
# "confiar en el Estado" y "confiar en la sociedad civil" sean
# conceptos completamente independientes -- de hecho, es razonable
# esperar que estén correlacionados (una desconfianza generalizada
# podría abarcar ambos tipos de instituciones a la vez).

fa_wvs <- fa(items_wvs, nfactors = 2, fm = 'ml', rotate = 'oblimin', cor = 'poly')
# Igual que en fa.parallel(), cor = 'poly' le pide a fa() calcular
# y usar la matriz de correlaciones policóricas -- sin este
# argumento, fa() habría usado Pearson por defecto sobre datos
# que en realidad son ordinales.
print(fa_wvs$loadings, cutoff = 0.3)
fa_wvs$Phi
# Nota: con la corrección policórica, es esperable que las cargas
# y la correlación entre factores (Phi) cambien un poco respecto a
# lo que se vio en la explicación teórica -- normalmente hacia
# valores algo MÁS ALTOS, ya que Pearson subestimaba la relación
# real entre estos ítems ordinales.

fa.diagram(fa_wvs, main = 'Diagrama factorial: confianza política vs. sociedad civil (WVS)')
# Este diagrama es el más ilustrativo del curso hasta ahora: dos
# círculos de factor claramente separados, cada uno con su propio
# conjunto de ítems, y una línea curva conectando ambos factores que
# representa la correlación (Phi) entre ellos.

fa_wvs$communality
# La comunalidad de cada ítem: qué proporción de su varianza está
# explicada por el modelo de 2 factores en conjunto. Ítems como
# gobierno o partidos deberían tener comunalidades altas (bien
# explicados por Factor 1); fuerzas_armadas podría tener una
# comunalidad más modesta (menos "centrales" al factor político).

fa_wvs$scores
# Puntuaciones factoriales de cada uno de los ~1,591 encuestados en
# los 2 factores (política y sociedad civil) -- útil si más adelante
# se quisieran usar estos factores como variables en un modelo de
# regresión (por ejemplo, para explicar participación política).

# ============================================================
# Análisis Factorial Confirmatorio (AFC)
# Sesión 6
# Curso: Análisis multivariado y métodos de clasificación de datos con R
# El Colegio de México - Centro de Estudios Sociológicos
# Profesor: Isaac Cisneros Yescas
# ============================================================
# Este script retoma el hallazgo de las Sesiones 4-5: con datos
# de WVS México, la confianza institucional se organiza en 2
# factores (política/estatal y sociedad civil). Ahí lo DESCUBRIMOS
# con AFE, sin restricciones. Aquí lo PONEMOS A PRUEBA formalmente
# con AFC: le decimos a R exactamente qué ítem va con qué factor,
# y evaluamos si esa estructura ajusta bien a los datos.
#
# Hipótesis a confirmar (H1):
# La confianza institucional en México se organiza en dos
# dimensiones correlacionadas: confianza política/estatal y
# confianza en la sociedad civil.
# ============================================================


# ------------------------------------------------------------
# 0. PAQUETES
# ------------------------------------------------------------
library(tidyverse)
library(lavaan)   # cfa(), summary(), fitMeasures(), modindices(), parameterEstimates()
library(haven)    # read_dta()


# ------------------------------------------------------------
# 1. CARGA Y PREPARACIÓN DE DATOS (igual que en la Sesión 4-5)
# ------------------------------------------------------------
wvs <- read_dta("WVS_Wave_7_Mexico_Stata_v5.1.dta")
wvs <- read_dta('/Users/isaaccisneros/Desktop/ISAAC/CURSO/Cursos 2026/DOCUMENTOS CURSO/R/SESION 2/WVS_Wave_7_Mexico_Stata_v5.1.dta')

# Ajusta la ruta según donde tengas guardado el archivo, y recuerda
# usar "/" en vez de "\", incluso en Windows.

items_wvs <- wvs |>
  select(fuerzas_armadas = Q65,
         prensa          = Q66,
         television      = Q67,
         sindicatos      = Q68,
         policia         = Q69,
         tribunales      = Q70,
         gobierno        = Q71,
         partidos        = Q72,
         parlamento      = Q73,
         serv_civiles    = Q74,
         elecciones      = Q76,
         mov_ambiental   = Q79,
         mov_mujeres     = Q80,
         org_caritativas = Q81) |>
  drop_na()
# select(nuevo = original, ...): selecciona y renombra en un solo
# paso, usando los mismos nombres que ya conoces de la Sesión 4-5.
# drop_na(): elimina cualquier fila con NA en alguna de las 14
# columnas seleccionadas.

nrow(items_wvs)
# Confirma cuántos casos completos quedaron disponibles (~1,591).


# ------------------------------------------------------------
# 2. ESPECIFICACIÓN DEL MODELO
# ------------------------------------------------------------
# Esta es LA diferencia central con el AFE: en fa() nunca le
# dijimos a R qué ítem pertenecía a qué factor -- eso lo DESCUBRIÓ
# el análisis exploratorio. Aquí, en cambio, ESCRIBIMOS la
# hipótesis directamente en la sintaxis del modelo.
#
# Sintaxis de lavaan:
#   =~   significa "se mide a través de" / "está indicado por"
#        (el factor va a la izquierda, sus ítems observados a la
#        derecha, separados por "+")
#   ~~   (no se usa explícitamente aquí) especificaría covarianzas
#        entre variables si se necesitara -- ver Paso 5, índices
#        de modificación, donde sí aparece.

modelo_confianza <- '
  politica       =~ gobierno + partidos + parlamento + policia + tribunales +
                     serv_civiles + sindicatos + television + prensa +
                     elecciones + fuerzas_armadas
  sociedad_civil =~ org_caritativas + mov_mujeres + mov_ambiental
'
# El objeto modelo_confianza es una CADENA DE TEXTO (entre comillas
# simples), no código R ejecutable directamente -- lavaan tiene su
# propio "mini-lenguaje" para describir modelos, que luego cfa()
# interpreta.
#
# Nota importante: NO hace falta escribir una línea aparte para la
# correlación entre "politica" y "sociedad_civil" -- cfa() permite
# que los factores covaríen entre sí AUTOMÁTICAMENTE por defecto
# (a diferencia de fa(), donde había que elegir explícitamente
# rotate = 'oblimin' para permitir esa correlación).


# ------------------------------------------------------------
# 3. ESTIMACIÓN DEL MODELO
# ------------------------------------------------------------
# Los 14 ítems son ordinales de 4 categorías (1 = "mucha
# confianza" ... 4 = "ninguna confianza"), NO variables continuas
# -- el mismo problema que ya resolvimos en el AFE de WVS (Sesión
# 4-5) con polychoric() y cor = 'poly'. Aquí el equivalente es el
# argumento ordered.

fit_afc <- cfa(modelo_confianza,
               data = items_wvs,
               ordered = c("gobierno", "partidos", "parlamento", "policia",
                           "tribunales", "serv_civiles", "sindicatos",
                           "television", "prensa", "elecciones",
                           "fuerzas_armadas", "org_caritativas",
                           "mov_mujeres", "mov_ambiental"))
# Argumentos de cfa():
#   modelo_confianza: primer argumento, la especificación del modelo
#                      (el texto que escribiste en el Paso 2).
#   data:              la tabla de datos, items_wvs.
#   ordered:           vector con los nombres de las variables que
#                      deben tratarse como ORDINALES. Al incluir
#                      esto, cfa() automáticamente:
#                        (a) calcula correlaciones POLICÓRICAS en
#                            vez de Pearson, y
#                        (b) cambia el estimador de ML a WLSMV
#                            ("Weighted Least Squares Mean and
#                            Variance adjusted") -- el método
#                            recomendado para datos ordinales, que
#                            además no exige el supuesto de
#                            normalidad multivariada que si exige ML.
#
# Atajo: como los 14 ítems del modelo son EXACTAMENTE los mismos 14
# que se declaran en "ordered", se podría simplificar con
# ordered = TRUE (le pide a cfa() tratar TODOS los ítems del modelo
# como ordinales, sin escribirlos uno por uno). Aquí se dejó la
# lista completa por claridad didáctica.


# ------------------------------------------------------------
# 4. RESULTADOS: ÍNDICES DE AJUSTE Y CARGAS
# ------------------------------------------------------------
summary(fit_afc, fit.measures = TRUE, standardized = TRUE)
# Argumentos de summary():
#   fit_afc:            el objeto que acabas de estimar.
#   fit.measures=TRUE:  agrega a la salida CFI, TLI, RMSEA, SRMR y
#                        otros índices de ajuste global del modelo.
#   standardized=TRUE:  agrega una columna con las cargas
#                        ESTANDARIZADAS (comparables entre sí, en
#                        la misma escala -1 a 1 que ya conoces de
#                        fa()$loadings -- las cargas "crudas" sin
#                        estandarizar dependen de la escala
#                        original de cada ítem y son más difíciles
#                        de comparar entre variables).

# --- Umbrales convencionales para interpretar los índices ---
#   CFI / TLI  > 0.90 (aceptable)  / > 0.95 (bueno)
#   RMSEA      < 0.08 (aceptable)  / < 0.05 (bueno)
#   SRMR       < 0.08
# Ningún umbral es una regla matemática absoluta -- son
# convenciones ampliamente usadas en la literatura (Hu y Bentler,
# 1999, en tus lecturas complementarias del Tema 6), pero conviene
# interpretarlas junto con el contexto sustantivo, no de forma
# mecánica.

fitMeasures(fit_afc, c("cfi", "tli", "rmsea", "srmr"))
# fitMeasures(objeto, vector_de_nombres): extrae SOLO esos 4
# índices, sin el resto de la tabla completa de summary() -- útil
# para reportarlos de forma compacta en un artículo o tarea.

parameterEstimates(fit_afc, standardized = TRUE) |>
  filter(op == "=~")
# Alternativa a summary() para ver SOLO las cargas factoriales (no
# los índices de ajuste ni las varianzas). op == "=~" filtra las
# filas correspondientes a relaciones "factor mide ítem" (el mismo
# operador que usaste al escribir el modelo); existen otros
# operadores como "~~" (covarianzas) que aquí no interesan.


# ------------------------------------------------------------
# 5. DIAGNÓSTICO: ÍNDICES DE MODIFICACIÓN
# ------------------------------------------------------------
# Antes de modificar el modelo por intuición (por ejemplo,
# "quitemos el ítem con la carga más baja"), hay que preguntarle
# al propio modelo qué modificación específica mejoraría más el
# ajuste. Eso es justo lo que hace modindices().

modificaciones <- modindices(fit_afc, sort = TRUE)
# sort = TRUE: ordena el resultado de mayor a menor índice de
# modificación (columna "mi"), para ver primero los candidatos más
# influyentes.

head(modificaciones, 10)
# Muestra las primeras 10 filas (las de mayor "mi").
#
# Columnas más relevantes de esta tabla:
#   lhs, op, rhs: la relación candidata. Si op = "~~" entre dos
#                 ítems (por ejemplo, television ~~ prensa), sugiere
#                 permitir que esos dos ítems compartan varianza
#                 adicional MÁS ALLÁ de lo que ya comparten por
#                 pertenecer al mismo factor -- típicamente porque
#                 tienen algo en común entre sí (ambos son medios
#                 de comunicación) que el modelo actual no captura.
#   mi:           el índice de modificación -- cuánto BAJARÍA el
#                 chi-cuadrado del modelo si se agregara ese
#                 parámetro. Valores mayores a 10-11 suelen
#                 considerarse candidatos a revisar.
#
# IMPORTANTE -- disciplina metodológica: un índice de modificación
# alto es una PISTA, no una autorización automática para modificar
# el modelo. Cada cambio debe tener sentido TEÓRICO, no solo
# estadístico (recordar la cita de García, Jiménez y Rodríguez de
# la Sesión 4: "no hay factor sin variable que lo sature" -- lo
# mismo aplica en sentido inverso: no se agregan relaciones solo
# porque el modelo "las pide" numéricamente, sin justificación
# conceptual).


# ------------------------------------------------------------
# 6. COMPARACIÓN: ¿VALE LA PENA QUITAR "fuerzas_armadas"?
# ------------------------------------------------------------
# Ejercicio de comparación directa: mismo modelo, sin ese ítem (el
# de menor comunalidad en el AFE de la Sesión 4-5).

modelo_sin_fa <- '
  politica       =~ gobierno + partidos + parlamento + policia + tribunales +
                     serv_civiles + sindicatos + television + prensa + elecciones
  sociedad_civil =~ org_caritativas + mov_mujeres + mov_ambiental
'

fit_sin_fa <- cfa(modelo_sin_fa,
                   data = items_wvs,
                   ordered = TRUE)
# Aquí sí se puede usar ordered = TRUE directo (sin listar nombres),
# porque los 13 ítems del modelo_sin_fa son exactamente los únicos
# ítems presentes en esta versión del modelo.

fitMeasures(fit_sin_fa, c("cfi", "tli", "rmsea", "srmr"))
# Comparar manualmente contra fitMeasures(fit_afc, ...) del Paso 4.
#
# Pregunta de fondo: ¿la mejora en ajuste (si la hay) es lo
# suficientemente grande como para justificar sacrificar un ítem
# teóricamente relevante? No hay una regla automática -- es una
# decisión que combina evidencia estadística con criterio
# sustantivo. (Como referencia: al explorar este mismo ejercicio
# con un método aproximado, la diferencia entre ambos modelos
# resultó marginal -- ninguno de los dos alcanzó los umbrales de
# "buen ajuste" de forma holgada, lo que sugiere que el problema de
# ajuste NO está concentrado únicamente en fuerzas_armadas. Confirma
# esto con tus propios números exactos al correr este bloque.)

fitMeasures(fit_afc, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
fitMeasures(fit_sin_fa, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))

# ------------------------------------------------------------
# 7. CIERRE: DE AFE A AFC -- EL RECORRIDO COMPLETO
# ------------------------------------------------------------
# Con este resultado se cierra el arco narrativo completo del Tema
# 2: en las Sesiones 4-5 (AFE), sin ninguna restricción impuesta,
# el análisis DESCUBRIÓ que estos 14 ítems se organizan en 2
# factores (política y sociedad civil), con cargas limpias y una
# correlación moderada entre ambos (~0.51 con Phi). Aquí, en la
# Sesión 6 (AFC), se tomó esa misma estructura -- ahora como una
# hipótesis explícita -- y se puso a prueba formalmente contra los
# datos, obteniendo índices de ajuste que permiten evaluar si la
# teoría se sostiene.
#
print(fitMeasures(fit_afc, c('cfi','tli','rmsea','srmr')))
# -- compara este resultado contra fa_wvs$Phi de la Sesión 4-5 y
# discute en clase: ¿el AFC confirma lo que el AFE ya sugería, o
# revela algo que el AFE no podía ver?

modelo_confianza_v2 <- '
  politica       =~ gobierno + partidos + parlamento + policia + tribunales +
                     serv_civiles + sindicatos + television + prensa +
                     elecciones + fuerzas_armadas
  sociedad_civil =~ org_caritativas + mov_mujeres + mov_ambiental

  television ~~ prensa
'

fit_afc_v2 <- cfa(modelo_confianza_v2,
                  data = items_wvs,
                  ordered = TRUE)
# ordered = TRUE funciona directo aquí porque los 14 ítems del
# modelo son exactamente los mismos 14 que ya declarabas uno por
# uno en fit_afc -- no hace falta volver a escribir la lista completa.

summary(fit_afc_v2, fit.measures = TRUE, standardized = TRUE)
# Revisa aquí, en la sección "Covariances", que aparezca la nueva
# línea "television ~~ prensa" con su estimación -- confirma que
# el modelo sí incorporó la covarianza que agregaste.

fitMeasures(fit_afc_v2, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
# LA comparación que responde la pregunta de fondo: compárala
# directamente contra:
#   fit_afc (modelo original):     CFI=0.858  TLI=0.830  RMSEA=0.126
#   fit_sin_fa (sin fuerzas_armadas): CFI=0.868  TLI=0.840  RMSEA=0.128

# ------------------------------------------------------------
# LIMPIEZA DE SESIÓN
# ------------------------------------------------------------
rm(list = ls())
graphics.off()


# ------------------------------------------------------------
# LIMPIEZA DE SESIÓN (al terminar de trabajar)
# ------------------------------------------------------------
rm(list = ls())
graphics.off()
