# ============================================================
# TEMA 1 - Análisis de Componentes Principales (PCA)
# Curso: Análisis multivariado y métodos de clasificación de datos con R
# El Colegio de México - Centro de Estudios Sociológicos
# Profesor: Isaac Cisneros Yescas
# ============================================================
# Ver manual completo (código explicado línea por línea) en:
# Manual_Tema1_PCA_en_R.docx (en esta misma carpeta)
# ============================================================

library(tidyverse)
library(psych)
library(factoextra)
library(FactoMineR)

# ------------------------------------------------------------
# SECCIÓN A: Índice sumatorio simple
# ------------------------------------------------------------
# Nota: Los datos simulados de confianza institucional replican
# la estructura de los ítems B1-B21 de LAPOP.

set.seed(123)
n <- 500
datos <- tibble(
  conf_ejecutivo = sample(1:7, n, replace = TRUE),
  conf_legislativo = sample(1:7, n, replace = TRUE),
  conf_judicial = sample(1:7, n, replace = TRUE),
  conf_partidos = sample(1:7, n, replace = TRUE),
  conf_electoral = sample(1:7, n, replace = TRUE)
)

# Índice sumatorio simple (promedio de ítems)
datos <- datos |>
  mutate(indice_suma = rowMeans(across(starts_with("conf"))))

summary(datos$indice_suma)
hist(datos$indice_suma, main = 'Distribución del índice', xlab = 'Valor')

# ------------------------------------------------------------
# SECCIÓN B: PCA básico y scree plot
# ------------------------------------------------------------
# IMPORTANTE: siempre escalar antes del PCA (scale. = TRUE)

pca_base <- prcomp(datos |>
                      select(starts_with('conf')),
                    scale. = TRUE)

summary(pca_base)

# Scree plot
varianza <- pca_base$sdev^2 / sum(pca_base$sdev^2)

tibble(componente = 1:5,
       varianza_pct = varianza * 100,
       acumulada = cumsum(varianza * 100)) |>
  ggplot(aes(x = componente, y = varianza_pct)) +
  geom_col(fill = "#2C3E50") +
  geom_line(aes(y = acumulada), color = 'red', linewidth = 1) +
  geom_point(aes(y = acumulada), color = 'red', size = 3) +
  labs(title = 'Scree Plot: varianza explicada por componente',
       x = 'Componente', y = '% Varianza') +
  theme_minimal()

##########################
##### SESIÓN 1A.1 ##########
##########################

# ------------------------------------------------------------
# 0. PAQUETES (todos al inicio, sin excepción)
# ------------------------------------------------------------
library(tidyverse)   # manipulación de datos y gráficos (incluye dplyr, ggplot2, tibble)
library(psych)       # PCA con rotación, KMO, Bartlett, corr.test()
library(GPArotation) # requerido por psych para rotaciones oblicuas (oblimin)
library(FactoMineR)  # PCA con salidas enriquecidas
library(factoextra)  # visualización de PCA (scree plot, biplot, contribuciones)
library(corrplot)    # visualización de matrices de correlación
library(haven)       # lectura de archivos de Stata (.dta)

# Nota: Hmisc NO se carga con library() -- se llama directamente con
# Hmisc::rcorr() más abajo. Cargar Hmisc junto con tidyverse puede
# "enmascarar" funciones de dplyr (como summarize()), así que es más
# seguro usarlo puntualmente sin cargarlo por completo.


# ============================================================
# BLOQUE 1: DATOS SIMULADOS CON ESTRUCTURA FACTORIAL
# ============================================================

set.seed(456)
n <- 500
factor_comun <- rnorm(n)

datos_correlacionados <- tibble(
  conf_ejecutivo    = 0.75*factor_comun + rnorm(n, 0, 0.5),
  conf_legislativo  = 0.70*factor_comun + rnorm(n, 0, 0.5),
  conf_judicial     = 0.65*factor_comun + rnorm(n, 0, 0.5),
  conf_partidos     = 0.68*factor_comun + rnorm(n, 0, 0.5),
  conf_electoral    = 0.72*factor_comun + rnorm(n, 0, 0.5)
)

# --- Correlación y significancia ---
cor(datos_correlacionados)

resultado_sim <- corr.test(datos_correlacionados)
resultado_sim$r   # matriz de coeficientes de correlación
resultado_sim$p   # matriz de valores p (significancia)

corrplot(cor(datos_correlacionados),
         method = 'color', type = 'upper', addCoef.col = 'black',
         tl.col = 'black', tl.srt = 45,
         title = 'Correlación entre ítems (datos con estructura factorial)',
         mar = c(0,0,1,0))

# --- Adecuación muestral ---
KMO(datos_correlacionados)
cortest.bartlett(cor(datos_correlacionados), n = nrow(datos_correlacionados))

# --- PCA con psych (rotación) ---
# Un solo objeto, con el número de factores que decidiste usar (2):
pca_psych_sim <- principal(datos_correlacionados, nfactors = 2, rotate = 'none')
print(pca_psych_sim$loadings, cutoff = 0.3)
print(pca_psych_sim$loadings)

# --- PCA con FactoMineR (para scree plot y biplot) ---
pca_fm_sim <- PCA(datos_correlacionados, scale.unit = TRUE, ncp = 5, graph = FALSE)
pca_fm_sim$eig

# SCREE PLOT (versión correcta -- una sola línea, con fviz_eig):
fviz_eig(pca_fm_sim, addlabels = TRUE,
         main = 'Scree Plot: datos con estructura factorial')

fviz_pca_biplot(pca_fm_sim,
                repel = TRUE, col.var = "#E74C3C", col.ind = "#BDC3C7",
                label = "var",
                title = 'Biplot PCA: datos con estructura factorial')

fviz_contrib(pca_fm_sim, choice = 'var', axes = 1)
fviz_contrib(pca_fm_sim, choice = 'var', axes = 2)


# ============================================================
# BLOQUE 2: DATOS REALES -- LAPOP MÉXICO 2023
# ============================================================

getwd()

# Ajusta la ruta según donde tengas guardado el archivo.
# OJO: el nombre real del archivo usa guion bajo (v1_0), no punto (v1.0).
lapop <- read_dta('/Users/isaaccisneros/Desktop/2023_MEX_2023_LAPOP_AmericasBarometer_v1.0_w.dta')


items_lapop <- lapop |>
  select(conf_ejecutivo   = b21a,
         conf_legislativo = b13,
         conf_judicial    = b31,
         conf_partidos    = b21,
         conf_electoral   = b47a) |>
  drop_na()

nrow(items_lapop)

# --- Correlación y significancia ---
cor(items_lapop)

resultado_lapop <- corr.test(items_lapop)
resultado_lapop$r
resultado_lapop$p

corrplot(cor(items_lapop),
         method = 'color', type = 'upper', addCoef.col = 'black',
         tl.col = 'black', tl.srt = 45,
         title = 'Correlación entre ítems de confianza institucional (LAPOP México 2023)',
         mar = c(0,0,1,0))

# --- Adecuación muestral ---
KMO(items_lapop)
cortest.bartlett(cor(items_lapop), n = nrow(items_lapop))

# --- PCA con psych (SIN rotación), objeto separado del de FactoMineR ---
pca_psych_lapop <- principal(items_lapop, nfactors = 2, rotate = 'none')
print(pca_psych_lapop$loadings)
print(pca_psych_lapop$loadings, cutoff = 0)

# --- PCA con FactoMineR (para eigenvalues, scree plot y biplot) ---
pca_fm_lapop <- PCA(items_lapop, scale.unit = TRUE, ncp = 5, graph = FALSE)
pca_fm_lapop$eig

# SCREE PLOT (versión correcta -- una sola línea, con fviz_eig):
fviz_eig(pca_fm_lapop, addlabels = TRUE, main = 'Scree Plot: LAPOP México 2023')

fviz_pca_biplot(pca_fm_lapop,
                repel = TRUE, label = "var",
                col.var = "#E74C3C", col.ind = "#BDC3C7",
                title = 'Biplot PCA: Confianza institucional (LAPOP México 2023)')

fviz_contrib(pca_fm_lapop, choice = 'var', axes = 1)
fviz_contrib(pca_fm_lapop, choice = 'var', axes = 2)

##PCA con psych (SIN rotación), objeto separado del de FactoMineR ---
pca_psych_lapop <- principal(items_lapop, nfactors = 2, rotate = 'none')
print(pca_psych_lapop$loadings)
print(pca_psych_lapop$loadings, cutoff = 0.3)

cor(pca_psych_lapop$scores)

plot(pca_psych_lapop$loadings,
     xlim = c(-1, 1), ylim = c(-1, 1),
     main = 'Cargas sin rotación (LAPOP)', xlab = 'PC1', ylab = 'PC2')
text(pca_psych_lapop$loadings, labels = rownames(pca_psych_lapop$loadings), pos = 3, col = 'red')
abline(h = 0, v = 0, lty = 2)

# --- PCA con psych: rotación VARIMAX (ortogonal) ---
pca_psych_lapop_varimax <- principal(items_lapop, nfactors = 2, rotate = 'varimax')
print(pca_psych_lapop_varimax$loadings)
print(pca_psych_lapop_varimax$loadings, cutoff = 0.3)

plot(pca_psych_lapop_varimax$loadings,
     xlim = c(-1, 1), ylim = c(-1, 1),
     main = 'Cargas rotación varimax (LAPOP)', xlab = 'RC1', ylab = 'RC2')
text(pca_psych_lapop_varimax$loadings, labels = rownames(pca_psych_lapop_varimax$loadings), pos = 3, col = 'red')
abline(h = 0, v = 0, lty = 2)

# --- PCA con psych: rotación OBLIMIN (oblicua) ---
pca_psych_lapop_oblimin <- principal(items_lapop, nfactors = 2, rotate = 'oblimin')
print(pca_psych_lapop_oblimin$loadings)
print(pca_psych_lapop_oblimin$loadings, cutoff = 0.3)
pca_psych_lapop_oblimin$Phi  # Correlación interfactorial (solo aplica a rotaciones oblicuas)

plot(pca_psych_lapop_oblimin$loadings,
     xlim = c(-1, 1), ylim = c(-1, 1),
     main = 'Cargas rotación oblimin (LAPOP)', xlab = 'TC1', ylab = 'TC2')
text(pca_psych_lapop_oblimin$loadings, labels = rownames(pca_psych_lapop_oblimin$loadings), pos = 3, col = 'red')
abline(h = 0, v = 0, lty = 2)
# Nota: con oblimin, psych a veces nombra los componentes TC1/TC2 en vez de
# RC1/RC2 -- verifica en tu consola cómo salieron y ajusta xlab/ylab si hace falta.



# ------------------------------------------------------------
# LIMPIEZA DE SESIÓN (solo al final, cuando ya terminaste de trabajar)
# ------------------------------------------------------------
# rm(list = ls())
# graphics.off()
