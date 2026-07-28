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

# ------------------------------------------------------------
# SECCIÓN C: PCA con FactoMineR y rotación con psych
# ------------------------------------------------------------
# Nota: La decisión entre varimax y oblimin es teórica: si los
# factores pueden estar correlacionados (lo habitual en actitudes
# políticas), oblimin es más apropiado.

pca_fm <- PCA(datos |>
                select(starts_with('conf')),
              scale.unit = TRUE,
              ncp = 5,
              graph = FALSE)

# Eigenvalues
pca_fm$eig

# Contribución de variables al PC1
fviz_contrib(pca_fm, choice = 'var', axes = 1)

# Biplot
fviz_pca_biplot(pca_fm,
                repel = TRUE,
                col.var = "#E74C3C",
                col.ind = "#BDC3C7",
                label = "var",
                title = 'Biplot PCA: Confianza institucional')

# --- Rotación con psych ---

# Sin rotación
pca_psych <- principal(datos |>
                          select(starts_with('conf')),
                        nfactors = 2, rotate = 'none')
print(pca_psych$loadings, cutoff = 0.3)

# Rotación varimax (ortogonal)
pca_varimax <- principal(datos |>
                            select(starts_with('conf')),
                          nfactors = 2, rotate = 'varimax')
print(pca_varimax$loadings, cutoff = 0.3)

# Rotación oblimin (oblicua - permite correlación entre factores)
pca_oblimin <- principal(datos |>
                            select(starts_with('conf')),
                          nfactors = 2, rotate = 'oblimin')
print(pca_oblimin$loadings, cutoff = 0.3)
pca_oblimin$Phi  # Correlación interfactorial

# ------------------------------------------------------------
# EJERCICIO ADICIONAL: PCA con datos que SÍ tienen estructura
# factorial real (comparación pedagógica)
# ------------------------------------------------------------

set.seed(456)
n <- 500
factor_comun <- rnorm(n)  # el "factor latente" de confianza institucional

datos_correlacionados <- tibble(
  conf_ejecutivo    = 0.75*factor_comun + rnorm(n, 0, 0.5),
  conf_legislativo  = 0.70*factor_comun + rnorm(n, 0, 0.5),
  conf_judicial     = 0.65*factor_comun + rnorm(n, 0, 0.5),
  conf_partidos     = 0.68*factor_comun + rnorm(n, 0, 0.5),
  conf_electoral    = 0.72*factor_comun + rnorm(n, 0, 0.5)
)

pca_correlacionado <- principal(datos_correlacionados, nfactors = 1, rotate = 'none')
print(pca_correlacionado$loadings, cutoff = 0.3)

# PCA completo con prcomp (para scree plot y biplot)
pca_correlacionado_completo <- prcomp(datos_correlacionados, scale. = TRUE)

varianza_corr <- pca_correlacionado_completo$sdev^2 / sum(pca_correlacionado_completo$sdev^2)

tibble(componente = 1:5,
       varianza_pct = varianza_corr * 100,
       acumulada = cumsum(varianza_corr * 100)) |>
  ggplot(aes(x = componente, y = varianza_pct)) +
  geom_col(fill = "#2C3E50") +
  geom_line(aes(y = acumulada), color = 'red', linewidth = 1) +
  geom_point(aes(y = acumulada), color = 'red', size = 3) +
  labs(title = 'Scree Plot: datos CON estructura factorial',
       x = 'Componente', y = '% Varianza') +
  theme_minimal()

pca_fm_corr <- PCA(datos_correlacionados,
                    scale.unit = TRUE,
                    ncp = 5,
                    graph = FALSE)

fviz_pca_biplot(pca_fm_corr,
                repel = TRUE,
                col.var = "#E74C3C",
                col.ind = "#BDC3C7",
                label = "var",
                title = 'Biplot PCA: datos CON estructura factorial')

fviz_contrib(pca_fm_corr, choice = 'var', axes = 1)

# ------------------------------------------------------------
# Limpieza de sesión (opcional, al terminar de trabajar)
# ------------------------------------------------------------
# rm(list = ls())
# graphics.off()
