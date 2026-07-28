# Análisis multivariado y métodos de clasificación de datos con R

Código y manuales elaborados durante el curso **"Análisis multivariado y métodos de clasificación de datos con R"**, El Colegio de México, Centro de Estudios Sociológicos (Maestría y Doctorado en Ciencia Social con especialidad en Sociología), semestre agosto–diciembre 2026. Profesor: Isaac Cisneros Yescas.

Este repositorio se actualiza sesión por sesión conforme avanza el curso.

## Contenido

| Tema | Técnica | Carpeta |
|---|---|---|
| 1 | Análisis de Componentes Principales (PCA) | [`Tema_1_PCA/`](./Tema_1_PCA) |
| 2 | Análisis Factorial Exploratorio | próximamente |
| 3 | Análisis de Conglomerados | próximamente |
| 4 | MDS y Análisis de Correspondencias | próximamente |
| 5 | Análisis de Clases Latentes | próximamente |
| 6 | Modelos de Ecuaciones Estructurales (SEM) | próximamente |

Cada carpeta de tema incluye:
- El script de R (`.R`) con el código completo y comentado.
- Un manual en Word (`.docx`) con el código explicado línea por línea, los resultados obtenidos y las lecciones clave.

## Paquetes de R utilizados en el curso

```r
install.packages(c(
  "tidyverse", "FactoMineR", "factoextra", "psych", "GPArotation",
  "corrplot", "cluster", "NbClust", "dendextend", "MASS",
  "poLCA", "tidyLPA", "lavaan"
))
```

## Sobre este repositorio

Líneas de investigación relacionadas: ideología política, desafección política y partidista, comportamiento político y opinión pública.

## Cómo citar

Si utiliza este repositorio, por favor cítelo de la siguiente manera:

> Cisneros-Yescas, I. (2026). *Análisis multivariado y métodos de clasificación de datos con R*. GitHub. https://github.com/IcisnerosY/analisis-multivariado-r

ORCID: [0000-0001-9905-0777](https://orcid.org/0000-0001-9905-0777)

También puede usar el archivo [`CITATION.cff`](./CITATION.cff) incluido en este repositorio, que GitHub reconoce automáticamente y muestra en un botón **"Cite this repository"** en la barra lateral derecha de la página del repo.
