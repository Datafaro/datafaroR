
<!-- README.md is generated from README.Rmd. Please edit that file -->

# domar <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![R build
status](https://github.com/drdsdaniel/domar/workflows/R-CMD-check/badge.svg)](https://github.com/drdsdaniel/domar/actions)
<!-- badges: end -->

Este paquete provee funciones que obtienen archivos de datos de la web y
los devuelve en formato tabular. También proporciona funciones para
almacenar estos datos en Google Sheets o una base de datos local de
forma programática.

## Instalación

domar no es apto para CRAN.

Puedes instalar la versión de desarrollo desde
[GitHub](https://github.com/) con:

``` r
# install.packages("devtools")
devtools::install_github("drdsdaniel/domar", build_vignettes=TRUE)
```

Si tienes un archivo binario, lo cual es muy probable, instálalo.

## Comenzando

Una vez has instalado el paquete ejecuta `browseVignettes('domar')` en
la consola de comando de R para ver más información relevante.

<!-- Asegurate de tener una sola instancia corriendo al mismo tiempo en el cron. O aún mejor, que cada instancia sea la que crea un nuevo cron al salir -->
