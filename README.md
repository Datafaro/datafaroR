
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datafaro <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/datafaro/datafaroR/branch/master/graph/badge.svg)](https://codecov.io/gh/datafaro/datafaroR?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/datafaro)](https://CRAN.R-project.org/package=datafaro)
[![R-CMD-check](https://github.com/Datafaro/datafaroR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Datafaro/datafaroR/actions/workflows/R-CMD-check.yaml)
![r-universe](https://adatar-do.r-universe.dev/badges/datafaro)
<!-- badges: end -->

> DataFaro ofrece acceso a datos recurrentes de una variedad de fuentes.
> Abstrae las preocupaciones relacionadas con la frecuencia de los
> datos, el formato y el calendario de publicación. Este paquete
> proporciona un envoltorio conveniente para interactuar con la API de
> DataFaro, facilitando a los usuarios de R el aprovechamiento del poder
> de DataFaro.

## Instalación

Puedes instalar `datafaro` desde el repositorio r-universe de Adatar:

``` r
install.packages("datafaro", repos = c("https://adatar-do.r-universe.dev", "https://cran.r-project.org"))
```

## Ejemplo

Aquí tienes un ejemplo básico para comenzar con datafaro:

``` r
library(datafaro)
get_data('NC_XDC')
#> # A tibble: 132 × 2
#>    date        value
#>    <date>      <dbl>
#>  1 1991-03-01 25592.
#>  2 1991-06-01 25884.
#>  3 1991-09-01 26270.
#>  4 1991-12-01 28863.
#>  5 1992-03-01 29160.
#>  6 1992-06-01 30660.
#>  7 1992-09-01 31504.
#>  8 1992-12-01 36430.
#>  9 1993-03-01 32681.
#> 10 1993-06-01 34340.
#> # ℹ 122 more rows
#> Datafaro's datalight: NC_XDC
#> Full name: PIB Gasto > Consumo final total nominal (RD$)
#> Cached: 2024-04-25
#> Next update: 2024-06-11T22:20:08-04:00
```

**Nota:** Si ejecutas esto en un notebook (RMarkdown o Quarto), presta
atención a la consola para obtener instrucciones sobre cómo
autenticarte.

<!-- Testing the package
```r
docker build -t datafaror .
docker run -it datafaror
``` 
-->
