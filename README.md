
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datafaro <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/datafaro/datafaroR/workflows/R-CMD-check/badge.svg)](https://github.com/datafaro/datafaroR/actions)
[![Codecov test
coverage](https://codecov.io/gh/datafaro/datafaroR/branch/master/graph/badge.svg)](https://codecov.io/gh/datafaro/datafaroR?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/datafaro)](https://CRAN.R-project.org/package=datafaro)
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
install.packages('datafaro', repos = 'https://adatar-do.r-universe.dev')
```

## Ejemplo

Aquí tienes un ejemplo básico para comenzar con datafaro:

``` r
library(datafaro)

list_indicators(area = 'ECONREAL')
#>           id                                                  name description
#> 1       imae       Indicador Mensual de Actividad Económica (IMAE)         ...
#> 2  pib-gasto Producto Interno Bruto (PIB) por el enfoque del gasto         ...
#> 3 pib-origen   Producto Interno Bruto (PIB) por sectores de origen         ...
#>       area
#> 1 ECONREAL
#> 2 ECONREAL
#> 3 ECONREAL

list_datasets(area = 'ECONREAL')
#>                                       id  indicator institution
#> 1                imae-desestacionalizado       imae        BCRD
#> 2                          imae-original       imae        BCRD
#> 3                    imae-tedencia-ciclo       imae        BCRD
#> 4   pib-origen-incidencia-acumulado-2007 pib-origen        BCRD
#> 5    pib-gasto-incidencia-acumulado-2007  pib-gasto        BCRD
#> 6              pib-gasto-incidencia-2007  pib-gasto        BCRD
#> 7             pib-origen-incidencia-2007 pib-origen        BCRD
#> 8           pib-gasto-ive-acumulado-2007  pib-gasto        BCRD
#> 9          pib-origen-ive-acumulado-2007 pib-origen        BCRD
#> 10                    pib-gasto-ive-2007  pib-gasto        BCRD
#> 11                   pib-origen-ive-2007 pib-origen        BCRD
#> 12 pib-origen-ponderacion-acumulado-2007 pib-origen        BCRD
#> 13  pib-gasto-ponderacion-acumulado-2007  pib-gasto        BCRD
#> 14           pib-origen-ponderacion-2007 pib-origen        BCRD
#> 15            pib-gasto-ponderacion-2007  pib-gasto        BCRD
#> 16          pib-origen-tc-acumulado-2007 pib-origen        BCRD
#> 17           pib-gasto-tc-acumulado-2007  pib-gasto        BCRD
#> 18                     pib-gasto-tc-2007  pib-gasto        BCRD
#> 19                    pib-origen-tc-2007 pib-origen        BCRD
#> 20      pib-gasto-nominal-acumulado-2007  pib-gasto        BCRD
#> 21     pib-origen-nominal-acumulado-2007 pib-origen        BCRD
#> 22               pib-origen-nominal-2007 pib-origen        BCRD
#> 23                pib-gasto-nominal-2007  pib-gasto        BCRD
#>                                                            name description
#> 1                                 IMAE Serie Desestacionalizada         ...
#> 2                                           IMAE Serie Original         ...
#> 3                                    IMAE Serie Tendencia-Ciclo         ...
#> 4           Incidencia IVE acumulado por componente (base 2007)         ...
#> 5           Incidencia IVE acumulado por componente (base 2007)         ...
#> 6                     Incidencia IVE por componente (base 2007)         ...
#> 7                     Incidencia IVE por componente (base 2007)         ...
#> 8    Índices de Volumen Encadenados (IVE) acumulado (base 2007)         ...
#> 9    Índices de Volumen Encadenados (IVE) acumulado (base 2007)         ...
#> 10             Índices de Volumen Encadenados (IVE) (base 2007)         ...
#> 11             Índices de Volumen Encadenados (IVE) (base 2007)         ...
#> 12             Ponderación por componente acumulada (base 2007)         ...
#> 13             Ponderación por componente acumulada (base 2007)         ...
#> 14                       Ponderacion por componente (base 2007)         ...
#> 15                       Ponderacion por componente (base 2007)         ...
#> 16 Tasa de crecimiento IVE acumulado por componente (base 2007)         ...
#> 17 Tasa de crecimiento IVE acumulado por componente (base 2007)         ...
#> 18           Tasa de crecimiento IVE por componente (base 2007)         ...
#> 19           Tasa de crecimiento IVE por componente (base 2007)         ...
#> 20                          Valor nominal acumulado (base 2007)         ...
#> 21                          Valor nominal acumulado (base 2007)         ...
#> 22                                    Valor nominal (base 2007)         ...
#> 23                                    Valor nominal (base 2007)         ...
#>    status                      last_update                      next_update
#> 1       A 2024-01-03T08:57:53.269010-04:00 2024-01-31T20:57:53.268979-04:00
#> 2       A 2024-01-03T09:05:53.380853-04:00 2024-01-31T21:05:53.380831-04:00
#> 3       A 2024-01-03T09:07:53.279877-04:00 2024-01-31T21:07:53.279862-04:00
#> 4       A                             NULL 2024-01-04T01:11:53.290734-04:00
#> 5       A                             NULL        2024-01-04T01:14:53-04:00
#> 6       A                             NULL 2024-01-04T01:25:53.285891-04:00
#> 7       A 2024-01-03T09:19:53.383051-04:00 2024-03-28T21:19:53.383024-04:00
#> 8       A                             NULL 2024-01-04T01:37:53.189194-04:00
#> 9       A 2024-01-03T09:36:53.321087-04:00 2024-03-28T21:36:53.321069-04:00
#> 10      A 2024-01-03T09:41:53.275367-04:00 2024-03-28T21:41:53.275342-04:00
#> 11      A 2024-01-03T09:39:53.571136-04:00 2024-03-28T21:39:53.571112-04:00
#> 12      A 2024-01-03T09:49:53.406109-04:00 2024-03-28T21:49:53.406084-04:00
#> 13      A 2024-01-03T09:53:53.210197-04:00 2024-03-28T21:53:53.210183-04:00
#> 14      A 2024-01-03T09:56:53.433427-04:00 2024-03-28T21:56:53.433405-04:00
#> 15      A 2024-01-03T09:57:53.292347-04:00 2024-03-28T21:57:53.292330-04:00
#> 16      A 2024-01-03T09:58:53.404192-04:00 2024-03-28T21:58:53.404167-04:00
#> 17      A                             NULL 2024-01-04T01:59:53.181702-04:00
#> 18      A                             NULL 2024-01-04T02:01:53.193628-04:00
#> 19      A 2024-01-03T10:00:53.347051-04:00 2024-03-28T22:00:53.347033-04:00
#> 20      A 2024-01-03T10:06:53.277705-04:00 2024-03-28T22:06:53.277690-04:00
#> 21      A 2024-01-03T10:05:53.355480-04:00 2024-01-03T17:09:53.395643-04:00
#> 22      A 2024-01-03T10:08:53.480933-04:00 2024-03-28T22:08:53.480916-04:00
#> 23      A 2024-01-03T10:09:53.201126-04:00 2024-03-28T22:09:53.201103-04:00
#>    frequency interval
#> 1          M        1
#> 2          M        1
#> 3          M        1
#> 4          M        3
#> 5          M        3
#> 6          M        3
#> 7          M        3
#> 8          M        3
#> 9          M        3
#> 10         M        3
#> 11         M        3
#> 12         M        3
#> 13         M        3
#> 14         M        3
#> 15         M        3
#> 16         M        3
#> 17         M        3
#> 18         M        3
#> 19         M        3
#> 20         M        3
#> 21         M        3
#> 22         M        3
#> 23         M        3

get_data(.dataset = 'imae-original')
#> # A tibble: 203 × 5
#>    date        imae imae__tci imae__tca imae__tcp
#>    <date>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1 2007-01-01  94.0        NA        NA        NA
#>  2 2007-02-01  96.9        NA        NA        NA
#>  3 2007-03-01 101.         NA        NA        NA
#>  4 2007-04-01  95.7        NA        NA        NA
#>  5 2007-05-01 103.         NA        NA        NA
#>  6 2007-06-01  98.0        NA        NA        NA
#>  7 2007-07-01 101.         NA        NA        NA
#>  8 2007-08-01 102.         NA        NA        NA
#>  9 2007-09-01  97.7        NA        NA        NA
#> 10 2007-10-01 103.         NA        NA        NA
#> # ℹ 193 more rows
```
