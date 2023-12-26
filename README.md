
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gateway <img src="man/figures/gatewayLogo.png" align="right" />

[![lifecycle](https://img.shields.io/badge/lifecycle-deprecated-orange.svg)](https://www.tidyverse.org/lifecycle/#deprecated)
[![Travis-CI Build
Status](https://travis-ci.org/slu-openGIS/gateway.svg?branch=master)](https://travis-ci.org/slu-openGIS/gateway)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/slu-openGIS/gateway?branch=master&svg=true)](https://ci.appveyor.com/project/chris-prener/gateway)
[![codecov](https://codecov.io/gh/slu-openGIS/gateway/branch/master/graph/badge.svg)](https://codecov.io/gh/slu-openGIS/gateway)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/gateway)](https://cran.r-project.org/package=gateway)

The `gateway` package contains tools for manipulating spatial data for
the City of St. Louis. It was never published, though it was described in
the following publication:

```r
Prener, Christopher G., and Branson Fox. "Creating open source composite geocoders: Pitfalls and opportunities." Transactions in GIS 25.4 (2021): 1868-1887.
```

## Installation

You can install gateway from Github with the `remotes` package:

``` r
remotes::install_github("slu-openGIS/gateway")
```

## Usage

### Data Access

`gateway` contains a function, `gw_get_data()`, that provides access to
18 data sets hosted by the City of St. Louis, the Saint Louis University
openGIS Project, and (via `tigris`) the U.S. Census Bureau. This
provides a “one stop shop” for downloading and accessing data about
St. Louis.

``` r
> # download census tracts (via tigris)
> tracts <- gw_get_data(data = "Tracts", class = "sf")
>
> # download neighborhoods (via the City of St. Louis)
> nhoods <- gw_get_data(data = "Neighborhoods", class = "tibble")
```

### Geocoding

Along with its cousin
[`postmastr`](https:://slu-openGIS.github.io/postmastr/), `gateway` can
be used to gecode data. In order for the local geocoder to work
correctly, data needed to be parsed and standardized ahead of time. The
package contains a data set, `sushi`, that is the same as `sushi2` in
the `postmastr` package but has already been processed and is ready for
geocoding:

``` r
> sushi <- sushi
```

Next, we need to build our local geocoder with `gw_build_geocoder()`:

``` r
> geocoder <- gw_build_geocoder(class = "sf")
```

This can take a short period of time, both because it will download a
fresh master address list from the City of St. Louis (~ 7.8MB in size)
and because the size of this list, approximately 330,000 addresses. Once
the function finishes executing, we can move on to geocoding our data

``` r
> # geocode
> sushi_sf <- gw_geocode(sushi, type = "local", class = "sf", 
+                        address = "address", geocoder = geocoder)
```

Since we requested `sf` output with the `class` argument, our data are
returned both with the unique address identification number that the
City of St. Louis uses and the coordinate in the `geometry` column of
our `sf` object:

``` r
> sushi_sf
Simple feature collection with 15 features and 4 fields
geometry type:  POINT
dimension:      XY
bbox:           xmin: -90.26318 ymin: 38.5984 xmax: -90.19386 ymax: 38.645
epsg (SRID):    4269
proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs
# A tibble: 15 x 5
   name                            address              visit    addrrecnum             geometry
   <chr>                           <chr>                <chr>    <chr>               <POINT [°]>
 1 BaiKu Sushi Lounge              3407 Olive St        3/20/18  10022116    (-90.2282 38.63671)
 2 Cafe Mochi                      3221 S Grand Blvd    10/10/18 10049492    (-90.24307 38.5984)
 3 Drunken Fish - Central West End 1 Maryland Plz       12/2/18  10279312     (-90.26181 38.645)
 4 Kampai Sushi Bar                4949 W Pine Blvd     2/13/18  10158461   (-90.26318 38.64242)
 5 Midtown Sushi & Ramen           3674 Forest Park Ave 3/4/18   10173725   (-90.23708 38.63332)
 6 Mizu Sushi Bar                  1013 Washington Ave  9/12/18  10015227   (-90.19418 38.63163)
 7 Sapporo 2                       3043 Olive St        3/1/18   10022067   (-90.22336 38.63539)
 8 Sapporo 2                       3043 Olive St        7/3/18   10022067   (-90.22336 38.63539)
 9 Sub Zero Vodka Bar              308 N Euclid Ave     12/7/18  10096925     (-90.26088 38.645)
10 Sushi Ai                        910 Olive St         3/29/18  10021018   (-90.19386 38.62869)
11 Sushi Ai                        910 Olive St         5/20/18  10021018   (-90.19386 38.62869)
12 Sushi Ai                        910 Olive St         6/17/18  10021018   (-90.19386 38.62869)
13 Sushi Ai                        910 Olive St         8/25/18  10021018   (-90.19386 38.62869)
14 Sushi Ai                        910 Olive St         10/30/18 10021018   (-90.19386 38.62869)
15 SUSHI KOI                       4 N Euclid Ave       1/17/18  10096866   (-90.26177 38.64049)
```

With the data converted to a `sf` object, they can be mapped using a
variety of tools (including `ggplot2`, `tmap`, and `leaflet`) or further
geoprocessed (see the next section).

### Spatial Joins

The `gateway` package contains data for further modifying point data
from St. Louis. There are two primary verbs, `gw_identify()` and
`gw_aggregate()`. The `gw_identify()` function applies identifiers from
an areal unit to the point data, while the `gw_aggregate()` function
produces counts of points per feature of the areal unit.

#### Identify Points

If we wanted to retain our geocoded point data, but add identifiers from
one of several geographies, we can use `gw_identify()`. For example, we
can add Census tract identifiers (`GEOID`) to our data:

``` r
> gw_identify(sushi_sf, to = "tract")
Point data transformed to Missouri State Plane East NAD83 (2011) for spatial join.
Simple feature collection with 15 features and 5 fields
geometry type:  POINT
dimension:      XY
bbox:           xmin: 270616.6 ymin: 306882.3 xmax: 276655.9 ymax: 312051.7
epsg (SRID):    6512
proj4string:    +proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.999933333 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs
# A tibble: 15 x 6
   name                            address              visit    addrrecnum GEOID                  geometry
   <chr>                           <chr>                <chr>    <chr>      <chr>               <POINT [m]>
 1 BaiKu Sushi Lounge              3407 Olive St        3/20/18  10022116   29510121100 (273663.4 311138.8)
 2 Cafe Mochi                      3221 S Grand Blvd    10/10/18 10049492   29510116301   (272381 306882.3)
 3 Drunken Fish - Central West End 1 Maryland Plz       12/2/18  10279312   29510112400 (270734.8 312050.5)
 4 Kampai Sushi Bar                4949 W Pine Blvd     2/13/18  10158461   29510112400 (270616.6 311764.6)
 5 Midtown Sushi & Ramen           3674 Forest Park Ave 3/4/18   10173725   29510118600   (272891.3 310760)
 6 Mizu Sushi Bar                  1013 Washington Ave  9/12/18  10015227   29510125600 (276626.9 310583.9)
 7 Sapporo 2                       3043 Olive St        3/1/18   10022067   29510121100 (274085.6 310994.1)
 8 Sapporo 2                       3043 Olive St        7/3/18   10022067   29510121100 (274085.6 310994.1)
 9 Sub Zero Vodka Bar              308 N Euclid Ave     12/7/18  10096925   29510112400 (270816.1 312051.7)
10 Sushi Ai                        910 Olive St         3/29/18  10021018   29510125600 (276655.9 310257.9)
11 Sushi Ai                        910 Olive St         5/20/18  10021018   29510125600 (276655.9 310257.9)
12 Sushi Ai                        910 Olive St         6/17/18  10021018   29510125600 (276655.9 310257.9)
13 Sushi Ai                        910 Olive St         8/25/18  10021018   29510125600 (276655.9 310257.9)
14 Sushi Ai                        910 Olive St         10/30/18 10021018   29510125600 (276655.9 310257.9)
15 SUSHI KOI                       4 N Euclid Ave       1/17/18  10096866   29510119101   (270740 311550.6)
```

The `gw_identify()` function also includes support for Census block
groups, precincts, wards, and neighborhoods.

#### Aggregate Points

We can also convert these to a shapefile representing the neighborhood
(or one of several other geographies) where sushi resturants are
located:

``` r
> sushi_nhoods <- gw_aggregate(sushi_sf, to = "neighborhood")
Point data transformed to Missouri State Plane East NAD83 (2011) for spatial join.
```

We now can see that sushi resturants are concentrated in Downtown,
Midtown, and the Central West End:

``` r
> sushi_nhoods %>% 
+ dplyr::arrange(dplyr::desc(COUNT))
Simple feature collection with 88 features and 4 fields
geometry type:  MULTIPOLYGON
dimension:      XY
bbox:           xmin: 265637.5 ymin: 299617.3 xmax: 278237.4 ymax: 326428.3
epsg (SRID):    6512
proj4string:    +proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.999933333 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs
First 10 features:
   ID                      NAME      AREA COUNT                       geometry
1  35                  Downtown 2675026.5     6 MULTIPOLYGON (((277330.3 31...
2  38          Central West End 4896607.3     4 MULTIPOLYGON (((270585.1 31...
3  37                   Midtown 3502572.3     3 MULTIPOLYGON (((274791.6 31...
4  77 Covenant Blu-Grand Center 1466647.6     1 MULTIPOLYGON (((273999.1 31...
5  15         Tower Grove South 3831397.4     1 MULTIPOLYGON (((272447.2 30...
6  43                Franz Park 1023053.7     0 MULTIPOLYGON (((267510.7 30...
7  29                   Tiffany  546954.1     0 MULTIPOLYGON (((272923.9 30...
8  28         Botanical Heights 1076380.0     0 MULTIPOLYGON (((272452.7 31...
9  40                 Kings Oak  437270.6     0 MULTIPOLYGON (((270502.8 31...
10 41                Cheltenham  858961.8     0 MULTIPOLYGON (((269632.1 31...
```

Other options for spatial joins are block groups, tracts, precincts,
wards, or city as a whole.

### Data Wrangling

#### Neighborhood Identifier and Name Conversion

We can also convert the neighborhood names into numeric identification
numbers and back using `gw_nhood()`:

``` r
> gw_nhood(sushi_nhoods, var = NAME, to = "numeric")
Simple feature collection with 88 features and 4 fields
geometry type:  MULTIPOLYGON
dimension:      XY
bbox:           xmin: 265637.5 ymin: 299617.3 xmax: 278237.4 ymax: 326428.3
epsg (SRID):    6512
proj4string:    +proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.999933333 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs
First 10 features:
   ID NAME      AREA COUNT                       geometry
1  43   43 1023053.7     0 MULTIPOLYGON (((267510.7 30...
2  29   29  546954.1     0 MULTIPOLYGON (((272923.9 30...
3  28   28 1076380.0     0 MULTIPOLYGON (((272452.7 31...
4  40   40  437270.6     0 MULTIPOLYGON (((270502.8 31...
5  41   41  858961.8     0 MULTIPOLYGON (((269632.1 31...
6  42   42  907781.7     0 MULTIPOLYGON (((268176 3105...
7  39   39 1672008.7     0 MULTIPOLYGON (((272350.9 31...
8  44   44  721220.9     0 MULTIPOLYGON (((267019.9 31...
9  36   36 2978410.3     0 MULTIPOLYGON (((276518.1 31...
10 37   37 3502572.3     3 MULTIPOLYGON (((274791.6 31...
```

The `gw_nhood()` function also offers the reverse functionality,
allowing users to conver the data from numeric to string.

#### Extracting Coordinates

Finally, `gateway` offers a helper function designed to take the
`geometry` column in a `sf` point object and convert its coordinates to
columns:

``` r
> gw_get_coords(sushi_sf, names = c("x","y"), crs = 4269)
Simple feature collection with 15 features and 6 fields
geometry type:  POINT
dimension:      XY
bbox:           xmin: -90.26318 ymin: 38.5984 xmax: -90.19386 ymax: 38.645
epsg (SRID):    4269
proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs
# A tibble: 15 x 7
   name                            address              visit    addrrecnum     x     y             geometry
   <chr>                           <chr>                <chr>    <chr>      <dbl> <dbl>          <POINT [°]>
 1 BaiKu Sushi Lounge              3407 Olive St        3/20/18  10022116   -90.2  38.6  (-90.2282 38.63671)
 2 Cafe Mochi                      3221 S Grand Blvd    10/10/18 10049492   -90.2  38.6  (-90.24307 38.5984)
 3 Drunken Fish - Central West End 1 Maryland Plz       12/2/18  10279312   -90.3  38.6   (-90.26181 38.645)
 4 Kampai Sushi Bar                4949 W Pine Blvd     2/13/18  10158461   -90.3  38.6 (-90.26318 38.64242)
 5 Midtown Sushi & Ramen           3674 Forest Park Ave 3/4/18   10173725   -90.2  38.6 (-90.23708 38.63332)
 6 Mizu Sushi Bar                  1013 Washington Ave  9/12/18  10015227   -90.2  38.6 (-90.19418 38.63163)
 7 Sapporo 2                       3043 Olive St        3/1/18   10022067   -90.2  38.6 (-90.22336 38.63539)
 8 Sapporo 2                       3043 Olive St        7/3/18   10022067   -90.2  38.6 (-90.22336 38.63539)
 9 Sub Zero Vodka Bar              308 N Euclid Ave     12/7/18  10096925   -90.3  38.6   (-90.26088 38.645)
10 Sushi Ai                        910 Olive St         3/29/18  10021018   -90.2  38.6 (-90.19386 38.62869)
11 Sushi Ai                        910 Olive St         5/20/18  10021018   -90.2  38.6 (-90.19386 38.62869)
12 Sushi Ai                        910 Olive St         6/17/18  10021018   -90.2  38.6 (-90.19386 38.62869)
13 Sushi Ai                        910 Olive St         8/25/18  10021018   -90.2  38.6 (-90.19386 38.62869)
14 Sushi Ai                        910 Olive St         10/30/18 10021018   -90.2  38.6 (-90.19386 38.62869)
15 SUSHI KOI                       4 N Euclid Ave       1/17/18  10096866   -90.3  38.6 (-90.26177 38.64049)
```

## Contributor Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](.github/CONDUCT.md). By participating in this project you agree
to abide by its terms.
