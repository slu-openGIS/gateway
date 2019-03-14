# gateway 0.3.0.9000

**Major, Breaking Changes:**
* All data download functionality now enclosed in `gw_get_data()`
* All data formally exported by the package has been removed, and is now accessible via `gw_get_data()`
* Neighborhood conversion functionality now enclused in `gw_nhood()`

**Major Additions:**
* Local geocoding functionality with `gw_geocode()` and `gw_build_geocoder()`
* Spatial join functionality with `gw_identify()` and `gw_aggregate()`
* Dictionary data for `postmastr` - `stl_std_streets` and `stl_std_suffix`
* Add helper function for extrating coordinates from `sf` `POINT` objects, `gw_get_coords()`
* Add two sample data sets, `sushi` and `sushi_sf`, as well as a `sample_geocoder`

**Minor Changes:**
* Added a `NEWS.md` file to track changes to the package.
* Add all `.github/` community files
* Add a `pkgdown` site
* Update `README` with additional examples
* Update `LICENSE` to GPL-3

# gateway 0.2.0

* Add functionality for downloading data from the City of St. Louis and the SLU OpenGIS Project

# gateway 0.1.0

* The initial release of the gateway package includes functions for converting string City of St. Louis neighborhood names to numeric codes, and numeric codes to string neighborhood names.
