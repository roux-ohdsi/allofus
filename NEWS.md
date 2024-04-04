# allofus 1.1.0

* added new features including creation of temporary tables using `aou_compute()` and `aou_create_temp_table()` and
a new `aou_collect()` function which accomodates bit64 integers when needed.

* simplified `aou_observation_period()` function to look at earliest and 
latest clinical events rather than strictly implementing OHDSI conventions per
expert advice

* all functions with the option to collect data locally default to `collect = FALSE`

* minor big fixes, improved error messages, and improved documentation
