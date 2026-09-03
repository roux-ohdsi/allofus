# allofus 1.3.0

The package now supports All of Us workbench 2.0. In particular, environment variables are resolved on package load. `aou_create_bucket()` can now be used to create a workspace bucket, as it is no longer automatic.

In addition, this version fixes `aou_create_temp_table()` on workbench 2.0, where the resulting table could not be joined to a CDR table. The workbench 2.0 CDR lives in a single BigQuery region (`us-central1`), but a query built entirely from local data references no CDR table, so BigQuery had nothing to infer a location from and ran it in the `US` multi-region instead. Queries are now run with the CDR as the default dataset, which pins them to the CDR's region.


# allofus 1.2.0

* The `allofus` R package now has a peer-reviewed publication in the Journal of the American Medical Informatics Association (JAMIA) under the Special Issue: *Focus Issue on Returning Value to Communities from the All of Us Research Program through Innovative Approaches for Data Use, Analysis, Dissemination, and Research Capacity Building*. The citation is:

  * Smith LH, Cavanaugh R (2024). “allofus: An R Package to Facilitate Use of the All of Us Researcher Workbench.” Journal of the American Medical Informatics Association, ocae198. doi:10.1093/jamia/ocae198.

  * preprint: https://doi.org/10.1101/2024.04.10.24305611

* Fixed with `aou_sql()` and `aou_atlas_cohort()` to clarify that a connection
is necessary when `collect = FALSE`.

* updated documentation on installing `allofus` on the RStudio Workbench and reference to community workspace

* fixed links in vignettes

* Updated DOI throughout documentation

# allofus 1.1.0

* added new features including creation of temporary tables using `aou_compute()` and `aou_create_temp_table()` and
a new `aou_collect()` function which accomodates bit64 integers when needed.

* simplified `aou_observation_period()` function to look at earliest and
latest clinical events rather than strictly implementing OHDSI conventions per
expert advice

* all functions with the option to collect data locally default to `collect = FALSE`

* minor big fixes, improved error messages, and improved documentation
