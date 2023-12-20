## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

* The package is designed to be used on the [All of Us Researcher Workbench](https://www.researchallofus.org/data-tools/workbench/), which is a cloud-based platform for researchers to analyze data from the All of Us Research Program. The package is not intended to be used outside of this environment (and will not in general have any functionality). All examples and tests run successfully on the Workbench.

* Changes:

- Omitted "Tools" from the title
- Added quotations around "Atlas"
- Added link to the Workbench in the Description
- Removed all \dontrun{} from examples
- Changed functions and tests in aou_ls.R to use temporary directories instead of writing to the user's home filespace
