
# allofus

R package to streamline use of the AllofUs Resarch Workbench 

## Installation

```
install.packages("remotes")
remotes::install_github("roux-ohdsi/allofus")
```

## Use

Use `aou_connect()` to establish a database connection. It will also
save your bucket name as a variable called `bucket` (this can be customized using the 
`bucket_name` argument)

```
con <- aou_connect()
```

Then, you can use `aou_ls_bucket()` to list files in your bucket

```
aou_ls_bucket()
```

Files in your workspace are only accessible to you, but not your collaborators. If you need to move a file from your workspace bucket to your personal workspace, use `aou_bucket_to_workspace()` to move files from your bucket to your workspace

```
aou_bucket_to_workspace("file1.csv")
```

And when you want to save something from your workspace to your bucket so that
your collaborators can use it (or so that you can shut down your workspace and save \$\$\$), you can use `aou_workspace_to_bucket()` to move files from your workspace to your bucket
for permanent storage.

```
aou_bucket_to_workspace("file1.csv")
```

There is also a function that you can use to load packages, and install them if
they are not yet (or no longer) installed in your environment. 

```
bookstore(c("CohortGenerator", "tidyr")
```

## Bugs

Please leave us comments, requests, and report bugs using the "Issues" tab on github.
