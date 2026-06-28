# Extract all data for a particular dataset

Function to subset all data associated with a particular dataset from a
traits.build relational database.

## Usage

``` r
extract_dataset(database, dataset_id)
```

## Arguments

- database:

  traits.build database (list object)

- dataset_id:

  character string that matches a `dataset_id` in the database

## Value

List of tibbles containing all traits.build data and metadata for the
specified dataset(s).

## Details

`extract_dataset` has been developed to extract data for specific
datasets from databases built using the traits.build workflow. Learn
more at: <https://github.com/traitecoevo/traits.build> &
<https://github.com/traitecoevo/traits.build-book>

Note to AusTraits users:

- This function works with AusTraits version \>= 5.0.0 (from Nov 2023
  release)

- For AusTraits versions \<= 4.2.0 (up to Sept 2023 release) see
  <https://github.com/traitecoevo/austraits> for how to install old
  versions of the package or download a newer version of the database.

## Author

Daniel Falster - daniel.falster@unsw.edu.au

## Examples

``` r
if (FALSE) { # \dontrun{
extract_dataset(database, "Falster_2003")
} # }
```
