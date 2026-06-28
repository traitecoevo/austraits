# Pivot long format traits table into wide format

Function to "widen" long format data ("tidy data").

Data in a traits.build databases' traits table are organised in a long
format where each trait measurement is on a different row and
measurement metadata is recorded in other columns. Multiple traits may
be measured as part of a single observation and this function pivots the
data wider, such that each trait is its own column. *Note that if two
trait measurements have the same observation_id but different value
types (min, mean, mode, etc.) these will be on separate rows.*

The function austraits::trait_pivot_longer reverts the actions of this
function.

## Usage

``` r
trait_pivot_wider(database)
```

## Arguments

- database:

  The traits tibble from a traits.build database

## Value

traits.build traits table in wide format

## Details

\`trait_pivot_wider“ has been developed to pivot the traits table for a
database build using the traits.build workflow. Learn more at:
<https://github.com/traitecoevo/traits.build> &
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

data <- austraits_5.0.0_lite$traits %>% filter(dataset_id == "Falster_2003")
data #long format 
traits_wide <- trait_pivot_wider(data) 
traits_wide #wide format
} # }
```
