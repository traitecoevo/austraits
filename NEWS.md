# austraits 3.1.0
- Change functions to work with traits.build databases that include an identifiers table (or ones that don't)

# austraits 3.0.2
- Updated dependencies, placing graphics related packages in Suggests
- Added internal function to check the compatibility of databases
- Added internal function notify users that some database versions will not be supported
- The following functions will no longer support AusTraits version < 5.0.0:
    - `extract_*` 
    - `trait_pivot_wider`
    - `join_*`
    - `plot_site/locations`
    - `plot_trait_beeswarm`
    - `as_wide_table`
- `trait_pivot_longer` is deprecated
- `summarise_trait_means` will trigger warning due to uninformed calculations
- Added new function `extract_data`
- Added new function `bind_databases`
- Added new function `flatten_databases`
- Updated print function `print.traits.build` - thanks @Rekyt!