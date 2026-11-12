# austraits (development version)
- Support databases built with traits.build 2.3, where every observation names the reference it was taken from in `source_id` (the `dataset_id` unless it came from another study) and the methods table's reference columns are `dataset_id_type`, `dataset_id_citation`, `secondary_id` and `secondary_citation` (traitecoevo/traits.build#296). Older databases still work.
- New function `join_sources()` adds each observation's references to the traits table: `dataset_id_citation`, `source_id_citation` (formatted from `sources[[source_id]]`), `secondary_id` and `secondary_citation`.
- `flatten_database()` takes its references from `join_sources()`, so the flattened table has those columns in place of the methods table's `source_*` columns.
- `as_wide_table()` has `dataset_id_citation`, `source_id_citation` and `secondary_citation` in place of `source_primary_citation` and `source_secondary_citation`.
- `extract_data()` keeps every reference the extracted observations cite, including the studies observations were taken from, which it used to drop; it no longer fails on a database without `source_secondary_key`.
- `bind_databases()` warns when one citation key names different references in different datasets (titles or years differ), since `sources` keeps only one entry per key and the other is lost.

# austraits 3.1.1
- Fix bug in function `load_austraits` that was preventing it from loading austraits.build v.7.0.0, as both relational and flattened database versions are now on Zenodo. For now, ignoring flattened data table.

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