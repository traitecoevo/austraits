# Package index

## Accessing and exploring the database

- [`load_austraits()`](https://traitecoevo.github.io/austraits/reference/load_austraits.md)
  : Load AusTraits database into R console
- [`get_compiled_by_traits.build()`](https://traitecoevo.github.io/austraits/reference/get_compiled_by_traits.build.md)
  : Retrieve compiled by information from metadata table
- [`get_traits_table()`](https://traitecoevo.github.io/austraits/reference/get_traits_table.md)
  : Retrieve traits table if user passes traits.build object.
- [`get_version_latest()`](https://traitecoevo.github.io/austraits/reference/get_version_latest.md)
  : Retrieve the latest version of AusTraits
- [`get_versions()`](https://traitecoevo.github.io/austraits/reference/get_versions.md)
  : Print out AusTraits versions
- [`summarise_database()`](https://traitecoevo.github.io/austraits/reference/summarise_database.md)
  : Summarise counts for a particular variable of interest
- [`lookup_context_property()`](https://traitecoevo.github.io/austraits/reference/lookup_context_property.md)
  : Look up context properties
- [`lookup_location_property()`](https://traitecoevo.github.io/austraits/reference/lookup_location_property.md)
  : Look up location properties
- [`lookup_trait()`](https://traitecoevo.github.io/austraits/reference/lookup_trait.md)
  : Look up a particular trait term

## Filter from the database

- [`extract_data()`](https://traitecoevo.github.io/austraits/reference/extract_data.md)
  : Extract data from traits.build database
- [`extract_dataset()`](https://traitecoevo.github.io/austraits/reference/extract_dataset.md)
  : Extract all data for a particular dataset
- [`extract_taxa()`](https://traitecoevo.github.io/austraits/reference/extract_taxa.md)
  : Extract all data for specific taxa
- [`extract_trait()`](https://traitecoevo.github.io/austraits/reference/extract_trait.md)
  : Extract all data for specific traits

## Join data to traits table

- [`join_context_properties()`](https://traitecoevo.github.io/austraits/reference/join_context_properties.md)
  : Joining context properties to traits table
- [`join_contributors()`](https://traitecoevo.github.io/austraits/reference/join_contributors.md)
  : Joining data contributor metadata to traits table
- [`join_identifiers()`](https://traitecoevo.github.io/austraits/reference/join_identifiers.md)
  : Joining identifiers to traits table
- [`join_location_coordinates()`](https://traitecoevo.github.io/austraits/reference/join_location_coordinates.md)
  : Joining location coordinates to traits table
- [`join_location_properties()`](https://traitecoevo.github.io/austraits/reference/join_location_properties.md)
  : Joining location properties to traits table
- [`join_methods()`](https://traitecoevo.github.io/austraits/reference/join_methods.md)
  : Joining methodological information to traits table
- [`join_taxa()`](https://traitecoevo.github.io/austraits/reference/join_taxa.md)
  : Joining taxonomy to traits table
- [`join_taxonomic_updates()`](https://traitecoevo.github.io/austraits/reference/join_taxonomic_updates.md)
  : Joining taxonomic updates information to traits table

## Reshape traits table

- [`trait_pivot_longer()`](https://traitecoevo.github.io/austraits/reference/trait_pivot_longer.md)
  **\[deprecated\]** : Pivot wide format traits table into long format
- [`trait_pivot_wider()`](https://traitecoevo.github.io/austraits/reference/trait_pivot_wider.md)
  : Pivot long format traits table into wide format
- [`join_context_properties()`](https://traitecoevo.github.io/austraits/reference/join_context_properties.md)
  : Joining context properties to traits table
- [`join_contributors()`](https://traitecoevo.github.io/austraits/reference/join_contributors.md)
  : Joining data contributor metadata to traits table
- [`join_identifiers()`](https://traitecoevo.github.io/austraits/reference/join_identifiers.md)
  : Joining identifiers to traits table
- [`join_location_coordinates()`](https://traitecoevo.github.io/austraits/reference/join_location_coordinates.md)
  : Joining location coordinates to traits table
- [`join_location_properties()`](https://traitecoevo.github.io/austraits/reference/join_location_properties.md)
  : Joining location properties to traits table
- [`join_methods()`](https://traitecoevo.github.io/austraits/reference/join_methods.md)
  : Joining methodological information to traits table
- [`join_taxa()`](https://traitecoevo.github.io/austraits/reference/join_taxa.md)
  : Joining taxonomy to traits table
- [`join_taxonomic_updates()`](https://traitecoevo.github.io/austraits/reference/join_taxonomic_updates.md)
  : Joining taxonomic updates information to traits table
- [`bind_trait_values()`](https://traitecoevo.github.io/austraits/reference/bind_trait_values.md)
  : Bind trait values
- [`separate_trait_values()`](https://traitecoevo.github.io/austraits/reference/separate_trait_values.md)
  : Separate bounded trait values

## Reshape database(s)

- [`as_wide_table()`](https://traitecoevo.github.io/austraits/reference/as_wide_table.md)
  : Create a single wide table from a traits.build data object
- [`flatten_database()`](https://traitecoevo.github.io/austraits/reference/flatten_database.md)
  : Create combined traits.build table
- [`bind_databases()`](https://traitecoevo.github.io/austraits/reference/bind_databases.md)
  : Bind multiple traits.build data objects into a single data object

## Quick data visualisations

Plot site or variance distributions

- [`plot_locations()`](https://traitecoevo.github.io/austraits/reference/plot_locations.md)
  : Produce location maps of trait values
- [`plot_site_locations()`](https://traitecoevo.github.io/austraits/reference/plot_site_locations.md)
  **\[deprecated\]** : Produce location maps of trait values
- [`plot_trait_distribution_beeswarm()`](https://traitecoevo.github.io/austraits/reference/plot_trait_distribution_beeswarm.md)
  : Beeswarm Trait distribution

## Helpful functions

- [`convert_df_to_list()`](https://traitecoevo.github.io/austraits/reference/convert_df_to_list.md)
  : Convert dataframe to list
- [`convert_list_to_df1()`](https://traitecoevo.github.io/austraits/reference/convert_list_to_df1.md)
  : Convert list with single entries to dataframe
- [`convert_list_to_df2()`](https://traitecoevo.github.io/austraits/reference/convert_list_to_df2.md)
  : Convert list of lists to dataframe
- [`print(`*`<traits.build>`*`)`](https://traitecoevo.github.io/austraits/reference/print.traits.build.md)
  : Generic for outputting a nice summary for austraits objects
