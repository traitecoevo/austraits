# outputs are consistent with versions

    Code
      print.traits.build(austraits_5.0.0_lite)
    Message
      
      -- This is 5.0.0 of AusTraits: a curated plant trait database for the Australian
      i This database is built using traits.build version 1.0.1
      i This database contains a total of 115733 records, for 7324 taxa and 143
        traits.
      
      -- This object is a 'list' with the following components: --
      
      * traits: A table containing measurements of traits.
      * locations: A table containing observations of location/site characteristics
      associated with information in `traits`. Cross referencing between the two
      dataframes is possible using combinations of the variables `dataset_id`,
      `location_name`.
      * contexts: A table containing observations of contextual characteristics
      associated with information in `traits`. Cross referencing between the two
      dataframes is possible using combinations of the variables `dataset_id`,
      `link_id`, and `link_vals`.
      * methods: A table containing details on methods with which data were
      collected, including time frame and source. Cross referencing with the `traits`
      table is possible using combinations of the variables `dataset_id`,
      `trait_name`.
      * excluded_data: A table of data that did not pass quality test and so were
      excluded from the master dataset.
      * taxonomic_updates: A table of all taxonomic changes implemented in the
      construction of AusTraits. Changes are determined by comparing against the APC
      (Australian Plant Census) and APNI (Australian Plant Names Index).
      * taxa: A table containing details on taxa associated with information in
      `traits`. This information has been sourced from the APC (Australian Plant
      Census) and APNI (Australian Plant Names Index) and is released under a CC-BY3
      license.
      * contributors: A table of people contributing to each study.
      * sources: Bibtex entries for all primary and secondary sources in the
      compilation.
      * definitions: A copy of the definitions for all tables and terms. Information
      included here was used to process data and generate any documentation for the
      study.
      * schema: A copy of the schema for all tables and terms. Information included
      here was used to process data and generate any documentation for the study.
      * metadata: Metadata associated with the dataset, including title, creators,
      license, subject, funding sources.
      * build_info: A description of the computing environment used to create this
      version of the dataset, including version number, git commit and R
      session_info.
      i To access a component, try using the $ e.g. austraits$traits

---

    Code
      print.traits.build(austraits_3.0.2_lite)
    Message
      
      -- This database contains a total of 0 records, for 0 taxa and 0 traits. -------
      
      -- This object is a 'list' with the following components: --
      
      * traits: A table containing measurements of traits.
      * locations: A table containing observations of location/site characteristics
      associated with information in `traits`. Cross referencing between the two
      dataframes is possible using combinations of the variables `dataset_id`,
      `location_name`.
      * contexts: A table containing observations of contextual characteristics
      associated with information in `traits`. Cross referencing between the two
      dataframes is possible using combinations of the variables `dataset_id`,
      `link_id`, and `link_vals`.
      * methods: A table containing details on methods with which data were
      collected, including time frame and source. Cross referencing with the `traits`
      table is possible using combinations of the variables `dataset_id`,
      `trait_name`.
      * excluded_data: A table of data that did not pass quality test and so were
      excluded from the master dataset.
      * taxonomic_updates: A table of all taxonomic changes implemented in the
      construction of AusTraits. Changes are determined by comparing against the APC
      (Australian Plant Census) and APNI (Australian Plant Names Index).
      * taxa: A table containing details on taxa associated with information in
      `traits`. This information has been sourced from the APC (Australian Plant
      Census) and APNI (Australian Plant Names Index) and is released under a CC-BY3
      license.
      * definitions: A copy of the definitions for all tables and terms. Information
      included here was used to process data and generate any documentation for the
      study.
      * contributors: A table of people contributing to each study.
      * sources: Bibtex entries for all primary and secondary sources in the
      compilation.
      * build_info: A description of the computing environment used to create this
      version of the dataset, including version number, git commit and R
      session_info.
      i To access a component, try using the $ e.g. austraits$traits

