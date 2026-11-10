#' @title Produce location maps of trait values 
#' @description Plot location where trait data was collected from
#' @param database traits.build database OR traits table from a traits.build database. Note location details must be joined. See join_location_coordinates and examples
#' @param feature grouping/classification categories e.g trait_name, collection_type for <= v3.0.2, basis of record for >3.0.2
#' @param ... arguments passed to ggpointdensity::geom_pointdensity()
#' @param basemap `"australia"` (the default) draws the locations over a map
#' of Australia, and leaves out any location beyond the map, with a message
#' saying how many records that affects. `NULL` draws every location without
#' a map, for databases from outside Australia.
#' @author Dony Indiarto - d.indiarto@student.unsw.edu.au
#' @return ggplot of sites
#' @examples
#' \dontrun{
#' #All traits from a given study
#' data <- austraits %>% extract_dataset(dataset_id = "Falster_2003") %>% join_location_coordinates()
#' data %>% plot_locations("trait_name")
#'
#' #Single trait
#' data <- austraits %>% extract_trait(trait_names = c("plant_height")) %>% join_location_coordinates()
#' data$traits %>% plot_locations("trait_name")
#'
#' #Every location, without the map of Australia
#' data$traits %>% plot_locations("trait_name", basemap = NULL)
#' }
#' @export


plot_locations <- function(database, feature="trait_name", ..., basemap = "australia"){

  if(!is.null(basemap) && !identical(basemap, "australia")){
    cli::cli_abort("`basemap` must be \"australia\" or NULL")
  }
  
  # Check if traits.build object or the traits table
  # If traits.build, check if traits table contains coordinate cols
  if( is.null(dim(database)) ){
    
    # Extract traits table if needed
    traits <- get_traits_table(database)
    
    if( length(stringr::str_which(names(traits), "(deg)")) < 2 ){
      cli::cli_alert_info("Coordinate columns were not detected, joining location tables now.")
      database <- database %>% join_location_coordinates()
      traits <- get_traits_table(database)
    }
  } else {
    traits <- database #If not traits.build, assign traits table as traits
    
    # Check if traits contains coordinate cols in traits table
    if( length(stringr::str_which(names(traits), "(deg)")) < 2 ) 
      cli::cli_abort("No location data found in traits table - try `join_location_coordinates()` first before `plot_locations()`")
  }
  
  plot_locations2(traits, feature, basemap = basemap, ...)
}

#' Location plot for AusTraits versions > 3.0.2
#' @noRd
plot_locations2 <- function(database, feature, basemap = "australia", ...){
  #Create site data
  sites <-
    database %>%
    dplyr::select(tidyselect::all_of(as.character(feature[!is.na(feature)])), tidyselect::any_of(c("site_name", "location_name", "latitude (deg)", "longitude (deg)"))) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(dplyr::across(c("longitude (deg)","latitude (deg)"), ~ suppressWarnings(as.numeric(.x)))) %>%
    dplyr::filter(!is.na(`latitude (deg)`), !is.na(`longitude (deg)`))

  site_map <- ggplot2::ggplot()

  if(!is.null(basemap)){
    au_map <- australia_map_raster %>%
      dplyr::mutate(australia = as.factor(australia))

    # Leave out locations beyond the map (plus a margin), saying so, rather
    # than letting a handful of distant locations shrink the map
    margin <- 0.5
    inside <-
      dplyr::between(sites$`longitude (deg)`, min(au_map$x) - margin, max(au_map$x) + margin) &
      dplyr::between(sites$`latitude (deg)`, min(au_map$y) - margin, max(au_map$y) + margin)

    if(any(!inside)){
      outside <- sites[!inside, ]
      n_locations <- nrow(dplyr::distinct(outside, `latitude (deg)`, `longitude (deg)`))
      cli::cli_alert_info(paste0(
        sum(!inside), " record(s) at ", n_locations,
        " location(s) lie beyond the map of Australia and are not shown. ",
        "Use `basemap = NULL` to show every location."
      ))
      sites <- sites[inside, ]
    }

    site_map <- site_map +
      ggplot2::geom_raster(data = au_map, ggplot2::aes(x = x, y = y, fill = australia), show.legend = FALSE)
  }

  #Create site map
  site_map <-
    site_map +
    # Add trait data
    ggpointdensity::geom_pointdensity(
      data = sites,
      ggplot2::aes(y = `latitude (deg)`, x = `longitude (deg)`),
      inherit.aes = FALSE,
      show.legend = TRUE,
      adjust = 1,
      ...
    ) +
    ggplot2::scale_fill_manual(values = "cadetblue4", na.value="white", guide = "none") +
    viridis::scale_color_viridis(option = "plasma") +
    ggplot2::theme(
      legend.justification = c(-0.1, 0),
      legend.position = "bottom",
      legend.direction  = "horizontal",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, linewidth = 1),
      axis.ticks.length = ggplot2::unit(1, "mm"),
      axis.ticks = ggplot2::element_line(linewidth = 1)
    )  + ggplot2::xlab("") + ggplot2::ylab("") + 
    ggplot2::coord_fixed()
  
  # facet by feature if specified - default
  if(!is.na(feature)){
    site_map <- site_map + ggplot2::facet_wrap(paste("~", feature))
  }
  suppressWarnings(print(site_map))
}


#' Produce location maps of trait values 
#'
#' @description `r lifecycle::badge('deprecated')`
#'
#'Plot location where trait data was collected from
#' @param trait_data traits table in a traits.build database with site details appended. See join_location_coordinates and examples
#' @param feature grouping/classification categories e.g trait_name, collection_type for <= v3.0.2
#' @param ... arguments passed to ggplot()
#' @author Dony Indiarto - d.indiarto@student.unsw.edu.au
#' @return ggplot of sites
#' @export

plot_site_locations <- function(trait_data, feature="trait_name", ...){
  # Extract function name
  function_name <- as.character(sys.calls()[[1]])[1]
  
  # Determine if traits table or traits.build object
  if( is.null(dim(trait_data))){
    # Extract AusTraits version
    AusTraits_version <- print_version(trait_data)
  } else
    AusTraits_version <- "< 5.0.0"
  
  cli::cli_abort(c(
    "i" = "{function_name} has been deprecated!",
    ">" =  "Use plot_locations() instead"
  )
  )
}
