#' @title Produce location maps of trait values 
#' @description Plot location where trait data was collected from
#' @param aus_traits austraits object OR traits table. Note location details must be joined. See join_all and examples
#' @param feature grouping/classification categories e.g trait_name, collection_type for <= v3.0.2, basis of record for >3.0.2
#' @param ... arguments passed to ggplot()
#' @author Dony Indiarto - d.indiarto@student.unsw.edu.au
#' @return ggplot of sites
#' @examples 
#' \dontrun{
#' #All traits from a given study
#' data <- austraits %>% extract_dataset(dataset_id = "Falster_2003") %>% join_all() 
#' data %>% plot_locations("trait_name")
#' 
#' #Single trait
#' data <- austraits %>% extract_trait(trait_names = c("plant_height")) %>% join_all() 
#' data$traits %>% plot_locations("trait_name")
#' }
#' @export


plot_locations <- function(aus_traits, feature="trait_name", ...){
  
  # Check if traits.build object or the traits table
  # If traits.build, check if traits table contains coordinate cols
  if( is.null(dim(aus_traits)) ){
    
    # Extract traits table if needed
    traits <- get_traits_table(aus_traits)
    
    if( length(stringr::str_which(names(traits), "(deg)")) < 2 ){
      cli::cli_alert_info("Coordinate columns were not detected, joining location tables now.")
      aus_traits <- aus_traits |> join_locations()
      traits <- get_traits_table(aus_traits)
    }
  } else {
    traits <- aus_traits #If not traits.build, assign traits table as traits
    
    # Check if traits contains coordinate cols in traits table
    if( length(stringr::str_which(names(traits), "(deg)")) < 2 ) 
      cli::cli_abort("No location data found in traits table - try `join_locations()` first before `plot_locations()`")
  }
  
  plot_locations2(traits, feature)
}

#' Location plot for AusTraits versions > 3.0.2
#' @noRd
plot_locations2 <- function(aus_traits, feature, ...){
  au_map <- australia_map_raster %>%
    dplyr::mutate(australia = as.factor(australia))
  
  #Create site data
  sites <-
    aus_traits |> 
    dplyr::select(!!feature, tidyselect::any_of(c("site_name", "location_name", "latitude (deg)", "longitude (deg)"))) |> 
    tidyr::drop_na() |> 
    dplyr::mutate(dplyr::across(c("longitude (deg)","latitude (deg)"), as.numeric)) %>% 
    dplyr::filter(
      `latitude (deg)` > (-45), `latitude (deg)` < (-9.5),
      `longitude (deg)` > (110), `longitude (deg)` < (153))
  
  #Create site map  
  site_map <- 
    ggplot2::ggplot() +
    ggplot2::geom_raster(data = au_map, ggplot2::aes(x = x, y = y, fill = australia), show.legend = FALSE) +
    # Add trait data
    ggpointdensity::geom_pointdensity(
      data = sites,
      ggplot2::aes(y = `latitude (deg)`, x = `longitude (deg)`),
      inherit.aes = FALSE,
      show.legend = TRUE,
      adjust = 1,
      ...
    ) +
    ggplot2::scale_x_continuous(limits = c(NA, 154)) +
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
#' @param traits  traits table with site details appended. See join_all and examples
#' @param feature grouping/classification categories e.g trait_name, collection_type for <= v3.0.2
#' @param ... arguments passed to ggplot()
#' @author Dony Indiarto - d.indiarto@student.unsw.edu.au
#' @return ggplot of sites
#' @export

plot_site_locations <- function(traits, feature="trait_name", ...){
  # Extract function name
  function_name <- as.character(sys.calls()[[1]])[1]
  
  # Determine if traits table or traits.build object
  if( is.null(dim(traits))){
    # Extract AusTraits version
    AusTraits_version <- print_version(aus_traits)
  } else
    AusTraits_version <- "< 5.0.0"
  
  cli::cli_abort(c(
    "i" = "{function_name} has been deprecated!",
    ">" =  "Use plot_locations() instead"
  )
  )
}
