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
  # Setting up
  ## Determine version using col names of traits table
  if(any(stringr::str_detect(names(aus_traits), "location"))){
    version = "newer"
  } else(
    version = "older"
  )

  switch (version,
          'newer' = plot_locations2(aus_traits, feature),
          'older' = plot_locations1(aus_traits, feature)
          )
}

#' Location plot for AusTraits versions <= 3.0.2
#' @noRd
plot_locations1 <- function(aus_traits, feature, ...){
  au_map <- australia_map_raster %>%
    dplyr::mutate(australia = as.factor(australia))
  
  if( is.null(dim(aus_traits)) ){
    traits <- aus_traits$traits
  } else{
    traits <- aus_traits
  }
  #Create site data
  sites <- 
    traits %>%
    dplyr::select(site_name, `latitude (deg)`, `longitude (deg)`, !!feature) %>%
    tidyr::drop_na() %>%
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
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1),
      axis.ticks.length = ggplot2::unit(1, "mm"),
      axis.ticks = ggplot2::element_line(size = 1)
    )  + ggplot2::xlab("") + ggplot2::ylab("") + 
    ggplot2::coord_fixed()
  
  # facet by feature if specified - default
  if(!is.na(feature)){
    site_map <- site_map + ggplot2::facet_wrap(paste("~", feature))
  }
  suppressWarnings(print(site_map))
}

#' Location plot for AusTraits versions > 3.0.2
#' @noRd
plot_locations2 <- function(aus_traits, feature, ...){
  au_map <- australia_map_raster %>%
    dplyr::mutate(australia = as.factor(australia))
  
  if( is.null(dim(aus_traits)) ){
  traits <- aus_traits$traits
  } else{
    traits <- aus_traits
  }
  
  #Create site data
  sites <- 
    traits %>%
    dplyr::select(location_name, `latitude (deg)`, `longitude (deg)`, !!feature) %>%
    tidyr::drop_na() %>%
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
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1),
      axis.ticks.length = ggplot2::unit(1, "mm"),
      axis.ticks = ggplot2::element_line(size = 1)
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
  .Deprecated("plot_locations")
  
  plot_locations1(traits, feature="trait_name", ...)
}
