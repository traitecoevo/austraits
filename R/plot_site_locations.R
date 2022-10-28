#' @title Produce site maps 
#' @description Plot location where trait data was collected from
#' @param traits trait dataframe generated from austraits with site details appended. See join_all and examples
#' @param feature grouping/classification categories e.g trait_name, collection_type
#' @param ... arguments passed to ggplot()
#' @author Dony Indiarto - d.indiarto@student.unsw.edu.au
#' @return ggplot of sites
#' @examples 
#' \dontrun{
#' #All traits from a given study
#' data <- austraits %>% extract_dataset(dataset_id = "Falster_2003") %>% join_all() 
#' data$traits %>% plot_site_locations("trait_name")
#' 
#' #Single trait
#' data <- austraits %>% extract_trait(trait_names = c("plant_height")) %>% join_all() 
#' data$traits %>% plot_site_locations("trait_name")
#' }
#' @export
#' @importFrom rlang .data

plot_site_locations <- function(traits, feature="trait_name", ...){
  
  au_map <- australia_map_raster %>%
    dplyr::mutate(australia = as.factor(.data$australia))
  
  #Create site data
  sites <- 
    traits %>%
    dplyr::select(.data$location_name, .data$`latitude (deg)`, .data$`longitude (deg)`, !!feature) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(dplyr::across(c("longitude (deg)","latitude (deg)"), as.numeric)) %>% 
    dplyr::filter(
      .data$`latitude (deg)` > (-45), .data$`latitude (deg)` < (-9.5),
      .data$`longitude (deg)` > (110), .data$`longitude (deg)` < (153))

  #Create site map  
  site_map <- 
    ggplot2::ggplot() +
    ggplot2::geom_raster(data = au_map, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$australia), show.legend = FALSE) +
    # Add trait data
    ggpointdensity::geom_pointdensity(
      data = sites,
      ggplot2::aes(y = .data$`latitude (deg)`, x = .data$`longitude (deg)`),
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

