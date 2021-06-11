#' @title Produce site maps 
#' @description Plot
#' @param trait trait dataframe generated from austraits with site details appended. See join_all and examples
#' @param feature grouping/classification categories
#' @author Dony Indiarto - d.indiarto@student.unsw.edu.au
#' @return ggplot of sites
#' @importFrom ggplot2 ggplot aes geom_raster theme scale_fill_grey xlab ylab facet_wrap
#' @examples 
#' \dontrun{
#' data <- austraits %>% join_all() %>% `[[`("traits") 
#' data %>% plot_site_locations()
#' 
#' data <- austraits %>% extract_trait(trait_names = c("plant_height")) %>% join_all() %>% `[[`("traits")
#' data %>% plot_site_locations("trait_name")
#' }
#' @export
#' @importFrom rlang .data
plot_site_locations <- function(traits, feature="trait_name", size=0.5, alpha = 0.8, xlab = "", ylab=""){

  sites <- 
    traits %>%
    dplyr::select(.data$site_name, .data$`latitude (deg)`, .data$`longitude (deg)`, !!feature) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(dplyr::across(c("longitude (deg)","latitude (deg)"), as.numeric)) %>% 
    dplyr::filter(
      .data$`latitude (deg)` > (-45), .data$`latitude (deg)` < (-9.5),
      .data$`longitude (deg)` > (110), .data$`longitude (deg)` < (153))
  
  site_map <- 
    ggplot() +
    # base map
    geom_raster(data = raster::as.data.frame(austraits::australia_map_raster, xy = T), 
                aes(x = x, y = y,fill = factor(australia))
    )  +
    # add data
    ggpointdensity::geom_pointdensity(
      data = sites,
      aes(y = `latitude (deg)`, x = `longitude (deg)`),
      inherit.aes = FALSE,
      show.legend = TRUE,
      adjust = 1,
      size = size,
      alpha=alpha
    ) +
    viridis::scale_color_viridis(option = "plasma") +
    theme(
      legend.justification = c(-0.1, 0),
      legend.position = c(0.05, 0.05),
      legend.direction  = "horizontal",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            axis.ticks.length = unit(1, "mm"),
            axis.ticks = element_line(size = 1)
    ) +
    scale_fill_grey(
      name = "",
      start = 0.8,
      guide = FALSE,
      na.value = "white"
    ) + xlab(xlab) + ylab(ylab)
  
  # facet by feature if specified
  if(!is.na(feature)){
      site_map <- site_map + facet_wrap(paste("~", feature))
  }
  
  site_map
}

