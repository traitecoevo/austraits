#' @title Trait distribution
#' @description Plots distribution of trait values by a  grouping variable using ggbeeswarm package  
#'
#' @param austraits austraits data object
#' @param plant_trait_name Name of trait to plot
#' @param y_axis_category One of `dataset_id`, `family`
#' @param highlight specify a group to highlight
#' @param hide_ids add label on y_axis?
#'
#' @export
#'
#' @examples 
#' \dontrun{
#' austraits %>% plot_trait_distribution_beeswarm("wood_density", "dataset_id", "Westoby_2014")
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes theme theme_bw xlab ylab element_blank element_text scale_x_log10 scale_x_continuous rel
#
plot_trait_distribution_beeswarm <- function(austraits, plant_trait_name, y_axis_category, highlight=NA, hide_ids = FALSE) {
  
  # Subset data to this trait
  austraits_trait <- extract_trait(austraits, plant_trait_name)
  
  my_shapes = c("_min" = 60, "_mean" = 16, "_max" =62, "unknown" = 18)
  
  as_shape <- function(value_type) {
    p <- rep("unknown", length(value_type))
    
    p[grepl("mean", value_type)] <- "_mean" #16
    p[grepl("min", value_type)] <- "_min" #60
    p[grepl("max", value_type)] <- "_max" #62
    factor(p, levels=names(my_shapes))
  }
  
  data <- 
    austraits_trait$traits %>%
    dplyr::mutate(shapes = as_shape(.data$value_type)) %>%
    dplyr::left_join(by = "taxon_name", dplyr::select(.data$taxa, .data$taxon_name, .data$family))
  
  # Define grouping variables and derivatives
  if(!y_axis_category %in% names(data)){
    stop("incorrect grouping variable")
  }
  
  # define grouping variable, ordered by group-level by mean values
  # use log_value where possible
  if(min(data$value, na.rm=TRUE) > 0 ) {
    data$value2 <- log10(data$value)
  } else {
    data$value2 <- data$value
  }
  data$Group = forcats::fct_reorder(data[[y_axis_category]], data$value2, na.rm=TRUE)
  
  n_group <- levels(data$Group) %>% length()
  
  # set colour to be alternating
  data$colour = ifelse(data$Group %in% levels(data$Group)[seq(1, n_group, by=2)],
                       "a", "b")
  
  # set colour of group to highlight
  if(!is.na(highlight) & highlight %in% data$Group) {
    data <- dplyr::mutate(data, colour = ifelse(.data$Group %in% highlight, "c", .data$colour))
  }
  
  # Check range on x-axis
  vals <- austraits_trait$definitions$traits$elements[[plant_trait_name]]$values
  range <- (vals$maximum/vals$minimum)
  
  # Check range on y-axis
  y.text <- ifelse(n_group > 20, 0.75, 1)
  heights = c(1, max(1, n_group/7))
  
  # Top plot - plain histogram of data
  p1 <-
    ggplot(data, ggplot2::aes(x=.data$value)) +
    ggplot2::geom_histogram(ggplot2::aes(y = .data$..density..), color="darkgrey", fill="darkgrey", bins=50) +
    ggplot2::geom_density(color="black") +
    xlab("") + ggplot2::ylab("All data") +
    theme_bw()  +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks.y= element_blank(),
          axis.text= element_blank(),
          panel.background = element_blank()
    )
  # Second plot -- dots by groups, using ggbeeswarm package
  p2 <-
    ggplot(data, ggplot2::aes(x = .data$value, y = .data$Group, colour = .data$colour, shape = .data$shapes)) +
    ggbeeswarm::geom_quasirandom(groupOnX=FALSE) +
    ylab(paste("By ", y_axis_category)) +
    # inclusion of custom shapes: for min, mean, unknown
    # NB: this single line of code makes function about 4-5 slower for some reason
    ggplot2::scale_shape_manual(values = my_shapes) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x=element_text(size=rel(1.25)),
          axis.text.y=element_text(size=rel(y.text))
    ) +
    ggplot2::guides(colour=FALSE)
  
  if(hide_ids) {
    p2 <- p2 + theme(axis.text.y = element_blank())
  }
  
  # Define scale on x-axis and transform to log if required
  if(vals$minimum !=0 & range > 20) {
    #log transformation
    p1 <- p1 +
      scale_x_log10(name="",
                             breaks = scales::trans_breaks("log10", function(x) 10^x),
                             labels = scales::trans_format("log10", scales::math_format(10^.data$.x)),
                             limits=c(vals$minimum, vals$maximum))
    p2 <- p2 +
      scale_x_log10(name=paste(plant_trait_name, ' (', data$unit[1], ')'),
                             breaks = scales::trans_breaks("log10", function(x) 10^x),
                             labels = scales::trans_format("log10", scales::math_format(10^.data$.x)),
                             limits=c(vals$minimum, vals$maximum))
  } else {
    p1 <- p1 + scale_x_continuous(limits=c(vals$minimum, vals$maximum))
    p2 <- p2 + scale_x_continuous(limits=c(vals$minimum, vals$maximum)) +
      xlab(paste(plant_trait_name, ' (', data$unit[1], ')'))
    
  }
  
  # combine plots
  # Might be a better way to do this with other packages?
  
  f <- function(x) {suppressWarnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(x)))}
  p1 <- f(p1)
  p2 <- f(p2)
  # Fix width of second plot to be same as bottom using ggplot_table
  p1$widths[2:3] <- p2$widths[2:3]
  gridExtra::grid.arrange(p1, p2, nrow=2, widths=c(1), heights=heights)
}
