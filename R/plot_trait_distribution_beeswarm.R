#' @title Beeswarm Trait distribution 
#' @description Plots distribution of trait values by a  grouping variable using ggbeeswarm package  
#'
#' @param austraits austraits data object
#' @param trait_name Name of trait to plot
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

#
plot_trait_distribution_beeswarm <- function(austraits,
                                             trait_name,
                                             y_axis_category,
                                             highlight=NA,
                                             hide_ids = FALSE) {
  
  # Check compatability
  status <- check_compatibility(austraits)
  
  # If compatible
  if(!status) {
    function_not_supported(austraits)
  }
  # Subset data to this trait
  austraits_trait <- extract_trait(austraits, trait_name)
  
  my_shapes <- c("_min" = 60, "_mean" = 16, "_max" = 62, "unknown" = 18)
  
  as_shape <- function(value_type) {
    p <- rep("unknown", length(value_type))
    
    p[grepl("mean", value_type)] <- "_mean" #16
    p[grepl("min", value_type)] <- "_min" #60
    p[grepl("max", value_type)] <- "_max" #62
    factor(p, levels=names(my_shapes))
  }
  
  tax_info  <- austraits_trait$taxa %>% dplyr::select(taxon_name, family)
  
  data <-
    austraits_trait$traits %>%
    dplyr::mutate(shapes = as_shape(value_type)) %>%
    dplyr::left_join(by = "taxon_name", tax_info) %>%
    dplyr::mutate(value = as.numeric(value))
  
  # Define grouping variables and derivatives
  if(!y_axis_category %in% names(data)) {
    stop("Incorrect grouping variable! Currently implemented for `family` or `dataset_id`")
  }
  
  # define grouping variable, ordered by group-level by mean values
  # use log_value where possible
  
  if(min(data$value, na.rm=TRUE) > 0) {
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
    data <- dplyr::mutate(data, colour = ifelse(Group %in% highlight, "c", colour))
  }
  

  vals <- list(minimum = purrr::pluck(austraits_trait, "definitions", trait_name, "allowed_values_min"),
           maximum = purrr::pluck(austraits_trait, "definitions", trait_name, "allowed_values_max"))
  
  range <- (vals$maximum/vals$minimum)
  
  # Check range on y-axis
  y.text <- ifelse(n_group > 20, 0.75, 1)
  heights = c(1, max(1, n_group/7))
  
  # Top plot - plain histogram of data
  p1 <-
    ggplot2::ggplot(data, ggplot2::aes(x=value)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), color="darkgrey", fill="darkgrey", bins=50) +
    ggplot2::geom_density(color="black") +
    ggplot2::xlab("") + ggplot2::ylab("All data") +
    ggplot2::theme_bw()  +
    ggplot2::theme(legend.position = "none",
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          axis.ticks.y= ggplot2::element_blank(),
          axis.text= ggplot2::element_blank(),
          panel.background = ggplot2::element_blank()
    )
  # Second plot -- dots by groups, using ggbeeswarm package
  p2 <-
    ggplot2::ggplot(data, ggplot2::aes(x = value, y = Group, colour = colour, shape = shapes)) +
    ggbeeswarm::geom_quasirandom(orientation = 'x') +
    ggplot2::ylab(paste("By ", y_axis_category)) +
    # inclusion of custom shapes: for min, mean, unknown
    # NB: this single line of code makes function about 4-5 slower for some reason
    ggplot2::scale_shape_manual(values = my_shapes) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size=ggplot2::rel(1.25)),
          axis.text.y = ggplot2::element_text(size=ggplot2::rel(y.text))
    ) #+
   # guides(colour=FALSE)

  
  if(hide_ids) {
    p2 <- p2 + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }
  
  #Sourced from https://gist.github.com/bbolker/5ba6a37d64b06a176e320b2b696b6733
  scientific_10 <- function(x,suppress_ones=TRUE) {
    s <- scales::scientific_format()(x)
    ## substitute for exact zeros
    s[s=="0e+00"] <- "0"
    ## regex: [+]?  = "zero or one occurrences of '+'"
    s2 <- gsub("e[+]?", " %*% 10^", s )
    ## suppress 1 x
    if (suppress_ones) s2 <- gsub("1 %\\*% +","",s2)
    parse(text=s2)
  }
  
  # Define scale on x-axis and transform to log if required
  if(vals$minimum !=0 & range > 20) {
    #log transformation
    p1 <- p1 +
      ggplot2::scale_x_log10(name="",
                    breaks = scales::breaks_log(),
                    labels = scientific_10,
                    limits=c(vals$minimum, vals$maximum))
    p2 <- p2 +
      ggplot2::scale_x_log10(name=paste(trait_name, ' (', data$unit[1], ')'),
                    breaks = scales::breaks_log(),
                    labels = scientific_10,
                    limits=c(vals$minimum, vals$maximum))
  } else {
    p1 <- p1 + ggplot2::scale_x_continuous(limits=c(vals$minimum, vals$maximum))
    p2 <- p2 + ggplot2::scale_x_continuous(limits=c(vals$minimum, vals$maximum)) +
      ggplot2::xlab(paste(trait_name, ' (', data$unit[1], ')'))
    
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
