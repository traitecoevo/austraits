not_supported_austraits <- list(austraits_3.0.2_lite, austraits_4.2.0_lite)


test_that("Function doesn't throw error", {
  
  #expect_invisible(austraits_5.0.0_lite %>% plot_trait_distribution_beeswarm("leaf_mass_per_area", "dataset_id", "Bloomfield_2018"))
  
  expect_invisible(austraits_5.0.0_lite %>% plot_trait_distribution_beeswarm("leaf_mass_per_area", "dataset_id"))
  
  # this function is currently really slow, blocking effective testing
  expect_invisible((austraits_5.0.0_lite %>% extract_trait("wood_density") %>% join_location_coordinates())$trait %>% plot_locations())
  expect_invisible(austraits_5.0.0_lite %>% extract_trait("wood_density") %>% plot_locations())
 }) 



test_non_compatibile <- function(austraits){
  test_that("Throws correct errors for deprecated or non-compatible",{
    expect_error(austraits %>% 
                   extract_dataset("Falster_2003") %>% 
                   purrr::pluck("traits") %>% plot_locations()
    )
    
    expect_error(austraits %>% 
                   extract_dataset("Falster_2003") %>% 
                   purrr::pluck("traits") %>% plot_site_locations()
    )
  })
}

purrr::walk(not_supported_austraits,
            test_non_compatibile)

test_that("plot_locations keeps every location on the map of Australia", {
  sites <- tibble::tibble(
    trait_name = "leaf_area",
    location_name = c("Perth", "Brisbane", "Byron Bay", "Torres Strait", "Lord Howe Island", "Kuala Lumpur"),
    `latitude (deg)` = c(-31.95, -27.47, -28.64, -9.4, -31.55, 3.14),
    `longitude (deg)` = c(115.86, 153.03, 153.61, 142.2, 159.08, 101.69)
  )
  shown <- function(p) sort(unique(p$layers[[length(p$layers)]]$data$location_name))

  # Brisbane and Byron Bay (east of 153) and the Torres Strait (north of -9.5)
  # used to be dropped silently; locations beyond the map are reported
  expect_message(p <- plot_locations(sites), "2 record\\(s\\) at 2 location\\(s\\) lie beyond the map")
  expect_equal(shown(p), sort(c("Perth", "Brisbane", "Byron Bay", "Torres Strait")))

  # Without the map, every location is shown
  p <- plot_locations(sites, basemap = NULL)
  expect_equal(shown(p), sort(sites$location_name))
  expect_length(p$layers, 1)

  # Coordinates that are not numbers are left out rather than failing
  sites_bad <- sites
  sites_bad$`latitude (deg)` <- as.character(sites_bad$`latitude (deg)`)
  sites_bad$`latitude (deg)`[1] <- "unknown"
  p <- plot_locations(sites_bad, basemap = NULL)
  expect_false("Perth" %in% shown(p))

  # feature = NA draws one map instead of one per feature
  p <- plot_locations(sites, feature = NA, basemap = NULL)
  expect_equal(shown(p), sort(sites$location_name))

  expect_error(plot_locations(sites, basemap = "malaysia"), "`basemap` must be")
})

test_that("Function throws error", {
  expect_error(austraits_5.0.0_lite$traits |> plot_locations())
  expect_error(austraits_3.0.2_lite$traits |> plot_site_locations())
  expect_error(austraits_3.0.2_lite |> plot_site_locations())
  expect_error(austraits_3.0.2_lite |> plot_trait_distribution_beeswarm())
  expect_error(austraits_5.0.0_lite |> plot_trait_distribution_beeswarm("branch_mass_fraction", y_axis_category = "location_property"))
}) 


# Tear down code
unlink("Rplots.pdf")