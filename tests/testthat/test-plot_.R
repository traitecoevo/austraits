skip_if_not_installed("ggplot2")
skip_if_not_installed("ggpointdensity")
skip_if_not_installed("viridis")

sites <- tibble::tibble(
  trait_name = "leaf_area",
  location_name = c("Perth", "Brisbane", "Byron Bay", "Torres Strait", "Lord Howe Island", "Kuala Lumpur"),
  `latitude (deg)` = c(-31.95, -27.47, -28.64, -9.4, -31.55, 3.14),
  `longitude (deg)` = c(115.86, 153.03, 153.61, 142.2, 159.08, 101.69)
)
shown <- function(p) sort(unique(p$layers[[length(p$layers)]]$data$location_name))

test_that("plot_locations draws over the map of Australia by default", {
  # Brisbane and Byron Bay (east of 153) and the Torres Strait (north of -9.5)
  # are on the map; locations beyond it are reported
  expect_message(p <- plot_locations(sites), "2 record\\(s\\) at 2 location\\(s\\) lie beyond the map")
  expect_equal(shown(p), sort(c("Perth", "Brisbane", "Byron Bay", "Torres Strait")))
  expect_length(p$layers, 2)
  expect_equal(nrow(p$layers[[1]]$data), nrow(australia_map_raster))

  # A database from outside Australia says to use basemap = NULL
  expect_error(plot_locations(sites[sites$location_name == "Kuala Lumpur", ]), "Use `basemap = NULL`")
})

test_that("plot_locations maps a traits.build database, or its traits table", {
  leaf_area <- traits.use::austraits_lite |>
    extract_trait("leaf_area") |>
    join_location_coordinates()
  located <- leaf_area$traits |>
    dplyr::filter(!is.na(.data[["latitude (deg)"]]), !is.na(.data[["longitude (deg)"]]))
  expect_gt(nrow(located), 1000)

  # Every leaf area location in austraits_lite lies on the map of Australia
  expect_no_message(p <- plot_locations(leaf_area, "basis_of_record"))
  expect_s3_class(p, "ggplot")
  points <- p$layers[[length(p$layers)]]$data
  expect_equal(nrow(points), nrow(located))
  expect_setequal(points$basis_of_record, unique(located$basis_of_record))

  expect_no_message(q <- plot_locations(leaf_area$traits, "basis_of_record"))
  expect_equal(q$layers[[length(q$layers)]]$data, points)
})

test_that("plot_locations draws without a map, or over another map", {
  p <- plot_locations(sites, basemap = NULL)
  expect_equal(shown(p), sort(sites$location_name))
  expect_length(p$layers, 1)

  # feature = NA draws one map instead of one per feature
  p <- plot_locations(sites, feature = NA, basemap = NULL)
  expect_equal(shown(p), sort(sites$location_name))

  # Any other map is passed on to traits.use::plot_locations()
  malaysia <- expand.grid(x = seq(99, 120, length.out = 10), y = seq(0, 8, length.out = 10))
  expect_message(p <- plot_locations(sites, basemap = malaysia), "5 record\\(s\\) at 5 location\\(s\\) lie beyond the map")
  expect_equal(shown(p), "Kuala Lumpur")

  expect_error(plot_locations(sites, basemap = "malaysia"), "`basemap` must be")
})
