test_that("outputs are consistent with versions", {
  expect_snapshot(print.traits.build(austraits_5.0.0_lite))
  expect_snapshot(print.traits.build(austraits_3.0.2_lite))
})
