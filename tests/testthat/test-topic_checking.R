context("mi and imi")

load(system.file("extdata/sotu50.Rdata", package = "topicmodeltoolbox"))

test_that(desc="imi",{
  imi_test <- imi(sotu50)
  expect_equal(imi_test$imi[1:2], c(0.2901117, 0.7248372), tolerance = .00001)
})

test_that(desc="mi",{
  mi_test <- mi(sotu50)
  expect_equal(mi_test$mi[1:2], c(3.333453, 3.436727), tolerance = .00001)
})

