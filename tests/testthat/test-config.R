# tests/testthat/test-config.R
# Data-free guardrails: fail loudly if analysis_config.yaml structure drifts.
# If you change the config ON PURPOSE, update the corresponding expectation.

find_config <- function() {
  cands <- c("analysis_config.yaml", file.path("..", "..", "analysis_config.yaml"))
  hit <- cands[file.exists(cands)]
  if (!length(hit)) testthat::skip("analysis_config.yaml not found")
  hit[1]
}
cfg <- yaml::read_yaml(find_config())

test_that("global settings are intact", {
  expect_equal(cfg$global$item_pattern, "^Q\\d{2}$")
  expect_lt(cfg$global$scale$min, cfg$global$scale$max)
  expect_equal(cfg$global$scale$min, 1)
  expect_equal(cfg$global$scale$max, 4)
})

test_that("active dataset is a non-empty string", {
  expect_true(is.character(cfg$data_source$dataset) && nzchar(cfg$data_source$dataset))
})

test_that("scale_structure for items_30-Q14-Q19 has not drifted", {
  ss <- cfg$scale_structure[["items_30-Q14-Q19"]]
  expect_false(is.null(ss))
  expect_setequal(names(ss), c("factor_1","factor_2","factor_3","factor_4","factor_5"))
  expect_equal(ss$factor_1$items, c("Q10","Q12","Q15","Q16"))
  expect_equal(ss$factor_2$items, c("Q01","Q02","Q03"))
  expect_equal(ss$factor_3$items, c("Q07","Q11","Q13","Q24","Q25","Q30"))
  expect_equal(ss$factor_4$items,
               c("Q04","Q05","Q06","Q08","Q09","Q17","Q18","Q20","Q21","Q22","Q23"))
  expect_equal(ss$factor_5$items, c("Q26","Q27","Q28","Q29"))
})

test_that("all subscale items match the item pattern", {
  ss <- cfg$scale_structure[["items_30-Q14-Q19"]]
  items <- unlist(lapply(ss, `[[`, "items"), use.names = FALSE)
  expect_true(all(grepl(cfg$global$item_pattern, items)))
})

test_that("validity hypotheses are well-formed (target/proximal/distal)", {
  h <- cfg$validity_hypotheses
  expect_gte(length(h), 1)
  for (nm in names(h))
    expect_true(all(c("target","proximal","distal") %in% names(h[[nm]])), info = nm)
})
