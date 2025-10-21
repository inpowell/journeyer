library(dplyr)

default_data <- list(
  NSW = list(
    APDC = tibble(
      ID = character(),
      PPN = integer(),
      source_of_referral_recode = character(),
      mode_of_separation_recode = character(),
      episode_start = lubridate::as_datetime(numeric()),
      episode_end = lubridate::as_datetime(numeric()),
      facility_identifier_recode = character(),
      facility_trans_from_recode = character(),
      facility_trans_to_recode = character(),
      ed_status = character()
    ),
    EDDC = tibble(
      ID = character(),
      PPN = integer(),
      arrival = lubridate::as_datetime(numeric()),
      actual_departure = lubridate::as_datetime(numeric()),
      ed_source_of_referral = character(),
      mode_of_separation = character(),
      facility_identifier = character()
    )
  )
)

# NSW ---------------------------------------------------------------------

test_that('transfers algorithm runs with NSW APDC-APDC defaults', {
  apdc <- default_data$NSW$APDC

  expected <- tibble(ID_parent = character(), ID_child = character())

  observed <- inner_join(apdc, apdc, by = 'PPN', suffix = c('_parent', '_child')) %>%
    filter(ID_parent != ID_child) %>%
    find_links(!!!default_predicates$NSW$APDC_APDC) %>%
    select(ID_parent, ID_child)

  expect_equal(observed, expected)
})

test_that('transfers algorithm runs with NSW EDDC-APDC defaults', {
  apdc <- default_data$NSW$APDC
  eddc <- default_data$NSW$EDDC

  expected <- tibble(ID_parent = character(), ID_child = character())

  observed <- inner_join(eddc, apdc, by = 'PPN', suffix = c('_parent', '_child')) %>%
    filter(ID_parent != ID_child) %>%
    find_links(!!!default_predicates$NSW$EDDC_APDC) %>%
    select(ID_parent, ID_child)

  expect_equal(observed, expected)
})

test_that('transfers algorithm runs with NSW EDDC-EDDC defaults', {
  eddc <- default_data$NSW$EDDC

  expected <- tibble(ID_parent = character(), ID_child = character())

  observed <- inner_join(eddc, eddc, by = 'PPN', suffix = c('_parent', '_child')) %>%
    filter(ID_parent != ID_child) %>%
    find_links(!!!default_predicates$NSW$EDDC_EDDC) %>%
    select(ID_parent, ID_child)

  expect_equal(observed, expected)
})
