library(DBI)
library(dplyr)
library(dbplyr)

# Set up datasets
EDDC <- setDT(tibble::tribble(
  ~RL_ID, ~PPN,           ~arrival,  ~actual_departure, ~mode_of_separation, ~source_of_referral,
  'E1',      1, '2022-01-01 12:00', '2022-01-02 09:00',                 '1',                '01',
  'E2',      1, '2022-01-02 09:15', '2022-01-02 12:00',                '01',                '01',
  'E3',      2, '2022-01-01 12:00', '2022-01-02 09:00',                 '1',                '01',
  'E4',      2, '2022-01-01 09:15', '2022-01-01 13:00',                '01',                '01',
  'E5',      3, '2022-01-01 12:00', '2022-01-01 15:00',                '05',                '01',
  'E6',      3, '2022-01-01 18:30', '2022-01-01 23:36',                 '1',                '06'
))
EDDC[, c('arrival', 'actual_departure') :=
       lapply(.(arrival, actual_departure), as.POSIXct)]

# Set up connections
cons <- vector('list')

if (requireNamespace('duckdb')) {
  cons[['duckdb']] <- dbConnect(duckdb::duckdb())
  withr::defer(dbDisconnect(cons[['duckdb']], shutdown = TRUE))
}

for (pkg in names(cons)) {
  con <- cons[[pkg]]
  DBI::dbWriteTable(con, 'EDDC', EDDC)

  test_that("overlap_presentations identifies overlapping ED episodes_correctly", {
    expected <- setDT(tibble::tribble(
      ~RL_ID_parent, ~RL_ID_child,
      'E4', 'E3'
    ))

    EDDC <- tbl(con, 'EDDC')

    observed <- inner_join(
      EDDC, EDDC,
      by = 'PPN',
      suffix = c("_parent", "_child")
    ) %>%
      filter(RL_ID_parent != RL_ID_child) %>%
      find_links(
        overlap = !!overlap_presentations(arrival_parent, actual_departure_parent, arrival_child)
      ) %>%
      select(RL_ID_parent, RL_ID_child) %>%
      arrange(RL_ID_parent, RL_ID_child)

    expect_equal(
      setDT(dplyr::collect(observed)),
      expected
    )
  })

  test_that("ed_transfers_received identifies ED transfers", {
    expected <- setDT(tibble::tribble(
      ~RL_ID_parent, ~RL_ID_child,
      'E5', 'E6'
    ))

    EDDC <- tbl(con, 'EDDC')

    observed <- inner_join(
      EDDC, EDDC,
      by = 'PPN',
      suffix = c("_parent", "_child")
    ) %>%
      filter(RL_ID_parent != RL_ID_child) %>%
      find_links(
        trans = !!ed_transfers_received(
          actual_departure_parent,
          arrival_child, source_of_referral_child
        )
      ) %>%
      select(RL_ID_parent, RL_ID_child) %>%
      arrange(RL_ID_parent, RL_ID_child)

    expect_equal(
      setDT(dplyr::collect(observed)),
      expected
    )
  })

  test_that("ed_transfers_sent identifies ED transfers", {
    expected <- setDT(tibble::tribble(
      ~RL_ID_parent, ~RL_ID_child,
      'E5', 'E6'
    ))

    EDDC <- tbl(con, 'EDDC')

    observed <- inner_join(
      EDDC, EDDC,
      by = 'PPN',
      suffix = c("_parent", "_child")
    ) %>%
      filter(RL_ID_parent != RL_ID_child) %>%
      find_links(
        trans = !!ed_transfers_sent(
          actual_departure_parent, mode_of_separation_parent,
          arrival_child
        )
      ) %>%
      select(RL_ID_parent, RL_ID_child) %>%
      arrange(RL_ID_parent, RL_ID_child)

    expect_equal(
      setDT(dplyr::collect(observed)),
      expected
    )
  })
}
