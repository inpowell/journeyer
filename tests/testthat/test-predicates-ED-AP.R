library(DBI)
library(dplyr)
library(dbplyr)

# Set up datasets
APDC <- setDT(tibble::tribble(
  ~RL_ID, ~PPN,        ~episode_start,          ~episode_end, ~facility_identifier_recode, ~source_of_referral_recode, ~ed_status, ~mode_of_separation_recode,
  'A1',      1, '2022-01-01 15:00:00', '2022-02-01 12:00:00',                      'A111',                       '01',        '2',                        '1',
  'A2',      2, '2022-01-01 12:00:00', '2022-02-01 12:00:00',                      'A111',                       '01',        '1',                        '1',
  'A3',      3, '2022-01-01 11:55:00', '2022-02-01 12:00:00',                      'A111',                       '06',        '5',                        '1',
  'A4',      4, '2022-01-01 15:00:00', '2022-02-01 12:00:00',                      'A111',                       '14',        '4',                        '1',
  'A5',      5, '2022-01-01 20:59:00', '2022-02-01 12:00:00',                      'B222',                       '05',        '3',                        '1',
  'A6',      6, '2022-01-01 15:00:00', '2022-02-01 12:00:00',                      'A111',                       '01',        '3',                        '1',
  'A7',      7, '2021-01-01 13:00:00', '2021-01-15 11:30:00',                      'C333',                       '01',        '1',                        '5'
))
APDC[, c('episode_start', 'episode_end') :=
       lapply(.(episode_start, episode_end), as.POSIXct)]

EDDC <- setDT(tibble::tribble(
  ~RL_ID, ~PPN,           ~arrival,  ~actual_departure, ~facility_identifier, ~modesep,
  'E1',      1, '2022-01-01 12:00', '2022-01-01 15:00',               'A111',      '1',
  'E2',      2, '2022-01-01 12:00', '2022-01-01 15:00',               'A111',     '01',
  'E3',      3, '2022-01-01 12:00', '2022-01-01 15:00',               'A111',     '10',
  'E4',      4, '2022-01-01 12:00', '2022-01-01 15:00',               'A111',     '03',
  'E5',      5, '2022-01-01 12:00', '2022-01-01 15:00',               'A111',      '1',
  'E6',      6, '2022-01-01 12:00', '2022-01-01 15:00',               'A111',      '1',
  'E7',      7, '2021-01-15 16:00', '2021-01-15 22:00',               'A111',      '1'
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

  DBI::dbWriteTable(con, 'APDC', APDC)
  DBI::dbWriteTable(con, 'EDDC', EDDC)

  test_that("ap_from_ed_received identifies admissions from ED correctly", {
    expected <- setDT(tibble::tribble(
      ~RL_ID_parent, ~RL_ID_child, ~involved, ~referred, ~either,
      'E1', 'A1',  TRUE,  TRUE, TRUE,
      'E2', 'A2',  TRUE,  TRUE, TRUE,
      'E3', 'A3',  TRUE, FALSE, TRUE,
      'E4', 'A4',  TRUE, FALSE, TRUE,
      'E6', 'A6', FALSE,  TRUE, TRUE,
    ))

    APDC <- tbl(con, "APDC")
    EDDC <- tbl(con, 'EDDC')

    observed <- inner_join(
      EDDC, APDC,
      by = 'PPN',
      suffix = c("_parent", "_child")
    ) %>%
      filter(RL_ID_parent != RL_ID_child) %>%
      find_links(
        involved = !!ap_from_ed_received(
          arrival, actual_departure, facility_identifier,
          episode_start, source_of_referral_recode, ed_status, facility_identifier_recode,
          ED_requirements = 'involved'
        ),
        referred = !!ap_from_ed_received(
          arrival, actual_departure, facility_identifier,
          episode_start, source_of_referral_recode, ed_status, facility_identifier_recode,
          ED_requirements = 'referred'
        ),
        either = !!ap_from_ed_received(
          arrival, actual_departure, facility_identifier,
          episode_start, source_of_referral_recode, ed_status, facility_identifier_recode,
          ED_requirements = 'either'
        ),
        .keep = TRUE
      ) %>%
      select(RL_ID_parent, RL_ID_child, involved, referred, either) %>%
      arrange(RL_ID_parent, RL_ID_child)

    expect_equal(
      setDT(dplyr::collect(observed)),
      expected
    )
  })

  test_that("ap_from_ed_sent identifies ED separations into admissions correctly", {
    expected <- setDT(tibble::tribble(
      ~RL_ID_parent, ~RL_ID_child,
      'E1', 'A1',
      'E2', 'A2',
      'E3', 'A3',
      'E6', 'A6'
    ))

    APDC <- tbl(con, "APDC")
    EDDC <- tbl(con, 'EDDC')

    observed <- inner_join(
      EDDC, APDC,
      by = 'PPN',
      suffix = c("_parent", "_child")
    ) %>%
      filter(RL_ID_parent != RL_ID_child) %>%
      find_links(
        edap_sent = !!ap_from_ed_sent(
          arrival, actual_departure, modesep, facility_identifier,
          episode_start, facility_identifier_recode
        )
      ) %>%
      select(RL_ID_parent, RL_ID_child) %>%
      arrange(RL_ID_parent, RL_ID_child)

    expect_equal(
      setDT(dplyr::collect(observed)),
      expected
    )
  })

  test_that("ap_from_ed_both identifies admission from ED correctly", {
    expected <- setDT(tibble::tribble(
      ~RL_ID_parent, ~RL_ID_child,
      'E1', 'A1',
      'E2', 'A2',
      'E3', 'A3',
      'E6', 'A6'
    ))

    APDC <- tbl(con, "APDC")
    EDDC <- tbl(con, 'EDDC')

    observed <- inner_join(
      EDDC, APDC,
      by = 'PPN',
      suffix = c("_parent", "_child")
    ) %>%
      filter(RL_ID_parent != RL_ID_child) %>%
      find_links(
        aped_both = !!ap_from_ed_both(
          arrival, actual_departure, modesep, facility_identifier,
          episode_start, source_of_referral_recode, ed_status, facility_identifier_recode,
          facilities_must_match = FALSE
        )
      ) %>%
      select(RL_ID_parent, RL_ID_child) %>%
      arrange(RL_ID_parent, RL_ID_child)

    expect_equal(
      setDT(dplyr::collect(observed)),
      expected
    )
  })

  test_that("ed_from_ap_sent identifies transfers to ED correctly", {
    expected <- setDT(tibble::tribble(
      ~RL_ID_parent, ~RL_ID_child,
      'E7', 'A7'
    ))

    APDC <- tbl(con, "APDC")
    EDDC <- tbl(con, 'EDDC')

    observed <- inner_join(
      EDDC, APDC,
      by = 'PPN',
      suffix = c("_parent", "_child")
    ) %>%
      filter(RL_ID_parent != RL_ID_child) %>%
      find_links(
        aped_sent = !!ed_from_ap_sent(
          arrival,
          episode_end, mode_of_separation_recode
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
