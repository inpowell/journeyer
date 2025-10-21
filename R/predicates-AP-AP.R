#' Episode linkage predicate calls
#'
#' Episode linkage predicate calls identify which episodes have transfers
#' between them. All of these calls assume that the person is the same for each
#' episode, meaning they can be included in a query after an inner join on PPN.
#' These episode linkage predicates are designed for use with `dplyr` and
#' `dbplyr` interfacing with SQL.
#'
#' @section Inputs:
#'
#'   The inputs to any SQL episode call generator should be unquoted columns,
#'   possibly operated on by database-internal SQL macros. See
#'   \code{\link[dbplyr]{partial_eval}} in the `dbplyr` package for more
#'   information.
#'
#' @section Value:
#'
#'   Every episode linkage function should return a call which, when evaluated
#'   by `dbplyr`, returns true if a pair of records with matching PPNs has a
#'   transfer between them, and false otherwise.
#'
#' @name LinkPredicates
#' @md
#' @family link predicates
NULL

#' Find links based on predicates using `dbplyr`
#'
#' `find_links` is used to find links between datasets according to a number of
#' user-specified predicates. By default, `dplyr::filter` takes the intersection
#' of all arguments, whereas this function returns the union.
#'
#' @param .data The input data to filter, which should usually be a lazy `tbl`
#'   from the `dbplyr` package.
#' @param ... Expressions that return a logical value, in terms of the variables
#'   of `.data`. Links are returned if they satisfy ANY of these expressions,
#'   which differs from the behaviour of, for example, `dplyr::filter`.
#' @param .keep If this is true, then the columns of predicates are retained in
#'   the output data. By default, they are dropped.
#'
#' @return `find_links` returns an object of the same type as `.data`.
#'
#' @export
#' @md
#' @family link predicates
find_links <- function(.data, ..., .keep = FALSE) {
  # Quote calls -- we defuse until the very end, where expressions are evaluated
  # by dplyr::filter
  preds <- rlang::enquos(..., .named = TRUE)

  # Evaluate predicates into new columns
  .data <- dplyr::mutate(.data, !!!preds)

  # Combine calls to return union/logical disjunction of predicates
  unioncl <- Reduce(function(x, y) call('|', x, y), rlang::syms(names(preds)))

  .data <- dplyr::filter(.data, !!unioncl)

  if (!.keep)
    .data <- dplyr::select(.data, !dplyr::all_of(names(preds)))

  .data
}

# Arrange functions in alphabetical order ---------------------------------

#' Episodes that overlap in time
#'
#' `overlap_episodes` finds episodes that have some overlap in their admission
#' times. It checks to see if the child episode starts during the parent episode
#' (inclusive of start and end times).
#'
#' @param episode_start_parent,episode_end_parent The start and end timestamps
#'   in the candidate parent episode.
#' @param episode_start_child The start timestamp of the candidate
#'   child episode.
#'
#' @return `overlap_episodes` returns a call to be used in `dplyr`/`dbplyr`
#'   filters.
#'
#' @export
#' @md
#'
#' @family link predicates
overlap_episodes <- function(
    episode_start_parent,
    episode_end_parent,
    episode_start_child) {
  rlang::expr(
    {{episode_start_parent}} <= {{episode_start_child}} &
      {{episode_start_child}} <= {{episode_end_parent}}
  )
}

#' Explicit transfers within and between hospitals
#'
#' `sameday_transfer` identifies transfers where the source/parent episode
#' explicitly ends with a type-change or transfer to another hospital, and the
#' destination/child episode starts on the same day.
#'
#' @param modesep_parent The recorded mode of separation in the
#'   candidate parent episode.
#' @param transfer_codes The codes in `modesep_parent` that correspond to
#'   transfers in the same day.
#' @param episode_start_parent,episode_end_parent The start and end timestamps
#'   in the candidate parent episode.
#' @param episode_start_child The start timestamp of the candidate
#'   child episode.
#'
#' @return `sameday_transfer` returns a call to be used in `dplyr`/`dbplyr`
#'   filters.
#'
#' @export
#' @md
#'
#' @family link predicates
sameday_transfer <- function(
    episode_start_parent,
    episode_end_parent,
    modesep_parent,
    episode_start_child,
    transfer_codes = !!getOption('jnyr.apdc.modesep.sameday')) {
  rlang::expr(as.Date({{episode_end_parent}}) == as.Date({{episode_start_child}}) &
                {{episode_start_parent}} <= {{episode_start_child}} &
                {{modesep_parent}} %in% {{transfer_codes}})
}

#' Episodes with same start date and time
#'
#' `samestart` identifies transfers where two episodes have the exact same start
#' date and time. The child episode finishes on or before the end of the parent
#' episode.
#'
#' @param episode_start_parent,episode_end_parent The start and end timestamps
#'   in the candidate parent episode.
#' @param episode_start_child,episode_end_child The start and end timestamps in
#'   the candidate child episode.
#'
#' @return `samestart` returns a call to be used in `dplyr`/`dbplyr` filters.
#'
#' @export
#' @md
#'
#' @family link predicates
samestart <- function(
    episode_start_parent,
    episode_end_parent,
    episode_start_child,
    episode_end_child) {
  rlang::expr({{episode_start_parent}} == {{episode_start_child}} &
                {{episode_end_parent}} >= {{episode_end_child}})
}

#' Transfers where child episode has evidence of transfer
#'
#' `transfers_in` finds transfers between episodes, where the child episode has
#' evidence of a transfer coming in. In this function, the parent episode does
#' not require evidence of a transfer going out. However, by default the parent
#' episode's facility must match the child episode's facility transferred from,
#' but this can be disabled with the `facilities_must_match` argument.
#'
#' @param episode_end_parent End timestamp of candidate parent episode.
#' @param facility_parent Facility of candidate parent episode.
#' @param episode_start_child Start timestamp of candidate child episode.
#' @param facility_from_child In the child episode, the recorded facility the
#'   patient was transferred from.
#' @param transfer_time The time allowed between episodes for a transfer to take
#'   place, in a form suitable for your database management system.
#' @param facilities_must_match Does `facility_from_child` have to match
#'   `facility_parent` for a transfer to be indicated?
#' @param source_referral_child The source of referral for the child episode.
#' @param referral_codes The source of referral codes that correspond to a
#'   patient being transferred into an episode.
#'
#' @return `transfers_in` returns a call to be used in `dplyr`/`dbplyr` filters.
#'
#' @export
#' @md
#'
#' @family link predicates
#' @importFrom rlang call2 enquo
transfers_in <- function(
  episode_end_parent,
  facility_parent,
  episode_start_child,
  source_referral_child,
  facility_from_child,
  referral_codes = !!getOption('jnyr.apdc.refer.transfers'),
  transfer_time = !!getOption('jnyr.apdc.time.transfer'),
  facilities_must_match = TRUE
) {
  cl1 <- call2('%in%', enquo(source_referral_child), enquo(referral_codes))
  cl2 <- call2('<=', enquo(episode_end_parent), enquo(episode_start_child))
  cl3 <- call2('<=', enquo(episode_start_child), call2('+', enquo(episode_end_parent), enquo(transfer_time)))
  cl4 <- call2('==', enquo(facility_from_child), enquo(facility_parent))

  Reduce(function(x, y) call2('&', x, y), if (facilities_must_match) list(cl1, cl2, cl3, cl4) else list(cl1, cl2, cl3))
}

#' Transfers where parent episode has evidence of transfer
#'
#' `transfers_out` finds transfers between episodes, where the parent episode
#' has evidence of a transfer coming in. In this function, the child episode
#' does not require evidence of a transfer coming in. However, by default the
#' child episode's facility must match the parent episode's facility transferred
#' to, but this can be disabled with the `facilities_must_match` argument.
#'
#' @param episode_end_parent End timestamp of candidate parent episode.
#' @param episode_start_child Start timestamp of candidate child episode.
#' @param transfer_time The time allowed between episodes for a transfer to take
#'   place, in a form suitable for your database management system.
#' @param facilities_must_match Does `facility_from_child` have to match
#'   `facility_parent` for a transfer to be indicated?
#' @param modesep_parent The mode of separation of the candidate parent episode.
#' @param facility_to_parent The destination facility transferred to, as
#'   recorded in the candidate parent episode.
#' @param facility_child Facility of the candidate child episode.
#' @param transfer_codes Mode of separation codes corresponding to a transfer.
#'
#' @return `transfers_in` returns a call to be used in `dplyr`/`dbplyr` filters.
#'
#' @export
#' @md
#'
#' @family link predicates
#' @importFrom rlang call2 enquo
transfers_out <- function(
  episode_end_parent,
  modesep_parent,
  facility_to_parent,
  episode_start_child,
  facility_child,
  transfer_codes = !!getOption('jnyr.apdc.modesep.transfers'),
  transfer_time = !!getOption('jnyr.apdc.time.transfer'),
  facilities_must_match = TRUE
) {
  # (1) modesep_parent %in% transfer_codes
  # (2) 0 <= !!getOption('jnyr.apdc.start.child') - episode_end_parent
  # (3) episode_start_child - episode_end_parent <= transfer_time
  # (4, sometimes) facility_child == facility_to_parent

  cl1 <- call2('%in%', enquo(modesep_parent), enquo(transfer_codes))
  cl2 <- call2('<=', enquo(episode_end_parent), enquo(episode_start_child))
  cl3 <- call2('<=', enquo(episode_start_child), call2('+', enquo(episode_end_parent), enquo(transfer_time)))
  cl4 <- call2('==', enquo(facility_child), enquo(facility_to_parent))
  cl5 <- call2('!', call2('is.na', enquo(facility_to_parent)))

  Reduce(function(x, y) call2('&', x, y), if (facilities_must_match) list(cl1, cl2, cl3, cl4, cl5) else list(cl1, cl2, cl3))
}

#' Transfers where both parent and child episodes have evidence of transfer
#'
#' `transfers_both` finds transfers between episodes, where the parent episode
#' has evidence of a transfer coming in and the child episode has evidence of a
#' transfer going out. Optionally, this function can require the appropriate
#' facility columns to match, but by default does not.
#'
#' @param episode_end_parent End timestamp of candidate parent episode.
#' @param modesep_parent The mode of separation of the candidate parent episode.
#' @param facility_parent Facility of candidate parent episode.
#' @param facility_to_parent The destination facility transferred to, as
#'   recorded in the candidate parent episode.
#' @param episode_start_child Start timestamp of candidate child episode.
#' @param facility_child Facility of the candidate child episode.
#' @param source_referral_child The source of referral for the child episode.
#' @param referral_codes The source of referral codes that correspond to a
#'   patient being transferred into an episode.
#' @param facility_from_child In the child episode, the recorded facility the
#'   patient was transferred from.
#' @param transfer_time The time allowed between episodes for a transfer to take
#'   place, in a form suitable for your database management system.
#' @param facilities_must_match Does `facility_from_child` have to match
#'   `facility_parent` for a transfer to be indicated?
#' @param transfer_codes Mode of separation codes corresponding to a transfer.
#'
#' @return `transfers_in` returns a call to be used in `dplyr`/`dbplyr` filters.
#'
#' @export
#' @md
#'
#' @family link predicates
#' @importFrom rlang call2 enquo
transfers_both <- function(
    # Parent columns
    episode_end_parent,
    modesep_parent,
    facility_parent,
    facility_to_parent,
    # Child columns
    episode_start_child,
    source_referral_child,
    facility_child,
    facility_from_child,
    # Parameters
    referral_codes = !!getOption('jnyr.apdc.refer.transfers'),
    transfer_codes = !!getOption('jnyr.apdc.modesep.transfers'),
    transfer_time = !!getOption('jnyr.apdc.time.transfer'),
    facilities_must_match = FALSE
) {
  tin <- transfers_in(
    {{ episode_end_parent }}, {{ facility_parent }},
    {{ episode_start_child }}, {{ source_referral_child }}, {{ facility_from_child }},
    {{ referral_codes }},  {{ transfer_time }}, facilities_must_match
  )
  tout <- transfers_out(
    {{ episode_end_parent }}, {{ modesep_parent }}, {{ facility_to_parent }},
    {{ episode_start_child }}, {{ facility_child }},
    {{ transfer_codes }}, {{ transfer_time }}, facilities_must_match
  )

  call2('&', tin, tout)
}

#' Type-change separations and admissions
#'
#' `type_change` finds type changes which correspond to a new episode in the
#' same period of hospital care where a patient's type of care has changed. This
#' change in care can result in a new record. By default, the parent episode
#' must end in a type change separation (TCS), and the child episode must start
#' with a type change admission (TCA), and both episodes must be in the same
#' facility.
#'
#' @param episode_end_parent End timestamp of candidate parent episode.
#' @param modesep_parent The mode of separation of the candidate parent episode.
#' @param facility_parent Facility of candidate parent episode.
#' @param episode_start_child Start timestamp of candidate child episode.
#' @param facility_child Facility of the candidate child episode.
#' @param source_referral_child The source of referral for the child episode.
#' @param separation_codes Mode of separation codes corresponding to a type
#'   change.
#' @param referral_codes The source of referral codes that correspond to a
#'   patient starting an episode with a type change.
#' @param transfer_time The time allowed between episodes for a type change to
#'   take place, in a form suitable for your database management system.
#' @param facilities_must_match Does `facility_parent` have to match
#'   `facility_child` for a link to be eligible for a type change?
#' @param require Whether evidence of a either a type change admission,
#'   separation, or both is required.
#'
#' @return `type_change` returns a call to be used in `dplyr`/`dbplyr` filters.
#' @export
#' @md
#'
#' @family link predicates
#' @importFrom rlang call2
type_change <- function(
    # Parent columns
    episode_end_parent,
    modesep_parent,
    facility_parent,
    # Child columns
    episode_start_child,
    source_referral_child,
    facility_child,
    # Parameters
    separation_codes = !!getOption('jnyr.apdc.modesep.tcs'),
    referral_codes = !!getOption('jnyr.apdc.refer.tcs'),
    transfer_time = !!getOption('jnyr.apdc.time.tcs'),
    facilities_must_match = TRUE,
    require = c('both', 'separation', 'referral')) {
  ### Predicate components
  tcs_time <- rlang::expr(
    {{ episode_end_parent }} - {{ transfer_time }} <= {{ episode_start_child }} &
      {{ episode_start_child }} <= {{ episode_end_parent }} + {{ transfer_time }}
  )
  tcs_refer <- rlang::expr({{ source_referral_child }} %in% {{ referral_codes }})
  tcs_sep <- rlang::expr({{ modesep_parent }} %in% {{ separation_codes }})
  tcs_fac <- rlang::expr({{ facility_parent }} == {{ facility_child }})

  ### Combining
  comb <- list(tcs_time)

  require <- match.arg(require)
  if (identical(require, 'both')) {
    comb <- c(comb, list(tcs_refer, tcs_sep))
  } else if (identical(require, 'separation')) {
    comb <- c(comb, list(tcs_sep))
  } else if (identical(require, 'referral')) {
    comb <- c(comb, list(tcs_refer))
  } else {
    stop("Assertion error: impossible case")
  }

  if (facilities_must_match) {
    comb <- c(comb, list(tcs_fac))
  }

  Reduce(function(x, y) call2('&', x, y), comb)
}
