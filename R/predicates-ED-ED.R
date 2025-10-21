#' Transfers received by an emergency department.
#'
#' `ed_transfers_received` considers two presentations linked where the recorded
#' source of referral for a presentation is another hospital or an emergency
#' department, and there is a presentation ending soon earlier. The maximum time
#' allowed between presentations is adjustable, as are the codes for transfer.
#'
#' @param actual_departure_parent The actual departure timestamp from the
#'   candidate parent presentation.
#' @param arrival_child The arrival timestamp from the candidate child
#'   presentation.
#' @param source_of_referral_child The recorded source of referral code in the
#'   candidate child presentation.
#' @param transfer_time Time allowed for transfers between emergency
#'   departments.
#' @param referral_codes The codes in `source_of_referral_child` that indicate
#'   an inter-department transfer may have taken place.
#'
#' @return An `rlang::expr` for use in `find_links`.
#' @export
#'
#' @family link predicates
#'
#' @references - Source of referral codes:
#'   [](http://hird.health.nsw.gov.au/hird/view_domain_values.cfm?ItemID=10810)
ed_transfers_received <- function(
    actual_departure_parent,
    arrival_child,
    source_of_referral_child,
    transfer_time = !!getOption('jnyr.eddc.time.transfer'),
    referral_codes = !!getOption('jnyr.eddc.refer.transfer')) {
  rlang::expr(
    {{actual_departure_parent}} <= {{arrival_child}} &
      {{arrival_child}} <= {{actual_departure_parent}} + {{transfer_time}} &
      {{source_of_referral_child}} %in% {{referral_codes}}
  )
}

#' Transfers sent by an emergency department.
#'
#' `ed_transfers_sent` considers two presentations linked where the recorded
#' mode of separation for a presentation indicates departure for another
#' hospital, emergency department or health service, and there is a presentation
#' starting within six hours after departure. The maximum time allowed between
#' presentations is adjustable, as are the codes for transfer.
#'
#' @param actual_departure_parent The actual departure timestamp from the
#'   candidate parent presentation.
#' @param modesep_parent The recorded mode of separation in the candidate parent
#'   presentation.
#' @param arrival_child The arrival timestamp from the candidate child
#'   presentation.
#' @param transfer_time Time allowed for transfers between emergency
#'   departments.
#' @param transfer_codes The codes in `modesep_parent` that indicate an
#'   inter-department transfer may have taken place.
#'
#' @return An `rlang::expr` for use in `find_links`.
#' @export
#'
#' @family link predicates
#'
#' @references - Mode of separation codes:
#'   [](http://hird.health.nsw.gov.au/hird/view_domain_values.cfm?ItemID=10803)
#'
ed_transfers_sent <- function(
  actual_departure_parent,
  modesep_parent,
  arrival_child,
  transfer_time = !!getOption('jnyr.eddc.time.transfer'),
  transfer_codes = !!getOption('jnyr.eddc.modesep.transfer')) {
  rlang::expr(
    {{actual_departure_parent}} <= {{arrival_child}} &
      {{arrival_child}} <= {{actual_departure_parent}} + {{transfer_time}} &
      {{modesep_parent}} %in% {{transfer_codes}}
  )
}

#' Emergency presentations that overlap in time
#'
#' `overlap_presentations` finds presentations that have some overlap time. It
#' checks to see if the child presentation starts during the parent presentation
#' (inclusive of start and end times).
#'
#' @param arrival_parent,actual_departure_parent The start and end timestamps in
#'   the candidate parent presentation.
#' @param arrival_child The start timestamp of the candidate child presentation.
#'
#' @return An `rlang::expr` for use in `find_links`.
#'
#' @export
#' @md
#'
#' @family link predicates
overlap_presentations <- function(
  arrival_parent,
  actual_departure_parent,
  arrival_child) {
  rlang::expr(
    {{arrival_parent}} <= {{arrival_child}} &
      {{arrival_child}} <= {{actual_departure_parent}}
  )
}
