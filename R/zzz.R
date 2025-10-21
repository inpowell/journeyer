# Set default arguments ---------------------------------------------------

#' @rdname default_predicates
#' @name default_predicates
#' @section NSW:
default_predicates$NSW <- local({
  op <- options(default_pred_options$NSW)
  on.exit(options(op), add = TRUE)
  list(
    #'
    #' ## APDC-APDC links
    #'
    #' The predicates to find links within the NSW APDC are:
    #'
    APDC_APDC = list(
      #' - \code{\link{overlap_episodes}}
      overlap_episodes = overlap_episodes(
        episode_start_parent, episode_end_parent,
        episode_start_child
      ),
      #' - \code{\link{transfers_in}}, with facilities required to match
      transfers_in = transfers_in(
        episode_end_parent, facility_identifier_recode_parent,
        episode_start_child, source_of_referral_recode_child, facility_trans_from_recode_child,
        facilities_must_match = TRUE
      ),
      #' - \code{\link{transfers_out}}, with facilities required to match
      transfers_out = transfers_out(
        episode_end_parent, mode_of_separation_recode_parent, facility_trans_to_recode_parent,
        episode_start_child, facility_identifier_recode_child,
        facilities_must_match = TRUE
      ),
      #' - \code{\link{transfers_both}}, where facilities do not have to match
      transfers_both = transfers_both(
        episode_end_parent, mode_of_separation_recode_parent,
        facility_identifier_recode_parent, facility_trans_to_recode_parent,
        episode_start_child, source_of_referral_recode_child,
        facility_identifier_recode_child, facility_trans_from_recode_child,
        facilities_must_match = FALSE
      ),
      #' - \code{\link{type_change}}, where parent ends with a type change
      #'   separation and child starts with type change admission in the same
      #'   facility
      type_change = type_change(
        episode_end_parent, mode_of_separation_recode_parent, facility_identifier_recode_parent,
        episode_start_child, source_of_referral_recode_child, facility_identifier_recode_child,
        require = 'both'
      )
    ),
    #'
    #' ## EDDC-APDC links
    #'
    #' `r lifecycle::badge('experimental')`
    #'
    #' The predicates to find links between the NSW EDDC and the NSW APDC are:
    #'
    EDDC_APDC = list(
      #' - \code{\link{overlap_episodes}}, where the admission starts during a
      #' presentation
      ap_during_ed = overlap_episodes(
        arrival, actual_departure,
        episode_start
      ),
      #' - \code{\link{overlap_episodes}}, where the presentation starts during
      #' an admission
      ed_during_ap = overlap_episodes(
        episode_start, episode_end,
        arrival
      ),
      #' - \code{\link{ap_from_ed_received}}, where presentation and admission
      #' must be in the same hospital
      ap_from_ed_received = ap_from_ed_received(
        arrival, actual_departure, facility_identifier,
        episode_start, source_of_referral_recode, ed_status, facility_identifier_recode,
        facilities_must_match = TRUE
      ),
      #' - \code{\link{ap_from_ed_sent}}, where the presentation and admission
      #' must be in the same hospital
      ap_from_ed_sent = ap_from_ed_sent(
        arrival, actual_departure, mode_of_separation, facility_identifier,
        episode_start, facility_identifier_recode,
        facilities_must_match = TRUE
      ),
      #' - \code{\link{ap_from_ed_both}}, where the presentation and admission
      #' can be in separate hospitals
      ap_from_ed_both = ap_from_ed_both(
        arrival, actual_departure, mode_of_separation, facility_identifier,
        episode_start, source_of_referral_recode, ed_status, facility_identifier_recode,
        facilities_must_match = FALSE
      ),
      #' - \code{\link{ed_from_ap_sent}}
      ed_from_ap_sent = ed_from_ap_sent(
        arrival, episode_end, mode_of_separation_recode
      )
    ),
    #'
    #' ## EDDC-EDDC links
    #'
    #' `r lifecycle::badge('experimental')`
    #'
    #' The predicates to find links within the NSW EDDC are:
    #'
    EDDC_EDDC = list(
      #' - \code{\link{ed_transfers_sent}}
      ed_transfers_sent = ed_transfers_sent(
        actual_departure_parent, mode_of_separation_parent,
        arrival_child
      ),
      #' - \code{\link{ed_transfers_received}}
      ed_transfers_received = ed_transfers_received(
        actual_departure_parent,
        arrival_child, ed_source_of_referral_child
      ),
      #' - \code{\link{overlap_presentations}}
      overlap_presentations = overlap_presentations(
        arrival_parent, actual_departure_parent,
        arrival_child
      )
    )
  )
})

# Package load hook -------------------------------------------------------

.onLoad <- function(lib, pkg) {
  newop <- default_pred_options$NSW
  op <- options()
  toset <- !names(newop) %in% names(op)
  if (any(toset)) options(newop[toset])

  invisible()
}
