#' Admissions from Emergency Departments, with evidence in admission record
#'
#' `ap_from_ed_received` identifies transfers where there is evidence of an
#' admission from an emergency department in the *receiving* admitted patient
#' record. It requires that the admission start during or slightly after the
#' presentations, and can require the facilities of each event to match and/or
#' the admitted patient record to include evidence of emergency department
#' involvement.
#'
#' This evidence can come from two situations:
#'
#' 1. The emergency department is involved in the admitted patient record
#' (`ed_status`)
#'
#' 2. The admission was referred from the emergency department
#' (`source_referral`)
#'
#' If `ED_requirements` is `'any'`, then neither of these situations need to be
#' satisfied. If `'either'`, then either (1) or (2) need to be satisfied (but
#' not necessarily both). If `'involved'`, then (1) is required. If
#' `'referred'`, then (2) is required. If `'both'`, then both (1) and (2) are
#' required simultaneously.
#'
#' @param arrival The emergency presentation arrival timestamp.
#' @param actual_departure The actual departure timestamp from the emergency
#'   department.
#' @param episode_start The timestamp of the admitted patient episode start.
#' @param source_referral The admitted patient source of referral code.
#' @param ed_status The admitted patient record of emergency department
#'   involvement.
#' @param facility_admission The recorded facility of the admitted patient
#'   record.
#' @param facility_emergency The recorded facility in the emergency presentation
#'   record.
#' @param facilities_must_match If true, then `facility_admission` must match
#'   `facility_emergency` for a link to be considered.
#' @param ED_requirements The types of situations where a link can be
#'   considered. See Details.
#' @param referral_codes Codes in `source_referral` that indicate referral from
#'   an emergency department.
#' @param involvement_codes Codes in `ed_status` that indicate involvement of an
#'   emergency department in an admission.
#' @param admission_time Time allowed from the end of presentation to the
#'   admission starting.
#' @param pre_presentation_buffer Fudge factor: how much earlier can the
#'   admission start before the presentation arrival for a link to be
#'   considered. Only included to counter data quality issues.
#'
#' @return An `rlang::expr` for use in `find_links`.
#' @export
#'
#' @family link predicates
ap_from_ed_received <- function(
    arrival,
    actual_departure,
    facility_emergency,
    episode_start,
    source_referral,
    ed_status,
    facility_admission,
    facilities_must_match = TRUE,
    ED_requirements = c('either', 'involved', 'referred', 'both', 'any'),
    referral_codes = !!getOption('jnyr.apdc.refer.ed'),
    involvement_codes = !!getOption('jnyr.apdc.involve.ed'),
    admission_time = !!getOption('jnyr.eddc.time.admission'),
    pre_presentation_buffer = !!getOption('jnyr.eddc.time.buffer')) {
  ## Time requirements
  timereq <- rlang::expr(
    {{arrival}} - {{pre_presentation_buffer}} <= {{episode_start}} &
      {{episode_start}} <= {{actual_departure}} + {{admission_time}}
  )

  ED_requirements <- match.arg(ED_requirements)
  edreq <- switch(
    ED_requirements,
    any =
      rlang::expr(TRUE),
    either =
      rlang::expr(({{source_referral}} %in% {{referral_codes}} |
                    {{ed_status}} %in% {{involvement_codes}})),
    involved =
      rlang::expr({{ed_status}} %in% {{involvement_codes}}),
    referred =
      rlang::expr({{source_referral}} %in% {{referral_codes}}),
    both =
      rlang::expr({{source_referral}} %in% {{referral_codes}} &
                    {{ed_status}} %in% {{involvement_codes}})
  )

  facreq <- if (facilities_must_match)
    rlang::expr({{facility_admission}} == {{facility_emergency}})
  else
    rlang::expr(TRUE)

  rlang::expr(!!timereq & !!edreq & !!facreq)
}

#' Admissions from Emergency Departments, with evidence in presentation record
#'
#' `ap_from_ed_sent` finds transfers where a patient was admitted from an
#' emergency department presentation. The evidence for these transfers is taken
#' from the mode of separation at the end of the presentation.
#'
#' @param arrival The emergency presentation arrival timestamp.
#' @param actual_departure The actual departure timestamp from the emergency
#'   department.
#' @param episode_start The timestamp of the admitted patient episode start.
#' @param facility_admission The recorded facility of the admitted patient
#'   record.
#' @param facility_emergency The recorded facility in the emergency presentation
#'   record.
#' @param facilities_must_match If true, then `facility_admission` must match
#'   `facility_emergency` for a link to be considered.
#' @param admission_time Time allowed from the end of presentation to the
#'   admission starting.
#' @param pre_presentation_buffer Fudge factor: how much earlier can the
#'   admission start before the presentation arrival for a link to be
#'   considered. Only included to counter data quality issues.
#' @param modesep_presentation The mode of separation for the end of the
#'   emergency presentation.
#' @param admission_codes The codes in `modesep_presentation` that correspond to
#'   an admission.
#'
#' @return An `rlang::expr` for use in `find_links`.
#' @export
#'
#' @family link predicates
ap_from_ed_sent <- function(
    arrival,
    actual_departure,
    modesep_presentation,
    facility_emergency,
    episode_start,
    facility_admission,
    facilities_must_match = TRUE,
    admission_codes = !!getOption('jnyr.eddc.modesep.admission'),
    admission_time = !!getOption('jnyr.eddc.time.admission'),
    pre_presentation_buffer = !!getOption('jnyr.eddc.time.buffer')) {
  ## Time requirements
  timereq <- rlang::expr(
    {{arrival}} - {{pre_presentation_buffer}} <= {{episode_start}} &
      {{episode_start}} <= {{actual_departure}} + {{admission_time}}
  )

  ## Facility requirements
  facreq <- if (facilities_must_match)
    rlang::expr({{facility_admission}} == {{facility_emergency}})
  else
    rlang::expr(TRUE)

  ## Separation requirements
  sepreq <- rlang::expr({{modesep_presentation}} %in% {{admission_codes}})

  rlang::expr(!!timereq & !!facreq & !!sepreq)
}

#' Admission from emergency department, with evidence in both records
#'
#' `ap_from_ed_both` finds transfers where a patient is admitted from an
#' emergency department presentation. The evidence for this transfer is required
#' in both records.
#'
#' @param arrival The emergency presentation arrival timestamp.
#' @param actual_departure The actual departure timestamp from the emergency
#'   department.
#' @param episode_start The timestamp of the admitted patient episode start.
#' @param facility_admission The recorded facility of the admitted patient
#'   record.
#' @param facility_emergency The recorded facility in the emergency presentation
#'   record.
#' @param facilities_must_match If true, then `facility_admission` must match
#'   `facility_emergency` for a link to be considered.
#' @param admission_time Time allowed from the end of presentation to the
#'   admission starting.
#' @param pre_presentation_buffer Fudge factor: how much earlier can the
#'   admission start before the presentation arrival for a link to be
#'   considered. Only included to counter data quality issues.
#' @param modesep_presentation The mode of separation for the end of the
#'   emergency presentation.
#' @param admission_codes The codes in `modesep_presentation` that correspond to
#'   an admission.
#' @param source_referral The admitted patient source of referral code.
#' @param ed_status The admitted patient record of emergency department
#'   involvement.
#' @param ED_requirements The types of situations where a link can be
#'   considered. See \code{\link{ap_from_ed_received}}.
#' @param referral_codes Codes in `source_referral` that indicate referral from
#'   an emergency department.
#' @param involvement_codes Codes in `ed_status` that indicate involvement of an
#'   emergency department in an admission.
#'
#' @return An `rlang::expr` for use in `find_links`.
#' @export
#'
#' @family link predicates
ap_from_ed_both <- function(
    arrival,
    actual_departure,
    modesep_presentation,
    facility_emergency,
    episode_start,
    source_referral,
    ed_status,
    facility_admission,
    facilities_must_match = FALSE,
    ED_requirements = c('either', 'involved', 'referred', 'both', 'any'),
    referral_codes = !!getOption('jnyr.apdc.refer.ed'),
    involvement_codes = !!getOption('jnyr.apdc.involve.ed'),
    admission_codes = !!getOption('jnyr.eddc.modesep.admission'),
    admission_time = !!getOption('jnyr.eddc.time.admission'),
    pre_presentation_buffer = !!getOption('jnyr.eddc.time.buffer')) {

  asent <- ap_from_ed_sent(
    arrival = {{arrival}},
    actual_departure = {{actual_departure}},
    modesep_presentation = {{modesep_presentation}},
    facility_emergency = {{facility_emergency}},
    episode_start = {{episode_start}},
    facility_admission = {{facility_admission}},
    facilities_must_match = facilities_must_match,
    admission_codes = {{admission_codes}},
    admission_time = {{admission_time}},
    pre_presentation_buffer = {{pre_presentation_buffer}}
  )

  arec <- ap_from_ed_received(
    arrival = {{arrival}},
    actual_departure = {{actual_departure}},
    facility_emergency = {{facility_emergency}},
    episode_start = {{episode_start}},
    source_referral = {{source_referral}},
    ed_status = {{ed_status}},
    facility_admission = {{facility_admission}},
    facilities_must_match = {{facilities_must_match}},
    ED_requirements = {{ED_requirements}},
    referral_codes = {{referral_codes}},
    involvement_codes = {{involvement_codes}},
    admission_time = {{admission_time}},
    pre_presentation_buffer = {{pre_presentation_buffer}}
  )

  call2('&', asent, arec)
}

#' Transfers from an admitted patient episode to an emergency department
#'
#' `ed_from_ap_sent` identifies presentations to an emergency department where
#' patients were transferred from an admitted patient episode.
#'
#' @param arrival The emergency presentation arrival timestamp.
#' @param episode_end The admission episode end timestamp.
#' @param modesep_admission The mode of separation from the admission record.
#' @param transfer_time The time allowed for a transfer to take place
#' @param transfer_codes Codes for `modesep_admission` that indicate a transfer
#'   has taken place.
#'
#' @return An `rlang::expr` for use in `find_links`.
#' @export
#'
#' @family link predicates
ed_from_ap_sent <- function(
    arrival,
    episode_end,
    modesep_admission,
    transfer_time = !!getOption('jnyr.apdc.time.transfer'),
    transfer_codes = !!getOption('jnyr.apdc.modesep.edtrans')) {
  ## Time requirements
  timereq <- rlang::expr(
    {{episode_end}} <= {{arrival}} &
      {{arrival}} <= {{episode_end}} + {{transfer_time}}
  )

  ## Separation requirements
  transreq <- rlang::expr({{modesep_admission}} %in% {{transfer_codes}})

  rlang::expr(!!timereq & !!transreq)
}
