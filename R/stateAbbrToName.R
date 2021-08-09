#' Convert state abbreviations to state names
#'
#' This makes use of Arul John's `us-states` repository to convert state
#' abbrevations/territory abbreviations to state/territory names
#'
#' @param df A \code{data.frame}, expected to contain at least two variables:
#'   \code{StateName}, and \code{FIPS}.
#'
#' @return The same \code{data.frame}, but with all abbreviations found in
#'   \code{StateName} or \code{FIPS} replaced with full names.
#'   
#' @export
stateAbbrToName <- function(df) {
  mutate(
    df,
    StateName = ifelse(
      StateName %in% names(abbr2state),
      abbr2state[StateName],
      StateName
    ),
    FIPS = ifelse(
      FIPS %in% names(abbr2state),
      abbr2state[FIPS],
      FIPS
    )
  )
}
