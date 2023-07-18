
#' Calculate Developmental Durations
#'
#' This function calculates the duration of wall thickening for the current day based on environmental control.
#'
#' @details The function calculates the duration of wall thickening for the current day.
#'
#' @return The calculated wall thickening duration for the current day.
#'
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  "edit this depending on the function you translate"
#' @export
#' @example
CalculateDevelopmentalDurations <- function(day) {
  # We calculate the duration of wall thickening for the current day
  if (ThickeningDurationControl == 0) {
    ThickeningDuration <- MaxDaysSecThickening * (1 - fEnvironment[day])^WallThickDurationSensEnvironment
    # if environmental stress increases wall thickening duration
  } else {
    ThickeningDuration <- MaxDaysSecThickening * fEnvironment[day]^WallThickDurationSensEnvironment
  }

  if (ThickeningDuration > MaxDaysSecThickening) {
    ThickeningDuration <- MaxDaysSecThickening
  }

  MinDaysSecThickening <- MaxDaysSecThickening / 2
  # This defines the lower limit of wall thickening duration

  if (ThickeningDuration < MinDaysSecThickening) {
    ThickeningDuration <- MinDaysSecThickening
  }

  return(ThickeningDuration)
}
