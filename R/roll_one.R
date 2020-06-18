#' Roll a dice command
#'
#' @param dice a string corresponding to a dice command
#'
#' @return result of the roll(s)
#'
#' @examples
roll_one <- function(dice, operator){
  no_dice_pattern = "^\\d+$"
  simple_pattern = "^(\\d+)[dD](\\d+)$"
  keep_h_pattern = "^(\\d+)[dD](\\d+)[Hh](\\d+)"
  keep_l_pattern = "^(\\d+)[dD](\\d+)[Ll](\\d+)"

  if (!is.na(stringr::str_match(dice, no_dice_pattern)[1])) {
    result = stringr::str_match(dice, no_dice_pattern)[1]
    message(paste0("Adding ",operator, result))
  } else if (!is.na(stringr::str_match(dice, simple_pattern)[1])) {
    match = stringr::str_match(dice, simple_pattern)
    message(paste0("Rolling ", match[2],'d',match[3],':'))
    rolls = sample(1:match[3],match[2], replace = TRUE)
    message(paste('\t',paste(rolls, collapse = ', ')))
    result = sum(rolls)
  } else if (!is.na(stringr::str_match(dice, keep_h_pattern)[1])) {
    match = stringr::str_match(dice, keep_h_pattern)
    message(paste0("Rolling ", match[2],'d',match[3],':'))
    rolls = sample(1:match[3],match[2], replace = TRUE)
    message(paste('\t',paste(rolls, collapse = ', ')))
    message(paste("keeping", match[4], "highest roll(s):", paste(sort(rolls,decreasing = T)[as.numeric(1:match[4])], collapse=", ")))
    result =  sum(sort(rolls,decreasing = T)[1:as.numeric(match[4])])
  } else if (!is.na(stringr::str_match(dice, keep_l_pattern)[1])) {
    match = stringr::str_match(dice, keep_l_pattern)
    message(paste0("Rolling ", match[2],'d',match[3],':'))
    rolls = sample(1:match[3],match[2], replace = TRUE)
    message(paste('\t',paste(rolls, collapse = ', ')))
    message(paste("keeping", match[4], "lowest roll(s):", paste(sort(rolls)[as.numeric(1:match[4])], collapse=", ")))
    result =  sum(sort(rolls)[as.numeric(1:match[4])])
  } else {
    result = NA
    warning('dice could not be rolled')
  }
  return(result)
}
