#' Roll several dice and perform computations
#'
#' @param cmd the string describing the dice roll to simulate
#'
#' @return result of the dice roll computation
#' @export
#'
#' @examples
#'
roll_dice <- function(cmd,roll_history=FALSE) {
  if (roll_history) {
    message(paste0('Evaluating "', cmd,'" \n',
                   '=========='))
  }

  parsed_cmd <- parse_roll_cmd(cmd)
  if (roll_history) {
    result <- evaluate_roll_cmd(parsed_cmd)
  } else {
    result <- suppressMessages(evaluate_roll_cmd(parsed_cmd))
  }

  if (roll_history) {
  message(paste('==========\n',
            "Result is", result))
  }
  return(result)
}
