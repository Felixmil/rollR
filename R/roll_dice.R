#' Roll Dice
#'
#' @description Main function to use the library. This function wraps all the computing functions of the library. let the user choose between print or hide all rolls details in the console.
#'
#' @param cmd character string describing the dice roll to compute.
#' @param roll_history boolean, if TRUE, dice rolls details are visible in the console.
#'
#' @return result of the dice roll computation
#'
#' @export
#'
#' @examples
#'
#' roll_dice("1d10 + 20")
#' roll_dice("1d4 * 2")
#' roll_dice("2d20h1")
#'
roll_dice <- function(cmd, roll_history=FALSE) {
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
