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
roll_dice <- function(cmd, roll_history=FALSE, repetitions = 1, verbose = FALSE) {
  if(length(cmd) > 1) {
    out <- lapply(cmd, roll_dice, roll_history = roll_history, repetitions = repetitions)
    names(out) <- cmd
    return(out)
  }

  tbl <- rolltable(cmd, repetitions = repetitions, verbose = verbose)
  result <- calculate(tbl)

  if(verbose) {
    message(paste('==========\n',
                  "Result is", result))
  }

  if(roll_history) {
    return(list(result = result,
                roll_history = tbl))
  }

  return(result)
}
