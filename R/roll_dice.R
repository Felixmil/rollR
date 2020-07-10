#' Roll Dice
#'
#' @description Main function to use the library. User can provide one or more commands using character strings, and choose to see roll details in the console.
#'
#' @param cmd character string describing the dice roll to compute.
#' @param roll_history boolean, if TRUE, a list of rolltables will be returned along with the result.
#' @param repetitions integer value
#' @param verbose boolean, if TRUE, dice rolls details are visible in the console.
#'
#' @return result of the dice roll computation, as a rolltable_calculation.
#' If roll_history is TRUE, a list will be returned with two elements: the result and the roll_history.
#' The returned roll_history is a named list, with a single rolltable for each dice set.
#'
#' @export
#'
#' @details
#' The \code{roll_dice} function parses one or more commands, where each command can be any valid dice set or sets,
#' using one or more mathematical operations. See \code{\link[base]{Ops}}. Parentheticals are permitted for controlling
#' order of operations. The \code{roll_dice} function first creates a \code{\link{rolltable}} for each dice set, then
#' applies the command operations to the rolltable(s). The result is a rolltable_calculation. See \code{\link{calculate}}.
#'
#' @section Dice sets:
#' Dice sets are commands for rolling one or more dice, possibly applying a modifier,
#' and possibly applying a test for success. The various dice set commands are described at
#' https://github.com/Felixmil/rollR.
#'
#' Basic dice sets are:
#' \itemize{
#'   \item Integer. A positive integer. Usually used as part of a formula, e.g. 1d20 + 5
#'   \item Dice: NdX. Roll N dice with sides X. Example: 3d6, 2d20, 1d10.
#'   \item Fate: Ndf. Roll N Fudge/Fate dice. Typically 4df.
#' }
#'
#' Modifications to dice are:
#' \itemize{
#'   \item High/Low: 2d20hN or 2d20lN. Keep N highest or N lowest dice from the roll.
#'         Examples: D&D 5e advantage: 2d20h1. Disadvantage: 2d20l1. Ability roll: 4d6h3.
#'   \item Reroll: 2d20rN. Reroll whenever N appears, and keep the result.
#'         Examples: Halflings are lucky and reroll on a 1: 1d20r1.
#'   \item Exploding: 2d20!N. For each N rolled, roll again and add the result to the initial roll.
#'         You continue adding rolls as long as N appears.
#'         Variation: 2d20!>N. Reroll (explode) any N or higher.
#'         Example: Modified critical: 1d20!>19.
#'   \item Double: 2d20tN. If N is rolled, then count it twice.
#' }
#'
#' Success tests:
#' \itemize{
#'   \item Equality: 2d20=N. Only count a success if the roll equals N.
#'   \item Difficulty check: 2d20>=N. Only count a success if the roll is greater or equal to N.
#' }
#'
#' To use a success test along with a modification, just add the success test after the modification.
#'
#' Spaces: Be careful in using spaces. Die sets should not contain spaces.
#' For example, '2d20h1>=10' is a difficulty success test; '2d20h1!>10' explodes everything 10 or higher;
#' '2d20h1 >= 10' is a mathematical operation, comparing the result of the 2d20h1 roll to 10.
#'
#' @section Operations and rollr classes:
#' Rollr introduces two types of classes to facilitate dice rolls and manipulations: rolltable and rolltable_calculation.
#' Rolltables store all the information needed to recreate the rolls for a dice set. See \code{\link{rolltable}}.
#' Rolltable calculations are the result of applying \code{\link{calculate}} on a rolltable, or applying a mathematical
#' operation. For example, \code{roll_dice("4d6 + 2")}. See \code{\link{calculate}} and \code{\link{new_rolltable_calculation}}.
#'
#' @section Repetitions:
#' Rollr can run multiple repetitions of a command, and can use mathematical operations across repetitions.
#' In rolltables, repetitions are stored in a dataframe.
#' In rolltable calculations, repetitions are represented by a vector.
#'
#' @examples
#'
#' roll_dice("4d6h3")
#' roll_dice("1d10 + 20")
#'
#' # See the rolls as they happen
#' roll_dice("2d20h1", verbose = TRUE)
#'
#' # Examine roll history and run multiple repetitions
#' outcome <- roll_dice("4d6!>5 + 1d4", roll_history = TRUE, repetitions = 3)
#' print(outcome$roll_history, rolls = TRUE)
#'
roll_dice <- function(cmd, roll_history=FALSE, repetitions = 1, verbose = FALSE) {
  stopifnot(repetitions > 0)

  if(length(cmd) > 1) {
    out <- lapply(cmd, roll_dice, roll_history = roll_history, repetitions = repetitions)
    names(out) <- cmd
    return(out)
  }

  # parse the command and create a roll table for each dices.
  parsed_cmd <- parse_roll_cmd(cmd)

  tbls <- lapply(parsed_cmd$dices, rolltable, repetitions = repetitions, verbose = verbose)
  # names(tbls) <- parsed_cmd$dices
  names(tbls) <- infinite_letters(length(tbls)) # use letters to avoid subbing over numbers such as 1d6+1 -> tbl[[1]] + tbl[[2]]

  cmd_to_eval <- cmd
  for(i in seq_along(parsed_cmd$dices)) {
    cmd_to_eval <- sub(pattern = parsed_cmd$dices[[i]], replacement = sprintf("tbls[['%s']]", names(tbls)[[i]]), x = cmd_to_eval, fixed = TRUE)
  }
  result <- eval(parse(text = cmd_to_eval))

  # if the command is simple, it might be left as a rolltable
  if(inherits(result, "rolltable")) result <- calculate(result)


  if(roll_history) {
    names(tbls) <- parsed_cmd$dices
    return(list(result = result,
                roll_history = tbls))
  }

  return(result)
}

#' Helper function to create names using only letters, not numbers
#' So that a command can be substituted without accidentally overwriting the number.
#' For example, subbing "1d6 + 1" with tbls[[1]] and tbls[[2]] needs to result in tbls[[1]] + tbls[[2]]
#' But instead will result in tbls[[tbls[[2]]]] + 1
#' @param n Number of distinct names required
#' @return a character vector of unique names
#' @importFrom utils combn
infinite_letters <- function(n) {
  max_n <- 0
  k <- 0
  while(max_n < n) {
    k <- k + 1
    max_n <- choose(26, k)
  }
  values <- unlist(combn(letters, k, paste0, simplify = FALSE, collapse = ""))
  values[1:n]
}
