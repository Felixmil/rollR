#' Rolltable class
#'
#' Class used to store multiple dice rolls for a roll command.
#' This function is the main helper, which sets up a new rolltable.
#'
#' @param cmd character string describing the dice roll to compute.
#' @param repetitions Number of times to roll the command.
#' @param verbose If TRUE, dice rolls details are visible in the console.
#' @export
rolltable <- function(cmd, repetitions = 1, verbose = FALSE) {
  cmd <- tolower(cmd) # avoid needing to check for a bunch of capital letters
  parsed_cmd <- rollr:::parse_roll_cmd(cmd)
  dice_tbls <- lapply(parsed_cmd$dices, construct_dice_table)
  tbl <- new_rolltable(lst = dice_tbls,
                       cmd = cmd,
                       dices = parsed_cmd$dices,
                       operators = parsed_cmd$operators)
  roll(tbl, repetitions = repetitions, verbose = verbose)
}

#' Constructor for rolltable
#'
#' Internal function to build the rolltable class structure.
#' Minimal checks for validity.
new_rolltable <- function(lst, cmd, dices, operators) {
  stopifnot(is.list(lst))
  stopifnot(is.character(cmd),
            length(cmd) == 1)
  stopifnot(is.character(dices),
            length(dices) == length(lst))
  stopifnot(is.character(operators),
            length(operators) == (length(dices) - 1))

  names(lst) <- dices
  structure(
    .Data = lst,
    command = cmd,
    dices = dices,
    operators = operators,
    class = "rolltable"
  )
}

#' Method to roll one or more iterations of dice.
#'
#' Takes a rolltable and rolls to create a new set of results.
#'
#' @param tbl A rolltable class.
#' @param repetitions Number of times to roll the command.
#' @param verbose If TRUE, dice roll details are visible in the console.
#' @return An updated rolltable.
#' @export
roll <- function(tbl, ...) { UseMethod("roll") }
roll.rolltable <- function(tbl, repetitions = 1, verbose = FALSE) {
  fn <- ifelse(verbose, lapply, function(...) suppressMessages(lapply(...)))
  rollhist <- fn(tbl, function(tbl, n) { do.call(rbind, replicate(n, calculate_dice_table(tbl), simplify = FALSE)) },
                     n = repetitions)

  # lapply strips class, so add back.
  new_rolltable(rollhist, cmd = attr(tbl, "command"), dices = attr(tbl, "dices"), operators = attr(tbl, "operators"))
}

#' Method to calculate the current value of a rolltable
#'
#' For each repetition in the rolltable, summarizes each dice set and
#' applies the operators in turn.
#'
#' @param tbl A rolltable class.
#' @param .summary_fn Function used to summarize results between repetitions. Default returns each repetition separately.
#' @return A numeric vector whose names are the individual sums for each dice set.
#' @export
calculate <- function(tbl, ...) { UseMethod("calculate") }
calculate.rolltable <- function(tbl, .summary_fn = "identity") {
  out <- parse_result(tbl, operators = attr(tbl, "operators"), .summary_fn = get(.summary_fn))
  attr(out, "command") <- attr(tbl, "command")
  attr(out, "parse_function") <- .summary_fn
  out
}

#' Determines the average over repetitions for the rolls.
#' Calculates the total by applying the operators to each average.
#'
#' @param tbl A rolltable class.
#' @return A numeric vector whose names are the individual means for each dice set.
mean.rolltable <- function(x) {
  calculate(x, .summary_fn = "mean")
}

#' Determines the median over repetitions for the rolls.
#' Calculates the total by applying the operators to each median value.
#'
#' @param tbl A rolltable class.
#' @return A numeric vector whose names are the individual means for each dice set.
median.rolltable <- function(x, na.rm = FALSE) {
  calculate(x, .summary_fn = "median")
}



#' Determines the minimum over repetitions for the rolls.
#' Calculates the total by applying the operators to each minimum.
#'
#' @param tbl A rolltable class.
#' @return A numeric vector whose names are the individual minimums for each dice set.
min.rolltable <- function(tbl, ..., na.rm = FALSE) {
  calculate(tbl, .summary_fn = "min")
}

#' Determines the maximum over repetitions for the rolls.
#' Calculates the total by applying the operators to each maximum.
#'
#' @param tbl A rolltable class.
#' @return A numeric vector whose names are the individual maximums for each dice set.
max.rolltable <- function(tbl, ..., na.rm = FALSE) {
  calculate(tbl, .summary_fn = "max")
}

